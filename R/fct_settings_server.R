monitor_perf <- function(r = shiny::reactiveValues(), action = "stop", task = character()){
  
  if (!r$perf_monitoring) return()
  if (action == "start") datetime_start <<- Sys.time()
  
  if (action == "stop"){
    datetime_stop <<- Sys.time()
    
    r$perf_monitoring_table <- 
      r$perf_monitoring_table %>% 
      dplyr::bind_rows(tibble::tribble(
        ~task, ~datetime_start, ~datetime_stop, 
        task, datetime_start, datetime_stop))
    
    datetime_start <<- Sys.time() 
  }
}

#' Add new settings data
#' 
#' @param session Shiny session variable
#' @param output Shiny output variable
#' @param r Shiny r reactive value, to communicate between modules (reactiveValue)
#' @param language Language used (character)
#' @param id ID of current module / page (character)
#' @param data A list with data to add (list)
#' @param table Name of the corresponding table in database (character)
#' @param required_textfields Which textfields are required (not empty) before inserting data in database ? (character)
#' @param req_unique_values Which fields require unique values before inserting data in database ? (character)
#' @param required_dropdowns Which dropdowns are required (not empty) before insert data in database ? Default to "all" (character)
#' @param dropdowns Tibble with the values of distinct dropdowns names (tibble)
#' @examples 
#' \dontrun{
#' data <- list()
#' data$name <- "New datamart"
#' data$description <- "Description of the datamart"
#' data$data_source <- 5
#' add_settings_new_data(output = output, r = r, language = language, id = "settings_datamarts", data = data, dropdowns = "data_source")
#' }

add_settings_new_data <- function(session, output, r = shiny::reactiveValues(), language = "EN", id = character(),
  data = tibble::tibble(), table = character(), required_textfields = character(), req_unique_values = character(), 
  required_dropdowns = "all", dropdowns = character()){
  
  # For textfields, we have the choice if empty data is possible or not
  # For dropdowns, we want all of them filled
  
  # Check if required textfields are not empty
  
  sapply(required_textfields, function(textfield){
    
    if (is.na(data[[textfield]])){
      shiny.fluent::updateTextField.shinyInput(session, 
        textfield, errorMessage = translate(language, paste0("provide_valid_", textfield), r$words))
    }
    else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
    
    req(!is.na(data[[textfield]]))
  })
  
  # Check if values required to be unique are unique
  sapply(req_unique_values, function(field){
    
    # If it is a new module, group by module family, so a name musn't be unique in distinct modules families
    if (table %in% c("patient_lvl_modules", "aggregated_modules")){
      if (is.na(data$parent_module)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE 
        AND module_family_id = {data$module_family} AND parent_module_id IS NULL", .con = r$db)
      if (!is.na(data$parent_module)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
        AND module_family_id = {data$module_family} AND parent_module_id = {data$parent_module}", .con = r$db)
    }
    else if (table == "studies") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND datamart_id = {data$datamart}", .con = r$db)
    else sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE", .con = r$db)
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
      
    if (tolower(data[[field]]) %in% distinct_values) show_message_bar(output, 2, paste0(field, "_already_used"), "severeWarning", language)
      req(tolower(data[[field]]) %not_in% distinct_values)
  })
  
  # Check if dropdowns are not empty (if all are required)
  dropdowns_check <- TRUE
  
  if (required_dropdowns == "all"){
    sapply(dropdowns, function(dropdown){
      if (dropdown != ""){
        if (is.null(data[[dropdown]])) dropdowns_check <<- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <<- FALSE
      }
    })
  }

  else {
    sapply(required_dropdowns, function(dropdown){
      if (dropdown != ""){
        if (is.null(data[[dropdown]])) dropdowns_check <<- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <<- FALSE
      }
    })
  }
  
  if (!dropdowns_check) show_message_bar(output, 2, "dropdown_empty", "severeWarning", language)
  req(dropdowns_check)


  # Get last_row nb
  last_row <- get_last_row(r$db, table)

  # Creation of new_data variable for data_management pages
  if (table %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){

    # These columns are found in all of these tables
    new_data <- tibble::tribble(~id, ~name, ~description, last_row + 1, as.character(data$name), as.character(data$description))

    if (id == "settings_datamarts") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.integer(data$data_source)))
    if (id == "settings_studies") new_data <- new_data %>% dplyr::bind_cols(
      tibble::tribble(~datamart_id, ~patient_lvl_module_family_id, ~aggregated_module_family_id,
        as.integer(data$datamart), as.integer(data$patient_lvl_module_family), as.integer(data$aggregated_module_family)))
    if (id == "settings_subsets") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~study_id, as.integer(data$study)))
    if (id == "settings_thesaurus") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.character(data$data_source)))

    # These columns are also found in all of these tables
    # Add them at last to respect the order of cols
    new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, r$user_id, as.character(Sys.time()), FALSE))
  }

  # Creation of new_data variable for plugins page
  if (table == "plugins"){
    new_data <- tibble::tribble(~id, ~name, ~description, ~module_type_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), "", as.integer(data$module_type), as.character(Sys.time()), FALSE)
  }
  
  # Creation of new_data variable for scripts page
  if (table == "scripts"){
    new_data <- tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), "", as.integer(data$data_source), r$user_id, as.character(Sys.time()), FALSE)
  }

  # Creation of new_data variable for users sub-pages
  # Password is hashed
  if (table == "users"){
    new_data <- tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$username), as.character(data$firstname), as.character(data$lastname),
      rlang::hash(data$password), as.integer(data$user_access), as.integer(data$user_status), as.character(Sys.time()), FALSE)
  }

  if (table %in% c("users_accesses", "users_statuses")){
    new_data <- tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), as.character(data$description), as.character(Sys.time()), FALSE)
  }

  if (table %in% c("patient_lvl_modules", "aggregated_modules")){
    new_data <- tibble::tribble(~id, ~name,  ~description, ~module_family_id, ~parent_module_id,  ~display_order, ~creator_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), as.character(data$description), as.integer(data$module_family), as.integer(data$parent_module),
      as.integer(data$display_order), r$user_id, as.character(Sys.time()), FALSE)
  }

  if (table %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    new_data <- tibble::tribble(~id, ~name,  ~description, ~creator_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), as.character(data$description), r$user_id, as.character(Sys.time()), FALSE)
  }

  # Append data to the table
  DBI::dbAppendTable(r$db, table, new_data)
  add_log_entry(r = r, category = paste0(table, " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_data))
  
  # Refresh r variables
  update_r(r = r, table = table, language = language)

  # Add new rows in code table & options table
  # Add default subsets when creating a new study
  last_row_code <- get_last_row(r$db, "code")
  last_row_options <- get_last_row(r$db, "options")
  last_row_subsets <- get_last_row(r$db, "subsets")

  # Add a row in code if table is datamarts, thesaurus
  if (table %in% c("datamarts", "thesaurus")){
    
    new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row_code + 1, get_singular(word = table), last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_code)
    update_r(r = r, table = "code", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_code))
  }

  # For options of plugins, add one row for long description (Markdown) & 2 rows for users allowed to use this plugin
  # The value is the default syntax of a plugin description
  # For code of plugins, add two rows, one for UI code & one for server code
  if (id == "settings_plugins"){

    # Add options rows
    # value <- paste0("- **Version** : 0.0.1\n- **Libraries** : *put libraries needed here*\n- **Data allowed** : *put data allowed here*\n",
    #   "- **Previous plugin needed first** : *put previous plugins needed here*\n\n*Put full description here*")
    
    new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      # last_row_options + 1, "plugin", last_row + 1, "markdown_description", value, NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 1, "plugin", last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 2, "plugin", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "options", new_options)
    update_r(r = r, table = "options", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_options))

    # Add code rows
    new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row_code + 1, "plugin_ui", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_code + 2, "plugin_server", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_code)
    update_r(r = r, table = "code", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_code))
  }
  
  # For options of scripts, add one row for long description (Markdown)
  if (id == "scripts"){
    
    # Add options rows
    
    #new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      # last_row_options + 1, "plugin", last_row + 1, "markdown_description", value, NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
    #  last_row_options + 1, "plugin", last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
    #  last_row_options + 2, "plugin", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
    #DBI::dbAppendTable(r$db, "options", new_options)
    #update_r(r = r, table = "options", language = language)
    
    #add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_options))
    
    # Add code rows
    new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row_code + 1, "script", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_code)
    update_r(r = r, table = "code", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_code))
  }

  # For options of datamarts, need to add 3 rows in options
  if (id == "settings_datamarts"){
    
    new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row_options + 1, "datamart", last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 2, "datamart", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 3, "datamart", last_row + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "options", new_options)
    update_r(r = r, table = "options", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_options))
  }

  # For studies, need to add one row in options and add rows of code for subsets, with default value
  if (id == "settings_studies"){

    new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row_options + 1, "study", last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 2, "study", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "options", new_options)
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_options))
    
    # Add rows in subsets table, for inclusion / exclusion subsets
    # Add also code corresponding to each subset
    new_subsets <- tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id,  ~datetime, ~deleted,
      last_row_subsets + 1, translate(language, "subset_all_patients", r$words), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_subsets + 2, translate(language, "subset_included_patients", r$words), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_subsets + 3, translate(language, "subset_excluded_patients", r$words), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "subsets", new_subsets)
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_subsets))

    # Add code for creating subset with all patients
    code <- paste0('run_datamart_code(output = output, r = r, datamart_id = %datamart_id%)\n\n',
                   'patients <- d$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at("patient_id", as.integer)\n\n',
                   'add_patients_to_subset(output = output, r = r, patients = patients, subset_id = %subset_id%)\n\n',
                   'update_r_new(r = r, m = m, table = "subset_patients")')
    new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row_code + 1, "subset", last_row_subsets + 1, code, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_code + 2, "subset", last_row_subsets + 2, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_code + 3, "subset", last_row_subsets + 3, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_code)
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_code))

    # Add patient_lvl & aggregated modules families
    
    new_patient_lvl_module_family <- tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "patient_lvl_modules_families") + 1, data$name, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "patient_lvl_modules_families", new_patient_lvl_module_family)
    add_log_entry(r = r, category = paste0("patient_lvl_module_family", " - ", translate(language, "insert_new_data", r$words)),
      name = translate(language, "sql_query", r$words), value = toString(new_patient_lvl_module_family))
    
    new_aggregated_module_family <- tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "aggregated_modules_families") + 1, data$name, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "aggregated_modules_families", new_aggregated_module_family)
    add_log_entry(r = r, category = paste0("aggregated_module_family", " - ", translate(language, "insert_new_data", r$words)),
      name = translate(language, "sql_query", r$words), value = toString(new_aggregated_module_family))
    
    # Update r$options, r$code & r$subsets, & r modules families
    update_r(r, "options", language)
    update_r(r, "subsets", language)
    update_r(r, "code", language)
    update_r(r, "patient_lvl_modules_families", language)
    update_r(r, "aggregated_modules_families", language)
    
    # Add patients to subset
    tryCatch({
      patients <- d$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)
      add_patients_to_subset(output, r, patients, last_row_subsets + 1)
    }, 
    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_adding_patients_to_subset", 
      error_name = paste0("add study - add_patients_to_subsets - id = ", last_row_subsets + 1), category = "Error", error_report = toString(e), language = language))
    
    # Update sidenav dropdown with the new study
    r$studies_choices <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE datamart_id = ", data$datamart))
    
    # Select new study as current study
    m$chosen_study <- last_row + 1
    r$study_page <- Sys.time()
  }
  
  # For options of patient_lvl & aggregated modules families, need to add two rows, for users accesses
  if (table %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    
    new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row_options + 1, get_singular(word = table), last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 2, get_singular(word = table), last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "options", new_options)
    update_r(r = r, table = "options", language = language)
    
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_options))
  }

  # Hide creation card & options card, show management card
  # Except for modules (we usually add several modules)
  if (table %not_in% c("patient_lvl_modules", "aggregated_modules")){
    shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
    shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
    shiny.fluent::updateToggle.shinyInput(session, "datatable_card_toggle", value = TRUE)
  }

  show_message_bar(output = output, id = 1, message = paste0(get_singular(table), "_added"), type = "success", language = language)

  # Reset textfields
  if (table == "users") sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
  else sapply(c("plugin_name", "script_name", "study_name", "name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
}

add_settings_new_data_new <- function(session, output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  i18n = R6::R6Class(), id = character(), data = tibble::tibble(), table = character(), required_textfields = character(), req_unique_values = character(), 
  required_dropdowns = "all", dropdowns = character()){
  
  ns <- shiny::NS(id)
  
  # --- --- --- --- --- --- --- --- -
  # Check textfields & dropdowns ----
  # --- --- --- --- --- --- --- --- -
  
  # For textfields, we have the choice if empty data is possible or not
  # For dropdowns, we want all of them filled
  
  # Check if required textfields are not empty
  
  sapply(required_textfields, function(textfield){
    
    if (is.na(data[[textfield]])){
      shiny.fluent::updateTextField.shinyInput(session, 
        textfield, errorMessage = i18n$t(paste0("provide_valid_", textfield)))
    }
    else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
    
    req(!is.na(data[[textfield]]))
  })
  
  # Check if values required to be unique are unique
  sapply(req_unique_values, function(field){
    
    # If it is a new module, group by module family, so a name musn't be unique in distinct modules families
    if (table %in% c("patient_lvl_modules", "aggregated_modules")){
      if (is.na(data$parent_module)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE 
        AND module_family_id = {data$module_family} AND parent_module_id IS NULL", .con = r$db)
      if (!is.na(data$parent_module)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
        AND module_family_id = {data$module_family} AND parent_module_id = {data$parent_module}", .con = r$db)
    }
    else if (table == "studies") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND datamart_id = {data$datamart}", .con = r$db)
    else if (table == "plugins") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND module_type_id = {data$module_type}", .con = r$db)
    else sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE", .con = r$db)
    distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(data[[field]]) %in% distinct_values) show_message_bar_new(output = output, id = 2, message = paste0(field, "_already_used"), type = "severeWarning", i18n = i18n, ns = ns)
    req(tolower(data[[field]]) %not_in% distinct_values)
  })
  
  # Check if dropdowns are not empty (if all are required)
  dropdowns_check <- TRUE
  
  if (required_dropdowns == "all"){
    for(dropdown in dropdowns){
      if (dropdown != ""){
        if (length(data[[dropdown]]) > 1) {break}
        if (is.null(data[[dropdown]])) dropdowns_check <- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <- FALSE
      }
    }
  }
  
  else {
    for(dropdown in required_dropdowns){
      if (dropdown != ""){
        if (length(data[[dropdown]]) > 1) {break}
        if (is.null(data[[dropdown]])) dropdowns_check <- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <- FALSE
      }
    }
  }
  
  if (!dropdowns_check) show_message_bar_new(output = output, id = 2, message = "dropdown_empty", type = "severeWarning", i18n = i18n, ns = ns)
  req(dropdowns_check)
  
  # --- --- --- --- --- --- --- --- -- -
  # Add data in data specific table ----
  # --- --- --- --- --- --- --- --- -- -
  
  # db variable depending on table
  m_tables <- c("patients_options", "modules_elements_options", "subsets" , "subset_patients")
  if (table %in% m_tables) db <- m$db
  else db <- r$db
  
  # Get last_row nb
  last_row <- list()
  for (var in c("code", "options", "subsets", "thesaurus")){
    if (var == "subsets") last_row[[var]] <- get_last_row(m$db, "subsets") 
    else last_row[[var]] <- get_last_row(r$db, var)
  }
  last_row$data <- get_last_row(db, table)
  
  # Create a list with all new data
  new_data <- list()
  
  # Creation of new_data$data variable for data_management pages
  if (table %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
    
    # These columns are found in all of these tables
    new_data$data <- tibble::tribble(~id, ~name, ~description, last_row$data + 1, as.character(data$name), as.character(data$description))
    
    if (table == "datamarts") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.integer(data$data_source)))
    if (table == "studies") new_data$data <- new_data$data %>% dplyr::bind_cols(
      tibble::tribble(~datamart_id, ~patient_lvl_module_family_id, ~aggregated_module_family_id,
        as.integer(data$datamart), as.integer(data$patient_lvl_module_family), as.integer(data$aggregated_module_family)))
    if (table == "subsets") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~study_id, as.integer(data$study)))
    if (table == "thesaurus") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data$data_source))
    
    # These columns are also found in all of these tables
    # Add them at last to respect the order of cols
    new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, r$user_id, as.character(Sys.time()), FALSE))
  }
  
  # Creation of new_data$data variable for plugins page
  if (table == "plugins"){
    new_data$data <- tibble::tribble(~id, ~name, ~description, ~module_type_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), "", as.integer(data$module_type), as.character(Sys.time()), FALSE)
  }
  
  # Creation of new_data$data variable for scripts page
  if (table == "scripts"){
    new_data$data <- tibble::tribble(~id, ~name, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.integer(data$data_source), r$user_id, as.character(Sys.time()), FALSE)
  }
  
  # Creation of new_data$data variable for users sub-pages
  # Password is hashed
  if (table == "users"){
    new_data$data <- tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$username), as.character(data$firstname), as.character(data$lastname),
      rlang::hash(data$password), as.integer(data$user_access), as.integer(data$user_status), as.character(Sys.time()), FALSE)
  }
  
  if (table %in% c("users_accesses", "users_statuses")){
    new_data$data <- tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.character(data$description), as.character(Sys.time()), FALSE)
  }
  
  if (table %in% c("patient_lvl_modules", "aggregated_modules")){
    new_data$data <- tibble::tribble(~id, ~name,  ~description, ~module_family_id, ~parent_module_id,  ~display_order, ~creator_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.character(data$description), as.integer(data$module_family), as.integer(data$parent_module),
      as.integer(data$display_order), r$user_id, as.character(Sys.time()), FALSE)
  }
  
  if (table %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    new_data$data <- tibble::tribble(~id, ~name,  ~description, ~creator_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.character(data$description), r$user_id, as.character(Sys.time()), FALSE)
  }
  
  # Append data to the table and to r / m variables
  DBI::dbAppendTable(db, table, new_data$data)
  if (table %in% m_tables) m[[table]] <- m[[table]] %>% dplyr::bind_rows(new_data$data)
  else r[[table]] <- r[[table]] %>% dplyr::bind_rows(new_data$data)
  add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data$data))
  
  # Empty new variables
  new_data_vars <- c("options", "subsets", "code", "patient_lvl_modules_family", "aggregated_modules_family", "thesaurus")
  for(var in new_data_vars) new_data[[var]] <- tibble::tibble()
  
  # --- --- --- --- --- --- --- --- --- --
  # Add data in code & options tables ----
  # --- --- --- --- --- --- --- --- --- --
  
  # Add a row in code if table is datamarts, thesaurus
  if (table %in% c("datamarts", "thesaurus")){
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, get_singular(word = table), last_row$data + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }
  
  # Add a new thesaurus if a new data source is created
  if (table == "data_sources"){
    
    new_thesaurus <- tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      last_row$thesaurus + 1, paste0(data$name, " - scripts items"), "", last_row$data + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE)
  
    # Add also the code for the new thesaurus
    
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "thesaurus", last_row$thesaurus + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }
  
  # For options of plugins, add one row for long description (Markdown) & 2 rows for users allowed to use this plugin
  # The value is the default syntax of a plugin description
  # For code of plugins, add two rows, one for UI code & one for server code
  if (id == "settings_plugins"){
    
    # Add options rows
    # value <- paste0("- **Version** : 0.0.1\n- **Libraries** : *put libraries needed here*\n- **Data allowed** : *put data allowed here*\n",
    #   "- **Previous plugin needed first** : *put previous plugins needed here*\n\n*Put full description here*")
    username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::mutate(fullname = paste0(firstname, " ", lastname)) %>% dplyr::pull(fullname)
    
    new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row$options + 1, "plugin", last_row$data + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 2, "plugin", last_row$data + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 3, "plugin", last_row$data + 1, "version", "0.0.1", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 4, "plugin", last_row$data + 1, "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 5, "plugin", last_row$data + 1, "author", username, NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 6, "plugin", last_row$data + 1, "image", "", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 7, "plugin", last_row$data + 1, "description_fr", "", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 8, "plugin", last_row$data + 1, "description_en", "", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE
      )
    # Add code rows
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "plugin_ui", last_row$data + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$code + 2, "plugin_server", last_row$data + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }

  # For options of scripts, add one row for long description (Markdown)
  if (id == "scripts"){
    
    # Add options rows
    
    new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row$options + 1, "script", last_row$data + 1, "markdown_description", "", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    # Add code rows
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "script", last_row$data + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }
  
  # For datamarts options, need to add 3 rows in options
  if (table == "datamarts"){
    
    new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row$options + 1, "datamart", last_row$data + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 2, "datamart", last_row$data + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 3, "datamart", last_row$data + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }
  
  # For studies, need to add one row in options and add rows of code for subsets, with default value
  if (table == "studies"){
    
    new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row$options + 1, "study", last_row$data + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 2, "study", last_row$data + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    # Add rows in subsets table, for inclusion / exclusion subsets
    # Add also code corresponding to each subset
    new_data$subsets <- tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id,  ~datetime, ~deleted,
      last_row$subsets + 1, i18n$t("subset_all_patients"), "", last_row$data + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$subsets + 2, i18n$t("subset_included_patients"), "", last_row$data + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$subsets + 3, i18n$t("subset_excluded_patients"), "", last_row$data + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    # Add code for creating subset with all patients
    code <- paste0('run_datamart_code_new(output = output, r = r, d = d, datamart_id = %datamart_id%)\n\n',
      'patients <- d$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at("patient_id", as.integer)\n\n',
      'add_patients_to_subset_new(output = output, r = r, m = m, patients = patients, subset_id = %subset_id%)\n\n',
      'update_r_new(r = r, m = m, table = "subset_patients")')
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "subset", last_row$subsets + 1, code, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$code + 2, "subset", last_row$subsets + 2, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$code + 3, "subset", last_row$subsets + 3, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    # Add patient_lvl & aggregated modules families
    
    new_data$patient_lvl_module_family <- tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "patient_lvl_modules_families") + 1, data$name, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    new_data$aggregated_module_family <- tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "aggregated_modules_families") + 1, data$name, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    
    # Add patients to subset
    tryCatch({
      patients <- d$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)
      add_patients_to_subset_new(output, r, m, patients, last_row$subsets + 1)
    }, 
      error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_adding_patients_to_subset", 
        error_name = paste0("add study - add_patients_to_subsets - id = ", last_row$subsets + 1), category = "Error", error_report = toString(e), language = language))
    
    # Update sidenav dropdown with the new study
    r$studies_choices <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE datamart_id = ", data$datamart))
    
    # Select new study as current study
    m$chosen_study <- last_row$data + 1
    r$study_page <- Sys.time()
  }
  
  # For options of patient_lvl & aggregated modules families, need to add two rows, for users accesses
  if (table %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row$options + 1, get_singular(word = table), last_row$data + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row$options + 2, get_singular(word = table), last_row$data + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
  }
  
  # Hide creation card & options card, show management card
  # Except for modules (we usually add several modules)
  if (table %not_in% c("patient_lvl_modules", "aggregated_modules")){
    shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
    shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
    shiny.fluent::updateToggle.shinyInput(session, "datatable_card_toggle", value = TRUE)
  }
  
  # Add new data to r variables and database
  for (var in new_data_vars){
    if (nrow(new_data[[var]]) > 0){
      if (var == "subsets"){
        DBI::dbAppendTable(m$db, var, new_data$subsets)
        m$subsets <- m$subsets %>% dplyr::bind_rows(new_data$subsets)
      } 
      else {
        DBI::dbAppendTable(r$db, var, new_data[[var]])
        r[[var]] <- r[[var]] %>% dplyr::bind_rows(new_data[[var]]) 
      }
      add_log_entry(r = r, category = paste0(var, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data[[var]]))
    }
  }
  
  show_message_bar_new(output = output, id = 1, message = paste0(get_singular(table), "_added"), type = "success", i18n = i18n, ns = ns)
  
  # Reset textfields
  if (table == "users") sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
  else sapply(c("plugin_name", "script_name", "study_name", "name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
}

prepare_data_datatable <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN", id = character(),
  table = character(), dropdowns = character(), dropdowns_multiselect = character(), dropdowns_null_value = character(), factorize_cols = character(),
  action_buttons = character(), data_input = tibble::tibble(), data_output = tibble::tibble(), words = tibble::tibble()
){
  
  # req(nrow(data_input) > 0)
  
  monitor_perf(r = r, action = "start")
  monitor_perf(r = r, action = "stop", task = paste0("prepare_data_datatable _ table = ", table, " / id = ", id))
  
  # Initiate data_output, starting from data_input
  data_output <- data_input
  
  # Add module family column for modules elements
  if (grepl("modules_elements", table)){
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
    if (grepl("aggregated", table)) prefix <- "aggregated"
    data_output <- data_output %>% dplyr::left_join(r[[paste0(prefix, "_modules")]] %>%
        dplyr::select(module_id = id, module_family_id), by = "module_id") %>% dplyr::relocate(module_family_id, .after = name)
  }
  
  # Add a column action in the DataTable
  # Action column is already loaded for thesaurus_items (cache system)
  if (!grepl("thesaurus_items", table) & length(action_buttons) != 0) data_output["action"] <- NA_character_
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)
  
  # Transform dropdowns columns in the dataframe to character
  if (length(dropdowns) != 0) lapply(names(dropdowns), function(col_name) data_output %>% dplyr::mutate_at(col_name, as.character) ->> data_output)
  
  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name
  
  # Loop over data only if necessary (eg not necessary for thesaurus_items, with a lot of rows...)
  # Not necessary if no dropdowns, no action_buttons & no creator_id col
  if (!grepl("thesaurus_items", table) & (length(dropdowns) != 0 | length(action_buttons) != 0 | "creator_id" %in% names(data_output))){
    
    for (i in 1:nrow(data_output)){
      
      # --- --- --- --
      # Dropdowns ----
      # --- --- --- --
      
      if (length(dropdowns) != 0){
        lapply(names(dropdowns), function(name){
          
          # name here is like "data_source_id"
          # dropdowns[name] here is like "data_sources"
          # so r[[dropdowns[[name]]]] is like r$data_sources, var containing data_sources data
          
          multiSelect = FALSE
          null_value <- FALSE
          
          # Put a null value in dropdowns
          if (name %in% dropdowns_null_value) null_value <- TRUE
          
          # If this is a multiselect dropdown, split results to integers
          if (name %in% dropdowns_multiselect){
            
            multiSelect <- TRUE
            value <- NULL
            if (!(TRUE %in% grepl("[a-zA-Z]", stringr::str_split(data_output[[i, name]], ", ") %>% unlist()))){
              value <- stringr::str_split(data_output[[i, name]], ", ") %>% unlist() %>% as.integer()
            }
          }
          else value <- as.integer(data_output[[i, name]])
          
          options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = null_value, words = words) 
          
          # For dropdown parent_module in patient_lvl & aggregated_modules, need to select only modules depending on the same module family
          
          if (dropdowns[[name]] %in% c("patient_lvl_modules", "aggregated_modules")){
            options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]] %>% dplyr::filter(module_family_id == data_output[[i, "module_family_id"]]),
              key_col = "id", text_col = "name", null_value = null_value, words = words)
          }
          
          data_output[i, name] <<- as.character(
            div(
              # ID is like "data_sources13" if ID = 13
              shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[[name]], data_output[[i, "id"]])), options = options, value = value, multiSelect = multiSelect),
              # On click, we set variable "dropdown_updated" to the ID of the row (in our example, 13)
              onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[[name]], data_output[[i, "id"]]), "', {priority: 'event'})"),
              style = "width:200px")
          )
          
        })
      }
      
      # --- --- --- --- ---
      # Action buttons ----
      # --- --- --- --- ---
      
      # Action buttons : if in action_buttons vector, add action button
      actions <- tagList()
      
      # Add add_item button
      if ("add" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("add_item_", data_output[i, "id"]), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", id, "-add_item", "', this.id, {priority: 'event'})")), "")}
      
      # Add options button
      if ("options" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("options_", data_output[i, "id"]), "", icon = icon("cog"),
            onclick = paste0("Shiny.setInputValue('", id, "-options", "', this.id, {priority: 'event'})")), "")}
      
      # Add edit code button
      if ("edit_code" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("edit_code_", data_output[i, "id"]), "", icon = icon("file-code"),
            onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}
      
      # Add sub datatable button
      if ("sub_datatable" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("sub_datatable_", data_output[i, "id"]), "", icon = icon("table"),
            onclick = paste0("Shiny.setInputValue('", id, "-sub_datatable", "', this.id, {priority: 'event'})")), "")}
      
      # Add delete button
      if ("delete" %in% action_buttons){
        
        # If row is deletable (we havn't made a function argument for deletable or not, only default subsets are not deletable)
        # Could be changed later
        
        delete <- actionButton(paste0("delete_", data_output[i, "id"]), "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})"))
        
        # Default subsets are not deletable
        if (id == "settings_subsets"){
          if (data_output[i, "name"] %in% c(translate("EN", "subset_all_patients", r$words), translate("EN", "subset_included_patients", r$words), translate("EN", "subset_excluded_patients", r$words),
            translate("FR", "subset_all_patients", r$words), translate("FR", "subset_included_patients", r$words), translate("FR", "subset_excluded_patients", r$words))) delete <- ""
        }
        
        actions <- tagList(actions, delete)
      }
      
      # Update action column in dataframe
      if (length(action_buttons) != 0) data_output[i, "action"] <- as.character(div(actions))
      
      # --- --- --- --- -
      # Creator name ----
      # --- --- --- --- -
      
      if ("creator_id" %in% names(data_output)){
        if (nrow(r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]])) > 0){
          data_output[i, "creator_id"] <-
            r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]]) %>%
            dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
            dplyr::pull(creator)
        }
        else data_output[i, "creator_id"] <- translate(language, "deleted_user", r$words)
      }
      
      # Get names for other columns if there are not dropdowns
      
      cols <- c("data_source_id" = "data_sources", "datamart_id" = "datamarts", "study_id" = "studies", "module_type_id" = "module_types")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[cols[[name]]]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
      
      cols <- c("module_family_id" = "modules_families", "module_id" = "modules", "plugin_id" = "plugins")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
          if (grepl("aggregated", table)) prefix <- "aggregated_"
          if (name == "plugin_id") prefix <- ""
          
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[paste0(prefix, cols[[name]])]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
    }
  }
  
  # Factorize cols
  
  data_output <- data_output %>% dplyr::mutate_at(factorize_cols, as.factor)
  
  data_output
}

#' Reload cache
#' 
#' @param r Shiny r reactive value
#' @param table Database table name
#' @param data Data

prepare_data_datatable_new <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), i18n = R6::R6Class(), id = character(),
  table = character(), dropdowns = character(), dropdowns_multiselect = character(), dropdowns_null_value = character(), factorize_cols = character(),
  action_buttons = character(), data_input = tibble::tibble(), data_output = tibble::tibble(), words = tibble::tibble()
){
  
  monitor_perf(r = r, action = "start")
  monitor_perf(r = r, action = "stop", task = paste0("prepare_data_datatable _ table = ", table, " / id = ", id))
  
  # Initiate data_output, starting from data_input
  data_output <- data_input
  
  # Add module family column for modules elements
  if (grepl("modules_elements", table)){
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
    if (grepl("aggregated", table)) prefix <- "aggregated"
    data_output <- data_output %>% dplyr::left_join(r[[paste0(prefix, "_modules")]] %>%
        dplyr::select(module_id = id, module_family_id), by = "module_id") %>% dplyr::relocate(module_family_id, .after = name)
  }
  
  # Add a column action in the DataTable
  # Action column is already loaded for thesaurus_items (cache system)
  if (!grepl("thesaurus_items", table) & length(action_buttons) != 0) data_output["action"] <- NA_character_
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)
  
  # Transform dropdowns columns in the dataframe to character
  if (length(dropdowns) != 0) lapply(names(dropdowns), function(col_name) data_output %>% dplyr::mutate_at(col_name, as.character) ->> data_output)
  
  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name
  
  # Loop over data only if necessary (eg not necessary for thesaurus_items, with a lot of rows...)
  # Not necessary if no dropdowns, no action_buttons & no creator_id col
  if (!grepl("thesaurus_items", table) & (length(dropdowns) != 0 | length(action_buttons) != 0 | "creator_id" %in% names(data_output))){
    
    for (i in 1:nrow(data_output)){
      
      # --- --- --- --
      # Dropdowns ----
      # --- --- --- --
      
      if (length(dropdowns) != 0){
        lapply(names(dropdowns), function(name){
          
          # name here is like "data_source_id"
          # dropdowns[name] here is like "data_sources"
          # so r[[dropdowns[[name]]]] is like r$data_sources, var containing data_sources data
          
          multiSelect = FALSE
          null_value <- FALSE
          
          # Put a null value in dropdowns
          if (name %in% dropdowns_null_value) null_value <- TRUE
          
          # If this is a multiselect dropdown, split results to integers
          if (name %in% dropdowns_multiselect){
            
            multiSelect <- TRUE
            value <- NULL
            if (!(TRUE %in% grepl("[a-zA-Z]", stringr::str_split(data_output[[i, name]], ", ") %>% unlist()))){
              value <- stringr::str_split(data_output[[i, name]], ", ") %>% unlist() %>% as.integer()
            }
          }
          else value <- as.integer(data_output[[i, name]])
          
          options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = null_value, words = words) 
          
          # For dropdown parent_module in patient_lvl & aggregated_modules, need to select only modules depending on the same module family
          
          if (dropdowns[[name]] %in% c("patient_lvl_modules", "aggregated_modules")){
            options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]] %>% dplyr::filter(module_family_id == data_output[[i, "module_family_id"]]),
              key_col = "id", text_col = "name", null_value = null_value, words = words)
          }
          
          data_output[i, name] <<- as.character(
            div(
              # ID is like "data_sources13" if ID = 13
              shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[[name]], data_output[[i, "id"]])), options = options, value = value, multiSelect = multiSelect),
              # On click, we set variable "dropdown_updated" to the ID of the row (in our example, 13)
              onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[[name]], data_output[[i, "id"]]), "', {priority: 'event'})"),
              style = "width:200px")
          )
          
        })
      }
      
      # --- --- --- --- ---
      # Action buttons ----
      # --- --- --- --- ---
      
      # Action buttons : if in action_buttons vector, add action button
      actions <- tagList()
      
      # Add add_item button
      if ("add" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("add_item_", data_output[i, "id"]), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", id, "-add_item", "', this.id, {priority: 'event'})")), "")}
      
      # Add options button
      if ("options" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("options_", data_output[i, "id"]), "", icon = icon("cog"),
            onclick = paste0("Shiny.setInputValue('", id, "-options", "', this.id, {priority: 'event'})")), "")}
      
      # Add edit code button
      if ("edit_code" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("edit_code_", data_output[i, "id"]), "", icon = icon("file-code"),
            onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}
      
      # Add sub datatable button
      if ("sub_datatable" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("sub_datatable_", data_output[i, "id"]), "", icon = icon("table"),
            onclick = paste0("Shiny.setInputValue('", id, "-sub_datatable", "', this.id, {priority: 'event'})")), "")}
      
      # Add delete button
      if ("delete" %in% action_buttons){
        
        # If row is deletable (we havn't made a function argument for deletable or not, only default subsets are not deletable)
        # Could be changed later
        
        delete <- actionButton(paste0("delete_", data_output[i, "id"]), "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})"))
        
        # Default subsets are not deletable
        if (id == "settings_subsets"){
          if (data_output[i, "name"] %in% c("All patients", "Tous les patients", "Included patients", "Patients inclus", 
            "Excluded patients", "Patients exclus")) delete <- ""
        }
        
        actions <- tagList(actions, delete)
      }
      
      # Update action column in dataframe
      if (length(action_buttons) != 0) data_output[i, "action"] <- as.character(div(actions))
      
      # --- --- --- --- -
      # Creator name ----
      # --- --- --- --- -
      
      if ("creator_id" %in% names(data_output)){
        if (nrow(r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]])) > 0){
          data_output[i, "creator_id"] <-
            r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]]) %>%
            dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
            dplyr::pull(creator)
        }
        else data_output[i, "creator_id"] <- i18n$t("deleted_used")
      }
      
      # Get names for other columns if there are not dropdowns
      
      cols <- c("data_source_id" = "data_sources", "datamart_id" = "datamarts", "study_id" = "studies", "module_type_id" = "module_types")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[cols[[name]]]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
      
      cols <- c("module_family_id" = "modules_families", "module_id" = "modules", "plugin_id" = "plugins")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
          if (grepl("aggregated", table)) prefix <- "aggregated_"
          if (name == "plugin_id") prefix <- ""
          
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[paste0(prefix, cols[[name]])]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
    }
  }
  
  # Factorize cols
  
  data_output <- data_output %>% dplyr::mutate_at(factorize_cols, as.factor)
  
  data_output
}

prepare_data_shiny_tree <- function(data = tibble::tibble(), stopened = FALSE){
  full_list <- list()
  
  if (nrow(data) > 0){
    
    # Loop over categories levels
    for (i in 1:(max(data$level))){
      
      if (i == 1){
        
        current_lvl_categories <- data %>% dplyr::filter(level == 1)
        
        # Loop over categories
        for (j in 1:nrow(current_lvl_categories)){
          
          row <- current_lvl_categories[j, ]
          
          new_list <- list(structure("", stid = row$item_id, sttype = "default", stopened = stopened))
          names(new_list) <- row$name
          full_list <- append(full_list, new_list)
        }
      }
      
      else {
        
        filtered_categories <- data %>% dplyr::filter(level == i)
        
        # Create path column
        paths <- data %>% 
          dplyr::filter(level == i) %>%
          dplyr::pull(path)
        
        # Loop over paths
        for (j in 1:length(paths)){
          
          path <- paths[j]
          
          # Select data with the same path
          same_path_categories <- filtered_categories %>% dplyr::filter(path == !!path)
          
          new_list <- list()
          
          for (k in 1:nrow(same_path_categories)){
            
            row <- same_path_categories[k, ]
            new_list_child <- list(structure("", stid = row$item_id, sttype = "default", stopened = stopened))
            names(new_list_child) <- paste0(row$name, " (", row$count_patients_rows, " | ", row$count_items_rows, ")")
            
            new_list <- new_list %>% append(new_list_child)
          }
          
          eval(parse(text = paste0("full_list$", path, " <- structure(
            sttype = 'default', stid = attributes(full_list$", path, ")$stid, stopened = ", stopened, ", new_list)")))
        }
      }
    }
  }
  
  full_list
}

#' Reload cache
#' 
#' @param r Shiny r reactive value
#' @param table Database table name
#' @param data Data

reload_cache_for_settings <- function(r = shiny::reactiveValues(), table = character(), data = tibble::tibble()){
  
  # Delete old values from cache
  
  sql <- glue::glue_sql("DELETE FROM cache_for_settings WHERE table_name = {table}", .con = r$db)
  query <- DBI::dbSendStatement(r$db, sql)
  DBI::dbClearResult(query)
  
  # Add values to cache
  
  for (i in 1:nrow(data)){
    
    row <- data[i, ] %>% dplyr::rename(link_id = id)
    row_values <- row %>% unlist(use.names = FALSE)
    
    get_last_row(r$db, "cache_for_settings")
    
    sql <- glue::glue_sql("INSERT INTO cache_for_settings(id, table_name, {`colnames(row)`*})
            SELECT {get_last_row(r$db, 'cache_for_settings') + 1}, {table}, {row*}", .con = r$db)
    query <- DBI::dbSendStatement(r$db, sql)
    DBI::dbClearResult(query)
  }
}

#' Create cache for datatable data
#' 
#' @param output Shiny output value, to show message bars
#' @param r Shiny r reactive value, to communicate between modules
#' @param language Language used (character)
#' @param module_id ID of current page / module (character)
#' @param thesaurus_id ID of thesaurus, which thesaurus items depend on (integer)
#' @param datamart_id ID of datamart to count rows by item of the thesaurus (integer)
#' @param category Category of cache, depending of the page of Settings (character)

create_datatable_cache <- function(output, r, language = "EN", module_id = character(), 
  thesaurus_id = integer(), datamart_id = 0, category = character()){
  
  # Load join between our data and the cache
  
  # For action buttons (delete & plus_minus) & colours, don't use datamart_id / link_id_bis
  if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus", "colours_module", "colours_plugin") |
      grepl("plus_data_explorer", category)){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted, c.value
      FROM thesaurus_items t
      LEFT JOIN cache c ON c.link_id = t.id AND c.category = '", category, "'
      WHERE t.thesaurus_id = ", thesaurus_id, " AND t.deleted IS FALSE
      ORDER BY t.id")) %>% tibble::as_tibble()
  }
  # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
  if (category %in% c("count_patients_rows", "count_items_rows")){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted, c.value
      FROM thesaurus_items t
      LEFT JOIN cache c ON c.link_id = t.id AND c.link_id_bis = ", datamart_id, " AND c.category = '", category, "'
      WHERE t.thesaurus_id = ", thesaurus_id, " AND t.deleted IS FALSE
      ORDER BY t.id")) %>% tibble::as_tibble()
  }
  
  # If there are missing data in the cache, reload cache 
  
  reload_cache <- FALSE
  if (NA_character_ %in% data$value | "" %in% data$value) reload_cache <- TRUE
  
  # Reload cache if necessary
  if (reload_cache){

    # Reload data
    data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", thesaurus_id, " AND deleted IS FALSE ORDER BY id"))

    # Make action column, depending on category
    # If category is count_items_rows, add a count row column with number of rows by item in the datamart
    # If category is count_patients_rows, add a count row column with number of patients by item in the datamart
    # If category is delete, add a delete button only
    # If category is plus_minus, add plus and minus buttons

    if (category == "count_items_rows"){
      
      # Run datamart code
      # Could interfere with r data variables in patient-lvl & aggregated data, but rare case...
      # A solution could be adding a global variable indicating that r data variables have changed
      run_datamart_code(output = output, r = r, language = language, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      
      if (nrow(d$labs_vitals) != 0) rows_labs_vitals <- d$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(d$text) != 0) rows_text <- d$text %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(d$orders) != 0) rows_orders <- d$orders %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      
      # Merge rows count vars
      count_items_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      # Check if argument thesaurus_id corresponds to thesaurus_name in the data
      # Select thesaurus from r$thesaurus with thesaurus_id
      # Inner join with correspondance between thesaurus_name

      count_items_rows <- count_items_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
          dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_items_rows) != 0){
        count_items_rows <- count_items_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data <- data %>% dplyr::left_join(count_items_rows, by = "item_id") %>% dplyr::rename(value = count_items_rows)
      }
      if (nrow(count_items_rows) == 0) data <- data %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data <- data %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", T ~ value))
    }
    
    if (category == "count_patients_rows"){
      
      run_datamart_code(output = output, r = r, language = language, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      
      if (nrow(d$labs_vitals) != 0) rows_labs_vitals <- d$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(d$text) != 0) rows_text <- d$text %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(d$orders) != 0) rows_orders <- d$orders %>% dplyr::group_by(thesaurus_name, item_id) %>%
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      
      count_patients_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      count_patients_rows <- count_patients_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
          dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_patients_rows) != 0){
        count_patients_rows <- count_patients_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data <- data %>% dplyr::left_join(count_patients_rows, by = "item_id") %>% dplyr::rename(value = count_patients_rows)
      }
      
      if (nrow(count_patients_rows) == 0) data <- data %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data <- data %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", T ~ value))
      
    }
    
    if (category == "delete"){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("sub_delete_", id), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-thesaurus_items_deleted_pressed', this.id, {priority: 'event'})")))))
    }
    if (category == "plus_minus"){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_selected', this.id, {priority: 'event'})")),
          shiny::actionButton(paste0("remove_", id), "", icon = icon("minus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_removed', this.id, {priority: 'event'})")))))
    }
    if (category %in% c("plus_module", "plus_plugin")){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_selected', this.id, {priority: 'event'})")))))
    }
    if (grepl("plus_data_explorer", category)){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-data_explorer_item_selected', this.id, {priority: 'event'})")))))
    }
    if (category %in% c("colours_module", "colours_plugin")){
      
      colorCells <- list(
        list(id = "#EF3B2C", color = "#EF3B2C"),
        list(id = "#CB181D", color = "#CB181D"),
        list(id = "#7BCCC4", color = "#7BCCC4"),
        list(id = "#2B8CBE", color = "#2B8CBE"),
        list(id = "#5AAE61", color = "#5AAE61"),
        list(id = "#FFD92F", color = "#FFD92F"),
        list(id = "#000000", color = "#000000"))
      
      ns <- NS(module_id)
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        div(shiny.fluent::SwatchColorPicker.shinyInput(ns(paste0("colour_", id)), value = "#EF3B2C", colorCells = colorCells, columnCount = length(colorCells), 
            cellHeight = 15, cellWidth = 15#, cellMargin = 10
          )#,
          #style = "height:20px; padding:0px; margin-top:24px;"
        )
        ))
    }

    # Delete old cache
    
    if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus", "colours_module", "colours_plugin") | grepl("plus_data_explorer", category)){
      DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE id IN (
        SELECT c.id FROM cache c
        INNER JOIN thesaurus_items t ON c.link_id = t.id AND c.category = '", category, "'
        WHERE t.thesaurus_id = ", thesaurus_id, 
      ")")) -> query
    }
    
    # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
    if (category %in% c("count_patients_rows", "count_items_rows")){
      DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE id IN (
        SELECT c.id FROM cache c
        INNER JOIN thesaurus_items t ON c.link_id = t.id AND c.link_id_bis = ", datamart_id, " AND c.category = '", category, "'
        WHERE t.thesaurus_id = ", thesaurus_id, 
      ")")) -> query
    }
    
    # DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE category = '", category, "' AND link_id_bis = ", datamart_id)) -> query
    DBI::dbClearResult(query)

    # Get last row & insert new data
    last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM cache") %>% dplyr::pull())
    data_insert <-
      data %>%
      dplyr::transmute(
        category = !!category,
        link_id = id,
        link_id_bis = datamart_id,
        value,
        datetime = as.character(Sys.time()))
    data_insert$id <- seq.int(nrow(data_insert)) + last_row
    data_insert <- data_insert %>% dplyr::relocate(id)

    # Add data in cache table
    DBI::dbAppendTable(r$db, "cache", data_insert)
  }
  
  if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus") | grepl("plus_data_explorer", category)) data <- data %>% dplyr::rename(action = value)
  if (category %in% c("colours_module", "colours_plugin")) data <- data %>% dplyr::rename(colour = value)
  if (category %in% c("count_patients_rows", "count_items_rows")) data <- data %>% dplyr::rename(!!category := value) %>% dplyr::select(item_id, !!category)
  
  data
}

create_datatable_cache_new <- function(output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), i18n = R6::R6Class(), module_id = character(), 
  thesaurus_id = integer(), datamart_id = 0, category = character()){
  
  # Load join between our data and the cache
  
  # For action buttons (delete & plus_minus) & colours, don't use datamart_id / link_id_bis
  if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus", "colours_module", "colours_plugin") |
      grepl("plus_data_explorer", category)){
    sql <- glue::glue_sql(paste0(
      "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted, c.value ",
      "FROM thesaurus_items t ",
      "LEFT JOIN cache c ON c.link_id = t.id AND c.category = {category} ", 
      "WHERE t.thesaurus_id = {thesaurus_id} AND t.deleted IS FALSE ",
      "ORDER BY t.id"), .con = r$db)
    data <- DBI::dbGetQuery(r$db, sql)
  }
  
  # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
  if (category %in% c("count_patients_rows", "count_items_rows")){
    sql <- glue::glue_sql(paste0(
      "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted, c.value ",
      "FROM thesaurus_items t ",
      "LEFT JOIN cache c ON c.link_id = t.id AND c.link_id_bis = {datamart_id} AND c.category = {category} ",
      "WHERE t.thesaurus_id = {thesaurus_id} AND t.deleted IS FALSE ",
      "ORDER BY t.id"), .con = r$db)
    data <- DBI::dbGetQuery(r$db, sql)
  }
  
  # For thumbs_and_delete, search in thesaurus_items_mapping table
  if (category == "thumbs_and_delete"){
    sql <- glue::glue_sql(paste0("SELECT t.id, t.deleted, c.value ",
      "FROM thesaurus_items_mapping t ",
      "LEFT JOIN cache c ON c.link_id = t.id AND c.category = {category} ",
      "WHERE (t.thesaurus_id_1 IN ({thesaurus_id*}) OR t.thesaurus_id_2 IN ({thesaurus_id*})) ",
      "AND t.category = 'user_added_mapping' AND t.deleted IS FALSE ",
      "ORDER BY t.id"), .con = r$db)
    data <- DBI::dbGetQuery(r$db, sql)
  }
  
  # If there are missing data in the cache, reload cache 
  
  # reload_cache <- FALSE
  ids_to_keep <- data %>% dplyr::filter(!is.na(data$value) & data$value != "") %>% dplyr::pull(id)
  if (length(ids_to_keep) == 0) ids_to_keep <- c(0)
  
  # if (NA_character_ %in% data$value | "" %in% data$value) reload_cache <- TRUE
  
  # Reload cache if necessary
  if (data %>% dplyr::filter(is.na(data$value) | data$value == "") %>% nrow() > 0){
    
    print(paste0("reload cache ", category))
    
    # Reload data
    if (category %in% c("count_items_rows", "count_patients_rows")){
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} ",
      " AND deleted IS FALSE ORDER BY id"), .con = r$db)
      data_reload <- DBI::dbGetQuery(r$db, sql)
    }
    else if (category == "thumbs_and_delete"){
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items_mapping t WHERE ",
        "(t.thesaurus_id_1 IN ({thesaurus_id*}) OR t.thesaurus_id_2 IN ({thesaurus_id*})) ",
        "AND t.id NOT IN ({ids_to_keep*}) AND t.category = 'user_added_mapping' AND deleted IS FALSE ",
        "ORDER BY id"), .con = r$db)
      data_reload <- DBI::dbGetQuery(r$db, sql)
    }
    else {
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} ",
      "AND t.id NOT IN ({ids_to_keep*}) AND deleted IS FALSE ORDER BY id"), .con = r$db)
      data_reload <- DBI::dbGetQuery(r$db, sql)
    } 
    
    # Make action column, depending on category
    # If category is count_items_rows, add a count row column with number of rows by item in the datamart
    # If category is count_patients_rows, add a count row column with number of patients by item in the datamart
    # If category is delete, add a delete button only
    # If category is plus_minus, add plus and minus buttons
    # If category is thumbs_and_delete, add thumbs_up, thumbs_down and delete buttons
    
    if (category == "count_items_rows"){
      
      # Run datamart code
      # Reload r$datamarts so that datamart dropdown on sidenav is reset
      update_r_new(r = r, table = "datamarts")
      run_datamart_code_new(output = output, r = r, d = d, i18n = i18n, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      
      if (nrow(d$labs_vitals) != 0) rows_labs_vitals <- d$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(d$text) != 0) rows_text <- d$text %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(d$orders) != 0) rows_orders <- d$orders %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      
      # Merge rows count vars
      count_items_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      # Check if argument thesaurus_id corresponds to thesaurus_name in the data
      # Select thesaurus from r$thesaurus with thesaurus_id
      # Inner join with correspondance between thesaurus_name
      
      count_items_rows <- count_items_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
            dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_items_rows) != 0){
        count_items_rows <- count_items_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data_reload <- data_reload %>% dplyr::left_join(count_items_rows, by = "item_id") %>% dplyr::rename(value = count_items_rows)
      }
      if (nrow(count_items_rows) == 0) data_reload <- data_reload %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data_reload <- data_reload %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", TRUE ~ value))
    }
    
    if (category == "count_patients_rows"){
      
      # Reload r$datamarts so that datamart dropdown on sidenav is reset
      update_r_new(r = r, table = "datamarts")
      run_datamart_code_new(output = output, r = r, d = d, i18n = i18n, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      
      if (nrow(d$labs_vitals) != 0) rows_labs_vitals <- d$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(d$text) != 0) rows_text <- d$text %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(d$orders) != 0) rows_orders <- d$orders %>% dplyr::group_by(thesaurus_name, item_id) %>%
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      
      count_patients_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      count_patients_rows <- count_patients_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
            dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_patients_rows) != 0){
        count_patients_rows <- count_patients_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data_reload <- data_reload %>% dplyr::left_join(count_patients_rows, by = "item_id") %>% dplyr::rename(value = count_patients_rows)
      }
      
      if (nrow(count_patients_rows) == 0) data_reload <- data_reload %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data_reload <- data_reload %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", TRUE ~ value))
      
    }
    
    if (category == "delete"){
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("sub_delete_", id), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-thesaurus_items_deleted_pressed', this.id, {priority: 'event'})")))))
    }
    if (category == "plus_minus"){
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_selected', this.id, {priority: 'event'})")),
          shiny::actionButton(paste0("remove_", id), "", icon = icon("minus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_removed', this.id, {priority: 'event'})")))))
    }
    if (category %in% c("plus_module", "plus_plugin")){
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_selected', this.id, {priority: 'event'})")))))
    }
    if (category == "thumbs_and_delete"){
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("positive_eval_", id), "", icon = icon("thumbs-up"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_mapping_evaluated_positive', this.id, {priority: 'event'})"),
            style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;"),
          shiny::actionButton(paste0("negative_eval_", id), "", icon = icon("thumbs-down"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_mapping_evaluated_negative', this.id, {priority: 'event'})"),
            style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;"),
          shiny::actionButton(paste0("remove_", id), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_mapping_deleted_pressed', this.id, {priority: 'event'})"),
            style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;")
          )))
    }
    if (grepl("plus_data_explorer", category)){
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-data_explorer_item_selected', this.id, {priority: 'event'})")))))
    }
    if (category %in% c("colours_module", "colours_plugin")){
      
      colorCells <- list(
        list(id = "#EF3B2C", color = "#EF3B2C"),
        list(id = "#CB181D", color = "#CB181D"),
        list(id = "#7BCCC4", color = "#7BCCC4"),
        list(id = "#2B8CBE", color = "#2B8CBE"),
        list(id = "#5AAE61", color = "#5AAE61"),
        list(id = "#FFD92F", color = "#FFD92F"),
        list(id = "#000000", color = "#000000"))
      
      ns <- NS(module_id)
      data_reload <- data_reload %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        div(shiny.fluent::SwatchColorPicker.shinyInput(ns(paste0("colour_", id)), value = "#EF3B2C", colorCells = colorCells, columnCount = length(colorCells), 
          cellHeight = 15, cellWidth = 15#, cellMargin = 10
        )#,
          #style = "height:20px; padding:0px; margin-top:24px;"
        )
      ))
    }
    
    # Delete old cache
    
    if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus", "colours_module", "colours_plugin") | grepl("plus_data_explorer", category)){
      sql <- glue::glue_sql(paste0("DELETE FROM cache WHERE id IN (",
        "SELECT c.id FROM cache c ",
        "INNER JOIN thesaurus_items t ON c.link_id = t.id AND c.category = {category} AND t.id NOT IN ({ids_to_keep*}) ",
        "WHERE t.thesaurus_id = {thesaurus_id}", 
        ")", .con = r$db))
      DBI::dbSendStatement(r$db, sql) -> query
    }
    
    # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
    if (category %in% c("count_patients_rows", "count_items_rows")){
      sql <- glue::glue_sql(paste0("DELETE FROM cache WHERE id IN (",
        "SELECT c.id FROM cache c ",
        "INNER JOIN thesaurus_items t ON c.link_id = t.id AND c.link_id_bis = {datamart_id} AND c.category = {category} ",
        "WHERE t.thesaurus_id = {thesaurus_id}",
        ")"), .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
    }
    
    # For thumbs_and_delete, use thesaurus_items_mapping table
    if (category == "thumbs_and_delete"){
      sql <- glue::glue_sql(paste0("DELETE FROM cache WHERE id IN (",
        "SELECT c.id FROM cache c ",
        "INNER JOIN thesaurus_items_mapping t ON c.link_id = t.id AND c.category = {category} AND t.id NOT IN ({ids_to_keep*}) ",
        "AND (t.thesaurus_id_1 IN ({thesaurus_id*}) OR t.thesaurus_id_2 IN ({thesaurus_id*})) ",
        ")"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
    }
    
    # Merge new data & old data
    if (category %not_in% c("count_items_rows", "count_patients_rows")){
      if (nrow(data) > 0) data <- data %>% dplyr::filter(!is.na(data$value) & data$value != "") %>% dplyr::bind_rows(data_reload)
      else data <- data_reload
    } 
    else data <- data_reload
    
    # DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE category = '", category, "' AND link_id_bis = ", datamart_id)) -> query
    DBI::dbClearResult(query)
    
    # Get last row & insert new data
    last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM cache") %>% dplyr::pull())
    data_insert <-
      data_reload %>%
      dplyr::transmute(
        category = !!category,
        link_id = id,
        link_id_bis = datamart_id,
        value,
        datetime = as.character(Sys.time()))
    data_insert$id <- seq.int(nrow(data_insert)) + last_row
    data_insert <- data_insert %>% dplyr::relocate(id)
    
    # Add data in cache table
    DBI::dbAppendTable(r$db, "cache", data_insert)
  }
  
  if (category %in% c("delete", "plus_module", "plus_plugin", "plus_minus", "thumbs_and_delete") | grepl("plus_data_explorer", category)) data <- data %>% dplyr::rename(action = value)
  if (category %in% c("colours_module", "colours_plugin")) data <- data %>% dplyr::rename(colour = value)
  if (category %in% c("count_patients_rows", "count_items_rows")) data <- data %>% dplyr::rename(!!category := value) %>% dplyr::select(item_id, !!category)
  
  data
}

#' Update datatable
#' 
#' @param input Shiny input variable
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param dropdowns Dropdowns shown on datatable (character)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' update_settings_datatable(r = r, ns = ns, table = "datamarts", dropdowns = "data_source", language = "EN")
#' }

update_settings_datatable <- function(input, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), dropdowns = character(), language = "EN"){
  
  sapply(r[[table]] %>% dplyr::pull(id), function(id){
    sapply(dropdowns, function(dropdown){
      
      observer_name <- paste0(get_plural(word = dropdown), id)
      
      # Load the observer only once
      if (observer_name %not_in% r$loaded_observers){
        observeEvent(input[[observer_name]], {
          
          dropdown_table <- dropdown
          dropdown_input <- dropdown
          
          # If table is a module table, dropdown name is different
          if (table %in% c("patient_lvl_modules", "aggregated_modules")){
            dropdown_table <- switch(dropdown, 
              "patient_lvl_module_family" = "module_family", "aggregated_module_family" = "module_family",
              "patient_lvl_module" = "parent_module", "aggregated_module" = "parent_module")
          }
          
          # When we load a page, every dropdown triggers the event
          # Change temp variable only if new value is different than old value
          old_value <- r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]]
          new_value <- NA_integer_
          
          # If thesaurus, data_source_id can accept multiple values (converting to string)
          if (table == "thesaurus") new_value <- toString(as.integer(input[[paste0("data_sources", id)]]))
          if (table %in% c("data_sources", "datamarts", "studies", "subsets", "plugins", "users", "patient_lvl_modules", "aggregated_modules")){
            # if (!is.null(input[[paste0(get_plural(word = dropdown_input), id)]])) {
            #   if (length(input[[paste0(get_plural(word = dropdown_input), id)]]) == 0) new_value <- NA_integer_
              new_value <- coalesce2("int", input[[paste0(get_plural(word = dropdown_input), id)]])
            # }
          }
  
          if (length(new_value) > 0 & length(old_value) > 0){
            if (new_value == "" | is.na(new_value)){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- NA_integer_
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
            else if (old_value == "" | is.na(old_value) | new_value != old_value){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- new_value
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
          }
        }, ignoreInit = TRUE)
      
        # Keep trace that this observer has been loaded
        r$loaded_observers <- c(r$loaded_observers, observer_name)
      }
    })
  })
}

update_settings_datatable_new <- function(input, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), dropdowns = character(), i18n = R6::R6Class()){
  
  sapply(r[[table]] %>% dplyr::pull(id), function(id){
    sapply(dropdowns, function(dropdown){
      
      observer_name <- paste0(get_plural(word = dropdown), id)
      
      # Load the observer only once
      if (observer_name %not_in% r$loaded_observers){
        observeEvent(input[[observer_name]], {
          
          dropdown_table <- dropdown
          dropdown_input <- dropdown
          
          # If table is a module table, dropdown name is different
          if (table %in% c("patient_lvl_modules", "aggregated_modules")){
            dropdown_table <- switch(dropdown, 
              "patient_lvl_module_family" = "module_family", "aggregated_module_family" = "module_family",
              "patient_lvl_module" = "parent_module", "aggregated_module" = "parent_module")
          }
          
          # When we load a page, every dropdown triggers the event
          # Change temp variable only if new value is different than old value
          old_value <- r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]]
          new_value <- NA_integer_
          
          # If thesaurus, data_source_id can accept multiple values (converting to string)
          if (table == "thesaurus") new_value <- toString(as.integer(input[[paste0("data_sources", id)]]))
          if (table %in% c("data_sources", "datamarts", "studies", "subsets", "plugins", "users", "patient_lvl_modules", "aggregated_modules")){
            # if (!is.null(input[[paste0(get_plural(word = dropdown_input), id)]])) {
            #   if (length(input[[paste0(get_plural(word = dropdown_input), id)]]) == 0) new_value <- NA_integer_
            new_value <- coalesce2("int", input[[paste0(get_plural(word = dropdown_input), id)]])
            # }
          }
          
          if (length(new_value) > 0 & length(old_value) > 0){
            if (new_value == "" | is.na(new_value)){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- NA_integer_
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
            else if (old_value == "" | is.na(old_value) | new_value != old_value){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- new_value
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
          }
        }, ignoreInit = TRUE)
        
        # Keep trace that this observer has been loaded
        r$loaded_observers <- c(r$loaded_observers, observer_name)
      }
    })
  })
}

#' Save changes in datatable
#' 
#' @param output Shiny output variable
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param duplicates_allowed Are duplicates in the name column allowed (logical)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' save_settings_datatable_updates(output = output, r = r, ns = ns, table = "datamarts", language = "EN")
#' }

save_settings_datatable_updates <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), 
  table = character(), r_table = character(), duplicates_allowed = FALSE, language = "EN"){
  
  # Make sure there's no duplicate in names, if duplicates_allowed is set to FALSE
  
  # If r_table is different than table
  if (length(r_table) == 0) r_table <- table
  
  if (!duplicates_allowed){

    duplicates_name <- 0
    duplicates_display_order <- 0
    module_is_its_own_parent <- 0
    loop_over_modules <- 0
    
    # For modules tables (patient_lvl & aggregated, modules / modules_families / modules_elements)
    # Duplicates names are grouped (by family for modules, by module for modules elements)
    # It's the same with the display order
    
    if (grepl("modules", table)){
      if (table %in% c("patient_lvl_modules", "aggregated_modules")){
        
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(module_family_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(module_family_id, parent_module_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()

        # A module cannot be its own parent (infinite loop...)
        module_is_its_own_parent <- r[[paste0(table, "_temp")]] %>% dplyr::filter(id == parent_module_id) %>% nrow()

        if (module_is_its_own_parent == 0) loop_over_modules <- r[[paste0(table, "_temp")]] %>% dplyr::filter(!is.na(parent_module_id)) %>%
          dplyr::left_join(r[[paste0(table, "_temp")]] %>% dplyr::select(parent_module_id = id, parent_module_id_bis = parent_module_id), by = "parent_module_id") %>%
          dplyr::filter(id == parent_module_id_bis) %>% nrow()
      }
      
      if (table == "aggregated_modules_elements"){
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(module_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(module_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      }
      
      if (duplicates_name > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
      if (duplicates_display_order > 0) show_message_bar(output, 1, "modif_display_order_duplicates", "severeWarning", language)
      if (module_is_its_own_parent > 0) show_message_bar(output, 1, "module_cannot_be_its_own_parent", "severeWarning", language)
      if (loop_over_modules > 0) show_message_bar(output, 1, "module_loop_between_modules", "severeWarning", language)
      
    }
    
    # For other tables
    if (!grepl("modules", table)){
      
      if (table == "users") duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("username", tolower) %>%
          dplyr::group_by(username) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (table != "users") duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (duplicates_name > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
    }

    req(duplicates_name == 0, duplicates_display_order == 0, module_is_its_own_parent == 0, loop_over_modules == 0)
  }
  
  # Save changes in database
  
  ids_to_del <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  
  if (length(ids_to_del) == 0) show_message_bar(output, 2, "modif_saved", "success", language)
  
  req(length(ids_to_del) > 0)
  
  DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
  DBI::dbClearResult(query)
  
  # If action in columns, remove before insert into database (for thesaurus_items with cache system)
  # Same with count_items_rows (and count_patients_rows, always with count_items_rows)
  data <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  if ("action" %in% names(data)) data <- data %>% dplyr::select(-action)
  if ("count_items_rows" %in% names(data)) data <- data %>% dplyr::select(-count_items_rows, -count_patients_rows)
  
  DBI::dbAppendTable(r$db, table, data)
  
  # Reload r variable
  if (table == "thesaurus_items") r$datamart_refresh_thesaurus_items <- paste0(r$thesaurus_refresh_thesaurus_items, "_update")
  else update_r(r = r, table = table, language = language)
  
  # Notify user
  show_message_bar(output, 2, "modif_saved", "success", language)
}

save_settings_datatable_updates_new <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), 
  table = character(), r_table = character(), duplicates_allowed = FALSE, i18n = R6::R6Class()){
  
  # Make sure there's no duplicate in names, if duplicates_allowed is set to FALSE
  
  # If r_table is different than table
  if (length(r_table) == 0) r_table <- table
  
  if (!duplicates_allowed){
    
    duplicates_name <- 0
    duplicates_display_order <- 0
    module_is_its_own_parent <- 0
    loop_over_modules <- 0
    
    # For modules tables (patient_lvl & aggregated, modules / modules_families / modules_elements)
    # Duplicates names are grouped (by family for modules, by module for modules elements)
    # It's the same with the display order
    
    if (grepl("modules", table)){
      if (table %in% c("patient_lvl_modules", "aggregated_modules")){
        
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(module_family_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(module_family_id, parent_module_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        # A module cannot be its own parent (infinite loop...)
        module_is_its_own_parent <- r[[paste0(table, "_temp")]] %>% dplyr::filter(id == parent_module_id) %>% nrow()
        
        if (module_is_its_own_parent == 0) loop_over_modules <- r[[paste0(table, "_temp")]] %>% dplyr::filter(!is.na(parent_module_id)) %>%
          dplyr::left_join(r[[paste0(table, "_temp")]] %>% dplyr::select(parent_module_id = id, parent_module_id_bis = parent_module_id), by = "parent_module_id") %>%
          dplyr::filter(id == parent_module_id_bis) %>% nrow()
      }
      
      if (table == "aggregated_modules_elements"){
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(module_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(module_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      }
      
      if (duplicates_name > 0) show_message_bar_new(output, 1, "modif_names_duplicates", "severeWarning", i18n, ns = ns)
      if (duplicates_display_order > 0) show_message_bar_new(output, 1, "modif_display_order_duplicates", "severeWarning", i18n, ns = ns)
      if (module_is_its_own_parent > 0) show_message_bar_new(output, 1, "module_cannot_be_its_own_parent", "severeWarning", i18n, ns = ns)
      if (loop_over_modules > 0) show_message_bar_new(output, 1, "module_loop_between_modules", "severeWarning", i18n, ns = ns)
      
    }
    
    # For other tables
    if (!grepl("modules", table)){
      
      if (table == "users") duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("username", tolower) %>%
          dplyr::group_by(username) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (table != "users") duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (duplicates_name > 0) show_message_bar_new(output, 1, "modif_names_duplicates", "severeWarning", i18n, ns = ns)
    }
    
    req(duplicates_name == 0, duplicates_display_order == 0, module_is_its_own_parent == 0, loop_over_modules == 0)
  }
  
  # Save changes in database
  
  ids_to_del <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  
  if (length(ids_to_del) == 0) show_message_bar_new(output, 2, "modif_saved", "success", i18n, ns = ns)
  
  req(length(ids_to_del) > 0)
  
  DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
  DBI::dbClearResult(query)
  
  # If action in columns, remove before insert into database (for thesaurus_items with cache system)
  # Same with count_items_rows (and count_patients_rows, always with count_items_rows)
  data <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  if ("action" %in% names(data)) data <- data %>% dplyr::select(-action)
  if ("count_items_rows" %in% names(data)) data <- data %>% dplyr::select(-count_items_rows, -count_patients_rows)
  DBI::dbAppendTable(r$db, table, data)
  
  # Reload r variable
  r[[r_table]] <- r[[paste0(r_table, "_temp")]] %>% dplyr::select(-modified)
  # if (table == "thesaurus_items") r$datamart_refresh_thesaurus_items <- paste0(r$thesaurus_refresh_thesaurus_items, "_update")
  # else update_r_new(r = r, table = table, i18n = i18n)
  
  # Notify user
  show_message_bar_new(output, 2, "modif_saved", "success", i18n, ns = ns)
}

#' Render delete react
#' 
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' render_settings_delete_react(r = r, table = "datamarts")
#' }

render_settings_delete_react <- function(r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), language = "EN"){
  prefix <- ""
  if (table == "thesaurus_items") prefix <- "thesaurus_items_"
  
  dialogContentProps <- list(
    type = 0,
    title = translate(language, paste0(table, "_delete"), r$words),
    closeButtonAriaLabel = "Close",
    subText = translate(language, paste0(table, "_delete_subtext"), r$words)
  )
  shiny.fluent::Dialog(
    hidden = !r[[paste0(table, "_delete_dialog")]],
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", prefix, "hide_dialog', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "delete_confirmed")), text = translate(language, "delete", r$words)),
      shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "delete_canceled")), text = translate(language, "dont_delete", r$words))
    )
  )
}

render_settings_delete_react_new <- function(r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), i18n = R6::R6Class()){
  prefix <- ""
  if (table == "thesaurus_items") prefix <- "thesaurus_items_"
  
  dialogContentProps <- list(
    type = 0,
    title = i18n$t(paste0(table, "_delete")),
    closeButtonAriaLabel = "Close",
    subText = i18n$t(paste0(table, "_delete_subtext"))
  )
  shiny.fluent::Dialog(
    hidden = !r[[paste0(table, "_delete_dialog")]],
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", prefix, "hide_dialog', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "delete_confirmed")), text = i18n$t("delete")),
      shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "delete_canceled")), text = i18n$t("dont_delete"))
    )
  )
}

#' Delete a row in datatable
#' 
#' @param output Shiny output variable
#' @param r r Shiny reactive value used to communicate between modules
#' @param ns Shiny namespace
#' @param language Language used (character)
#' @param row_deleted ID of row to delete (integer) 
#' @param table Name of the table used (character)
#' @examples 
#' \dontrun{
#' delete_settings_datatable_row(output = output, r = r, ns = ns, language = "EN", row_deleted = 13, table = "datamarts")
#' }

delete_settings_datatable_row <- function(output, id = character(), r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN",
  link_id = integer(), category = character(), row_deleted = integer(), table = character()){
  
  # Close dialog box
  r[[paste0(table, "_delete_dialog")]] <- FALSE
  
  # Delete row in database
  DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted)) -> query
  DBI::dbClearResult(query)
  
  # Prefix, for patient_lvl & aggregated tables
  prefix <- ""
  if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
  if (grepl("aggregated", table)) prefix <- "aggregated"
  
  # If we delete a datamart, delete all studies & subsets associated
  if (table == "datamarts"){
    
    studies <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM studies WHERE datamart_id = ", row_deleted))
    
    if(nrow(studies) > 0){
      studies <- studies %>% dplyr::pull()
      
      DBI::dbSendStatement(r$db, paste0("UPDATE studies SET deleted = TRUE WHERE datamart_id = ", row_deleted)) -> query
      DBI::dbClearResult(query)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE subsets SET deleted = TRUE WHERE study_id IN (", paste(studies, collapse = ","), ")")) -> query
      DBI::dbClearResult(query)
      
      update_r(r = r, table = "studies")
      update_r(r = r, table = "subsets")
    }
  }
  
  # If we delete a study, delete all subsets associated
  if (table == "studies"){
    
    DBI::dbSendStatement(r$db, paste0("UPDATE subsets SET deleted = TRUE WHERE study_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = "subsets")
  }
  
  # If we delete a module family, delete all modules & modules elements associated
  if (table == paste0(prefix, "_modules_families")){
    
    modules <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM ", prefix, "_modules WHERE module_family_id = ", row_deleted)) %>% dplyr::pull()
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules SET deleted = TRUE WHERE module_family_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules_elements SET deleted = TRUE WHERE module_id IN (", paste(modules, collapse = ","), ")")) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = paste0(prefix, "_modules"))
    update_r(r = r, table = paste0(prefix, "_modules_elements"))
  }
  
  # If we delete a module, delete all modules elements associated
  if (table == paste0(prefix, "_modules")){
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules_elements SET deleted = TRUE WHERE module_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = paste0(prefix, "_modules_elements"))
  }
  
  # Update r vars
  # For thesaurus_items : the r variable is r$sub_thesaurus_items (from page settings / data management / thesaurus)
  # This page is the only place from where we can delete a thesaurus item
  # Distinct r$ names are to avoid conflict with other pages (settings / plugins & settings / modules)
  
  if (table == "thesaurus_items") r$thesaurus_refresh_thesaurus_items <- paste0(r$thesaurus_refresh_thesaurus_items, "_delete")
  
  if (table != "thesaurus_items") update_r(r = r, table = table, language = language)
  
  # Notification to user
  show_message_bar(output = output, id = 3, paste0(get_singular(word = table), "_deleted"), type ="severeWarning", language = language)
}

delete_settings_datatable_row_new <- function(output, id = character(), r = shiny::reactiveValues(), ns = shiny::NS(), i18n = R6::R6Class(),
  link_id = integer(), category = character(), row_deleted = integer(), table = character()){
  
  # Close dialog box
  r[[paste0(table, "_delete_dialog")]] <- FALSE
  
  # Delete row in database
  DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted)) -> query
  DBI::dbClearResult(query)
  
  # Prefix, for patient_lvl & aggregated tables
  prefix <- ""
  if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
  if (grepl("aggregated", table)) prefix <- "aggregated"
  
  # If we delete a datamart, delete all studies & subsets associated
  if (table == "datamarts"){
    
    studies <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM studies WHERE datamart_id = ", row_deleted))
    
    if(nrow(studies) > 0){
      studies <- studies %>% dplyr::pull()
      
      DBI::dbSendStatement(r$db, paste0("UPDATE studies SET deleted = TRUE WHERE datamart_id = ", row_deleted)) -> query
      DBI::dbClearResult(query)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE subsets SET deleted = TRUE WHERE study_id IN (", paste(studies, collapse = ","), ")")) -> query
      DBI::dbClearResult(query)
      
      update_r(r = r, table = "studies")
      update_r(r = r, table = "subsets")
    }
  }
  
  # If we delete a study, delete all subsets associated
  if (table == "studies"){
    
    DBI::dbSendStatement(r$db, paste0("UPDATE subsets SET deleted = TRUE WHERE study_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = "subsets")
  }
  
  # If we delete a module family, delete all modules & modules elements associated
  if (table == paste0(prefix, "_modules_families")){
    
    modules <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM ", prefix, "_modules WHERE module_family_id = ", row_deleted)) %>% dplyr::pull()
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules SET deleted = TRUE WHERE module_family_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules_elements SET deleted = TRUE WHERE module_id IN (", paste(modules, collapse = ","), ")")) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = paste0(prefix, "_modules"))
    update_r(r = r, table = paste0(prefix, "_modules_elements"))
  }
  
  # If we delete a module, delete all modules elements associated
  if (table == paste0(prefix, "_modules")){
    
    DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, "_modules_elements SET deleted = TRUE WHERE module_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = paste0(prefix, "_modules_elements"))
  }
  
  # Update r vars
  # For thesaurus_items : the r variable is r$sub_thesaurus_items (from page settings / data management / thesaurus)
  # This page is the only place from where we can delete a thesaurus item
  # Distinct r$ names are to avoid conflict with other pages (settings / plugins & settings / modules)
  
  if (table == "thesaurus_items") r$thesaurus_refresh_thesaurus_items <- paste0(r$thesaurus_refresh_thesaurus_items, "_delete")
  
  if (table != "thesaurus_items") update_r(r = r, table = table)
  
  # Notification to user
  show_message_bar_new(output = output, id = 3, paste0(get_singular(word = table), "_deleted"), type = "severeWarning", i18n = i18n)
}

#' Save options
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "datamart", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param data New data to store in options table (list)
#' @param language Language used
#' @examples
#' \dontrun{
#' data <- list()
#' data$show_only_aggregated_data <- TRUE
#' data$users_allowed_read <- c(1, 3, 4)
#' save_settings_options(output = output, r = r, id = "settings_datamart", category = "datamart", code_id_input = "edit_code_3",
#'   data = data, language = "EN")
#' }

save_settings_options <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), data = data, language = "EN", page_options = character()){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("options_") + 1, nchar(code_id_input)))
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  if (nrow(options) == 0) show_message_bar(output, 4, "modif_saved", "success", language)
  req (nrow(options) > 0)
  
  # Get options with page ID
  if (length(page_options) == 0) page_options <- get_page_options(id = id)
  
  if("show_only_aggregated_data" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value_num = ", as.numeric(data$show_only_aggregated_data), " WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
  }
  
  if ("users_allowed_read" %in% page_options){
    
    # Save users_allowed_read_group value (everybody or people_picker)
    # Don't need to delete each users allowed if you change from "choose people" to "everybody"
    option_id <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(id)
    query <- DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", data$users_allowed_read_group,
      "' WHERE id = ", option_id))
    DBI::dbClearResult(query)
    
    # The idea is to delete every rows of options for this module, and then reinsert one row per user
    # Get unique ID (peoplePicker can select twice a user, if he's already checked at the initiation of the input)
    
    # Delete all users allowed in the options table
    rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE id IN (", paste(rows_to_del, collapse = ","),")")) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
    
    # Add users in the selected list
    if (length(data$users_allowed_read) != 0){
      data$users_allowed_read <- unique(data$users_allowed_read)
      last_row <- max(r$options["id"])
      DBI::dbAppendTable(r$db, "options",
        tibble::tibble(id = (last_row + (1:length(data$users_allowed_read))), category = category, link_id = link_id,
          name = "user_allowed_read", value = "", value_num = as.numeric(data$users_allowed_read), creator_id = as.integer(r$user_id),
          datetime = as.character(Sys.time()), deleted = FALSE))
      update_r(r = r, table = "options", language = language)
    }
  }
  
  if ("markdown_description" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "markdown_description") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", stringr::str_replace_all(data$markdown_description, "'", "''"), "' WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
  }
  
  show_message_bar(output, 4, "modif_saved", "success", language)
}

save_settings_options_new <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), data = data, i18n = R6::R6Class(), page_options = character()){
  
  ns <- shiny::NS(id)
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("options_") + 1, nchar(code_id_input)))
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  if (nrow(options) == 0) show_message_bar_new(output, 4, "modif_saved", "success", i18n = i18n, ns = ns)
  req (nrow(options) > 0)
  
  # Get options with page ID
  if (length(page_options) == 0) page_options <- get_page_options(id = id)
  
  if("show_only_aggregated_data" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value_num = ", as.numeric(data$show_only_aggregated_data), " WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
  }
  
  if ("users_allowed_read" %in% page_options){
    
    # Save users_allowed_read_group value (everybody or people_picker)
    # Don't need to delete each users allowed if you change from "choose people" to "everybody"
    option_id <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(id)
    query <- DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", data$users_allowed_read_group,
      "' WHERE id = ", option_id))
    DBI::dbClearResult(query)
    
    # The idea is to delete every rows of options for this module, and then reinsert one row per user
    # Get unique ID (peoplePicker can select twice a user, if he's already checked at the initiation of the input)
    
    # Delete all users allowed in the options table
    rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE id IN (", paste(rows_to_del, collapse = ","),")")) -> query
    DBI::dbClearResult(query)
    
    # Add users in the selected list
    if (length(data$users_allowed_read) != 0){
      data$users_allowed_read <- unique(data$users_allowed_read)
      last_row <- max(r$options["id"])
      DBI::dbAppendTable(r$db, "options",
        tibble::tibble(id = (last_row + (1:length(data$users_allowed_read))), category = category, link_id = link_id,
          name = "user_allowed_read", value = "", value_num = as.numeric(data$users_allowed_read), creator_id = as.integer(r$user_id),
          datetime = as.character(Sys.time()), deleted = FALSE))
    }
  }
  
  for (field in c("markdown_description", "version", "author", "image", "description_fr", "description_en")){
    if (field %in% page_options){
      option_id <- options %>% dplyr::filter(name == field) %>% dplyr::pull(id)
      DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", stringr::str_replace_all(data[[field]], "'", "''"), "' WHERE id = ", option_id)) -> query
      DBI::dbClearResult(query)
    }
  }
  
  update_r_new(r = r, table = "options")
  
  show_message_bar_new(output, 4, "modif_saved", "success", i18n = i18n, ns = ns)
}

#' Save code edition
#' 
#' @description Save code in code table after editing it
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "datamart", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param edited_code New code, after editing it (character)
#' @param language Language used
#' @examples
#' \dontrun{
#' save_settings_code(output = output, r = r, id = "settings_datamart", category = "datamart", code_id_input = "edit_code_5",
#'   edited_code = "print('test code edition')", language = "EN")
#' }

save_settings_code <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), edited_code = character(), language = "EN"){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("edit_code_") + 1, nchar(code_id_input)))
  
  # Reload r$code before querying
  update_r(r = r, table = "options", language = language)
  code_id <- r$code %>% dplyr::filter(category == !!category, link_id == !!link_id) %>% dplyr::pull(id) %>% as.integer()
  
  # Replace ' with '' and store in the database
  DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(edited_code, "'", "''"), "' WHERE id = ", code_id)) -> query
  # DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(edited_code, "'", "''"), "' WHERE id = ", code_id)) -> query
  DBI::dbClearResult(query)
  r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
  
  # Notification to user
  show_message_bar(output, 4, "modif_saved", "success", language)
}

save_settings_code_new <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), edited_code = character(), i18n = R6::R6Class()){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("edit_code_") + 1, nchar(code_id_input)))
  
  # Reload r$code before querying
  # update_r(r = r, table = "options")
  code_id <- r$code %>% dplyr::filter(category == !!category, link_id == !!link_id) %>% dplyr::pull(id) %>% as.integer()
  
  # Replace ' with '' and store in the database
  edited_code <- stringr::str_replace_all(edited_code, "'", "''")
  sql <- glue::glue_sql("UPDATE code SET code = {edited_code} WHERE id = {code_id}", .con = r$db)
  query <- DBI::dbSendStatement(r$db, sql)
  DBI::dbClearResult(query)
  # r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
  r$code <- r$code %>% dplyr::mutate(
    code = dplyr::case_when(
      id == code_id ~ edited_code,
      TRUE ~ code
    ))
  
  # Notify user
  show_message_bar_new(output, 4, "modif_saved", "success", i18n = i18n, ns = shiny::NS(id))
}

#' Execute / test code after edition
#' 
#' @description Execute code entered in the ShinyAce editor\cr
#' For plugins page, UI & server code are displayed in distinct outputs (uiOutput for UI code, textOutput for server code to display error messages)
#' @param input variable from Shiny, used to execute UI & server code (plugins page)
#' @param output variable from Shiny, used to render messages on the message bar
#' @param session variable from Shiny, used to execute UI & server code (plugins page)
#' @param id ID of the current page / module
#' @param ns Shiny namespace
#' @param language language used (character)
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param edited_code New code, after editing it (character)
#' @param code_type For plugins page, code_type could be UI or server (character)
#' @param data A list containing dataframes / tibbles, if data need to be used in the evaluated code (list)
#' @examples 
#' \dontrun{
#'  execute_settings_code(output = output, r = r, edited_code = "print('test')")
#' }

execute_settings_code <- function(input, output, session, id = character(), ns = shiny::NS(), language = "EN", 
  r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  edited_code = character(), code_type = "", data = list()){

  result <- ""
  
  # If code is UI, execute it only
  if (code_type == "ui"){
    
    tryCatch(result <- eval(parse(text = edited_code)), error = function(e) stop(e), warning = function(w) stop(w))
  }
  
  # If code is server, capture the console output
  # Server is also used by R_console page
  if (code_type == "server"){
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = edited_code)), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # Display result
    paste(paste(captured_output), collapse = "\n") -> result
  }
  
  # If code is not UI or server, capture the console output after replacing %% values
  if (code_type %not_in% c("ui", "server")){
    
    # Replace %CODE% from code to real values
    code <- edited_code %>%
      stringr::str_replace_all("%datamart_id%", as.character(isolate(r$datamart_id))) %>%
      stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
      stringr::str_replace_all("%thesaurus_id%", as.character(isolate(r$thesaurus_id)))
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = as.character(code))), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # Display result
    # paste(captured_output, collapse = "\n") -> result
    paste(strwrap(captured_output), collapse = "\n") -> result
  }
  
  result
}

execute_settings_code_new <- function(input, output, session, id = character(), ns = shiny::NS(), i18n = R6::R6Class(), 
  r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  edited_code = character(), code_type = "", data = list()){
  
  result <- ""
  
  # If code is UI, execute it only
  if (code_type == "ui"){
    
    tryCatch(result <- eval(parse(text = edited_code)), error = function(e) stop(e), warning = function(w) stop(w))
  }
  
  # If code is server, capture the console output
  # Server is also used by R_console page
  if (code_type == "server"){
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = edited_code)), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # Display result
    paste(paste(captured_output), collapse = "\n") -> result
  }
  
  # If code is not UI or server, capture the console output after replacing %% values
  if (code_type %not_in% c("ui", "server")){
    
    # Replace %CODE% from code to real values
    code <- edited_code %>%
      stringr::str_replace_all("%datamart_id%", as.character(isolate(r$datamart_id))) %>%
      stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
      stringr::str_replace_all("%thesaurus_id%", as.character(isolate(r$thesaurus_id)))
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = as.character(code))), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # Display result
    # paste(captured_output, collapse = "\n") -> result
    paste(strwrap(captured_output), collapse = "\n") -> result
  }
  
  result
}

#' Get authorized data for a user
#'
#' @param r Shiny r reactive value, used to communicate between modules
#' @param table Name of the table the data comes from (character)
#' @param data If data is not r[[table]] (tibble / dataframe)

get_authorized_data <- function(r, table, data = tibble::tibble()){
  
  if (nrow(data) == 0) data <- r[[table]]
  
  # Merge with options
  options <- data %>% dplyr::inner_join(r$options %>% dplyr::filter(category == get_singular(table)) %>% 
    dplyr::select(option_id = id, link_id, option_name = name, value, value_num), by = c("id" = "link_id"))
  
  # Vector of authorized data
  data_allowed <- integer()
  
  # For each data row, select those the user has access
  sapply(unique(options$id), function(data_id){
    
    # Loop over each data ID
    
    users_allowed_read_group <- options %>% dplyr::filter(id == data_id, option_name == "users_allowed_read_group")
    users_allowed_read <- options %>% dplyr::filter(id == data_id, option_name == "user_allowed_read")
    
    if (users_allowed_read_group %>% dplyr::pull(value) == "everybody") data_allowed <<- c(data_allowed, data_id)
    else if (nrow(users_allowed_read %>% dplyr::filter(value_num == r$user_id)) > 0) data_allowed <<- c(data_allowed, data_id)
    
  })
  
  # Select authorized data
  data %>% dplyr::filter(id %in% data_allowed)
}

show_hide_cards <- function(r = shiny::reactiveValues(), session, input, table = character(), id = character(), cards = character()){
    
  # If user has access, show or hide card when Pivot is clicked
  observeEvent(input$current_tab, {
    
    if (length(table > 0)) card_user_access <- paste0(table, "_", input$current_tab)
    else card_user_access <- input$current_tab
    
    sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
    sapply(cards %>% setdiff(., input$current_tab), function(card) shinyjs::hide(paste0(card, "_forbidden")))
    
    if (card_user_access %in% r$user_accesses) shinyjs::show(input$current_tab)
    else shinyjs::show(paste0(input$current_tab, "_forbidden"))
  })
}

#' Delete element
#' 

delete_element <- function(r = shiny::reactiveValues(), session, input, output, ns = shiny::NS(), language = "EN",
  delete_prefix = character(), dialog_title = character(), dialog_subtext = character(),
  react_variable = character(), table = character(), id_var_sql = character(), id_var_r = character(),
  delete_message = character(), reload_variable = character(), information_variable = character(), translation = TRUE){
  
  delete_variable <- paste0(delete_prefix, "_open_dialog")

  r[[delete_variable]] <- FALSE

  if (translation){
    dialog_title <- translate(language, dialog_title, r$words)
    dialog_subtext <- translate(language, dialog_subtext, r$words)
  }

  dialog_content <- list(
    type = 0,
    title = dialog_title,
    closeButtonAriaLabel = "Close",
    subText = dialog_subtext
  )
  
  output[[react_variable]] <- shiny.fluent::renderReact({

    shiny.fluent::Dialog(
      hidden = !r[[delete_variable]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", delete_prefix, "_hide_dialog', Math.random()); }")),
      dialogContentProps = dialog_content,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(delete_prefix, "_delete_confirmed")), text = translate(language, "delete", r$words)),
        shiny.fluent::DefaultButton.shinyInput(ns(paste0(delete_prefix, "_delete_canceled")), text = translate(language, "dont_delete", r$words))
      )
    )
  })
  
  # Whether to close or not delete dialog box
  observeEvent(input[[paste0(delete_prefix, "_hide_dialog")]], r[[delete_variable]] <- FALSE)
  observeEvent(input[[paste0(delete_prefix, "_delete_canceled")]], r[[delete_variable]] <- FALSE)
  
  # When the delete is confirmed...
  observeEvent(input[[paste0(delete_prefix, "_delete_confirmed")]], {

    r[[delete_variable]] <- FALSE
    # Delete row in DB table
    sql <- glue::glue_sql("UPDATE {`table`} SET deleted = TRUE WHERE {`id_var_sql`} = {r[[id_var_r]]}" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)

    update_r(r = r, table = table)

    # Notify user
    show_message_bar(output = output, id = 4, delete_message, type ="severeWarning", language = language)

    # Activate reload variable
    r[[reload_variable]] <- Sys.time()
    
    # Information variable
    if (length(information_variable) > 0) r[[information_variable]] <- r[[id_var_r]]

  })
}

delete_element_new <- function(r = shiny::reactiveValues(), session, input, output, ns = shiny::NS(), i18n = R6::R6Class(),
  delete_prefix = character(), dialog_title = character(), dialog_subtext = character(),
  react_variable = character(), table = character(), r_table = character(), id_var_sql = character(), id_var_r = character(),
  delete_message = character(), reload_variable = character(), information_variable = character(), translation = TRUE){
  
  delete_variable <- paste0(delete_prefix, "_open_dialog")
  
  r[[delete_variable]] <- FALSE
  
  if (translation){
    dialog_title <- i18n$t(dialog_title)
    dialog_subtext <- i18n$t(dialog_subtext)
  }
  
  dialog_content <- list(
    type = 0,
    title = dialog_title,
    closeButtonAriaLabel = "Close",
    subText = tagList(dialog_subtext, br(), br())
  )
  
  output[[react_variable]] <- shiny.fluent::renderReact({
    
    shiny.fluent::Dialog(
      hidden = !r[[delete_variable]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", delete_prefix, "_hide_dialog', Math.random()); }")),
      dialogContentProps = dialog_content,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(delete_prefix, "_delete_confirmed")), text = i18n$t("delete")),
        shiny.fluent::DefaultButton.shinyInput(ns(paste0(delete_prefix, "_delete_canceled")), text = i18n$t("dont_delete"))
      )
    )
  })
  
  # Whether to close or not delete dialog box
  observeEvent(input[[paste0(delete_prefix, "_hide_dialog")]], r[[delete_variable]] <- FALSE)
  observeEvent(input[[paste0(delete_prefix, "_delete_canceled")]], r[[delete_variable]] <- FALSE)
  
  # When the deletion is confirmed...
  observeEvent(input[[paste0(delete_prefix, "_delete_confirmed")]], {
    
    r[[delete_variable]] <- FALSE
    
    # Delete row in DB table and associated tables
    sql <- glue::glue_sql("UPDATE {`table`} SET deleted = TRUE WHERE {`id_var_sql`} IN ({r[[id_var_r]]*})" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    
    sql <- glue::glue_sql("UPDATE options SET deleted = TRUE WHERE category = {table} AND link_id IN ({r[[id_var_r]]*})" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    
    sql <- glue::glue_sql("UPDATE code SET deleted = TRUE WHERE category = {table} AND link_id IN ({r[[id_var_r]]*})" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    
    if (length(r_table) > 0) r[[r_table]] <- r[[r_table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]])
    else r[[table]] <- r[[table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]])
    
    # # Notify user
    show_message_bar_new(output = output, id = 4, delete_message, type ="severeWarning", i18n = i18n, ns = ns)

    # Activate reload variable
    if (length(reload_variable) > 0) r[[reload_variable]] <- Sys.time()

    # Information variable
    if (length(information_variable) > 0) r[[information_variable]] <- r[[id_var_r]]
    
  })
}