##########################################
# Add new data                           #
##########################################

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
    if (table %in% c("patient_lvl_modules", "aggregated_modules")) sql <- 
        glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE AND module_family_id = {data$module_family}", .con = r$db)
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
      tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
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
    value <- paste0("- **Version** : 0.0.1\n- **Libraries** : *put libraries needed here*\n- **Data allowed** : *put data allowed here*\n",
      "- **Previous plugin needed first** : *put previous plugins needed here*\n\n*Put full description here*")
    
    new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row_options + 1, "plugin", last_row + 1, "markdown_description", value, NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 2, "plugin", last_row + 1, "users_allowed_read_group", "everybody", 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_options + 3, "plugin", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE)
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
                   'patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at("patient_id", as.integer)\n\n',
                   'add_patients_to_subset(output = output, r = r, patients = patients, subset_id = %subset_id%)\n\n',
                   'update_r(r = r, table = "subset_patients")')
    new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row_code + 1, "subset", last_row_subsets + 1, code, as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_code + 2, "subset", last_row_subsets + 2, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
      last_row_code + 3, "subset", last_row_subsets + 3, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_code)
    add_log_entry(r = r, category = paste0("code", " - ", translate(language, "insert_new_data", r$words)), name = translate(language, "sql_query", r$words), value = toString(new_code))

    # Update r$options, r$code & r$subsets
    update_r(r, "options", language)
    update_r(r, "subsets", language)
    update_r(r, "code", language)

    # Run code to add patients in the subset. Get datamart_id first.
    
    r$patients <- tibble::tibble()
    
    datamart_id <- r$studies %>% dplyr::filter(id == last_row) %>% dplyr::pull(datamart_id)
    
    tryCatch(run_datamart_code(output = output, r = r, datamart_id = datamart_id),
      error = function(e) show_message_bar(output = output, id = 2, message = "error_loading_datamart", type = "severeWarning", language = language),
      warning = function(w) show_message_bar(output = output, id = 2, message = "error_loading_datamart", type = "severeWarning", language = language))
    
    if (nrow(r$patients) == 0) show_message_bar(output = output, id = 2, message = "error_loading_datamart", type = "severeWarning", language = language)
    if (nrow(r$patients) != 0){
      tryCatch({
        patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)
        add_patients_to_subset(output, r, patients, last_row_subsets + 1)
        update_r(r = r, table = "subset_patients")
      }, error = function(e) show_message_bar(output = output, id = 2, message = "error_adding_patients_to_subset", type = "severeWarning", language = language),
         warning = function(w) error = function(e) show_message_bar(output = output, id = 2, message = "error_adding_patients_to_subset", type = "severeWarning", language = language))
    }
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
  shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
  shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
  shiny.fluent::updateToggle.shinyInput(session, "datatable_card_toggle", value = TRUE)

  show_message_bar(output = output, id = 1, message = paste0(get_singular(table), "_added"), type = "success", language = language)

  # Reset textfields
  if (table == "users") sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
  else sapply(c("name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
}

##########################################
# Generate datatable                     #
##########################################

#' Render management datatable
#' 
#' @description Renders a datatable (from library DT)
#' @details 
#' NB : don't forget Action column in the col_names argument.\cr\cr
#' For more informations about DT, see https://datatables.net/manual/options.\cr
#' See DOM documentation here : https://datatables.net/reference/option/dom\cr
#' See columnDefs doc here : https://datatables.net/reference/option/columnDefs
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param ns Shiny namespace
#' @param language Language used (charater)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param output_name Name of the datatable output
#' @param col_names A character vector containing colnames, already translated (character)
#' @param table Name of a table of database (character)
#' @param dropdowns Character vector with names of the dropdowns available in the datatable (character)
#' @param action_buttons Character vector with action_buttons needed (character)
#' @param datatable_dom Character containing DOM code for the datatable (character)
#' @param page_length Page length of the datatable, default to 10 rows (integer)
#' @param start Which page display (used when we save datatable state), default to 1 (integer)
#' @param editable_cols Which cols are editable (character vector)
#' @param sortable_cols Which cols are sortable (character vector)
#' @param centered_cols Which cols are centered (character vector)
#' @param filter If TRUE, we can filter we search box each column (logical)
#' @param factorize_cols Which columns are factorized (to be filtered with a dropdown) (character)
#' @param searchable_cols If filter is TRUE, choose which columns are searchable (character)
#' @param column_widths Columns widths (named character vector)
#' @examples 
#' \dontrun{
#' data <- tibble::tribble(~id, ~name, ~description, ~data_source_id,
#'   2, "Name of the datamart", "Description of the datamart", 3)
#'   
#'
#' col_names <- c("ID", "Name", "Description", "Data source ID", "Action")
#' action_buttons <- c("delete", "edit_code")
#' editable_cols <- c("id", "name")
#' sortable_cols <- "id"
#' centered_cols <- "id"
#' filter <- TRUE
#' searchable_cols <- c("name", "description")
#' column_widths <- c("name" = "200px", "description" = "300px")
#'
#' render_settings_datatable(
#'   output = output, r = r, 
#'   ns = NS("settings_datamart"), language = "EN",
#'   id = "settings_datamart",
#'   output_name = "management_datatable"
#'   col_names = col_names, 
#'   table = "datamarts", 
#'   dropdowns = "data_source",
#'   action_buttons = action_buttons, 
#'   datatable_dom = "<'top'ft>",
#'   page_length = 20, 
#'   start = 1, 
#'   editable_cols = editable_cols, 
#'   sortable_cols = sortable_cols,
#'   centered_cols = centered_cols, 
#'   filter = filter,
#'   searchable_cols = searchable_cols,
#'   column_widths = column_widths)
#' }

render_settings_datatable <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN", id = character(),
  output_name = character(), col_names = character(), table = character(), dropdowns = character(), action_buttons = character(),
  datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = 10, start = 1,
  editable_cols = character(), sortable_cols = character(), centered_cols = character(), searchable_cols = character(), 
  filter = FALSE, factorize_cols = character(), column_widths = character()
){
  
  # Translation for datatable
  dt_translation <- list(
    paginate = list(previous = translate(language, "DT_previous_page", r$words), `next` = translate(language, "DT_next_page", r$words)),
    search = translate(language, "DT_search", r$words),
    lengthMenu = translate(language, "DT_length", r$words),
    emptyTable = translate(language, "DT_empty", r$words))
  
  # Load temp data
  data <- r[[paste0(table, "_temp")]]
  
  # If no row in dataframe, stop here
  if (nrow(data) == 0) return({
    data <- tibble::tribble(~id, ~name, ~description,  ~datetime)
    names(data) <- c(translate(language, "id", r$words), translate(language, "name", r$words), translate(language, "description", r$words), translate(language, "datetime", r$words))
    output[[output_name]] <- DT::renderDT(data, options = list(dom = 'tp'))
  })
  
  # Add module family column for modules elements
  if (grepl("modules_elements", table)){
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
    if (grepl("aggregated", table)) prefix <- "aggregated"
    data <- data %>% dplyr::left_join(r[[paste0(prefix, "_modules")]] %>% 
      dplyr::select(module_id = id, module_family_id), by = "module_id") %>% dplyr::relocate(module_family_id, .after = name)
  }
  
  
  # Add a column action in the DataTable
  # Action column is already loaded for thesaurus_items (cache system)
  if (!grepl("thesaurus_items", table) & length(action_buttons) != 0) data["action"] <- NA_character_

  # Drop deleted column & modified column : we don't want to show them in the datatable
  if (nrow(data) != 0) data <- data %>% dplyr::select(-deleted, -modified)
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)

  # Transform dropdowns columns in the dataframe to character
  if (length(dropdowns) != 0) lapply(names(dropdowns), function(col_name) data %>% dplyr::mutate_at(col_name, as.character) ->> data)

  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name

  # Loop over data only if necessary (eg not necessary for thesaurus_items, with a lot of rows...)
  # Not necessary if no dropdowns, no action_buttons & no creator_id col
  if (!grepl("thesaurus_items", table) & (length(dropdowns) != 0 | length(action_buttons) != 0 | "creator_id" %in% names(data))){
  
    for (i in 1:nrow(data)){

      #############
      # DROPDOWNS #
      #############

      if (length(dropdowns) != 0){
        lapply(names(dropdowns), function(name){

          # Particularity with thesaurus, data_source_id column can contains multiple values (multiSelect = TRUE)
          # We have to split data_source_id column, to have an integer vector (saved with collapse by commas)

          # name here is like "data_source_id"
          # dropdowns[name] here is like "data_sources"
          # so r[[dropdowns[[name]]]] is like r$data_sources, var containing data_sources data

          if (id == "settings_thesaurus"){
            value <- NULL
            if (length(data[[i, name]] > 0)){
              if (!(TRUE %in% grepl("[a-zA-Z]", stringr::str_split(data[[i, name]], ", ") %>% unlist()))){
                value <- stringr::str_split(data[[i, name]], ", ") %>% unlist() %>% as.integer()
              }
            }
            data[[i, name]] <<- as.character(
              div(
                shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[[name]], data[[i, "id"]])),
                  options = convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = FALSE),
                  value = value,
                  multiSelect = TRUE),
                  onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[[name]], data[[i, "id"]]), "', {priority: 'event'})"),
                  style = "width:200px")
            )
          }

          else {
            null_value <- FALSE
            if (name == "parent_module_id") null_value <- TRUE
            
            # For dropdown parent_module in patient_lvl & aggregated_modules, need to select only modules depending on the same module family
            
            options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = null_value) 
            
            if (dropdowns[[name]] %in% c("patient_lvl_modules", "aggregated_modules")){
              options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]] %>% dplyr::filter(module_family_id == data[[i, "module_family_id"]]),
                key_col = "id", text_col = "name", null_value = null_value)
            }
            else options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = null_value)
            
            data[i, name] <<- as.character(
              div(
                # So ID is like "data_sources13" if ID = 13
              shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[[name]], data[[i, "id"]])),
                # To get options, convert data var to tibble (convert r$data_sources to list)
                options = options,
                # value is an integer, the value of the column like "data_source_id"
                value = as.integer(data[[i, name]])),
                # On click, we set variable "dropdown_updated" to the ID of the row (in our example, 13)
                onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[[name]], data[[i, "id"]]), "', {priority: 'event'})"),
                style = "width:200px")
            )
          }

        })
      }

      ##################
      # ACTION BUTTONS #
      ##################

      # Action buttons : if in action_buttons vector, add action button
      actions <- tagList()

      # Add options button
      if ("options" %in% action_buttons){
        actions <- tagList(actions,
          shiny::actionButton(paste0("options_", data[i, "id"]), "", icon = icon("cog"),
            onclick = paste0("Shiny.setInputValue('", id, "-options", "', this.id, {priority: 'event'})")), "")}

      # Add edit code button
      if ("edit_code" %in% action_buttons){
        actions <- tagList(actions,
          shiny::actionButton(paste0("edit_code_", data[i, "id"]), "", icon = icon("file-code"),
            onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}

      # Add sub datatable button
      if ("sub_datatable" %in% action_buttons){
        actions <- tagList(actions,
          shiny::actionButton(paste0("sub_datatable_", data[i, "id"]), "", icon = icon("table"),
            onclick = paste0("Shiny.setInputValue('", id, "-sub_datatable", "', this.id, {priority: 'event'})")), "")}

      # Add delete button
      if ("delete" %in% action_buttons){

        # If row is deletable (we havn't made a function argument for deletable or not, only default subsets are not deletable)
        # Could be changed later
        
        delete <- shiny::actionButton(paste0("delete_", data[i, "id"]), "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})"))
        
        # Default subsets are not deletable
        if (id == "settings_subsets"){
          if (data[i, "name"] %in% c(translate("EN", "subset_all_patients", r$words), translate("EN", "subset_included_patients", r$words), translate("EN", "subset_excluded_patients", r$words),
                                     translate("FR", "subset_all_patients", r$words), translate("FR", "subset_included_patients", r$words), translate("FR", "subset_excluded_patients", r$words))) delete <- ""
        }

        actions <- tagList(actions, delete)
      }

      # Update action column in dataframe
      if (length(action_buttons) != 0) data[i, "action"] <- as.character(div(actions))

      ################
      # CREATOR NAME #
      ################

      if ("creator_id" %in% names(data)){
        if (nrow(r$users %>% dplyr::filter(id == data[[i, "creator_id"]])) > 0){
          data[i, "creator_id"] <-
            r$users %>% dplyr::filter(id == data[[i, "creator_id"]]) %>%
            dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
            dplyr::pull(creator)
        }
        else data[i, "creator_id"] <- translate(language, "deleted_user", r$words)
      }

      # Get names for other columns if there are not dropdowns

      cols <- c("data_source_id" = "data_sources", "datamart_id" = "datamarts", "study_id" = "studies", "module_type_id" = "module_types")
      sapply(names(cols), function(name){
        if (name %in% names(data) & name %not_in% names(dropdowns)){
          row_id <- data[[i, name]]
          if (length(row_id) > 0) result <- r[[cols[[name]]]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data[[i, name]] <<- result
        }
      })
      
      cols <- c("module_family_id" = "modules_families", "module_id" = "modules", "plugin_id" = "plugins")
      sapply(names(cols), function(name){
        if (name %in% names(data) & name %not_in% names(dropdowns)){
          if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
          if (grepl("aggregated", table)) prefix <- "aggregated_"
          if (name == "plugin_id") prefix <- ""

          row_id <- data[[i, name]]
          if (length(row_id) > 0) result <- r[[paste0(prefix, cols[[name]])]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data[[i, name]] <<- result
        }
      })
    }
  }
  
  
  # Remove some cols
  
  # If page is plugins, remove column description from datatable (it will be editable from datatable row options edition)
  # /!\ Careful : it changes the index of columns, use to update informations directy on datatable
  if (table == "plugins") data <- data %>% dplyr::select(-description)
  if (grepl("thesaurus_items", table)) data <- data %>% dplyr::select(-id, -thesaurus_id, -datetime)
  if (grepl("modules_elements", table)){
    
    if (grepl("patient_lvl", table)){
      prefix <- "patient_lvl"
      data <- data %>% dplyr::select(-id, -group_id, -thesaurus_item_id, -thesaurus_item_colour, -display_order, -creator_id, -datetime)
    }
    if (grepl("aggregated", table)){
      prefix <- "aggregated"
      data <- data %>% dplyr::select(-id, -group_id, -creator_id, -datetime)
    }
  }
  
  
  # Which columns are non editable

  cols <- c(1:length(names(data))) - 1
  editable_cols_vec <- integer()
  sapply(editable_cols, function(col){
    editable_cols_vec <<- c(editable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_editable_cols_vec <- cols[!cols %in% editable_cols_vec]

  # Which columns are non sortable
  sortable_cols_vec <- integer()
  sapply(sortable_cols, function(col){
    sortable_cols_vec <<- c(sortable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
  
  # Which cols are centered
  centered_cols_vec <- integer()
  sapply(centered_cols, function(col){
    centered_cols_vec <<- c(centered_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are searchable
  searchable_cols_vec <- integer()
  sapply(searchable_cols, function(col){
    searchable_cols_vec <<- c(searchable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_searchable_cols_vec <- cols[!cols %in% searchable_cols_vec]
  
  # If filter is TRUE
  if (filter) filter_list <- list(position = "top")
  if (!filter) filter_list <- list()

  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
  
  # Add searchable cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(searchable = FALSE, targets = non_searchable_cols_vec))
  
  # Transform searchable cols to factor
  # Don't factorize name & description cols, except for subsets (name are usually included / excluded / all patients...)
  sapply(factorize_cols, function(col) data <<- data %>% dplyr::mutate_at(col, as.factor))

  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable

  output[[output_name]] <- DT::renderDT(
    # Data
    data,

    # Options of the datatable
    options = list(
      dom = datatable_dom,
      stateSave = TRUE, stateDuration = 30,
      pageLength = page_length, displayStart = start,
      columnDefs = column_defs,
      language = dt_translation
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    
    # Default options
    rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,

    # Javascript code allowing to have dropdowns & actionButtons on the DataTable
    callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
      var $this = $(this.node());
      $this.attr('id', this.data()[0]);
      $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
}

##########################################
# Create a cache for action buttons col  #
##########################################

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
  if (category %in% c("delete", "plus_minus", "colours")){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted, c.value
      FROM thesaurus_items t
      LEFT JOIN cache c ON c.link_id = t.id AND c.category = '", category, "'
      WHERE t.thesaurus_id = ", thesaurus_id, " AND t.deleted IS FALSE
      ORDER BY t.id")) %>% tibble::as_tibble()
  }
  # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
  if (category %in% c("count_patients_rows", "count_items_rows")){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted, c.value
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
      
      if (nrow(r$labs_vitals) != 0) rows_labs_vitals <- r$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(r$text) != 0) rows_text <- r$text %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(r$orders) != 0) rows_orders <- r$orders %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      
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
      
      if (nrow(r$labs_vitals) != 0) rows_labs_vitals <- r$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(r$text) != 0) rows_text <- r$text %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(r$orders) != 0) rows_orders <- r$orders %>% dplyr::group_by(thesaurus_name, item_id) %>%
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
    if (category == "colours"){
      
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
        shiny.fluent::SwatchColorPicker.shinyInput(ns(paste0("colour_", id)), value = "#EF3B2C", colorCells = colorCells, columnCount = length(colorCells), 
          cellHeight = 18, cellWidth = 18#, cellMargin = 10
        )))
    }

    # Delete old cache
    DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE category = '", category, "' AND link_id_bis = ", datamart_id)) -> query
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
  
  if (category %in% c("delete", "plus_minus")) data <- data %>% dplyr::rename(action = value)
  if (category == "colours") data <- data %>% dplyr::rename(colour = value)
  if (category %in% c("count_patients_rows", "count_items_rows")) data <- data %>% dplyr::rename(!!category := value) %>% dplyr::select(item_id, !!category)
  
  data
}


##########################################
# Save updates in datatable              #
##########################################

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

save_settings_datatable_updates <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(),
  duplicates_allowed = FALSE, language = "EN"){
  
  # Make sure there's no duplicate in names, if duplicates_allowed is set to FALSE
  
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
      
      if (table != "users") duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (table == "users") duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("username", tolower) %>%
          dplyr::group_by(username) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      
      if (duplicates_name > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
    }

    req(duplicates_name == 0, duplicates_display_order == 0, module_is_its_own_parent == 0, loop_over_modules == 0)
  }
  
  # Save changes in database
  
  if (table == "thesaurus_items") r_table <- "sub_thesaurus_items"
  else r_table <- table
  
  ids_to_del <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
  DBI::dbClearResult(query)
  
  # If action in columns, remove before insert into database (for thesaurus_items with cache system)
  # Same with count_items_rows (and count_patients_rows, always with count_items_rows)
  data <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  if ("action" %in% names(data)) data <- data %>% dplyr::select(-action)
  if ("count_items_rows" %in% names(data)) data <- data %>% dplyr::select(-count_items_rows, -count_patients_rows)
  
  DBI::dbAppendTable(r$db, table, data)
  
  # Reload r variable
  if (table == "thesaurus_items") r$thesaurus_refresh_thesaurus_items <- paste0(r$thesaurus_refresh_thesaurus_items, "_update")
  else update_r(r = r, table = table, language = language)
  
  # Notification to user
  show_message_bar(output, 2, "modif_saved", "success", language)
}
  
##########################################
# Delete a row in datatable              #
##########################################

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
    
    studies <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM studies WHERE datamart_id = ", row_deleted)) %>% dplyr::pull()
    
    DBI::dbSendStatement(r$db, paste0("UPDATE studies SET deleted = TRUE WHERE datamart_id = ", row_deleted)) -> query
    DBI::dbClearResult(query)
    
    DBI::dbSendStatement(r$db, paste0("UPDATE subsets SET deleted = TRUE WHERE study_id IN (", paste(studies, collapse = ","), ")")) -> query
    DBI::dbClearResult(query)
    
    update_r(r = r, table = "studies")
    update_r(r = r, table = "subsets")
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

##########################################
# Save updates of options                #
##########################################  

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
  code_id_input = integer(), data = data, language = "EN"){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("options_") + 1, nchar(code_id_input)))
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  # Get options with page ID
  page_options <- get_page_options(id = id)
  
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

##########################################
# Save edition of the code               #
##########################################  

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

##########################################
# Execute the code in edit_code          #
########################################## 

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

execute_settings_code <- function(input, output, session, id = character(), ns = shiny::NS(), language = "EN", r = shiny::reactiveValues(), 
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
    paste(captured_output, collapse = "\n") -> result
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
    paste(captured_output, collapse = "\n") -> result
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

#' Show or hide cards
#' 
#' 

show_hide_cards <- function(r = shiny::reactiveValues(), session, input, table = character(), id = character(), toggles = character()){
  
  sapply(toggles, function(toggle){
    
    # Reset toggles when we load the page (restart reactivity, sometimes frozen)
    observe({
      shiny.router::get_query_param()
      shiny.fluent::updateToggle.shinyInput(session, paste0(toggle, "_toggle"), value = FALSE)
      # If this toggle was activated, reactivate it
      if (paste0(id, toggle) %in% isolate(r$activated_toggles)) shiny.fluent::updateToggle.shinyInput(session, paste0(toggle, "_toggle"), value = TRUE)
    })
    
    # If user has no access, hide card
    if (length(table > 0)) toggle_user_access <- paste0(table, "_", toggle)
    else toggle_user_access <- toggle
        
    observeEvent(r$user_accesses, if (toggle_user_access %not_in% r$user_accesses) shinyjs::hide(toggle)) 
    
    # If user has access, show or hide card when toggle is clicked
    observeEvent(input[[paste0(toggle, "_toggle")]], {
      if (toggle_user_access %in% r$user_accesses){
        if(input[[paste0(toggle, "_toggle")]]){
          shinyjs::show(toggle) 
          # Save that this toggle is activated
          r$activated_toggles <- c(r$activated_toggles, paste0(id, toggle))
        }
        else{
          shinyjs::hide(toggle)
          # Save that this toggle is disabled
          r$activated_toggles <- r$activated_toggles[r$activated_toggles != paste0(id, toggle)]
        }
      }
    })
  })
}