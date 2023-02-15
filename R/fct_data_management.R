#' Run datamart code 
#'
#' @description Runs datamart code
#' @details ...
#' @param output Output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param datamart_id ID of datamart containing the code (integer)
#' @param language language used for error / warning messages (character, default = "EN")
#' @examples 
#' \dontrun{
#' run_datamart_code(output = output, r = r, datamart_id = 3)
#' }
run_datamart_code_new <- function(output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), datamart_id = integer(), i18n = R6::R6Class(), quiet = TRUE){
  # Reset r$chosen_datamart to clear patient-level data & aggregated data (use the same r variables for data) 
  
  # Get code from datamart
  tryCatch(r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_load_code", 
        error_name = paste0("run_datamart_code - load_code - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_load_code"))}
  )
  code <- r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code)
  
  # Replace %datamart_id% with real datamart_id
  # Replace \r by \n to prevent bug
  code <- code %>% 
    stringr::str_replace_all("%datamart_id%", as.character(datamart_id)) %>%
    stringr::str_replace_all("\r", "\n")
  
  # Reset r variables
  vars <- c("patients", "stays", "labs_vitals", "text", "orders", "diagnoses")
  for (var in vars) d[[var]] <- tibble::tibble()
  
  # Load data from datamart
  
  tryCatch(eval(parse(text = code)),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_execute_code", 
        error_name = paste0("run_datamart_code - execute_code - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_execute_code"))}
  )
  
  # If data is loaded, nb of rows of d variable > 0
  # if (!quiet) for (var in vars) if (nrow(d[[var]]) != 0) show_message_bar_new(output, 1, paste0("import_datamart_success_", var), "success", i18n)
}

#' Add patients to a subset
#'
#' @description Add patients to a subset, only if not already in the subset
#' @details ...
#' @param output Output variable from Shiny, used to render messages on the message bar
#' @param r The r reactive variable, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param patients data variable containing patients (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param success_notification Should a message bar be displayed if insertion of patient(s) is a success ? Default to TRUE
#' @param language language used for error / warning messages (character, default = "EN")
#' @examples 
#' \dontrun{
#' patients <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' subset_add_patients(output = output, r = r, patients = patients, subset_id = 3, language = "EN")
#' }
add_patients_to_subset_new <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), patients = tibble::tibble(),
  subset_id = integer(), success_notification = FALSE, i18n = R6::R6Class()){
  
  # Check subset_id
  
  if (length(subset_id) == 0){
    show_message_bar_new(output, 1, "invalid_subset_id_value", "severeWarning", i18n = i18n)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("add_patients_to_subset - invalid_subset_id - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("invalid_subset_id_value"))}
  )
  
  if (is.na(subset_id)){
    show_message_bar_new(output, 1, "invalid_subset_id_value", "severeWarning", i18n = i18n)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "patient_id", "integer")
  
  # Check col names
  if (!identical(names(patients), "patient_id")){
    show_message_bar_new(output, 1, "invalid_col_names", "severeWarning", i18n = i18n)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(patients[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
  })
  
  # Transform as tibble
  tryCatch(patients <- tibble::as_tibble(patients), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("add_patients_to_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  # Keep only patients not already in the subset
  actual_patients <- DBI::dbGetQuery(m$db, paste0("SELECT patient_id FROM subset_patients WHERE subset_id = ", subset_id))
  
  patients <- patients %>% dplyr::anti_join(actual_patients, by = "patient_id")
  
  # If there are patients to add
  if (nrow(patients) > 0){
    
    # Add new patient(s) in the subset
    tryCatch({
      last_id <- DBI::dbGetQuery(m$db, "SELECT COALESCE(MAX(id), 0) FROM subset_patients") %>% dplyr::pull()
      other_cols <- tibble::tibble(
        id = 1:nrow(patients) + last_id, subset_id = subset_id, creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
      )
      patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::relocate(patient_id, .after = "subset_id")
      DBI::dbAppendTable(m$db, "subset_patients", patients)
    },
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_inserting_data", 
          error_name = paste0("add_patients_to_subset - error_inserting_data - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_inserting_data"))}
    )
  }
  
  if (success_notification){
    show_message_bar_new(output, 1, "add_patients_subset_success", "success", i18n = i18n)
    print(i18n$t("add_patients_subset_success"))
  }
}

#' Remove patients from a subset
#'
#' @description Remove patients from a subset
#' @details ...
#' @param output Output variable from Shiny, used to render messages on the message bar
#' @param r The r reactive variable, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param patients data variable containing patients (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param language language used for error / warning messages (character, default = "EN")

remove_patients_from_subset <- function(output, r = shiny::reactiveValues(), patients = tibble::tibble(), 
  subset_id = integer(), language = "EN"){
  
  # Check subset_id
  
  if (length(subset_id) == 0){
    show_message_bar(output, 1, "invalid_subset_id_value", "severeWarning", language, r$words)
    stop(translate(language, "invalid_subset_id_value", r$words))
  }
  
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_patients_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "invalid_subset_id_value", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_patients_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "invalid_subset_id_value", r$words))}
  )
  
  if (is.na(subset_id)){
    show_message_bar(output, 1, "invalid_subset_id_value", "severeWarning", language, r$words)
    stop(translate(language, "invalid_subset_id_value", r$words))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "patient_id", "integer")
  
  # Check col names
  if (!identical(names(patients), "patient_id")){
    show_message_bar(output, 1, "invalid_col_names", "severeWarning", language, r$words)
    stop(translate(language, "valid_col_names_are", r$words), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(patients[[var_name]])){
      show_message_bar(output, 1, "invalid_col_types", "severeWarning", language, r$words)
      stop(paste0(translate(language, "column", r$words), " ", var_name, " ", translate(language, "type_must_be_integer", r$words)))
    }
  })
  
  # Transform as tibble
  tryCatch(patients <- tibble::as_tibble(patients), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_patients_from_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_patients_from_subset - error_transforming_tibble - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))}
  )
  
  tryCatch({ query <- DBI::dbSendStatement(r$db, paste0("DELETE FROM subset_patients WHERE subset_id = ", subset_id, " AND
    patient_id IN (", paste(patients %>% dplyr::pull(patient_id), collapse = ",") , ")"))
    DBI::dbClearResult(query)
  }, 
  error = function(e){
    if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_removing_patients_from_subset", 
      error_name = paste0("remove_patients_from_subset - error_removing_patients_from_subset - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
    stop(translate(language, "error_removing_patients_from_subset", r$words))},
  warning = function(w) if (nchar(w[1]) > 0){
    report_bug(r = r, output = output, error_message = "error_removing_patients_from_subset", 
      error_name = paste0("remove_patients_from_subset - error_removing_patients_from_subset - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
    stop(translate(language, "error_removing_patients_from_subset", r$words))}
  )
}

get_thesaurus_items_levels <- function(data = tibble::tibble()){
  
  data_with_levels <- tibble::tibble()
  
  i <- 1
  while (i > 0){
    if (i == 1){
      if (nrow(data %>% dplyr::filter(is.na(parent_item_id))) == 0) i <- -1
      temp <-
        data %>%
        dplyr::filter(is.na(parent_item_id)) %>%
        dplyr::mutate(level = i)
      
      data_with_levels <- temp
    }
    
    temp <-
      temp %>%
      dplyr::select(parent_item_id = item_id, parent_name = name, dplyr::starts_with("name_level_")) %>%
      dplyr::inner_join(data, by = "parent_item_id") %>%
      dplyr::mutate(level = i + 1, !!paste0("name_level_", i) := parent_name) %>%
      dplyr::select(-parent_name)
    
    if (nrow(temp) == 0) i <- -1
    else {
      data_with_levels <-
        data_with_levels %>%
        dplyr::bind_rows(temp)
      
      temp <- temp %>% dplyr::filter(level == i + 1)
      
      i <- i + 1
    }
  }
  
  data_with_levels <- data_with_levels %>%
    dplyr::left_join(data %>% dplyr::select(item_id = parent_item_id, children_item_id = item_id), by = "item_id") %>%
    dplyr::group_by(item_id, parent_item_id, name, unit, count_patients_rows, count_items_rows, level, dplyr::across(dplyr::starts_with("name_level_"))) %>%
    dplyr::summarize(n_children = max(children_item_id)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(has_children = dplyr::case_when(is.na(n_children) ~ FALSE, TRUE ~ TRUE)) %>%
    dplyr::select(-n_children)
  
  data_with_levels
}

get_thesaurus_items_paths <- function(data = tibble::tibble()){
  
  data_with_paths <- data %>% dplyr::mutate(path = "")
  
  full_list <- list()
  
  if (nrow(data) > 0){
    
    # Loop over categories levels
    for (i in 1:(max(data$level))){
      
      if (i > 1) {
        
        filtered_categories <- data %>% dplyr::filter(level == i)
        
        # Create path column
        paths <- data %>% 
          dplyr::filter(level == i) %>%
          dplyr::select(-item_id, -parent_item_id, -unit, -level, -name, -count_patients_rows, -count_items_rows) %>%
          dplyr::select(seq(1, i - 1)) %>%
          tidyr::unite("path", sep = "$", na.rm = TRUE, remove = TRUE) %>%
          dplyr::mutate_at("path", stringr::str_replace_all, "\\$", "`$`") %>%
          dplyr::mutate(path = paste0("`", path, "`" )) %>%
          dplyr::pull()
        
        filtered_categories$path <- paths
        
        data_with_paths <- data_with_paths %>%
          dplyr::filter(level != i) %>%
          dplyr::bind_rows(filtered_categories)
      }
    }
  }
  
  data_with_paths
}

get_thesaurus_name <- function(r = shiny::reactiveValues(), datamart_id = integer(), thesaurus_id = integer()){
  
  result <- NA_character_
  
  if (length(datamart_id) > 0){
    data_source_id <- r$datamarts %>% dplyr::filter(id == datamart_id) %>% dplyr::pull(data_source_id)
    if (length(data_source_id) > 0) data_source_name <- r$data_sources %>% dplyr::filter(id == data_source_id) %>% dplyr::pull(name)
    result <- paste0(data_source_name, " - scripts")
  }
  
  if (length(thesaurus_id) > 0){
    result <- r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% dplyr::pull(name)
  }
  
  result
}

get_thesaurus_item <- function(output, r = shiny::reactiveValues(), thesaurus_name = character(), 
  item_name = character(), item_id = integer(), method = character(), create = FALSE, item_unit = NA_character_, 
  i18n = R6::R6Class(), ns = shiny::NS()){
  
  stop_fct <- FALSE
  
  # Check thesaurus_name
  
  if (length(thesaurus_name) == 0) stop_fct <- TRUE
  else if (is.na(thesaurus_name)) stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar_new(output, 1, "invalid_thesaurus_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_name_value"))
  }
  
  # Get thesaurus_id
  
  sql <- glue::glue_sql("SELECT * FROM thesaurus WHERE name = {thesaurus_name} AND deleted IS FALSE", .con = r$db)
  thesaurus_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  if (length(thesaurus_id) == 0){
    show_message_bar_new(output, 1, "thesaurus_id_not_found", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("thesaurus_id_not_found"))
  }
  
  # Search by item name
  
  if (length(method) == 0) stop_fct <- TRUE
  else if (is.na(method)) stop_fct <- TRUE
  if (stop_fct){
    show_message_bar_new(output, 1, "invalid_thesaurus_search_method", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_search_method"))
  }
  
  if (method == "item_name"){
    
    if (length(item_name) == 0) stop_fct <- TRUE
    else if (is.na(item_name)) stop_fct <- TRUE
    if (stop_fct){
      show_message_bar_new(output, 1, "invalid_thesaurus_item_name", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("invalid_thesaurus_item_name"))
    }
    
    sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND ",
      "name = {item_name} AND deleted IS FALSE"), .con = r$db)
    result <- DBI::dbGetQuery(r$db, sql)
  }
  
  # Search by item id
  
  if (method == "item_id"){
    
    if (length(item_id) == 0) stop_fct <- TRUE
    else if (is.na(item_id)) stop_fct <- TRUE
    if (stop_fct){
      show_message_bar_new(output, 1, "invalid_thesaurus_item_id", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("invalid_thesaurus_item_id"))
    }
    
    sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND ",
      "item_id = {item_id} AND deleted IS FALSE"), .con = r$db)
    result <- DBI::dbGetQuery(r$db, sql)
  }
  
  if (nrow(result) == 0 & create){
    add_thesaurus_item(output = output, r = r, thesaurus_name = thesaurus_name, item_name = item_name, item_unit = item_unit, i18n = i18n, ns = ns)
  }
  
  else result
}

add_thesaurus_item <- function(output, r = shiny::reactiveValues(), thesaurus_name = character(), item_name = character(), 
  item_unit = NA_character_, i18n = R6::R6Class(), ns = shiny::NS()){
  
  stop_fct <- FALSE
  
  # Check thesaurus_name
  
  if (length(thesaurus_name) == 0) stop_fct <- TRUE
  else if (is.na(thesaurus_name)) stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar_new(output, 1, "invalid_thesaurus_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_name_value"))
  }
  
  # Get thesaurus_id
  
  sql <- glue::glue_sql("SELECT * FROM thesaurus WHERE name = {thesaurus_name} AND deleted IS FALSE", .con = r$db)
  thesaurus_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  if (length(thesaurus_id) == 0){
    show_message_bar_new(output, 1, "thesaurus_id_not_found", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("thesaurus_id_not_found"))
  }
  
  # Check item_name
  
  if (length(item_name) == 0) stop_fct <- TRUE
  else if (is.na(item_name)) stop_fct <- TRUE
  if (stop_fct){
    show_message_bar_new(output, 1, "invalid_thesaurus_item_name", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_item_name"))
  }
  
  # Check item_unit
  
  if (length(item_unit) == 0) item_unit <- NA_character_
  
  # Check if this item already exists
  sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND ",
    "name = {item_name} AND deleted IS FALSE"), .con = r$db)
  check_item <- DBI::dbGetQuery(r$db, sql)
  
  if (nrow(check_item) > 0){
    show_message_bar_new(output, 1, "thesaurus_item_already_exists", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_item_name"))
  }
  
  if (nrow(check_item) == 0){
    
    sql <- glue::glue_sql(paste0("SELECT COALESCE(MAX(item_id), 0) FROM thesaurus_items WHERE ",
      "thesaurus_id = {thesaurus_id}"), .con = r$db)
    last_item_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    
    new_data <- tibble::tribble(~id, ~thesaurus_id, ~item_id, ~name, ~display_name, ~unit, ~datetime, ~deleted,
      get_last_row(r$db, "thesaurus_items"), thesaurus_id, last_item_id + 1, item_name, "", item_unit, as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "thesaurus_items", new_data)
  }
  
  show_message_bar_new(output, 1, "thesaurus_item_added", "success", i18n = i18n, ns = ns)
}

create_scripts_thesaurus <- function(output, r = shiny::reactiveValues(), data_source_id = integer(), 
  i18n = R6::R6Class(), ns = shiny::NS()){
  
  # Check if thesaurus exists
  data_source_name <- r$data_sources %>% dplyr::filter(id == data_source_id) %>% dplyr::pull(name)
  sql <- glue::glue_sql("SELECT * FROM thesaurus WHERE name = {paste0(data_source_name, ' - scripts')} AND deleted IS FALSE", .con = r$db)
  thesaurus <- DBI::dbGetQuery(r$db, sql)
  
  # Create if doesn't exist
  if (nrow(thesaurus) == 0){
    
    last_row_thesaurus <- get_last_row(r$db, "thesaurus")
    
    new_data <- tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      last_row_thesaurus + 1, paste0(data_source_name, " - scripts"), NA_character_, as.character(data_source_id), r$user_id, 
      as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "thesaurus", new_data)
    r$thesaurus <- r$thesaurus %>% dplyr::bind_rows(new_data)
    
    new_data <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "code") + 1, "thesaurus", last_row_thesaurus + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_data)
    r$code <- r$code %>% dplyr::bind_rows(new_data)
    
    show_message_bar_new(output, 1, "thesaurus_added", "success", i18n = i18n, ns = ns)
  }
}