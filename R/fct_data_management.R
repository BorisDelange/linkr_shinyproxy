#' Run dataset code 
#'
#' @description Runs dataset code
#' @details The code to load the dataset is loaded from the application database, in code table, from dataset_id.
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param d A shiny::reactiValues object, used to communicate between tabs. Contains data loaded from dataset code (d$patients, d$labs_vitals...).
#' @param dataset_id ID of dataset containing the code (integer)
#' @param i18n shiny.i18n object for translations
#' @examples 
#' \dontrun{
#' run_dataset_code(output = output, r = r, d = d, i18n = i18n, dataset_id = 3)
#' }
run_dataset_code <- function(output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), dataset_id = integer(), i18n = character()){
  
  # Get code from dataset
  tryCatch(r$code %>% dplyr::filter(category == "dataset" & link_id == dataset_id) %>% dplyr::pull(code),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_code", 
        error_name = paste0("run_dataset_code - load_code - id = ", dataset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_load_code"))}
  )
  code <- r$code %>% dplyr::filter(category == "dataset" & link_id == dataset_id) %>% dplyr::pull(code)
  
  # Replace %dataset_id% with real dataset_id
  code <- code %>% 
    stringr::str_replace_all("%dataset_id%", as.character(dataset_id)) %>%
    stringr::str_replace_all("\r", "\n")
  
  # Reset d variables
  main_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
    "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
    "drug_era", "dose_era", "condition_era", 
    "person", "observation_period", "visit_occurrence", "visit_detail",
    "location", "care_site", "provider")
  sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
  
  # Run code

  tryCatch(eval(parse(text = code)),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_execute_code", 
        error_name = paste0("run_dataset_code - execute_code - id = ", dataset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_execute_code"))}
  )
}

#' Add patients to a subset
#'
#' @description Add patients to a subset, only if not already in the subset
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param m A shiny::reactiValues object, used to communicate between tabs
#' @param patients data variable containing patients (data.frame / tibble)
#' @param subset_id ID of the subset (integer)
#' @param i18n shiny.i18n object for translations
#' @param ns shiny namespace object, used to render messages on message bars
#' @examples 
#' \dontrun{
#' patients <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' subset_add_patients(output = output, r = r, m = m, patients = patients, subset_id = 3, i18n = i18n, ns = ns)
#' }
add_persons_to_subset <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), persons = tibble::tibble(),
  subset_id = integer(), i18n = character(), ns = character()){
  
  # Check subset_id
  
  if (length(subset_id) == 0){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("add_persons_to_subset - invalid_subset_id - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("invalid_subset_id_value"))}
  )
  
  if (is.na(subset_id)){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "person_id", "integer")
  
  # Check col names
  if (!identical(names(persons), "person_id")){
    show_message_bar(output, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(persons[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
  })
  
  # Transform as tibble
  tryCatch(persons <- tibble::as_tibble(persons), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("add_persons_to_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  # Keep only persons not already in the subset
  sql <- glue::glue_sql("SELECT person_id FROM subset_persons WHERE subset_id = {subset_id} AND deleted IS FALSE", .con = m$db)
  actual_persons <- DBI::dbGetQuery(m$db, sql)
  
  persons <- persons %>% dplyr::anti_join(actual_persons, by = "person_id")
  
  # If there are persons to add
  if (nrow(persons) > 0){
    
    # Add new patient(s) in the subset
    tryCatch({
      last_id <- DBI::dbGetQuery(m$db, "SELECT COALESCE(MAX(id), 0) FROM subset_persons") %>% dplyr::pull()
      other_cols <- tibble::tibble(
        id = 1:nrow(persons) + last_id, subset_id = subset_id, creator_id = m$user_id, datetime = as.character(Sys.time()), deleted = FALSE
      )
      persons <- persons %>% dplyr::bind_cols(other_cols) %>% dplyr::relocate(person_id, .after = "subset_id")
      DBI::dbAppendTable(m$db, "subset_persons", persons)
    },
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_inserting_data", 
        error_name = paste0("add_persons_to_subset - error_inserting_data - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("error_inserting_data"))}
    )
  }
  
  show_message_bar(output, "add_persons_subset_success", "success", i18n = i18n, ns = ns)
}

#' Remove patients from a subset
#'
#' @description Remove patients from a subset
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param m A shiny::reactiValues object, used to communicate between tabs
#' @param patients data variable containing patients (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param i18n shiny.i18n object for translations
#' @param ns shiny namespace object, used to render messages on message bars
#' @examples 
#' \dontrun{
#' patients <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' remove_patients_from_subset(output = output, r = r, m = m, patients = patients, subset_id = 3, i18n = i18n, ns = ns)
#' }
remove_persons_from_subset <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), persons = tibble::tibble(), 
  subset_id = integer(), i18n = character(), ns = character()){
  
  # Check subset_id
  
  if (length(subset_id) == 0){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_persons_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("invalid_subset_id_value"))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_persons_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
      stop(i18n$t("invalid_subset_id_value"))}
  )
  
  if (is.na(subset_id)){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "person_id", "integer")
  
  # Check col names
  if (!identical(names(persons), "person_id")){
    show_message_bar(output, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(persons[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
  })
  
  # Transform as tibble
  tryCatch(persons <- tibble::as_tibble(persons), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_persons_from_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("error_transforming_tibble"))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_persons_from_subset - error_transforming_tibble - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  tryCatch({ 
    sql <- glue::glue_sql(paste0("DELETE FROM subset_persons WHERE subset_id = {subset_id} AND ",
      "person_id IN ({persons %>% dplyr::pull(person_id)*})"), .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
  }, 
  error = function(e){
    if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_removing_persons_from_subset", 
      error_name = paste0("remove_persons_from_subset - error_removing_persons_from_subset - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
    stop(i18n$t("error_removing_persons_from_subset"))},
  warning = function(w) if (nchar(w[1]) > 0){
    report_bug(r = r, output = output, error_message = "error_removing_persons_from_subset", 
      error_name = paste0("remove_persons_from_subset - error_removing_persons_from_subset - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
    stop(i18n$t("error_removing_persons_from_subset"))}
  )
  
  show_message_bar(output, "remove_persons_subset_success", "success", i18n = i18n, ns = ns)
}

#' Get thesaurus items levels
#'
#' @description Get levels of thesaurus items to create thesaurus tree
#' @param data datatable containing thesaurus items
#' @param r A shiny::reactiValues object, used to communicate between tabs
# get_thesaurus_items_levels <- function(data = tibble::tibble(), r = shiny::reactiveValues()){
#   
#   data_with_levels <- tibble::tibble()
#   
#   i <- 1
#   while (i > 0){
#     if (i == 1){
#       if (nrow(data %>% dplyr::filter(is.na(parent_item_id))) == 0) i <- -1
#       temp <-
#         data %>%
#         dplyr::filter(is.na(parent_item_id)) %>%
#         dplyr::mutate(level = i)
#       
#       data_with_levels <- temp
#     }
#     
#     temp <-
#       temp %>%
#       dplyr::select(parent_item_id = item_id, parent_name = name, dplyr::starts_with("name_level_")) %>%
#       dplyr::inner_join(data, by = "parent_item_id") %>%
#       dplyr::mutate(level = i + 1, !!paste0("name_level_", i) := parent_name) %>%
#       dplyr::select(-parent_name)
#     
#     if (nrow(temp) == 0) i <- -1
#     else {
#       data_with_levels <-
#         data_with_levels %>%
#         dplyr::bind_rows(temp)
#       
#       temp <- temp %>% dplyr::filter(level == i + 1)
#       
#       i <- i + 1
#     }
#   }
#   
#   data_with_levels <- data_with_levels %>%
#     dplyr::left_join(data %>% dplyr::select(item_id = parent_item_id, children_item_id = item_id), by = "item_id") %>%
#     dplyr::group_by(item_id, parent_item_id, name, unit, count_patients_rows, count_items_rows, level, dplyr::across(dplyr::starts_with("name_level_"))) %>%
#     dplyr::summarize(n_children = max(children_item_id)) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(has_children = dplyr::case_when(is.na(n_children) ~ FALSE, TRUE ~ TRUE)) %>%
#     dplyr::select(-n_children)
#
#   data_with_levels
# }

#' Get thesaurus items paths
#'
#' @description Get paths of thesaurus items to create thesaurus tree
#' @param data datatable containing thesaurus items
#' @param r A shiny::reactiValues object, used to communicate between tabs
# get_thesaurus_items_paths <- function(data = tibble::tibble(), r = shiny::reactiveValues()){
#   
#   data_with_paths <- data %>% dplyr::mutate(path = "")
#   
#   full_list <- list()
#   
#   if (nrow(data) > 0){
#     
#     # Loop over categories levels
#     for (i in 1:(max(data$level))){
#       
#       if (i > 1) {
#         
#         filtered_categories <- data %>% dplyr::filter(level == i)
#         
#         # Create path column
#         paths <- data %>% 
#           dplyr::filter(level == i) %>%
#           dplyr::select(-item_id, -parent_item_id, -unit, -level, -name, -count_patients_rows, -count_items_rows) %>%
#           dplyr::select(seq(1, i - 1)) %>%
#           tidyr::unite("path", sep = "$", na.rm = TRUE, remove = TRUE) %>%
#           dplyr::mutate_at("path", stringr::str_replace_all, "\\$", "`$`") %>%
#           dplyr::mutate(path = paste0("`", path, "`" )) %>%
#           dplyr::pull()
#         
#         filtered_categories$path <- paths
#         
#         data_with_paths <- data_with_paths %>%
#           dplyr::filter(level != i) %>%
#           dplyr::bind_rows(filtered_categories)
#       }
#     }
#   }
#
#   data_with_paths
# }

#' Get thesaurus item
#'
#' @description Get a thesaurus item, from the thesaurus name and ID or name of the item.
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param thesaurus_name Name of the thesaurus (character)
#' @param item_name Name of the item to get, if method = "item_name" (character)
#' @param item_id ID of the item to get, if method = "item_id" (integer)
#' @param method Method used to get the thesaurus item. Could be "item_name" or "item_id" (character).
#' @param create If the item is not found and create is TRUE, the item is created, with add_thesaurus_item function (logical).
#' @param item_unit Unit of the item, if it is not found and create is TRUE (character).
#' @param i18n shiny.i18n object for translations
#' @param ns shiny namespace object, used to render messages on message bars
#' @examples 
#' \dontrun{
#' # Search item "Respiratory rate" from thesaurus "MIMIC-IV"
#' get_thesaurus_item(output = output, r = r, thesaurus_name = "MIMIC-IV", item_name = "Respiratory rate", method = "item_name", i18n = i18n, ns = ns)
#' 
#' # Search item 83748 from thesaurus "MIMIC-IV", and create it if not found
#' get_thesaurus_item(output = output, r = r, thesaurus_name = "MIMIC-IV", item_id = 83746, 
#'  item_name = "Heart rate", item_unit = "bpm", method = "item_id", i18n = i18n, ns = ns)
#' }
get_thesaurus_item <- function(output, r = shiny::reactiveValues(), thesaurus_name = character(), 
  item_name = character(), item_id = integer(), method = character(), create = FALSE, item_unit = NA_character_, 
  i18n = character(), ns = character()){
  
  stop_fct <- FALSE
  
  # Check thesaurus_name
  
  if (length(thesaurus_name) == 0) stop_fct <- TRUE
  else if (is.na(thesaurus_name)) stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar(output, "invalid_thesaurus_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_name_value"))
  }
  
  # Get thesaurus_id
  
  sql <- glue::glue_sql("SELECT * FROM thesaurus WHERE name = {thesaurus_name} AND deleted IS FALSE", .con = r$db)
  thesaurus_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  if (length(thesaurus_id) == 0){
    show_message_bar(output, "thesaurus_id_not_found", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("thesaurus_id_not_found"))
  }
  
  if (length(method) == 0) stop_fct <- TRUE
  else if (is.na(method)) stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_thesaurus_search_method", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_search_method"))
  }
  
  # Search by item name
  
  if (method == "item_name"){
    
    if (length(item_name) == 0) stop_fct <- TRUE
    else if (is.na(item_name)) stop_fct <- TRUE
    if (stop_fct){
      show_message_bar(output, "invalid_thesaurus_item_name", "severeWarning", i18n = i18n, ns = ns)
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
      show_message_bar(output, "invalid_thesaurus_item_id", "severeWarning", i18n = i18n, ns = ns)
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

#' Add thesaurus item
#'
#' @description Add an item to a thesaurus
#' @details To add an item in a thesaurus, you need to specify its name and unit.
#' The item ID will be the ID of the last item of this thesaurus, plus one.
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param thesaurus_name Name of the thesaurus where the item will be added (character)
#' @param item_name Name of the item (character)
#' @param item_unit Unit of the item (character)
#' @param i18n shiny.i18n object for translations
#' @param ns shiny namespace object, used to render messages on message bars
#' @examples 
#' \dontrun{
#' # Create item "Heart rate" in thesaurus "MIMIC-IV"
#' add_thesaurus_item(output = output, r = r, thesaurus_name = "MIMIC-IV", item_name = "Heart rate", item_unit = "bpm")
#' }
add_thesaurus_item <- function(output, r = shiny::reactiveValues(), thesaurus_name = character(), item_name = character(), 
  item_unit = NA_character_, i18n = character(), ns = character()){
  
  stop_fct <- FALSE
  
  # Check thesaurus_name
  
  if (length(thesaurus_name) == 0) stop_fct <- TRUE
  else if (is.na(thesaurus_name)) stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar(output, "invalid_thesaurus_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_name_value"))
  }
  
  # Get thesaurus_id
  
  sql <- glue::glue_sql("SELECT * FROM thesaurus WHERE name = {thesaurus_name} AND deleted IS FALSE", .con = r$db)
  thesaurus_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  if (length(thesaurus_id) == 0){
    show_message_bar(output, "thesaurus_id_not_found", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("thesaurus_id_not_found"))
  }
  
  # Check item_name
  
  if (length(item_name) == 0) stop_fct <- TRUE
  else if (is.na(item_name)) stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_thesaurus_item_name", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_item_name"))
  }
  
  # Check item_unit
  
  if (length(item_unit) == 0) item_unit <- NA_character_
  
  # Check if this item already exists
  sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND ",
    "name = {item_name} AND deleted IS FALSE"), .con = r$db)
  check_item <- DBI::dbGetQuery(r$db, sql)
  
  if (nrow(check_item) > 0){
    show_message_bar(output, "thesaurus_item_already_exists", "severeWarning", i18n = i18n, ns = ns)
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
  
  show_message_bar(output, "thesaurus_item_added", "success", i18n = i18n, ns = ns)
}

#' Create scripts thesaurus
#'
#' @description Create a thesaurus for the items created in the scripts
#' @details A script is a code executed each time we load a dataset.
#' In some scripts, we create new thesaurus items.
#' The scripts are attached to data sources. 
#' This function creates a new thesaurus, for a data source, named [data_source_name - scripts], which will include
#' all items created in the scripts depending of this data source.
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiValues object, used to communicate between tabs
#' @param data_source_id ID of the data source where the script is recorded
#' @param i18n shiny.i18n object for translations
#' @param ns shiny namespace object, used to render messages on message bars
#' @examples 
#' \dontrun{
#' # If my script depends of the data source with ID 13, named "My data source", this function will create a thesaurus
#' called "My data source - scripts".
#' create_scripts_thesaurus(output = output, r = r, data_source_id = 13, i18n = i18n, ns = ns)
#' }
create_scripts_thesaurus <- function(output, r = shiny::reactiveValues(), data_source_id = integer(), 
  i18n = character(), ns = character()){
  
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
      get_last_row(r$db, "code") + 1, "thesaurus", last_row_thesaurus + 1, "", r$user_id, as.character(Sys.time()), FALSE)
    DBI::dbAppendTable(r$db, "code", new_data)
    r$code <- r$code %>% dplyr::bind_rows(new_data)
    
    show_message_bar(output, "thesaurus_added", "success", i18n = i18n, ns = ns)
  }
}
