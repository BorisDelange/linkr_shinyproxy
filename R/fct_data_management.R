#' Run dataset code 
#'
#' @description Runs the code of a dataset
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param d A shiny::reactiveValues object, used to communicate between modules. Contains data loaded from dataset code (d$patients, d$labs_vitals...).
#' @param dataset_id ID of the dataset we want to load (integer)
#' @param i18n Translator object from shiny.i18n library
#' @examples 
#' \dontrun{
#' run_dataset_code(output = output, r = r, d = d, dataset_id = 3, i18n = i18n)
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
#' @description Add patients to a subset
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param persons data variable containing patients / persons (data.frame / tibble)
#' @param subset_id ID of the subset (integer)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' persons <- tibble::tribble(~person_id, 123L, 456L, 789L)
#' subset_add_patients(output = output, r = r, m = m, persons = persons, subset_id = 3, i18n = i18n, ns = ns)
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
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param persons data variable containing patients / persons (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' persons <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' remove_persons_from_subset(output = output, r = r, m = m, persons = persons, subset_id = 3, i18n = i18n, ns = ns)
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

#' Get vocabulary concept
#'
#' @description Get a thesaurus item, from the thesaurus name and ID or name of the item.
#' @param output Output variable from Shiny, used to render messages on message bars
#' @param r A shiny::reactiveValues object, used to communicate between modules
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

#' Add vocabulary concept
#'
#' @description Add a new concept in the database concept table
#' @details See \href{https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT}{OMOP concept table} for more details.\cr\cr
#' Vocabulary_id must be present in the database when you add a new concept.\cr
#' Not null values are required for concept_name, domain_id, concept_class_id and concept_code.\cr
#' The value you provide for domain_id & concept_class_id must be present in the database.
#' @param output Shiny output variable
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param vocabulary_id Value of vocabulary_id where the concept will be added (character)
#' @param concept_name Value of concept_name (character)
#' @param domain_id Value of domain_id, must be present in database (character)
#' @param concept_class_id Value of concept_class_id, must be present in database (character)
#' @param concept_code Value of concept_code (character)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' 
#' }
add_vocabulary_concept <- function(output, m = shiny::reactiveValues(), vocabulary_id = character(), 
  concept_name = character(), domain_id = character(), concept_class_id = character(), concept_code = character(), i18n = character(), ns = character()){
  
  stop_fct <- FALSE
  
  # Check vocabulary_id
  
  if (length(vocabulary_id) == 0) stop_fct <- TRUE
  else if (is.na(vocabulary_id) | vocabulary_id == "") stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar(output, "invalid_vocabulary_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_vocabulary_id_value"))
  }
  
  # Check concept_name
  
  if (length(concept_name) == 0) stop_fct <- TRUE
  else if (is.na(concept_name) | concept_name == "") stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_concept_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_name_value"))
  }
  
  # Check domain_id
  
  if (length(domain_id) == 0) stop_fct <- TRUE
  else if (is.na(domain_id) | domain_id == "") stop_fct <- TRUE
  else {
    sql <- glue::glue_sql("SELECT * from domain WHERE domain_id == {domain_id}", .con = m$db)
    if (nrow(DBI::dbGetQuery(m$db, sql)) == 0) stop_fct <- TRUE
  }
  if (stop_fct){
    show_message_bar(output, "invalid_domain_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_domain_id_value"))
  }
  
  # Check concept_class_id
  
  if (length(concept_class_id) == 0) stop_fct <- TRUE
  else if (is.na(concept_class_id) | concept_class_id == "") stop_fct <- TRUE
  else {
    sql <- glue::glue_sql("SELECT * from concept_class WHERE concept_class_id == {concept_class_id}", .con = m$db)
    if (nrow(DBI::dbGetQuery(m$db, sql)) == 0) stop_fct <- TRUE
  }
  if (stop_fct){
    show_message_bar(output, "invalid_concept_class_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_class_id_value"))
  }
  
  # Check concept_name
  
  if (length(concept_code) == 0) stop_fct <- TRUE
  else if (is.na(concept_code) | concept_code == "") stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_concept_code_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_code_value"))
  }
  
  # Check if the vocabulary_id provided exists in database
  sql <- glue::glue_sql(paste0("SELECT * FROM vocabulary WHERE vocabulary_id = {vocabulary_id}"), .con = m$db)
  check_vocabulary_id <- DBI::dbGetQuery(m$db, sql)
  
  if (nrow(check_vocabulary_id) == 0){
    show_message_bar(output, "vocabulary_id_doesnt_exist", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("vocabulary_id_doesnt_exist"))
  }
  
  # Check if this concept already exists
  sql <- glue::glue_sql(paste0("SELECT * FROM concept WHERE concept_name = {concept_name} AND vocabulary_id = {vocabulary_id} ",
    "AND domain_id == {domain_id} AND concept_class_id = {concept_class_id}"), .con = m$db)
  check_concept <- DBI::dbGetQuery(m$db, sql)
  
  if (nrow(check_concept) > 0){
    show_message_bar(output, "vocabulary_concept_already_exists", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("vocabulary_concept_already_exists"))
  }
  
  if (nrow(check_concept) == 0){
    
    sql <- glue::glue_sql("SELECT COALESCE(MAX(concept_id), 0) FROM concept", .con = m$db)
    last_concept_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    if (last_concept_id < 2000000000) new_concept_id = 2000000000
    else new_concept_id <- last_concept_id + 1
    
    new_data <- tibble::tribble(
      ~id, ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id, ~concept_class_id, ~standard_concept, 
      ~concept_code, ~valid_start_date, ~valid_end_date, ~invalid_reason,
      get_last_row(m$db, "concept") + 1, new_concept_id, concept_name, domain_id, vocabulary_id, 
      concept_class_id, "C", concept_code, "1970-01-01", "2099-12-31", NA_character_)
    
    print(new_data)
    
    DBI::dbAppendTable(m$db, "concept", new_data)
  }
  
  show_message_bar(output, "vocabulary_concept_added", "success", i18n = i18n, ns = ns)
}