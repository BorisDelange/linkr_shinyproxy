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
run_datamart_code <- function(output, r = shiny::reactiveValues(), datamart_id = integer(), language = "EN", quiet = TRUE){
  # Reset r$chosen_datamart to clear patient-level data & aggregated data (use the same r variables for data) 
  
  # Get code from datamart
  tryCatch(r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_code", 
        error_name = paste0("run_datamart_code - load_code - id = ", datamart_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "fail_load_code", r$words))}
  )
  code <- r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code)
  
  # Replace %datamart_id% with real datamart_id
  # Replace \r by \n to prevent bug
  code <- code %>% 
    stringr::str_replace_all("%datamart_id%", as.character(datamart_id)) %>%
    stringr::str_replace_all("\r", "\n")
  
  # Reset r variables
  d$patients <- tibble::tibble()
  d$stays <- tibble::tibble()
  d$labs_vitals <- tibble::tibble()
  d$text <- tibble::tibble()
  d$orders <- tibble::tibble()
  
  # Load data from datamart
  
  tryCatch(eval(parse(text = code)),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_execute_code", 
        error_name = paste0("run_datamart_code - execute_code - id = ", datamart_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "fail_execute_code", r$words))}
  )
  
  # If data is loaded, nb of rows of r variable > 0
  if (!quiet){
    if (nrow(d$patients) != 0) show_message_bar(output, 1, "success_load_patients", "success", language, r$words)
    if (nrow(d$stays) != 0) show_message_bar(output, 2, "success_load_stays", "success", language, r$words)
    if (nrow(d$labs_vitals) != 0) show_message_bar(output, 3, "success_load_labs_vitals", "success", language, r$words)
    if (nrow(d$text) != 0) show_message_bar(output, 4, "success_load_text", "success", language, r$words)
    if (nrow(d$orders) != 0) show_message_bar(output, 5, "success_load_orders", "success", language, r$words)
  }
}

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
add_patients_to_subset <- function(output, r = shiny::reactiveValues(), patients = tibble::tibble(), subset_id = integer(),
  success_notification = TRUE, language = "EN"){
  
  # Check subset_id
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("add_patients_to_subset - invalid_subset_id - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "invalid_subset_id_value", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("add_patients_to_subset - invalid_subset_id - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "invalid_subset_id_value", r$words))}
  )
  
  if (is.na(subset_id) | length(subset_id) == 0){
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
        error_name = paste0("add_patients_to_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("add_patients_to_subset - error_transforming_tibble - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))}
  )
  
  # Keep only patients not already in the subset
  actual_patients <- DBI::dbGetQuery(r$db, paste0("SELECT patient_id FROM subset_patients WHERE subset_id = ", subset_id))
  
  patients <- patients %>% dplyr::anti_join(actual_patients, by = "patient_id")
  
  # If there are patients to add
  if (nrow(patients) > 0){
    
    # Add new patient(s) in the subset
    tryCatch({
      last_id <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM subset_patients") %>% dplyr::pull()
      other_cols <- tibble::tibble(
        id = 1:nrow(patients) + last_id, subset_id = subset_id, creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
      )
      patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::relocate(patient_id, .after = "subset_id")
      DBI::dbAppendTable(r$db, "subset_patients", patients)
      },
      error = function(e){
        if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_inserting_data", 
          error_name = paste0("add_patients_to_subset - error_inserting_data - id = ", subset_id), category = "Error", error_report = toString(e), language = language)
        stop(translate(language, "error_inserting_data", r$words))},
      warning = function(w) if (nchar(w[1]) > 0){
        report_bug(r = r, output = output, error_message = "error_inserting_data", 
          error_name = paste0("add_patients_to_subset - error_inserting_data - id = ", subset_id), category = "Warning", error_report = toString(w), language = language)
        stop(translate(language, "error_inserting_data", r$words))}
    )
  }
  
  if (success_notification){
    show_message_bar(output, 1, "add_patients_subset_success", "success", language, r$words)
    print(translate(language, "add_patients_subset_success", r$words))
  }
}

add_patients_to_subset_new <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), patients = tibble::tibble(),
  subset_id = integer(), success_notification = FALSE, i18n = R6::R6Class()){
  
  # Check subset_id
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("add_patients_to_subset - invalid_subset_id - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("invalid_subset_id_value"))}
  )
  
  if (is.na(subset_id) | length(subset_id) == 0){
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
  
  if (is.na(subset_id) | length(subset_id) == 0){
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
