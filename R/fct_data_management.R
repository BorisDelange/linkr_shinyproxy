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
run_datamart_code <- function(output, r = shiny::reactiveValues(), datamart_id = integer(), language = "EN"){
  # Reset r$chosen_datamart to clear patient-level data & aggregated data (use the same r variables for data) 
  
  # Get code from datamart
  tryCatch(r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code),
    error = function(e){
      show_message_bar(output, 1, "fail_load_code", "severeWarning", language)
      stop(translate(language, "fail_load_code"))
    }, warning = function(w){
      show_message_bar(output, 1, "fail_load_code", "severeWarning", language)
      stop(translate(language, "fail_load_code"))
    })
  code <- r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code)
  
  # Replace %datamart_id% with real datamart_id
  code <- code %>% stringr::str_replace_all("%datamart_id%", as.character(datamart_id))
  
  # Reset r variables
  r$patients <- tibble::tibble()
  r$stays <- tibble::tibble()
  r$labs_vitals <- tibble::tibble()
  r$text <- tibble::tibble()
  r$orders <- tibble::tibble()
  
  # Load data from datamart
  
  tryCatch(eval(parse(text = code)), 
    error = function(e){
      show_message_bar(output, 1, "fail_execute_code", "severeWarning", language)
      stop(translate(language, "fail_execute_code"))
    }, warning = function(w){
      show_message_bar(output, 1, "fail_execute_code", "severeWarning", language)
      stop(translate(language, "fail_execute_code"))
    })
  
  # If data is loaded, nb of rows of r variable > 0
  if (nrow(r$patients) != 0) show_message_bar(output, 1, "success_load_patients", "success", language)
  if (nrow(r$stays) != 0) show_message_bar(output, 2, "success_load_stays", "success", language)
  if (nrow(r$labs_vitals) != 0) show_message_bar(output, 3, "success_load_labs_vitals", "success", language)
  if (nrow(r$text) != 0) show_message_bar(output, 4, "success_load_text", "success", language)
  if (nrow(r$orders) != 0) show_message_bar(output, 5, "success_load_orders", "success", language)
}

#' Add patients to a subset
#'
#' @description Add patients to a subset
#' @details ...
#' @param output Output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param patients data variable containing patients (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param erase if TRUE, erase patients contained in selected subset (logical)
#' @param language language used for error / warning messages (character, default = "EN")
#' @examples 
#' \dontrun{
#' patients <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' subset_add_patients(output = output, r = r, patients = patients, subset_id = 3, erase = FALSE, language = "EN")
#' }
add_patients_to_subset <- function(output, r = shiny::reactiveValues(), patients = tibble::tibble(), subset_id = integer(), erase = FALSE, language = "EN"){
  
  # Check subset_id
  tryCatch(subset_id <- as.integer(subset_id), 
  error = function(e){
    show_message_bar(output, 1, "invalid_subset_id_value", "severeWarning", language)
    stop(translate(language, "invalid_subset_id_value"))
  }, warning = function(w){
    show_message_bar(output, 1, "invalid_subset_id_value", "severeWarning", language)
    stop(translate(language, "invalid_subset_id_value")) 
  })
  if (is.na(subset_id) | length(subset_id) == 0){
    show_message_bar(output, 1, "invalid_subset_id_value", "severeWarning", language)
    stop(translate(language, "invalid_subset_id_value"))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "patient_id", "integer")
  
  # Check col names
  if (!identical(names(patients), "patient_id")){
    show_message_bar(output, 1, "invalid_col_names", "severeWarning", language)
    stop(translate(language, "valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(patients[[var_name]])){
      show_message_bar(output, 1, "invalid_col_types", "severeWarning", language)
      stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_integer")))
    }
  })
  
  # Transform as tibble
  tryCatch(patients <- tibble::as_tibble(patients), 
    error = function(e){
      show_message_bar(output, 1, "error_transforming_tibble", "severeWarning", language)
      stop(translate(language, "error_transforming_tibble"))
    }, warning = function(w){
      show_message_bar(output, 1, "error_transforming_tibble", "severeWarning", language)
      stop(translate(language, "error_transforming_tibble")) 
    })
  
  # Check if there are already patients in this subset
  actual_patients <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM subset_patients WHERE subset_id = ", subset_id))
  if (nrow(actual_patients) > 0){
    if (!erase){
      show_message_bar(output, 1, "subset_containing_patients_erase_false", "severeWarning", language)
      stop(translate(language, "subset_containing_patients_erase_false"))
    }
    if (erase){
      tryCatch({
        query <- DBI::dbSendStatement(r$db, paste0("DELETE FROM subset_patients WHERE subset_id = ", subset_id))
        DBI::dbClearResult(query)},
        error = function(e){
          show_message_bar(output, 1, "error_erasing_data", "severeWarning", language)
          stop(translate(language, "error_erasing_data"))
        }, warning = function(w){
          show_message_bar(output, 1, "error_erasing_data", "severeWarning", language)
          stop(translate(language, "error_erasing_data"))
      })
    }
  }
  
  # Add new patients in the subset
  tryCatch({
      last_id <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM subset_patients") %>% dplyr::pull()
      other_cols <- tibble::tibble(
        id = 1:nrow(patients) + last_id, subset_id = subset_id, creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
      )
      patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::relocate(patient_id, .after = "subset_id")
      DBI::dbAppendTable(r$db, "subset_patients", patients)
    },
    error = function(e){
      show_message_bar(output, 1, "error_inserting_data", "severeWarning", language)
      stop(translate(language, "error_inserting_data"))
    }, warning = function(w){
      show_message_bar(output, 1, "error_inserting_data", "severeWarning", language)
      stop(translate(language, "error_inserting_data"))
    }
  )
  
  show_message_bar(output, 1, "add_patients_subset_success", "success", language)
  print(translate(language, "add_patients_subset_success"))
}