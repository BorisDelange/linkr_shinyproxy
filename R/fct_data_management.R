#' Run datamart code 
#'
#' @description Runs datamart code
#' @param output Output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param datamart_id ID of datamart containing the code
#' @param language language used for error / warning messages (character, default = language)
#' @examples 
#' \dontrun{
#' run_datamart_code(output = output, r = r, datamart_id = 3)
#' }
run_datamart_code <- function(output, r = shiny::reactiveValues(), datamart_id = integer(), language = "EN"){
  # Reset r$chosen_datamart to clear patient-level data & aggregated data (use the same r variables for data) 
  
  # Get code from datamart
  tryCatch(code <- r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code),
    error = function(e){
      message_bar(output, 1, "fail_load_code", "severeWarning", language)
      stop(translate(language, "fail_load_code"))
    }, warning = function(w){
      message_bar(output, 1, "fail_load_code", "severeWarning", language)
      stop(translate(language, "fail_load_code"))
    })
  
  # Reset r variables
  r$patients <- tibble::tibble()
  r$stays <- tibble::tibble()
  r$labs_vitals <- tibble::tibble()
  r$text <- tibble::tibble()
  r$orders <- tibble::tibble()
  
  # Load data from datamart
  
  tryCatch(eval(parse(text = code)), 
    error = function(e){
      message_bar(output, 1, "fail_execute_code", "severeWarning", language)
      stop(translate(language, "fail_execute_code"))
    }, warning = function(w){
      message_bar(output, 1, "fail_execute_code", "severeWarning", language)
      stop(translate(language, "fail_execute_code"))
    })
  
  # If data is loaded, nb of rows of r variable > 0
  if (nrow(r$patients) != 0) message_bar(output, 1, "success_load_patients", "success", language)
  if (nrow(r$stays) != 0) message_bar(output, 2, "success_load_stays", "success", language)
  if (nrow(r$labs_vitals) != 0) message_bar(output, 3, "success_load_labs_vitals", "success", language)
  if (nrow(r$text) != 0) message_bar(output, 4, "success_load_text", "success", language)
  if (nrow(r$orders) != 0) message_bar(output, 5, "success_load_orders", "success", language)
}

#' Add patients to a subset
#'
subset_add_patients <- function(output, r = shiny::reactiveValues(), patients = tibble::tibble(), subset_id = integer()){
  
}