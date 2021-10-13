#' Run datamart code 
#'
#'
run_datamart_code <- function(r = shiny::reactiveValues(), datamart_id = integer()){
  # Reset r$chosen_datamart to clear patient-level data & aggregated data (use the same r variables for data) 
  
  # Get code from datamart
  tryCatch(code <- r$code %>% dplyr::filter(category == "datamart" & link_id == datamart_id) %>% dplyr::pull(code),
           error = function(e) stop("fail load code"), warning = function(w) stop())
  
  # Reset r variables
  r$patients <- tibble::tibble()
  r$stays <- tibble::tibble()
  r$labs_vitals <- tibble::tibble()
  r$text <- tibble::tibble()
  r$orders <- tibble::tibble()
  
  # Load data from datamart
  
  tryCatch(eval(parse(text = code)), 
           error = function(e) stop("fail execute code"), warning = function(w) stop())
  
  # If data is loaded, nb of rows of r$patients > 0
  if (nrow(r$patients) != 0) print("success loading patients")
  if (nrow(r$stays) != 0) print("success loading stays")
  if (nrow(r$labs_vitals) != 0) print("success loading labs_vitals")
  if (nrow(r$text) != 0) print("success loading text")
  if (nrow(r$orders) != 0) print("success loading orders")
}