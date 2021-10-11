#' server 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

create_datamart <- function(datamart_id, data, type, save_as_csv = TRUE){
  folder <- paste0(golem::get_golem_wd(), "/data/datamart_", datamart_id)
  
  # Transform as tibble
  tryCatch(tibble::as_tibble(data), error = function(e) stop("Error transforming data to tibble"))
  
  # Type = c("patients", "stays", "labs_vitals", "text", "orders")
  # Check if type is valid
  if (type %not_in% c("patients", "stays", "labs_vitals", "text", "orders")) stop("Variable 'type' is not valid")

  # For type patients, cols & type of variable needed
  # tibble::tribble(patient_id = integer(), gender = character(), age = numeric(), dod = character())
  if (type == "patients" & !identical(names(data), c("patient_id", "gender", "age", "dod"))) stop("Column names are not valid")
  
  # if (type == "patients") tryCatch(lubridate::ymd_hms(data$dod), error = function(e) stop ("'dod' column variable type is not valid")) -> datetime_conversion
  # if (TRUE %in% is.na(datetime_conversion)) stop ("'dod' column variable type is not valid")
  if (type == "patients" & (!is.integer(data$patient_id) | !is.character(data$gender) | 
    !is.numeric(data$age) | !lubridate::is.Date(data$dod))) stop ("Column variables types are not valid")
  
  # For type stays, cols & type of variable needed
  # tibble::tribble()
  if (type == "stays" & !identical(names(data), c("patient_id", "stay_id", "unit_name", "admission_datetime", "discharge_datetime"))) stop("Column names are not valid")
  if (type == "stays" & (!is.integer(data$patient_id))) stop ("Column variables types are not valid")

  path <- paste0(folder, "/", type, ".csv")

  # if  save_as_csv is TRUE, save data in datamart folder
  if (!file.exists(folder)) dir.create(folder)
  # readr::write_csv(data, path)
  if (save_as_csv) tryCatch(readr::write_csv(data, path), error = function(e)  stop("Error saving data as csv"))
  
  data
}