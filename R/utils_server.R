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

  # Data cols
  tibble::tribble(
    ~var, ~cols,
    "patients", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "gender", "character",
      "age", "numeric",
      "dod", "datetime"),
    "stays", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "stay_id", "integer",
      "unit_name", "character",
      "admission_datetime", "datetime",
      "discharge_datetime", "datetime"),
    "labs_vitals", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "thesaurus_name", "character",
      "item_id", "integer",
      "datetime_start", "datetime",
      "datetime_stop", "datetime",
      "value", "character",
      "value_num", "numeric",
      "comments", "character"),
    "text", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "thesaurus_name", "character",
      "item_id", "integer",
      "datetime_start", "datetime",
      "datetime_stop", "datetime",
      "value", "character",
      "comments", "character"),
    "order", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "thesaurus_name", "character",
      "item_id", "integer",
      "datetime_start", "datetime",
      "datetime_stop", "datetime",
      "route", "character",
      "continuous", "integer",
      "duration", "numeric",
      "duration_unit", "character",
      "amount", "numeric",
      "amount_unit", "character",
      "rate", "numeric",
      "rate_unit", "character",
      "concentration", "numeric",
      "concentration_unit", "character",
      "comments", "character")
    ) -> data_cols
  
  # Check columns var types
  var_cols <- data_cols %>% dplyr::filter(var == type) %>% dplyr::pull(cols)
  var_cols <- var_cols[[1]]
  if (!identical(names(data), var_cols %>% dplyr::pull(name))) stop("Column names are not valid. Valid col names are : ", toString(var_cols %>% dplyr::pull(name)))
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer") if (!is.integer(data[[var_name]])) stop(paste0("Column ", var_name, " type must be an integer"))
    if (var_cols[[i, "type"]] == "character") if (!is.character(data[[var_name]])) stop(paste0("Column ", var_name, " type must be a character"))
    if (var_cols[[i, "type"]] == "numeric") if (!is.numeric(data[[var_name]])) stop(paste0("Column ", var_name, " type must be a numeric"))
    if (var_cols[[i, "type"]] == "datetime") if (!lubridate::is.Date(data[[var_name]])) stop(paste0("Column ", var_name, " type must be a datetime"))
  })
  
  path <- paste0(folder, "/", type, ".csv")

  # if  save_as_csv is TRUE, save data in datamart folder
  if (!file.exists(folder)) dir.create(folder)
  # readr::write_csv(data, path)
  if (save_as_csv) tryCatch(readr::write_csv(data, path), error = function(e)  stop("Error saving data as csv"))
  
  data
}