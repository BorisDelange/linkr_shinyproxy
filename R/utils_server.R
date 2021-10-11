#' server 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

create_datamart <- function(r, datamart_id, data, type, save_as_csv = TRUE, rewrite = FALSE, language = "EN"){
  # Type = c("patients", "stays", "labs_vitals", "text", "orders")
  # Check if type is valid
  if (type %not_in% c("patients", "stays", "labs_vitals", "text", "orders")) stop(translate(language, "var_type_not_valid"))
  
  folder <- paste0(golem::get_golem_wd(), "/data/datamart_", datamart_id)
  path <- paste0(folder, "/", type, ".csv")
  
  # If files already exists and we do not want to rewrite it
  if (save_as_csv & !rewrite & file.exists(path)){
    tryCatch(return(r[[type]] <- readr::read_csv(path)), error = function(e) stop(translate(language, "error_loading_csv")),
     warning = function(w) stop(translate(language, "error_loading_csv")))
  }
  
  # Transform as tibble
  tryCatch(tibble::as_tibble(data), error = function(e) stop(translate(language, "error_transforming_tibble")))

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
  if (!identical(names(data), var_cols %>% dplyr::pull(name))) stop(translate(language, "valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer") if (!is.integer(data[[var_name]])) stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_integer")))
    if (var_cols[[i, "type"]] == "character") if (!is.character(data[[var_name]])) stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_character")))
    if (var_cols[[i, "type"]] == "numeric") if (!is.numeric(data[[var_name]])) stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_numeric")))
    if (var_cols[[i, "type"]] == "datetime") if (!lubridate::is.Date(data[[var_name]])) stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_datetime")))
  })
  
 

  # if  save_as_csv is TRUE, save data in datamart folder
  if (save_as_csv){
    if (!file.exists(folder)) dir.create(folder)
    if (!file.exists(path)) tryCatch(readr::write_csv(data, path), error = function(e) stop(translate(language, "error_saving_csv")))
    if (file.exists(path) & rewrite) tryCatch({
      # file.remove(path)
      readr::write_csv(data, path)}, error = function(e) stop(translate(language, "error_saving_csv")))
  }
  
  r[[type]] <- data
}