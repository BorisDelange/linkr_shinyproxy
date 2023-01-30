#' Import a datamart
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param datamart_id ID of the datamart, used to create directory in data/ (eg : datamart_3)
#' @param data data variable (data.frame or tibble)
#' @param type type or name of data to create c("patients", "stays", "labs_vitals", "text", "orders")
#' @param save_as_csv save or not the data to CSV file (logical, default = TRUE)
#' @param rewrite if save_as_csv is TRUE, rewrite or not existing CSV file (logical, default = FALSE)
#' @param language language used for error / warning messages (character, default = "EN")
#' @description Load +/- save data when a datamart is chosen by the user.
#' @details The function is used in a datamart code and is launched each time a user selects a datamart. \cr
#' You can choose to \strong{load data each time} the function is used with save_as_csv set to FALSE (eg when datamart is small and the
#' connection to source database is good) or you can \strong{save the data in a CSV file} with save_as_csv set to TRUE. \cr
#' Basically, 5 data variables are created for each datamart (distinct values of 'type' parameter).\cr\cr
#' Columns needed for each data type :\cr\cr
#' \strong{type = "patients"} :\cr
#' \itemize{
#' \item{patient_id = integer}
#' \item{gender = character}
#' \item{age = numeric}
#' \item{dod = datetime}
#' }
#' \strong{type = "stays"} :\cr
#' \itemize{
#' \item{patient_id = integer}
#' \item{stay_id = integer}
#' \item{thesaurus_name = character}
#' \item{item_id = integer}
#' \item{admission_datetime = datetime}
#' \item{discharge_datetime = datetime}
#' }
#' \strong{type = "labs_vitals"} :\cr
#' \itemize{
#' \item{patient_id = integer}
#' \item{thesaurus_name = character}
#' \item{item_id = integer}
#' \item{datetime_start = datetime}
#' \item{datetime_stop = datetime}
#' \item{value = character}
#' \item{value_num = numeric}
#' \item{unit = character}
#' \item{comments = character}
#' }
#' \strong{type = "text"} :\cr
#' \itemize{
#' \item{patient_id = integer}
#' \item{thesaurus_name = character}
#' \item{item_id = integer}
#' \item{datetime_start = datetime}
#' \item{datetime_stop = datetime}
#' \item{value = character}
#' \item{comments = character}
#' }
#' \strong{type = "orders"} :\cr
#' \itemize{
#' \item{patient_id = integer}
#' \item{thesaurus_name = character}
#' \item{item_id = integer}
#' \item{datetime_start = datetime}
#' \item{datetime_stop = datetime}
#' \item{route = character}
#' \item{continuous = integer}
#' \item{amount = numeric}
#' \item{amount_unit = character}
#' \item{rate = numeric}
#' \item{rate_unit = character}
#' \item{concentration = numeric}
#' \item{concentration_unit = character}
#' \item{comments = character}
#' }
#' @examples
#' \dontrun{
#' patients <- tibble::tribble(~patient_id, ~gender, ~age, ~dod, 44565L, "F", 45, "2021-05-01 00:00:00") %>%
#'   dplyr::mutate_at("dod", lubridate::ymd_hms)
#'     
#' import_datamart(output = output, r = r, datamart_id = 5, data = patients, type = "patients", 
#'   save_as_csv = FALSE, rewrite = FALSE, language = language)
#' }
import_datamart <- function(output, ns = shiny::NS(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), datamart_id = integer(), data = tibble::tibble(), 
  type = "patients", save_as_csv = TRUE, rewrite = FALSE, i18n = R6::R6Class(), quiet = TRUE){
  
  # Check datamart_id
  tryCatch(as.integer(datamart_id),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "invalid_datamart_id_value", 
        error_name = paste0("import_datamart - invalid_datamart_id_value - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("invalid_datamart_id_value"))}
  )
  
  # If try is a success, assign new value to data (problem with assignment inside the tryCatch)
  datamart_id <- as.integer(datamart_id)
  
  if (is.na(datamart_id) | length(datamart_id) == 0){
    show_message_bar_new(output, 1, "invalid_datamart_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_datamart_id_value"))
  }
  
  # Type = c("patients", "stays", "labs_vitals", "text", "orders")
  # Check if type is valid
  if (type %not_in% c("patients", "stays", "labs_vitals", "text", "orders")){
    show_message_bar_new(output, 1, "var_type_not_valid", "severeWarning", i18n = i18n, ns = ns) 
    stop(i18n$t("var_type_not_valid"))
  }
  id_message_bar <- switch(type, "patients" = 1, "stays" = 2, "labs_vitals" = 3, "text" = 4, "orders" = 5, "diagnoses" = 6)
  
  # If a datamarts_folder is provided, take this value
  # Take package working directory else
  folder <- paste0(r$app_folder, "/datamarts/", datamart_id)
  path <- paste0(folder, "/", type, ".csv")
  
  # If files already exists and we do not want to rewrite it
  if (save_as_csv & !rewrite & file.exists(path)){
    tryCatch({
      return({
        col_types <- switch(type, 
          "patients" = "icT",
          "stays" = "iinciTT",
          "labs_vitals" = "iciTTcncc",
          "text" = "iciTTcc",
          "orders" = "iciTTcincncncc")
        d[[type]] <- readr::read_csv(path, col_types = col_types)
        if (!quiet & nrow(d[[type]]) > 0) show_message_bar_new(output, id_message_bar, paste0("import_datamart_success_", type), "success", i18n = i18n, ns = ns)
      })
    }, 
      
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_loading_csv", 
          error_name = paste0("import_datamart - error_loading_csv - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_loading_csv"))},
      warning = function(w){
        if (nchar(w[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_loading_csv", 
          error_name = paste0("import_datamart - error_loading_csv - id = ", datamart_id), category = "Error", error_report = toString(w), i18n = i18n)
        stop(i18n$t("error_loading_csv"))}
    )
  }
  
  # Transform as tibble
  tryCatch(tibble::as_tibble(data), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("import_datamart - error_transforming_tibble - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  data <- tibble::as_tibble(data)
  
  # Data cols
  tibble::tribble(
    ~var, ~cols,
    "patients", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "gender", "character",
      "dod", "datetime"),
    "stays", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "stay_id", "integer",
      "age", "numeric",
      "thesaurus_name", "character",
      "item_id", "integer",
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
      "unit", "character",
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
    "orders", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "thesaurus_name", "character",
      "item_id", "integer",
      "datetime_start", "datetime",
      "datetime_stop", "datetime",
      "route", "character",
      "continuous", "integer",
      "amount", "numeric",
      "amount_unit", "character",
      "rate", "numeric",
      "rate_unit", "character",
      "concentration", "numeric",
      "concentration_unit", "character",
      "comments", "character"),
    "diagnoses", tibble::tribble(
      ~name, ~type,
      "patient_id", "integer",
      "thesaurus_name", "character",
      "item_id", "integer",
      "datetime_start", "datetime",
      "datetime_stop", "datetime",
      "comments", "character"),
  ) -> data_cols
  
  # Check columns var types
  var_cols <- data_cols %>% dplyr::filter(var == type) %>% dplyr::pull(cols)
  var_cols <- var_cols[[1]]
  if (!identical(names(data), var_cols %>% dplyr::pull(name))) stop(paste0(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name))))
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(data[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    } 
    if (var_cols[[i, "type"]] == "character" & !is.character(data[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character"))) 
    }
    if (var_cols[[i, "type"]] == "numeric" & !is.numeric(data[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t( "column"), " ", var_name, " ", i18n$t("type_must_be_numeric")))
    } 
    if (var_cols[[i, "type"]] == "datetime" & !lubridate::is.POSIXct(data[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_datetime")))
    }
  })
  
  # if  save_as_csv is TRUE, save data in datamart folder
  if (save_as_csv){
    if (!file.exists(folder)) dir.create(folder, recursive = TRUE)
    if (!file.exists(path)) tryCatch(readr::write_csv(data, path),
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_saving_csv", 
          error_name = paste0("import_datamart - error_saving_csv - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_saving_csv"))}
    )
    if (file.exists(path) & rewrite) tryCatch({
      # file.remove(path)
      readr::write_csv(data, path)}, 
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_saving_csv", 
          error_name = paste0("import_datamart - error_saving_csv - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_saving_csv"))}
    )
  }
  
  # If type is stays, link with thesaurus
  if (type == "stays"){
    tryCatch({
      data <- data %>%
        dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      # For each thesaurus, left join with corresponding unit name
      for (thesaurus_id in data %>% dplyr::distinct(thesaurus_id) %>% dplyr::pull()){
        sql <- glue::glue_sql("SELECT * FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id}", .con = r$db)
        thesaurus_items <- DBI::dbGetQuery(r$db, sql)
        data <- data %>%
          dplyr::left_join(thesaurus_items %>% dplyr::select(thesaurus_id, item_id, name, display_name), by = c("thesaurus_id", "item_id")) %>%
          dplyr::mutate(unit_name = ifelse((is.na(display_name) | display_name == ""), name, display_name)) %>%
          dplyr::select(-name, -display_name)
      }
    },
      error = function(e){
        if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_linking_stays_with_thesaurus", 
          error_name = paste0("import_datamart - error_linking_stays_with_thesaurus - id = ", datamart_id), category = "Error", error_report = toString(e), language = language)
        stop(i18n$t("error_linking_stays_with_thesaurus"))}
    )
  }
  
  d[[type]] <- data
  
  if (!quiet) show_message_bar_new(output, id_message_bar, paste0("import_datamart_success_", type), "success", i18n = i18n, ns = ns)
}

#' Import a thesaurus
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param thesaurus_id ID of the thesaurus, used to update thesaurus table (integer)
#' @param thesaurus thesaurus data variable (data.frame or tibble)
#' @param language language used for error / warning messages (character, default = "EN")
#' @description Add new thesaurus items to database
#' @details The function is used in a thesaurus code, it is launched only when you click on "Run code" on the thesaurus page.\cr\cr
#' Columns needed  :\cr\cr
#' \itemize{
#' \item{item_id = integer}
#' \item{name = character}
#' \item{display_name = character}
#' \item{category = character}
#' \item{unit = character}
#' }
#' @examples
#' \dontrun{
#' thesaurus <- tibble::tribble(~item_id, ~name, ~display_name, ~category, ~unit,
#'   44543L, "Heart rate", "HR", "Vitals", "bpm",
#'   46531L, "Respiratory rate", "RR", "Vitals", "cpm")
#'   
#' import_thesaurus(output = output, r = r, thesaurus_id = 5, thesaurus = thesaurus, language = language)
#' }
import_thesaurus <- function(output, r = shiny::reactiveValues(), thesaurus_id = integer(), thesaurus = tibble::tibble(), language = "EN"){
  # Check thesaurus_id
  tryCatch(thesaurus_id <- as.integer(thesaurus_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_thesaurus_id_value", 
        error_name = paste0("import_thesaurus - invalid_thesaurus_id_value - id = ", thesaurus_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "invalid_thesaurus_id_value", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "invalid_thesaurus_id_value", 
        error_name = paste0("import_thesaurus - invalid_thesaurus_id_value - id = ", thesaurus_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "invalid_thesaurus_id_value", r$words))}
  )
  
  if (is.na(thesaurus_id) | length(thesaurus_id) == 0){
    show_message_bar(output, 1, "invalid_thesaurus_id_value", "severeWarning", language, r$words)
    stop(translate(language, "invalid_thesaurus_id_value", r$words))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "item_id", "integer",
    "name", "character",
    "display_name", "character",
    "category", "character",
    "unit", "character")
  
  # Check col names
  if (!identical(names(thesaurus), c("item_id", "name", "display_name", "category", "unit"))){
    show_message_bar(output, 1, "invalid_col_names", "severeWarning", language, r$words)
    stop(translate(language, "valid_col_names_are", r$words), toString(var_cols %>% dplyr::pull(name)))
  }
    
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(thesaurus[[var_name]])){
      show_message_bar(output, 1, "invalid_col_types", "severeWarning", language, r$words)
      stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_integer", r$words)))
    }
    if (var_cols[[i, "type"]] == "character" & !is.character(thesaurus[[var_name]])){
      show_message_bar(output, 1, "invalid_col_types", "severeWarning", language, r$words)
      stop(paste0(translate(language, "column"), " ", var_name, " ", translate(language, "type_must_be_character", r$words)))
    }
  })
  
  # Transform as tibble
  tryCatch(thesaurus <- tibble::as_tibble(thesaurus), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("import_thesaurus - error_transforming_tibble - id = ", thesaurus_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("import_thesaurus - error_transforming_tibble - id = ", thesaurus_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "error_transforming_tibble", r$words))}
  )
  
  # Check if there are no duplicates in items_id
  items_duplicates <- thesaurus %>% dplyr::group_by(item_id) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
  if (items_duplicates > 0){
    show_message_bar(output, 1, "error_multiple_item_id", "severeWarning", language, r$words)
    stop(translate(language, "error_multiple_item_id", r$words))
  }
  
  # Get actual items of this thesaurus saved in the database
  tryCatch(actual_items <- 
    DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", thesaurus_id, " AND deleted IS FALSE")) %>% dplyr::select(item_id),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_get_actuel_items", 
        error_name = paste0("import_thesaurus - error_get_actuel_items - id = ", thesaurus_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "error_get_actuel_items", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_get_actuel_items", 
        error_name = paste0("import_thesaurus - error_get_actuel_items - id = ", thesaurus_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "error_get_actuel_items", r$words))}
  )
  
  # Get items to insert with an anti-join
  items_to_insert <- thesaurus %>% dplyr::anti_join(actual_items, by = "item_id")
  
  # Last ID in thesaurus table
  last_id <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) AS id FROM thesaurus_items") %>% dplyr::pull()
  
  if (nrow(items_to_insert) == 0){
    show_message_bar(output, 1, "thesaurus_no_items_to_insert", "severeWarning", language, r$words)
    stop(translate(language, "thesaurus_no_items_to_insert", r$words)) 
  }
  
  items_to_insert <- 
    items_to_insert %>% 
    dplyr::transmute(
      id = 1:dplyr::n() + last_id, 
      thesaurus_id = !!thesaurus_id, 
      item_id, 
      name, 
      display_name, 
      category,
      unit,
      datetime = as.character(Sys.time()), 
      deleted = FALSE)
  
  tryCatch(DBI::dbAppendTable(r$db, "thesaurus_items", items_to_insert),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "thesaurus_error_append_table", 
        error_name = paste0("import_thesaurus - thesaurus_error_append_table - id = ", thesaurus_id), category = "Error", error_report = toString(e), language = language)
      stop(translate(language, "thesaurus_error_append_table", r$words))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "thesaurus_error_append_table", 
        error_name = paste0("import_thesaurus - thesaurus_error_append_table - id = ", thesaurus_id), category = "Warning", error_report = toString(w), language = language)
      stop(translate(language, "thesaurus_error_append_table", r$words))}
  )
  
  show_message_bar(output, 1, "import_thesaurus_success", "success", language, r$words)
  # print(translate(language, "import_thesaurus_success", r$words))
}

import_thesaurus_new <- function(output, ns = shiny::NS(), r = shiny::reactiveValues(), thesaurus_id = integer(), category = "character", thesaurus = tibble::tibble(), i18n = R6::R6Class()){
  # Check thesaurus_id
  tryCatch(thesaurus_id <- as.integer(thesaurus_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "invalid_thesaurus_id_value", 
        error_name = paste0("import_thesaurus - invalid_thesaurus_id_value - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("invalid_thesaurus_id_value"))}
  )
  
  if (is.na(thesaurus_id) | length(thesaurus_id) == 0){
    show_message_bar_new(output, 1, "invalid_thesaurus_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_thesaurus_id_value"))
  }
  
  if (category %not_in% c("items", "items_mapping")){
    show_message_bar_new(output, 1, "invalid_category", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_category"))
  }
  
  if (category == "items") var_cols <- tibble::tribble(
    ~name, ~type,
    "item_id", "integer",
    "name", "character",
    "display_name", "character",
    "unit", "character")
  else if (category == "items_mapping") var_cols <- tibble::tribble(
    ~name, ~type,
    "item_id_1", "integer",
    "relation_id", "integer",
    "item_id_2", "integer")
  
  # Check col names
  if (category == "items" & !identical(names(thesaurus), c("item_id", "name", "display_name", "unit"))){
    show_message_bar_new(output, 1, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  if (category == "items_mapping" & !identical(names(thesaurus), c("item_id_1", "relation_id", "item_id_2"))){
    show_message_bar_new(output, 1, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(thesaurus[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
    if (var_cols[[i, "type"]] == "character" & !is.character(thesaurus[[var_name]])){
      show_message_bar_new(output, 1, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character")))
    }
  })
  
  # Transform as tibble
  tryCatch(thesaurus <- tibble::as_tibble(thesaurus), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("import_thesaurus - error_transforming_tibble - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  # Check if there are no duplicates in items_id
  if (category == "items"){
    items_duplicates <- thesaurus %>% dplyr::group_by(item_id) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    if (items_duplicates > 0){
      show_message_bar_new(output, 1, "error_multiple_item_id", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("error_multiple_item_id"))
    }
  }
  else if (category == "items_mapping"){
    items_duplicates <- thesaurus %>% dplyr::group_by(item_id_1, relation_id, item_id_2) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    if (items_duplicates > 0){
      show_message_bar_new(output, 1, "error_duplicate_mappings", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("error_duplicate_mappings"))
    }
  }
  
  if (category == "items"){
    # Get actual items of this thesaurus saved in the database
    tryCatch({
      sql <- glue::glue_sql("SELECT item_id FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND deleted IS FALSE", .con = r$db)
      actual_items <- DBI::dbGetQuery(r$db, sql)
    },
    error = function(e){
      if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_get_actuel_items", 
        error_name = paste0("import_thesaurus - error_get_actuel_items - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("error_get_actuel_items"))}
    )
    
    # Get items to insert with an anti-join
    items_to_insert <- thesaurus %>% dplyr::anti_join(actual_items, by = "item_id")
    
    # Last ID in thesaurus table
    last_id <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) AS id FROM thesaurus_items") %>% dplyr::pull()
    
    if (nrow(items_to_insert) == 0){
      show_message_bar_new(output, 1, "thesaurus_no_items_to_insert", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("thesaurus_no_items_to_insert")) 
    }
    
    items_to_insert <- 
      items_to_insert %>% 
      dplyr::transmute(
        id = 1:dplyr::n() + last_id,
        thesaurus_id = !!thesaurus_id,
        item_id,
        name,
        display_name,
        unit,
        datetime = as.character(Sys.time()),
        deleted = FALSE)
    
    tryCatch(DBI::dbAppendTable(r$db, "thesaurus_items", items_to_insert),
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "thesaurus_error_append_table", 
          error_name = paste0("import_thesaurus - thesaurus_error_append_table - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("thesaurus_error_append_table"))}
    )
  }
  
  if (category == "items_mapping"){
    # Get actual items of this thesaurus saved in the database
    tryCatch({
      sql <- glue::glue_sql(paste0("SELECT item_id_1, relation_id, item_id_2 FROM thesaurus_items_mapping WHERE ",
      "thesaurus_id_1 = {thesaurus_id} AND thesaurus_id_2 = {thesaurus_id} AND deleted IS FALSE"), .con = r$db)
      actual_items <- DBI::dbGetQuery(r$db, sql)
    },
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "error_get_actuel_items", 
          error_name = paste0("import_thesaurus - error_get_actuel_items - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_get_actuel_items"))}
    )
    
    # Get items to insert with an anti-join
    items_to_insert <- thesaurus %>% dplyr::anti_join(actual_items, by = c("item_id_1", "relation_id", "item_id_2"))
    
    # Last ID in thesaurus table
    last_id <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) AS id FROM thesaurus_items_mapping") %>% dplyr::pull()
    
    if (nrow(items_to_insert) == 0){
      show_message_bar_new(output, 1, "thesaurus_no_items_to_insert", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("thesaurus_no_items_to_insert")) 
    }
    
    items_to_insert <- 
      items_to_insert %>% 
      dplyr::transmute(
        id = 1:dplyr::n() + last_id,
        category = "import_thesaurus_mapping",
        thesaurus_id_1 = !!thesaurus_id,
        item_id_1,
        thesaurus_id_2 = !!thesaurus_id,
        item_id_2,
        relation_id,
        creator_id = NA_integer_,
        datetime = as.character(Sys.time()),
        deleted = FALSE)
    
    tryCatch(DBI::dbAppendTable(r$db, "thesaurus_items_mapping", items_to_insert),
      error = function(e){
        if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "thesaurus_error_append_table", 
          error_name = paste0("import_thesaurus - thesaurus_error_append_table - id = ", thesaurus_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("thesaurus_error_append_table"))}
    )
  }
  
  show_message_bar_new(output, 1, "import_thesaurus_success", "success", i18n, ns = ns)
}