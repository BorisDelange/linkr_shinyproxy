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
import_datamart <- function(output, ns = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), datamart_id = integer(), data = tibble::tibble(), 
  type = "patients", save_as_csv = TRUE, rewrite = FALSE, i18n = character(), quiet = TRUE){
  
  # Check datamart_id
  tryCatch(as.integer(datamart_id),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_datamart_id_value", 
        error_name = paste0("import_datamart - invalid_datamart_id_value - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("invalid_datamart_id_value"))}
  )
  
  # If try is a success, assign new value to data (problem with assignment inside the tryCatch)
  datamart_id <- as.integer(datamart_id)
  
  if (is.na(datamart_id) | length(datamart_id) == 0){
    show_message_bar(output, "invalid_datamart_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_datamart_id_value"))
  }
  
  # Type = c("patients", "stays", "labs_vitals", "text", "orders")
  # Check if type is valid
  if (type %not_in% c("patients", "stays", "labs_vitals", "text", "orders")){
    show_message_bar(output, "var_type_not_valid", "severeWarning", i18n = i18n, ns = ns) 
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
        d[[type]] <- readr::read_csv(path, col_types = col_types, show_col_types = FALSE)
        if (!quiet & nrow(d[[type]]) > 0) show_message_bar(output, id_message_bar, paste0("import_datamart_success_", type), "success", i18n = i18n, ns = ns)
      })
    }, 
      
      error = function(e){
        if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_csv", 
          error_name = paste0("import_datamart - error_loading_csv - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_loading_csv"))},
      warning = function(w){
        if (nchar(w[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_csv", 
          error_name = paste0("import_datamart - error_loading_csv - id = ", datamart_id), category = "Error", error_report = toString(w), i18n = i18n)
        stop(i18n$t("error_loading_csv"))}
    )
  }
  
  # Transform as tibble
  tryCatch(tibble::as_tibble(data), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
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
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    } 
    if (var_cols[[i, "type"]] == "character" & !is.character(data[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character"))) 
    }
    if (var_cols[[i, "type"]] == "numeric" & !is.numeric(data[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t( "column"), " ", var_name, " ", i18n$t("type_must_be_numeric")))
    } 
    if (var_cols[[i, "type"]] == "datetime" & !lubridate::is.POSIXct(data[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_datetime")))
    }
  })
  
  # if  save_as_csv is TRUE, save data in datamart folder
  if (save_as_csv){
    if (!file.exists(folder)) dir.create(folder, recursive = TRUE)
    if (!file.exists(path)) tryCatch(readr::write_csv(data, path),
      error = function(e){
        if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_saving_csv", 
          error_name = paste0("import_datamart - error_saving_csv - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_saving_csv"))}
    )
    if (file.exists(path) & rewrite) tryCatch({
      # file.remove(path)
      readr::write_csv(data, path)}, 
      error = function(e){
        if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_saving_csv", 
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
          error_name = paste0("import_datamart - error_linking_stays_with_thesaurus - id = ", datamart_id), category = "Error", error_report = toString(e), i18n = i18n)
        stop(i18n$t("error_linking_stays_with_thesaurus"))}
    )
  }
  
  d[[type]] <- data
  
  if (!quiet) show_message_bar(output, id_message_bar, paste0("import_datamart_success_", type), "success", i18n = i18n, ns = ns)
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
import_vocabulary_table <- function(output, ns = character(), i18n = character(), r = shiny::reactiveValues(), m = shiny::reactiveValues(),
  table_name = character(), data = tibble::tibble(), messages_bars = FALSE){
 
  if (table_name %not_in% c("concept", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
    if (messages_bars) show_message_bar(output, "invalid_vocabulary_table", "severeWarning", i18n = i18n, ns = ns)
    return(i18n$t("invalid_vocabulary_table"))
  }
  
  if (table_name == "concept") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id", "integer",
    "concept_name", "character",
    "domain_id", "character",
    "vocabulary_id", "character",
    "concept_class_id", "character",
    "standard_concept", "character",
    "concept_code", "character",
    "valid_start_date", "character",
    "valid_end_date", "character",
    "invalid_reason", "character")
  
  else if (table_name == "domain") var_cols <- tibble::tribble(
    ~name, ~type,
    "domain_id", "character",
    "domain_name", "character",
    "domain_concept_id", "integer")
  
  else if (table_name == "concept_class") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_class_id", "character",
    "concept_class_name", "character",
    "concept_class_concept_id", "integer")
  
  else if (table_name == "concept_relationship") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id_1", "integer",
    "concept_id_2", "integer",
    "relationship_id", "character",
    "valid_start_date", "character",
    "valid_end_date", "character",
    "invalid_reason", "character")
  
  else if (table_name == "relationship") var_cols <- tibble::tribble(
    ~name, ~type,
    "relationship_id", "character",
    "relationship_name", "character",
    "is_hierarchical", "character",
    "defines_ancestry", "character",
    "reverse_relationship_id", "character",
    "relationship_concept_id", "integer")
  
  else if (table_name == "concept_synonym") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id", "integer",
    "concept_synonym_name", "character",
    "language_concept_id", "integer")
  
  else if (table_name == "concept_ancestor") var_cols <- tibble::tribble(
    ~name, ~type,
    "ancestor_concept_id", "integer",
    "descendant_concept_id", "integer",
    "min_levels_of_separation", "integer",
    "max_levels_of_separation", "integer")
  
  else if (table_name == "drug_strength") var_cols <- tibble::tribble(
    ~name, ~type,
    "drug_concept_id", "integer",
    "ingredient_concept_id", "integer",
    "amount_value", "numeric",
    "amount_unit_concept_id", "integer",
    "numerator_value", "numeric",
    "numerator_unit_concept_id", "integer",
    "denominator_value", "numeric",
    "denominator_unit_concept_id", "integer",
    "box_size", "integer",
    "valid_start_date", "character",
    "valid_end_date", "character",
    "invalid_reason", "character")

  # Check col names
  
  if (!identical(names(data), var_cols$name)){
    if (messages_bars) show_message_bar(output, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    return(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(data[[var_name]])){
      if (messages_bars) show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      return(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
    else if (var_cols[[i, "type"]] == "character" & !is.character(data[[var_name]])){
      if (messages_bars) show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      return(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character")))
    }
    else if (var_cols[[i, "type"]] == "numeric" & !is.numeric(data[[var_name]])){
      if (messages_bars) show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      return(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_numeric")))
    }
  })

  # Transform as tibble
  tryCatch(data <- tibble::as_tibble(data), 
    error = function(e){
      if (nchar(e[1]) > 0 & messages_bars) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = "import_vocabulary_table - error_transforming_tibble", category = "Error", error_report = toString(e), i18n = i18n)
      return(i18n$t("error_transforming_tibble"))}
  )

  # Add data to database
  
  tables_with_primary_key <- c("concept", "domain", "concept_class", "relationship")

  # Case 1 : table has a primary key, add data not already in database, filtered by primary key
  
  if (table_name %in% tables_with_primary_key){
  
  # Check if there are duplicates in primary_keys

    primary_keys_duplicates <- data %>% dplyr::group_by_at(paste0(table_name, "_id")) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    if (primary_keys_duplicates > 0){
      if (messages_bars) show_message_bar(output, "error_multiple_primary_keys", "severeWarning", i18n = i18n, ns = ns)
      return(i18n$t("error_multiple_primary_keys"))
    }

    tryCatch({
      sql <- glue::glue_sql("SELECT {`paste0(table_name, '_id')`} FROM {`table_name`}", .con = m$db)
      actual_data <- DBI::dbGetQuery(m$db, sql)
    },
      error = function(e){
        if (nchar(e[1]) > 0 & messages_bars) report_bug(r = r, output = output, error_message = "error_get_actual_primary_keys",
          error_name = "import_vocabulary_concepts - error_get_actual_primary_keys", category = "Error", error_report = toString(e), i18n = i18n)
        return(i18n$t("error_get_actual_primary_keys"))}
    )

    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = paste0(table_name, "_id"))
  }

  # Case 2 : table has no primary key, add data if not already in database filtered with selected cols
  
  if (table_name %not_in% tables_with_primary_key){
    
    data_duplicates_cols <- switch(table_name,
      "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id"),
      "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
      "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
      "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
        "denominator_value", "denominator_unit_concept_id", "box_size")
    )
    
    data_duplicates <- data %>% dplyr::group_by_at(data_duplicates_cols) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    if (data_duplicates > 0){
      if (messages_bars) show_message_bar(output, "error_multiple_identical_values", "severeWarning", i18n = i18n, ns = ns)
      return(i18n$t("error_multiple_identical_values"))
    }
    
    tryCatch({
      sql <- glue::glue_sql("SELECT {`data_duplicates_cols`*} FROM {`table_name`}", .con = m$db)
      actual_data <- DBI::dbGetQuery(m$db, sql)
    },
      error = function(e){
        if (nchar(e[1]) > 0 & messages_bars) report_bug(r = r, output = output, error_message = "error_get_actual_primary_keys",
          error_name = "import_vocabulary_concepts - error_get_actual_primary_keys - id = ", category = "Error", error_report = toString(e), i18n = i18n)
        return(i18n$t("error_get_actual_primary_keys"))}
    )
    
    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = data_duplicates_cols)
  }
  
  # Insert data
  
  # Last ID in vocabulary table
  last_id <- get_last_row(m$db, table_name)
  
  if (nrow(data_to_insert) == 0){
    if (messages_bars) show_message_bar(output, "vocabulary_no_data_to_insert", "severeWarning", i18n = i18n, ns = ns)
    return(i18n$t("vocabulary_no_data_to_insert"))
  }
  
  else {
    data_to_insert <- data_to_insert %>% dplyr::mutate(id = 1:dplyr::n() + last_id, .before = 1)
    
    # Count rows inserted
    r$import_vocabulary_count_rows <- r$import_vocabulary_count_rows %>%
      dplyr::bind_rows(tibble::tibble(table_name = !!table_name, n_rows = nrow(data_to_insert)))

    tryCatch(DBI::dbAppendTable(m$db, table_name, data_to_insert),
      error = function(e){
        if (nchar(e[1]) > 0 & messages_bars) report_bug(r = r, output = output, error_message = "vocabulary_error_append_table",
          error_name = "import_vocabulary_table - vocabulary_error_append_table - id = ", category = "Error", error_report = toString(e), i18n = i18n)
        return(i18n$t("vocabulary_error_append_table"))}
    )
  }
  
  if (messages_bars) show_message_bar(output, "import_vocabulary_table_success", "success", i18n, ns = ns)
  return(i18n$t("import_vocabulary_table_success"))
}
