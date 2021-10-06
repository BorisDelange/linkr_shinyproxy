#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# New data                               #
##########################################

data_management_new_data <- function(prefix, new_id, name, description, creator_id, datetime = as.character(Sys.time()), deleted = FALSE, 
                                     data_source_id = NA_integer_, datamart_id = NA_integer_, study_id = NA_integer_,
                                     patient_lvl_module_family_id = NA_integer_, aggregated_module_family_id = NA_integer_){
  data <- tibble::tribble(~id, ~name, ~description, new_id, name, description)
  if (prefix == "datamarts") data <- data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data_source_id))
  if (prefix == "studies") data <- data %>% dplyr::bind_cols(
    tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
                    datamart_id, patient_lvl_module_family_id, aggregated_module_family_id))
  if (prefix == "subsets") data <- data %>% dplyr::bind_cols(tibble::tribble(~study_id, study_id))
  # if (prefix == "thesaurus")
  # if (prefix == "thesaurus_items")
  data <- data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, creator_id, datetime, FALSE))
  data
}

##########################################
# Datatable options                      #
##########################################

data_management_datatable_options <- function(data, prefix, option){
  if (nrow(data) == 0) return("")
  
  data <- data %>% dplyr::bind_rows(tibble::tibble(action = character()))
  
  # Non-sortabled columns : action & id columns (except first one)
  if (option == "non_sortable"){
    if (prefix == "thesaurus_items") result <- c(which(grepl("action", names(data))) - 1)
    if (prefix != "thesaurus_items") result <- c(which(grepl("id|action", names(data))) - 1)
    result <- result[!result %in% c(0)]
  }
  
  # Disabled columns
  else if (option == "disable"){
    result <- c(1:length(names(data)))
    result <- c(0, result[-which(grepl("name|description|category", names(data)))] - 1)
  }
  
  result
}