#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

tibble_to_list <- function(data, key_col, text_col, rm_deleted_rows = FALSE, null_value = FALSE, language = "EN"){
  if (null_value) my_list <- list(list(key = "", text = translate(language, "none")))
  if (!null_value) my_list <- list()
  if (nrow(data) != 0){
    if (rm_deleted_rows) data <- data %>% dplyr::filter(!deleted)
    if (nrow(data) != 0){
      for (i in 1:nrow(data)){
        my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
      }
    }
  }
  return(my_list)
}

id_get_other_name <- function(id, type, language = NULL){
  if (grepl("settings_", id)) id <- substr(id, nchar("settings_") + 1, nchar(id))
  
  if (type == "singular_form"){
    switch(id, 
           "data_sources" = "data_source",
           "datamarts" = "datamart",
           "studies" = "study",
           "subsets" = "subset",
           "patient_lvl_module_families" = "patient_lvl_module_family",
           "aggregated_module_families" = "aggregated_module_family") -> result
  }
  
  if (type == "plural_form"){
    switch(id, 
           "data_source" = "data_sources",
           "datamart" = "datamarts",
           "study" = "studies",
           "subset" = "subsets",
           "patient_lvl_module_family" = "patient_lvl_module_families",
           "aggregated_module_family" = "aggregated_module_families") -> result
  }
  
  if (type == "colnames_text_version"){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"))
    c(result, switch(id,
      "datamarts" = translate(language, "data_source"),
      "studies" = c(translate(language, "datamart"), translate(language, "patient_lvl_module_family"),
                             translate(language, "aggregated_module_family")),
      "subsets" = translate(language, "study"))) -> result
    result <- c(result, translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))
  }
  
  if (type == "options_by_cat"){
    result <- switch(id,
                     "datamarts" = c("show_only_aggregated_data", "user_allowed_read"),
                     "studies" = c("", "user_allowed_read"))
  }
  result
}
