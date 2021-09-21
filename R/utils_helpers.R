#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

tibble_to_list <- function(data, key_col, text_col, rm_deleted_rows = FALSE){
  my_list <- list()
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

id_get_other_name <- function(id, type){
  if (type == "data_var"){
    switch(id, 
           "settings_data_sources" = "data_sources_data",
           "settings_datamarts" = "datamarts_data",
           "settings_studies" = "studies_data",
           "settings_subsets" = "subsets_data") -> result
  }
  if (type == "singular_form"){
    switch(id, 
           "settings_data_sources" = "data_source",
           "settings_datamarts" = "datamart",
           "settings_studies" = "study",
           "settings_subsets" = "subset") -> result
  }
  result
}