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
    if (rm_deleted_rows) data <- data %>% dplyr::filter(!Deleted)
    if (nrow(data) != 0){
      for (i in 1:nrow(data)){
        my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
      }
    }
  }
  return(my_list)
}
