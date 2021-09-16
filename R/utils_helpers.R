#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

tibble_to_list <- function(data, key_col, text_col){
  my_list <- list()
  for (i in 1:nrow(data)){
    my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
  }
  return(my_list)
}