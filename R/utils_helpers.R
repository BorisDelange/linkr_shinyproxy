#' Convert a tibble to list
#' 
#' @description Converts a tibble to a list for input options in ComboBox & Dropdowns of shiny.fluent library
#' @param data A tibble or a dataframe containing data
#' @param key_col Name of the column containing the key (character)
#' @param text_col Name of the column containing the text (character)
#' @param null_value Add a null value at the beginning (logical)
#' @param i18n Translator object from shiny.i18n library
#' @return A list with this structure : list(list(key = "my_key1", text = "my_text1"), list(key = "my_key2", text = "my_text2"))
#' @examples
#' data <- tibble::tribble(~key, ~text, "my_key1", "my_text1", "my_key2", "my_text2")
#' my_list <- convert_tibble_to_list(data = data, key_col = "key", text_col = "text", null_value = TRUE, i18n = i18n)
#' print(my_list)
convert_tibble_to_list <- function(data = tibble::tibble(), key_col = character(), text_col = character(), null_value = FALSE, i18n = character()){
  
  # Create a null / an empty value (used in dropdowns)
  if (null_value) my_list <- list(list(key = "", text = i18n$t("none")))
  if (!null_value) my_list <- list()
  
  # If our data is not empty, for each row append the list
  if (nrow(data) != 0){
    for (i in 1:nrow(data)){
      my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
    }
  }
  my_list
}

#' Coalesce2
#' 
#' @description Returns a NA value
#' @param type Type of the variable c("char", "int") (character)
#' @param x Variable
#' @return Return NA if variable is null or empty, returns the variable if not null & not empty
#' @examples 
#' coalesce2("char", "my_char")

coalesce2 <- function(type, x){
  if (length(x) >= 2) return(x)
  if (type == "int"){
    if (is.null(x)) return(NA_integer_)
    if (length(x) == 0) return(NA_integer_)
    if (x == "") return(NA_integer_)
    return(tryCatch(as.integer(x)))
  }
  if (type == "char"){
    if (is.null(x)) return(NA_character_)
    if(length(x) == 0) return(NA_character_)
    if (x == "") return(NA_character_)
    return(tryCatch(as.character(x)))
  }
}

`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)