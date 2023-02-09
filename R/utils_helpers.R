#' Convert a tibble to list
#' 
#' @param data a tibble or a dataframe containing data
#' @param key_col name of the column containing the key (character)
#' @param text_col name of the column containing the text (character)
#' @param null_value add a null value (logical)
#' @param language language used (character)
#' @param words words used for translations (tibble)
#' @return A list with this structure : list(list(key = "my_key1", text = "my_text1"), list(key = "my_key2", text = "my_text2"))
#' @examples
#' data <- tibble::tribble(~key, ~text, "my_key1", "my_text1", "my_key2", "my_text2")
#' my_list <- convert_tibble_to_list(data = data, key_col = "key", text_col = "text", null_value = TRUE, language = "EN")
#' print(my_list)

convert_tibble_to_list <- function(data = tibble::tibble(), key_col = character(), text_col = character(), null_value = FALSE, language = "EN", words = tibble::tibble()){

  # Create a null / an empty value (used in dropdowns)
  if (null_value) my_list <- list(list(key = "", text = translate(language, "none", words)))
  if (!null_value) my_list <- list()
  
  # If our data is not empty, for each row append the list
  if (nrow(data) != 0){
    for (i in 1:nrow(data)){
      my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
    }
  }
  my_list
}

convert_tibble_to_list_new <- function(data = tibble::tibble(), key_col = character(), text_col = character(), null_value = FALSE, i18n = R6::R6Class()){
  
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

# Delete asap
id_get_other_name <- function(id, type, language = NULL){
  if (grepl("settings_", id)) id <- substr(id, nchar("settings_") + 1, nchar(id))
  
  if (type == "singular_form"){
    switch(id, 
           "data_sources" = "data_source",
           "datamarts" = "datamart",
           "studies" = "study",
           "subsets" = "subset",
           "thesaurus" = "thesaurus",
           "thesaurus" = "thesaurus_items",
           "patient_lvl_module_families" = "patient_lvl_module_family",
           "aggregated_module_families" = "aggregated_module_family") -> result
  }
  
  if (type == "plural_form"){
    switch(id, 
           "data_source" = "data_sources",
           "datamart" = "datamarts",
           "study" = "studies",
           "subset" = "subsets",
           "thesaurus" = "thesaurus",
           "thesaurus" = "thesaurus_items",
           "patient_lvl_module" = "patient_lvl_modules",
           "patient_lvl_module_family" = "patient_lvl_module_families",
           "aggregated_module" = "aggregated_modules",
           "aggregated_module_family" = "aggregated_module_families") -> result
  }
  
  if (type == "colnames_text_version"){
    if (id %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
      result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"))
      c(result, switch(id,
        "datamarts" = translate(language, "data_source"),
        "studies" = c(translate(language, "datamart"), translate(language, "patient_lvl_module_family"),
                               translate(language, "aggregated_module_family")),
        "subsets" = translate(language, "study"),
        "thesaurus" = translate(language, "data_sources"))) -> result
      result <- c(result, translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))
    }
    if (id == "thesaurus_items"){
      result <- c(translate(language, "id"), translate(language, "thesaurus_id"), translate(language, "item_id"), translate(language, "name"),
                  translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
                  translate(language, "datetime"), translate(language, "action"))
    }
  }
  
  if (type == "options_by_cat"){
    result <- switch(id,
                     "datamarts" = c("show_only_aggregated_data", "user_allowed_read"),
                     "studies" = c("", "user_allowed_read"))
  }
  result
}

#' Coalesce
#' 
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
