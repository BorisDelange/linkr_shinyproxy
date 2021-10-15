
#' Get singular form of a word
#' 
#' @description Returns the singular form of a word
#' @param word Original word, in its plural form (character)
#' @param language language used for conversion (character)
#' @examples 
#' get_singular(word = "studies", language = "EN")
get_singular <- function(word = character(), language = "EN"){
  # IDs of settings pages are prefixed with "settings_", if this is the case, remove this prefix
  if (grepl("settings_", word)) word <- substr(word, nchar("settings_") + 1, nchar(word))
  switch(word, 
         "data_sources" = "data_source",
         "datamarts" = "datamart",
         "studies" = "study",
         "subsets" = "subset",
         "thesaurus" = "thesaurus",
         "thesaurus_items" = "thesaurus_item",
         "patient_lvl_module_families" = "patient_lvl_module_family",
         "aggregated_module_families" = "aggregated_module_family") -> result
  result  
}

#' Get plural form of a word
#' 
#' @description Returns the plural form of a word
#' @param word Original word, in its singular form (character)
#' @param language language used for conversion (character)
#' @examples 
#' get_plural(word = "study", language = "EN")
get_plural <- function(word = character(), language = "EN"){
  # IDs of settings pages are prefixed with "settings_", if this is the case, remove this prefix
  if (grepl("settings_", word)) word <- substr(word, nchar("settings_") + 1, nchar(word))
  switch(word, 
         "data_source" = "data_sources",
         "datamart" = "datamarts",
         "study" = "studies",
         "subset" = "subsets",
         "thesaurus" = "thesaurus",
         "thesaurus_item" = "thesaurus_items",
         "patient_lvl_module" = "patient_lvl_modules",
         "patient_lvl_module_family" = "patient_lvl_module_families",
         "aggregated_module" = "aggregated_modules",
         "aggregated_module_family" = "aggregated_module_families") -> result
  result
}