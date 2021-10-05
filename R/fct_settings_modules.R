#' settings_modules 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

settings_modules_get_table <- function(prefix, module_type){
  switch(prefix,
    "patient_lvl" = switch(module_type, "module" = "patient_lvl_modules", "family" = "patient_lvl_module_families"),
    "aggregated" = switch(module_type, "module" = "aggregated_modules", "family" = "aggregated_module_families"))
}

settings_modules_get_dropdowns <- function(prefix, module_type){
  switch(prefix,
    "patient_lvl" =
      switch(module_type,
        "module" = c("module_family_id" = "patient_lvl_module_families", "parent_module_id" = "patient_lvl_modules"),
        "family" = ""),
    "aggregated" =
      switch(module_type,
        "module" = c("module_family_id" = "aggregated_module_families", "parent_module_id" = "aggregated_modules"),
        "family" = ""))
}

settings_modules_thesaurus_datatable <- function(ns, r, language, page_id, prefix, data, new_colnames = ""){
  if (nrow(data) == 0) return(data)
  
  # Order data by ID
  data <- data %>% dplyr::arrange(id) %>% dplyr::select(-deleted)
  
  # Add a column action in the DataTable
  data["action"] <- NA_character_
  
  # For each row of the dataframe :
  # - add an Action column with checkbox
  # if (nrow(data) != 0){
  #   for (i in 1:nrow(data)){
  #     data[i, "action"] <- as.character(shiny::actionButton(paste0(prefix, "_select_", data[i, 1]), "", icon = icon("plus"),
  #       onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_item_selected', this.id, {priority: 'event'})")))
  #   }
  # }
  
  data <- data %>% dplyr::rowwise() %>% dplyr::mutate(action = as.character(
    tagList(
      shiny::actionButton(paste0(prefix, "_select_", id), "", icon = icon("plus"),
        onclick = paste0("Shiny.setInputValue('", page_id, "-", prefix, "_item_selected', this.id, {priority: 'event'})")),
      shiny::actionButton(paste0(prefix, "_remove_", id), "", icon = icon("minus"),
        onclick = paste0("Shiny.setInputValue('", page_id, "-", prefix, "_item_removed', this.id, {priority: 'event'})")))))
  
  # Change name of cols 
  colnames(data) <- c(translate(language, "id"), translate(language, "thesaurus_id"), translate(language, "item_id"),
    translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
    translate(language, "datetime"), translate(language, "action"))
  
  data
}