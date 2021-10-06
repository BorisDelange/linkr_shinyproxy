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

settings_modules_thesaurus_cache <- function(r, prefix, page_id, data){
  
  if (prefix == "patient_lvl"){
    
    data %>%
      dplyr::left_join(
        DBI::dbGetQuery(r$db, "SELECT * FROM cache WHERE category = 'modules_patient_lvl_thesaurus_items'") %>% 
          dplyr::select(id = link_id, action = value),
        by = "id") -> data
    
    # Missing data in cache, reload cache
    reload_cache <- FALSE
    if ("action" %not_in% names(data)) reload_cache <- TRUE
    if ("action" %in% names(data)){
      if (NA_character_ %in% data$action) reload_cache <- TRUE
    }
    
    if (reload_cache){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(action = as.character(
        tagList(
          shiny::actionButton(paste0(prefix, "_select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", page_id, "-", prefix, "_item_selected', this.id, {priority: 'event'})")),
          shiny::actionButton(paste0(prefix, "_remove_", id), "", icon = icon("minus"),
            onclick = paste0("Shiny.setInputValue('", page_id, "-", prefix, "_item_removed', this.id, {priority: 'event'})")))))
      
      last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM cache") %>% dplyr::pull()
      DBI::dbAppendTable(r$db, "cache",
       data %>% 
         dplyr::transmute(
           category = "modules_patient_lvl_thesaurus_items",
           link_id = id,
           value = action,
           datetime = as.character(Sys.time())) %>%
         dplyr::mutate(id = 1:dplyr::n() + last_row) %>%
         dplyr::relocate(id)
      )
    }
  }
  
  data
}

settings_modules_datatable_data <- function(ns, r, type, prefix, data, new_colnames = ""){
  if (nrow(data) == 0) return(data)
  
  # Order data by ID
  data <- data %>% dplyr::arrange(id) %>% dplyr::select(-deleted)
  
  # Link with module, plugin & thesaurus names
  if (type == "elements_management"){
    data <- data %>%
      # dplyr::left_join(r[[paste0(prefix, "_modules")]] %>% dplyr::select(module_id = id, module_name = name), by = "module_id") %>%
      # dplyr::relocate(module_name, .after = "name") %>%
      # dplyr::left_join(r$plugins %>% dplyr::select(plugin_id = id, plugin_name = name), by = "plugin_id") %>%
      # dplyr::relocate(plugin_name, .after = "name") %>%
      dplyr::left_join(r$thesaurus_items %>% dplyr::select(thesaurus_item_id = id, thesaurus_item_name = name), by = "thesaurus_item_id") %>%
      dplyr::relocate(thesaurus_item_name, .after = "id") %>%
      dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, creator_name = paste0(firstname, " ", lastname)), by = "creator_id") %>%
      dplyr::relocate(creator_name, .after = "thesaurus_item_unit") %>%
      # Delete id col, the important one is group_id
      dplyr::select(-name, -display_order, -module_id, -plugin_id, -thesaurus_item_id, -creator_id)
    
    data["action"] <- NA_character_
  }
  
  # Change name of cols
  colnames(data) <- new_colnames
  
  data
}
