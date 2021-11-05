#' Update r variable
#' 
#' @description Update r value, requesting the corresponding table in the database
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param table Database table name (character)
#' @param language language used for the translation (character)
#' @examples
#' \dontrun{
#' update_r(r = r, table = "subsets")
#' }
update_r <- function(r = shiny::reactiveValues(), table = character(), language = "EN"){
  tables <- c("users", "users_accesses", "users_statuses",
    "data_sources", "datamarts", "studies", "subsets", "subset_patients", "thesaurus", "thesaurus_items",
    "plugins", 
    "patient_lvl_modules_families", "patient_lvl_modules", "patient_lvl_modules_elements",
    "aggregated_modules_families", "aggregated_modules", "aggregated_modules_elements",
    "code", 
    "options",
    "plugins_options", "patients_options")
  
  if (table %not_in% tables) stop(paste0(translate(language, "invalid_table_name"), ". ", translate(language, "tables_allowed"), " : ", toString(tables)))
  
  # Don't load password col for table users
  if (table == "users"){
    new_table <- DBI::dbGetQuery(r$db, "SELECT id, username, firstname, lastname, user_access_id, user_status_id, datetime, deleted
      FROM users WHERE deleted IS FALSE ORDER BY id")
  }
  else new_table <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
  
  r[[table]] <- new_table
  r[[paste0(table, "_temp")]] <- new_table %>% dplyr::mutate(modified = FALSE)
  
  # Access by table
  
  if (table %in% c("studies", "datamarts", "plugins")){
    if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
      if (nrow(r[[table]] > 0)){
        r[[table]] <- get_authorized_data(r = r, table = table)
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      }
    }
  }
  
  if (table %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
    if (grepl("aggregated", table)) prefix <- "aggregated"
    
    if (paste0(prefix, "_modules_see_all_data") %not_in% r$user_accesses){
      if (nrow(r[[table]] > 0)){
        r[[table]] <- get_authorized_data(r = r, table = table)
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      }
    }
  }
  
  # Access by authorship
  if (table %in% c("data_sources", "subsets", "thesaurus")){
    if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
      if (nrow(r[[table]] > 0)){
        r[[table]] <- get_authorized_data(r = r, table = table)
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      }
    }
  }
  
  # Access by parent
  if (table %in% c("patient_lvl_modules", "aggregated_modules", "patient_lvl_modules_elements", "aggregated_modules_elements")){
    
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
    if (grepl("aggregated", table)) prefix <- "aggregated_"
    
    if (paste0(prefix, "_modules_see_all_data") %not_in% r$user_accesses){
      modules_families_ids <- get_authorized_data(r = r, table = paste0(prefix, "modules_families")) %>% dplyr::pull(id)
      if (nrow(r[[paste0(prefix, "modules")]]) > 0) modules_ids <- r[[paste0(prefix, "modules")]] %>%
          dplyr::filter(module_family_id %in% modules_families_ids) %>% dplyr::pull(id)
      
      if (nrow(r[[table]] > 0)){
        if (grepl("modules$", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_family_id %in% modules_families_ids)
        if (grepl("modules_elements", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_id %in% modules_ids)
      }
      
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    }
  }
}

#' Get options of a page
#' 
#' @description Get the options of a setting page (as datamarts, studies...)
#' @param id ID of the module / page 
#' @return A character vector with options
#' @examples 
#' get_page_options(id == "settings_datamarts")
get_page_options <- function(id = character()){
  result <- ""
  switch(id,
    "settings_datamarts" = c("show_only_aggregated_data", "users_allowed_read"),
    "settings_studies" = "users_allowed_read",
    "settings_plugins" = c("markdown_description", "users_allowed_read"),
    "settings_users_accesses_options" = "users_accesses_options",
    "settings_modules_patient_lvl_modules_families_options" = "users_allowed_read",
    "settings_modules_aggregated_modules_families_options" = "users_allowed_read") -> result
  result
}

#' Get column names
#' 
#' @param table_name Name of the table (character)
#' @param language Language used (charater)
#' @examples 
#' get_col_names(table_name = "datamarts", language = "EN")
get_col_names <- function(table_name = character(), language = "EN"){
  result <- ""
  
  if (table_name %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"))
    c(result, switch(table_name,
      "datamarts" = translate(language, "data_source"),
      "studies" = c(translate(language, "datamart"), translate(language, "patient_lvl_module_family"),
        translate(language, "aggregated_module_family")),
      "subsets" = translate(language, "study"),
      "thesaurus" = translate(language, "data_sources"))) -> result
    result <- c(result, translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name == "thesaurus_items"){
    result <- c(translate(language, "item_id"), translate(language, "name"),
      translate(language, "display_name"), translate(language, "category"), translate(language, "unit"), translate(language, "action"))
  }
  
  if (table_name == "modules_thesaurus_items"){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
      translate(language, "item_colour"), translate(language, "action"))
  }
  
  if (table_name == "thesaurus_items_with_counts"){
    result <- c(translate(language, "id"), translate(language, "name"),
      translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
      translate(language, "num_patients"), translate(language, "num_rows"), translate(language, "action"))
  }
  
  if (table_name == "modules_thesaurus_items_with_counts"){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
      translate(language, "item_colour"), translate(language, "num_patients"), translate(language, "num_rows"), translate(language, "action"))
  }
  
  if (table_name == "plugins"){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "module_type"), 
      translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name == "users"){
    result <- c(translate(language, "id"), translate(language, "username"), translate(language, "firstname"), translate(language, "lastname"),
      translate(language, "user_access"), translate(language, "user_status"), translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name %in% c("users_accesses", "users_statuses")){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"), 
      translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name %in% c("patient_lvl_modules", "aggregated_modules")){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "module_family"),
      translate(language, "parent_module"), translate(language, "display_order"), translate(language, "creator"), translate(language, "datetime"), 
      translate(language, "action"))
  }
  
  if (table_name %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    result <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
      translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name == "patient_lvl_modules_elements"){
    result <- c(translate(language, "name"), translate(language, "module_family"), translate(language, "module"), translate(language, "plugin"), 
      translate(language, "thesaurus"), translate(language, "display_name"), translate(language, "unit"), translate(language, "action"))
  }
  
  if (table_name == "aggregated_modules_elements"){
    result <- c(translate(language, "name"), translate(language, "module_family"), translate(language, "module"), 
      translate(language, "plugin"), translate(language, "display_order"), translate(language, "action"))
  }
  
  if (table_name == "log"){
    result <- c(translate(language, "id"), translate(language, "category"), translate(language, "name"),
      translate(language, "value"), translate(language, "user"), translate(language, "datetime"))
  }
  
  result
}
