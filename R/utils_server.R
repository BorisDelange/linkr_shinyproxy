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
  tables <- c("users", "users_accesses", "users_statuses", "users_accesses_details",
    "data_sources", "datamarts", "studies", "subsets", "thesaurus", "thesaurus_items",
    "plugins", "patient_lvl_module_families", "patient_lvl_modules", "patient_lvl_module_elements",
    "aggregated_module_families", "aggregated_modules", "code", "options")
  
  if (table %not_in% tables) stop(paste0(translate(language, "invalid_table_name"), ". ", translate(language, "tables_allowed"), " : ", toString(tables)))
  
  # Don't load password col for table users
  if (table == "users"){
    new_table <- DBI::dbGetQuery(r$db, "SELECT id, username, firstname, lastname, user_access_id, user_status_id, datetime, deleted
      FROM users WHERE deleted IS FALSE ORDER BY id")
  }
  else new_table <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
  
  r[[table]] <- new_table
  r[[paste0(table, "_temp")]] <- new_table %>% dplyr::mutate(modified = FALSE)
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
    "settings_plugins" = c("markdown_description", "visibility")) -> result
  result
}

#' Get column names
#' 
#' @param table_name Name of the table (character)
#' @param language Language used (charater)
#' @examples 
#' get_col_names(table_name == "datamarts", language = "EN")
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
      translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
      translate(language, "datetime"), translate(language, "action"))
  }
  
  if (table_name == "thesaurus_items_with_counts"){
    result <- c(translate(language, "item_id"), translate(language, "name"),
      translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
      translate(language, "datetime"), translate(language, "num_patients"), translate(language, "num_rows"), translate(language, "action"))
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
  
  result
}
