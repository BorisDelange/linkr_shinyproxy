#' page_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_main_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Home page                              #
  ##########################################
  
  if (grepl("^home", id)){
    mod_home_ui(id = "home", language = language, words = words) -> result
  }
  
  ##########################################
  # Patient-lvl & aggregated data pages    #
  ##########################################
  
  if (id == "patient_level_data") mod_patient_and_aggregated_data_ui(id = "patient_level_data", language = language, words = words) -> result
  if (id == "aggregated_data") mod_patient_and_aggregated_data_ui(id = "aggregated_data", language = language, words = words) -> result
  
  ##########################################
  # Plugins page                           #
  ##########################################
  
  if (id == "plugins_patient_lvl") mod_plugins_ui(id = "plugins_patient_lvl", language = language, words = words) -> result
  if (id == "plugins_aggregated") mod_plugins_ui(id = "plugins_aggregated", language = language, words = words) -> result
  
  ##########################################
  # Settings pages                         #
  ##########################################
  
  if (grepl("^settings", id)){
    
    if (id == "settings_general_settings") mod_settings_general_ui(id = "settings_general_settings", language = language, words = words) -> result
    if (id == "settings_app_db") mod_settings_app_database_ui(id = "settings_app_db", language = language, words = words) -> result
    if (id == "settings_users") mod_settings_users_ui(id = "settings_users", language = language, words = words) -> result
    if (id == "settings_r_console") mod_settings_r_console_ui(id = "settings_r_console", language = language, words = words) -> result
    
    # Subpages of Settings / data management
    sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page_settings){
      if (id == paste0("settings_", page_settings)) mod_settings_data_management_ui(id = paste0("settings_", page_settings), 
        language = language, words = words) ->> result
    })
    
    if (id == "settings_plugins") mod_settings_plugins_ui(id = "settings_plugins", language = language, words = words) -> result
    if (id == "settings_patient_lvl_modules") mod_settings_modules_ui(id = "settings_patient_lvl_modules", language = language, words = words) -> result
    if (id == "settings_aggregated_modules") mod_settings_modules_ui(id = "settings_aggregated_modules", language = language, words = words) -> result
    if (id == "settings_log") mod_settings_log_ui(id = "settings_log", language = language, words = words) -> result
  }

  result
}