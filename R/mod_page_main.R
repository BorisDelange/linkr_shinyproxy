#' page_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_main_ui <- function(id, language){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Home page                              #
  ##########################################
  
  if (grepl("^home", id)){
    div(class = "main",
      div(
        div(
          class = glue::glue("card ms-depth-8 ms-sm{8} ms-xl{8}"),
          shiny.fluent::Stack(
            tokens = list(childrenGap = 5),
            shiny.fluent::Text(variant = "large", "Datamarts", block = TRUE),
            # shiny.fluent::Text(paste0("id = ", id)),
            shiny.fluent::Text(shiny::textOutput(ns("test2")))
          )
        )
      )
    ) -> result
  }
  
  ##########################################
  # Patient-lvl & aggregated data pages    #
  ##########################################
  
  if (id == "patient_level_data") mod_patient_and_aggregated_data_ui("patient_level_data", language, id) -> result
  if (id == "aggregated_data") mod_patient_and_aggregated_data_ui("aggregated_data", language, id) -> result
  
  ##########################################
  # Settings pages                         #
  ##########################################
  
  if (grepl("^settings", id)){
    
    if (id == "settings_general_settings") mod_settings_general_ui("settings_general_settings", language) -> result
    if (id == "settings_app_db") mod_settings_app_database_ui("settings_app_db", language) -> result
    if (id == "settings_users") mod_settings_users_ui("settings_users", language) -> result
    if (id == "settings_r_console") mod_settings_r_console_ui("settings_r_console", language, id) -> result
    
    # Subpages of Settings / data management
    sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page_settings){
      if (id == paste0("settings_", page_settings)) mod_settings_data_management_ui(id = paste0("settings_", page_settings), 
        language = language) ->> result
    })
    
    if (id == "settings_plugins") mod_settings_plugins_ui(id = "settings_plugins", language = language) -> result
    if (id == "settings_modules_patient_lvl") mod_settings_modules_ui("settings_modules_patient_lvl", language) -> result
    if (id == "settings_modules_aggregated") mod_settings_modules_ui("settings_modules_aggregated", language) -> result
    if (id == "settings_log") result
  }

  result
}

#' page_main Server Functions
#'
#' @noRd 

mod_page_main_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}