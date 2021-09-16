#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(page_style, router){
  function( input, output, session ) {
    r <- reactiveValues()
    r$studies_data <- tibble::tribble(~Action, ~`Study ID`, ~`Study name`, ~Datamart, ~`Patient-level data module family`, ~`Aggregated data module family`,
                                      "", 1, "Study 1", "Weaning from mechanical ventilation", "eHOP default", "Default 1",
                                      "", 2, "Study 2", "Heparin datamart Metavision", "Metavision default", "Default 1",
                                      "", 3, "Study 3", "My new study", "App default", "Default 2")
    
    r$datamarts_data <- tibble::tribble(~Action, ~`Study ID`, ~`Study name`, ~Datamart, ~`Patient-level data module family`, ~`Aggregated data module family`,
                                        "", 1, "Study 1", "Weaning from mechanical ventilation", "eHOP default", "Default 1",
                                        "", 2, "Study 2", "Heparin datamart Metavision", "Metavision default", "Default 1",
                                        "", 3, "Study 3", "My new study", "App default", "Default 2")
    
    if (page_style == "fluent") router$server(input, output, session)
    
    mod_settings_data_management_server("settings_data_sources", r, language)
    mod_settings_data_management_server("settings_datamarts", r, language)
    mod_settings_data_management_server("settings_studies", r, language)
    mod_settings_data_management_server("settings_subsets", r, language)
  }
}
