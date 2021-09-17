#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(page_style, router, language){
  function( input, output, session ) {
    
    r <- reactiveValues()
    
    r$data_sources_data <- tibble::tribble(~`Data source ID`, ~`Data source name`, ~`Data source description`, ~`Creator`, ~`Date & time`, ~`Deleted`,
                                           13, "MIMIC-IV", "MIMIC database version 4", "Admin", "2021-09-16 17:58:21", FALSE,
                                           15, "eHOP", "eHOP university hosiptal of Rennes", "Admin", "2021-09-16 17:59:00", FALSE)
    
    r$datamarts_data <- tibble::tribble(~`Datamart ID`, ~`Datamart name`, ~`Datamart description`, ~`Data source ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
                                        2, "Weaning from mechanical ventilation", "A study with MV", 13, "John Doe", "2021-09-15 16:20:21", FALSE,
                                        4, "Heparin data Metavision", "Prediction of response of UFH therapy in ICU", 15, "Admin", "2021-09-14 15:20:23", FALSE)
    
    r$studies_data <- tibble::tribble(~`Study ID`, ~`Study name`, ~`Study description`, ~`Datamart ID`,
                                      ~`Patient-level data module family`, ~`Aggregated data module family`, ~`Creator`, ~`Date & time`, ~`Deleted`,
                                      1, "Study 1", "Weaning from mechanical ventilation", 2, "eHOP default", "Default 1", "John Doe", "2021-08-23 08:53:45", FALSE,
                                      2, "Study 2", "Heparin datamart Metavision", 4, "Metavision default", "Default 1", "Jane Doe", "2021-07-25 09:45:43", FALSE,
                                      3, "Study 3", "My new study", 2, "App default", "Default 2", "Admin", "2021-06-23 21:13:47", FALSE)
    
    r$subsets_data <- tibble::tribble(~`Subset ID`, ~`Subset name`, ~`Subset description`, ~`Study ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
                                      1, "Included patients", "A subset with only included patients", 1, "Admin", "2021-07-21 19:45:13", FALSE)
    
    r$patient_lvl_modules_families <- tibble::tribble(~`Module family ID`, ~`Module family name`, ~`Module family description`, ~`Creator`, ~`Date & time`, ~`Deleted`)
    
    r$patient_lvl_modules <- tibble::tribble(~`Module ID`, ~`Module name`, ~`Module description`, ~`Module family ID`, ~`Parent module ID`,
                                             ~`Creator`, ~`Date & time`, ~`Deleted`)
    
    r$aggregated_modules <- tibble::tribble(~`Module ID`, ~`Module name`, ~`Module description`, ~`Parent module ID`, 
                                            ~`Creator`, ~`Date & time`, ~`Deleted`)
    
    if (page_style == "fluent") router$server(input, output, session)
    
    mod_settings_data_management_server("settings_data_sources", r, language)
    mod_settings_data_management_server("settings_datamarts", r, language)
    mod_settings_data_management_server("settings_studies", r, language)
    mod_settings_data_management_server("settings_subsets", r, language)
  }
}
