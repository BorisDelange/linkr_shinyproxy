#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    div(class = "main",
      tableOutput(ns("test"))
    ) -> result
  }
  result
}
    
#' patient_and_aggregated_data Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    # Once a study is chosen, load its tabs
    observeEvent(r$chosen_study, {
      study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))
      
      patient_lvl_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_modules WHERE module_family_id = ", 
        study_infos$patient_lvl_module_family_id))
      
      output$test <- renderTable(patient_lvl_modules)
    })
    
  })
}
    
## To be copied in the UI
# mod_patient_and_aggregated_data_ui("patient_and_aggregated_data_ui_1")
    
## To be copied in the server
# mod_patient_and_aggregated_data_server("patient_and_aggregated_data_ui_1")
