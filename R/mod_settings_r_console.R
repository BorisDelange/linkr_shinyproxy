#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_r_console_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_r_console_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
