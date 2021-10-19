#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_r_console_ui <- function(id, language, page){
  ns <- NS(id)
  div(class = "main",
    div(shinyAce::aceEditor(ns("ace_code"), "", mode = "r", 
      autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
    shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), br(),
    div(shiny::verbatimTextOutput(ns("code_result")), 
      style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_r_console_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
      observeEvent(input$execute_code, (
      output$code_result <- renderText(
        execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
          language = language, r = r, edited_code = isolate(input$ace_code), code_type = "server"))))
  })
}
