#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# mod_test_ui <- function(id = character()){
#   # ns <- NS(id)
#   
#   tagList(
#     shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
#       shiny.fluent::TextField.shinyInput("text_input_1"),
#       shiny.fluent::PrimaryButton.shinyInput("submit_1", "Show")
#     ),
#     div(verbatimTextOutput("text_output_1"), style = "border:dashed 1px; margin-top:10px;")
#   )
# }

#' plugins Server Functions
#'
#' @noRd 
mod_test_server <- function(id = character()){
  moduleServer(id, function(input, output, session){
    # ns <- session$ns
    
    # observeEvent(input$submit_1, {
    #     output$text_output_1 <- renderText(paste0("Input text is", " : ", isolate(input$text_input_1)))
    # })
    
  })
}
