#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  observeEvent(input$submit_1, {
    output$text_output_1 <- renderText(paste0("Input text is", " : ", isolate(input$text_input_1)))
  })
}
