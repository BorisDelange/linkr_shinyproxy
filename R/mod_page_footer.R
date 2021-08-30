#' page_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_footer_ui <- function(id){
  ns <- NS(id)
  div(class = "footer", 
      shiny.fluent::Stack(
        horizontal = TRUE,
        horizontalAlign = 'space-between',
        tokens = list(childrenGap = 20),
        shiny.fluent::Text(variant = "medium", "Github", block = TRUE)
      )    
  )
}
    
#' page_footer Server Functions
#'
#' @noRd 
mod_page_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_page_footer_ui("page_footer_ui_1")
    
## To be copied in the server
# mod_page_footer_server("page_footer_ui_1")
