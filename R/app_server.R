#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(page_style, router = NULL){
  function( input, output, session ) {
    if (page_style == "fluent") router$server(input, output, session)
  }
}
