#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(router_on = FALSE, router = NULL){
  function( input, output, session ) {
    if (router_on) router$server(input, output, session)
    # if (!router_on)
  }
}
