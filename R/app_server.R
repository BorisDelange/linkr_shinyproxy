#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(router){
  function( input, output, session ) {
  # Your application server logic 
  # root_page <- div(h2("Root page"))
  # other_page <- div(h3("Other page"))
  # 
  # router <- shiny.router::make_router(
  #   shiny.router::route("/", root_page),
  #   shiny.router::route("other", other_page)
  # )
  router$server(input, output, session)
  # my_router <- router(mod_page_main_ui("page_main_ui_1"))
  # my_router$server(input, output, session)
  }
}
