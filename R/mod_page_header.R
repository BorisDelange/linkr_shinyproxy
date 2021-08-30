#' top_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_header_ui <- function(id, page_style, page){
  ns <- NS(id)
  div(class = "header",
      div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
      div(class = "header_left_bar", 
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem("Home", "Home", href = shiny.router::route_link("/")),
              shiny.fluent::CommandBarItem("Patient-level data", "Contact", href = shiny.router::route_link("patient_level_data")),
              shiny.fluent::CommandBarItem("Grouped data", "BIDashboard", href = shiny.router::route_link("aggregated_data"))
            )
          )
      ),
      div(class = "header_right_bar",
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem("Parameters", "Settings", iconOnly = TRUE, 
                                           href = shiny.router::route_link("settings")),
              shiny.fluent::CommandBarItem("Help", "Info", iconOnly = TRUE, href = shiny.router::route_link("help"))
            )
          )
      )
  )
}

#' top_panel Server Functions
#'
#' @noRd 
mod_page_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_header_ui("top_panel_ui_1")
    
## To be copied in the server
# mod_page_header_server("top_panel_ui_1")
