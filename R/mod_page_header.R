#' top_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# mod_page_header_ui <- function(id){
#   ns <- NS(id)
#   div(class = "header",
#     div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
#     div(class = "header_left_bar", 
#       shiny.fluent::CommandBar(
#         items = list(
#           shiny.fluent::CommandBarItem("Home", "Home"),
#           shiny.fluent::CommandBarItem("Patient-level data", "Contact"),
#           shiny.fluent::CommandBarItem("Grouped data", "BIDashboard")
#         )
#       )
#     ),
#     div(class = "header_right_bar",
#       shiny.fluent::CommandBar(
#         items = list(
#           shiny.fluent::CommandBarItem("Parameters", "Settings", iconOnly = TRUE, subitems = list(
#             shiny.fluent::CommandBarItem("Data", key = "data"),
#             shiny.fluent::CommandBarItem("Settings", key = "settings")
#           )),
#           shiny.fluent::CommandBarItem("Help", "Info", iconOnly = TRUE)
#         )
#       )
#     )
#   )
# }
    
mod_page_header_ui <- function(id){
  ns <- NS(id)
  div(class = "header",
      div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
      div(class = "header_left_bar", 
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem("Home", "Home"),
              shiny.fluent::CommandBarItem("Patient-level data", "Contact"),
              shiny.fluent::CommandBarItem("Grouped data", "BIDashboard")
              #   shiny.fluent::CommandBarItem("New", "Add", subitems = list(
              #     shiny.fluent::CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
              #     shiny.fluent::CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
              #   )),
              #   shiny.fluent::CommandBarItem("Upload sales plan", "Upload"),
              #   shiny.fluent::CommandBarItem("Share analysis", "Share"),
              #   shiny.fluent::CommandBarItem("Download report", "Download")
            )
          )
      ),
      div(class = "header_right_bar",
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem("Parameters", "Settings", iconOnly = TRUE),
              shiny.fluent::CommandBarItem("Help", "Info", iconOnly = TRUE)
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
