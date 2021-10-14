#' top_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_header_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- div()
  
  if (page_style == "fluent"){
    div(class = "header",
      div(htmltools::img(src = "www/logo.png", style = "height: 25px;"), class = "logo"),
      div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
      div(class = "header_left_bar", 
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem(translate(language, "home"), "Home", href = shiny.router::route_link("home/datamarts_studies")),
              shiny.fluent::CommandBarItem(translate(language, "patient_level_data"), "Contact", href = shiny.router::route_link("patient_level_data")),
              shiny.fluent::CommandBarItem(translate(language, "aggregated_data"), "BIDashboard", href = shiny.router::route_link("aggregated_data"))
            )
          )
      ),
      # div(class = "header_center_bar",
      #   shiny.fluent::SearchBox.shinyInput(translate(language, "search"), placeholder = translate(language, "search"))
      # ),
      div(class = "header_right_bar",
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem(translate(language, "settings"), "Settings", iconOnly = TRUE, 
                                           href = shiny.router::route_link("settings/general")),
              shiny.fluent::CommandBarItem(translate(language, "help"), "Info", iconOnly = TRUE, href = "https://github.com/BorisDelange/cdwtools", target = "_blank"),
              shiny.fluent::CommandBarItem(translate(language, "messages"), "Message", iconOnly = TRUE,
                                           href = shiny.router::route_link("home/messages")),
              shiny.fluent::CommandBarItem(translate(language, "disconnect"), "PowerButton", iconOnly = TRUE)
            )
          )
      )
    ) -> result
  }
  
  if (page_style == "fluid"){
    
    # -> result
  }
  
  result
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
