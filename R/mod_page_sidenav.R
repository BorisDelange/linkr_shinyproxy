#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_sidenav_ui <- function(id, page_style, page){
  ns <- NS(id)
  
  result <- div()
  
  if (page_style == "fluent"){
    if (page == "home"){
      div(class = "sidenav",
          div(class = "dropdown_title", "Datamart"),
          shiny.fluent::Dropdown.shinyInput("datamart", "Datamart", value = "ufh",
                                            options = list(list(key = "ufh", text = "Cohorte héparine"),
                                                           list(key = "wmv", text = "Sevrage ventilation"))),
          div(class = "dropdown_title", "Study"),
          shiny.fluent::Dropdown.shinyInput("study", "Study", value = "study1",
                                            options = list(list(key = "study1", text = "Etude 1 - premier anti-Xa"),
                                                           list(key = "study2", text = "Etude 2 - tous les anti-Xa")))
      ) -> result
    }
    if (page == "settings"){
      div(class = "sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = list(
              list(name = "General settings", key = "general"),
              list(name = "Data management", key = "data")
              # list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
            ))
          ),
          initialSelectedKey = "general",
          styles = list(
            root = list(
              height = "100%",
              boxSizing = "border-box",
              overflowY = "auto"
            )
          )
        )
      ) -> result
    }
  }
  
  if (page_style == "fluid"){
    
  }
  
  result
}
    
#' page_sidenav Server Functions
#'
#' @noRd 
mod_page_sidenav_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_sidenav_ui("page_sidenav_ui_1")
    
## To be copied in the server
# mod_page_sidenav_server("page_sidenav_ui_1")
