#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_sidenav_ui <- function(id){
  ns <- NS(id)
  div(class = "sidenav",
    div(class = "dropdown_title", "Datamart"),
    shiny.fluent::Dropdown.shinyInput("datamart", "Datamart", value = "ufh",
                                      options = list(list(key = "ufh", text = "Cohorte héparine"),
                                                      list(key = "wmv", text = "Sevrage ventilation"))),
    div(class = "dropdown_title", "Study"),
    shiny.fluent::Dropdown.shinyInput("study", "Study", value = "study1",
                                      options = list(list(key = "study1", text = "Etude 1 - premier anti-Xa"),
                                                     list(key = "study2", text = "Etude 2 - tous les anti-Xa")))
    # shiny.fluent::Nav(
    #   groups = list(
    #     list(links = list(
    #       list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
    #       list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
    #       list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
    #       list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph'),
    #       list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    #     ))
    #   ),
    #   initialSelectedKey = 'home',
    #   styles = list(
    #     root = list(
    #       height = '100%',
    #       boxSizing = 'border-box',
    #       overflowY = 'auto'
    #     )
    #   )
    # )
  )
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
