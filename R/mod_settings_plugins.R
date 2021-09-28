#' settings_plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_plugins_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
      plugins_toggle_card(language, ns, activated = "plugins_management_card"),
      div(
        id = ns("plugins_creation_card"),
        make_card(
          translate(language, "plugins_creation"),
          div()
        )
      ),
      div(
        id = ns("plugins_management_card"),
        make_card(
          translate(language, "plugins_management"),
          div()
        )
      ),
      div(
        id = ns("plugins_code_card"),
        make_card(
          translate(language, "plugins_code"),
          div()
        )
      )
    ) -> result
  }
  
  result
}
    
#' settings_plugins Server Functions
#'
#' @noRd 
mod_settings_plugins_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toggles <- c("plugins_creation_card", "plugins_management_card", "plugins_code_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
  })
}
    
## To be copied in the UI
# mod_settings_plugins_ui("settings_plugins_ui_1")
    
## To be copied in the server
# mod_settings_plugins_server("settings_plugins_ui_1")
