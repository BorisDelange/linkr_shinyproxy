#' settings_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_general_ui <- function(id, language){
  ns <- NS(id)
  div(class = "main",
    # Hidden aceEditor, allows the other to be displayed...
    div(shinyAce::aceEditor("hidden"), style = "display: none;"),
    make_card("Dev / choose user for tests", make_dropdown(language = language, ns = ns, label = "user", width = "300px")),
    textOutput(ns("test")), br(),
    textOutput(ns("test2"))
  )
}
    
#' settings_general Server Functions
#'
#' @noRd 

mod_settings_general_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(r$db, {
      shiny.fluent::updateDropdown.shinyInput(session, "user",
        options = convert_tibble_to_list(r$users, key_col = "id", text_col = "username"))
    })
    
    observeEvent(input$user, r$user_id <- input$user)
    
    observeEvent(r$user_id, {
      output$test <- renderText(paste0("user_id == ", r$user_id))
    })
    observeEvent(r$user_accesses, {
      output$test2 <- renderText(paste0("user_accesses = ", r$user_accesses))
    })
  })
}