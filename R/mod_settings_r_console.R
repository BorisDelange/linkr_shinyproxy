#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_r_console_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  div(class = "main",
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "edit_code_card", label = "r_console")), words = words),
    
    div(id = ns("edit_code_card"),
      div(shinyAce::aceEditor(ns("ace_code"), "", mode = "r", 
        autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
     
      shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code", words)), br(),
      div(shiny::verbatimTextOutput(ns("code_result")), 
        style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
    )
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_r_console_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # R console / Show or hide cards         #
    ##########################################
    
    toggles <- "edit_code_card"
    show_hide_cards(r = r, input = input, session = session, table = "r_console", id = id, toggles = toggles)
    
    ##########################################
    # R console / Execute code               #
    ##########################################
    
    observeEvent(input$execute_code, {
      # If user has access
      req("r_console_edit_code_card" %in% r$user_accesses)
      
      edited_code <- isolate(input$ace_code %>% stringr::str_replace_all("\r", "\n"))
      
      output$code_result <- renderText(
        execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
          language = language, r = r, edited_code = edited_code, code_type = "server"))
      })
  })
}
