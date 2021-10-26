#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_r_console_ui <- function(id, language, page){
  ns <- NS(id)
  div(class = "main",
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "edit_code_card", label = "r_console"))),
    
    div(id = ns("edit_code_card"),
      div(shinyAce::aceEditor(ns("ace_code"), "", mode = "r", 
        autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
     
      shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), br(),
      div(shiny::verbatimTextOutput(ns("code_result")), 
        style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
    )
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_r_console_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    toggles <- "edit_code_card"
    
    ##########################################
    # R console / Show or hide cards         #
    ##########################################
    
    # Depending on user_accesses
    # observeEvent(r$user_accesses, {
      # Hide toggles if user has no access
      # (Doesn't work anymore : we had a condition in server.R to not load server data if user has no access)
      # if ("r_console" %not_in% r$user_accesses) shinyjs::hide("toggles") else shinyjs::show("toggles")
    # })
    
    # Depending on toggles activated
    sapply(toggles, function(toggle){
      
      # If user has no access, hide card
      observeEvent(r$user_accesses, if (paste0("r_console_", toggle) %not_in% r$user_accesses) shinyjs::hide(toggle)) 
      
      # If user has access, show or hide card when toggle is clicked
      observeEvent(input[[paste0(toggle, "_toggle")]], {
        if (paste0("r_console_", toggle) %in% r$user_accesses){
          if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) 
          else shinyjs::hide(toggle)
        }
      })
    })
    
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
