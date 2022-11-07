#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_r_console_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  div(class = "main",
    shiny.fluent::Breadcrumb(items = list(
      list(key = "r_console", text = i18n$t("R console"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("R console"))
    ),
    forbidden_card(ns = ns, name = "edit_code_card", language = "EN", words = words),
    shinyjs::hidden(
      div(id = ns("edit_code_card"),
        make_card("",
          div(
            div(shinyAce::aceEditor(ns("ace_code"), "", mode = "r", 
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
           
            shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("Run code")), br(), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        )
      )
    )
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_r_console_server <- function(id = character(), r = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # R console / Show or hide cards         #
    ##########################################
    
    # cards <- "edit_code_card"
    # show_hide_cards(r = r, input = input, session = session, table = "r_console", id = id, cards = cards)
    
    if ("r_console" %in% r$user_accesses){
      shinyjs::show("edit_code_card")
      shinyjs::hide("edit_code_card_forbidden") 
    }
    else {
      shinyjs::show("edit_code_card_forbidden")
      shinyjs::hide("edit_code_card")
    }
    
    ##########################################
    # R console / Execute code               #
    ##########################################
    
    # Refresh reactivity
    if ("r_console" %in% r$user_accesses){
      
      observe({
        shiny.router::get_query_param()
        shinyjs::hide("edit_code_card")
        shinyjs::show("edit_code_card")
      })
    
      observeEvent(input$execute_code, {
        # If user has access
        
        edited_code <- isolate(input$ace_code %>% stringr::str_replace_all("\r", "\n"))
        
        output$code_result <- renderText(
          execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
            language = "EN", r = r, edited_code = edited_code, code_type = "server"))
      })
    }
  })
}
