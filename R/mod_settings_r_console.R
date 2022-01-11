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
    # render_settings_toggle_card(language = language, ns = ns, cards = list(
    #   list(key = "edit_code_card", label = "r_console")), words = words),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "r_console", text = translate(language, "r_console", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = translate(language, "r_console", words))
    ),
    
    shinyjs::hidden(
      div(id = ns("edit_code_card"),
        div(shinyAce::aceEditor(ns("ace_code"), "", mode = "r", 
          autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
       
        shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code", words)), br(),
        div(shiny::verbatimTextOutput(ns("code_result")), 
          style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
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
    
    cards <- "edit_code_card"
    show_hide_cards_new(r = r, input = input, session = session, table = "r_console", id = id, cards = cards)
    
    ##########################################
    # R console / Execute code               #
    ##########################################
    
    observe({
      shiny.router::get_query_param()
      shinyjs::hide("edit_code_card")
      shinyjs::show("edit_code_card")
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
