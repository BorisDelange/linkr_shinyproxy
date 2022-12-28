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
      list(key = "r_console", text = i18n$t("r_console"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "r_console_edit_code_card", itemKey = "r_console_edit_code_card", headerText = i18n$t("r_console"))
    ),
    forbidden_card_new(ns = ns, name = "r_console_edit_code_card", i18n = i18n),
    shinyjs::hidden(
      div(id = ns("r_console_edit_code_card"),
        make_card_shiny_ace("",
          div(
            div(
              shinyAce::aceEditor(
                outputId = ns("ace_code"), value = "", mode = "r",
                code_hotkeys = list(
                  "r", list(
                    run_selection = list(
                      win = "CTRL-ENTER",
                      mac = "CTRL-ENTER|CMD-ENTER"
                    ),
                    run_all = list(
                      win = "CTRL-SHIFT-ENTER",
                      mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"
                    )
                  )
                ),
                wordWrap = TRUE, debounce = 10
                # autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
              ),
              style = "width: 100%;"
            ),

            shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(), br(),
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

mod_settings_r_console_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- "r_console_edit_code_card"
    
    observe({
      shiny.router::get_query_param()

      if ("r_console_edit_code_card" %in% r$user_accesses){
        shinyjs::show("r_console_edit_code_card")
        shinyjs::hide("r_console_edit_code_card_forbidden")
      }
      else {
        shinyjs::show("r_console_edit_code_card_forbidden")
        shinyjs::hide("r_console_edit_code_card")
      }
    })
  
    # --- --- --- --- -
    # Execute code ----
    # --- --- --- --- -
    
    observeEvent(input$execute_code, {
      r$r_console_code <- input$ace_code
    })
    
    observeEvent(input$ace_code_run_selection, {
      if(!shinyAce::is.empty(input$ace_code_run_selection$selection)){
        r$r_console_code <- input$ace_code_run_selection$selection
      }
      else {
        r$r_console_code <- input$ace_code_run_selection$line
      }
    })

    observeEvent(input$ace_code_run_all, {
      r$r_console_code <- input$ace_code
    })

    observeEvent(r$r_console_code, {

      if ("r_console_edit_code_card" %in% r$user_accesses){
        edited_code <- r$r_console_code %>% stringr::str_replace_all("\r", "\n")
        
        output$code_result <- renderText(
          execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, r = r, d = d, m = m,
            edited_code = edited_code, code_type = "server"))
      }
    })
    
  })
}
