#' scripts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scripts_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("scripts_datatable_card", "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "scripts_thesaurus_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = "EN", words = words))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    #shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t("Scripts"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      id = ns("scripts_pivot"),
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "scripts_datatable_card", itemKey = "scripts_datatable_card", headerText = i18n$t("Scripts management")),
      shiny.fluent::PivotItem(id = "scripts_creation_card", itemKey = "scripts_creation_card", headerText = i18n$t("Create a script")),
      shiny.fluent::PivotItem(id = "scripts_edit_code_card", itemKey = "scripts_edit_code_card", headerText = i18n$t("Edit script code")),
      shiny.fluent::PivotItem(id = "scripts_options_card", itemKey = "scripts_options_card", headerText = i18n$t("Script options")),
      shiny.fluent::PivotItem(id = "scripts_thesaurus_card", itemKey = "scripts_thesaurus_card", headerText = i18n$t("Thesaurus items"))
    ),
    forbidden_cards,
    shinyjs::hidden(
      div(
        id = ns("scripts_datatable_card"),
        make_card(i18n$t("Scripts management"),
          div(
            DT::DTOutput(ns("scripts_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_management"), i18n$t("Save"))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("scripts_creation_card"),
        make_card(i18n$t("Create a script"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield_new(i18n = i18n, ns = ns, label = "Name", id = "script_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_script"), i18n$t("Add"))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("scripts_edit_code_card"),
        make_card(i18n$t("Edit script code"),
          div(
            make_combobox(language = language, ns = ns, label = "script", id = "code_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE)
            
            # thesaurus_items_div, br(),
            # 
            # shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            #   shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
            #     list(key = "ui", text = i18n$t("UI")),
            #     list(key = "server", text = i18n$t("Server"))
            #   ), className = "inline_choicegroup"),
            #   div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
            #   div(i18n$t("Hide editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            # ),
            # shinyjs::hidden(div(id = ns("div_br"), br())),
            # 
            # conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
            #   div(shinyAce::aceEditor(ns("ace_edit_code_ui"), "", mode = "r", 
            #     autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            # conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
            #   div(shinyAce::aceEditor(ns("ace_edit_code_server"), "", mode = "r", 
            #     autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            # 
            # shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("Save")), " ",
            # shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("Run code")), br(), br(),
            # shiny::uiOutput(ns("code_result_ui")), br(),
            # div(verbatimTextOutput(ns("code_result_server")), 
            #   style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("scripts_options_card"),
        make_card(i18n$t("Script options"),
          div(
            make_combobox(language = language, ns = ns, label = "script", id = "options_chosen_plugin",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            
            shiny.fluent::PrimaryButton.shinyInput(ns("save_script_options"), i18n$t("Save"))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("scripts_thesaurus_card"),
        make_card("",
          div(
            
          )
        ), br()
      )
    )
  ) -> result
  
  result
}
    
#' scripts Server Functions
#'
#' @noRd 
mod_scripts_server <- function(id = character(), r = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("scripts_datatable_card", "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "scripts_thesaurus_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("scripts_datatable_card" %in% r$user_accesses) shinyjs::show("scripts_datatable_card")
    else shinyjs::show("all_scripts_card_forbidden")
    
    ##########################################
    # Create a script                        #
    ##########################################
    
    observeEvent(input$add_script, {
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$script_name)
      new_data$script_name <- new_data$name
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = "scripts", 
        data = new_data, table = "scripts", required_textfields = "script_name", req_unique_values = "name")
      
    })
    
  })
}