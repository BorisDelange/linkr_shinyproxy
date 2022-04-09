#' my_subsets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_subsets_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  cards <- c("subset_datatable_card", "subset_creation_card", "subset_management_card", "subset_edit_code_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = language, words = words))
  })
  
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "subset_main", text = translate(language, "my_subsets", words))
    ), maxDisplayedItems = 3),
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "subset_datatable_card", itemKey = "subset_datatable_card", headerText = translate(language, "subsets_management", words)),
          shiny.fluent::PivotItem(id = "subset_creation_card", itemKey = "subset_creation_card", headerText = translate(language, "create_subset", words)),
          shiny.fluent::PivotItem(id = "subset_management_card", itemKey = "subset_management_card", headerText = translate(language, "subset_management", words)),
          shiny.fluent::PivotItem(id = "subset_edit_code_card", itemKey = "subset_edit_code_card", headerText = translate(language, "edit_subset_code", words))
        )
      )
    ),
    forbidden_cards,
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(translate(language, "choose_a_study", words), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_management_card"),
        make_card("",#translate(language, "subset_management", words),
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_edit_code_card"),
        make_card("",#translate(language, "edit_subset_code", words), 
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_creation_card"),
        make_card("",#translate(language, "create_subset", words),
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_datatable_card"),
        make_card("",#translate(language, "subsets_management", words),
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    )
  )
}
    
#' my_subsets Server Functions
#'
#' @noRd 
mod_my_subsets_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    # if (id == "patient_level_data_subsets") prefix <- "patient_lvl"
    # if (id == "aggregated_data_subsets") prefix <- "aggregated"
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("subset_management_card", "subset_edit_code_card", "subset_creation_card", "subset_datatable_card")
    show_hide_cards_new(r = r, input = input, session = session, id = id, cards = cards)
 
    ##########################################
    # Render subsets UI                      #
    ##########################################
    
    observeEvent(r$chosen_study, {
      
      req(!is.na(r$chosen_study))
      
      # Show first card & hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("subset_datatable_card" %in% r$user_accesses) shinyjs::show("subset_datatable_card")
        else shinyjs::show("subset_datatable_card_forbidden")
      }
      
    })
    
    observeEvent(r$chosen_subset, {
      
      # Render subset UI
      # ...
      
      # Subset data are loaded when the study is loaded
    })
    
  })
}