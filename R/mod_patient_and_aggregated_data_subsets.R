#' patient_and_aggregated_data_subsets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_subsets_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  # Prefix depending on page id
  if (id == "patient_level_data_subsets") prefix <- "patient_lvl"
  if (id == "aggregated_data_subsets") prefix <- "aggregated"
  
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "subset_main", text = translate(language, "subsets", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-subsets_current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "subset_management_card", itemKey = "subset_management_card", headerText = translate(language, "subset_management", words)),
      shiny.fluent::PivotItem(id = "subset_edit_code_card", itemKey = "subset_edit_code_card", headerText = translate(language, "edit_subset_code", words)),
      shiny.fluent::PivotItem(id = "subset_creation_card", itemKey = "subset_creation_card", headerText = translate(language, "create_subset", words)),
      shiny.fluent::PivotItem(id = "subset_datatable_card", itemKey = "subset_datatable_card", headerText = translate(language, "subsets_management", words))
    ),
    div(
      id = ns("subset_management_card"),
      make_card(translate(language, "subset_management", words),
        div("...")
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_edit_code_card"),
        make_card(translate(language, "edit_subset_code", words), 
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_creation_card"),
        make_card(translate(language, "create_subset", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("subset_datatable_card"),
        make_card(translate(language, "subsets_management", words),
          div("...")
        )
      )
    )
  )
}
    
#' patient_and_aggregated_data_subsets Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_subsets_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data_subsets") prefix <- "patient_lvl"
    if (id == "aggregated_data_subsets") prefix <- "aggregated"
 
    ##########################################
    # Render subsets UI                      #
    ##########################################
    
    subsets_cards <- c("subset_management_card", "subset_edit_code_card", "subset_creation_card", "subset_datatable_card")
    
    observeEvent(input$subsets_current_tab, {
      sapply(subsets_cards %>% setdiff(., input$subsets_current_tab), shinyjs::hide)
      shinyjs::show(input$subsets_current_tab)
    })
    
    observeEvent(r$chosen_subset, {
      
      # Render subset UI
      # ...
      
      # Subset data are loaded when the study is loaded
    })
  })
}