#' patient_and_aggregated_data_datamart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_datamart_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  # Prefix depending on page id
  if (id == "patient_level_data_datamart") prefix <- "patient_lvl"
  if (id == "aggregated_data_datamart") prefix <- "aggregated"
  
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "datamart_main", text = translate(language, "datamart", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-datamart_current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "datamart_options_card", itemKey = "datamart_options", headerText = translate(language, "datamart_options", words)),
      shiny.fluent::PivotItem(id = "edit_datamart_code_card", itemKey = "edit_datamart_code", headerText = translate(language, "edit_datamart_code", words)),
      shiny.fluent::PivotItem(id = "create_study_card", itemKey = "create_study", headerText = translate(language, "create_study", words)),
      shiny.fluent::PivotItem(id = "studies_management_card", itemKey = "studies_management", headerText = translate(language, "studies_management", words)),
      shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study", headerText = translate(language, "import_study", words)),
      shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study", headerText = translate(language, "export_study", words)),
      shiny.fluent::PivotItem(id = "thesaurus_card", itemKey = "thesaurus", headerText = translate(language, "thesaurus", words))
    ),
    div(
      id = ns("datamart_options_card"),
      make_card(translate(language, "datamart_options", words),
        div("...")
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("edit_datamart_code_card"),
        make_card(translate(language, "edit_datamart_code", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("create_study_card"),
        make_card(translate(language, "create_study", words), 
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("studies_management_card"),
        make_card(translate(language, "studies_management", words),
          div("...")          
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_study_card"),
        make_card(translate(language, "import_study", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_card(translate(language, "export_study", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thesaurus_card"),
        make_card(translate(language, "thesaurus", words),
          div("...")          
        )
      )
    )
  )
}
    
#' patient_and_aggregated_data_datamart Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_datamart_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data_datamart") prefix <- "patient_lvl"
    if (id == "aggregated_data_datamart") prefix <- "aggregated"
 
    # When a datamart is chosen
    
    observeEvent(r$chosen_datamart, {
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      ##########################################
      # Load datamart data                     #
      ##########################################
      
      # Reset r variables (prevent bug later if datamart code doesn't work)
      r$patients <- tibble::tibble()
      r$stays <- tibble::tibble()
      r$labs_vitals <- tibble::tibble()
      r$text <- tibble::tibble()
      r$orders <- tibble::tibble()
      
      # Try to load datamart 
      tryCatch({
        run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language, quiet = TRUE)
        
        # A r variable to update Study dropdown, when the load of datamart is finished
        r$loaded_datamart <- r$chosen_datamart
        
        show_message_bar(output, 1, "import_datamart_success", "success", language, r$words)
      },
      error = function(e) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
        error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language))
      
      ##########################################
      # Render datamart UI                     #
      ##########################################
      
      # Show datamart UI, hide other UIs
      r$datamart_page <- Sys.time()
      
      # Show or hide datamart cards
      
      datamarts_cards <- c("datamart_options_card", "edit_datamart_code_card", "create_study_card", "studies_management_card",
        "import_study_card", "export_study_card")
      
      observeEvent(input$datamart_current_tab, {
        
        sapply(datamarts_cards %>% setdiff(., input$datamart_current_tab), shinyjs::hide)
        shinyjs::show(input$datamart_current_tab)
      })
    })
    
  })
}