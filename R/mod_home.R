#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "get_started", text = translate(language, "get_started", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "import_excel_card", itemKey = "import_excel", headerText = translate(language, "import_excel", words)),
      shiny.fluent::PivotItem(id = "import_csv_card", itemKey = "import_csv", headerText = translate(language, "import_csv", words)),
      shiny.fluent::PivotItem(id = "connect_db_card", itemKey = "connect_db", headerText = translate(language, "connect_db", words))
    ),
    div(
      id = ns("import_excel_card"),
      make_card(translate(language, "import_excel", words),
        div("...")
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_csv_card"),
        make_card(translate(language, "import_csv", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("connect_db_card"),
        make_card(translate(language, "connect_db", words),
          div("...")
        )
      )
    )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # Show or hide datamart cards
    
    cards <- c("import_excel_card", "import_csv_card", "connect_db_card")
    
    observeEvent(input$current_tab, {
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
  })
}