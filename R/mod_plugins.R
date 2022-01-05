#' plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plugins_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)

  div(class = "main",
      
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = translate(language, id, words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "all_plugins_card", itemKey = "all_plugins", headerText = translate(language, "all_plugins", words)),
      shiny.fluent::PivotItem(id = "create_plugin_card", itemKey = "create_plugin", headerText = translate(language, "create_plugin", words)),
      shiny.fluent::PivotItem(id = "plugins_management_card", itemKey = "plugins_management", headerText = translate(language, "plugins_management", words)),
      shiny.fluent::PivotItem(id = "edit_plugin_code_card", itemKey = "edit_plugin_code", headerText = translate(language, "edit_plugin_code", words)),
      shiny.fluent::PivotItem(id = "import_plugin_card", itemKey = "import_plugin", headerText = translate(language, "import_plugin", words)),
      shiny.fluent::PivotItem(id = "export_plugin_card", itemKey = "export_plugin", headerText = translate(language, "export_plugin", words))
    ),
    div(
      id = ns("all_plugins_card"),
      make_card(translate(language, "all_plugins", words),
        div(
          br(),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
            shiny.fluent::DocumentCard(
              shiny.fluent::DocumentCardPreview(previewImages = list(
                list(
                  previewImageSrc = "https://cdn.thenewstack.io/media/2017/12/f67e69da-screen-shot-2017-12-28-at-4.17.36-pm.png",
                  width = 318,
                  height = 196
                ))
              ),
              shiny.fluent::DocumentCardTitle(
                title = "Dygraph",
                shouldTruncate = TRUE
              ),
              shiny.fluent::DocumentCardActivity(
                activity = "2020-05-21",
                people = list(list(name = "John Doe"))
              )
            ),
            shiny.fluent::DocumentCard(
              shiny.fluent::DocumentCardPreview(previewImages = list(
                list(
                  previewImageSrc = "https://cran.r-project.org/web/packages/vistime/readme/man/figures/ward_movements.png",
                  width = 318,
                  height = 196
                ))
              ),
              shiny.fluent::DocumentCardTitle(
                title = "Vistime",
                shouldTruncate = TRUE
              ),
              shiny.fluent::DocumentCardActivity(
                activity = "2021-12-12",
                people = list(list(name = "Boris Delange"))
              )
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("create_plugin_card"),
        make_card(translate(language, "create_plugin", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("plugins_management_card"),
        make_card(translate(language, "plugins_management", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("edit_plugin_code_card"),
        make_card(translate(language, "edit_plugin_code", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_plugin_card"),
        make_card(translate(language, "import_plugin", words),
          div("...")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("export_plugin_card"),
        make_card(translate(language, "export_plugin", words),
          div("...")
        )
      )
    )
  ) -> result
  
  result
}
    
#' plugins Server Functions
#'
#' @noRd 
mod_plugins_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("all_plugins_card", "create_plugin_card", "plugins_management_card", "edit_plugin_code_card",
      "import_plugin_card", "export_plugin_card")
    
    observeEvent(input$current_tab, {
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
  })
}