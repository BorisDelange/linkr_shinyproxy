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
      
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "selected_plugin_card", label = "selected_plugin"),
      list(key = "all_plugins_card", label = "all_plugins")
    ), activated = "all_plugins_card", words = words),
    # 
    # render_settings_default_elements(ns = ns),
    div(
      id = ns("selected_plugin_card"),
      ""
    ),
    div(
      id = ns("all_plugins_card"),
      make_card(
        translate(language, "all_plugins", words = words),
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
              # shiny.fluent::DocumentCardDetails(
                shiny.fluent::DocumentCardTitle(
                  title = "Dygraph",
                  shouldTruncate = TRUE
                ),
              #   shiny.fluent::DocumentCardTitle(
              #     title = "With library dygraphs",
              #     shouldTruncate = TRUE,
              #     showAsSecondaryTitle = TRUE
              #   )
              # ),
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
              # shiny.fluent::DocumentCardDetails(
                shiny.fluent::DocumentCardTitle(
                  title = "Vistime",
                  shouldTruncate = TRUE
                ),
              #   shiny.fluent::DocumentCardTitle(
              #     title = "With library vistime",
              #     shouldTruncate = TRUE,
              #     showAsSecondaryTitle = TRUE
              #   )
              # ),
              shiny.fluent::DocumentCardActivity(
                activity = "2021-12-12",
                people = list(list(name = "Boris Delange"))
              )
            )
          )
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
    
    toggles <- c("selected_plugin_card", "all_plugins_card")

    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], {
        if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
        else shinyjs::hide(toggle)
      })
    })
    
  })
}