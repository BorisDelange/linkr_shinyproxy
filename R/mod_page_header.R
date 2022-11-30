#' page_header UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of current page (character)
#' @param language Language used (character)
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_header_ui <- function(id = character(), language = "EN", words = tibble::tibble(), i18n = R6::R6Class()){
  
  ns <- NS(id)

  # result <- div()

  div(class = "header",
    
    div(htmltools::img(src = "www/logo.png", style = "height: 25px;"), class = "logo"),
    div(class = "title", shiny.fluent::Text(variant = "xLarge", "LinkR")),
    div(class = "header_left_bar", 
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem(i18n$t("home"), "Home", href = shiny.router::route_link("home")),
          shiny.fluent::CommandBarItem(i18n$t("my_studies"), "CustomList", href = shiny.router::route_link("my_studies")),
          shiny.fluent::CommandBarItem(i18n$t("my_subsets"), "People", href = shiny.router::route_link("my_subsets")),
          shiny.fluent::CommandBarItem(i18n$t("data"), "BIDashboard", href = shiny.router::route_link("data")),
          shiny.fluent::CommandBarItem(i18n$t("thesaurus"), "AllApps", href = shiny.router::route_link("thesaurus")),
          shiny.fluent::CommandBarItem(i18n$t("scripts"), "CodeEdit", href = shiny.router::route_link("scripts")),
          shiny.fluent::CommandBarItem(i18n$t("plugins"), "Code", href = shiny.router::route_link("plugins/patient_lvl"))
        )
      )
    ),
    div(class = "header_right_bar",
      shiny.fluent::Stack(horizontal = TRUE, tokens = (childrenGap = 0),
        shiny.fluent::CommandBar(),
        div(uiOutput(ns("username")), style = "font-weight:bold; padding: 12px 10px 0px 0px;"),
        shiny.fluent::CommandBarButton.shinyInput(ns("help"), iconProps = list("iconName" = "Help")),
        # shiny.fluent::CommandBarButton.shinyInput("github_ref", iconProps = list("iconName" = "Info"), href = "https://borisdelange.github.io/cdwtools/articles/", target = "_blank"),
        shiny.fluent::CommandBarButton.shinyInput("settings", iconProps = list("iconName" = "Settings"), href = shiny.router::route_link("settings/general_settings")),
        shiny.fluent::CommandBarButton.shinyInput(".shinymanager_logout", iconProps = list("iconName" = "PowerButton"))
      )
    )
  )
}

#' page_header Server Functions
#'
#' @noRd 
mod_page_header_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$username <- renderUI(r$username)
  })
}