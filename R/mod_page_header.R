#' top_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of current page (character)
#' @param language Language used (character)
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_header_ui <- function(id, language){
  ns <- NS(id)
  result <- div()
  
  div(class = "header",
    div(htmltools::img(src = "www/logo.png", style = "height: 25px;"), class = "logo"),
    div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
    div(class = "header_left_bar", 
        shiny.fluent::CommandBar(
          items = list(
            shiny.fluent::CommandBarItem(translate(language, "home"), "Home", href = shiny.router::route_link("home/datamarts_studies")),
            shiny.fluent::CommandBarItem(translate(language, "patient_level_data"), "Contact", href = shiny.router::route_link("patient_level_data")),
            shiny.fluent::CommandBarItem(translate(language, "aggregated_data"), "BIDashboard", href = shiny.router::route_link("aggregated_data"))
          )
        )
    ),
    div(class = "header_right_bar",
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem(translate(language, "settings"), "Settings", iconOnly = TRUE, 
            href = shiny.router::route_link("settings/general")),
          shiny.fluent::CommandBarItem(translate(language, "help"), "Info", iconOnly = TRUE,
            href = "https://borisdelange.github.io/cdwtools/articles/", target = "_blank"),
          shiny.fluent::CommandBarItem(translate(language, "messages"), "Message", iconOnly = TRUE,
            href = shiny.router::route_link("home/messages")),
          shiny.fluent::CommandBarItem(translate(language, "disconnect"), "PowerButton", iconOnly = TRUE)
        )
      )
    )
  )
}