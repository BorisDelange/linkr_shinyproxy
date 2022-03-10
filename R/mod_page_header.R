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

mod_page_header_ui <- function(language = "EN", words = tibble::tibble()){
  
  result <- div()
  
  div(class = "header",
    
    div(htmltools::img(src = "www/logo.png", style = "height: 25px;"), class = "logo"),
    div(class = "title", shiny.fluent::Text(variant = "xLarge", "CDW Tools")),
    div(class = "header_left_bar", 
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem(translate(language, "home", words), "Home", href = shiny.router::route_link("home/get_started")),
          shiny.fluent::CommandBarItem(translate(language, "my_studies", words), "CustomList", href = shiny.router::route_link("my_studies")),
          shiny.fluent::CommandBarItem(translate(language, "my_subsets", words), "People", href = shiny.router::route_link("my_subsets")),
          shiny.fluent::CommandBarItem(translate(language, "thesaurus", words), "AllApps", href = shiny.router::route_link("thesaurus")),
          shiny.fluent::CommandBarItem(translate(language, "patient_level_data", words), "Contact", href = shiny.router::route_link("patient_level_data")),
          shiny.fluent::CommandBarItem(translate(language, "aggregated_data", words), "BIDashboard", href = shiny.router::route_link("aggregated_data")),
          shiny.fluent::CommandBarItem(translate(language, "plugins", words), "Code", href = shiny.router::route_link("plugins/patient_lvl"))
        )
      ),
    ),
    div(class = "header_right_bar",
      shiny.fluent::Stack(horizontal = TRUE, tokens = (childrenGap = 0),
        shiny.fluent::CommandBar(),
        shiny.fluent::CommandBarButton.shinyInput("help", iconProps = list("iconName" = "Help")),
        shiny.fluent::CommandBarButton.shinyInput("github_ref", iconProps = list("iconName" = "Info"), href = "https://borisdelange.github.io/cdwtools/articles/", target = "_blank"),
        shiny.fluent::CommandBarButton.shinyInput("settings", iconProps = list("iconName" = "Settings"), href = shiny.router::route_link("settings/general_settings")),
        shiny.fluent::CommandBarButton.shinyInput(".shinymanager_logout", iconProps = list("iconName" = "PowerButton"))
      )
    )
  )
}