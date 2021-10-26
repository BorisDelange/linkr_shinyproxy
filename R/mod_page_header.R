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

mod_page_header_ui <- function(language){
  # ns <- NS(id)
  result <- div()
  
  div(class = "header",
    # ShinyManager logout button, hidden
    div(shinymanager::fab_button(inputId = "logout"), style = "display:none;"),
    
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
        # farItems = list(
        #   list(key = "disconnect", text = translate(language, "disconnect"), iconOnly = TRUE)
            # onClick = paste0("Shiny.setInputValue('disconnect', 1, {priority: 'event'})"))
          # list(id = "settigns", key = "settings", text = translate(language, "settings"), ),
          # shiny.fluent::CommandBarItem(translate(language, "settings"), "Settings", iconOnly = TRUE, 
          #   href = shiny.router::route_link("settings/general_settings")),
          # shiny.fluent::CommandBarItem(translate(language, "help"), "Info", iconOnly = TRUE,
          #   href = "https://borisdelange.github.io/cdwtools/articles/", target = "_blank"),
          # shiny.fluent::ActionButton.shinyInput("disconnect", "Disconnect")
          # shiny.fluent::CommandBarItem(translate(language, "disconnect"), "PowerButton", iconOnly = TRUE)
          # shiny.fluent::CommandBarItem(list(id = "disconnect", key = "disconnect", iconProps = list(iconName = "PowerButton")))
        # )
        items = list(
          shiny.fluent::CommandBarItem(translate(language, "settings"), "Settings", iconOnly = TRUE,
                                       href = shiny.router::route_link("settings/general_settings")),
          shiny.fluent::CommandBarItem(translate(language, "help"), "Info", iconOnly = TRUE,
                                       href = "https://borisdelange.github.io/cdwtools/articles/", target = "_blank"),
          # shiny.fluent::ActionButton.shinyInput("disconnect", "Disconnect")
          # shiny.fluent::CommandBarItem(translate(language, "disconnect"), "PowerButton", iconOnly = TRUE)
          shiny.fluent::CommandBarItem(list(id = "disconnect", key = "disconnect", iconProps = list(iconName = "PowerButton")))
        )
      )#,
      # textOutput("test")
    )
  )
}

mod_page_header_server <- function(input, output, session){
  # moduleServer(id, function(input, output, session){
    # ns <- session$ns
    
    # observeEvent(input$disconnect, {
    #   output$test <- renderText("TEST")
    #   shinyjs::click("logout")
    # })
    
  # })     
}