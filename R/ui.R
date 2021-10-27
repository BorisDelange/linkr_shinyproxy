#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' @import shiny
#' @noRd

app_ui <- function(request, css, page, language) {
  
  # Secure page with ShinyManager
  # shinymanager::secure_app(
    tagList(
      golem_add_external_resources(css),
      shiny.fluent::fluentPage(page$ui)
    )#,
    # enable_admin = FALSE, language = tolower(language), fab_position = "top-right"
  # )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function(css){
  
  add_resource_path('www', app_sys('app/www'))
 
  tags$head(
    # favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cdwtools'
    ),
    # Link to CSS file
    htmltools::tags$link(href = css, rel = "stylesheet", type = "text/css"),
    
    # Shinyjs is used to show and hide message bars
    shinyjs::useShinyjs(),
    
    # Shinybusy is used to add a busy bar on top of the page, when there are loading times
    shinybusy::add_busy_bar(timeout = 1000, color = "#0D98FF", height = "3px")
  )
}