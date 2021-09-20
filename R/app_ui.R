#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request, css, page_style, page) {
  
  switch(page_style, "fluent" = shiny.fluent::fluentPage(page$ui),
                     "fluid" = fluidPage(page)) -> page
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(css),

    page
  )
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
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cdwtools'
    ),
    htmltools::tags$link(href = css, rel = "stylesheet", type = "text/css"),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
  
  # shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
  # shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
  # shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
}

