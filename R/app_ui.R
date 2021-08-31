#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request, router_on = FALSE, css = "style.css", page_style = "fluent", page) {
  
  if (router_on == TRUE) page <- page$ui
  
  switch(page_style, "fluent" = shiny.fluent::fluentPage(page),
                     "fluid" = fluidPage(page),
                     "navbar" = navbarPage(page)) -> page
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    htmltools::tags$head(htmltools::tags$link(href = css, rel = "stylesheet", type = "text/css")),
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'cdwtools'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
  
  # shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
  # shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
  # shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
}

