#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request, router) {
  # root_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  # other_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  # 
  # router <- shiny.router::make_router(
  #   shiny.router::route("/", root_page),
  #   shiny.router::route("other", other_page)
  # )
  
  # my_router <- router(mod_page_main_ui("page_main_ui_1"))
  
  # root_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  # other_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  # my_router <- shiny.router::make_router(
  #   shiny.router::route("/", root_page),
  #   shiny.router::route("other", other_page)
  # )
  # shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
  # shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
  # shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

  tagList(

    # Leave this function for adding external resources
    golem_add_external_resources(),

    # htmltools::tags$head(
    #   htmltools::tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
    # ),
    # shiny.fluent::fluentPage(
    #   div(class = "grid-container",
    #       mod_page_header_ui("page_header"),
    #       mod_page_sidenav_ui("page_sidenav_ui_1"),
    #       mod_page_main_ui("page_main_ui_1"),
    #       mod_page_footer_ui("page_footer_ui")
    #   )
    # )

    htmltools::tags$head(htmltools::tags$link(href = "style.css", rel = "stylesheet", type = "text/css")),
                         # shiny_router_script_tag),
    # shiny.fluent::fluentPage(my_router$ui,
    #                          htmltools::tags$head(htmltools::tags$link(href = "style.css", rel = "stylesheet", type = "text/css")))
    # make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
    # shiny.fluent::fluentPage(make_layout(main = mod_page_main_ui("page_main_ui_1")))
    # make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
    # my_router$ui
    shiny.fluent::fluentPage(
      htmltools::tags$head(htmltools::tags$link(href = "style.css", rel = "stylesheet", type = "text/css")),
      title = "Router demo",
      router$ui
    )
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

