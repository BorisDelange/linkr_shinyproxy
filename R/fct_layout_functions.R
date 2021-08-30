#' layout_functions
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
make_card <- function(title, content, size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

make_page <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    htmltools::span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    htmltools::span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

make_layout <- function(page_style = "fluent", main){
  content <- div(class = "grid-container",
               mod_page_header_ui("page_header_ui"),
               mod_page_sidenav_ui("page_sidenav_ui"),
               main,
               mod_page_footer_ui("page_footer_ui"))
  page <- switch(page_style, "fluent" = shiny.fluent::fluentPage(content),
                             "fluid" = fluidPage(content))

  # tagList(
  #   htmltools::tags$head(htmltools::tags$link(href = css, rel = "stylesheet", type = "text/css")),
  #   shiny.fluent::fluentPage(content)
  # )
  content
}

# router <- function(page){
#   shiny.router::make_router(
#     shiny.router::route("/", make_layout(page_style = "fluent", page)),
#     shiny.router::route("other", make_layout(page_style = "fluent", page))
#   )
# }