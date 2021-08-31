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

make_layout <- function(language, page_style, page){
  if (page_style == "fluent"){
    div(class = "grid-container",
      mod_page_header_ui(paste0("page_header_ui_", page), language, page_style, page),
      mod_page_sidenav_ui(paste0("page_sidenav_ui_", page), language, page_style, page),
      mod_page_main_ui(paste0("page_header_ui_", page), language, page_style, page),
      mod_page_footer_ui(paste0("page_footer_ui_", page), language, page_style, page)
    ) -> result
  }
  
  if (page_style == "fluid"){
    shiny::tabPanel(title = translate(language, page), 
      shiny::sidebarLayout(
        mod_page_sidenav_ui(paste0("page_sidenav_ui_", page), language, page_style, page),
        mod_page_main_ui(paste0("page_header_ui_", page), language, page_style, page)
      )                
    ) -> result
  }
  
  result
}

make_textfield <- function(language, label){
  div(div(class = "input_title", translate(language, label)),
      shiny.fluent::TextField.shinyInput(label))
}