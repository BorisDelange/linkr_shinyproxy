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
      mod_page_main_ui(paste0("page_main_ui_", page), language, page_style, page),
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

make_textfield <- function(language, ns, label, id = NULL, value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL){
  if (is.null(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0("width: ", width)
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0("min-width: ", min_width, "; max-width: ", max_width)
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::TextField.shinyInput(ns(id), value = value, type = type, canRevealPassword = canRevealPassword), style = style)
  )
}

make_dropdown <- function(language, ns, label, options, id = NULL, value = NULL, width = NULL, min_width = NULL, max_width = NULL){
  # options <- lapply(split(names(options), options), unname)
  # if (is.null(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0("width: ", width)
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0("min-width: ", min_width, "; max-width: ", max_width)
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::Dropdown.shinyInput(ns(label), value = value, options = options), style = style)
  )
}

make_persona_picker <- function(language, ns, label, options, value = NULL, width = NULL, min_width = NULL, max_width = NULL){
  style <- ""
  if (!is.null(width)) style <- paste0("width: ", width)
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0("min-width: ", min_width, "; max-width: ", max_width)
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
     ns(label),
      options = options,
      pickerSuggestionsProps = list(
        suggestionsHeaderText = translate(language, "matching_people"),
        noResultsFoundText = translate(language, "no_results_found"),
        showRemoveButtons = TRUE
      ),
     defaultSelectedItems = options %>% dplyr::filter(key %in% value)),
      style = style
    )
  )
}