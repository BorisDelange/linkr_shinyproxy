#' Make a shiny.fluent card
#' 
#' @description Code available in shiny.fluent github pages (Articles). Creates a shiny.fluent card.
#' @param title Title of the card (character)
#' @param content Content of the card (character)
#' @param size Size of a card (integer)
#' @param style CSS code to custom the card (character)
#' 
#' @result Returns HTML code of the card
#' 
#' @examples 
#' \dontrun{
#' make_card(title = "Introduction", content = "This is the text of my introduction card", size = 12)
#' }
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

#' Make a page
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

#' Make a complete layout with header, sidenav, main & footer
make_layout <- function(language, page_style, page){
  if (page_style == "fluent"){
    div(class = "grid-container",
      mod_page_header_ui(page, language, page_style, page),
      mod_page_sidenav_ui(page, language, page_style, page),
      mod_page_main_ui(page, language, page_style, page),
      mod_page_footer_ui(page, language, page_style, page)
    ) -> result
  }
  
  if (page_style == "fluid"){
    shiny::tabPanel(title = translate(language, page), 
      shiny::sidebarLayout(
        mod_page_sidenav_ui(page, language, page_style, page),
        mod_page_main_ui(page, language, page_style, page)
      )
    ) -> result
  }
  
  result
}

#' Make a shiny.fluent textfield
make_textfield <- function(language, ns, label, id = NULL, value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL, margin_right = NULL){
  if (is.null(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0(style, "min-width: ", min_width, "; max-width: ", max_width, ";")
  if (!is.null(margin_right)) style <- paste0(style, "margin-right:", margin_right, ";")
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::TextField.shinyInput(ns(id), value = value, type = type, canRevealPassword = canRevealPassword), style = style)
  )
}

#' Make a shiny.fluent dropdown
make_dropdown <- function(language, ns, label, options = list(), multiSelect = FALSE,
                          id = NULL, value = NULL, width = NULL, min_width = NULL, max_width = NULL, margin_right = NULL){
  if (is.null(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0(style, "min-width: ", min_width, "; max-width: ", max_width, ";")
  if (!is.null(margin_right)) style <- paste0(style, "margin-right:", margin_right, ";")
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::Dropdown.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect), style = style)
  )
}

#' Make a shiny.fluent people picker
make_people_picker <- function(language, ns, label, options, value = NULL, width = NULL, min_width = NULL, max_width = NULL){
  style <- ""
  if (!is.null(width)) style <- paste0("width: ", width)
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0("min-width: ", min_width, "; max-width: ", max_width)
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
      ns(label),
      options = options,# %>% dplyr::filter(key %not_in% value),
      pickerSuggestionsProps = list(
        suggestionsHeaderText = translate(language, "matching_people"),
        noResultsFoundText = translate(language, "no_results_found"),
        showRemoveButtons = TRUE
      ),
      defaultSelectedItems = options %>% dplyr::filter(key %in% value),
      value = value),
      style = style
    )
  )
}

#' Make a shiny.fluent toggle
make_toggle <- function(language, ns, label, id = NULL, value = FALSE, inline = FALSE){
  if (is.null(id)) id <- label
  if (inline){
    tagList(
      shiny.fluent::Toggle.shinyInput(ns(id), value = value),
      div(class = "toggle_title", translate(language, label))
    ) -> result
  }
  if (!inline){
    tagList(
      div(class = "input_title", translate(language, label)),
      shiny.fluent::Toggle.shinyInput(ns(id), value = value)
    )  -> result
  }
  result
}

#' Display an error message
#' 
#' @description Displays an error message on the top of the page
#' @details Choose different warning IDs on one page, to allows multiple messages to be displayed.
#' The different possible types are : c("info", "error", "blocked", "severeWarning", "success", "warning")
#' @param id ID of the warning output used (integer)
#' @param message message that will be displayed, after translation (character)
#' @param type type of message bar displayed (reference to Microsoft MessageBarType enum) (character)
#' @param language language used for the translation (character)
#' @param time time the message bar will by displayed, in ms (integer)
#' @examples 
#' \dontrun{
#' message_bar(id = 2, message = "name_already_used", type = "severeWarning", language = language, time = 5000)
#' }

show_message_bar <- function(output, id = integer(), message = character(), type = "severeWarning", language = "EN", time = 3000){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  output[[paste0("message_bar", id)]] <- renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = type), style = "margin-top:10px;"))
}