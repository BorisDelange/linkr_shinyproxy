#' Make a shiny.fluent card
#' 
#' @description Code available in shiny.fluent github pages (Articles). Creates a shiny.fluent card.
#' @param title Title of the card (character)
#' @param content Content of the card (character)
#' @param size Size of a card (integer)
#' @param style CSS code to custom the card (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' make_card(title = "Introduction", content = "This is the text of my introduction card", size = 12)

make_card <- function(title = character(), content = character(), size = 12, style = "") {
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
#' 
#' @description Code available in shiny.fluent github pages (Articles). Creates a shiny.fluent page.
#' @param title Title of the page (character)
#' @param subtitle Subtitle of the page (character)
#' @param contents Contents of the page (character)
#' @return Shiny UI elements / HTML code
#' @examples
#' make_page(title = "My page title", subtitle = "My page subtitle", contents = "shiny::div('My page content')")

make_page <- function (title = character(), subtitle = character(), contents = character()) {
  tagList(div(
    class = "page-title",
    htmltools::span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style = "color: #323130"),
    htmltools::span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style = "color: #605E5C; margin: 14px;")
  ),
  contents)
}

#' Make a complete layout with header, sidenav, main & footer
make_layout <- function(language, page){
  div(class = "grid-container",
    mod_page_header_ui(language = language),
    mod_page_sidenav_ui(id = stringr::str_replace(page, "/", "_"), language = language),
    mod_page_main_ui(id = stringr::str_replace(page, "/", "_"), language = language),
    mod_page_footer_ui()
  )
}

#' Make a shiny.fluent textfield
#' 
#' @return Shiny UI elements / HTML code
#' @examples
make_textfield <- function(language = "EN", ns = shiny::NS(), label = character(), id = NA_character_, 
  value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL, margin_right = NULL){
  if (is.na(id)) id <- label
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
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the dropdown (list)
#' @param value Value of the toggle : TRUE or FALSE (logical)
#' @param multiSelect Is multiselection of options is possible ? (logical)
#' @param width Width of the dropdown, CSS code so "300px" or "100\%" are accepted
#' @examples 
#' \dontrun{
#' options <- list(
#'   list(key = "my_key1", text = "my_text1"),
#'   list(key = "my_key2", text = "my_text2")
#' )
#' make_dropdown(language = "EN", ns = NS("settings_datamarts"), label = "my_dropdown", id = "my_dropdown",
#'   options = options, multiSelect = FALSE, value = "my_key1", width = "100%")
#' }

make_dropdown <- function(language = "EN", ns = shiny::NS(), label = character(), options = list(), multiSelect = FALSE,
  id = NA_character_, value = NULL, width = NULL){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::Dropdown.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect), style = style)
  )
}

#' Make a shiny.fluent combo box
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the dropdown (list)
#' @param value Value of the toggle : TRUE or FALSE (logical)
#' @param multiSelect Is multiselection of options is possible ? (logical)
#' @param width Width of the dropdown, CSS code so "300px" or "100\%" are accepted
#' @param allowFreeForm Allows user to enter free text, not provided by options (logical)

make_combobox <- function(language = "EN", ns = shiny::NS(), label = character(), options = list(), multiSelect = TRUE,
  allowFreeform = FALSE, autoComplete = "on", id = NA_character_, value = NULL, width = NULL){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::ComboBox.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect, 
      allowFreeform = allowFreeform, autoComplete = autoComplete,
      multiSelectDelimiter = ","), style = style)
  )
}


#' Make a shiny.fluent people picker
#' 
#' @description Creates a shiny.fluent NormalPeoplePicker
#' @details 
#' options argument has to be a data.frame or a tibble, with columns = c("key", "imageInitials", "text", "secondaryText")
#' key is the ID of the choice, text is the text of the choice, imageInitials are the letters put on a circle,
#' secondaryText is the text under the principal text
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the input (data.frame or tibble)
#' @param value Options already selected (character)
#' @param width CSS code for width, could be all CSS forms, as "200px" or "100\%" etc (character)
#' @return HTML / Shiny UI code
#' @examples
#' \dontrun{
#' options <- tibble::tribble(~key,  ~imageInitials, ~text, ~secondaryText,
#'   1, "JD", "John Doe", "Clinician",
#'   2, "DA", "Doug Altman", "Statistician")
#' make_people_picker(language = "EN", ns = ns, id = "my_people_picker", "My people picker", options = options, value = 2, width = "200px")
#' }

make_people_picker <- function(language = "EN", ns = shiny::NS(), id = NA_character_, label = character(), 
  options = tibble::tibble(), value = NULL, width = NULL, style = character()){
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width)
  if (is.na(id)) id <- label
  div(
    div(class = "input_title", translate(language, label)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
      ns(id),
      options = options,
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
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param value Value of the toggle : TRUE or FALSE (logical)
#' @param inline Should the toggle displayed inline (logical)
#' @return HTML / Shiny UI code
#' @examples
#' \dontrun{
#' make_toggle(language = "EN", ns = ns, label = "My toggle", id = "my_toggle", value = TRUE, inline = FALSE)
#' }

make_toggle <- function(language = "EN", ns = shiny::NS(), label = character(), id = NULL, value = FALSE, inline = FALSE){
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
#' @param output Shiny output variable
#' @param id ID of the warning output used (integer)
#' @param message message that will be displayed, after translation (character)
#' @param type type of message bar displayed (reference to Microsoft MessageBarType enum) (character)
#' @param language language used for the translation (character)
#' @param time time the message bar will by displayed, in ms (integer)
#' @examples 
#' \dontrun{
#' message_bar(id = 2, message = "name_already_used", type = "severeWarning", language = language, time = 5000)
#' }

show_message_bar <- function(output, id = integer(), message = character(), type = "severeWarning", language = "EN", time = 7000){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  output[[paste0("message_bar", id)]] <- renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = type), style = "margin-top:10px;"))
}