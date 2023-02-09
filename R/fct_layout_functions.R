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

make_card_shiny_ace <- function(title = character(), content = character(), size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
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
make_layout <- function(language = "EN", page = character(), words = tibble::tibble(), i18n = R6::R6Class()){
  div(class = "grid-container",
    mod_page_header_ui(id = stringr::str_replace(page, "/", "_"), language = language, words = words, i18n = i18n),
    mod_page_sidenav_ui(id = stringr::str_replace(page, "/", "_"), language = language, words = words, i18n = i18n),
    mod_page_main_ui(id = stringr::str_replace(page, "/", "_"), language = language, words = words, i18n = i18n),
    mod_page_footer_ui(words = words, i18n = i18n)
  )
}

#' Make a shiny.fluent textfield
#' 
#' @return Shiny UI elements / HTML code
#' @examples
#' 
make_textfield <- function(language = "EN", ns = shiny::NS(), label = character(), id = NA_character_, 
  value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL, 
  margin_right = NULL, words = tibble::tibble()){
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0(style, "min-width: ", min_width, "; max-width: ", max_width, ";")
  if (!is.null(margin_right)) style <- paste0(style, "margin-right:", margin_right, ";")
  div(
    div(class = "input_title", translate(language, label, words)),
    div(shiny.fluent::TextField.shinyInput(ns(id), value = value, type = type, canRevealPassword = canRevealPassword), style = style)
  )
}

#' Make a shiny.fluent textfield
#' 
#' @return Shiny UI elements / HTML code
#' @examples
#' 
make_textfield_new <- function(i18n = R6::R6Class(), ns = shiny::NS(), label = character(), id = NA_character_, 
  value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL, 
  margin_right = NULL, disabled = FALSE){
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0(style, "min-width: ", min_width, "; max-width: ", max_width, ";")
  if (!is.null(margin_right)) style <- paste0(style, "margin-right:", margin_right, ";")
  div(
    div(class = "input_title", i18n$t(label)),
    div(shiny.fluent::TextField.shinyInput(ns(id), value = value, type = type, canRevealPassword = canRevealPassword, disabled = disabled), style = style)
  )
}

#' Make a shiny.fluent dropdown
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the dropdown (list)
#' @param value Value of the dropdown (character)
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
  id = NA_character_, value = NULL, width = NULL, words = tibble::tibble()){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(id = ns(paste0(id, "_title")), class = "input_title", translate(language, label, words)),
    div(shiny.fluent::Dropdown.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect), style = style)
  )
}

make_dropdown_new <- function(i18n = R6::R6Class(), ns = shiny::NS(), label = character(), options = list(), multiSelect = FALSE,
  id = NA_character_, value = NULL, width = NULL){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(id = ns(paste0(id, "_title")), class = "input_title", i18n$t(label)),
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

make_combobox <- function(language = "EN", ns = shiny::NS(), label = character(), options = list(), multiSelect = FALSE,
  allowFreeform = FALSE, autoComplete = "on", id = NA_character_, value = NULL, width = NULL, words = tibble::tibble()){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(class = "input_title", translate(language, label, words)),
    div(shiny.fluent::ComboBox.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect, 
      allowFreeform = allowFreeform, autoComplete = autoComplete,
      multiSelectDelimiter = ","), style = style)
  )
}

make_combobox_new <- function(i18n = R6::R6Class(), ns = shiny::NS(), label = character(), options = list(), multiSelect = FALSE,
  allowFreeform = FALSE, autoComplete = "on", id = NA_character_, value = NULL, width = NULL, words = tibble::tibble()){
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(class = "input_title", i18n$t(label)),
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
  options = tibble::tibble(), value = NULL, width = NULL, style = character(), words = tibble::tibble()){
  
  if (!is.null(value)) default_selected_items <- options %>% dplyr::filter(key %in% value)
  else default_selected_items <- NULL
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width)
  if (is.na(id)) id <- label
  div(
    div(class = "input_title", translate(language, label, words)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
      ns(id),
      options = options,
      pickerSuggestionsProps = list(
        suggestionsHeaderText = translate(language, "matching_people", words),
        noResultsFoundText = translate(language, "no_results_found", words),
        showRemoveButtons = TRUE
      ),
      defaultSelectedItems = default_selected_items,
      value = value),
      style = style
    )
  )
}

make_people_picker_new <- function(i18n = R6::R6Class(), ns = shiny::NS(), id = NA_character_, label = character(), 
  options = tibble::tibble(), value = NULL, width = NULL, style = character()){
  
  if (!is.null(value)) default_selected_items <- options %>% dplyr::filter(key %in% value)
  else default_selected_items <- NULL
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width)
  if (is.na(id)) id <- label
  div(
    div(class = "input_title", i18n$t(label)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
      ns(id),
      options = options,
      pickerSuggestionsProps = list(
        suggestionsHeaderText = i18n$t("users"),
        noResultsFoundText = i18n$t("no_results_found"),
        showRemoveButtons = TRUE
      ),
      defaultSelectedItems = default_selected_items,
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

make_toggle <- function(language = "EN", ns = shiny::NS(), label = character(), id = NULL, value = FALSE, 
  inline = FALSE, translate = TRUE, words = tibble::tibble()){
  if (is.null(id)) id <- label
  if (translate) label <- translate(language, label, words)
  if (inline){
    tagList(
      shiny.fluent::Toggle.shinyInput(ns(id), value = value),
      div(class = "toggle_title", label)
    ) -> result
  }
  if (!inline){
    tagList(
      div(class = "input_title", label),
      shiny.fluent::Toggle.shinyInput(ns(id), value = value)
    )  -> result
  }
  result
}

make_toggle_new <- function(i18n = R6::R6Class(), ns = shiny::NS(), label = character(), id = NULL, value = FALSE, inline = FALSE, translate = TRUE, bold = TRUE){
  if (is.null(id)) id <- label
  if (translate) label <- i18n$t(label)
  if (bold) style <- "" else style <- "font-weight:normal;"
  if (inline){
    tagList(
      shiny.fluent::Toggle.shinyInput(ns(id), value = value),
      div(class = "toggle_title", label, style = style)
    ) -> result
  }
  if (!inline){
    tagList(
      div(class = "input_title", label, style = style),
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

show_message_bar <- function(output, id = integer(), message = character(), type = "severeWarning", language = "EN", words = tibble::tibble(), time = 7000){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  
  # If translation of the message doesn't exist, return raw message
  output_message <- translate(language, message, words)
  if (output_message == "") output_message <- message
  
  output[[paste0("message_bar", id)]] <- renderUI(div(shiny.fluent::MessageBar(output_message, messageBarType = type), style = "margin-top:10px;"))
}

show_message_bar_new <- function(output, id = integer(), message = character(), type = "severeWarning", i18n = R6::R6Class(), time = 7000, ns = character()){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  
  id <- sample(1:20, 1)
  
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  
  output_message <- i18n$t(message)
  
  if (length(ns) > 0){
    output[[paste0("message_bar", id)]] <- renderUI(div(
      div(shiny.fluent::MessageBar(output_message, messageBarType = type), style = "z-index:1; margin-top:10px; height:30px; margin-bottom:-30px; max-width:calc(100% - 330px);"),
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0("close_message_bar_", id)), "", iconProps = list(iconName = "Cancel")), 
        style = "position:sticky; z-index:2; margin-left:calc(100% - 362px);"
      ))
    )
  }
  else {
    output[[paste0("message_bar", id)]] <- renderUI(div(
      shiny.fluent::MessageBar(output_message, messageBarType = type),
      style = "margin-top:10px; max-width:calc(100% - 330px);")
    )
  }
}

#' Make a shiny.fluent choiceGroup
#'
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the choiceGroup (list)
#' @param value Value of the choiceGroup (character)
#' @param inline Is the choiceGroup displayed inline ? (logical)

make_choicegroup <- function(language = "EN", ns = shiny::NS(), label = character(), id = NA_character_, options = list(), value = character(), inline = FALSE){
  
  if (is.na(id)) id <- label
  if (inline) shiny.fluent::ChoiceGroup.shinyInput(ns(id), options = options, value = value, className = "inline_choicegroup")
  else shiny.fluent::ChoiceGroup.shinyInput(ns(id), options = options, value = value)
}

#' Render a DT datatable
#' 
#' @description Renders a datatable (from library DT)
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param ns Shiny namespace
#' @param language Language used (charater)
#' @param data data used in the datatable (tibble or dataframe)
#' @param output_name Name of the datatable output
#' @param col_names A character vector containing colnames, already translated (character)
#' @param datatable_dom Character containing DOM code for the datatable (character)
#' @param page_length Page length of the datatable, default to 10 rows (integer)
#' @param start Which page display (used when we save datatable state), default to 1 (integer)
#' @param editable_cols Which cols are editable (character vector)
#' @param sortable_cols Which cols are sortable (character vector)
#' @param centered_cols Which cols are centered (character vector)
#' @param filter If TRUE, we can filter we search box each column (logical)
#' @param searchable_cols If filter is TRUE, choose which columns are searchable (character)
#' @param factorize_cols Which columns are factorized (to be filtered with a dropdown) (character)
#' @param column_widths Columns widths (named character vector)

render_datatable <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN", data = tibble::tibble(),
  output_name = character(), col_names = character(), datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", page_length = 10, start = 0,
  editable_cols = character(), sortable_cols = character(), centered_cols = character(), searchable_cols = character(), 
  filter = FALSE, factorize_cols = character(), column_widths = character(), hidden_cols = character(), truncated_cols = character(),
  default_tibble = tibble::tibble()
){
  
  # Translation for datatable
  dt_translation <- list(
    paginate = list(previous = translate(language, "DT_previous_page", r$words), `next` = translate(language, "DT_next_page", r$words)),
    search = translate(language, "DT_search", r$words),
    lengthMenu = translate(language, "DT_length", r$words),
    emptyTable = translate(language, "DT_empty", r$words))
  
  # If no row in dataframe, stop here
  if (nrow(data) == 0){
    
    if (length(names(default_tibble)) == 0) return({
      data <- tibble::tribble(~id, ~datetime)
      names(data) <- c(translate(language, "id", r$words), translate(language, "datetime", r$words))
      output[[output_name]] <- DT::renderDT(data, options = list(dom = datatable_dom), 
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE)
    })
    
    if (length(names(default_tibble)) > 0) data <- default_tibble
  }

  
  # Which columns are non editable
  
  cols <- c(1:length(names(data))) - 1
  editable_cols_vec <- integer()
  sapply(editable_cols, function(col){
    editable_cols_vec <<- c(editable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_editable_cols_vec <- cols[!cols %in% editable_cols_vec]
  
  # Which columns are non sortable
  sortable_cols_vec <- integer()
  sapply(sortable_cols, function(col){
    sortable_cols_vec <<- c(sortable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
  
  # Which cols are centered
  centered_cols_vec <- integer()
  sapply(centered_cols, function(col){
    centered_cols_vec <<- c(centered_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are hidden
  hidden_cols_vec <- integer()
  sapply(hidden_cols, function(col){
    hidden_cols_vec <<- c(hidden_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are searchable
  searchable_cols_vec <- integer()
  sapply(searchable_cols, function(col){
    searchable_cols_vec <<- c(searchable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_searchable_cols_vec <- cols[!cols %in% searchable_cols_vec]
  
  # Which cols are truncated
  truncated_cols_vec <- integer()
  sapply(truncated_cols, function(col){
    truncated_cols_vec <<- c(truncated_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # If filter is TRUE
  if (filter) filter_list <- list(position = "top")
  if (!filter) filter_list <- list()
  
  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add hidden_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(visible = FALSE, targets = hidden_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
  
  # Add searchable cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(searchable = FALSE, targets = non_searchable_cols_vec))
  
  # Truncate long text
  column_defs <- rlist::list.append(column_defs, list(render = htmlwidgets::JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data != null && data.length > 30 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
    "}"), targets = truncated_cols_vec))
  
  # Transform searchable cols to factor
  sapply(factorize_cols, function(col) data <<- data %>% dplyr::mutate_at(col, as.factor))
  
  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable
  
  output[[output_name]] <- DT::renderDT(
    # Data
    data,
    
    # Options of the datatable
    options = list(
      dom = datatable_dom,
      pageLength = page_length, displayStart = start,
      columnDefs = column_defs,
      language = dt_translation,
      compact = TRUE, hover = TRUE
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    
    # Default options
    rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    
    # Javascript code allowing to have dropdowns & actionButtons on the DataTable
    callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
      var $this = $(this.node());
      $this.attr('id', this.data()[0]);
      $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
}

render_datatable_new <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), i18n = R6::R6Class(), data = tibble::tibble(),
  output_name = character(), col_names = character(), datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", page_length = 10, start = 0,
  editable_cols = character(), sortable_cols = character(), centered_cols = character(), searchable_cols = character(), 
  filter = FALSE, factorize_cols = character(), column_widths = character(), hidden_cols = character(), truncated_cols = character(),
  selection = "single", default_tibble = tibble::tibble()
){
  
  # Translation for datatable
  dt_translation <- list(
    paginate = list(previous = i18n$t("dt_previous"), `next` = i18n$t("dt_next")),
    search = i18n$t("dt_search"),
    lengthMenu = i18n$t("dt_entries"),
    emptyTable = i18n$t("dt_empty"))
  
  # If no row in dataframe, stop here
  if (nrow(data) == 0){
    
    if (length(names(default_tibble)) == 0) return({
      data <- tibble::tribble(~id)
      names(data) <- c("")
      output[[output_name]] <- DT::renderDT(data, options = list(dom = datatable_dom, language = dt_translation), 
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE)
    })
    
    if (length(names(default_tibble)) > 0) data <- default_tibble
  }
  
  
  # Which columns are non editable
  
  cols <- c(1:length(names(data))) - 1
  editable_cols_vec <- integer()
  sapply(editable_cols, function(col){
    editable_cols_vec <<- c(editable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_editable_cols_vec <- cols[!cols %in% editable_cols_vec]
  
  # Which columns are non sortable
  sortable_cols_vec <- integer()
  sapply(sortable_cols, function(col){
    sortable_cols_vec <<- c(sortable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
  
  # Which cols are centered
  centered_cols_vec <- integer()
  sapply(centered_cols, function(col){
    centered_cols_vec <<- c(centered_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are hidden
  hidden_cols_vec <- integer()
  sapply(hidden_cols, function(col){
    hidden_cols_vec <<- c(hidden_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are searchable
  searchable_cols_vec <- integer()
  sapply(searchable_cols, function(col){
    searchable_cols_vec <<- c(searchable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_searchable_cols_vec <- cols[!cols %in% searchable_cols_vec]
  
  # Which cols are truncated
  truncated_cols_vec <- integer()
  sapply(truncated_cols, function(col){
    truncated_cols_vec <<- c(truncated_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are with non-selection attribute
  # no_selection_cols_vec <- integer()
  # sapply(no_selection_cols, function(col){
  #   no_selection_cols_vec <<- c(no_selection_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  # })
  
  # If filter is TRUE
  if (filter) filter_list <- list(position = "top")
  if (!filter) filter_list <- list()
  
  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add hidden_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(visible = FALSE, targets = hidden_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
  
  # Add searchable cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(searchable = FALSE, targets = non_searchable_cols_vec))
  
  # Add no_selection cols to columns_defs
  # column_defs <- rlist::list.append(column_defs, list(className = "no-selection", targets = no_selection_cols_vec))
  
  # Truncate long text
  column_defs <- rlist::list.append(column_defs, list(render = htmlwidgets::JS(
    "function(data, type, row, meta) {",
    "return type === 'display' && data != null && data.length > 30 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
    "}"), targets = truncated_cols_vec))
  
  # Transform searchable cols to factor
  sapply(factorize_cols, function(col) data <<- data %>% dplyr::mutate_at(col, as.factor))
  
  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable
  
  output[[output_name]] <- DT::renderDT(
    # Data
    data,
    
    # Options of the datatable
    options = list(
      dom = datatable_dom,
      pageLength = page_length, displayStart = start,
      columnDefs = column_defs,
      language = dt_translation,
      compact = TRUE, hover = TRUE
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    selection = selection,
    
    # Default options
    rownames = FALSE, escape = FALSE, server = TRUE,
    
    # Javascript code allowing to have dropdowns & actionButtons on the DataTable
    callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
      var $this = $(this.node());
      $this.attr('id', this.data()[0]);
      $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
      # table.on('select.dt', function(e, dt, type, indexes) {
      #   if (table.columns('.no-selection').indexes().length) {
      #     table.rows(indexes).deselect();
      #   }
      # });")
  )
}