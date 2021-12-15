##########################################
# Default elements                       #
##########################################

#' Render UI of settings default elements
#' 
#' @description Set default UI elements on top of the page : message_bar outputs, react output to confirm delete of a table element
#' 
#' @param ns Shiny namespace
#' @return Shiny UI elements / HTML code
#' @examples
#' render_settings_default_elements(ns = NS("settings_datamart"))

render_settings_default_elements <- function(ns = shiny::NS()){
  tagList(shiny::uiOutput(ns("message_bar1")), shiny::uiOutput(ns("message_bar2")), shiny::uiOutput(ns("message_bar3")), 
    shiny::uiOutput(ns("message_bar4")), shiny::uiOutput(ns("message_bar5")), shiny.fluent::reactOutput(ns("delete_confirm")))
}

##########################################
# Toggle card                            #
##########################################

#' Render UI of settings toggle card
#' 
#' @description At the top of main page, there's a card with toggle buttons, it allows to show or hide distinct cards of the page.
#' @param ns Shiny namespace
#' @param cards A list containing distinct cards (list)
#' @param activated Which toggles are set to ON (character)
#' @param language Language used (character)
#' @return Shiny UI elements / HTML code
#' @examples
#' cards <- list(
#'   list(key = "creation_card", label = "create_datamart"),
#'   list(key = "datatable_card", label = "datamarts_management"),
#'   list(key = "edit_code_card", label = "edit_datamart_code"))
#' settings_toggle_card(ns = NS("settings_datamart"), cards = cards, activated = c("creation_card", "datatable_card"), language = "EN")

render_settings_toggle_card <- function(language = "EN", ns = shiny::NS(), cards = list(), activated = "", translate = TRUE, words = tibble::tibble()){
  
  toggles <- tagList()
  # For each card, create a toggle
  sapply(cards, function(card){
    if (card$label != "") toggles <<- tagList(toggles, 
      make_toggle(language, ns, label = card$label, 
        id = paste0(card$key, "_toggle"), value = ifelse(card$key %in% activated, TRUE, FALSE), inline = TRUE, translate = translate, words = words))
  })
  # Render card with distinct togglesmo
  div(id = ns("toggles"),
    make_card("",
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10), toggles
      )
    )
  )
}

##########################################
# Creation card                          #
##########################################

#' Render UI of settings creation card
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param id ID of current module / page (character)
#' @param title Title used to create the card, it will be translated with translate function (character)
#' @param textfields A character vector containing distinct textfields to render in the card (character)
#' @param textfields_width Width of the textfields, CSS code, so it could be "100\%", "200px" etc (character)
#' @param dropdowns A character vector containing distinct dropdowns to render in the card (character)
#' @param dropdowns_width Width of the dropdowns, CSS code, so it could be "100\%", "200px" etc (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' render_settings_creation_card(language = "EN", ns = NS("settings_datamart"), title = "create_datamart",
#' textfields = c("name", "description"), dropdowns = "data_source")

render_settings_creation_card <- function(language = "EN", ns = shiny::NS(), id = character(), title = character(), 
  textfields = character(), textfields_width = "200px", dropdowns = character(), dropdowns_width = "200px", words = tibble::tibble()){
  
  # For plugins dropdown, add a dropdown with module choice
  plugins_dropdown <- ""
  if (id == "settings_plugins"){
    plugins_dropdown <- make_dropdown(language = language, ns = ns, label = "module_type", id = "module_type", width = "300px", 
      options = list(
        list(key = 1, text = translate(language, "patient_level_data", words)),
        list(key = 2, text = translate(language, "aggregated_data", words))
    ), value = 1, words = words)
  }
  
  div(id = ns("creation_card"),
    make_card(
      title = translate(language, title, words),
      content = div(
        shiny.fluent::Stack(
          # Horizontal alignment, with gap of 50 px between elements
          horizontal = TRUE, tokens = list(childrenGap = 50),
          # For each textfield, use make_textfield function
          lapply(textfields, function(label){
            if (label == "password") make_textfield(language = language, ns = ns, label = label, id = label, width = textfields_width, type = "password", canRevealPassword = TRUE, words = words)
            else make_textfield(language = language, ns = ns, label = label, id = label, width = textfields_width, words = words)
          })
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(dropdowns, function(label){
            # Allow multiSelect for thesaurus, column data source
            multiSelect <- FALSE
            if (id == "settings_thesaurus") multiSelect <- TRUE
            make_dropdown(language = language, ns = ns, label = label, id = label, multiSelect = multiSelect, width = dropdowns_width, words = words)
          })
        ), 
        plugins_dropdown, br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add", words))
      )
    )
  )
}

##########################################
# Datatable card                         #
##########################################

#' Render UI of settings datatable card
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param div_id ID of the div, to show or hide with toggles, default = "datatable_card" (character)
#' @param output_id ID of div & DTOutput, allows to have multiple management_datatable in one module, default = "management_datatable" (character)
#' @param title Title used to create the card, it will be translated with translate function (character)
#' @examples 
#' \dontrun{
#' render_settings_datatable_card(language = "EN", ns = ns, output_id = "management_datatable", title = "datamarts_management")
#' }
render_settings_datatable_card <- function(language = "EN", ns = shiny::NS(), div_id = "datatable_card", 
  output_id = "management_datatable", title = character(), words = tibble::tibble()){
  div(id = ns(div_id),
    make_card(translate(language, title, words),
      div(
        DT::DTOutput(ns(output_id)),
        shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save", words))
      )
    )
  )
}

##########################################
# Options card                           #
##########################################

#' Render UI of options card
#' 
#' @param ns Shiny namespace
#' @param r Shiny r reactive value
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param title Title of the card (character)
#' @param code Code to show in ShinyAce editor (character)
#' @param category Category allowing to link with code table (character)
#' @param link_id ID allowing to link with code table (integer)
#' @param language Language used (character)

render_settings_options_card <- function(ns = shiny::NS(), r = r, id = character(), title = character(), code = character(), 
  category = character(), link_id = integer(), language = "EN", words = tibble::tibble()){
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  # Init UI variables
  people_picker <- ""
  toggles <- ""
  dropdowns <- ""
  ace_editor <- ""
  
  # Get options with page ID
  page_options <- get_page_options(id = id)
  
  ##########################################
  # Option = Users allowed to read         #
  ##########################################
  
  if("users_allowed_read" %in% page_options){
    # List of users in the database, with status as secondaryText
    picker_options <-
      r$users %>%
      dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
      dplyr::transmute(
        key = id, 
        imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
        text = paste0(firstname, " ", lastname), 
        secondaryText = user_status)
    
    # If this is study options, we have to show only users who have access to the parent datamart
    if(id == "settings_studies"){
      datamart_id <- r$studies %>% dplyr::filter(id == link_id) %>% dplyr::pull(datamart_id)
      users_allowed_datamart <- 
        r$options %>% 
        dplyr::filter(category == "datamart", link_id == datamart_id, name == "user_allowed_read") %>%
        dplyr::pull(value_num)
      users_allowed_read_group <- 
        r$options %>% 
        dplyr::filter(category == "datamart", link_id == datamart_id, name == "users_allowed_read_group") %>%
        dplyr::pull(value)
      
      # Don't filter if everybody has access to the datamart
      if (users_allowed_read_group != "everybody") picker_options <- picker_options %>% dplyr::filter(key %in% users_allowed_datamart)
    }
    
    # Users already allowed
    value <-
      picker_options %>%
      dplyr::mutate(n = 1:dplyr::n()) %>%
      dplyr::inner_join(
        options %>%
          dplyr::filter(name == "user_allowed_read") %>%
          dplyr::select(key = value_num),
        by = "key"
      ) %>%
      dplyr::pull(key)
    
    # Users allowed read group
    value_group <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value)
    
    people_picker <- div(
      div(class = "input_title", paste0(translate(language, paste0(get_singular(id), "_users_allowed_read"), words), " :")),
      shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), value = value_group, options = list(
        list(key = "everybody", text = translate(language, "everybody", words)),
        list(key = "people_picker", text = translate(language, "people_picker", words))
      ), className = "inline_choicegroup"),
      conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
        make_people_picker(
          language = language, ns = ns, id = "users_allowed_read", label = "blank",
          options = picker_options, value = value, width = "100%", style = "padding-bottom:10px;", words = words)
      ), br())
  }
  
  ##########################################
  # Option = show only aggregated data     #
  ##########################################
  
  if ("show_only_aggregated_data" %in% page_options){
    value <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num)
    toggles <- tagList(br(), 
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10),
        make_toggle(language = language, ns = ns, label = "show_only_aggregated_data", value = value, inline = TRUE, words = words)
      )
    )
  }
  
  ##########################################
  # Option = markdown description     #
  ##########################################
  
  if ("markdown_description" %in% page_options){
    value <- options %>% dplyr::filter(name == "markdown_description") %>% dplyr::pull(value)
    ace_editor <- div(shinyAce::aceEditor(ns("markdown_description"), value, mode = "markdown", 
      autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
    ), style = "width: 100%;")
  }
  
  ##########################################
  # Final UI code                          #
  ##########################################
  
  div(id = ns("options_card"),
    make_card(tagList(translate(language, title, words), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        toggles, people_picker,
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), dropdowns),
        ace_editor,
        shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), translate(language, "save", words))
      )
    )
  )
}


##########################################
# Edit code card                         #
##########################################

#' Render UI of edit_code card
#' 
#' @param ns Shiny namespace
#' @param r r Shiny reactive value to communicate between modules
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param title Title of the card (character)
#' @param code Code to show in ShinyAce editor (list)
#' @param link_id ID allows to link with code table (integer)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' render_settings_code_card(ns = NS("settings_datamarts"), r = r, id = "settings_datamarts", title = "edit_datamart_code",
#'   code = "Enter your code here", link_id = 3, language = "EN")
#' }

render_settings_code_card <- function(ns = shiny::NS(), r = shiny::reactiveValues(), id = character(), title = character(), code = list(), 
  link_id = integer(), language = "EN", words = tibble::tibble()){
  
  choice_ui_server <- tagList()
  choice_data <- tagList()
  
  # Default output : text output
  # For plugins, this output is UI, to test plugin's code
  output_div <- div(shiny::verbatimTextOutput(ns("code_result")), 
      style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
  
  # Default ace editor
  div(shinyAce::aceEditor(ns("ace_edit_code"), code$server, mode = "r", 
    autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;") -> ace_editor
  
  # For plugin page : 
  # - choose between UI code or Server code
  # - choose a datamart, a study & a subset (for aggregated data plugin) & a patient / a stay (for patient-lvl data)
  # - two ace editors, one for UI & one for server
  
  if (id == "settings_plugins"){
    
    # Get module_type_id of current plugin
    module_type_id <- r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(module_type_id)
    
    # Colours choices
    colorCells <- list(
      list(id = "#EF3B2C", color = "#EF3B2C"),
      list(id = "#CB181D", color = "#CB181D"),
      list(id = "#7BCCC4", color = "#7BCCC4"),
      list(id = "#2B8CBE", color = "#2B8CBE"),
      list(id = "#5AAE61", color = "#5AAE61"),
      list(id = "#FFD92F", color = "#FFD92F"),
      list(id = "#000000", color = "#000000"))
    
    # Dropdowns for choice of datamart etc
    # Depending if module_type is patient_lvl_data or aggregated_data
    if (module_type_id == 1){
      tagList(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_dropdown(language = language, ns = ns, label = "datamart", width = "300px",
            options = convert_tibble_to_list(data = r$datamarts, key_col = "id", text_col = "name"), words = words),
          make_dropdown(language = language, ns = ns, label = "study", width = "300px", words = words),
          make_dropdown(language = language, ns = ns, label = "patient", width = "300px", words = words),
          make_dropdown(language = language, ns = ns, label = "stay", width = "300px", words = words)),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_dropdown(language = language, ns = ns, label = "thesaurus", width = "300px", words = words),
          div(strong(translate(language, "show_only_used_items_patient", words), style = "display:block; padding-bottom:12px;"),
            shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;")),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_combobox(language = language, ns = ns, label = "thesaurus_items", multiSelect = TRUE, allowFreeform = TRUE, width = "300px", words = words),
          div(
            div(class = "input_title", translate(language, "item_colour", words)),
            div(shiny.fluent::SwatchColorPicker.shinyInput(ns("colour"), value = "#000000", colorCells = colorCells, columnCount = length(colorCells))),
            style = "margin-top:5px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("add_thesaurus_item"), translate(language, "add", words)), style = "margin-top:38px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("remove_thesaurus_item"), translate(language, "remove", words)), style = "margin-top:38px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), translate(language, "reset", words)), style = "margin-top:38px;")), br(),
        uiOutput(ns("thesaurus_selected_items"))) -> choice_data
    }
    if (module_type_id == 2){
      tagList(shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 50),
        make_dropdown(language = language, ns = ns, label = "datamart", width = "300px",
          options = convert_tibble_to_list(data = r$datamarts, key_col = "id", text_col = "name"), words = words),
        make_dropdown(language = language, ns = ns, label = "study", width = "300px", words = words),
        make_dropdown(language = language, ns = ns, label = "subset", width = "300px", words = words)
        )) -> choice_data
    }
    
    # Toggle for choice of UI or server code
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
        list(key = "ui", text = translate(language, "ui", words)),
        list(key = "server", text = translate(language, "server", words))
      ), className = "inline_choicegroup"),
      div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
      div(translate(language, "hide_editor", words), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
    ) -> choice_ui_server
    
    # Ace editors
    tagList(
      conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
        div(shinyAce::aceEditor(ns("ace_edit_code_ui"), code$ui, mode = "r", 
          autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
      conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
        div(shinyAce::aceEditor(ns("ace_edit_code_server"), code$server, mode = "r", 
          autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"))
    ) -> ace_editor
    
    # UI output to render UI code of the plugin and text output to render server error messages
    output_div <- tagList(
      shiny::uiOutput(ns("code_result_ui")), br(),
      div(verbatimTextOutput(ns("code_result_server")), 
          style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"))
  }
  
  div(id = ns("edit_code_card"),
    # Show current ID in the title
    make_card(tagList(translate(language, title, words), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        choice_data, br(),
        choice_ui_server,
        ace_editor,
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), translate(language, "save", words)), " ",
        shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), translate(language, "execute_code", words)), 
        br(), br(),
        output_div
      )
    )
  )
}