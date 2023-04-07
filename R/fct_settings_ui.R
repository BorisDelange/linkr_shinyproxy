#' Render UI of settings default elements
#' 
#' @description Set default UI elements on top of the page : message_bar outputs, react output to confirm delete of a table element
#' 
#' @param ns Shiny namespace
#' @return Shiny UI elements / HTML code
#' @examples
#' render_settings_default_elements(ns = NS("settings_dataset"))

render_settings_default_elements <- function(ns = character()){
  
  message_bars <- tagList()
  for (i in 1:20) message_bars <- tagList(message_bars, shiny::uiOutput(ns(paste0("message_bar", i))))
  
  div(
    div(class = "message_bars", message_bars), 
    shiny.fluent::reactOutput(ns("delete_confirm"))
  )
}

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
#' render_settings_creation_card(language = "EN", ns = NS("settings_dataset"), title = "create_dataset",
#' textfields = c("name", "description"), dropdowns = "data_source")

render_settings_creation_card <- function(i18n = character(), ns = character(), id = character(), title = character(), 
  textfields = character(), textfields_width = "200px", dropdowns = character(), dropdowns_width = "200px"){
  
  div(id = ns("creation_card"),
    make_card(
      title = i18n$t(title),
      content = div(
        shiny.fluent::Stack(
          # Horizontal alignment, with gap of 50 px between elements
          horizontal = TRUE, tokens = list(childrenGap = 50),
          # For each textfield, use make_textfield function
          lapply(textfields, function(label){
            if (label == "password") make_textfield(i18n = i18n, ns = ns, label = label, id = label, width = textfields_width, type = "password", canRevealPassword = TRUE)
            else make_textfield(i18n = i18n, ns = ns, label = label, id = label, width = textfields_width)
          })
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(dropdowns, function(label){
            # Allow multiSelect for thesaurus, column data source
            multiSelect <- FALSE
            if (id == "settings_thesaurus") multiSelect <- TRUE
            make_dropdown(i18n = i18n, ns = ns, label = label, id = label, multiSelect = multiSelect, width = dropdowns_width)
          })
        ), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add"))
      )
    )
  )
}

#' Render UI of settings datatable card
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param div_id ID of the div, to show or hide with toggles, default = "datatable_card" (character)
#' @param output_id ID of div & DTOutput, allows to have multiple management_datatable in one module, default = "management_datatable" (character)
#' @param title Title used to create the card, it will be translated with translate function (character)
#' @examples 
#' \dontrun{
#' render_settings_datatable_card(language = "EN", ns = ns, output_id = "management_datatable", title = "datasets_management")
#' }
render_settings_datatable_card <- function(i18n = character(), ns = character(), div_id = "datatable_card",
  output_id = "management_datatable", title = character(), inputs = character(), dropdown_multiselect = FALSE){
  
  inputs_div <- tagList()
  
  if (length(inputs) > 0){
    
    for (name in names(inputs)){
      
      input_type <- inputs[[name]]
      if (input_type == "textfield") inputs_div <- tagList(inputs_div, make_textfield(i18n = i18n, ns = ns, label = name, id = name, width = "300px"))
      if (input_type == "dropdown") inputs_div <- tagList(inputs_div, make_dropdown(i18n = i18n, ns = ns, label = name, id = name, width = "300px", multiSelect = dropdown_multiselect))
    }
    
    inputs_div <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
      inputs_div,
      div(shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add")), style = "margin-top:38px;")
    )
  }
  
  div(id = ns(div_id),
    make_card(i18n$t(title),
      div(
        inputs_div,
        div(DT::DTOutput(ns(output_id)), style = "z-index:2"),
        div(
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), i18n$t("save")),
            shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
          ),
        style = "position:relative; z-index:1; margin-top:-30px; width:500px;")
      )
    ), br()
  )
}

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
#' render_settings_code_card(ns = NS("settings_datasets"), r = r, id = "settings_datasets", title = "edit_dataset_code",
#'   code = "Enter your code here", link_id = 3, language = "EN")
#' }
render_settings_code_card <- function(ns = character(), r = shiny::reactiveValues(), id = character(), title = character(), code = list(), 
  link_id = integer(), i18n = character()){
  
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
  # - choose a dataset, a study & a subset (for aggregated data plugin) & a patient / a stay (for patient-lvl data)
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
    
    # Dropdowns for choice of dataset etc
    # Depending if module_type is patient_lvl_data or aggregated_data
    if (module_type_id == 1){
      tagList(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_dropdown(i18 = i18, ns = ns, label = "dataset", width = "300px",
            options = convert_tibble_to_list(data = r$datasets, key_col = "id", text_col = "name")),
          make_dropdown(i18 = i18, ns = ns, label = "study", width = "300px"),
          make_dropdown(i18 = i18, ns = ns, label = "patient", width = "300px"),
          make_dropdown(i18 = i18, ns = ns, label = "stay", width = "300px")),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_dropdown(i18 = i18, ns = ns, label = "thesaurus", width = "300px"),
          div(strong(i18$t("show_only_used_items_patient"), style = "display:block; padding-bottom:12px;"),
            shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;")),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
          make_combobox(i18 = i18, ns = ns, label = "thesaurus_items", multiSelect = TRUE, allowFreeform = TRUE, width = "300px"),
          div(
            div(class = "input_title", i18$t("item_colour")),
            div(shiny.fluent::SwatchColorPicker.shinyInput(ns("colour"), value = "#000000", colorCells = colorCells, columnCount = length(colorCells))),
            style = "margin-top:5px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("add_thesaurus_item"), i18$t("add")), style = "margin-top:38px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("remove_thesaurus_item"), i18$t("remove")), style = "margin-top:38px;"),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), i18$t("reset")), style = "margin-top:38px;")), br(),
        uiOutput(ns("thesaurus_selected_items"))) -> choice_data
    }
    if (module_type_id == 2){
      tagList(shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 50),
        make_dropdown(i18 = i18, ns = ns, label = "dataset", width = "300px",
          options = convert_tibble_to_list(data = r$datasets, key_col = "id", text_col = "name")),
        make_dropdown(i18 = i18, ns = ns, label = "study", width = "300px"),
        make_dropdown(i18 = i18, ns = ns, label = "subset", width = "300px")
      )) -> choice_data
    }
    
    # Toggle for choice of UI or server code
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
        list(key = "ui", text = i18$t("ui")),
        list(key = "server", text = i18$t("server"))
      ), className = "inline_choicegroup"),
      div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
      div(i18$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
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
    make_card(tagList(i18$t(title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        choice_data, br(),
        choice_ui_server,
        ace_editor,
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), i18$t("save")), " ",
        shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18$t("execute_code")), 
        br(), br(),
        output_div
      )
    )
  )
}

#' Forbidden card
#' 
forbidden_card <- function(ns = character(), name = character(), i18n = character()){
  shinyjs::hidden(
    div(
      id = ns(paste0(name, "_forbidden")),
      make_card("",
        div(shiny.fluent::MessageBar(i18n$t("unauthorized_access_page"), messageBarType = 5), style = "margin-top:10px;")
      )
    )
  )
}
