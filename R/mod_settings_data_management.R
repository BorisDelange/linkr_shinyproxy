#' settings_data_management UI Function
#'
#' @description Shiny module of settings / data management
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- div()
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datamarts", "data_source",
    "settings_thesaurus", "data_source")
  
  cards <- c("datatable_card", "edit_code_card", "options_card", "sub_datatable_card")#, "categories_card", "conversions_card", "mapping_card")
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  # --- --- --- --- --- --
  # Data sources card ----
  # --- --- --- --- --- --
  
  if (id == "settings_data_sources"){
      div(class = "main",
        render_settings_default_elements(ns = ns),
        shiny.fluent::reactOutput(ns("help_panel")),
        shiny.fluent::reactOutput(ns("help_modal")),
        shiny.fluent::Breadcrumb(items = list(
          list(key = "data_sources", text = i18n$t("data_sources"))
        ), maxDisplayedItems = 3),
        
        # --- --- --- --- -
        ## Pivot items ----
        # --- --- --- --- -
        
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("data_sources_management"))
        ),
        
        forbidden_cards,
        
        # --- --- --- --- --- -
        # Management card ----
        # --- --- --- --- --- -
        
        render_settings_datatable_card(i18n = i18n, ns = ns, div_id = "datatable_card", title = "data_sources_management", inputs = c("name" = "textfield"))
      ) -> result
    }
  
  # --- --- --- --- ---
  # Datamarts card ----
  # --- --- --- --- ---
  
  if (id == "settings_datamarts"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::reactOutput(ns("help_panel")),
      shiny.fluent::reactOutput(ns("help_modal")),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "datamarts", text = i18n$t("datamarts"))
      ), maxDisplayedItems = 3),
      
      # --- --- --- --- -
      ## Pivot items ----
      # --- --- --- --- -
      
      shiny.fluent::Pivot(
        id = ns("datamarts_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("datamarts_management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_datamart_code")),
        shiny.fluent::PivotItem(id = "options_card", itemKey = "options_card", headerText = i18n$t("datamart_options"))
      ),
      
      forbidden_cards,
      
      # --- --- --- --- --- -
      # Management card ----
      # --- --- --- --- --- -
      
      render_settings_datatable_card(i18n = i18n, ns = ns, div_id = "datatable_card", title = "datamarts_management", 
        inputs = c("name" = "textfield", "data_source" = "dropdown")),
      
      # --- --- --- --- -- -
      # Edit code card ----
      # --- --- --- --- -- -
      
      div(id = ns("edit_code_card"), 
        make_card(i18n$t("edit_datamart_code"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_combobox(i18n = i18n, ns = ns, label = "datamart", id = "code_chosen_datamart_thesaurus",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              div(style = "width:20px;"),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:45px;"),
              div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:45px; margin-right:30px;"), 
            ),
            conditionalPanel(condition = "input.hide_editor == true", ns = ns, br()),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              div(shinyAce::aceEditor(
                ns("ace_edit_code"), "", mode = "r", 
                code_hotkeys = list("r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"))
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000),
                style = "width: 100%;"
              )
            ),
            shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(), br(),
            div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"), br(),
            DT::DTOutput(ns("code_datatable"))
          )
        ), br()
      ),
      
      # --- --- --- --- --- ---
      # Edit options card ----
      # --- --- --- --- --- ---
      
      div(id = ns("options_card"),
        make_card(i18n$t("datamart_options"),
          div(
            make_combobox(i18n = i18n, ns = ns, label = "datamart", id = "options_chosen_datamart_thesaurus",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(), br(),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, label = "show_only_aggregated_data", inline = TRUE)
            ), br(),
            div(
              div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                list(key = "everybody", text = i18n$t("everybody")),
                list(key = "people_picker", text = i18n$t("choose_users"))
              ), className = "inline_choicegroup"),
              conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                uiOutput(ns("users_allowed_read_div"))
              )
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save"))
          )
        ), br()
      )
    ) -> result
  }
  
  # --- --- --- --- ---
  # Thesaurus card ----
  # --- --- --- --- ---
  
  if (id == "settings_thesaurus"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::reactOutput(ns("help_panel")),
      shiny.fluent::reactOutput(ns("help_modal")),
      shiny.fluent::reactOutput(ns("thesaurus_reload_cache_confirm")),
      shiny.fluent::reactOutput(ns("thesaurus_items_delete_confirm")),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "thesaurus", text = i18n$t("thesaurus"))
      ), maxDisplayedItems = 3),
      
      # --- --- --- --- -
      ## Pivot items ----
      # --- --- --- --- -
      
      shiny.fluent::Pivot(
        id = ns("thesaurus_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("thesaurus_management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_thesaurus_code")),
        shiny.fluent::PivotItem(id = "sub_datatable_card", itemKey = "sub_datatable_card", headerText = i18n$t("thesaurus_items"))
      ),
      
      
      forbidden_cards,
      
      # --- --- --- --- --- -
      # Management card ----
      # --- --- --- --- --- -
      
      div(id = ns("datatable_card"),
        make_card(i18n$t("vocabularies_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "vocabulary_id_textfield", id = "vocabulary_id", width = "300px"),
              make_textfield(i18n = i18n, ns = ns, label = "vocabulary_name_textfield", id = "vocabulary_name", width = "300px"),
              make_dropdown(i18n = i18n, ns = ns, label = "data_source", id = "data_source", width = "300px", multiSelect = TRUE),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add")), style = "margin-top:38px;")),
            div(DT::DTOutput(ns("management_datatable")), style = "z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:1; margin-top:-30px; width:500px;")
          )
        ), br()
      ),
      
      # --- --- --- --- -- -
      # Edit code card ----
      # --- --- --- --- -- -
      
      div(id = ns("edit_code_card"), 
        make_card(i18n$t("edit_vocabulary_code"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "code_chosen_datamart_thesaurus",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              div(style = "width:20px;"),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:45px;"),
              div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:45px; margin-right:30px;"), 
            ),
            conditionalPanel(condition = "input.hide_editor == true", ns = ns, br()),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              div(shinyAce::aceEditor(
                ns("ace_edit_code"), "", mode = "r", 
                code_hotkeys = list(
                  "r", list(
                    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), 
              style = "width: 100%;")
            ),
            shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(), br(),
            div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      ),
      
      # --- --- --- --- -- -
      # All items card ----
      # --- --- --- --- -- -
      
      div(id = ns("sub_datatable_card"),
        make_card(i18n$t("thesaurus_items"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 50),
              make_combobox(i18n = i18n, ns = ns, label = "thesaurus_single", id = "items_chosen_thesaurus",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              make_dropdown(i18n = i18n, ns = ns, label = "datamart", id = "thesaurus_datamart", width = "300px"),
              conditionalPanel(condition = "input.datamart !== ''", ns = ns,
                div(strong(i18n$t("show_only_used_items"), style = "display:block; padding-bottom:12px;"),
                  shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
            ),
            DT::DTOutput(ns("sub_datatable")), br(),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("sub_datatable_save"), i18n$t("save")),
              shiny.fluent::DefaultButton.shinyInput(ns("thesaurus_items_delete_selection"), i18n$t("delete_selection")),
              shiny.fluent::DefaultButton.shinyInput(ns("reload_thesaurus_cache"), i18n$t("reload_cache"))
            )
          )
        ), br()
      )
    ) -> result
  }
  result
}
    
#' settings_data_management Server Functions
#'
#' @param id ID of the module (character)
#' @param r Shiny reactive value
#' @param language Language used (character)
#' @noRd 

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(),
  d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = character(), 
  language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_settings_data_management_server - start"))
    
    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "settings_data_sources", "",
      "settings_datamarts", "data_source",
      "settings_thesaurus", "data_source")
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    if (table %in% c("datamarts", "thesaurus")){
      observeEvent(r[[table]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - updateComboBox"))
        
        options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        
        if (table == "datamarts") shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_datamart_thesaurus", options = options)
        if (table == "thesaurus") shiny.fluent::updateComboBox.shinyInput(session, "items_chosen_thesaurus", options = options)
        shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options)
      })
    }
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    # Toggles IDs
    cards <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "sub_datatable_card", "categories_card", "conversions_card", "mapping_card")
    sapply(cards, shinyjs::hide)
    
    show_hide_cards(r = r, input = input, session = session, table = table, id = id, cards = cards)
    
    # Show first card
    
    if (paste0(table, "_datatable_card") %in% r$user_accesses) shinyjs::show("datatable_card")
    else shinyjs::show("datatable_card_forbidden")
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_settings_data_management_", table, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_settings_data_management_", table, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_settings_data_management_", table, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_settings_data_management_", table, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_settings_data_management_", table, "_open_modal")]] <- FALSE
      r[[paste0("help_settings_data_management_", table, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_data_management_", table, "_page_", i)]] <- Sys.time())
    })
    
    help_settings_data_management(output = output, r = r, id = id, prefix = table, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --
    # Add a new element ----
    # --- --- --- --- --- --
    
    # Update dropdowns with reactive data
    
    uploaded_dropdowns <- dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist()
    
    sapply(uploaded_dropdowns, 
      function(data_var){
        
        # Create only needed observers for current page
        if (data_var != ""){
          data_var <- get_plural(data_var)
          observeEvent(r[[data_var]], {
            
            if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table))
            
            # Convert options to list
            options <- convert_tibble_to_list(data = r[[data_var]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        }
      })
    
    # When add button is clicked
    observeEvent(input$add, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$add"))
      
      # Create a list with new data
      # If page = thesaurus, data_source is character, not integer (multiple choices)
      new_data <- list()
      if (id == "settings_thesaurus") new_data_var <- c("vocabulary_id" = "char", "vocabulary_name" = "char", "data_source" = "char")
      else new_data_var <- c("name" = "char", "description" = "char", "data_source" = "int", "datamart" = "int", 
        "study" = "int", "patient_lvl_module_family" = "int", "aggregated_module_family" = "int")
      
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
      })
      
      # Convert data_source to string, for page thesaurus
      if (id == "settings_thesaurus"){
        if (length(new_data$data_source) == 1) new_data$data_source <- coalesce2(type = "char", x = input$data_source)
        else new_data$data_source <- toString(as.integer(new_data$data_source))
      }
      
      if (id == "settings_thesaurus"){
        add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
          data = new_data, table = "vocabulary", required_textfields = "vocabulary_id", req_unique_values = "vocabulary_id",
          dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
      }
      else add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = substr(id, nchar("settings_") + 1, nchar(id)), 
        required_textfields = "name", req_unique_values = "name",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
      
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$add"))
    })
    
    # --- --- --- --- --- ---
    # Generate datatable ----
    # --- --- --- --- --- ---
  
    table <- paste0(substr(id, nchar("settings_") + 1, nchar(id)))
      
    dropdowns_datatable <- switch(id,
      "settings_data_sources" = "",
      "settings_datamarts" = "",
      "settings_thesaurus" = c("data_source_id" = "data_sources"))
    
    # Dropdowns with multiSelect
    dropdowns_multiselect <- ""
    if (table == "thesaurus") dropdowns_multiselect <- "data_source_id"
    
    # Action buttons for each module / page
    action_buttons = switch(table,
      "data_sources" = "delete",
      "datamarts" = c("delete", "edit_code", "options"),
      "thesaurus" = c("delete", "edit_code", "sub_datatable")
    )
    
    # Editable cols
    editable_cols <- c("name", "description")
    
    # Sortable cols
    if (id == "settings_thesaurus") sortable_cols <- c("id", "name", "description", "creator_id", "datetime")
    if (id != "settings_thesaurus") sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    
    # Column widths
    column_widths <- c("id" = "80px", "datetime" = "130px", "creator_id" = "200px", "action" = "80px")
    
    # Centered columns
    centered_cols <- c("id", "creator", "datetime", "action")
    
    # Searchable_cols
    if (id != "settings_thesaurus") searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    if (id == "settings_thesaurus") searchable_cols <- c("name", "description", "creator_id")
    
    # Factorize_cols
    factorize_cols <- switch(id,
      "settings_data_sources" = "creator_id",
      "settings_datamarts" = c("data_source_id", "creator_id"),
      "settings_thesaurus" = "creator_id")
    
    hidden_cols <- c("id", "description", "deleted", "modified")
    
    # Reload datatable
    
    observeEvent(r[[table]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - reload datatable"))
      
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(r[[paste0(table, "_temp")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$", table, "_temp"))
      
      if (nrow(r[[paste0(table, "_temp")]]) == 0){
        
        if (table == "data_sources") data <- tibble::tibble(id = integer(), name = character(), description = character(),
          creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
        if (table == "datamarts") data <- tibble::tibble(id = integer(), name = character(), description = character(),
          data_source_id = character(), creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
        if (table == "thesaurus") data <- tibble::tibble(id = integer(), name = character(), description = character(),
          data_source_id = character(), creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      
      if (nrow(r[[paste0(table, "_temp")]]) > 0){

        r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = table, dropdowns = dropdowns_datatable, dropdowns_multiselect = dropdowns_multiselect, factorize_cols = factorize_cols,
          action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]])
        
        # Replace empty data_source_id by "deleted data source"
        if (table == "datamarts"){
          r[[paste0(table, "_datatable_temp")]] <- r[[paste0(table, "_datatable_temp")]] %>%
            dplyr::mutate_at("data_source_id", as.character) %>%
            dplyr::mutate(data_source_id = dplyr::case_when(
              data_source_id == "" ~ i18n$t("deleted_data_source"), TRUE ~ data_source_id
            ))
        }
        data <- r[[paste0(table, "_datatable_temp")]]
      }
      
      if (length(r[[paste0(table, "_datatable_proxy")]]) == 0){
        
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
          output_name = "management_datatable", col_names =  get_col_names(table_name = table, i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")

        r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r[[paste0(table, "_datatable_proxy")]]) > 0){
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], data, resetPaging = FALSE, rownames = FALSE)
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$", table, "_temp"))
    })
    
    # --- --- --- --- --- --- --- --
    # Save updates in datatable ----
    # --- --- --- --- --- --- --- --
    
      # Each time a row is updated, modify temp variable
      # Do that for main datatable (management_datatable) & sub_datatable
      observeEvent(input$management_datatable_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_datatable_cell_edit"))
        
        edit_info <- input$management_datatable_cell_edit
        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })
  
      observeEvent(input$sub_datatable_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$sub_datatable_cell_edit"))
        
        edit_info <- input$sub_datatable_cell_edit
        r$thesaurus_items_temp <- DT::editData(r$thesaurus_items_temp, edit_info, rownames = FALSE)
        r$thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      })
    
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r[[table]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - update dropdowns"))
        
        update_settings_datatable(input = input, module_id = id, r = r, ns = ns, table = table, 
          dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), i18n = i18n)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$", table, " - update dropdowns"))
      })
    
      # When save button is clicked
      # Do that for main datatable (management_datatable) & sub_datatable
      observeEvent(input$management_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_save"))
        
        req(nrow(r[[paste0(table, "_temp")]]) > 0)
        
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, i18n = i18n, duplicates_allowed = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$management_save"))
      })
      
      observeEvent(input$sub_datatable_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$sub_datatable_save"))
        
        req(input$items_chosen_thesaurus)
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items", 
          r_table = "thesaurus_items", duplicates_allowed = TRUE, i18n = i18n)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$sub_datatable_save"))
      })
      
      # --- --- --- --- --- --- --- --
      # Delete a row in datatable ----
      # --- --- --- --- --- --- --- --
      
      settings_delete_prefix <- table
      settings_dialog_title <- paste0(table, "_delete")
      settings_dialog_subtext <- paste0(table, "_delete_subtext")
      settings_react_variable <- "delete_confirm"
      settings_id_var_sql <- "id"
      settings_id_var_r <- paste0("delete_", get_plural(table))
      settings_delete_message <- paste0(table, "_deleted")
      settings_reload_variable <- paste0("reload_" , get_plural(table))
      settings_information_variable <- paste0(table, "_deleted")
      settings_delete_variable <- paste0(table, "_open_dialog")
      
      delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = settings_delete_prefix, dialog_title = settings_dialog_title, dialog_subtext = settings_dialog_subtext,
        react_variable = settings_react_variable, table = table, id_var_sql = settings_id_var_sql, id_var_r = settings_id_var_r,
        delete_message = settings_delete_message, translation = TRUE, reload_variable = settings_reload_variable,
        information_variable = settings_information_variable)
      
      # Delete one row (with icon on DT)
      
      observeEvent(input$deleted_pressed, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$deleted_pressed"))
        
        r[[paste0("delete_", get_plural(table))]] <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
        r[[settings_delete_variable]] <- TRUE
        
        # Reload datatable (to unselect rows)
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
      })
      
      # Delete multiple rows (with "Delete selection" button)
      
      observeEvent(input$delete_selection, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$delete_selection"))
        
        req(length(input[["management_datatable_rows_selected"]]) > 0)
        
        r[[paste0("delete_", get_plural(table))]] <- r[[paste0(table, "_temp")]][input[["management_datatable_rows_selected"]], ] %>% dplyr::pull(id)
        r[[settings_delete_variable]] <- TRUE
      })
      
      # --- --- --- --- --- --- --- --- -- -
      # Edit options by selecting a row ----
      # --- --- --- --- --- --- --- --- -- -
        
      if (table == "datamarts"){
          
        observeEvent(input$options, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options"))
          
          # Get link_id variable, to update options div
          link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
          options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_datamart_thesaurus", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to options_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(table, '_pivot')} button[name=\"{i18n$t(paste0(get_singular(table), '_options'))}\"]').click();"))
        })
        
        observeEvent(input$options_chosen_datamart_thesaurus, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options_chosen_datamart_thesaurus"))
          
          if (length(input$options_chosen_datamart_thesaurus) > 1) link_id <- input$options_chosen_datamart_thesaurus$key
          else link_id <- input$options_chosen_datamart_thesaurus
          if (length(input$code_chosen_datamart_thesaurus) > 0){
            if (length(input$code_chosen_datamart_thesaurus) > 1) code_link_id <- input$code_chosen_datamart_thesaurus$key
            else code_link_id <- input$code_chosen_datamart_thesaurus
          }
          else code_link_id <- 0L
          
          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options, value = value)
          }
          
          category <- get_singular(word = id)
          
          options <- r$options %>% dplyr::filter(category == "datamart", link_id == !!link_id)
          
          picker_options <-
            r$users %>%
            dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
            dplyr::transmute(
              key = id, 
              imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
              text = paste0(firstname, " ", lastname), 
              secondaryText = user_status)
          
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
          
          selected_items <- picker_options %>% dplyr::filter(key %in% value)
          
          shiny.fluent::updateToggle.shinyInput(session, "show_only_aggregated_data",
            value = options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num) %>% as.logical)
          shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
            value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
          output$users_allowed_read_div <- renderUI({
            make_people_picker(
              i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
              width = "100%", style = "padding-bottom:10px;")
          })
          
        })
        
        observeEvent(input$options_save, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options_save"))
          
          req(input$options_chosen_datamart_thesaurus)
          
          if (length(input$options_chosen_datamart_thesaurus) > 1) link_id <- input$options_chosen_datamart_thesaurus$key
          else link_id <- input$options_chosen_datamart_thesaurus
          
          category <- get_singular(id)
          
          data <- list()
          data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
          data$users_allowed_read <- input$users_allowed_read
          data$users_allowed_read_group <- input$users_allowed_read_group
  
          save_settings_options(output = output, r = r, id = id, category = category, code_id_input = paste0("options_", link_id), 
            i18n = i18n, data = data, page_options = c("show_only_aggregated_data", "users_allowed_read"))
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$options_save"))
        })
      }
      
      # --- --- --- --- --- --- --- -- --
      # Edit code by selecting a row ----
      # --- --- --- --- --- --- --- -- --
      
      if (table %in% c("datamarts", "thesaurus")){
        
        # Button "Edit code" is clicked on the datatable
        observeEvent(input$edit_code, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$edit_code"))

          # Get link_id variable, to update code editor
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options, value = value)
          if (table == "datamarts") shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_datamart_thesaurus", options = options, value = value)
          if (table == "thesaurus") shiny.fluent::updateComboBox.shinyInput(session, "items_chosen_thesaurus", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(table, '_pivot')} button[name=\"{i18n$t(paste0('edit_', get_singular(table), '_code'))}\"]').click();"))
          
        })
        
        observeEvent(input$code_chosen_datamart_thesaurus, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$code_chosen_datamart_thesaurus"))

          if (length(input$code_chosen_datamart_thesaurus) > 1) link_id <- input$code_chosen_datamart_thesaurus$key
          else link_id <- input$code_chosen_datamart_thesaurus
          
          if (table == "datamarts"){
            if (length(input$options_chosen_datamart_thesaurus) > 0){
              if (length(input$options_chosen_datamart_thesaurus) > 1) options_link_id <- input$options_chosen_datamart_thesaurus$key
              else options_link_id <- input$options_chosen_datamart_thesaurus
            }
            else options_link_id <- 0L

            if (link_id != options_link_id){
              options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_datamart_thesaurus", options = options, value = value)
            }
          }
          
          if (table == "thesaurus"){
            if (length(input$items_chosen_thesaurus) > 0){
              if (length(input$items_chosen_thesaurus) > 1) items_link_id <- input$items_chosen_thesaurus$key
              else items_link_id <- input$items_chosen_thesaurus
            }
            else items_link_id <- 0L
            
            if (link_id != items_link_id){
              options <- convert_tibble_to_list(r$thesaurus %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "items_chosen_thesaurus", options = options, value = value)
            }
          }

          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$thesaurus_id <- NA_integer_

          if (id == "settings_datamarts") r$datamart_id <- link_id
          if (id == "settings_thesaurus") r$thesaurus_id <- link_id

          category <- get_singular(id)

          # Get code from database
          code <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code)
          shinyAce::updateAceEditor(session, "ace_edit_code", value = code)

          # Reset code_result textOutput
          output$datetime_code_execution <- renderText("")
          output$code_result <- renderText("")
        })
        
        # When save button is clicked, or CTRL+C or CMD+C is pushed
        observeEvent(input$edit_code_save, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$edit_code_save"))
          r[[paste0(id, "_save")]] <- Sys.time()
        })
        observeEvent(input$ace_edit_code_save, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_save"))
          r[[paste0(id, "_save")]] <- Sys.time()
        })
        observeEvent(r[[paste0(id, "_save")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..save"))
          
          req(input$code_chosen_datamart_thesaurus)
          
          if (length(input$code_chosen_datamart_thesaurus) > 1) link_id <- input$code_chosen_datamart_thesaurus$key
          else link_id <- input$code_chosen_datamart_thesaurus
          
          save_settings_code(output = output, r = r, id = id, category = get_singular(id),
            code_id_input = paste0("edit_code_", link_id), edited_code = input$ace_edit_code, i18n = i18n)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..save"))
        })
        
        # When Execute code button is clicked
        
        observeEvent(input$execute_code, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$execute_code"))
          r[[paste0(id, "_code")]] <- input$ace_edit_code
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
        
        observeEvent(input$ace_edit_code_run_selection, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_run_selection"))
          if(!shinyAce::is.empty(input$ace_edit_code_run_selection$selection)) r[[paste0(id, "_code")]] <- input$ace_edit_code_run_selection$selection
          else r[[paste0(id, "_code")]] <- input$ace_edit_code_run_selection$line
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
        
        observeEvent(input$ace_edit_code_run_all, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_run_all"))
          r[[paste0(id, "_code")]] <- input$ace_edit_code
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
      
        observeEvent(r[[paste0(id, "_code_trigger")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..code_trigger"))
          
          # Reset d variable
          vars <- c("patients", "stays", "labs_vitals", "orders", "text", "diagnoses")
          for (var in vars) d[[var]] <- tibble::tibble()
          
          edited_code <- r[[paste0(id, "_code")]] %>% stringr::str_replace_all("\r", "\n")
          
          output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
          output$code_result <- renderText(
            execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
              i18n = i18n, r = r, d = d, edited_code = edited_code))
          
          r[[paste0(id, "_code_datatable_trigger")]] <- Sys.time()
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..code"))
        })
        
        observeEvent(r[[paste0(id, "_code_datatable_trigger")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..code_datatable_trigger"))
          
          data <- tibble::tibble(name = character(), rows = integer())
          
          vars <- c("patients", "stays", "labs_vitals", "orders", "text", "diagnoses")
          
          for (var in vars){
            data <- data %>% dplyr::bind_rows(
              tibble::tibble(name = var, rows = nrow(d[[var]]))
            )
          }
          
          render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
            output_name = "code_datatable", col_names = c(i18n$t("table_name"), i18n$t("rows")),
            column_widths = c("rows" = "150px"), datatable_dom = "")
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..code_datatable_trigger"))
        })
      }
      
      # --- --- --- --- --- --- --- --- --- --- --- --
      # Generate sub datatable with action button ----
      # --- --- --- --- --- --- --- --- --- --- --- --
        
      if (table == "thesaurus"){

        # Sub datatable is a datatable in thesaurus page, when we click on the subdatatable button of a thesaurus row
        # It opens a tab with a datatable containing items of chosen thesaurus

        observeEvent(input$sub_datatable, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$sub_datatable"))

          # Get link id
          link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))

          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))

          shiny.fluent::updateComboBox.shinyInput(session, "items_chosen_thesaurus", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options, value = value)

          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-thesaurus_pivot button[name=\"{i18n$t('thesaurus_items')}\"]').click();"))
        })

        observeEvent(input$items_chosen_thesaurus, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$items_chosen_thesaurus"))

          if (length(input$items_chosen_thesaurus) > 1) link_id <- input$items_chosen_thesaurus$key
          else link_id <- input$items_chosen_thesaurus
          if (length(input$code_chosen_datamart_thesaurus) > 0){
            if (length(input$code_chosen_datamart_thesaurus) > 1) code_link_id <- input$code_chosen_datamart_thesaurus$key
            else code_link_id <- input$code_chosen_datamart_thesaurus
          }
          else code_link_id <- 0L

          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$thesaurus %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_datamart_thesaurus", options = options, value = value)
          }

          # Create a r var to export value to other observers
          r$thesaurus_link_id <- link_id

          # Get datamarts linked to this thesaurus
          data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
          datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)

          options <- convert_tibble_to_list(datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_datamart", options = options, value = NULL)

          # Set r$thesaurus_refresh_thesaurus_items to "all_items", cause we havn't chosen yet the thesaurus or the datamart
          r$thesaurus_refresh_thesaurus_items <- paste0(link_id, "reset_all_items")
        })

        observeEvent(input$show_only_used_items, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$show_only_used_items"))
          
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- "only_used_items"
          else r$thesaurus_refresh_thesaurus_items <- "all_items"
        })

        # When value of datamart changes, change value or r$thesaurus_refresh_thesaurus_items, depending on show_only_used_items
        # Add input$thesaurus_datamart in the value, to refresh even if the value doesn't change
        # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...

        observeEvent(input$thesaurus_datamart, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_datamart"))
          
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "only_used_items")
          else r$thesaurus_refresh_thesaurus_items <- r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "all_items")
        })

        observeEvent(r$thesaurus_refresh_thesaurus_items, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$thesaurus_refresh_thesaurus_items"))

          req(r$thesaurus_link_id)

          # Get all items from the chosen thesaurus

          r$thesaurus_items <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, module_id = id, thesaurus_id = r$thesaurus_link_id, category = "delete")

          if (length(input$thesaurus_datamart) > 0 & !grepl("reset", r$thesaurus_refresh_thesaurus_items)){
            if (input$thesaurus_datamart != ""){

              count_items_rows <- tibble::tibble()
              count_patients_rows <- tibble::tibble()

              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_items_rows"),
                error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart",
                  error_name = paste0(id, " - count_items_rows"), category = "Error", error_report = toString(e), i18n = i18n))

              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_patients_rows"),
                error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart",
                  error_name = paste0(id, " - count_patients_rows"), category = "Error", error_report = toString(e), i18n = i18n))

              if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, "fail_load_datamart", "severeWarning", i18n = i18n, ns = ns)
              req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)

              # Transform count_rows cols to integer, to be sortable
              r$thesaurus_items <- r$thesaurus_items %>%
                dplyr::left_join(count_items_rows, by = "item_id") %>%
                dplyr::left_join(count_patients_rows, by = "item_id") %>%
                dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")

              # If r$thesaurus_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
              if (grepl("only_used_items", r$thesaurus_refresh_thesaurus_items)) r$thesaurus_items <- r$thesaurus_items %>% dplyr::filter(count_items_rows > 0)

              # r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
            }
          }

          r$thesaurus_items_temp <- r$thesaurus_items %>%
            dplyr::mutate(modified = FALSE) %>%
            dplyr::mutate_at("item_id", as.character) %>%
            dplyr::arrange(name)

          action_buttons <- "delete"

          editable_cols <- c("name", "display_name", "unit")
          searchable_cols <- c("item_id", "name", "display_name", "unit")
          factorize_cols <- c("unit")
          column_widths <- c("item_id" = "80px", "action" = "80px", "unit" = "100px")

          if ("count_patients_rows" %in% names(r$thesaurus_items)){
            sortable_cols <- c("id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
            centered_cols <- c("id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
            col_names <- get_col_names(table_name = "thesaurus_items_with_counts", i18n = i18n)
          }
          else {
            sortable_cols <- c("id", "name", "display_name", "category")
            centered_cols <- c("id", "unit", "datetime", "action")
            col_names <- get_col_names(table_name = "thesaurus_items", i18n = i18n)
          }

          hidden_cols <- c("id", "thesaurus_id", "datetime", "deleted", "modified")

          # Render datatable
          render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$thesaurus_items_temp,
            output_name = "sub_datatable", col_names =  col_names,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
            searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")

          # Create a proxy for datatatable
          r$sub_thesaurus_datatable_proxy <- DT::dataTableProxy("sub_datatable", deferUntilFlush = FALSE)

          # Reload datatable
          observeEvent(r$thesaurus_items_temp, {

            # Reload data of datatable
            DT::replaceData(r$sub_thesaurus_datatable_proxy, r$thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
          })
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$thesaurus_refresh_thesaurus_items"))

        })

        # Reload cache

        thesaurus_reload_cache_prefix <- "thesaurus_reload_cache"
        thesaurus_reload_cache_react_variable <- "thesaurus_reload_cache_confirm"
        thesaurus_reload_cache_variable <- paste0(thesaurus_reload_cache_prefix, "_open_dialog")

        r[[thesaurus_reload_cache_variable]] <- FALSE

        output[[thesaurus_reload_cache_react_variable]] <- shiny.fluent::renderReact({
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - output$thesaurus_reload_cache_confirm"))

          shiny.fluent::Dialog(
            hidden = !r[[thesaurus_reload_cache_variable]],
            onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", thesaurus_reload_cache_prefix, "_hide_dialog', Math.random()); }")),
            dialogContentProps = list(type = 0, title = i18n$t("thesaurus_reload_cache_text"), closeButtonAriaLabel = "Close", subText = tagList(i18n$t("thesaurus_reload_cache_subtext"), br(), br())),
            modalProps = list(),
            shiny.fluent::DialogFooter(
              shiny.fluent::PrimaryButton.shinyInput(ns(paste0(thesaurus_reload_cache_prefix, "_reload_cache_confirmed")), text = i18n$t("yes")),
              shiny.fluent::DefaultButton.shinyInput(ns(paste0(thesaurus_reload_cache_prefix, "_reload_cache_canceled")), text = i18n$t("no"))
            )
          )
        })

        # Whether to close or not delete dialog box
        observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_hide_dialog")]], {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_hide_dialog"))
          r[[thesaurus_reload_cache_variable]] <- FALSE
        })
        observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_reload_cache_canceled")]], {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_canceled"))
          r[[thesaurus_reload_cache_variable]] <- FALSE
        })

        observeEvent(input$reload_thesaurus_cache, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$reload_thesaurus_cache"))

          req(length(input$items_chosen_thesaurus) > 1)
          r[[thesaurus_reload_cache_variable]] <- TRUE
        })

        observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_reload_cache_confirmed")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_confirmed"))

          r[[thesaurus_reload_cache_variable]] <- FALSE

          sql <- glue::glue_sql("SELECT id FROM thesaurus_items WHERE thesaurus_id = {r$thesaurus_link_id}", .con = r$db)
          thesaurus_items <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)

          sql <- glue::glue_sql(paste0("SELECT * FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
            "link_id IN ({thesaurus_items*})"), .con = r$db)

          # Delete cache
          sql <- glue::glue_sql(paste0("DELETE FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
            "link_id IN ({thesaurus_items*})"), .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)

          sql <- glue::glue_sql(paste0("SELECT * FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
            "link_id IN ({thesaurus_items*})"), .con = r$db)

          # Reset datamart dropdown
          data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == r$thesaurus_link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
          datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)

          options <- convert_tibble_to_list(datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_datamart", options = options, value = NULL)

          # Reload datatable
          r$thesaurus_refresh_thesaurus_items <- paste0(Sys.time(), "reset")
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_confirmed"))
        })

        # Delete a row or multiple rows in datatable

        thesaurus_items_delete_prefix <- "thesaurus_items"
        thesaurus_items_delete_variable <- paste0(thesaurus_items_delete_prefix, "_open_dialog")
        thesaurus_items_dialog_title <- "thesaurus_items_delete"
        thesaurus_items_dialog_subtext <- "thesaurus_items_delete_subtext"
        thesaurus_items_react_variable <- "thesaurus_items_delete_confirm"
        thesaurus_items_table <- "thesaurus_items"
        thesaurus_items_id_var_sql <- "id"
        thesaurus_items_id_var_r <- paste0(id, "_delete_thesaurus_items")
        thesaurus_items_delete_message <- "thesaurus_items_deleted"
        thesaurus_items_reload_variable <- "reload_thesaurus_items"
        thesaurus_items_information_variable <- paste0(id, "_thesaurus_items_deleted")
        thesaurus_items_delete_variable <- paste0(thesaurus_items_delete_prefix, "_open_dialog")

        delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
          delete_prefix = thesaurus_items_delete_prefix, dialog_title = thesaurus_items_dialog_title, dialog_subtext = thesaurus_items_dialog_subtext,
          react_variable = thesaurus_items_react_variable, table = thesaurus_items_table, id_var_sql = thesaurus_items_id_var_sql, id_var_r = thesaurus_items_id_var_r,
          delete_message = thesaurus_items_delete_message, translation = TRUE, reload_variable = thesaurus_items_reload_variable,
          information_variable = thesaurus_items_information_variable)

        # Delete one row (with icon on DT)

        observeEvent(input$thesaurus_items_deleted_pressed, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_items_deleted_pressed"))

          r[[paste0(id, "_delete_thesaurus_items")]] <- as.integer(substr(input$thesaurus_items_deleted_pressed, nchar("sub_delete_") + 1, 100))
          r[[thesaurus_items_delete_variable]] <- TRUE
        })

        # Delete multiple rows (with "Delete selection" button)

        observeEvent(input$thesaurus_items_delete_selection, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_items_delete_selection"))

          req(length(input$sub_datatable_rows_selected) > 0)

          r[[paste0(id, "_delete_thesaurus_items")]] <- r$thesaurus_items_temp[input$sub_datatable_rows_selected, ] %>% dplyr::pull(id)
          r[[thesaurus_items_delete_variable]] <- TRUE
        })

        observeEvent(r[[thesaurus_items_reload_variable]], {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$reload_thesaurus_items"))

          # Reload datatable
          r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
        })
      }
  })
}
