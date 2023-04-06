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
    "settings_vocabularies", "data_source")
  
  cards <- c("datatable_card", "edit_code_card", "options_card", "vocabularies_tables_datatable_card", "import_vocabulary_card")
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
        
        # --- --- --- --- -- -
        # Management card ----
        # --- --- --- --- -- -
        
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
      
      # --- --- --- --- -- -
      # Management card ----
      # --- --- --- --- -- -
      
      render_settings_datatable_card(i18n = i18n, ns = ns, div_id = "datatable_card", title = "datamarts_management", 
        inputs = c("name" = "textfield", "data_source" = "dropdown")),
      
      # --- --- --- --- -- -
      # Edit code card ----
      # --- --- --- --- -- -
      
      div(id = ns("edit_code_card"), 
        make_card(i18n$t("edit_datamart_code"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_combobox(i18n = i18n, ns = ns, label = "datamart", id = "code_selected_datamart_or_vocabulary",
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
            make_combobox(i18n = i18n, ns = ns, label = "datamart", id = "options_selected_datamart_or_vocabulary",
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
  
  # --- --- --- --- --- --
  # Vocabularies card ----
  # --- --- --- --- --- --
  
  if (id == "settings_vocabularies"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::reactOutput(ns("help_panel")),
      shiny.fluent::reactOutput(ns("help_modal")),
      shiny.fluent::reactOutput(ns("vocabularies_table_reload_cache_confirm")),
      shiny.fluent::reactOutput(ns("vocabularies_table_delete_confirm")),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "vocabularies", text = i18n$t("vocabularies"))
      ), maxDisplayedItems = 3),
      
      # --- --- --- --- -
      ## Pivot items ----
      # --- --- --- --- -
      
      shiny.fluent::Pivot(
        id = ns("vocabularies_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("vocabularies_management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_vocabulary_code")),
        shiny.fluent::PivotItem(id = "vocabularies_tables_datatable_card", itemKey = "vocabularies_tables_datatable_card", headerText = i18n$t("vocabulary_tables")),
        shiny.fluent::PivotItem(id = "import_vocabulary_card", itemKey = "import_vocabulary_card", headerText = i18n$t("import_vocabulary"))
      ),
      
      
      forbidden_cards,
      
      # --- --- --- --- -- -
      # Management card ----
      # --- --- --- --- -- -
      
      div(id = ns("datatable_card"),
        make_card(i18n$t("vocabularies_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "id", id = "vocabulary_id", width = "300px"),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "vocabulary_name", width = "300px"),
              make_dropdown(i18n = i18n, ns = ns, label = "data_source", id = "data_source", width = "300px", multiSelect = TRUE),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add")), style = "margin-top:38px;")),
            div(DT::DTOutput(ns("management_datatable")), style = "z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:1; margin-top:-30px; width:500px;"
            )
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
              make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "code_selected_datamart_or_vocabulary",
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
      
      # --- --- --- --- --- --- --- -
      # Vocabularies tables card ----
      # --- --- --- --- --- --- --- -
      
      div(id = ns("vocabularies_tables_datatable_card"),
        make_card(i18n$t("vocabularies_tables"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_dropdown(i18n = i18n, ns = ns, label = "table", id = "vocabularies_table", width = "300px",
                options = list(
                  list(key = "concept", text = "CONCEPT"),
                  list(key = "domain", text = "DOMAIN"),
                  list(key = "concept_class", text = "CONCEPT CLASS"),
                  list(key = "concept_relationship", text = "CONCEPT RELATIONSHIP"),
                  list(key = "relationship", text = "RELATIONSHIP"),
                  list(key = "concept_synonym", text = "CONCEPT SYNONYM"),
                  list(key = "concept_ancestor", text = "CONCEPT ANCESTOR"),
                  list(key = "drug_strength", text = "DRUG STRENGTH")
                )),
              make_dropdown(i18n = i18n, ns = ns, label = "columns", id = "vocabularies_table_cols", width = "300px", multiSelect = TRUE),
              conditionalPanel(condition = "input.vocabularies_table == 'concept'", ns = ns,
                div(shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  make_toggle(i18n = i18n, ns = ns, label = "vocabularies_datatable_show_mapped_concepts", inline = TRUE), style = "margin-top:45px;"))),
              conditionalPanel(condition = "['concept_relationship', 'concept_synonym', 'concept_ancestor', 'drug_strength'].includes(input.vocabularies_table)", ns = ns,
                div(shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  make_toggle(i18n = i18n, ns = ns, label = "vocabularies_datatable_show_row_details", inline = TRUE), style = "margin-top:45px;")))
            ),
            # conditionalPanel(condition = "input.vocabularies_table == 'concept'", ns = ns,
            #   shiny.fluent::Stack(
            #     horizontal = TRUE, tokens = list(childrenGap = 50),
            #     make_dropdown(i18n = i18n, ns = ns, label = "datamart", id = "vocabularies_datamart", width = "300px"),
            #     conditionalPanel(condition = "input.datamart !== ''", ns = ns,
            #       div(strong(i18n$t("show_only_used_items"), style = "display:block; padding-bottom:12px;"),
            #         shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
            #   )
            # ),
            DT::DTOutput(ns("vocabularies_tables_datatable")), br(),
            conditionalPanel(condition = "input.vocabularies_table == null", ns = ns, div(br(), br(), br())),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("vocabularies_tables_datatable_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("vocabularies_tables_delete_selection"), i18n$t("delete_selection")),
                conditionalPanel(condition = "input.vocabularies_table == 'concept'", ns = ns,
                  shiny.fluent::DefaultButton.shinyInput(ns("reload_vocabularies_tables_cache"), i18n$t("reload_cache")))
              ),
              style = "position:relative; margin-top:-50px;"
            ),
            conditionalPanel(condition = "input.vocabularies_datatable_show_mapped_concepts == true && input.vocabularies_table == 'concept'", ns = ns, 
              br(), hr(), br(),
              DT::DTOutput(ns("vocabularies_tables_mapped_concepts_datatable"))
            ),
            conditionalPanel(condition = paste0("['concept_relationship', 'concept_synonym', 'concept_ancestor', 'drug_strength'].includes(input.vocabularies_table) && ",
              "input.vocabularies_datatable_show_row_details == true"), ns = ns, 
              br(),
              div(uiOutput(ns("vocabularies_datatable_row_details")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 8px; margin-right: 5px;")
            )
          )
        ), br()
      ),
      
      # --- --- --- --- --- --- ---
      # Import vocabulary card ----
      # --- --- --- --- --- --- ---
      
      div(id = ns("import_vocabulary_card"),
        make_card(i18n$t("import_vocabulary"),
          div(
            br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("import_vocabulary_browse"), i18n$t("choose_zip_file")),
              uiOutput(ns("import_vocabulary_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_vocabulary_button"), i18n$t("import_vocabulary"), iconProps = list(iconName = "Download")), br(),
            shinyjs::hidden(
              div(
                id = ns("imported_vocabularies_div"), br(),
                strong(i18n$t("imported_vocabularies")),
                div(DT::DTOutput(ns("imported_vocabularies")))
              )
            ),
            div(style = "display:none;", fileInput(ns("import_vocabulary_upload"), label = "", multiple = FALSE, accept = ".zip"))
          )
        )
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
      "settings_vocabularies", "data_source")
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    if (id == "settings_vocabularies") table <- "vocabulary"
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    if (table %in% c("datamarts", "vocabulary")){
      observeEvent(r[[table]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - updateComboBox"))
        
        if (table == "datamarts"){
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          shiny.fluent::updateComboBox.shinyInput(session, "options_selected_datamart_or_vocabulary", options = options)
        }
        
        if (table == "vocabulary"){
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
          shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options)
        }
        
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options)
      })
    }
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    # Toggles IDs
    cards <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "vocabularies_tables_datatable_card", "import_vocabulary_card")
    sapply(cards, shinyjs::hide)
    
    show_hide_cards(r = r, input = input, session = session, table = table, id = id, cards = cards)
    
    # Show first card
    
    if (paste0(get_plural(table), "_datatable_card") %in% r$user_accesses) shinyjs::show("datatable_card")
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
      # If page = vocabulary, data_source is character, not integer (multiple choices)
      new_data <- list()
      if (id == "settings_vocabularies") new_data_var <- c("vocabulary_id" = "char", "vocabulary_name" = "char", "data_source" = "char")
      else new_data_var <- c("name" = "char", "description" = "char", "data_source" = "int", "datamart" = "int", 
        "study" = "int", "patient_lvl_module_family" = "int", "aggregated_module_family" = "int")
      
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
      })
      
      # Convert data_source to string, for page vocabularies
      if (id == "settings_vocabularies"){
        if (length(new_data$data_source) == 1) new_data$data_source <- coalesce2(type = "char", x = input$data_source)
        else new_data$data_source <- toString(as.integer(new_data$data_source))
        
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
  
    dropdowns_datatable <- switch(table,
      "data_sources" = "",
      "datamarts" = "",
      "vocabulary" = c("data_source_id" = "data_sources"))
    
    # Dropdowns with multiSelect
    dropdowns_multiselect <- ""
    if (table == "vocabulary") dropdowns_multiselect <- "data_source_id"
    
    # Action buttons for each module / page
    action_buttons = switch(table,
      "data_sources" = "delete",
      "datamarts" = c("delete", "edit_code", "options"),
      "vocabulary" = c("delete", "edit_code")
    )
    
    # Editable cols
    if (table == "vocabulary") editable_cols <- c("vocabulary_id", "vocabulary_name")
    else editable_cols <- c("name", "description")
    
    # Sortable cols
    if (table == "vocabulary") sortable_cols <- c("vocabulary_id", "vocabulary_name", "datetime")
    else sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    
    # Column widths
    column_widths <- c("datetime" = "130px", "creator_id" = "200px", "action" = "80px")
    
    # Centered columns
    centered_cols <- c("creator_id", "datetime", "action")
    
    # Searchable_cols
    if (table == "vocabulary") searchable_cols <- c("vocabulary_id", "vocabulary_name")
    else searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    
    # Factorize_cols
    factorize_cols <- switch(table,
      "data_sources" = "creator_id",
      "datamarts" = c("data_source_id", "creator_id"),
      "vocabulary" = "creator_id")
    
    if (table == "vocabulary") hidden_cols <- c("id", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id",
      "display_order", "creator_id", "deleted", "modified")
    else hidden_cols <- c("id", "description", "deleted", "modified")
    
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
        if (table == "vocabulary") data <- tibble::tibble(id = integer(), vocabulary_id = character(), vocabulary_name = character(), 
          vocabulary_reference = character(), vocabulary_version = character(), vocabulary_concept_id = character(), data_source_id = character(), 
          display_order = integer(), creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
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
      # Do that for main datatable (management_datatable) & vocabularies_tables_datatable
      observeEvent(input$management_datatable_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_datatable_cell_edit"))
        
        edit_info <- input$management_datatable_cell_edit
        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })
  
      # observeEvent(input$vocabularies_tables_datatable_cell_edit, {
      #   
      #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_cell_edit"))
      #   
      #   edit_info <- input$vocabularies_tables_datatable_cell_edit
      #   r$thesaurus_items_temp <- DT::editData(r$thesaurus_items_temp, edit_info, rownames = FALSE)
      #   r$thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      # })
    
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r[[table]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - update dropdowns"))
        
        update_settings_datatable(input = input, module_id = id, r = r, ns = ns, table = table, 
          dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), i18n = i18n)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$", table, " - update dropdowns"))
      })
    
      # When save button is clicked
      # Do that for main datatable (management_datatable) & vocabularies_tables_datatable
      observeEvent(input$management_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_save"))
        
        req(nrow(r[[paste0(table, "_temp")]]) > 0)
        
        save_settings_datatable_updates(output = output, r = r, m = m, ns = ns, table = table, i18n = i18n, duplicates_allowed = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$management_save"))
      })
      
      # observeEvent(input$vocabularies_tables_datatable_save, {
      #   
      #   if (perf_monitoring) monitor_perf(r = r, action = "start")
      #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_save"))
      #   
      #   req(input$vocabulary_tables_selected_vocabulary)
      #   save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items", 
      #     r_table = "thesaurus_items", duplicates_allowed = TRUE, i18n = i18n)
      #   
      #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$vocabularies_tables_datatable_save"))
      # })
      
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
      
      delete_element(r = r, m = m, input = input, output = output, session = session, ns = ns, i18n = i18n,
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
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "options_selected_datamart_or_vocabulary", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to options_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(get_plural(table), '_pivot')} button[name=\"{i18n$t(paste0(get_singular(table), '_options'))}\"]').click();"))
        })
        
        observeEvent(input$options_selected_datamart_or_vocabulary, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options_selected_datamart_or_vocabulary"))
          
          if (length(input$options_selected_datamart_or_vocabulary) > 1) link_id <- input$options_selected_datamart_or_vocabulary$key
          else link_id <- input$options_selected_datamart_or_vocabulary
          if (length(input$code_selected_datamart_or_vocabulary) > 0){
            if (length(input$code_selected_datamart_or_vocabulary) > 1) code_link_id <- input$code_selected_datamart_or_vocabulary$key
            else code_link_id <- input$code_selected_datamart_or_vocabulary
          }
          else code_link_id <- 0L
          
          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options, value = value)
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
          
          req(input$options_selected_datamart_or_vocabulary)
          
          if (length(input$options_selected_datamart_or_vocabulary) > 1) link_id <- input$options_selected_datamart_or_vocabulary$key
          else link_id <- input$options_selected_datamart_or_vocabulary
          
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
      
      if (table %in% c("datamarts", "vocabulary")){
        
        # Button "Edit code" is clicked on the datatable
        observeEvent(input$edit_code, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$edit_code"))

          # Get link_id variable, to update code editor
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          if (table == "vocabulary") {
            options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
            value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
          }
          else {
            options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          }
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options, value = value)
          if (table == "datamarts") shiny.fluent::updateComboBox.shinyInput(session, "options_selected_datamart_or_vocabulary", options = options, value = value)
          if (table == "vocabulary") shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(get_plural(table), '_pivot')} button[name=\"{i18n$t(paste0('edit_', get_singular(table), '_code'))}\"]').click();"))
          
        })
        
        observeEvent(input$code_selected_datamart_or_vocabulary, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$code_selected_datamart_or_vocabulary"))

          if (length(input$code_selected_datamart_or_vocabulary) > 1) link_id <- input$code_selected_datamart_or_vocabulary$key
          else link_id <- input$code_selected_datamart_or_vocabulary
          
          if (table == "datamarts"){
            if (length(input$options_selected_datamart_or_vocabulary) > 0){
              if (length(input$options_selected_datamart_or_vocabulary) > 1) options_link_id <- input$options_selected_datamart_or_vocabulary$key
              else options_link_id <- input$options_selected_datamart_or_vocabulary
            }
            else options_link_id <- 0L

            if (link_id != options_link_id){
              options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "options_selected_datamart_or_vocabulary", options = options, value = value)
            }
          }
          
          if (table == "vocabulary"){
            if (length(input$vocabulary_tables_selected_vocabulary) > 0){
              if (length(input$vocabulary_tables_selected_vocabulary) > 1) items_link_id <- input$vocabulary_tables_selected_vocabulary$key
              else items_link_id <- input$vocabulary_tables_selected_vocabulary
            }
            else items_link_id <- 0L
            
            if (link_id != items_link_id){
              options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
              value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
              shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options, value = value)
            }
          }

          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$vocabulary_id <- NA_integer_

          if (id == "settings_datamarts") r$datamart_id <- link_id
          if (id == "settings_vocabularies") r$vocabulary_id <- link_id

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
          
          req(input$code_selected_datamart_or_vocabulary)
          
          if (length(input$code_selected_datamart_or_vocabulary) > 1) link_id <- input$code_selected_datamart_or_vocabulary$key
          else link_id <- input$code_selected_datamart_or_vocabulary
          
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
          if (table == "datamarts"){
            vars <- c("patients", "stays", "labs_vitals", "orders", "text", "diagnoses")
            for (var in vars) d[[var]] <- tibble::tibble()
          }
          
          edited_code <- r[[paste0(id, "_code")]] %>% stringr::str_replace_all("\r", "\n")
          
          output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
          output$code_result <- renderText(
            execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
              i18n = i18n, r = r, d = d, m = m, edited_code = edited_code))
          
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
      
      # --- --- --- --- --- --- --- --- --- --- -
      # Generate vocabulary tables datatable ----
      # --- --- --- --- --- --- --- --- --- --- -
        
      if (table == "vocabulary"){
        
        # Cols names depending on table
        
        table_cols_options <- list()
        table_cols_options_value <- list()
        
        table_cols_options$concept <- list(
          list(key = 1, text = "concept_id"),
          list(key = 2, text = "concept_name"),
          list(key = 3, text = "domain_id"),
          list(key = 4, text = "vocabulary_id"),
          list(key = 5, text = "concept_class_id"),
          list(key = 6, text = "standard_concept"),
          list(key = 7, text = "concept_code"),
          list(key = 8, text = "valid_start_date"),
          list(key = 9, text = "valid_end_date"),
          list(key = 10, text = "invalid_reason"))
        table_cols_options_value$concept <- c(1, 2, 3, 4, 5)
        
        table_cols_options$domain <- list(
          list(key = 1, text = "domain_id"),
          list(key = 2, text = "domain_name"),
          list(key = 3, text = "domain_concept_id"))
        table_cols_options_value$domain <- c(1, 2, 3)
        
        table_cols_options$concept_class <- list(
          list(key = 1, text = "concept_class_id"),
          list(key = 2, text = "concept_class_name"),
          list(key = 3, text = "concept_class_concept_id"))
        table_cols_options_value$concept_class <- c(1, 2, 3)
        
        table_cols_options$concept_relationship <- list(
          list(key = 1, text = "concept_id_1"),
          list(key = 2, text = "concept_id_2"),
          list(key = 3, text = "relationship_id"),
          list(key = 4, text = "valid_stard_date"),
          list(key = 5, text = "valid_end_date"),
          list(key = 6, text = "invalid_reason"))
        table_cols_options_value$concept_relationship <- c(1, 2, 3)
        
        table_cols_options$relationship <- list(
          list(key = 1, text = "relationship_id"),
          list(key = 2, text = "relationship_name"),
          list(key = 3, text = "is_hierachical"),
          list(key = 4, text = "defines_ancestry"),
          list(key = 5, text = "reverse_relationship_id"),
          list(key = 6, text = "relationship_concept_id"))
        table_cols_options_value$relationship <- c(1, 2, 3, 4, 5, 6)
        
        table_cols_options$concept_synonym <- list(
          list(key = 1, text = "concept_id"),
          list(key = 2, text = "concept_synonym"),
          list(key = 3, text = "language_concept_id"))
        table_cols_options_value$concept_synonym <- c(1, 2, 3)
        
        table_cols_options$concept_ancestor <- list(
          list(key = 1, text = "ancestor_concept_id"),
          list(key = 2, text = "descendant_concept_id"),
          list(key = 3, text = "min_levels_of_separation"),
          list(key = 4, text = "max_levels_of_separation"))
        table_cols_options_value$concept_ancestor <- c(1, 2, 3, 4)
        
        table_cols_options$drug_strength <- list(
          list(key = 1, text = "drug_concept_id"),
          list(key = 2, text = "ingredient_concept_id"),
          list(key = 3, text = "amount_value"),
          list(key = 4, text = "amount_unit_concept_id"),
          list(key = 5, text = "numerator_value"),
          list(key = 6, text = "numerator_unit_concept_id"),
          list(key = 7, text = "denominator_value"),
          list(key = 8, text = "denominator_unit_concept_id"),
          list(key = 9, text = "box_size"),
          list(key = 10, text = "valid_start_date"),
          list(key = 11, text = "valid_end_date"),
          list(key = 12, text = "invalid_reason"))
        table_cols_options_value$drug_strength <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
        
        observeEvent(input$vocabularies_table, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table"))
          
          # Reset vocabularies_datatable_row_details div
          output$vocabularies_datatable_row_details <- renderUI("")
          
          # Update dropdown with which cols to show
          
          shiny.fluent::updateDropdown.shinyInput(session, "vocabularies_table_cols", 
            options = table_cols_options[[input$vocabularies_table]], value = table_cols_options_value[[input$vocabularies_table]])
          
          # Load data if not already loaded
          if (length(r[[input$vocabularies_table]]) == 0){
            sql <- glue::glue_sql("SELECT * FROM {`input$vocabularies_table`}", .con = m$db)
            r[[input$vocabularies_table]] <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate(modified = FALSE)
          }
          
          # Render datatable
          r$vocabularies_table_render_datatable <- Sys.time()
        })
        
        # Reload datatable when toggle row_details is updated, to change selection (single / multiple)
        observeEvent(input$vocabularies_datatable_show_row_details, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_datatable_show_row_details"))
          r$vocabularies_table_render_datatable <- Sys.time()
        })
        
        # Render datatable
        observeEvent(r$vocabularies_table_render_datatable, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$vocabularies_table_render_datatable"))
          
          req(input$vocabularies_table)
          
          # Reset vocabularies_datatable_row_details output
          output$vocabularies_datatable_row_details <- renderUI("")
          
          # Datatable args
          
          editable_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
              "valid_start_date", "valid_end_date", "invalid_reason"),
            "domain" = c("domain_id", "domain_name", "domain_concept_id"),
            "concept_class" = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
            "relationship" = c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
            "concept_synonym" = c("concept_id", "concept_synonym", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
              "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")
          )
          sortable_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
              "valid_start_date", "valid_end_date", "invalid_reason"),
            "domain" = c("domain_id", "domain_name", "domain_concept_id"),
            "concept_class" = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
            "relationship" = c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
            "concept_synonym" = c("concept_id", "concept_synonym", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
              "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")
          )
          centered_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", 
              "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"),
            "domain" = "domain_concept_id",
            "concept_class" = "concept_class_id",
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
            "relationship" = c("is_hierarchical", "defines_ancestry", "relationship_concept_id"),
            "concept_synonym" = c("concept_id", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id",  "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
              "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")
          )
          searchable_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id"),
            "domain" = c("domain_id", "domain_name", "domain_concept_id"),
            "concept_class" = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id"),
            "relationship" = c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
            "concept_synonym" = c("concept_id", "concept_synonym", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id",  "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
              "denominator_value", "denominator_unit_concept_id", "box_size")
          )
          factorize_cols <- switch(input$vocabularies_table,
            "concept" = c("domain_id", "vocabulary_id", "concept_class_id"),
            "domain" = "",
            "concept_class" = "",
            "concept_relationship" = "relationship_id",
            "relationship" = "",
            "concept_synonym" = "language_concept_id",
            "concept_ancestor" = "",
            "drug_strength" = ""
          )
          
          # Transform integer cols to character, to be searchable
          cols_to_char <- switch(input$vocabularies_table, 
            "concept" = "concept_id",
            "domain" = "domain_concept_id",
            "concept_class" = "concept_class_concept_id",
            "concept_relationship" = c("concept_id_1", "concept_id_2"),
            "relationship" = "relationship_concept_id",
            "concept_synonym" = c("concept_id", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_unit_concept_id", "numerator_unit_concept_id", "denominator_unit_concept_id")
          )
          
          data <- r[[input$vocabularies_table]] %>% dplyr::mutate_at(cols_to_char, as.character)
          
          if (input$vocabularies_datatable_show_row_details) selection <- "single" else selection <- "multiple"
          
          render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
            output_name = "vocabularies_tables_datatable", editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
            hidden_cols = c("id", "modified"), searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, selection = selection)
          
          # Create a proxy
          
          if (length(r$vocabularies_tables_datatable_proxy) == 0) r$vocabularies_tables_datatable_proxy <- 
              DT::dataTableProxy("vocabularies_tables_datatable", deferUntilFlush = FALSE)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$vocabularies_table_render_datatable"))
        })
        
        # Update which cols are hidden
        observeEvent(input$vocabularies_table_cols, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_cols"))
          
          r$vocabularies_tables_datatable_proxy %>%
            DT::showCols(1:length(table_cols_options[[input$vocabularies_table]])) %>%
            DT::hideCols(setdiff(1:(length(table_cols_options[[input$vocabularies_table]])), input$vocabularies_table_cols))
        })
        
        # Show row details
        observeEvent(input$vocabularies_tables_datatable_rows_selected, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_rows_selected"))
          
          req(input$vocabularies_table %in% c("concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength"))
          req(input$vocabularies_datatable_show_row_details)
          
          if (length(input$vocabularies_tables_datatable_rows_selected) == 0) output$vocabularies_datatable_row_details <- renderUI("")
          else if (length(input$vocabularies_tables_datatable_rows_selected) > 1) output$vocabularies_datatable_row_details <- 
            renderUI(i18n$t("select_only_one_row_for_details"))
          else if (length(input$vocabularies_tables_datatable_rows_selected) == 1){
            
            result <- ""
            
            if (length(r$concept) == 0) result <- div(i18n$t("load_concept_data_before"))
            
            selected_row <- r[[input$vocabularies_table]][input$vocabularies_tables_datatable_rows_selected, ]
            
            if (length(r$concept) > 0){
              if (input$vocabularies_table == "concept_relationship"){
                result <- div(
                  strong("concept_id_1"), " : ", selected_row$concept_id_1, br(),
                  strong("concept_name_1"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$concept_id_1) %>% dplyr::pull(concept_name), br(),
                  strong("relationship_id"), " : ", selected_row$relationship_id, br(),
                  strong("concept_id_2"), " : ", selected_row$concept_id_2, br(),
                  strong("concept_name_2"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$concept_id_2) %>% dplyr::pull(concept_name), br(),
                )
              }
              
              else if (input$vocabularies_table == "concept_synonym"){
                result <- div(
                  strong("concept_id"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("concept_synonym_name"), " : ", selected_row$concept_synonym_name, br(),
                  strong("language_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$language_concept_id) %>% dplyr::pull(concept_name), br()
                )
              }
              
              else if (input$vocabularies_table == "concept_ancestor"){
                result <- div(
                  strong("ancestor_concept_id"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$ancestor_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("descendant_concept_id"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$descendant_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("min_level_of_separation"), " : ", selected_row$min_level_of_separation, br(),
                  strong("max_level_of_separation"), " : ", selected_row$max_level_of_separation, br()
                )
              }
              
              else if (input$vocabularies_table == "drug_strength"){
                result <- div(
                  strong("drug_concept_id"), " : ", selected_row$drug_concept_id, br(),
                  strong("drug_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$drug_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("ingredient_concept_id"), " : ", selected_row$ingredient_concept_id, br(),
                  strong("ingredient_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$ingredient_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("amount_value"), " : ", selected_row$amount_value, br(),
                  strong("amount_unit_concept_id"), " : ", selected_row$amount_unit_concept_id, br(),
                  strong("amount_unit_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$amount_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("numerator_value"), " : ", selected_row$numerator_value, br(),
                  strong("numerator_unit_concept_id"), " : ", selected_row$numerator_unit_concept_id, br(),
                  strong("numerator_unit_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$numerator_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("denominator_value"), " : ", selected_row$denominator_value, br(),
                  strong("denominator_unit_concept_id"), " : ", selected_row$denominator_unit_concept_id, br(),
                  strong("denominator_unit_concept_name"), " : ", r$concept %>% dplyr::filter(concept_id == selected_row$denominator_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                  strong("box_size"), " : ", selected_row$box_size, br(),
                )
              }
            }
            
            output$vocabularies_datatable_row_details <- renderUI(result)
          }
        })
        
        # --- --- --- --- --- --- --- --- --- -- -
        # Update rows in vocabulary datatable ----
        # --- --- --- --- --- --- --- --- --- -- -
        
        observeEvent(input$vocabularies_tables_datatable_cell_edit, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_cell_edit"))
          
          edit_info <- input$vocabularies_tables_datatable_cell_edit
          r[[input$vocabularies_table]] <- DT::editData(r[[input$vocabularies_table]], edit_info, rownames = FALSE)
          
          # Store that this row has been modified
          r[[input$vocabularies_table]][[edit_info$row, "modified"]] <- TRUE
        })
        
        # Save updates
        
        observeEvent(input$vocabularies_tables_datatable_save, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_save"))
          
          if (nrow(r[[input$vocabularies_table]] %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
          
          ids_to_del <- r[[input$vocabularies_table]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
          req(length(ids_to_del) > 0)
          
          # Check if there are duplicates
          
          data_check_duplicates_cols <- switch(input$vocabularies_table,
            "concept" = "concept_id",
            "domain" = "domain_id",
            "concept_class" = "concept_class_id",
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id"),
            "relationship" = "relationship_id",
            "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
              "denominator_value", "denominator_unit_concept_id", "box_size")
          )
          
          check_duplicates <- r[[input$vocabularies_table]] %>% dplyr::group_by_at(data_check_duplicates_cols) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
          
          if (check_duplicates > 0) show_message_bar(output, "vocab_tables_duplicates_cols", "severeWarning", i18n, ns = ns)
          
          data_required_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", 
              "concept_code", "valid_start_date", "valid_end_date"),
            "domain" = c("domain_id", "domain_name", "domain_concept_id"),
            "concept_class" = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date"),
            "relationship" = "relationship_id",
            "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "valid_start_date", "valid_end_date")
          )
          
          check_required_cols <- r[[input$vocabularies_table]] %>% dplyr::filter(dplyr::if_any(dplyr::all_of(data_required_cols), ~ . %in% c("", NA_character_, NA_integer_))) %>% nrow()
            
          if (check_required_cols > 0) show_message_bar(output, "vocab_tables_empty_cols", "severeWarning", i18n, ns = ns)
          
          req(check_duplicates == 0, check_required_cols == 0)
          
          sql <- glue::glue_sql("DELETE FROM {`input$vocabularies_table`} WHERE id IN ({ids_to_del*})", .con = m$db)
          DBI::dbSendStatement(m$db, sql) -> query
          DBI::dbClearResult(query)
          
          data <- r[[input$vocabularies_table]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
          
          DBI::dbAppendTable(m$db, input$vocabularies_table, data)
          
          show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
        })
        
        # --- --- --- --- --- --- --- --- --- --- --- --
        # Delete rows in vocabulary table datatable ----
        # --- --- --- --- --- --- --- --- --- --- --- --
        
        r$vocabularies_table_open_dialog <- FALSE
        
        # IDs of selected rows
        
        observeEvent(input$vocabularies_tables_delete_selection, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_delete_selection"))
          
          req(length(input$vocabularies_tables_datatable_rows_selected) > 0)
          
          r$delete_vocabularies_table_rows <- r[[input$vocabularies_table]][input$vocabularies_tables_datatable_rows_selected, ] %>% dplyr::pull(id)
          r$vocabularies_table_open_dialog <- TRUE
        })
        
        # React for deletion confirmation
        
        output$vocabularies_table_delete_confirm <- shiny.fluent::renderReact({
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - output$vocabularies_table_delete_confirm"))
          
          shiny.fluent::Dialog(
            hidden = !r$vocabularies_table_open_dialog,
            onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('vocabularies_table_hide_dialog', Math.random()); }")),
            dialogContentProps = list(
              type = 0,
              title = i18n$t("vocabularies_table_delete"),
              closeButtonAriaLabel = "Close",
              subText = tagList( i18n$t("vocabularies_table_delete_subtext"), br(), br())
            ),
            shiny.fluent::DialogFooter(
              shiny.fluent::PrimaryButton.shinyInput(ns("vocabularies_table_delete_confirmed"), text = i18n$t("delete")),
              shiny.fluent::DefaultButton.shinyInput(ns("vocabularies_table_delete_canceled"), text = i18n$t("dont_delete"))
            )
          )
        })
        
        # Close dialog box if deletion canceled
        observeEvent(input$vocabularies_table_delete_canceled, r$vocabularies_table_open_dialog <- FALSE)
        
        # When the deletion is confirmed
        observeEvent(input$vocabularies_table_delete_confirmed, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_delete_confirmed"))
          
          # Close dialog box
          r$vocabularies_table_open_dialog <- FALSE
          
          sql <- glue::glue_sql("DELETE FROM {`input$vocabularies_table`} WHERE id IN ({r$delete_vocabularies_table_rows*})" , .con = m$db)
          DBI::dbSendStatement(m$db, sql) -> query
          DBI::dbClearResult(query)
          
          r[[input$vocabularies_table]] <- r[[input$vocabularies_table]] %>% dplyr::filter(id %not_in% r$delete_vocabularies_table_rows)
          
          r$vocabulary_table_reload_datatable <- Sys.time()
          
          show_message_bar(output, "vocabulary_table_rows_deleted", type ="severeWarning", i18n = i18n, ns = ns)
        })
        
        # Reload datatable
        
        observeEvent(r$vocabulary_table_reload_datatable, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$vocabulary_table_reload_datatable"))
          
          DT::replaceData(r$vocabularies_tables_datatable_proxy, r[[input$vocabularies_table]], resetPaging = FALSE, rownames = FALSE)
        })
        
        # observeEvent(r[[plugin_reload_variable]], {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$reload..plugins"))
        #   
        #   # Reload datatable
        #   r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        #   
        #   # Reload github description if opened
        #   r$show_plugin_details <- Sys.time()
        #   
        #   # Reload export plugin datatable
        #   
        #   # Reset "edit plugin code" fields and "plugin options" fields
        #   
        #   options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        #   
        #   sapply(c("code_chosen_plugin", "thesaurus", "thesaurus_selected_items"), 
        #     function(name) shiny.fluent::updateComboBox.shinyInput(session, name, options = options, value = NULL))
        #   shiny.fluent::updateChoiceGroup.shinyInput(session, "edit_code_ui_server", value = "ui")
        #   shiny.fluent::updateToggle.shinyInput(session, "hide_editor", value = FALSE)
        #   sapply(c("ace_edit_code_ui", "ace_edit_code_server", "ace_edit_code_translations"),
        #     function(name) shinyAce::updateAceEditor(session, name, value = ""))
        #   
        #   shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options, value = NULL)
        #   sapply(c("plugin_author", "plugin_version", "plugin_name_fr", "plugin_name_en", "plugin_category_fr", "plugin_category_en"),
        #     function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
        #   shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group", value = "everybody")
        #   shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", options = list(), value = NULL)
        #   shiny.fluent::updateChoiceGroup.shinyInput(session, "plugin_description_language", value = "fr")
        #   sapply(c("plugin_description_fr", "plugin_description_en"), function(name) shinyAce::updateAceEditor(session, name, value = ""))
        #   
        #   shiny.fluent::updateComboBox.shinyInput(session, "plugins_to_export", value = "")
        # })

        # observeEvent(input$sub_datatable, {
        # 
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$sub_datatable"))
        # 
        #   # Get link id
        #   link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
        # 
        #   options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
        #   value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
        # 
        #   shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options, value = value)
        #   shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options, value = value)
        # 
        #   # Set current pivot to edit_code_card
        #   shinyjs::runjs(glue::glue("$('#{id}-vocabularies_pivot button[name=\"{i18n$t('vocabulary_tables')}\"]').click();"))
        # })

        # observeEvent(input$vocabulary_tables_selected_vocabulary, {
        # 
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabulary_tables_selected_vocabulary"))
        # 
        #   if (length(input$vocabulary_tables_selected_vocabulary) > 1) link_id <- input$vocabulary_tables_selected_vocabulary$key
        #   else link_id <- input$vocabulary_tables_selected_vocabulary
        #   if (length(input$code_selected_datamart_or_vocabulary) > 0){
        #     if (length(input$code_selected_datamart_or_vocabulary) > 1) code_link_id <- input$code_selected_datamart_or_vocabulary$key
        #     else code_link_id <- input$code_selected_datamart_or_vocabulary
        #   }
        #   else code_link_id <- 0L
        # 
        #   if (link_id != code_link_id){
        #     options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
        #     value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
        #     shiny.fluent::updateComboBox.shinyInput(session, "code_selected_datamart_or_vocabulary", options = options, value = value)
        #   }
        # 
        #   # Create a r var to export value to other observers
        #   r$vocabulary_link_id <- link_id
        # 
        #   # Get datamarts linked to this vocabulary
        #   data_sources <- stringr::str_split(r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
        #   datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)
        # 
        #   options <- convert_tibble_to_list(datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        #   shiny.fluent::updateComboBox.shinyInput(session, "vocabularies_datamart", options = options, value = NULL)
        # 
        #   # Set r$vocabulary_refresh_vocabulary_concepts to "all_items", cause we havn't selected yet the vocabulary or the datamart
        #   r$vocabulary_refresh_vocabulary_concepts <- paste0(link_id, "reset_all_items")
        # })

        # observeEvent(input$show_only_used_items, {
        # 
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$show_only_used_items"))
        # 
        #   if (input$show_only_used_items) r$vocabulary_refresh_vocabulary_concepts <- "only_used_items"
        #   else r$vocabulary_refresh_vocabulary_concepts <- "all_items"
        # })

        # When value of datamart changes, change value or r$thesaurus_refresh_thesaurus_items, depending on show_only_used_items
        # Add input$vocabularies_datamart in the value, to refresh even if the value doesn't change
        # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...

        # observeEvent(input$vocabularies_datamart, {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_datamart"))
        #   
        #   if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- paste0(input$vocabularies_datamart, "only_used_items")
        #   else r$thesaurus_refresh_thesaurus_items <- r$thesaurus_refresh_thesaurus_items <- paste0(input$vocabularies_datamart, "all_items")
        # })

        # observeEvent(r$thesaurus_refresh_thesaurus_items, {
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "start")
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$thesaurus_refresh_thesaurus_items"))
        # 
        #   req(r$thesaurus_link_id)
        # 
        #   # Get all items from the selected thesaurus
        # 
        #   r$thesaurus_items <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, module_id = id, thesaurus_id = r$thesaurus_link_id, category = "delete")
        # 
        #   if (length(input$vocabularies_datamart) > 0 & !grepl("reset", r$thesaurus_refresh_thesaurus_items)){
        #     if (input$vocabularies_datamart != ""){
        # 
        #       count_items_rows <- tibble::tibble()
        #       count_patients_rows <- tibble::tibble()
        # 
        #       # Add count_items_rows in the cache & get it if already in the cache
        #       tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, thesaurus_id = r$thesaurus_link_id,
        #         datamart_id = as.integer(input$vocabularies_datamart), category = "count_items_rows"),
        #         error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart",
        #           error_name = paste0(id, " - count_items_rows"), category = "Error", error_report = toString(e), i18n = i18n))
        # 
        #       # Add count_items_rows in the cache & get it if already in the cache
        #       tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, d = d, i18n = i18n, thesaurus_id = r$thesaurus_link_id,
        #         datamart_id = as.integer(input$vocabularies_datamart), category = "count_patients_rows"),
        #         error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart",
        #           error_name = paste0(id, " - count_patients_rows"), category = "Error", error_report = toString(e), i18n = i18n))
        # 
        #       if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, "fail_load_datamart", "severeWarning", i18n = i18n, ns = ns)
        #       req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
        # 
        #       # Transform count_rows cols to integer, to be sortable
        #       r$thesaurus_items <- r$thesaurus_items %>%
        #         dplyr::left_join(count_items_rows, by = "item_id") %>%
        #         dplyr::left_join(count_patients_rows, by = "item_id") %>%
        #         dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        #         dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
        # 
        #       # If r$thesaurus_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
        #       if (grepl("only_used_items", r$thesaurus_refresh_thesaurus_items)) r$thesaurus_items <- r$thesaurus_items %>% dplyr::filter(count_items_rows > 0)
        # 
        #       # r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
        #     }
        #   }
        # 
        #   r$thesaurus_items_temp <- r$thesaurus_items %>%
        #     dplyr::mutate(modified = FALSE) %>%
        #     dplyr::mutate_at("item_id", as.character) %>%
        #     dplyr::arrange(name)
        # 
        #   action_buttons <- "delete"
        # 
        #   editable_cols <- c("name", "display_name", "unit")
        #   searchable_cols <- c("item_id", "name", "display_name", "unit")
        #   factorize_cols <- c("unit")
        #   column_widths <- c("item_id" = "80px", "action" = "80px", "unit" = "100px")
        # 
        #   if ("count_patients_rows" %in% names(r$thesaurus_items)){
        #     sortable_cols <- c("id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
        #     centered_cols <- c("id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        #     col_names <- get_col_names(table_name = "thesaurus_items_with_counts", i18n = i18n)
        #   }
        #   else {
        #     sortable_cols <- c("id", "name", "display_name", "category")
        #     centered_cols <- c("id", "unit", "datetime", "action")
        #     col_names <- get_col_names(table_name = "thesaurus_items", i18n = i18n)
        #   }
        # 
        #   hidden_cols <- c("id", "thesaurus_id", "datetime", "deleted", "modified")
        # 
        #   # Render datatable
        #   render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$thesaurus_items_temp,
        #     output_name = "vocabularies_tables_datatable", col_names =  col_names,
        #     editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        #     searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")
        # 
        #   # Create a proxy for datatatable
        #   r$sub_thesaurus_datatable_proxy <- DT::dataTableProxy("vocabularies_tables_datatable", deferUntilFlush = FALSE)
        # 
        #   # Reload datatable
        #   observeEvent(r$thesaurus_items_temp, {
        # 
        #     # Reload data of datatable
        #     DT::replaceData(r$sub_thesaurus_datatable_proxy, r$thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
        #   })
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$thesaurus_refresh_thesaurus_items"))
        # 
        # })

        # Reload cache

        thesaurus_reload_cache_prefix <- "thesaurus_reload_cache"
        thesaurus_reload_cache_react_variable <- "thesaurus_reload_cache_confirm"
        thesaurus_reload_cache_variable <- paste0(thesaurus_reload_cache_prefix, "_open_dialog")

        r[[thesaurus_reload_cache_variable]] <- FALSE

        # output[[thesaurus_reload_cache_react_variable]] <- shiny.fluent::renderReact({
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - output$thesaurus_reload_cache_confirm"))
        # 
        #   shiny.fluent::Dialog(
        #     hidden = !r[[thesaurus_reload_cache_variable]],
        #     onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", thesaurus_reload_cache_prefix, "_hide_dialog', Math.random()); }")),
        #     dialogContentProps = list(type = 0, title = i18n$t("thesaurus_reload_cache_text"), closeButtonAriaLabel = "Close", subText = tagList(i18n$t("thesaurus_reload_cache_subtext"), br(), br())),
        #     modalProps = list(),
        #     shiny.fluent::DialogFooter(
        #       shiny.fluent::PrimaryButton.shinyInput(ns(paste0(thesaurus_reload_cache_prefix, "_reload_cache_confirmed")), text = i18n$t("yes")),
        #       shiny.fluent::DefaultButton.shinyInput(ns(paste0(thesaurus_reload_cache_prefix, "_reload_cache_canceled")), text = i18n$t("no"))
        #     )
        #   )
        # })

        # Whether to close or not delete dialog box
        # observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_hide_dialog")]], {
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_hide_dialog"))
        #   r[[thesaurus_reload_cache_variable]] <- FALSE
        # })
        # observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_reload_cache_canceled")]], {
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_canceled"))
        #   r[[thesaurus_reload_cache_variable]] <- FALSE
        # })

        # observeEvent(input$reload_vocabularies_tables_cache, {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$reload_vocabularies_tables_cache"))
        # 
        #   req(length(input$vocabulary_tables_selected_vocabulary) > 1)
        #   r[[thesaurus_reload_cache_variable]] <- TRUE
        # })

        # observeEvent(input[[paste0(thesaurus_reload_cache_prefix, "_reload_cache_confirmed")]], {
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "start")
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_confirmed"))
        # 
        #   r[[thesaurus_reload_cache_variable]] <- FALSE
        # 
        #   sql <- glue::glue_sql("SELECT id FROM thesaurus_items WHERE thesaurus_id = {r$thesaurus_link_id}", .con = r$db)
        #   thesaurus_items <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
        # 
        #   sql <- glue::glue_sql(paste0("SELECT * FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
        #     "link_id IN ({thesaurus_items*})"), .con = r$db)
        # 
        #   # Delete cache
        #   sql <- glue::glue_sql(paste0("DELETE FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
        #     "link_id IN ({thesaurus_items*})"), .con = r$db)
        #   query <- DBI::dbSendStatement(r$db, sql)
        #   DBI::dbClearResult(query)
        # 
        #   sql <- glue::glue_sql(paste0("SELECT * FROM cache WHERE category IN ('count_items_rows', 'count_patients_rows') AND ",
        #     "link_id IN ({thesaurus_items*})"), .con = r$db)
        # 
        #   # Reset datamart dropdown
        #   data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == r$thesaurus_link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
        #   datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)
        # 
        #   options <- convert_tibble_to_list(datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        #   shiny.fluent::updateComboBox.shinyInput(session, "vocabularies_datamart", options = options, value = NULL)
        # 
        #   # Reload datatable
        #   r$thesaurus_refresh_thesaurus_items <- paste0(Sys.time(), "reset")
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$thesaurus_reload_cache_reload_cache_confirmed"))
        # })

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

        # observeEvent(input$thesaurus_items_deleted_pressed, {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$thesaurus_items_deleted_pressed"))
        # 
        #   r[[paste0(id, "_delete_thesaurus_items")]] <- as.integer(substr(input$thesaurus_items_deleted_pressed, nchar("sub_delete_") + 1, 100))
        #   r[[thesaurus_items_delete_variable]] <- TRUE
        # })

        # Delete multiple rows (with "Delete selection" button)

        # observeEvent(input$vocabularies_tables_delete_selection, {
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_delete_selection"))
        # 
        #   req(length(input$vocabularies_tables_datatable_rows_selected) > 0)
        # 
        #   r[[paste0(id, "_delete_thesaurus_items")]] <- r$thesaurus_items_temp[input$vocabularies_tables_datatable_rows_selected, ] %>% dplyr::pull(id)
        #   r[[thesaurus_items_delete_variable]] <- TRUE
        # })

        # observeEvent(r[[thesaurus_items_reload_variable]], {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$reload_thesaurus_items"))
        # 
        #   # Reload datatable
        #   r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
        # })
      }
      
      # --- --- --- --- --- -- -
      # Import a vocabulary ----
      # --- --- --- --- --- -- -
      
      if (table == "vocabulary"){
        
      }
  })
}
