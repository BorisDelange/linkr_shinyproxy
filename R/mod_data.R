#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_data_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  language <- "EN"
  
  # Prefix depending on page id
  if (id == "patient_level_data"){
    prefix <- "patient_lvl"
    page_name <- "patient_level_data"
  } 
  if (id == "aggregated_data"){
    prefix <- "aggregated"
    page_name <- "aggregated_data"
  }
  
  # --- --- --- --- --- --
  # Tab creation card ----
  # --- --- --- --- --- --
  
  tab_creation_options <- list(
    list(key = "same_level", text = i18n$t("same_level_current_tab")),
    list(key = "level_under", text = i18n$t("level_under"))
  )
  
  tab_creation_card <- make_card(
    title = i18n$t("add_tab"),
    content = div(
      actionButton(ns(paste0(prefix, "_close_add_tab")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
        make_textfield(ns = ns, label = "name", id = "tab_name", width = "300px", i18n = i18n),
        div(shiny.fluent::ChoiceGroup.shinyInput(ns("add_tab_type"), value = "same_level", 
          options = tab_creation_options, className = "inline_choicegroup"), style = "padding-top:35px;")
      ), br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("add_tab_button"), i18n$t("add")), br()
    )
  )
  
  # --- --- --- --- --- -
  # Tab edition card ----
  # --- --- --- --- --- -
  
  tab_edition_card <- make_card(
    title = i18n$t("edit_tab"),
    content = div(
      actionButton(ns(paste0(prefix, "_close_edit_tab")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
      make_textfield(ns = ns, label = "name", id = "edit_tab_name", width = "300px", i18n = i18n), br(),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_tab_save"), i18n$t("save")),
        shiny.fluent::DefaultButton.shinyInput(ns("remove_tab"), i18n$t("delete_tab"))
      ), 
      br()
    )
  )
  
  # --- --- --- --- --- --- -
  # Widget creation card ----
  # --- --- --- --- --- --- -
  
  if (prefix == "patient_lvl"){
    widget_creation_card <- make_card(
      title = i18n$t("add_a_widget"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_widget")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(i18n = i18n, ns = ns, label = "name", id = "widget_creation_name", width = "300px"),
          make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "widget_creation_plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px")
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_combobox(i18n = i18n, ns = ns, label = "thesaurus", id = "widget_creation_thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
          make_dropdown(i18n = i18n, ns = ns, label = "items_mapping", id = "widget_creation_thesaurus_mapping", multiSelect = TRUE, width = "300px",
            options = list(
              list(key = 1, text = i18n$t("equivalent_to")),
              list(key = 2, text = i18n$t("included_in")),
              list(key = 3, text = i18n$t("include"))
            )
          ),
          conditionalPanel(condition = "input.widget_creation_thesaurus_mapping != null & input.widget_creation_thesaurus_mapping != ''", ns = ns, 
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_creation_merge_mapped_items")), value = TRUE), style = "margin-top:45px;"),
                div(i18n$t("merge_mapped_items"), style = "font-weight:bold; margin-top:45px;")
              ),
              style = "margin-left:-28px;"
            )
          )
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          div(
            div(id = ns("widget_creation_thesaurus_selected_items_title"), class = "input_title", i18n$t("thesaurus_selected_items")),
            div(shiny.fluent::Dropdown.shinyInput(ns("widget_creation_thesaurus_selected_items"), value = NULL, options = list(), multiSelect = TRUE,
              onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-widget_creation_thesaurus_selected_items_trigger', Math.random())"))), style = "width:650px;")
          ),
          
          div(shiny.fluent::DefaultButton.shinyInput(ns("widget_creation_reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
        ),
        div(DT::DTOutput(ns("widget_creation_thesaurus_items")), class = "thesaurus_table"), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("add_widget_button"), i18n$t("add_widget")))
      )
    )
    
    widget_settings_card <- make_card(
      title = i18n$t("widget_settings"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_widget_settings")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(i18n = i18n, ns = ns, label = "name", id = "widget_settings_name", width = "300px"),
          make_textfield(i18n = i18n, ns = ns, label = "plugin", id = "widget_settings_plugin", width = "300px", disabled = TRUE)
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_combobox(i18n = i18n, ns = ns, label = "thesaurus", id = "widget_settings_thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
          make_dropdown(i18n = i18n, ns = ns, label = "items_mapping", id = "widget_settings_thesaurus_mapping", multiSelect = TRUE, width = "300px",
            options = list(
              list(key = 1, text = i18n$t("equivalent_to")),
              list(key = 2, text = i18n$t("included_in")),
              list(key = 3, text = i18n$t("include"))
            )
          ),
          conditionalPanel(condition = "input.widget_settings_thesaurus_mapping != null & input.widget_settings_thesaurus_mapping != ''", ns = ns, 
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_settings_merge_mapped_items")), value = TRUE), style = "margin-top:45px;"),
                div(i18n$t("merge_mapped_items"), style = "font-weight:bold; margin-top:45px;")
              ),
              style = "margin-left:-28px;"
            )
          )
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          div(
            div(id = ns("widget_settings_thesaurus_selected_items_title"), class = "input_title", i18n$t("thesaurus_selected_items")),
            div(shiny.fluent::Dropdown.shinyInput(ns("widget_settings_thesaurus_selected_items"), value = NULL, options = list(), multiSelect = TRUE,
              onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-widget_settings_thesaurus_selected_items_trigger', Math.random())"))), style = "width:650px;")
          ),
          
          div(shiny.fluent::DefaultButton.shinyInput(ns("widget_settings_reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
        ),
        div(DT::DTOutput(ns("widget_settings_thesaurus_items")), class = "thesaurus_table"), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("edit_widget_button"), i18n$t("save")))
      )
    )
    
  }
  
  if (prefix == "aggregated"){
    widget_creation_card <- make_card(
      title = i18n$t("add_a_widget"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_widget")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(i18n = i18n, ns = ns, label = "name", id = "widget_creation_name", width = "300px"),
          make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "widget_creation_plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px")
        ), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("add_widget_button"), i18n$t("add_widget")))
      )
    )
    
    widget_settings_card <- make_card(
      title = i18n$t("widget_settings"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_widget_settings")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(i18n = i18n, ns = ns, label = "name", id = "widget_settings_name", width = "300px"),
          make_textfield(i18n = i18n, ns = ns, label = "plugin", id = "widget_settings_plugin", width = "300px", disabled = TRUE)
        ), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("edit_widget_button"), i18n$t("save")))
      )
    )
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("tab_delete_confirm")), shiny.fluent::reactOutput(ns("widget_delete_confirm")),
    div(id = ns("initial_breadcrumb"),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "main", text = i18n$t(paste0(prefix, "_data")), href = paste0("#!/", page_name), isCurrentItem = TRUE)),
        maxDisplayedItems = 3)
    ),
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_study_and_dataset_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(uiOutput(ns("study_menu"))),
    div(id = ns("study_cards")),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_widget")),
        widget_creation_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_widget_settings")),
        widget_settings_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_tab")),
        tab_creation_card,
        style = "position:relative;"
      )
    ), 
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_edit_tab")),
        tab_edition_card,
        style = "position:relative;"
      )
    ),
    # div(shinyAce::aceEditor(
    #   ns("ace_edit_code"), "", mode = "r",
    #   code_hotkeys = list(
    #     "r", list(
    #       run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
    #       run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
    #       save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
    #     )
    #   ),
    #   autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
    # ), style = "width: 100%;"),
    # shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(),
    # div(verbatimTextOutput(ns("code_result")),
    #   style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
    br()
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 

mod_data_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  o = shiny::reactiveValues(), language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- --- -
    # Summary ----
    # --- --- --- --- --- --- -
    
    # - INITIATE VARS
    # - HELP ON THIS PAGE
    # - LOAD DATA
    # - INITIATE UI
    # - LOAD UI
    # --- LOAD UI TABS
    # --- RENDER TABS MENU
    # --- RENDER WIDGETS
    # - LOAD SERVER
    # - OTHER SERVER REACTIVITY
    # --- SORTABLE / CHANGE PIVOTITEMS ORDER
    # --- SHOW / HIDE DIV WHEN PIVOT ITEM SELECTED
    # --- ADD A TAB
    # --- DELETE A TAB
    # --- ADD A WIDGET
    # --- DELETE A WIDGET
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - show_message_bars"))
    
    # --- --- --- --- --
    # Initiate vars ----
    # --- --- --- --- --
    
    if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - initiate vars"))
    
    # Prefix depending on page id
    if (id == "patient_level_data"){
      prefix <- "patient_lvl"
      page_name <- "patient_level_data"
    } 
    if (id == "aggregated_data"){
      prefix <- "aggregated"
      page_name <- "aggregated_data"
    }
    
    # Initiale variables for help panel & help modal
    
    r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE
    r[[paste0("help_data_", prefix, "_modal_text")]] <- div()
    r[[paste0("help_data_", prefix, "_modal_title")]] <- ""
    
    # Initiate var for already loaded studies, so that a UI element is not loaded twice
    r[[paste0(prefix, "_loaded_studies")]] <- integer()
    
    # Initiate var for list of cards
    r[[paste0(prefix, "_cards")]] <- character()
    
    # Load page from header
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer shiny_router::change_page"))
      
      if (prefix == "aggregated" & shiny.router::get_page() == "data" & r$data_page == "patient_level_data") shiny.router::change_page("patient_level_data")
      else if (prefix == "patient_lvl" & shiny.router::get_page() == "data" & r$data_page == "aggregated_data") shiny.router::change_page("aggregated_data")
      
      # Close help pages when page changes
      r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE
      r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE
      
      # Refresh reactivity
      shinyjs::hide("study_cards")
      shinyjs::show("study_cards")
    })
    
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - initiate vars"))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---

    observeEvent(input$help, if (id == shiny.router::get_page()) r[[paste0("help_data_", prefix, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_data_", prefix, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE
      r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_data_", prefix, "_page_", i)]] <- Sys.time())
    })
    
    help_data(output = output, r = r, id = id, prefix = prefix, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    if (prefix == "patient_lvl"){
      
      observeEvent(m$selected_person, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer m$selected_person"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # Reset variables
        
        visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
        person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
        
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_person) > 0) for(table in person_tables) if (nrow(d$data_subset[[table]]) > 0) d$data_person[[table]] <- 
          d$data_subset[[table]] %>% dplyr::filter(person_id == m$selected_person)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_person"))
      })
      
      observeEvent(m$selected_visit_detail, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer m$selected_visit_detail"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        req(d$data_person)
        
        visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_visit_detail) > 0) for(table in visit_detail_tables) if (nrow(d$data_person[[table]]) > 0) d$data_visit_detail[[table]] <- 
          d$data_person[[table]] %>% dplyr::filter(visit_detail_id == m$selected_visit_detail)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_visit_detail"))
      })
    }
    
    if (prefix == "aggregated"){
      
      observeEvent(m$subset_persons, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer m$subset_persons"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # Reset variables
        
        subset_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
          "drug_era", "dose_era", "condition_era", "person", "observation_period", "visit_occurrence", "visit_detail")
        for(table in subset_tables) d$data_subset[[table]] <- tibble::tibble()
        
        if (nrow(m$subset_persons) > 0) for(table in subset_tables) if (nrow(d[[table]]) > 0) d$data_subset[[table]] <- 
          d[[table]] %>% dplyr::inner_join(m$subset_persons %>% dplyr::select(person_id), by = "person_id")
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$subset_persons"))
      })
    }
    
    # --- --- --- -- -
    # Initiate UI ----
    # --- --- --- -- -
    
    # When a study is selected
    
    observeEvent(m$selected_study, {
      
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer m$selected_study"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      # Hide UI from previous loaded study
      # Don't use removeUI, cause when you switch study, it is deleted and cannot be reinserted
      
      sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
      
      req(!is.na(m$selected_study))
      
      # removeUI(selector = paste0("#", ns(r[[paste0(prefix, "_cards")]])), multiple = TRUE)

      # Initiiate vector of study cards
      # r[[paste0(prefix, "_cards")]] <- character()

      # Hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::hide("initial_breadcrumb")
      shinyjs::show("study_menu")

      # Reset selected key
      r[[paste0(prefix, "_selected_tab")]] <- NA_integer_
      
      # Reset shown tabs
      r[[paste0(prefix, "_opened_cards")]] <- ""

      # Hide Add widget card & Add tab
      shinyjs::hide("add_widget")
      shinyjs::hide("add_tab")

      r[[paste0(prefix, "_load_display_tabs")]] <- paste0("first_load_ui_", Sys.time())

      # Run observers
      r[[paste0(prefix, "_load_server")]] <- Sys.time()
      
      r[[paste0(prefix, "_load_ui_stage")]] <- "first_time"

      # Load tabs variables for this study
      update_r(r = r, m = m, table = paste0(prefix, "_tabs_groups"))
      update_r(r = r, m = m, table = paste0(prefix, "_tabs"))
      update_r(r = r, m = m, table = paste0(prefix, "_widgets"))
      update_r(r = r, m = m, table = paste0(prefix, "_widgets_items"))
      
      # Reload create widget fields
      
      ## Reload thesaurus datatable & selected_items
      if (length(r$widget_creation_thesaurus_items) > 0){
        r$widget_creation_thesaurus_items <- r$widget_creation_thesaurus_items %>% dplyr::slice(0)
        r$widget_creation_thesaurus_items_temp <- r$widget_creation_thesaurus_items %>% dplyr::mutate(modified = FALSE)
      }
      if (length(r$widget_creation_thesaurus_selected_items) > 0) r$widget_creation_thesaurus_selected_items <-
        r$widget_creation_thesaurus_selected_items %>% dplyr::slice(0)
      shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_thesaurus_selected_items", options = list(), value = NULL)
      
      ## Reload other fields
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_thesaurus_mapping", 
        options = list(list(key = 1, text = i18n$t("equivalent_to")), list(key = 2, text = i18n$t("included_in")), list(key = 3, text = i18n$t("include"))),
        value = NULL)
      r[[paste0(prefix, "_reload_thesaurus_dropdown")]] <- Sys.time()
      r[[paste0(prefix, "_reload_plugins_dropdown")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_study"))
    })
    
    # Reload thesaurus dropdown
    observeEvent(r[[paste0(prefix, "_reload_thesaurus_dropdown")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..reload_thesaurus_dropdown"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id) %>% as.character()
      thesaurus <- r$thesaurus %>% 
        dplyr::filter(
          grepl(paste0("^", data_source, "$"), data_source_id) | 
            grepl(paste0(", ", data_source, "$"), data_source_id) | 
            grepl(paste0("^", data_source, ","), data_source_id) |
            grepl(paste0(", ", data_source, ","), data_source_id)
        ) %>% dplyr::arrange(name)
      
      sapply(c("widget_creation_thesaurus", "widget_settings_thesaurus"), function(name) shiny.fluent::updateComboBox.shinyInput(session, name, 
        options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", i18n = i18n), value = NULL))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..reload_thesaurus_dropdown"))
    })
    
    # Reload plugins dropdown
    
    observeEvent(r$plugin, {
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$plugin"))
      r[[paste0(prefix, "_reload_plugins_dropdown")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_reload_plugins_dropdown")]], {
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..reload_plugins_dropdown"))

      tab_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
      
      plugins <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id)
      
      options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", i18n = i18n)
      shiny.fluent::updateComboBox.shinyInput(session, "widget_creation_plugin", options = options, value = NULL)
    })
    
    # Load study display tabs
    
    observeEvent(r[[paste0(prefix, "_load_display_tabs")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..load_display_tabs"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      # Load study informations
      # For one study, you choose ONE patient_lvl or aggregated data tab family
      study_infos <- r$studies %>% dplyr::filter(id == m$selected_study)
      # study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", m$selected_study))
      
      # Check if users has access only to aggregated data
      r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "show_only_aggregated_data") %>%
        dplyr::pull(value_num) -> r[[paste0(prefix, "_show_only_aggregated_data")]]
      
      # Load tabs belonging to this tab family
      # display_tabs <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, "_tabs WHERE tab_group_id = ",
      #   study_infos[[paste0(prefix, "_tab_group_id")]], " AND deleted IS FALSE"))
      display_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(tab_group_id == study_infos[[paste0(prefix, "_tab_group_id")]]) 
      
      # Tabs without parent are set to level 1
      display_tabs <- display_tabs %>% 
        dplyr::mutate(level = dplyr::case_when(is.na(parent_tab_id) ~ 1L, TRUE ~ NA_integer_))
      
      # Prevent infinite loop, max loops = 7
      i <- 1
      
      # Creating levels for distinct tabs
      while(nrow(display_tabs %>% dplyr::filter(is.na(level))) > 0 & i <= 7){
        display_tabs <-
          display_tabs %>%
          dplyr::left_join(display_tabs %>%
            dplyr::filter(!is.na(level)) %>%
            dplyr::transmute(parent_tab_id = id, parent_level = level), by = "parent_tab_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
        i <- i + 1
      }
      
      # Exclude tabs without level
      display_tabs <- display_tabs %>% dplyr::filter(!is.na(level))
      
      # Order by display order
      display_tabs <- display_tabs %>% dplyr::arrange(level, display_order)
      
      # Calculate first tab shown in the menu
      if(nrow(display_tabs) > 0 & "level" %in% names(display_tabs) & !is.na(m$selected_study)){
        
        # First tab shown
        first_tab_shown <- display_tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
        if (max(display_tabs$level) >= 2){
          sapply(2:max(display_tabs$level), function(current_level){
            children <- display_tabs %>% dplyr::filter(level == current_level, parent_tab_id == first_tab_shown$id) %>% dplyr::slice(1)
            if (nrow(children) > 0) first_tab_shown <<- children
          })
        }
        
        r[[paste0(prefix, "_first_tab_shown")]] <- first_tab_shown
      }
      
      r[[paste0(prefix, "_display_tabs")]] <- display_tabs
      
      # Load UI cards
      if (grepl("first_load_ui", r[[paste0(prefix, "_load_display_tabs")]])) r[[paste0(prefix, "_load_ui_cards")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_display_tabs"))
    })
    
    # --- --- -- -
    # Load UI ----
    # --- --- -- -
    
      # --- --- --- --- --- --
      ## Render tabs menu ----
      # --- --- --- --- --- --
      
      # Render menu
      output$study_menu <- renderUI({
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - output$study_menu"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # The output is reloaded in these conditions :
        # - Change in r[[paste0(prefix, "_display_tabs")]]
        # - Change in r[[paste0(prefix, "_load_ui_menu")]]
        # - Change in input$study_current_tab

        req(!is.na(m$selected_study))
        req(r[[paste0(prefix, "_display_tabs")]])
        r[[paste0(prefix, "_load_ui_menu")]]
        
        # Hide initial breadcrumb
        shinyjs::hide("initial_breadcrumb")
        
        # Check if users has access only to aggregated data
        if (prefix == "patient_lvl" & isolate(r[[paste0(prefix, "_show_only_aggregated_data")]]) == 1) show_message_bar(output, "only_aggregated_data_authorized", "severeWarning", i18n = i18n, ns = ns)
        req((prefix == "patient_lvl" & isolate(r[[paste0(prefix, "_show_only_aggregated_data")]]) != 1) | prefix == "aggregated")
        
        display_tabs <- isolate(r[[paste0(prefix, "_display_tabs")]])
        
        # If no tab to show, notify user
        if (nrow(display_tabs) == 0 | "level" %not_in% names(display_tabs)){
          
          # selected_key <- 0L
          # if (!is.na(isolate(r[[paste0(prefix, "_selected_tab")]]))) selected_key <- isolate(r[[paste0(prefix, "_selected_tab")]])
          
          return(tagList(
            shiny.fluent::Breadcrumb(items = list(
              list(key = "main", text = i18n$t(paste0(prefix, "_data")), href = paste0("#!/", page_name), isCurrentItem = TRUE,
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', 0)")))),
              maxDisplayedItems = 3),
            shiny.fluent::Pivot(
              onLinkClick = htmlwidgets::JS(paste0("item => {",
                "Shiny.setInputValue('", id, "-study_current_tab', item.props.id);",
                "Shiny.setInputValue('", id, "-study_current_tab_trigger', Math.random());",
                "}"
                )),
              selectedKey = NULL,
              shiny.fluent::PivotItem(id = paste0(prefix, "_add_tab_", 0), headerText = span(i18n$t("add_tab"), style = "padding-left:5px;"), itemIcon = "Add")
            )
          ))
        }

        req(nrow(display_tabs) > 0 & "level" %in% names(display_tabs) & !is.na(isolate(m$selected_study)))
        
        # First tab shown
        first_tab_shown <- isolate(r[[paste0(prefix, "_first_tab_shown")]])
        
        shown_tabs_temp <- tibble::tibble()
        
        # If we are at level one, show all levels one
        if (first_tab_shown$level == 1){
          shown_tabs <- display_tabs %>% dplyr::filter(level == 1)
        }
        
        # Else, show only current & those who has same level & same parent
        if (first_tab_shown$level > 1){
          shown_tabs_temp <- display_tabs %>% dplyr::filter(level == first_tab_shown$level & parent_tab_id == first_tab_shown$parent_tab_id)
          if (nrow(shown_tabs_temp) > 0) shown_tabs <- shown_tabs_temp
          if (nrow(shown_tabs_temp) == 0) shown_tabs <- first_tab_shown
        }

        if (length(input$study_current_tab) > 0){

          # If value = 0, go back to first level
          if (input$study_current_tab == 0){
            shown_tabs <- display_tabs %>% dplyr::filter(level == 1)
            r[[paste0(prefix, "_selected_tab")]] <- first_tab_shown$id
          } 
          else {

            if (grepl("show_tab", isolate(r[[paste0(prefix, "_selected_tab")]]))){
              study_current_tab <- as.integer(substr(isolate(r[[paste0(prefix, "_selected_tab")]]), nchar("show_tab_") + 1, 100))
              shown_tabs_temp <- display_tabs %>% dplyr::filter(parent_tab_id == study_current_tab)
            }
            else if (grepl("add_tab", input$study_current_tab)){
              shown_tabs_temp <- tibble::tibble()
              study_current_tab <- isolate(r[[paste0(prefix, "_selected_tab")]])
            }
            else {
              study_current_tab <- input$study_current_tab
              shown_tabs_temp <- display_tabs %>% dplyr::filter(parent_tab_id == study_current_tab)
            }

            # If current tab has children
            if (nrow(shown_tabs_temp) > 0) shown_tabs <- shown_tabs_temp
            
            # If current tab has no children
            if (nrow(shown_tabs_temp) == 0){
              current_tab <- display_tabs %>% dplyr::filter(id == study_current_tab)
              if (nrow(current_tab) > 0) shown_tabs <- display_tabs %>% dplyr::filter(parent_tab_id == current_tab$parent_tab_id & level == current_tab$level)
              else shown_tabs <- tibble::tibble()
              
              # If not any "brother", we are at level one
              if (nrow(shown_tabs) == 0){
                shown_tabs <- display_tabs %>% dplyr::filter(level == 1)
              }
            }
          }
        }
        
        # Currently selected tab

        print(isolate(r[[paste0(prefix, "_selected_tab")]]))
        print(isolate(r[[paste0(prefix, "_tabs")]]))
        print(shown_tabs)
        
        # We have just deleted a tab
        if (grepl("show_tab", isolate(r[[paste0(prefix, "_selected_tab")]]))){
          r[[paste0(prefix, "_selected_tab")]] <- 
            as.integer(substr(isolate(r[[paste0(prefix, "_selected_tab")]]), nchar("show_tab_") + 1, 100))
        }
        
        # First existing tab or load another study
        else if (length(input$study_current_tab) == 0 | grepl("first_time", isolate(r[[paste0(prefix, "_load_ui_stage")]])) |
            isolate(r[[paste0(prefix, "_selected_tab")]]) %not_in% isolate(r[[paste0(prefix, "_tabs")]] %>% dplyr::pull(id))){
          r[[paste0(prefix, "_selected_tab")]] <- shown_tabs %>% dplyr::slice(1) %>% dplyr::pull(id)
        }
        
        # We have clicked on a tab
        else if (length(input$study_current_tab) > 0){
          
          # Current tab has children, take the first of this level of tabs
          if (nrow(shown_tabs_temp) > 0) r[[paste0(prefix, "_selected_tab")]] <- shown_tabs %>% dplyr::slice(1) %>% dplyr::pull(id)
          
          # Take the input as current tab
          else if (!grepl("add_tab", input$study_current_tab) & input$study_current_tab != 0) r[[paste0(prefix, "_selected_tab")]] <- input$study_current_tab
        }

        nb_levels <- max(shown_tabs$level)
        
        # First level
        is_current_item <- FALSE
        if (nb_levels == 1) is_current_item <- TRUE
        
        items <- list(
          list(key = "main", text = i18n$t(page_name), href = paste0("#!/", page_name), isCurrentItem = is_current_item,
            onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', 0)"))))
        
        # Other levels
        if (nb_levels >= 2){
          
          # Remove last level
          tabs_tree <- display_tabs %>% dplyr::filter(level < nb_levels)
          
          current_parent <- NA_integer_
          sapply(nb_levels:1, function(current_level){
            if (!is.na(current_parent)){
              tabs_tree <<- tabs_tree %>% dplyr::filter(level != current_level | id == current_parent)
              current_parent <<- display_tabs %>% dplyr::filter(id == current_parent) %>% dplyr::pull(parent_tab_id)
            }
            if (is.na(current_parent)) current_parent <<- shown_tabs %>% dplyr::slice(1) %>% dplyr::pull(parent_tab_id)
          })
          tabs_tree <- tabs_tree %>% dplyr::arrange(level)
          sapply(1:nrow(tabs_tree), function(i){
            is_current_item <- FALSE
            if (tabs_tree[[i, "level"]] == nb_levels) is_current_item <- TRUE
            items <<- rlist::list.append(items, list(
              key = tabs_tree[[i, "name"]], text = tabs_tree[[i, "name"]], href = paste0("#!/", page_name), isCurrentItem = is_current_item,
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', ", tabs_tree[[i, "id"]], ")"))
            ))
          })
        }

        shown_tabs_final <- tagList()
        
        sapply(1:nrow(shown_tabs), function(i){
          shown_tabs_final <<- tagList(shown_tabs_final, shiny.fluent::PivotItem(id = shown_tabs[[i, "id"]], itemKey = shown_tabs[[i, "id"]], headerText = shown_tabs[[i, "name"]]))
        })
        
        # Add an add button, to add a new tab
        shown_tabs_final <- tagList(shown_tabs_final, shiny.fluent::PivotItem(id = paste0(prefix, "_add_tab_", itemKet = isolate(r[[paste0(prefix, "_selected_tab")]])), headerText = span(i18n$t("add_tab"), style = "padding-left:5px;"), itemIcon = "Add"))
        
        r[[paste0(prefix, "_load_ui_stage")]] <- "end_loading_tabs_menu"
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - output$study_menu"))

        tagList(
          shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3),
          shiny.fluent::Pivot(
            id = paste0(prefix, "_study_pivot"),
            onLinkClick = htmlwidgets::JS(paste0("item => {",
              "Shiny.setInputValue('", id, "-study_current_tab', item.props.id);",
              "Shiny.setInputValue('", id, "-study_current_tab_trigger', Math.random());",
              "}"
            )),
            selectedKey = isolate(r[[paste0(prefix, "_selected_tab")]]),
            shown_tabs_final
          ),
          # A script to use sortable with PivotItems
          tags$script(paste0('$("#', prefix, '_study_pivot").children().first().attr("id", "', prefix, '_pivot_tabs");')),
          sortable::sortable_js(paste0(prefix, "_pivot_tabs"), options = sortable::sortable_options(onUpdate = htmlwidgets::JS(paste0("function(evt) { Shiny.setInputValue('", id, "-study_pivot_order', evt.from.innerText);}"))))
        )
        
      })
    
      # --- --- --- --- -- -
      ## Render widgets ----
      # --- --- --- --- -- -
    
      observeEvent(r[[paste0(prefix, "_load_ui_cards")]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..load_ui_cards"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # Don't reload study UI if already loaded
        req(m$selected_study %not_in% r[[paste0(prefix, "_loaded_studies")]])
        
        distinct_tabs <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::pull(id)

        code_ui <- tagList("")

        all_groups <- NA_integer_

        # Loop over distinct tabs, for this study

        selected_tab <- r[[paste0(prefix, "_selected_tab")]]

        if (grepl("show_tab", selected_tab)) selected_tab <- as.integer(substr(selected_tab, nchar("show_tab_") + 1, 100))

        sapply(distinct_tabs, function(tab_id){

          toggles <- tagList()

          widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% 
            dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)

          if (nrow(widgets) > 0){

            # Get widget group_id
            distinct_groups <- unique(widgets$group_id)

            # Loop over distinct cards (tabs elements), for this tab

            sapply(distinct_groups, function(group_id){

              # if (tab_id != r[[paste0(prefix, "_first_tab_shown")]]$id) all_groups <- c(all_groups, group_id)

              # Load UI code for this widget
              plugin_id <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              # if (length(plugin_id) != 0) code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)

              # Check if plugin has been deleted
              # check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id)) %>% dplyr::pull(deleted)
              check_deleted_plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
              if (nrow(check_deleted_plugin) == 0){
                code_ui_card <- paste0("div(shiny.fluent::MessageBar('", i18n$t("plugin_deleted"), "', messageBarType = 3), style = 'margin-top:10px;')")
                settings_widget_button <- ""
              }
              if (nrow(check_deleted_plugin) > 0){
                
                code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
                settings_widget_button <- actionButton(ns(paste0(prefix, "_settings_widget_", group_id)), "", icon = icon("cog"))
                
                # Create translations file & var
                
                i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
                
                plugin_translations <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
                
                if (plugin_translations != ""){
                  
                  tryCatch({
                    # Get plugin unique_id
                    plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == plugin_id) %>% dplyr::pull(value)
                    
                    # Create plugin folder in translations folder if doesn't exist
                    new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
                    if (!dir.exists(new_dir)) dir.create(new_dir)
                    
                    new_file <- paste0(new_dir, "/plugin_translations.csv")
                    if (!file.exists(new_file)) writeLines(plugin_translations, new_file)
                  },
                    error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
                      error_name = paste0(id, " - create translations files - plugin_id ", plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
                  
                  tryCatch({
                    i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
                    i18np$set_translation_language(language)},
                    error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
                      error_name = paste0(id, " - create i18np translator - plugin_id ", plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
                }
              }

              # Get name of widget
              widget_name <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)

              # Append a toggle to our cards list
              r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_group_", group_id))

              toggles <<- tagList(toggles,
                shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
                div(class = "toggle_title", widget_name, style = "padding-top:10px;"))

              # Try to run plugin UI code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                code_ui_card <- code_ui_card %>%
                  stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
                  stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("\r", "\n")

                if (length(m$selected_study) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))

                # Widget card

                element_code <- div(
                  make_card("",
                    div(
                      div(id = ns(paste0(prefix, "_widget_plugin_ui_", group_id)), eval(parse(text = code_ui_card))),
                      div(
                        id = ns(paste0(prefix, "_widget_settings_remove_buttons_", group_id)),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                          settings_widget_button,
                          actionButton(ns(paste0(prefix, "_remove_widget_", group_id)), "", icon = icon("trash-alt"))
                        ),
                        style = "position:absolute; top:8px; right: 10px;"
                      )
                    ),
                    style = "position:relative;"
                  )
                )

                ui_output <- uiOutput(ns(paste0(prefix, "_group_", group_id)))
                hide_div <- TRUE
                if (!is.na(selected_tab)) if (tab_id == selected_tab) hide_div <- FALSE
                if (hide_div) ui_output <- shinyjs::hidden(ui_output)

                insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = ui_output)
                output[[paste0(prefix, "_group_", group_id)]] <- renderUI(element_code)
              },
              error = function(e){
                report_bug(r = r, output = output, error_message = i18n$t("error_run_plugin_ui_code"),
                  error_name = paste0(id, " - run ui code - ", group_id), category = "Error", error_report = e, i18n = i18n, ns = ns)
              })
            })
          }

          # Put all div together

          r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_toggles_", tab_id))

          # Does this tab have sub-tabs ?
          if (r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == tab_id) %>% nrow() > 0) toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
                div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
              )
            )
          )

          else toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
                div(style = "width:20px;"),
                toggles
              )
            )
          )

          ui_output <- uiOutput(ns(paste0(prefix, "_toggles_", tab_id)))
          hide_div <- TRUE
          if (!is.na(selected_tab)) if (tab_id == selected_tab) hide_div <- FALSE
          if (hide_div) ui_output <- shinyjs::hidden(ui_output)

          insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = ui_output)
          output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)

        })
        
        # Indicate that this study has been loaded, so that UI elements aren't loaded twice
        r[[paste0(prefix, "_loaded_studies")]] <- c(r[[paste0(prefix, "_loaded_studies")]], m$selected_study)

        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(100, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_ui_cards"))
      })
    
    # --- --- --- --- --- ---
    # Close creation div ----
    # --- --- --- --- --- ---
    
    observeEvent(input[[paste0(prefix, "_close_add_widget")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..close_add_widget"))

      # Show opened cards before opening Add widget div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)

      # Hide Add widget div
      shinyjs::hide(paste0(prefix, "_add_widget"))
    })
    
    # --- --- --- -- -
    # Load server ----
    # --- --- --- -- -
    
    observeEvent(r[[paste0(prefix, "_load_server")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..load_server"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")

      req(!is.na(m$selected_study))

      # Get tabs elements, arrange them by display_order

      tab_group <- r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(paste0(prefix, "_tab_group_id"))
      tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(tab_group_id == tab_group) %>% dplyr::select(tab_id = id)
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::inner_join(tabs, by = "tab_id") %>% dplyr::rename(group_id = id)
      widgets_items <- r[[paste0(prefix, "_widgets_items")]] %>% dplyr::inner_join(widgets %>% dplyr::select(group_id), by = "group_id")

      # --- --- --- --- --- --- --- ---
      ## Run server code for cards ----
      # --- --- --- --- --- --- --- ---

      # If no thesaurus elements to show in this tab, notify the user
      # if (nrow(widgets) == 0) show_message_bar(output, message = "no_widget_to_show", type = "severeWarning", language = language)

      if (nrow(widgets) > 0){

        # Get widget group_id
        distinct_groups <- unique(widgets$group_id)

        toggles <- c()

        # Loop over distinct cards
        sapply(distinct_groups, function(group_id){

          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(prefix, "_", group_id, "_", m$selected_study)
          # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
          if (trace_code %not_in% r$server_tabs_groups_loaded){

            # Add the trace_code to loaded plugins list
            r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)

            # Server code for toggles reactivity
            toggle <- paste0(prefix, "_group_", group_id)
            observeEvent(input[[paste0(toggle, "_toggle")]], {
              req(r[[paste0(prefix, "_selected_tab")]] == widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::distinct(tab_id) %>% dplyr::pull())
              if (input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle)
            })

            # Get name of widget
            # widget_name_escaping <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>%
            # dplyr::pull(name) %>% stringr::str_replace_all(c("-" = "_", "/" = "_", "\\(" = "_", "\\)" = "_"))

            # toggles <<- c(toggles, paste0(prefix, "_group_", group_id))

            thesaurus_selected_items <- tibble::tibble()
            
            if (prefix == "patient_lvl"){
              
              thesaurus_selected_items <- widgets_items %>% dplyr::filter(group_id == !!group_id) %>%
                dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                  thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
            }

            # Get plugin code

            ids <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)

            # Check if plugin has been deleted
            check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", ids$plugin_id)) %>% dplyr::pull(deleted)
            if (!check_deleted_plugin){

              code_server_card <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
                dplyr::pull(code) %>%
                stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
                stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
                stringr::str_replace_all("\r", "\n")

              # If it is an aggregated plugin, change %study_id% with current selected study
              if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
            }
            else code_server_card <- ""
            
            # Create translations file & var
            if (!check_deleted_plugin){
              
              i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
              
              plugin_translations <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>%
                dplyr::pull(code)
              
              if (plugin_translations != ""){
                
                tryCatch({
                  # Get plugin unique_id
                  plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == !!ids$plugin_id) %>% dplyr::pull(value)
                  
                  # Create plugin folder in translations folder if doesn't exist
                  new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
                  if (!dir.exists(new_dir)) dir.create(new_dir)
                  
                  new_file <- paste0(new_dir, "/plugin_translations.csv")
                  if (!file.exists(new_file)) writeLines(plugin_translations, new_file)
                },
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
                    error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
                
                tryCatch({
                  i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
                  i18np$set_translation_language(language)},
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
                    error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
              }
            }

            # Create a session number, to inactivate older observers
            # Reset all older observers for this group_id
            
            session_code <- paste0("tab_", ids$tab_id, "_group_", group_id)
            if (length(o[[session_code]]) == 0) session_num <- 1L
            if (length(o[[session_code]]) > 0) session_num <- o[[session_code]] + 1
            o[[session_code]] <- session_num
            
            # NB : req(o[[session_code]] == session_num) must be put at the beginning of each observeEvent in plugins code
            
            # Variables to hide
            new_env_vars <- list("r" = NA)
            # Variables to keep
            for (var in c("d", "m", "o", "thesaurus_selected_items", "session_code", "session_num")) new_env_vars[[var]] <- eval(parse(text = var))
            new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
            tryCatch(eval(parse(text = code_server_card), envir = new_env), error = function(e) print(e), warning = function(w) print(w))
            
            # --- --- --- --- --- ---
            #### Delete a widget ----
            # --- --- --- --- --- ---

            observeEvent(input[[paste0(prefix, "_remove_widget_", group_id)]], {
              r[[paste0(prefix, "_selected_widget")]] <- group_id
              r[[widget_delete_variable]] <- TRUE
            })
            
            # --- --- --- --- --- ---
            #### Widget settings ----
            # --- --- --- --- --- ---

            observeEvent(input[[paste0(prefix, "_settings_widget_", group_id)]], {
              r[[paste0(prefix, "_settings_widget_trigger")]] <- Sys.time()
              r[[paste0(prefix, "_settings_widget")]] <- group_id
            })
          }
        })
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_server"))
    })
    
    # --- --- --- --- --- --- -- -
    # Other server reactivity ----
    # --- --- --- --- --- --- -- -
    
      # --- --- --- --- -- -
      ## Widget settings ----
      # --- --- --- --- -- -
    
      observeEvent(r[[paste0(prefix, "_settings_widget_trigger")]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..settings_widget_trigger"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::show(paste0(prefix, "_widget_settings"))
        r[[paste0(prefix, "_widget_card_selected_type")]] <- "widget_settings"
        
        widget_infos <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id == r[[paste0(prefix, "_settings_widget")]])
        req(nrow(widget_infos) > 0)
        
        # Update thesaurus combobox
        r[[paste0(prefix, "_reload_thesaurus_dropdown")]] <- Sys.time()
        
        # Update name & plugin textfields
        
        widget_plugin_infos <- r$plugins %>% dplyr::filter(id == widget_infos$plugin_id)
        
        shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_name", value = widget_infos$name)
        shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_plugin", value = widget_plugin_infos$name)
        
        # Reset datatable
        r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_items")
        if (length(r[[r_var]]) > 0){
          r[[r_var]] <- r[[r_var]] %>% dplyr::slice(0)
          r[[paste0(r_var, "_temp")]] <- r[[paste0(r_var, "_temp")]] %>% dplyr::slice(0)
        }
        
        # Get selected_items for this widget
        
        if (nrow(r[[paste0(prefix, "_widgets_items")]] %>%
            dplyr::filter(group_id == r[[paste0(prefix, "_settings_widget")]])) > 0){
          
          r$widget_settings_thesaurus_selected_items <- r[[paste0(prefix, "_widgets_items")]] %>%
            dplyr::filter(group_id == r[[paste0(prefix, "_settings_widget")]]) %>%
            dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name") %>%
            dplyr::select(id = db_item_id, thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit,
              thesaurus_item_colour, mapped_to_item_id, merge_items) %>%
            dplyr::left_join(
              r[[paste0(prefix, "_widgets_items")]] %>% dplyr::select(mapped_to_item_id = id, new_mapped_to_item_id = db_item_id),
              by = "mapped_to_item_id"
            ) %>%
            dplyr::relocate(new_mapped_to_item_id, .after = "mapped_to_item_id") %>%
            dplyr::select(-mapped_to_item_id) %>%
            dplyr::rename(mapped_to_item_id = new_mapped_to_item_id) %>%
            dplyr::mutate(input_text = dplyr::case_when(
              !is.na(mapped_to_item_id) ~ paste0("--- ", thesaurus_name, " - ", thesaurus_item_display_name, " (", tolower(i18n$t("mapped_item")), ")"),
              TRUE ~ paste0(thesaurus_name, " - ", thesaurus_item_display_name),
            ), .after = "thesaurus_item_colour") %>%
            tibble::as_tibble() %>%
            dplyr::mutate_at("merge_items", as.logical)
          
          # Manque id que l'on doit récupérer dans thesaurus_items
          
          options <- convert_tibble_to_list(r$widget_settings_thesaurus_selected_items, key_col = "id", text_col = "input_text", i18n = i18n)
          value <- r$widget_settings_thesaurus_selected_items %>% dplyr::pull(id)
          shiny.fluent::updateDropdown.shinyInput(session, "widget_settings_thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..settings_widget_trigger"))
      })
    
      observeEvent(input$widget_settings_thesaurus, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_thesaurus"))
        r[[paste0(prefix, "_load_thesaurus_trigger")]] <- Sys.time()
      })
      
      # When an item is selected
      observeEvent(input$widget_settings_item_selected, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_item_selected"))
        r[[paste0(prefix, "_item_selected_trigger")]] <- Sys.time()
      })
      
      # When reset button is clicked
      observeEvent(input$widget_settings_reset_thesaurus_items, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_reset_thesaurus_items"))
        r[[paste0(prefix, "_reset_thesaurus_items_trigger")]] <- Sys.time()
      })
      
      # When dropdown is modified
      observeEvent(input$widget_settings_thesaurus_selected_items_trigger, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_thesaurus_selected_items_trigger"))
        r[[paste0(prefix, "_thesaurus_selected_items_trigger")]] <- Sys.time()
      })
      
      # Reload datatable
      observeEvent(r$widget_settings_thesaurus_items_temp, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$widget_settings_thesaurus_items_temp"))
        r[[paste0(prefix, "_reload_datatable_trigger")]] <- Sys.time()
      })
      
      # Updates in datatable
      
      observeEvent(input$widget_settings_thesaurus_items_cell_edit, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_thesaurus_items_cell_edit"))
        r[[paste0(prefix, "_edit_datatable_trigger")]] <- Sys.time()
      })
      
      # Close button clicked
      observeEvent(input[[paste0(prefix, "_close_widget_settings")]], {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..close_widget_settings"))
        shinyjs::hide(paste0(prefix, "_widget_settings"))
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      })
      
      # Save updates
      observeEvent(input$edit_widget_button, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..edit_widget_button"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        new_data <- list()
        
        new_data$name <- coalesce2(type = "char", x = input$widget_settings_name)
        
        group_id <- r[[paste0(prefix, "_settings_widget")]]
        ids <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id == group_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
        
        # Check if name is not empty
        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = i18n$t("provide_valid_name"))
        else shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = NULL)
        req(!is.na(new_data$name))
        
        # Check if values required to be unique are unique
        
        table <- paste0(prefix, "_widgets")
        
        sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND tab_id = {ids$tab_id} AND id != {group_id}", .con = r$db)
        distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        if (new_data$name %in% distinct_values) show_message_bar(output,  "name_already_used", "severeWarning", i18n = i18n, ns = ns)
        req(new_data$name %not_in% distinct_values)
        
        # Update name in database & r var
        r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>%
          dplyr::mutate(name = dplyr::case_when(
            id == group_id ~ new_data$name,
            TRUE ~ name
          ))
        sql <- glue::glue_sql("UPDATE {`table`} SET name = {new_data$name} WHERE id = {group_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        # Get last_row nb
        last_row_widgets_items <- get_last_row(r$db, paste0(prefix, "_widgets_items"))
        
        has_thesaurus_items <- TRUE
        thesaurus_selected_items <- tibble::tibble()
        
        if (prefix == "patient_lvl"){
          
          if (length(r$widget_settings_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          if (length(r$widget_settings_thesaurus_selected_items) > 0) if (nrow(r$widget_settings_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          
          if (has_thesaurus_items){
            
            new_data <-
              r$widget_settings_thesaurus_selected_items %>%
              dplyr::transmute(
                id_temp = 1:dplyr::n() + last_row_widgets_items + 1,
                db_item_id = id,
                group_id = !!group_id,
                thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, thesaurus_item_colour,
                mapped_to_item_id, merge_items,
                creator_id = r$user_id,
                datetime = as.character(Sys.time()),
                deleted = FALSE
              ) %>%
              dplyr::rename(id = id_temp)
            
            new_data <- new_data %>%
              dplyr::left_join(
                new_data %>% dplyr::select(mapped_to_item_id = db_item_id, new_mapped_to_item_id = id),
                by = "mapped_to_item_id"
              ) %>%
              dplyr::mutate(mapped_to_item_id = new_mapped_to_item_id) %>%
              dplyr::select(-new_mapped_to_item_id)
            
            # Remove old data
            table <- paste0(prefix, "_widgets_items")
            sql <- glue::glue_sql("UPDATE {`table`} SET deleted = TRUE WHERE group_id = {group_id}", .con = r$db)
            query <- DBI::dbSendStatement(r$db, sql)
            DBI::dbClearResult(query)
            r[[paste0(prefix, "_widgets_items")]] <- r[[paste0(prefix, "_widgets_items")]] %>%
              dplyr::filter(group_id != !!group_id)
            
            # Add new data
            
            DBI::dbAppendTable(r$db, paste0(prefix, "_widgets_items"), new_data)
            r[[paste0(prefix, "_widgets_items")]] <- r[[paste0(prefix, "_widgets_items")]] %>% dplyr::bind_rows(new_data)
            
            # Save thesaurus items for server code
            thesaurus_selected_items <- new_data %>%
              dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
          }
        }
        
        show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
        
        # Load translations file
        
        i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
        plugin_translations <- r$code %>% dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
        
        if (plugin_translations != ""){
          
          tryCatch({
            # Get plugin unique_id
            plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == ids$plugin_id) %>% dplyr::pull(value)
            
            # Create plugin folder in translations folder if doesn't exist
            new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
            if (!dir.exists(new_dir)) dir.create(new_dir)
            
            new_file <- paste0(new_dir, "/plugin_translations.csv")
            if (!file.exists(new_file)) writeLines(plugin_translations, new_file)
          },
            error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
              error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
          
          tryCatch({
            i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
            i18np$set_translation_language(language)},
            error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
              error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
        }
        
        # Run server code
        
        code_server_card <- r$code %>%
          dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
          dplyr::pull(code) %>%
          stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
          stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
          stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
          stringr::str_replace_all("\r", "\n")
        
        # If it is an aggregated plugin, change %study_id% with current selected study
        if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
        
        session_code <- paste0("tab_", ids$tab_id, "_group_", group_id)
        if (length(o[[session_code]]) == 0) session_num <- 1L
        if (length(o[[session_code]]) > 0) session_num <- o[[session_code]] + 1
        o[[session_code]] <- session_num
        
        # Variables to hide
        new_env_vars <- list("r" = NA)
        # Variables to keep
        for (var in c("d", "m", "o", "thesaurus_selected_items", "session_code", "session_num", "i18n", "i18np")) new_env_vars[[var]] <- eval(parse(text = var))
        new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
        
        tryCatch(eval(parse(text = code_server_card), envir = new_env), error = function(e) print(e), warning = function(w) print(w))
        
        # Update toggles
        
        widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == ids$tab_id) %>% 
          dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)
        
        # Get widget group_id
        distinct_groups <- unique(widgets$group_id)
        
        toggles <- tagList()
        
        # Loop over distinct cards (tabs elements), for this tab
        sapply(distinct_groups, function(group_id){

          # Get name of widget
          widget_name <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)

          toggles <<- tagList(toggles,
            shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
            div(class = "toggle_title", widget_name, style = "padding-top:10px;"))

          # Add to the list of opened cards
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
        })

        toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", ids$tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(style = "width:20px;"),
              toggles
            )
          )
        )

        # r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", tab_id))
        # 
        # # Show opened cards
        # 
        # sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        # # Add tab toggles UI
        # # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(prefix, "_toggles_", tab_id))))
        output[[paste0(prefix, "_toggles_", ids$tab_id)]] <- renderUI(toggles_div)
        
        # Hide settings card and show opened cards
        shinyjs::hide(paste0(prefix, "_widget_settings"))
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$..edit_widget_button"))
      })
    
      # --- --- --- --- --- --- --- --- --- -- -
      ## Sortable / change pivotitems order ----
      # --- --- --- --- --- --- --- --- --- -- -
      
      observeEvent(input$study_pivot_order, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$study_pivot_order"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        new_pivot_order <- tibble::tibble(name = stringr::str_split(input$study_pivot_order, "\n") %>% unlist()) %>%
          dplyr::mutate(display_order = 1:dplyr::n())
        
        table <- paste0(prefix, "_tabs")
        tab_group_id <- r[[table]] %>% dplyr::filter(id == r[[paste0(prefix, '_selected_tab')]]) %>% dplyr::pull(tab_group_id)
        
        if (is.na(tab_group_id)) all_tabs <- r[[table]] %>% dplyr::filter(is.na(tab_group_id))
        else all_tabs <- r[[table]] %>% dplyr::filter(tab_group_id == !!tab_group_id)
        
        all_tabs <- all_tabs %>%
          dplyr::select(-display_order) %>%
          dplyr::inner_join(new_pivot_order, by = "name") %>%
          dplyr::relocate(display_order, .after = "parent_tab_id")
        
        sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id IN ({all_tabs %>% dplyr::pull(id)*})", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        DBI::dbAppendTable(r$db, table, all_tabs)
        
        r[[table]] <- r[[table]] %>% 
          dplyr::anti_join(all_tabs %>% dplyr::select(id), by = "id") %>%
          dplyr::bind_rows(all_tabs) %>%
          dplyr::arrange(id)
        
        r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$study_pivot_oder"))
      })
    
      # --- --- --- --- --- --- --- --- --- --- --- -
      ## Show / hide div when pivot item selected ----
      # --- --- --- --- --- --- --- --- --- --- --- -

      observeEvent(r[[paste0(prefix, "_selected_tab")]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..selected_tab"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        req(!grepl("show_tab", r[[paste0(prefix, "_selected_tab")]]))

        # Hide all cards
        sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)

        # Hide Add widget card & Add tab card
        sapply(c(paste0(prefix, "_add_tab"), paste0(prefix, "_edit_tab"), paste0(prefix, "_add_widget"), paste0(prefix, "_widget_settings")), shinyjs::hide)

        # Show toggles for this tab
        shinyjs::show(paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_tab")]]))

        # Add to the list of open cards and reset the list
        r[[paste0(prefix, "_opened_cards")]] <- paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_tab")]])

        widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == r[[paste0(prefix, "_selected_tab")]]) %>% 
          dplyr::rename(group_id = id)
        distinct_groups <- unique(widgets$group_id)

        sapply(distinct_groups, function(group_id){

          # If toggle is ON
          if (length(input[[paste0(paste0(prefix, "_group_", group_id), "_toggle")]]) > 0){

            if (input[[paste0(paste0(prefix, "_group_", group_id), "_toggle")]]){

              # Show card
              shinyjs::show(paste0(prefix, "_group_", group_id))

              # Add to the list of open cards
              r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
            }
          }
          else {
            shinyjs::show(paste0(prefix, "_group_", group_id))
            r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
          }

        })
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..selected_tab"))
      })
    
      # --- --- --- - -
      ## Add a tab ----
      # --- --- --- - -
  
      observeEvent(input$study_current_tab_trigger, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..study_current_tab_trigger"))

        req(grepl("add_tab", input$study_current_tab))
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::hide(paste0(prefix, "_add_widget"))
        shinyjs::hide(paste0(prefix, "_edit_tab"))
        shinyjs::hide(paste0(prefix, "_widget_settings"))
        shinyjs::show(paste0(prefix, "_add_tab"))
      })

      # Close creation div
      observeEvent(input[[paste0(prefix, "_close_add_tab")]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..close_add_tab"))

        # Show opened cards before opening Add widget div
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)

        # Hide Add widget div
        shinyjs::hide(paste0(prefix, "_add_tab"))
      })

      # Add button clicked
      observeEvent(input$add_tab_button, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$add_tab_button"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")

        study <- r$studies %>% dplyr::filter(id == m$selected_study)
        tab <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]])

        new_data <- list()
        new_data$name <- coalesce2(type = "char", x = input$tab_name)
        new_data$description <- ""

        # Required textfields
        required_textfields <- "name"

        # Fields requiring unique value
        req_unique_values <- "name"

        # Get tab_group_id
        new_data$tab_group <- study %>% dplyr::pull(paste0(prefix, "_tab_group_id"))

        # If it is the first tab to be created
        if (nrow(tab) == 0){
          new_data$parent_tab <- NA_integer_
          new_data$display_order <- 1
        }

        # If already existing tabs
        if (nrow(tab) > 0){

          # If tab is at the same level of current tab, get common parent_tab_id
          # Calculate display order

          if (input$add_tab_type == "same_level") new_data$parent_tab <- tab %>% dplyr::pull(parent_tab_id)
          if (input$add_tab_type == "level_under") new_data$parent_tab <- tab %>% dplyr::pull(id)

          # Calculate display order
          if (!is.na(new_data$parent_tab)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_tabs')`}
            WHERE tab_group_id = {new_data$tab_group} AND parent_tab_id = {new_data$parent_tab}", .con = r$db)
          if (is.na(new_data$parent_tab)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_tabs')`}
            WHERE tab_group_id = {new_data$tab_group} AND parent_tab_id IS NULL", .con = r$db)

          new_data$display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() + 1

          # Can't add a tab at the level under if there are tabs elements attached to current tab
          if (input$add_tab_type == "level_under"){
            widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == r[[paste0(prefix, "_selected_tab")]], !deleted) %>%
              dplyr::rename(group_id = id)
            if (nrow(widgets) > 0) show_message_bar(output, message = "add_tab_has_widgets", i18n = i18n)
            req(nrow(widgets) == 0)
          }
        }

        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("provide_valid_name"))
        req(!is.na(new_data$name))

        add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id,
          data = new_data, table = paste0(prefix, "_tabs"), required_textfields = required_textfields, req_unique_values = req_unique_values)

        # Reset fields

        shiny.fluent::updateTextField.shinyInput(session, "tab_name", value = "")
        shiny.fluent::updateChoiceGroup.shinyInput(session, "add_tab_type", value = "same_level")

        # Reload UI, with new tab opened

        tab_id <- get_last_row(r$db, paste0(prefix, "_tabs"))
        r[[paste0(prefix, "_selected_tab")]] <- tab_id
        r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
        
        # Add Toggles div
        
        toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(style = "width:20px;")
            )
          )
        )
      
        insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", tab_id))))
        output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
        
        # If this is a sub-tab, change toggles div of parent tab also
        parent_tab_id <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == tab_id) %>% dplyr::pull(parent_tab_id)
        if(!is.na(parent_tab_id)){
          
          #removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", parent_tab_id))))
          shinyjs::hide(paste0(prefix, "_toggles_", parent_tab_id))
          
          parent_toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
                div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
              )
            )
          )
          
          insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = shinyjs::hidden(uiOutput(ns(paste0(prefix, "_toggles_", parent_tab_id)))))
          output[[paste0(prefix, "_toggles_", parent_tab_id)]] <- renderUI(parent_toggles_div)
        }
        
        # Add toggles div to vector of cards
        r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_toggles_", tab_id))
        
        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(100, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()

        # Hide currently opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$add_tab_button"))
      })
      
      # --- --- --- -- -
      ## Edit a tab ----
      # --- --- --- -- -
      
      observeEvent(input$edit_tab_trigger, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$edit_tab_trigger"))
        
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::hide(paste0(prefix, "_add_widget"))
        shinyjs::hide(paste0(prefix, "_widget_settings"))
        shinyjs::show(paste0(prefix, "_edit_tab"))
        
        # Update textfield with current tab name
        shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", 
          value = r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]]) %>% dplyr::pull(name))
      })
      
      # Save updates
      observeEvent(input$edit_tab_save, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$edit_tab_save"))
        
        # Check if the name is not empty
        if (input$edit_tab_name == "") shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", errorMessage = i18n$t("provide_valid_name"))
        
        req(input$edit_tab_name != "")
        
        # Check if the name is already used
        tab <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]])
        
        if (!is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id != tab$id, parent_tab_id == tab$parent_tab_id)
        if (is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id != tab$id, is.na(parent_tab_id))
        
        if (input$edit_tab_name %in% same_level_tabs$name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
        
        req(input$edit_tab_name %not_in% same_level_tabs$name)
        
        # Update database
        
        sql <- glue::glue_sql("UPDATE {`paste0(prefix, '_tabs')`} SET name = {input$edit_tab_name} WHERE id = {r[[paste0(prefix, '_selected_tab')]]}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        # Update r vars
        r[[paste0(prefix, "_tabs")]] <- r[[paste0(prefix, "_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
          id == r[[paste0(prefix, "_selected_tab")]] ~ input$edit_tab_name, TRUE ~ name))
        
        r[[paste0(prefix, "_display_tabs")]] <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
          id == r[[paste0(prefix, "_selected_tab")]] ~ input$edit_tab_name, TRUE ~ name))
        
        # Notify user
        show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
        
        # Reload output
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      })
      
      # Close edition div
      observeEvent(input[[paste0(prefix, "_close_edit_tab")]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..close_edit_tab"))
        
        # Show opened cards before opening Add widget div
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        # Hide Edit widget div
        shinyjs::hide(paste0(prefix, "_edit_tab"))
      })
      
      # --- --- --- --- --- --- --- --- --- --
      ## Show / hide widget creation card ----
      # --- --- --- --- --- --- --- --- --- --
      
      # Code to make Add widget button work
      observeEvent(input[[paste0(prefix, "_add_widget_trigger")]], {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..add_widget_trigger"))
        
        # Hide opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        
        # Show Add widget div
        shinyjs::show(paste0(prefix, "_add_widget"))
        
        r[[paste0(prefix, "_widget_card_selected_type")]] <- "widget_creation"
      })
      
      # --- --- --- --- --
      ## Delete a tab ----
      # --- --- --- --- --
      
      # Code to make Remove tab button work
      observeEvent(input$remove_tab, {
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$remove_tab"))
        r[[tab_delete_variable]] <- TRUE
      })
      
      tab_delete_prefix <- paste0(prefix, "_tab")
      tab_dialog_title <- paste0(prefix, "_tabs_delete")
      tab_dialog_subtext <- paste0(prefix, "_tabs_delete_subtext")
      tab_react_variable <- "tab_delete_confirm"
      tab_table <- paste0(prefix, "_tabs")
      tab_id_var_sql <- "id"
      tab_id_var_r <- paste0(prefix, "_selected_tab")
      tab_delete_message <- paste0(prefix, "_tab_deleted")
      tab_reload_variable <- paste0(prefix, "_load_ui")
      tab_information_variable <- paste0(prefix, "_tab_deleted")
      tab_delete_variable <- paste0(tab_delete_prefix, "_open_dialog")

      delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = tab_delete_prefix, dialog_title = tab_dialog_title, dialog_subtext = tab_dialog_subtext,
        react_variable = tab_react_variable, table = tab_table, id_var_sql = tab_id_var_sql, id_var_r = tab_id_var_r,
        delete_message = tab_delete_message, translation = TRUE, reload_variable = tab_reload_variable,
        information_variable = tab_information_variable)

      # When a tab is deleted, change current tab variable
      # Reload toggles if necessary
      # Delete sub-tabs either

      observeEvent(r[[tab_information_variable]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..tab_deleted"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")

        table <- paste0(prefix, "_tabs")
        deleted_tab_id <- r[[paste0(prefix, "_tab_deleted")]]
        sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_tab_id}", .con = r$db)
        tab_deleted <- DBI::dbGetQuery(r$db, sql)

        # If we are at level one, take first tab of level one
        if (is.na(tab_deleted$parent_tab_id)){
          show_tab_id <- r[[table]] %>%
            dplyr::filter(tab_group_id == tab_deleted$tab_group_id & is.na(parent_tab_id) & id != tab_deleted$id) %>%
            dplyr::arrange(display_order) %>%
            dplyr::slice(1) %>%
            dplyr::pull(id)
        }

        # Else, take first tab of the same level
        if (!is.na(tab_deleted$parent_tab_id)){
          show_tab <- r[[table]] %>%
            dplyr::filter(parent_tab_id == tab_deleted$parent_tab_id & id != tab_deleted$id)

          # If not any tab in this level, take lower level
          if (nrow(show_tab) == 0) show_tab <- r[[table]] %>%
              dplyr::filter(id == tab_deleted$parent_tab_id)

          show_tab_id <- show_tab %>%
            dplyr::arrange(display_order) %>%
            dplyr::slice(1) %>%
            dplyr::pull(id)
        }

        r[[paste0(prefix, "_selected_tab")]] <- paste0("show_tab_", show_tab_id)
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::show(paste0(prefix, "_toggles_", show_tab_id))

        # Reload UI menu
        r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
        
        # Check if parent tab still have children and reload toggles div if not
        sql <- glue::glue_sql("SELECT parent_tab_id FROM {`paste0(prefix, '_tabs')`} WHERE id = {deleted_tab_id}", .con = r$db)
        parent_tab_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(parent_tab_id)
        if (!is.na(parent_tab_id)){
          
          has_children <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == !!parent_tab_id) %>% nrow()
          if(has_children == 0){
            
            shinyjs::hide(paste0(prefix, "_toggles_", parent_tab_id))
            
            parent_toggles_div <- div(
              make_card("",
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", parent_tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                    onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", parent_tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                    onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
                  div(style = "width:20px;")
                )
              )
            )
            
            insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", parent_tab_id))))
            output[[paste0(prefix, "_toggles_", parent_tab_id)]] <- renderUI(parent_toggles_div)
          }
        }
        
        # Hide edit_tab div
        shinyjs::hide(paste0(prefix, "_edit_tab"))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..tab_deleted"))
      })

      # --- --- --- --- --
      ## Add a widget ----
      # --- --- --- --- --

      # Only for patient-lvl data

      if (prefix == "patient_lvl"){

        # --- --- --- --- --- --- -
        ## Thesaurus datatable ----
        # --- --- --- --- --- --- -

        # Load thesaurus attached to this dataset
        observeEvent(r$selected_dataset, {
          
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$selected_dataset"))
          if (perf_monitoring) monitor_perf(r = r, action = "start")

          req(!is.na(r$selected_dataset))
          
          # Reset study menu
          sapply(c("initial_breadcrumb", "choose_a_study_card"), shinyjs::show)
          sapply(c("study_menu", paste0(prefix, "_add_widget"), paste0(prefix, "_widget_settings")), shinyjs::hide)

          r[[paste0(prefix, "_reload_thesaurus_dropdown")]] <- Sys.time()
          
          # Reset datatable
          if (length(r$widget_creation_thesaurus_items) > 0){
            r$widget_creation_thesaurus_items <- r$widget_creation_thesaurus_items %>% dplyr::slice(0)
            r$widget_creation_thesaurus_items_temp <- r$widget_creation_thesaurus_items_temp %>% dplyr::slice(0)
          }
          
          # Reset selected_items combobox
          shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_thesaurus_selected_items", options = list(), value = NULL)
          if (length(r$widget_creation_thesaurus_selected_items) > 0){
            r$widget_creation_thesaurus_selected_items <- r$widget_creation_thesaurus_selected_items %>% dplyr::slice(0)
          }
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$selected_dataset"))
        })
        
        # If r$studies changes, hide study_menu
        observeEvent(r$studies, {
          
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$studies"))
          
          shinyjs::show("initial_breadcrumb")
          shinyjs::show("choose_a_study_card")
          shinyjs::hide("study_menu")
          sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
        })

        # Load thesaurus items
        
        observeEvent(input$widget_creation_thesaurus, {
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_thesaurus"))
          r[[paste0(prefix, "_load_thesaurus_trigger")]] <- Sys.time()
          # r[[paste0(prefix, "_load_thesaurus_type")]] <- "widget_creation"
        })
        
        # observeEvent(r[[paste0(prefix, "_load_thesaurus_trigger")]], {
        #   
        #   if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..load_thesaurus_trigger"))
        #   if (perf_monitoring) monitor_perf(r = r, action = "start")
        #   
        #   r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_items")
        #   thesaurus_id <- input[[paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus")]]$key
        #   
        #   r[[r_var]] <- create_datatable_cache(output = output, r = r, i18n = i18n, tab_id = id, thesaurus_id = thesaurus_id, category = paste0("plus_", r[[paste0(prefix, "_widget_card_selected_type")]]))
        #   colour_col <- create_datatable_cache(output = output, r = r, i18n = i18n, tab_id = id, thesaurus_id = thesaurus_id, category = paste0("colours_", r[[paste0(prefix, "_widget_card_selected_type")]]))
        # 
        #   if (nrow(colour_col) > 0) r[[r_var]] <- r[[r_var]] %>%
        #     dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
        # 
        #   count_items_rows <- tibble::tibble()
        #   count_patients_rows <- tibble::tibble()
        # 
        #   # Add count_items_rows in the cache & get it if already in the cache
        #   tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = thesaurus_id,
        #     dataset_id = r$selected_dataset, category = "count_items_rows"),
        #       error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus",
        #         error_name = paste0("patient_and_aggregated_data - create_datatable_cache - count_patients_rows - fail_load_thesaurus - id = ", thesaurus_id ,
        #           " - fail_load_dataset - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        # 
        #   # Add count_items_rows in the cache & get it if already in the cache
        #   tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = thesaurus_id,
        #     dataset_id = as.integer(r$selected_dataset), category = "count_patients_rows"),
        #       error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus",
        #         error_name = paste0("patient_and_aggregated_data - create_datatable_cache - count_patients_rows - fail_load_thesaurus - id = ", thesaurus_id ,
        #           " - fail_load_dataset - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        # 
        #   if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0){
        #     show_message_bar(output, "fail_load_thesaurus", "severeWarning", i18n = i18n, ns = ns)
        #     r[[r_var]] <- r[[r_var]] %>% dplyr::slice(0)
        #     r[[paste0(r_var, "_temp")]] <- r[[r_var]] %>% dplyr::mutate(modified = FALSE)
        #   }
        #   
        #   if(nrow(count_items_rows) != 0 & nrow(count_patients_rows) != 0){
        # 
        #     # Transform count_rows cols to integer, to be sortable
        #     r[[r_var]] <- r[[r_var]] %>%
        #       dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
        #       dplyr::left_join(count_items_rows, by = "item_id") %>%
        #       dplyr::left_join(count_patients_rows, by = "item_id") %>%
        #       dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        #       dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action") %>%
        #       dplyr::arrange(dplyr::desc(count_items_rows))
        # 
        #     # Filter on count_items_rows > 0
        #     r[[r_var]] <- r[[r_var]] %>% dplyr::filter(count_items_rows > 0)
        # 
        #     r[[paste0(r_var, "_temp")]] <- r[[r_var]] %>%
        #       dplyr::mutate(modified = FALSE) %>%
        #       dplyr::mutate_at("item_id", as.character)
        #   }
        # 
        #   editable_cols <- c("display_name", "unit")
        #   searchable_cols <- c("item_id", "name", "display_name", "unit")
        #   factorize_cols <- c("unit")
        #   column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")
        #   sortable_cols <- c("id", "item_id", "name", "display_name", "count_patients_rows", "count_items_rows")
        #   centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        #   col_names <- get_col_names(table_name = "tabs_thesaurus_items_with_counts", i18n = i18n)
        #   hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
        # 
        #   # Render datatable
        #   render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0(r_var, "_temp")]],
        #     output_name = r_var, col_names =  col_names,
        #     editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        #     searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        # 
        #   # Create a proxy for datatatable
        #   r[[paste0(r_var, "_proxy")]] <- DT::dataTableProxy(r_var, deferUntilFlush = FALSE)
        # 
        #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_thesaurus_trigger"))
        # })

        # Reload datatable
        observeEvent(r$widget_creation_thesaurus_items_temp, {
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$widget_creation_thesaurus_items_temp"))
          r[[paste0(prefix, "_reload_datatable_trigger")]] <- Sys.time()
        })

        observeEvent(r[[paste0(prefix, "_reload_datatable_trigger")]], {
          
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..reload_datatable_trigger"))

          r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_items")

          # Reload data of datatable
          DT::replaceData(r[[paste0(r_var, "_proxy")]], r[[paste0(r_var, "_temp")]], resetPaging = FALSE, rownames = FALSE)
        })

        # Updates in datatable

        observeEvent(input$widget_creation_thesaurus_items_cell_edit, {
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_thesaurus_items_cell_edit"))
          r[[paste0(prefix, "_edit_datatable_trigger")]] <- Sys.time()
        })

        observeEvent(r[[paste0(prefix, "_edit_datatable_trigger")]], {
          
          if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..edit_datatable_trigger"))

          r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_items")
          input_edit_info <- paste0(r_var, "_cell_edit")

          edit_info <- input[[input_edit_info]]

          r[[paste0(r_var, "_temp")]] <- DT::editData(r[[paste0(r_var, "_temp")]], edit_info, rownames = FALSE)
          r[[paste0(r_var, "_temp")]][[edit_info$row, "modified"]] <- TRUE
        })

        # --- --- --- --- --- -
        ## Thesaurus items ----
        # --- --- --- --- --- -

          # --- --- --- --- --- --- ---
          ### Thesaurus Item added ----
          # --- --- --- --- --- --- ---
          
          observeEvent(input$widget_creation_item_selected, {
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_item_selected"))
            r[[paste0(prefix, "_item_selected_trigger")]] <- Sys.time()
          })
          
          observeEvent(r[[paste0(prefix, "_item_selected_trigger")]], {
            
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..item_selected_trigger"))
            if (perf_monitoring) monitor_perf(r = r, action = "start")
            
            r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
            thesaurus_id <- input[[paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus")]]$key
            thesaurus_mapping_input <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_mapping")
            merge_mapped_items_input <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_merge_mapped_items")
            thesaurus_selected_items_input <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
            input_item_selected <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_item_selected")
            input_colour <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_colour")
            
            # Initiate r variable if doesn't exist
            if (length(r[[r_var]]) == 0){
              r[[r_var]] <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
                thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
                thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
            }
  
            # Get ID of selected thesaurus item
            link_id <- as.integer(substr(input[[input_item_selected]], nchar("select_") + 1, nchar(input[[input_item_selected]])))
  
            # If this thesaurus item is not already selected, add it to the "thesaurus selected items" dropdown
  
            # value <- integer(1)
            # if (nrow(r$widget_thesaurus_selected_items) > 0) value <- r$widget_thesaurus_selected_items %>%
            #   dplyr::filter(thesaurus_id == input$thesaurus$key) %>% dplyr::pull(id)
  
            # if (link_id %not_in% value){
  
            # Get thesaurus name
            thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% dplyr::pull(name)
  
            # Get item informations from datatable
            # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
  
            item <- r[[paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_items_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", display_name))
  
            # display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)
  
            # Get mapped items
            thesaurus_mapped_items <- tibble::tibble()
            if (length(input[[thesaurus_mapping_input]]) > 0){
              # if (input$thesaurus_mapping %in% c(1, 2, 3)){
  
              # Select only validated mappings (with at least one positive eval and more positive than negative evals)
              # Select mapping in the two ways (added item may be item_1, or item_2)
              sql <- glue::glue_sql(paste0(
                "SELECT m.thesaurus_id_2 AS thesaurus_id, m.item_id_2 AS thesaurus_item_id, e.evaluation_id, ",
                "i.id, i.name AS thesaurus_item_name, i.display_name AS thesaurus_item_display_name, i.unit AS thesaurus_item_unit, ",
                "u.name AS user_thesaurus_item_name, u.display_name AS user_thesaurus_item_display_name, u.unit AS user_thesaurus_item_unit ",
                "FROM thesaurus_items_mapping m ",
                "INNER JOIN thesaurus_items_mapping_evals e ON m.id = e.mapping_id AND e.deleted IS FALSE ",
                "INNER JOIN thesaurus_items i ON m.thesaurus_id_2 = i.thesaurus_id AND m.item_id_2 = i.item_id AND i.deleted IS FALSE ",
                "LEFT JOIN thesaurus_items_users u ON m.thesaurus_id_2 = u.thesaurus_id AND m.item_id_2 = u.item_id AND u.deleted IS FALSE ",
                "WHERE m.thesaurus_id_1 = {as.integer(thesaurus_id)} AND m.item_id_1 = {as.integer(item$item_id)} AND m.relation_id IN ({input[[thesaurus_mapping_input]]*}) ",
                "AND m.category = 'user_added_mapping' AND m.deleted IS FALSE ",
                "UNION ",
                "SELECT m.thesaurus_id_1 AS thesaurus_id, m.item_id_1 AS thesaurus_item_id, e.evaluation_id, ",
                "i.id, i.name AS thesaurus_item_name, i.display_name AS thesaurus_item_display_name, i.unit AS thesaurus_item_unit, ",
                "u.name AS user_thesaurus_item_name, u.display_name AS user_thesaurus_item_display_name, u.unit AS user_thesaurus_item_unit ",
                "FROM thesaurus_items_mapping m ",
                "INNER JOIN thesaurus_items_mapping_evals e ON m.id = e.mapping_id AND e.deleted IS FALSE ",
                "INNER JOIN thesaurus_items i ON m.thesaurus_id_1 = i.thesaurus_id AND m.item_id_1 = i.item_id AND i.deleted IS FALSE ",
                "LEFT JOIN thesaurus_items_users u ON m.thesaurus_id_1 = u.thesaurus_id AND m.item_id_1 = u.item_id AND u.deleted IS FALSE ",
                "WHERE m.thesaurus_id_2 = {as.integer(thesaurus_id)} AND m.item_id_2 = {as.integer(item$item_id)} AND m.relation_id IN ({input[[thesaurus_mapping_input]]*}) ",
                "AND m.category = 'user_added_mapping' AND m.deleted IS FALSE"
                ), .con = r$db)
  
              thesaurus_mapped_items <- DBI::dbGetQuery(r$db, sql) %>%
                dplyr::group_by(id, thesaurus_id, thesaurus_item_id,
                  thesaurus_item_display_name, user_thesaurus_item_display_name, thesaurus_item_name, user_thesaurus_item_name,
                  thesaurus_item_unit, user_thesaurus_item_unit) %>%
                dplyr::summarize(
                  positive_evals = sum(evaluation_id == 1, na.rm = TRUE),
                  negative_evals = sum(evaluation_id == 2, na.rm = TRUE)
                ) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(
                  positive_evals = ifelse(positive_evals > 0, positive_evals, 0),
                  negative_evals = ifelse(negative_evals > 0, negative_evals, 0)
                ) %>%
                dplyr::filter(positive_evals > negative_evals) %>%
                dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_id") %>%
                dplyr::mutate(
                  thesaurus_item_name = ifelse((user_thesaurus_item_name != "" & !is.na(user_thesaurus_item_name)), user_thesaurus_item_name, thesaurus_item_name),
                  thesaurus_item_display_name = ifelse((user_thesaurus_item_display_name != "" & !is.na(user_thesaurus_item_display_name)), user_thesaurus_item_display_name, thesaurus_item_display_name),
                  thesaurus_item_unit = ifelse((user_thesaurus_item_unit != "" & !is.na(user_thesaurus_item_unit)), user_thesaurus_item_unit, thesaurus_item_unit),
                ) %>%
                dplyr::mutate(
                  thesaurus_item_display_name = ifelse((thesaurus_item_display_name != "" & !is.na(thesaurus_item_display_name)), thesaurus_item_display_name, thesaurus_item_name)
                ) %>%
                dplyr::transmute(
                  id, thesaurus_id, thesaurus_name, thesaurus_item_id, thesaurus_item_display_name,
                  thesaurus_item_unit, thesaurus_item_colour = as.character(input[[paste0(input_colour, "_", link_id)]]),
                  input_text = paste0("--- ", thesaurus_name, " - ", thesaurus_item_display_name, " (", tolower(i18n$t("mapped_item")), ")"),
                  mapped_to_item_id = link_id, merge_items = input[[paste0(prefix, "_", merge_mapped_items_input)]]
                ) %>%
                dplyr::anti_join(r[[r_var]] %>% dplyr::select(id), by = "id")
              # }
            }
  
            # Add item to selected items
            add_thesaurus_items <-
              # r$widget_thesaurus_selected_items %>%
              # dplyr::bind_rows(
                tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name,
                  ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text, ~mapped_to_item_id, ~merge_items,
                  as.integer(link_id), as.integer(thesaurus_id), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
                  as.character(item$unit), as.character(input[[paste0(input_colour, "_", link_id)]]), as.character(item$input_text),
                  NA_integer_, FALSE)
              # )
            
            if (nrow(thesaurus_mapped_items) > 0) add_thesaurus_items <-
              add_thesaurus_items %>% dplyr::bind_rows(thesaurus_mapped_items)
  
            r[[r_var]] <- r[[r_var]] %>%
              dplyr::anti_join(add_thesaurus_items %>% dplyr::select(id), by = "id") %>%
              dplyr::bind_rows(add_thesaurus_items)
            
            # Update dropdown of selected items
            options <- convert_tibble_to_list(r[[r_var]], key_col = "id", text_col = "input_text", i18n = i18n)
            value <- r[[r_var]]%>% dplyr::pull(id)
            shiny.fluent::updateDropdown.shinyInput(session, thesaurus_selected_items_input,
              options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
            # }
            
            if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..item_selected_trigger"))
  
          })
  
          # When reset button is clicked
          observeEvent(input$widget_creation_reset_thesaurus_items, {
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_reset_thesaurus_items"))
            r[[paste0(prefix, "_reset_thesaurus_items_trigger")]] <- Sys.time()
          })
          
          observeEvent(r[[paste0(prefix, "_reset_thesaurus_items_trigger")]], {
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..reset_thesaurus_items_trigger"))
            
            r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
            input_thesaurus_selected_items <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
            
            r[[r_var]] <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
              thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
              thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
  
            shiny.fluent::updateDropdown.shinyInput(session, input_thesaurus_selected_items, options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
          })
          
          # --- --- --- --- --- -
          ### Dropdown update ----
          # --- --- --- --- --- -
  
          # When dropdown is modified
          observeEvent(input$widget_creation_thesaurus_selected_items_trigger, {
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_thesaurus_selected_items_trigger"))
            r[[paste0(prefix, "_thesaurus_selected_items_trigger")]] <- Sys.time()
          })
          
          observeEvent(r[[paste0(prefix, "_thesaurus_selected_items_trigger")]], {
            
            if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$..thesaurus_selected_items_trigger"))
            
            r_var <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
            input_thesaurus_selected_items <- paste0(r[[paste0(prefix, "_widget_card_selected_type")]], "_thesaurus_selected_items")
  
            if (length(input[[input_thesaurus_selected_items]]) == 0) r[[r_var]] <- r[[r_var]] %>% dplyr::slice(0)
            if (length(input[[input_thesaurus_selected_items]]) > 0) {
              r[[r_var]] <- r[[r_var]] %>%
                dplyr::filter(id %in% input[[input_thesaurus_selected_items]])
              # Delete also mapped items
              r[[r_var]] <- r[[r_var]] %>%
                dplyr::filter(is.na(mapped_to_item_id) | mapped_to_item_id %in% r[[r_var]]$id)
            }
  
            options <- convert_tibble_to_list(r[[r_var]], key_col = "id", text_col = "input_text", i18n = i18n)
            value <- r[[r_var]] %>% dplyr::pull(id)
            shiny.fluent::updateDropdown.shinyInput(session, input_thesaurus_selected_items,
              options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
          })
  
        }
        # End of if(prefix == "patient_lvl")
        
      # --- --- --- --- --- --- --- ---
      ## Widget add button clicked ----
      # --- --- --- --- --- --- --- ---
      
      observeEvent(input$add_widget_button, {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$add_widget_button"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")

        new_data <- list()

        new_data$name <- coalesce2(type = "char", x = input$widget_creation_name)
        new_data$tab_group <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]]) %>% dplyr::pull(tab_group_id)
        new_data$tab_new_element <- r[[paste0(prefix, "_selected_tab")]]
        new_data$plugin <- input$widget_creation_plugin$key

        # Check if name is not empty
        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = i18n$t("provide_valid_name"))
        else shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = NULL)
        req(!is.na(new_data$name))

        # Check if values required to be unique are unique

        table <- paste0(prefix, "_widgets")

        sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND tab_id = {new_data$tab_new_element}", .con = r$db)
        distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        if (new_data$name %in% distinct_values) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
        req(new_data$name %not_in% distinct_values)

        # Check if dropdowns are not empty (if all are required)
        dropdowns_check <- TRUE

        required_dropdowns <- c("plugin")

        for (dropdown in required_dropdowns){
          if (is.null(new_data[[dropdown]])) dropdowns_check <- FALSE
          else if (is.na(new_data[[dropdown]])) dropdowns_check <- FALSE
        }

        if (!dropdowns_check) show_message_bar(output, "dropdown_empty", "severeWarning", i18n = i18n, ns = ns)
        req(dropdowns_check)

        # Get last_row nb
        # last_row_widgets <- get_last_row(r$db, paste0(prefix, "_widgets"))
        group_id <- get_last_row(r$db, paste0(prefix, "_widgets")) + 1
        last_row_widgets_items <- get_last_row(r$db, paste0(prefix, "_widgets_items"))
        # group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table)) %>% dplyr::pull() %>% as.integer() + 1
        last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", paste0(prefix, "_widgets"), " WHERE tab_id = ", new_data$tab_new_element)) %>% dplyr::pull() %>% as.integer()

        new_data <- tibble::tribble(~id, ~name, ~tab_id, ~plugin_id, ~display_order, ~creator_id, ~datetime, ~deleted,
          group_id, as.character(new_data$name), as.integer(new_data$tab_new_element),
          as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
        
        DBI::dbAppendTable(r$db, paste0(prefix, "_widgets"), new_data)
        add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>% dplyr::bind_rows(new_data)
        
        has_thesaurus_items <- TRUE
        thesaurus_selected_items <- tibble::tibble()

        if (prefix == "patient_lvl"){

          if (length(r$widget_creation_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          if (length(r$widget_creation_thesaurus_selected_items) > 0) if (nrow(r$widget_creation_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE

          if (has_thesaurus_items){
            
            new_data <-
              r$widget_creation_thesaurus_selected_items %>%
              dplyr::transmute(
                id_temp = 1:dplyr::n() + last_row_widgets_items + 1,
                db_item_id = id,
                group_id = !!group_id,
                thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, thesaurus_item_colour,
                mapped_to_item_id, merge_items,
                creator_id = r$user_id,
                datetime = as.character(Sys.time()),
                deleted = FALSE
              ) %>%
              dplyr::rename(id = id_temp)
            
            new_data <- new_data %>%
              dplyr::left_join(
                new_data %>% dplyr::select(mapped_to_item_id = db_item_id, new_mapped_to_item_id = id),
                by = "mapped_to_item_id"
              ) %>%
              dplyr::mutate(mapped_to_item_id = new_mapped_to_item_id) %>%
              dplyr::select(-new_mapped_to_item_id)
            
            DBI::dbAppendTable(r$db, paste0(prefix, "_widgets_items"), new_data)
            add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
            r[[paste0(prefix, "_widgets_items")]] <- r[[paste0(prefix, "_widgets_items")]] %>% dplyr::bind_rows(new_data)
            
            # Save thesaurus items for server code
            thesaurus_selected_items <- new_data %>%
              dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
            
            # Reset r$widget_thesaurus_selected_items & dropdown
            r$widget_creation_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
              thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
              thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
            shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
          }
        }
        
        show_message_bar(output, message = paste0(get_singular(paste0(prefix, "_widgets")), "_added"), type = "success", i18n = i18n, ns = ns)

        # Reset name textfield & dropdowns
        shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")

        # Load translations file
        
        plugin_id <- input$widget_creation_plugin$key
        i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
        plugin_translations <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
        
        if (plugin_translations != ""){
          
          tryCatch({
            # Get plugin unique_id
            plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == plugin_id) %>% dplyr::pull(value)
            
            # Create plugin folder in translations folder if doesn't exist
            new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
            if (!dir.exists(new_dir)) dir.create(new_dir)
            
            new_file <- paste0(new_dir, "/plugin_translations.csv")
            if (!file.exists(new_file)) writeLines(plugin_translations, new_file)
          },
            error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
              error_name = paste0(id, " - create translations files - plugin_id ", plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
          
          tryCatch({
            i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
            i18np$set_translation_language(language)},
            error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
              error_name = paste0(id, " - create i18np translator - plugin_id ", plugin_id), category = "Error", error_report = e, i18n = i18n, ns = ns))
        }
        
        # Run server code

        trace_code <- paste0(prefix, "_", group_id, "_", m$selected_study)
        # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
        if (trace_code %not_in% r$server_tabs_groups_loaded){
  
          # Add the trace_code to loaded plugins list
          r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)
  
          # Get plugin code
  
          # Check if plugin has been deleted
          check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", input$widget_creation_plugin$key)) %>% dplyr::pull(deleted)
          if (!check_deleted_plugin){
  
            code_server_card <- r$code %>%
              dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_server") %>%
              dplyr::pull(code) %>%
              stringr::str_replace_all("%tab_id%", as.character(r[[paste0(prefix, "_selected_tab")]])) %>%
              stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
              stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
              stringr::str_replace_all("\r", "\n")
  
            # If it is an aggregated plugin, change %study_id% with current selected study
            if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
          }
          else code_server_card <- ""
          
          # Create a session number, to inactivate older observers
          # Reset all older observers for this group_id
          
          session_code <- paste0("tab_", r[[paste0(prefix, "_selected_tab")]], "_group_", group_id)
          if (length(o[[session_code]]) == 0) session_num <- 1L
          if (length(o[[session_code]]) > 0) session_num <- o[[session_code]] + 1
          o[[session_code]] <- session_num
          
          # NB : req(o[[session_code]] == session_num) must be put at the beginning of each observeEvent in plugins code

          # Variables to hide
          new_env_vars <- list("r" = NA)
          # Variables to keep
          for (var in c("d", "m", "o", "thesaurus_selected_items", "session_code", "session_num")) new_env_vars[[var]] <- eval(parse(text = var))
          new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
          
          tryCatch(eval(parse(text = code_server_card), envir = new_env), error = function(e) print(e), warning = function(w) print(w))
          
          # Code for toggle reactivity
          toggle <- paste0(prefix, "_group_", group_id)
  
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
            else shinyjs::hide(toggle)
          })
  
          # Code for removing widget
  
          observeEvent(input[[paste0(prefix, "_remove_widget_", group_id)]], {
            r[[paste0(prefix, "_selected_widget")]] <- group_id
            r[[widget_delete_variable]] <- TRUE
          })

          # Code for widget settings
          
          observeEvent(input[[paste0(prefix, "_settings_widget_", group_id)]], {
            r[[paste0(prefix, "_settings_widget_trigger")]] <- Sys.time()
            r[[paste0(prefix, "_settings_widget")]] <- group_id
          })
          
        }

        # Prepare widget UI code
        tab_id <- r[[paste0(prefix, "_selected_tab")]]
        
        code_ui_card <- isolate(r$code) %>% dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_ui") %>% dplyr::pull(code)
        element_code <- div()
        widget_name <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        tryCatch({
          code_ui_card <- code_ui_card %>%
            stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
            stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
            stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
            stringr::str_replace_all("\r", "\n") %>%
            stringr::str_replace_all("%study_id%", as.character(isolate(m$selected_study)))
          
          element_code <- div(
            make_card("",
              div(
                div(id = ns(paste0(prefix, "_widget_plugin_ui_", group_id)), eval(parse(text = code_ui_card))),
                div(
                  id = ns(paste0(prefix, "_widget_settings_remove_buttons_", group_id)),
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                    actionButton(ns(paste0(prefix, "_settings_widget_", group_id)), "", icon = icon("cog")),
                    actionButton(ns(paste0(prefix, "_remove_widget_", group_id)), "", icon = icon("trash-alt"))
                  ),
                  style = "position:absolute; top:8px; right: 10px;"
                )
              ),
              style = "position:relative;"
            )
          )
        },
        error = function(e){
          report_bug(r = r, output = output, error_message = i18n$t("error_run_plugin_ui_code"),
            error_name = paste0(id, " - run ui code - ", group_id), category = "Error", error_report = e, i18n = i18n, ns = ns)}
        )
        # Remove toggles UI for this tab
        # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", tab_id))))
        shinyjs::hide(paste0(prefix, "_toggles_", tab_id))
        
        # Add the new toggles UI for this tab
        
        toggles <- tagList()
        # update_r(r = r, table = paste0(prefix, "_widgets"))
        widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>%
          dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)
     
        # Get widget group_id
        distinct_groups <- unique(widgets$group_id)
        
        # Reset opened cards
        r[[paste0(prefix, "_opened_cards")]] <- ""

        # Loop over distinct cards (tabs elements), for this tab
        
        sapply(distinct_groups, function(group_id){
          
          # Get name of widget
          widget_name <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
          
          toggles <<- tagList(toggles,
            shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
            div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
          
          # Add to the list of opened cards
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
        })
        
        toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(style = "width:20px;"),
              toggles
            )
          )
        )
        
        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", tab_id))
        
        # Show opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        # Add tab toggles UI
        # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(prefix, "_toggles_", tab_id))))
        output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)

        # Add widget UI
        insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_group_", group_id))))
        output[[paste0(prefix, "_group_", group_id)]] <- renderUI(element_code)
        
        # Hide Add widget div
        shinyjs::hide(paste0(prefix, "_add_widget"))
        
        # Add this div to vector of cards
        r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_group_", group_id))
        
        # Reload UI menu
        r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
        
        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(300, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$add_widget_button"))
      })
      
      # --- --- --- --- --- -
      ## Delete a widget ----
      # --- --- --- --- --- -
      
      widget_delete_prefix <- paste0(prefix, "_widget")
      widget_dialog_title <- paste0(prefix, "_widgets_delete")
      widget_dialog_subtext <- paste0(prefix, "_widgets_delete_subtext")
      widget_react_variable <- "widget_delete_confirm"
      widget_table <- paste0(prefix, "_widgets")
      widget_id_var_sql <- "id"
      widget_id_var_r <- paste0(prefix, "_selected_widget")
      widget_delete_message <- paste0(prefix, "_widget_deleted")
      widget_reload_variable <- paste0(prefix, "_load_ui")
      widget_delete_variable <- paste0(widget_delete_prefix, "_open_dialog")
      widget_information_variable <- paste0(prefix, "_widget_deleted")

      delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = widget_delete_prefix, dialog_title = widget_dialog_title, dialog_subtext = widget_dialog_subtext,
        react_variable = widget_react_variable, table = widget_table, id_var_sql = widget_id_var_sql, id_var_r = widget_id_var_r,
        delete_message = widget_delete_message, translation = TRUE, reload_variable = widget_reload_variable,
        information_variable = widget_information_variable)
      
      # When a tab is element deleted, remove UI and reload toggles UI
      observeEvent(r[[widget_information_variable]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer r$..widget_deleted"))
        
        # table <- paste0(prefix, "_tabs")
        # deleted_tab_id <- r[[paste0(prefix, "_widget_group_deleted")]]
        # sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_tab_id}", .con = r$db)
        # tab_deleted <- DBI::dbGetQuery(r$db, sql)
        
        group_id <- r[[paste0(prefix, "_widget_deleted")]]
        # Remove UI card
        removeUI(selector = paste0("#", ns(paste0(prefix, "_group_", r[[paste0(prefix, "_widget_deleted")]]))))
        
        sql <- glue::glue_sql("SELECT DISTINCT(tab_id) FROM {`paste0(prefix, '_widgets')`} WHERE id = {group_id}", .con = r$db)
        tab_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

        # Remove toggles UI for this tab
        # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", tab_id))))
        shinyjs::hide(paste0(prefix, "_toggles_", tab_id))

        # Add the new toggles UI for this tab

        toggles <- tagList()
        r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id != group_id)
        r[[paste0(prefix, "_widgets_items")]] <- r[[paste0(prefix, "_widgets_items")]] %>% dplyr::filter(group_id != !!group_id)
        
        # update_r(r = r, table = paste0(prefix, "_widgets"))
        widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% 
          dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)

        # Get widget group_id
        distinct_groups <- unique(widgets$group_id)

        # Reset opened cards
        r[[paste0(prefix, "_opened_cards")]] <- ""

        # Loop over distinct cards (tabs elements), for this tab
        sapply(distinct_groups, function(group_id){
          
          # Get name of widget
          widget_name <- widgets %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)

          toggles <<- tagList(toggles,
            shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
            div(class = "toggle_title", widget_name, style = "padding-top:10px;"))

          # Add to the list of opened cards
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
        })
        
        # Does this tab have sub-tabs ?
        if (r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == tab_id) %>% nrow() > 0) toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
            )
          )
        )
        
        else toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(style = "width:20px;"),
              toggles
            )
          )
        )

        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", tab_id))

        # Show opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        # Hide edit tab card
        shinyjs::hide(paste0(prefix, "_edit_tab"))
        
        # Add tab toggles UI
        output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
      })
      
      # --- --- --- --- --- --- --
      ## Debug - Execute code ----
      # --- --- --- --- --- --- --
      
      # observeEvent(input$execute_code, {
      #   
      #   if (debug) print(paste0(Sys.time(), " - mod_data - ", id, " - observer input$execute_code"))
      #   
      #   print("cards = ")
      #   print(r[[paste0(prefix, "_cards")]])
      #   print(paste0("opened cards = ", r[[paste0(prefix, "_opened_cards")]]))
      #   print(paste0("selected tab = ", r[[paste0(prefix, "_selected_tab")]]))
      #   print(paste0("load_display_tabs = ", r[[paste0(prefix, "_load_display_tabs")]]))
      #   print(paste0("first tab shown = ", r[[paste0(prefix, "_first_tab_shown")]] %>% dplyr::pull(id)))
      #   print("display tabs")
      #   print("")
      #   print(r[[paste0(prefix, "_display_tabs")]])
      #   print(paste0("load ui cards = ", r[[paste0(prefix, "_load_ui_cards")]]))
      #   print(paste0("stage = ", r[[paste0(prefix, "_load_ui_stage")]]))
      #   print("loaded studies")
      #   print(r[[paste0(prefix, "_loaded_studies")]])
      #   
      #   code <- input$ace_edit_code %>% stringr::str_replace_all("\r", "\n")
      #   
      #   output$code_result <- renderText({
      #     
      #     options('cli.num_colors' = 1)
      #     
      #     # Capture console output of our code
      #     captured_output <- capture.output(
      #       tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      #     
      #     # Restore normal value
      #     options('cli.num_colors' = NULL)
      #     
      #     # Display result
      #     paste(strwrap(captured_output), collapse = "\n")
      #   })
      # })
  })
}
