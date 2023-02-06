#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_patient_and_aggregated_data_ui <- function(id = character(), i18n = R6::R6Class()){
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
  
  # --- --- --- --- --- --- -
  # Module creation card ----
  # --- --- --- --- --- --- -
  
  module_creation_options <- list(
    list(key = "same_level", text = i18n$t("same_level_current_tab")),
    list(key = "level_under", text = i18n$t("level_under"))
  )
  
  module_creation_card <- make_card(
    title = i18n$t("add_module"),
    content = div(
      actionButton(ns(paste0(prefix, "_close_add_module")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
        make_textfield_new(ns = ns, label = "name", id = "module_name", width = "300px", i18n = i18n),
        div(shiny.fluent::ChoiceGroup.shinyInput(ns("add_module_type"), value = "same_level", 
          options = module_creation_options, className = "inline_choicegroup"), style = "padding-top:35px;")
      ), br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("add_module_button"), i18n$t("add")), br(),
    )
  )
  
  # --- --- --- --- --- --- --- --- -
  # Module element creation card ----
  # --- --- --- --- --- --- --- --- -
  
  if (prefix == "patient_lvl"){
    module_element_creation_card <- make_card(
      title = i18n$t("add_module_element"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_module_element")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield_new(i18n = i18n, ns = ns, label = "name", id = "module_element_name", width = "300px"),
          make_combobox_new(i18n = i18n, ns = ns, label = "plugin", id = "plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px")
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_combobox_new(i18n = i18n, ns = ns, label = "thesaurus", id = "thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
          make_dropdown_new(i18n = i18n, ns = ns, label = "items_mapping", id = "thesaurus_mapping", multiSelect = TRUE, width = "300px",
            options = list(
              list(key = 1, text = i18n$t("equivalent_to")),
              list(key = 2, text = i18n$t("included_in")),
              list(key = 3, text = i18n$t("include"))
            )
          ),
          conditionalPanel(condition = "input.thesaurus_mapping != null & input.thesaurus_mapping != ''", ns = ns, 
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_merge_mapped_items")), value = TRUE), style = "margin-top:45px;"),
                div(i18n$t("merge_mapped_items"), style = "font-weight:bold; margin-top:45px;")
              ),
              style = "margin-left:-28px;"
            )
          )
          # make_toggle_new(i18n = i18n, ns = ns, id = paste0(prefix, "_merge_mapped_items"), label = "merge_mapped_items", inline = TRUE)
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          # make_dropdown_new(i18n = i18n, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items", multiSelect = TRUE, width = "650px"),
          
          div(
            div(id = ns("thesaurus_selected_items_title"), class = "input_title", i18n$t("thesaurus_selected_items")),
            div(shiny.fluent::Dropdown.shinyInput(ns("thesaurus_selected_items"), value = NULL, options = list(), multiSelect = TRUE,
              onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-thesaurus_selected_items_trigger', Math.random())"))), style = "width:650px;")
          ),
          
          div(shiny.fluent::DefaultButton.shinyInput(ns("reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
        ),
        div(DT::DTOutput(ns("module_element_thesaurus_items")), class = "thesaurus_table"), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element_button"), i18n$t("add_widget")))#, br(),
        # DT::DTOutput(ns("thesaurus_items"))
      )
    )
    
    module_element_settings_card <- make_card(
      title = i18n$t("module_element_settings"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_module_element_settings")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield_new(i18n = i18n, ns = ns, label = "name", id = "module_element_settings_name", width = "300px", disabled = TRUE),
          make_textfield_new(i18n = i18n, ns = ns, label = "plugin", id = "module_element_settings_plugin", width = "300px", disabled = TRUE)
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_combobox_new(i18n = i18n, ns = ns, label = "thesaurus", id = "module_element_settings_thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
          make_dropdown_new(i18n = i18n, ns = ns, label = "items_mapping", id = "module_element_settings_thesaurus_mapping", multiSelect = TRUE, width = "300px",
            options = list(
              list(key = 1, text = i18n$t("equivalent_to")),
              list(key = 2, text = i18n$t("included_in")),
              list(key = 3, text = i18n$t("include"))
            )
          ),
          conditionalPanel(condition = "input.module_element_settings_thesaurus_mapping != null & input.module_element_settings_thesaurus_mapping != ''", ns = ns, 
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_module_element_settings_merge_mapped_items")), value = TRUE), style = "margin-top:45px;"),
                div(i18n$t("merge_mapped_items"), style = "font-weight:bold; margin-top:45px;")
              ),
              style = "margin-left:-28px;"
            )
          )
          # make_toggle_new(i18n = i18n, ns = ns, id = paste0(prefix, "_merge_mapped_items"), label = "merge_mapped_items", inline = TRUE)
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          # make_dropdown_new(i18n = i18n, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items", multiSelect = TRUE, width = "650px"),
          
          div(
            div(id = ns("module_element_settings_thesaurus_selected_items_title"), class = "input_title", i18n$t("thesaurus_selected_items")),
            div(shiny.fluent::Dropdown.shinyInput(ns("module_element_settings_thesaurus_selected_items"), value = NULL, options = list(), multiSelect = TRUE,
              onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-module_element_settings_thesaurus_selected_items_trigger', Math.random())"))), style = "width:650px;")
          ),
          
          div(shiny.fluent::DefaultButton.shinyInput(ns("module_element_settings_reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
        ),
        div(DT::DTOutput(ns("module_element_settings_thesaurus_items")), class = "thesaurus_table"), br(),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("edit_module_element_settings_button"), i18n$t("save")))#, br(),
        # DT::DTOutput(ns("thesaurus_items"))
      )
    )
    
  }
  
  if (prefix == "aggregated"){
    module_element_creation_card <- make_card(
      title = i18n$t("add_module_element"),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_module_element")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(language = language, ns = ns, label = "name", id = "module_element_name", width = "300px", words = words),
          make_combobox(language = language, ns = ns, label = "plugin", id = "plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px", words = words)
        ),
        br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element_button"), i18n$t("add")),
      )
    )
    module_element_settings_card <- div()
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("module_delete_confirm")), shiny.fluent::reactOutput(ns("module_element_delete_confirm")),
    div(id = ns("initial_breadcrumb"),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "main", text = i18n$t(paste0(prefix, "_data")), href = paste0("#!/", page_name), isCurrentItem = TRUE)),
        maxDisplayedItems = 3)
    ),
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_study_and_datamart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(uiOutput(ns("study_menu"))),
    div(id = ns("study_cards")),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_module_element")),
        module_element_creation_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_module_element_settings")),
        module_element_settings_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_module")),
        module_creation_card,
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

mod_patient_and_aggregated_data_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  o = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    language <- "EN"
    
    sapply(1:6, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- --- -
    # Summary ----
    # --- --- --- --- --- --- -
    
    # - INITIATE VARS
    # - HELP ON THIS PAGE
    # - LOAD DATA
    # - INITIATE UI
    # - LOAD UI
    # --- LOAD UI MODULES
    # --- RENDER MODULES MENU
    # --- RENDER MODULES ELEMENTS
    # - LOAD SERVER
    # - OTHER SERVER REACTIVITY
    # --- SORTABLE / CHANGE PIVOTITEMS ORDER
    # --- SHOW / HIDE DIV WHEN PIVOT ITEM SELECTED
    # --- ADD A MODULE
    # --- DELETE A MODULE
    # --- ADD A MODULE ELEMENT
    # --- DELETE A MODULE ELEMENT
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar_new(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n, ns = ns))
    observeEvent(r$show_message_bar2, show_message_bar_new(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --
    # Initiate vars ----
    # --- --- --- --- --
    
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
    
    r[[paste0(prefix, "_open_help_panel")]] <- FALSE
    r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- TRUE
    r[[paste0(prefix, "_open_help_modal")]] <- FALSE
    r[[paste0(prefix, "_help_modal_text")]] <- div()
    r[[paste0(prefix, "_help_modal_title")]] <- ""
    
    # Initiate var for already loaded studies, so that a UI element is not loaded twice
    r[[paste0(prefix, "_loaded_studies")]] <- integer()
    
    # Initiate var for list of cards
    r[[paste0(prefix, "_cards")]] <- character()
    
    # Load page from header
    
    observe({
      if (prefix == "aggregated" & shiny.router::get_page() == "data" & r$data_page == "patient_level_data") shiny.router::change_page("patient_level_data")
      else if (prefix == "patient_lvl" & shiny.router::get_page() == "data" & r$data_page == "aggregated_data") shiny.router::change_page("aggregated_data")
      
      # Close help pages when page changes
      r[[paste0(prefix, "_open_help_panel")]] <- FALSE
      r[[paste0(prefix, "_open_help_modal")]] <- FALSE
    })
    
    # Refresh reactivity
    observe({
      shiny.router::get_query_param()
      shinyjs::hide("study_cards")
      shinyjs::show("study_cards")
    })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    # Help panel
    
    observeEvent(input$help, if (id == shiny.router::get_page()) r[[paste0(prefix, "_open_help_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0(prefix, "_open_help_panel")]] <- FALSE)
    
    help_panel_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
      "EN", "load_data", "Load data",
      "FR", "load_data", "Charger des données",
      "EN", "data_model", "Understanding the data model",
      "FR", "data_model", "Comprendre le modèle de données",
      "EN", "ind_or_agg_data", "Individual or aggregated data ?",
      "FR", "ind_or_agg_data", "Données individuelles ou agrégées ?",
      "EN", "whats_a_module", "What is a tab ?",
      "FR", "whats_a_module", "Qu'est ce qu'un onglet ?",
      "EN", "add_module", "Add a tab",
      "FR", "add_module", "Ajouter un onglet",
      "EN", "delete_module", "Delete a tab",
      "FR", "delete_module", "Supprimer un onglet",
      "EN", "modules_elements", "Modules",
      "FR", "modules_elements", "Modules",
      "EN", "whats_a_module_element", "What is a module ?",
      "FR", "whats_a_module_element", "Qu'est ce qu'un module ?",
      "EN", "add_module_element", "Add a module",
      "FR", "add_module_element", "Ajouter un module",
      "EN", "delete_modul_elemente", "Delete a module",
      "FR", "delete_module_element", "Supprimer un module"
    )
    
    output$help_panel <- shiny.fluent::renderReact({
      
      shiny.fluent::Panel(
        headerText = translate(language, "help", r$words),
        isOpen = r[[paste0(prefix, "_open_help_panel")]], br(),
        strong(translate(language, "data", r$words)), br(), br(),
        shiny.fluent::Link(translate(language, "load_data", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_1', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "data_model", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_2', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "ind_or_agg_data", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_3', Math.random()); }"))), br(), br(),
        strong(translate(language, "modules", r$words)), br(), br(),
        shiny.fluent::Link(translate(language, "whats_a_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_4', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "add_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_5', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "delete_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_6', Math.random()); }"))), br(), br(),
        strong(translate(language, "modules_elements", help_panel_words)), br(), br(),
        shiny.fluent::Link(translate(language, "whats_a_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_7', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "add_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_8', Math.random()); }"))), br(), br(),
        shiny.fluent::Link(translate(language, "delete_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_9', Math.random()); }"))), br(), br(),
        isLightDismiss = r[[paste0(prefix, "_open_help_panel_light_dismiss")]],
        isBlocking = r[[paste0(prefix, "_open_help_panel_light_dismiss")]],
        onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
        onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
      )
    })
    
    # Help modal
    
    observeEvent(input$show_modal, r[[paste0(prefix, "_open_help_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0(prefix, "_open_help_modal")]] <- FALSE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- TRUE
    })
    
    observeEvent(input$help_1, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Charger des données"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p(strong("1) Choisir un datamart")),
          p("Un datamart est un ", strong("set de données"), ", auquel vous avez accès. Il contient les données d'un ", strong("groupe de patients"), "."),
          p("Un même datamart peut contenir ", strong("plusieurs études différentes"), "."),
          p("Choisissez le datamart sur la gauche de l'écran, dans le menu déroulant."),
          p(strong("2) Chosir une étude")),
          p("Choisissez ensuite une étude, dans le menu déroulant."),
          p("Une même étude peut contenir ", strong("plusieurs subsets différents"), "."),
          p(strong("3) Chosir un subset")),
          p("Il existe trois subsets par défaut :"),
          tags$ul(
            tags$li("Tous les patients"),
            tags$li("Patients inclus"),
            tags$li("Patients exclus")
          ),
          p("Il est possible de ", strong("créer d'autres subsets"), " dans l'onglet ", tags$em("Mes subsets"), " en haut de l'écran."),
          p(strong("4) Choisir un patient & un séjour")),
          p("En chargeant un subset, la liste des patients appartenant à ce subset est chargée dans le menu déroulant ", tags$em("Patient"), 
            ", seulement si l'on se trouve dans les ", tags$em("Données individuelles"), "."),
          p("Les modules se ", strong("mettent en jour"), " à chaque changement de patient & de séjour.")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_2, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Comprendre le modèle de données"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Au chargement d'un datamart, cinq variables se chargent."),
          p("Les colonnes de chaque variable sont détaillées, avec les noms complets et le type de colonne (integer, character etc)."),
          p(strong("1) d$patients")),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("gender"), " : Sexe (M / F) (character)"),
            tags$li(strong("age"), " : Age (character)"),
            tags$li(strong("dod"), " : Date de décès (dod pour date of death) (datetime)")
          ),
          p(strong("2) d$stays")),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("stay_id"), " : Identifiant unique du séjour hospitalier (integer)"),
            tags$li(strong("unit_name"), " : Nom de l'unité / du service du séjour hospitalier (character)"),
            tags$li(strong("admission_datetime"), " : Date & heure d'admission dans l'unité / dans le service (datetime)"),
            tags$li(strong("discharge_datetime"), " : Date & heure de sortie de l'unité / de service (datetime)")
          ),
          p(strong("3) d$labs_vitals")),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
            tags$li(strong("value"), " : Valeur textuelle (character)"),
            tags$li(strong("value_num"), " : Valeur numérique (numeric)"),
            tags$li(strong("unit"), " : Unité de la valeur (character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ),
          p(strong("4) d$orders")),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur (datetime)"),
            tags$li(strong("route"), " : Voie d'administration du médicament (character)"),
            tags$li(strong("continuous"), " : Administration continue ou non (logical)"),
            tags$li(strong("amount"), " : Quantité du médicament / de la prescription non médicamenteuse (numeric)"),
            tags$li(strong("amount_unit"), " : Unité de la quantité (character)"),
            tags$li(strong("rate"), " : Débit du médicament (numeric)"),
            tags$li(strong("rate_unit"), " : Unité du débit (character)"),
            tags$li(strong("concentration"), " : Concentration du médicament (numeric)"),
            tags$li(strong("concentration_unit"), " : Unité de la concentration(character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ),
          p(strong("5) d$text")),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
            tags$li(strong("value"), " : Valeur textuelle (character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          )
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_3, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Données individuelles ou agrégées ?"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Vous pouvez choisir dans le menu à gauche de charger les données individuelles ou agrégées."),
          p(strong("1) Modules & plugins différents")),
          p("Selon que vous choisissiez les données individuelles ou agrégées, les modules & plugins chargés diffèrent."),
          p("Lorsque vous chargez une étude, vous chargez :"),
          tags$ul(
            tags$li("D'un côté les onglets & modules de données individuelles, permettant de ", strong("visualiser les données patient par patient")),
            tags$li("De l'autre côté les onglets & modules de données agrégées, permettant de ", strong("visualiser les données sur l'ensemble des patients ou sur le subset sélectionné"))
          ),
          p("En pratique, cela crée de ", strong("nouvelles variables"), " filtrant les variables générales sur ", 
            strong("le subset, le patient ou sur le séjour sélectionné"), "."),
          p(strong("2) Données agrégées - Variables du subset sélectionné")),
          p("Lorsque vous sélectionnez un subset, les variables suivantes sont créées, avec la même structure que détaillée dans ", tags$em("Comprendre le modèle de données"), " :"),
          tags$ul(
            tags$li(strong("d$data_subset$patients")),
            tags$li(strong("d$data_subset$stays")),
            tags$li(strong("d$data_subset$labs_vitals")),
            tags$li(strong("d$data_subset$orders")),
            tags$li(strong("d$data_subset$text"))
          ),
          p(strong("3) Données individuelles - Variables du patient sélectionné")),
          p("Lorsque vous sélectionnez un patient, les variables suivantes sont créées :"),
          tags$ul(
            tags$li(strong("r$data_patient$stays")),
            tags$li(strong("r$data_patient$labs_vitals")),
            tags$li(strong("r$data_patient$orders")),
            tags$li(strong("r$data_patient$text"))
          ),
          p(strong("4) Données individuelles - Variables du séjour sélectionné")),
          p("De la même façon, lorsque vous sélectionnez un séjour, les variables suivantes sont créées :"),
          tags$ul(
            tags$li(strong("d$data_stay$labs_vitals")),
            tags$li(strong("d$data_stay$orders")),
            tags$li(strong("d$data_stay$text"))
          )
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_4, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Qu'est ce qu'un onglet ?"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Une étude est ", strong("structurée autour d'onglets"), ", qui sont en quelque sorte des ", strong("pages personnalisées"),
            " sur lesquelles je choisis ", strong("quelles données afficher et sous quelle forme"), "."),
          p(strong("1) Onglets de données individuelles")),
          p("Les onglets de données individuelles ", strong("reproduisent un dossier clinique"), "."),
          p("Par exemple, si je fais une étude sur le choc septique, je crée un onglet de données individuelles ", tags$em("Hémodynamique"),
              " où j'affiche la FC, la PAs, la PAd, la PAm & les doses reçues de Noradrénaline."),
          p(strong("2) Onglets de données agrégées")),
          p("Les onglets de données agrégées ", strong("permettent de conduire une étude"), " sur mes données."),
          p("Sur cette même étude, je choisis de créer un onglet ", tags$em("Critères d'exclusion"), " où je vais créer mes critères ",
            "d'exclusion et les appliquer à mes patients."),
          p("Je peux également créer un onglet ", tags$em("Flowchart"), " pour afficher le flowchart de mon étude.")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_5, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Ajouter un onglet"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Pour ajouter un onglet, il faut ", strong("avoir chargé une étude"), " dans le menu déroulant à gauche de l'écran."),
          p("Il faut ensuite cliquer sur l'icône :"),
          div(shiny.fluent::Icon(iconName = "Add"), span(translate(language, "add_module", r$words), style = "padding:0px 0px 10px 10px;")), 
          p("Elle se trouve sous le titre (", tags$em("Données individuelles"), " ou ",
            tags$em("Données agrégées"), ")."),
          p("Ensuite, :"),
          tags$ul(
            tags$li(strong("Choisissez un nom"), " pour cet onglet"),
            tags$li(strong("Choisissez le niveau "), "de l'onglet. Faut-il qu'il soit au même niveau que l'onglet actuel,",
              " ou est-ce un sous-onglet de l'onglet actuellement sélectionné ?")
          ),
          p("Lorsque le menu ", tags$em("Ajouter un onglet"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux modules.")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_6, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Supprimer un onglet"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Pour supprimer un onglet, ", strong("cliquez sur l'onglet en question"), " puis cliquez sur :"),
          div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_help")), 
            translate(language, "remove_module", isolate(r$words)), iconProps = list(iconName = "Delete"))),
          p("Supprimer un onglet supprime ", strong("tous les modules situés dans cet onglet"),
            ", ainsi que ", strong("tous les onglets situés sous cet onglet"), ".")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_7, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Qu'est ce qu'un module ?"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Un onglet est ", strong("composé de modules"), ", qui sont des plugins appliqués à des données."),
          p(strong("1) Plugins")),
          p("Les plugins sont des scripts écrits en R - Shiny, permettant ", strong("d'ajouter une fonctionnalité à l'application"), "."),
          p("Quelques exemples :"),
          tags$ul(
            tags$li(strong("Plugin Datatable"), " : permet d'afficher des données sous forme de tableau."),
            tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, utile pour les prescriptions par exemple."),
            tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir des données d'une étude.")
          ),
          p("L'application a vocation à s'enrichir au fur et à mesure par la ", strong("création de nouveaux plugins"), "."),
          p("Les plugins des données individuelles ou agrégées ne sont pas les mêmes."),
          p(strong("2) Modules")),
          p("Un module est donc un plugin appliqué à des données."),
          p("Je choisis un plugin, quelles données vont être utilisées par ce plugin, puis le ",
            strong("plugin affiche ces données sous la forme désirée"), " (timeline pour le plugin timeline etc).")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_8, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Ajouter un module"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Pour ajouter un module, il faut ", strong("avoir chargé une étude "), " dans le menu déroulant à gauche de l'écran puis ",
            strong("avoir sélectionné un onglet"), "."),
          p("Il faut ensuite cliquer sur :"),
            div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_help")), 
              translate(language, "new_module_element", isolate(r$words)), iconProps = list(iconName = "Add"))),
          p("Ensuite, :"),
          tags$ul(
            tags$li(strong("Choisissez un nom"), " pour ce module"),
            tags$li(strong("Choisissez le plugin "), " que vous souhaitez utiliser pour ce module")
          ),
          p("S'il s'agit d'un module de données agrégées, cliquez sur Ajouter et c'est terminé."),
          p("S'il s'agit d'un module de données individuelles, vous devez :"),
          tags$ul(
            tags$li(strong("Sélectionner un thésaurus"), " : un thésaurus est un dictionnaire de concepts utilisés par un datamart."),
            tags$li(strong("Sélectionner les items "), " que vous souhaitez utiliser pour cet élément de module, avec le plugin sélectionné.")
          ),
          p("Lorsque le tableau des ", strong("items du thésaurus"), " est chargé, vous pouvez filtrer les données pour trouver les items qui vous intéresent :"),
          tags$ul(
            tags$li(strong("Nom d'affichage"), " : cherchez dans la barre de texte les items. En double-cliquant sur un nom, vous pouvez le changer : il sera affiché avec ce nouveau nom."),
            tags$li(strong("Catégorie"), " : filtrez les données sur les catégories, plusieurs catégories peuvent être sélectionnées à la fois."),
            tags$li(strong("Unité"), " : utile essentiellement pour changer l'affichage de l'unité."),
            tags$li(strong("Couleur de l'item"), " : utile pour différencier les items sur un graphique par ex."),
            tags$li(strong("Patients"), " : affiche le nombre total de patients ayant au moins une fois l'item."),
            tags$li(strong("Lignes"), " : nombre d'occurences de l'item dans le datamart, tous patients confondus.")
          ),
          p("Ajouter ensuite les items en cliquant sur l'icône "),
          div(actionButton(ns(paste0(prefix, "_add_thesaurus_item_help")), "", icon = icon("plus"))),
          p(" dans la dernière colonne du tableau."),
          p("Lorsque le menu ", tags$em("Nouveau module"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux modules.")
          
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    observeEvent(input$help_9, {
      
      if (language == "FR"){
        r[[paste0(prefix, "_help_modal_title")]] <- "Supprimer un module"
        
        r[[paste0(prefix, "_help_modal_text")]] <- div(
          p("Pour supprimer un module, ", strong("cliquez sur "), " :"),
          div(actionButton(ns(paste0(prefix, "_remove_module_element_help")), "", icon = icon("trash-alt"))),
          p("Cette icône se trouve en haut à droite du module.")
        )
      }
      
      else if (language == "EN"){
        r[[paste0(prefix, "_help_modal_title")]] <- ""
        r[[paste0(prefix, "_help_modal_text")]] <- div()
      }
      
      r[[paste0(prefix, "_open_help_modal")]] <- TRUE
      r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
      
    })
    
    output$help_modal <- shiny.fluent::renderReact({
      
      shiny.fluent::Modal(
        isOpen = r[[paste0(prefix, "_open_help_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
        onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
        div(
          style = "width: 1000px; padding: 15px 10px 0px 15px;",
          shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
            div(style = list(display = "flex"),
              shiny.fluent::Text(r[[paste0(prefix, "_help_modal_title")]], variant = "large"),
              div(style = list(flexGrow = 1)),
              shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
            ),
            r[[paste0(prefix, "_help_modal_text")]]
          )
        )
      )
    })
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    if (prefix == "patient_lvl"){
      
      observeEvent(m$chosen_patient, {
        
        # Reset variables
        d$data_patient$stays <- tibble::tibble()
        d$data_patient$labs_vitals <- tibble::tibble()
        d$data_patient$text <- tibble::tibble()
        d$data_patient$orders <- tibble::tibble()
        d$data_stay$labs_vitals <- tibble::tibble()
        d$data_stay$text <- tibble::tibble()
        d$data_stay$orders <- tibble::tibble()
        
        if (length(m$chosen_patient) > 0){
          if (!is.na(m$chosen_patient) & m$chosen_patient != ""){
            if (nrow(d$stays) > 0) d$data_patient$stays <- d$stays %>% dplyr::filter(patient_id == m$chosen_patient) %>% dplyr::arrange(admission_datetime)
            if (nrow(d$labs_vitals) > 0) d$data_patient$labs_vitals <- d$labs_vitals %>% dplyr::filter(patient_id == m$chosen_patient)
            if (nrow(d$text) > 0) d$data_patient$text <- d$text %>% dplyr::filter(patient_id == m$chosen_patient)
            if (nrow(d$orders) > 0) d$data_patient$orders <- d$orders %>% dplyr::filter(patient_id == m$chosen_patient)
          }
        }
      })
      
      observeEvent(m$chosen_stay, {
        
        req(r$data_patient)
        
        d$data_stay <- list()
        
        if (length(m$chosen_stay) > 0){
          if (!is.na(m$chosen_stay) & m$chosen_stay != ""){
            
            d$data_stay$stay <- d$data_patient$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::select(admission_datetime, discharge_datetime)
            
            if (nrow(d$data_patient$labs_vitals) > 0) d$data_stay$labs_vitals <- d$data_patient$labs_vitals %>% dplyr::filter(datetime_start >= d$data_stay$stay$admission_datetime & datetime_start <= d$data_stay$stay$discharge_datetime)
            if (nrow(d$data_patient$text) > 0) d$data_stay$text <- d$data_patient$text %>% dplyr::filter(datetime_start >= d$data_stay$stay$admission_datetime & datetime_start <= d$data_stay$stay$discharge_datetime)
            if (nrow(d$data_patient$orders) > 0) d$data_stay$orders <- d$data_patient$orders %>% dplyr::filter(datetime_start >= d$data_stay$stay$admission_datetime & datetime_start <= d$data_stay$stay$discharge_datetime)
          }
        }
      })
    }
    
    if (prefix == "aggregated"){
      
      observeEvent(m$chosen_subset, {
        
        d$data_subset <- list()
        patients <- tibble::tibble()
        
        # Reset variables
        vars <- c("patients", "stays", "labs_vitals", "text", "orders", "diagnoses")
        lapply(vars, function(var) d$data_subset[[var]] <- tibble::tibble())
        
        if (length(m$chosen_subset) > 0){
          if (!is.na(m$chosen_subset) & m$chosen_subset != "") patients <- m$subset_patients %>% dplyr::filter(subset_id == m$chosen_subset)
        }
        
        if (nrow(patients) > 0){
          patients <- patients %>% dplyr::select(patient_id)
          lapply(vars, function(var) if (nrow(d[[var]]) > 0) d$data_subset[[var]] <- d[[var]] %>% dplyr::inner_join(patients, by = "patient_id"))
        }
      })
    }
    
    # --- --- --- -- -
    # Initiate UI ----
    # --- --- --- -- -
    
    # When a study is chosen
    
    observeEvent(m$chosen_study, {
      
      # Hide UI from previous loaded study
      # Don't use removeUI, cause when you switch study, it is deleted and cannot be reinserted
      
      sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
      
      req(!is.na(m$chosen_study))
      
      # removeUI(selector = paste0("#", ns(r[[paste0(prefix, "_cards")]])), multiple = TRUE)

      # Initiiate vector of study cards
      # r[[paste0(prefix, "_cards")]] <- character()

      # Hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::hide("initial_breadcrumb")
      shinyjs::show("study_menu")

      # Reset selected key
      r[[paste0(prefix, "_selected_module")]] <- NA_integer_
      
      # Reset shown modules
      r[[paste0(prefix, "_opened_cards")]] <- ""

      # Hide Add module element card & Add module
      shinyjs::hide("add_module_element")
      shinyjs::hide("add_module")

      r[[paste0(prefix, "_load_display_modules")]] <- paste0("first_load_ui_", Sys.time())

      # Run observers
      r[[paste0(prefix, "_load_server")]] <- Sys.time()
      
      r[[paste0(prefix, "_load_ui_stage")]] <- "first_time"

      # Load modules variables for this study
      update_r_new(r = r, m = m, table = paste0(prefix, "_modules_families"))
      update_r_new(r = r, m = m, table = paste0(prefix, "_modules"))
      update_r_new(r = r, m = m, table = paste0(prefix, "_modules_elements"))
      update_r_new(r = r, m = m, table = paste0(prefix, "_modules_elements_items"))
      
    })
    
    # Load study display modules
    
    observeEvent(r[[paste0(prefix, "_load_display_modules")]], {
      # Load study informations
      # For one study, you choose ONE patient_lvl or aggregated data module family
      study_infos <- r$studies %>% dplyr::filter(id == m$chosen_study)
      # study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", m$chosen_study))
      
      # Check if users has access only to aggregated data
      r$options %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart & name == "show_only_aggregated_data") %>%
        dplyr::pull(value_num) -> r[[paste0(prefix, "_show_only_aggregated_data")]]
      
      # Load modules belonging to this module family
      # display_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, "_modules WHERE module_family_id = ",
      #   study_infos[[paste0(prefix, "_module_family_id")]], " AND deleted IS FALSE"))
      display_modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == study_infos[[paste0(prefix, "_module_family_id")]]) 
      
      # Modules without parent are set to level 1
      display_modules <- display_modules %>% 
        dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))
      
      # Prevent infinite loop, max loops = 7
      i <- 1
      
      # Creating levels for distinct modules
      while(nrow(display_modules %>% dplyr::filter(is.na(level))) > 0 & i <= 7){
        display_modules <-
          display_modules %>%
          dplyr::left_join(display_modules %>%
            dplyr::filter(!is.na(level)) %>%
            dplyr::transmute(parent_module_id = id, parent_level = level), by = "parent_module_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
        i <- i + 1
      }
      
      # Exclude modules without level
      display_modules <- display_modules %>% dplyr::filter(!is.na(level))
      
      # Order by display order
      display_modules <- display_modules %>% dplyr::arrange(level, display_order)
      
      # Calculate first module shown in the menu
      if(nrow(display_modules) > 0 & "level" %in% names(display_modules) & !is.na(m$chosen_study)){
        
        # First module shown
        first_module_shown <- display_modules %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
        if (max(display_modules$level) >= 2){
          sapply(2:max(display_modules$level), function(current_level){
            children <- display_modules %>% dplyr::filter(level == current_level, parent_module_id == first_module_shown$id) %>% dplyr::slice(1)
            if (nrow(children) > 0) first_module_shown <<- children
          })
        }
        
        r[[paste0(prefix, "_first_module_shown")]] <- first_module_shown
      }
      
      r[[paste0(prefix, "_display_modules")]] <- display_modules
      
      # Load UI cards
      if (grepl("first_load_ui", r[[paste0(prefix, "_load_display_modules")]])) r[[paste0(prefix, "_load_ui_cards")]] <- Sys.time()
    })
    
    # --- --- -- -
    # Load UI ----
    # --- --- -- -
    
      # --- --- --- --- --- --
      ## Render tabs menu ----
      # --- --- --- --- --- --
      
      # Render menu
      output$study_menu <- renderUI({
        # The output is reloaded in these conditions :
        # - Change in r[[paste0(prefix, "_display_modules")]]
        # - Change in r[[paste0(prefix, "_load_ui_menu")]]
        # - Change in input$study_current_tab
        
        req(!is.na(m$chosen_study))
        req(r[[paste0(prefix, "_display_modules")]])
        r[[paste0(prefix, "_load_ui_menu")]]
        
        # Hide initial breadcrumb
        shinyjs::hide("initial_breadcrumb")
        
        # Check if users has access only to aggregated data
        if (prefix == "patient_lvl" & isolate(r[[paste0(prefix, "_show_only_aggregated_data")]]) == 1) show_message_bar_new(output, 1, "only_aggregated_data_authorized", "severeWarning", i18n = i18n, ns = ns)
        req((prefix == "patient_lvl" & isolate(r[[paste0(prefix, "_show_only_aggregated_data")]]) != 1) | prefix == "aggregated")
        
        display_modules <- isolate(r[[paste0(prefix, "_display_modules")]])
        
        # If no module to show, notify user
        if (nrow(display_modules) == 0 | "level" %not_in% names(display_modules)){
          
          # selected_key <- 0L
          # if (!is.na(isolate(r[[paste0(prefix, "_selected_module")]]))) selected_key <- isolate(r[[paste0(prefix, "_selected_module")]])
          
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
              shiny.fluent::PivotItem(id = paste0(prefix, "_add_module_", 0), headerText = span(i18n$t("add_module"), style = "padding-left:5px;"), itemIcon = "Add")
            )
          ))
        }
        
        req(nrow(display_modules) > 0 & "level" %in% names(display_modules) & !is.na(isolate(m$chosen_study)))
        
        # First module shown
        first_module_shown <- isolate(r[[paste0(prefix, "_first_module_shown")]])
        
        shown_modules_temp <- tibble::tibble()
        
        # If we are at level one, show all levels one
        if (first_module_shown$level == 1){
          shown_modules <- display_modules %>% dplyr::filter(level == 1)
        }
        
        # Else, show only current & those who has same level & same parent
        if (first_module_shown$level > 1){
          shown_modules_temp <- display_modules %>% dplyr::filter(level == first_module_shown$level & parent_module_id == first_module_shown$parent_module_id)
          if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
          if (nrow(shown_modules_temp) == 0) shown_modules <- first_module_shown
        }
        
        if (length(input$study_current_tab) > 0){
          
          # If value = 0, go back to first level
          if (input$study_current_tab == 0){
            shown_modules <- display_modules %>% dplyr::filter(level == 1)
            r[[paste0(prefix, "_selected_module")]] <- first_module_shown$id
          } 
          else {
            
            if (grepl("show_module", isolate(r[[paste0(prefix, "_selected_module")]]))){
              study_current_tab <- as.integer(substr(isolate(r[[paste0(prefix, "_selected_module")]]), nchar("show_module_") + 1, 100))
              shown_modules_temp <- display_modules %>% dplyr::filter(parent_module_id == study_current_tab)
            }
            else if (grepl("add_module", input$study_current_tab)){
              shown_modules_temp <- tibble::tibble()
              study_current_tab <- isolate(r[[paste0(prefix, "_selected_module")]])
            }
            else {
              study_current_tab <- input$study_current_tab
              shown_modules_temp <- display_modules %>% dplyr::filter(parent_module_id == study_current_tab)
            }
            
            # If current tab has children
            if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
            
            # If current tab has no children
            if (nrow(shown_modules_temp) == 0){
              current_module <- display_modules %>% dplyr::filter(id == study_current_tab)
              if (nrow(current_module) > 0) shown_modules <- display_modules %>% dplyr::filter(parent_module_id == current_module$parent_module_id & level == current_module$level)
              else show_modules <- tibble::tibble()
              
              # If not any "brother", we are at level one
              if (nrow(shown_modules) == 0){
                shown_modules <- display_modules %>% dplyr::filter(level == 1)
              }
            }
          }
        }
        
        # Currently selected tab
        
        # We have just deleted a module
        if (grepl("show_module", isolate(r[[paste0(prefix, "_selected_module")]]))){
          r[[paste0(prefix, "_selected_module")]] <- 
            as.integer(substr(isolate(r[[paste0(prefix, "_selected_module")]]), nchar("show_module_") + 1, 100))
        }
        
        # First existing module or load another study
        else if (length(input$study_current_tab) == 0 | grepl("first_time", isolate(r[[paste0(prefix, "_load_ui_stage")]]))){
          r[[paste0(prefix, "_selected_module")]] <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
        }
        
        # We have clicked on a tab
        else if (length(input$study_current_tab) > 0){
          
          # Current module has children, take the first of this level of modules
          if (nrow(shown_modules_temp) > 0) r[[paste0(prefix, "_selected_module")]] <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
          
          # Take the input as current module
          else if (!grepl("add_module", input$study_current_tab) & input$study_current_tab != 0) r[[paste0(prefix, "_selected_module")]] <- input$study_current_tab
        }
        
        nb_levels <- max(shown_modules$level)
        
        # First level
        is_current_item <- FALSE
        if (nb_levels == 1) is_current_item <- TRUE
        
        items <- list(
          list(key = "main", text = i18n$t(page_name), href = paste0("#!/", page_name), isCurrentItem = is_current_item,
            onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', 0)"))))
        
        # Other levels
        if (nb_levels >= 2){
          
          # Remove last level
          modules_tree <- display_modules %>% dplyr::filter(level < nb_levels)
          
          current_parent <- NA_integer_
          sapply(nb_levels:1, function(current_level){
            if (!is.na(current_parent)){
              modules_tree <<- modules_tree %>% dplyr::filter(level != current_level | id == current_parent)
              current_parent <<- display_modules %>% dplyr::filter(id == current_parent) %>% dplyr::pull(parent_module_id)
            }
            if (is.na(current_parent)) current_parent <<- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(parent_module_id)
          })
          modules_tree <- modules_tree %>% dplyr::arrange(level)
          sapply(1:nrow(modules_tree), function(i){
            is_current_item <- FALSE
            if (modules_tree[[i, "level"]] == nb_levels) is_current_item <- TRUE
            items <<- rlist::list.append(items, list(
              key = modules_tree[[i, "name"]], text = modules_tree[[i, "name"]], href = paste0("#!/", page_name), isCurrentItem = is_current_item,
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', ", modules_tree[[i, "id"]], ")"))
            ))
          })
        }
        
        shown_tabs <- tagList()
        
        sapply(1:nrow(shown_modules), function(i){
          shown_tabs <<- tagList(shown_tabs, shiny.fluent::PivotItem(id = shown_modules[[i, "id"]], itemKey = shown_modules[[i, "id"]], headerText = shown_modules[[i, "name"]]))
        })
        
        # Add an add button, to add a new module
        shown_tabs <- tagList(shown_tabs, shiny.fluent::PivotItem(id = paste0(prefix, "_add_module_", isolate(r[[paste0(prefix, "_selected_module")]])), headerText = span(i18n$t("add_module"), style = "padding-left:5px;"), itemIcon = "Add"))
        
        r[[paste0(prefix, "_load_ui_stage")]] <- "end_loading_tabs_menu"
        
        tagList(
          shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3),
          shiny.fluent::Pivot(
            id = paste0(prefix, "_study_pivot"),
            onLinkClick = htmlwidgets::JS(paste0("item => {",
              "Shiny.setInputValue('", id, "-study_current_tab', item.props.id);",
              "Shiny.setInputValue('", id, "-study_current_tab_trigger', Math.random());",
              "}"
            )),
            selectedKey = isolate(r[[paste0(prefix, "_selected_module")]]),
            shown_tabs
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
        
        # Don't reload study UI if already loaded
        req(m$chosen_study %not_in% r[[paste0(prefix, "_loaded_studies")]])
        
        distinct_modules <- r[[paste0(prefix, "_display_modules")]] %>% dplyr::pull(id)

        code_ui <- tagList("")

        all_groups <- NA_integer_

        # Loop over distinct modules, for this study

        selected_module <- r[[paste0(prefix, "_selected_module")]]

        if (grepl("show_module", selected_module)) selected_module <- as.integer(substr(selected_module, nchar("show_module_") + 1, 100))

        sapply(distinct_modules, function(module_id){

          toggles <- tagList()

          module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == !!module_id) %>% 
            dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)

          if (nrow(module_elements) > 0){

            # Get module element group_id
            distinct_groups <- unique(module_elements$group_id)

            # Loop over distinct cards (modules elements), for this module

            sapply(distinct_groups, function(group_id){

              # if (module_id != r[[paste0(prefix, "_first_module_shown")]]$id) all_groups <- c(all_groups, group_id)

              # Load UI code for this module element
              plugin_id <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              if (length(plugin_id) != 0) code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)

              # Check if plugin has been deleted
              # check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id)) %>% dplyr::pull(deleted)
              check_deleted_plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
              if (nrow(check_deleted_plugin) == 0) code_ui_card <- paste0("div(shiny.fluent::MessageBar('", i18n$t("plugin_deleted"), "', messageBarType = 3), style = 'margin-top:10px;')")
              
              # if (check_deleted_plugin){
              #   code_ui_card <- paste0("div(shiny.fluent::MessageBar('", i18n$t("plugin_deleted"), "', messageBarType = 3), style = 'margin-top:10px;')")
              # }

              # Get name of module element
              module_element_name <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)

              # Append a toggle to our cards list
              r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_group_", group_id))

              toggles <<- tagList(toggles,
                shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
                div(class = "toggle_title", module_element_name, style = "padding-top:10px;"))

              # Try to run plugin UI code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                code_ui_card <- code_ui_card %>%
                  stringr::str_replace_all("%module_id%", as.character(module_id)) %>%
                  stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("\r", "\n")

                if (length(m$chosen_study) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%study_id%", as.character(m$chosen_study))

                # Module element card

                element_code <- div(
                  make_card("",
                    div(
                      div(id = ns(paste0(prefix, "_module_element_plugin_ui_", group_id)), eval(parse(text = code_ui_card))),
                      # shinyjs::hidden(
                      #   div(id = ns(paste0(prefix, "_module_element_settings_", group_id)),
                      #     shiny.fluent::Pivot(
                      #       id = ns(paste0(prefix, "_module_element_settings_pivot_", group_id)),
                      #       onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_module_element_settings_current_tab_", group_id, "', item.props.id)")),
                      #       shiny.fluent::PivotItem(id = "module_element_thesaurus_items", itemKey = "module_element_thesaurus_items", headerText = i18n$t("items")),
                      #       shiny.fluent::PivotItem(id = "module_element_settings", itemKey = "module_element_settings", headerText = i18n$t("settings"))
                      #     ),
                      #     conditionalPanel(condition = paste0(
                      #       "input.", prefix, "_module_element_settings_current_tab_", group_id, " == null | ",
                      #       "input.", prefix, "_module_element_settings_current_tab_", group_id, " == 'module_element_thesaurus_items'"), ns = ns,
                      #       DT::DTOutput(ns(paste0(prefix, "_module_element_settings_items_datatable_", group_id)))
                      #     ),
                      #     conditionalPanel(condition = paste0("input.", prefix, "_module_element_settings_current_tab_", group_id, " == 'module_element_settings'"), ns = ns,
                      #       div("settings"
                      # 
                      #       )
                      #     )
                      #     # p(span(i18n$t("widget_name"), style = "font-weight:bold;"), " : ", module_element_name),
                      #     # shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "_module_elements_settings_show_items_", group_id)), i18n$t("show_used_items"))
                      #   )
                      # ),
                      div(
                        id = ns(paste0(prefix, "_module_element_settings_remove_buttons_", group_id)),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                          actionButton(ns(paste0(prefix, "_settings_module_element_", group_id)), "", icon = icon("cog")),
                          actionButton(ns(paste0(prefix, "_remove_module_element_", group_id)), "", icon = icon("trash-alt"))
                        ),
                        style = "position:absolute; top:8px; right: 10px;"
                      )#,
                      # shinyjs::hidden(
                      #   div(
                      #     id = ns(paste0(prefix, "_module_element_quit_button_", group_id)),
                      #     actionButton(ns(paste0(prefix, "_quit_module_element_settings_", group_id)), "", icon = icon("times")),
                      #     style = "position:absolute; top:8px; right: 10px;"
                      #   )
                      # )
                    ),
                    style = "position:relative;"
                  )
                )

                ui_output <- uiOutput(ns(paste0(prefix, "_group_", group_id)))
                hide_div <- TRUE
                if (!is.na(selected_module)) if (module_id == selected_module) hide_div <- FALSE
                if (hide_div) ui_output <- shinyjs::hidden(ui_output)

                insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = ui_output)
                output[[paste0(prefix, "_group_", group_id)]] <- renderUI(element_code)
              },
              error = function(e){
                report_bug_new(r = r, output = output, error_message = i18n$t("error_run_plugin_ui_code"),
                  error_name = paste0(id, " - run ui code - ", group_id), category = "Error", error_report = e, i18n = i18n)
              })
            })
          }

          # Put all div together

          r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_toggles_", module_id))

          # Does this module have sub-modules ?
          if (r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == module_id) %>% nrow() > 0) toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
                div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
              )
            )
          )

          else toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", module_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_module_element_trigger', Math.random())"))),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
                #paste0("module_id = ", module_id),
                div(style = "width:20px;"),
                toggles
              )
            )
          )

          ui_output <- uiOutput(ns(paste0(prefix, "_toggles_", module_id)))
          hide_div <- TRUE
          if (!is.na(selected_module)) if (module_id == selected_module) hide_div <- FALSE
          if (hide_div) ui_output <- shinyjs::hidden(ui_output)

          insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = ui_output)
          output[[paste0(prefix, "_toggles_", module_id)]] <- renderUI(toggles_div)

        })
        
        # Indicate that this study has been loaded, so that UI elements aren't loaded twice
        r[[paste0(prefix, "_loaded_studies")]] <- c(r[[paste0(prefix, "_loaded_studies")]], m$chosen_study)

        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(100, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      })
    
    # --- --- --- --- --- ---
    # Close creation div ----
    # --- --- --- --- --- ---
    
    observeEvent(input[[paste0(prefix, "_close_add_module_element")]], {

      # Show opened cards before opening Add module element div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)

      # Hide Add module element div
      shinyjs::hide(paste0(prefix, "_add_module_element"))
    })

    
    # --- --- --- -- -
    # Load server ----
    # --- --- --- -- -
    
    observeEvent(r[[paste0(prefix, "_load_server")]], {

      req(!is.na(m$chosen_study))

      # Get modules elements, arrange them by display_order

      module_family <- r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
      modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == module_family) %>% dplyr::select(module_id = id)
      module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::inner_join(modules, by = "module_id") %>% dplyr::rename(group_id = id)
      module_elements_items <- r[[paste0(prefix, "_modules_elements_items")]] %>% dplyr::inner_join(module_elements %>% dplyr::select(group_id), by = "group_id")
      
      # --- --- --- --- --- --- --- -- -
      ## Delete tab & create widget ----
      # --- --- --- --- --- --- --- -- -

      # Loop over modules

      # sapply(modules$module_id, function(module_id){
      # 
      #   # --- --- --- --- --- --- --
      #   ### Create a new widget ----
      #   # --- --- --- --- --- --- --
      # 
      #   observeEvent(input[[paste0(prefix, "_add_module_element_", module_id)]], {
      # 
      #     # Hide opened cards
      #     sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      # 
      #     # Show Add module element div
      #     shinyjs::show(paste0(prefix, "_add_module_element"))
      # 
      #   })
      # 
      #   # --- --- --- --- - -
      #   ### Delete a tab ----
      #   # --- --- --- --- - -
      # 
      #   observeEvent(input[[paste0(prefix, "_remove_module_", module_id)]], {
      #     r[[module_delete_variable]] <- TRUE
      #   })
      # 
      # })

      # --- --- --- --- --- --- --- ---
      ## Run server code for cards ----
      # --- --- --- --- --- --- --- ---

      # If no thesaurus elements to show in this module, notify the user
      # if (nrow(module_elements) == 0) show_message_bar(output = output, id = 2, message = "no_module_element_to_show", type = "severeWarning", language = language)

      if (nrow(module_elements) > 0){

        # Get module element group_id
        distinct_groups <- unique(module_elements$group_id)

        toggles <- c()

        # Loop over distinct cards
        sapply(distinct_groups, function(group_id){

          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(prefix, "_", group_id, "_", m$chosen_study)
          # if (trace_code %in% r$server_modules_groups_loaded) print(trace_code)
          if (trace_code %not_in% r$server_modules_groups_loaded){

            # Add the trace_code to loaded plugins list
            r$server_modules_groups_loaded <- c(r$server_modules_groups_loaded, trace_code)

            # Server code for toggles reactivity
            toggle <- paste0(prefix, "_group_", group_id)
            observeEvent(input[[paste0(toggle, "_toggle")]], {
              req(r[[paste0(prefix, "_selected_module")]] == module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::distinct(module_id) %>% dplyr::pull())
              if (input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle)
            })

            # Get name of module element
            # module_element_name_escaping <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>%
            # dplyr::pull(name) %>% stringr::str_replace_all(c("-" = "_", "/" = "_", "\\(" = "_", "\\)" = "_"))

            # toggles <<- c(toggles, paste0(prefix, "_group_", group_id))

            if (prefix == "patient_lvl"){

              # Get thesaurus items with thesaurus own item_id
              # thesaurus_selected_items <- module_elements %>% dplyr::filter(group_id == !!group_id) %>%
              #   dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
              #     thesaurus_item_unit, colour = thesaurus_item_colour)
              
              thesaurus_selected_items <- module_elements_items %>% dplyr::filter(group_id == !!group_id) %>%
                dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                  thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
            }

            # Get plugin code

            ids <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, module_id)

            # Check if plugin has been deleted
            check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", ids$plugin_id)) %>% dplyr::pull(deleted)
            if (!check_deleted_plugin){

              code_server_card <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
                dplyr::pull(code) %>%
                stringr::str_replace_all("%module_id%", as.character(ids$module_id)) %>%
                stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
                stringr::str_replace_all("\r", "\n")

              # If it is an aggregated plugin, change %study_id% with current chosen study
              if (length(m$chosen_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$chosen_study))
            }
            else code_server_card <- ""

            # Create a session number, to inactivate older observers
            # Reset all older observers for this group_id
            
            session_code <- paste0("module_", r[[paste0(prefix, "_selected_module")]], "_group_", group_id)
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
            
            # Update items datatable for this widget
            
            
            # --- --- --- --- --- ---
            #### Delete a widget ----
            # --- --- --- --- --- ---

            observeEvent(input[[paste0(prefix, "_remove_module_element_", group_id)]], {
              r[[paste0(prefix, "_selected_module_element")]] <- group_id
              r[[module_element_delete_variable]] <- TRUE
            })
            
            # --- --- --- --- --- ---
            #### Widget settings ----
            # --- --- --- --- --- ---

            observeEvent(input[[paste0(prefix, "_settings_module_element_", group_id)]], {
              r[[paste0(prefix, "_settings_module_element_trigger")]] <- Sys.time()
              r[[paste0(prefix, "_settings_module_element")]] <- group_id
            })
            
            observeEvent(r[[paste0(prefix, "_settings_module_element_trigger")]], {
              sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
              shinyjs::show(paste0(prefix, "_module_element_settings"))
              
              module_element_infos <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(id == r[[paste0(prefix, "_settings_module_element")]])
              req(nrow(module_element_infos) > 0)
              
              # Update thesaurus toggle
              data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id) %>% as.character()
              thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) |
                  grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
              shiny.fluent::updateComboBox.shinyInput(session, "module_element_settings_thesaurus", 
                options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
              
              # Update name & plugin textfields
              
              module_element_plugin_infos <- r$plugins %>% dplyr::filter(id == module_element_infos$plugin_id)
              
              shiny.fluent::updateTextField.shinyInput(session = session, "module_element_settings_name", value = module_element_infos$name)
              shiny.fluent::updateTextField.shinyInput(session = session, "module_element_settings_plugin", value = module_element_plugin_infos$name)
            })
            
            observeEvent(input$module_element_settings_thesaurus, {
              r[[paste0(prefix, "_load_thesaurus_trigger")]] <- Sys.time()
              r[[paste0(prefix, "_load_thesaurus_type")]] <- "module_element_settings"
            })
            
            # Close button clicked
            observeEvent(input[[paste0(prefix, "_close_module_element_settings")]], {
              shinyjs::hide(paste0(prefix, "_module_element_settings"))
              sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
            })

          }
        })
      }
    })
    
    # --- --- --- --- --- --- -- -
    # Other server reactivity ----
    # --- --- --- --- --- --- -- -
    
      # --- --- --- --- --- --- --- --- --- -- -
      ## Sortable / change pivotitems order ----
      # --- --- --- --- --- --- --- --- --- -- -
      
      observeEvent(input$study_pivot_order, {
        
        new_pivot_order <- tibble::tibble(name = stringr::str_split(input$study_pivot_order, "\n") %>% unlist()) %>%
          dplyr::mutate(display_order = 1:dplyr::n())
        
        table <- paste0(prefix, "_modules")
        module_family_id <- r[[table]] %>% dplyr::filter(id == r[[paste0(prefix, '_selected_module')]]) %>% dplyr::pull(module_family_id)
        
        if (is.na(module_family_id)) all_modules <- r[[table]] %>% dplyr::filter(is.na(module_family_id))
        else all_modules <- r[[table]] %>% dplyr::filter(module_family_id == !!module_family_id)
        
        all_modules <- all_modules %>%
          dplyr::select(-display_order) %>%
          dplyr::inner_join(new_pivot_order, by = "name") %>%
          dplyr::relocate(display_order, .after = "parent_module_id")
        
        sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id IN ({all_modules %>% dplyr::pull(id)*})", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        DBI::dbAppendTable(r$db, table, all_modules)
        
        r[[table]] <- r[[table]] %>% 
          dplyr::anti_join(all_modules %>% dplyr::select(id), by = "id") %>%
          dplyr::bind_rows(all_modules) %>%
          dplyr::arrange(id)
        
        r[[paste0(prefix, "_load_display_modules")]] <- Sys.time()
      })
    
      # --- --- --- --- --- --- --- --- --- --- --- -
      ## Show / hide div when pivot item selected ----
      # --- --- --- --- --- --- --- --- --- --- --- -

      observeEvent(r[[paste0(prefix, "_selected_module")]], {
        
        req(!grepl("show_module", r[[paste0(prefix, "_selected_module")]]))

        # Hide all cards
        sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)

        # Hide Add module element card & Add module card
        sapply(c(paste0(prefix, "_add_module"), paste0(prefix, "_add_module_element"), paste0(prefix, "_module_element_settings")), shinyjs::hide)

        # Show toggles for this module
        shinyjs::show(paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_module")]]))

        # Add to the list of open cards and reset the list
        r[[paste0(prefix, "_opened_cards")]] <- paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_module")]])

        module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == r[[paste0(prefix, "_selected_module")]]) %>% 
          dplyr::rename(group_id = id)
        distinct_groups <- unique(module_elements$group_id)

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
      })
    
      # --- --- --- - -
      ## Add a tab ----
      # --- --- --- - -
  
      observeEvent(input$study_current_tab_trigger, {
        req(grepl("add_module", input$study_current_tab))
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::hide(paste0(prefix, "_add_module_element"))
        shinyjs::hide(paste0(prefix, "_module_element_settings"))
        shinyjs::show(paste0(prefix, "_add_module"))
      })

      # Close creation div
      observeEvent(input[[paste0(prefix, "_close_add_module")]], {

        # Show opened cards before opening Add module element div
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)

        # Hide Add module element div
        shinyjs::hide(paste0(prefix, "_add_module"))
      })

      # Add button clicked
      observeEvent(input$add_module_button, {

        study <- r$studies %>% dplyr::filter(id == m$chosen_study)
        module <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_module")]])

        new_data <- list()
        new_data$name <- coalesce2(type = "char", x = input$module_name)
        new_data$description <- ""

        # Required textfields
        required_textfields <- "name"

        # Fields requiring unique value
        req_unique_values <- "name"

        # Get module_family_id
        new_data$module_family <- study %>% dplyr::pull(paste0(prefix, "_module_family_id"))

        # If it is the first module to be created
        if (nrow(module) == 0){
          new_data$parent_module <- NA_integer_
          new_data$display_order <- 1
        }

        # If already existing modules
        if (nrow(module) > 0){

          # If module is at the same level of current module, get common parent_module_id
          # Calculate display order

          if (input$add_module_type == "same_level") new_data$parent_module <- module %>% dplyr::pull(parent_module_id)
          if (input$add_module_type == "level_under") new_data$parent_module <- module %>% dplyr::pull(id)

          # Calculate display order
          if (!is.na(new_data$parent_module)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_modules')`}
            WHERE module_family_id = {new_data$module_family} AND parent_module_id = {new_data$parent_module}", .con = r$db)
          if (is.na(new_data$parent_module)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_modules')`}
            WHERE module_family_id = {new_data$module_family} AND parent_module_id IS NULL", .con = r$db)

          new_data$display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() + 1

          # Can't add a module at the level under if there are modules elements attached to current module
          if (input$add_module_type == "level_under"){
            modules_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == r[[paste0(prefix, "_selected_module")]], !deleted) %>%
              dplyr::rename(group_id = id)
            if (nrow(modules_elements) > 0) show_message_bar_new(output = output, id = 3, message = "add_module_has_modules_elements", i18n = i18n)
            req(nrow(modules_elements) == 0)
          }
        }

        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "module_name", errorMessage = i18n$t("provide_valid_name"))
        req(!is.na(new_data$name))

        add_settings_new_data_new(session = session, output = output, r = r, m = m, i18n = i18n, id = id,
          data = new_data, table = paste0(prefix, "_modules"), required_textfields = required_textfields, req_unique_values = req_unique_values)

        # Reset fields

        shiny.fluent::updateTextField.shinyInput(session, "module_name", value = "")
        shiny.fluent::updateChoiceGroup.shinyInput(session, "add_module_type", value = "same_level")

        # Reload UI, with new module opened

        module_id <- get_last_row(r$db, paste0(prefix, "_modules"))
        r[[paste0(prefix, "_selected_module")]] <- module_id
        r[[paste0(prefix, "_load_display_modules")]] <- Sys.time()
        
        # Add Toggles div
        
        toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", module_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_module_element_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
              #paste0("module_id = ", module_id),
              div(style = "width:20px;")
            )
          )
        )
      
        insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", module_id))))
        output[[paste0(prefix, "_toggles_", module_id)]] <- renderUI(toggles_div)
        
        # If this is a sub-module, change toggles div of parent module also
        parent_module_id <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(id == module_id) %>% dplyr::pull(parent_module_id)
        if(!is.na(parent_module_id)){
          
          #removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", parent_module_id))))
          shinyjs::hide(paste0(prefix, "_toggles_", parent_module_id))
          
          parent_toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", parent_module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
                div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
              )
            )
          )
          
          insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = shinyjs::hidden(uiOutput(ns(paste0(prefix, "_toggles_", parent_module_id)))))
          output[[paste0(prefix, "_toggles_", parent_module_id)]] <- renderUI(parent_toggles_div)
        }
        
        # Add toggles div to vector of cards
        r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_toggles_", module_id))
        
        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(100, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()

        # Hide currently opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      })
      
      # --- --- --- --- --- --- --- --- --- --
      ## Show / hide widget creation card ----
      # --- --- --- --- --- --- --- --- --- --
      
      # Code to make Add module element button work
      observeEvent(input[[paste0(prefix, "_add_module_element_trigger")]], {
        # observeEvent(input[[paste0(prefix, "_add_module_element_", r[[paste0(prefix, "_selected_module")]])]], {
        
        # Hide opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        
        # Show Add module element div
        shinyjs::show(paste0(prefix, "_add_module_element"))
        
      })
      
      # --- --- --- --- --
      ## Delete a tab ----
      # --- --- --- --- --
      
      # Code to make Remove module button work
      # observeEvent(input[[paste0(prefix, "_remove_module_", r[[paste0(prefix, "_selected_module")]])]], r[[module_delete_variable]] <- TRUE)
      observeEvent(input[[paste0(prefix, "_remove_module_trigger")]], r[[module_delete_variable]] <- TRUE)
      
      module_delete_prefix <- paste0(prefix, "_module")
      module_dialog_title <- paste0(prefix, "_modules_delete")
      module_dialog_subtext <- paste0(prefix, "_modules_delete_subtext")
      module_react_variable <- "module_delete_confirm"
      module_table <- paste0(prefix, "_modules")
      module_id_var_sql <- "id"
      module_id_var_r <- paste0(prefix, "_selected_module")
      module_delete_message <- paste0(prefix, "_module_deleted")
      module_reload_variable <- paste0(prefix, "_load_ui")
      module_information_variable <- paste0(prefix, "_module_deleted")
      module_delete_variable <- paste0(module_delete_prefix, "_open_dialog")

      delete_element_new(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = module_delete_prefix, dialog_title = module_dialog_title, dialog_subtext = module_dialog_subtext,
        react_variable = module_react_variable, table = module_table, id_var_sql = module_id_var_sql, id_var_r = module_id_var_r,
        delete_message = module_delete_message, translation = TRUE, reload_variable = module_reload_variable,
        information_variable = module_information_variable)

      # When a module is deleted, change current module variable
      # Reload toggles if necessary
      # Delete sub-modules either

      observeEvent(r[[module_information_variable]], {

        table <- paste0(prefix, "_modules")
        deleted_module_id <- r[[paste0(prefix, "_module_deleted")]]
        sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_module_id}", .con = r$db)
        module_deleted <- DBI::dbGetQuery(r$db, sql)

        # If we are at level one, take first module of level one
        if (is.na(module_deleted$parent_module_id)){
          show_module_id <- r[[table]] %>%
            dplyr::filter(module_family_id == module_deleted$module_family_id & is.na(parent_module_id) & id != module_deleted$id) %>%
            dplyr::arrange(display_order) %>%
            dplyr::slice(1) %>%
            dplyr::pull(id)
        }

        # Else, take first module of the same level
        if (!is.na(module_deleted$parent_module_id)){
          show_module <- r[[table]] %>%
            dplyr::filter(parent_module_id == module_deleted$parent_module_id & id != module_deleted$id)

          # If not any module in this level, take lower level
          if (nrow(show_module) == 0) show_module <- r[[table]] %>%
              dplyr::filter(id == module_deleted$parent_module_id)

          show_module_id <- show_module %>%
            dplyr::arrange(display_order) %>%
            dplyr::slice(1) %>%
            dplyr::pull(id)
        }

        r[[paste0(prefix, "_selected_module")]] <- paste0("show_module_", show_module_id)
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        shinyjs::show(paste0(prefix, "_toggles_", show_module_id))

        # Reload UI menu
        r[[paste0(prefix, "_load_display_modules")]] <- Sys.time()
        
        # Remove toggles UI of this module
        # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", deleted_module_id))))
        
        # Check if parent module still have children and reload toggles div if not
        sql <- glue::glue_sql("SELECT parent_module_id FROM {`paste0(prefix, '_modules')`} WHERE id = {deleted_module_id}", .con = r$db)
        parent_module_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(parent_module_id)
        if (!is.na(parent_module_id)){
          
          has_children <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == !!parent_module_id) %>% nrow()
          if(has_children == 0){
            
            # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", parent_module_id))))
            shinyjs::hide(paste0(prefix, "_toggles_", parent_module_id))
            
            parent_toggles_div <- div(
              make_card("",
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", parent_module_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                    onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_module_element_trigger', Math.random())"))),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", parent_module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                    onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
                  #paste0("module_id = ", module_id),
                  div(style = "width:20px;")
                )
              )
            )
            
            insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", parent_module_id))))
            output[[paste0(prefix, "_toggles_", parent_module_id)]] <- renderUI(parent_toggles_div)
          }
        }
        
        # Delete children modules of deleted module
        # has_children <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == deleted_module_id) %>% nrow()
        # parent_module_id <- deleted_module_id
        
        
        
        # delete_children <- function(module_id){
        #   children_ids <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == !!module_id) %>% dplyr::pull(id)
        #   sql <- glue::glue_sql("UPDATE {`paste0(prefix, '_modules')`} SET deleted = TRUE WHERE id IN ({children_ids*})" , .con = r$db)
        #   DBI::dbSendStatement(r$db, sql) -> query
        #   DBI::dbClearResult(query)
        # }
        # 
        # 
        # while(has_children > 0){
        #   
        #   delete_children(module_id)
        #   children_ids <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == !!parent_module_id) %>% dplyr::pull(id)
        #   
        #   
        #   has_children <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == !!parent_module_id) %>% nrow()
        # }
        
        # update_r(r = r, table = module_table)
        
        # TO DO : loop to delete also sub-sub-modules...
      })

      # --- --- --- --- --
      ## Add a widget ----
      # --- --- --- --- --
      
      observeEvent(r$plugins, {

        module_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)

        plugins <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)

        options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
        shiny.fluent::updateComboBox.shinyInput(session, "plugin", options = options)
      })

      # Only for patient-lvl data

      if (prefix == "patient_lvl"){

        # --- --- --- --- --- --- -
        ## Thesaurus datatable ----
        # --- --- --- --- --- --- -

        # Load thesaurus attached to this datamart
        observeEvent(r$chosen_datamart, {

          req(!is.na(r$chosen_datamart))
          
          # Reset study menu
          shinyjs::show("initial_breadcrumb")
          shinyjs::show("choose_a_study_card")
          shinyjs::hide("study_menu")
          sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)

          data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id) %>% as.character()

          # Multiple cases
          # Only one ID, so it's the beginning and the end
          # Last ID, so it's the end
          # ID between begin and last, so separated by commas
          thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) |
            grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
          shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
        })
        
        # If r$studies changes, hide study_menu
        observeEvent(r$studies, {
          shinyjs::show("initial_breadcrumb")
          shinyjs::show("choose_a_study_card")
          shinyjs::hide("study_menu")
          sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
        })

        # Load thesaurus items
        
        observeEvent(input$thesaurus, {
          r[[paste0(prefix, "_load_thesaurus_trigger")]] <- Sys.time()
          r[[paste0(prefix, "_load_thesaurus_type")]] <- "module_element_creation"
        })
        
        observeEvent(r[[paste0(prefix, "_load_thesaurus_trigger")]], {

          if (r[[paste0(prefix, "_load_thesaurus_type")]] == "module_element_creation"){
            r_var <- "module_element_thesaurus_items"
            datatable <- "module_element_thesaurus_items"
            thesaurus_id <- input$thesaurus$key
          }
          if (r[[paste0(prefix, "_load_thesaurus_type")]] == "module_element_settings"){
            r_var <- "module_element_settings_thesaurus_items"
            datatable <- "module_element_settings_thesaurus_items"
            thesaurus_id <- input$module_element_settings_thesaurus$key
          }
          
          r[[r_var]] <- create_datatable_cache_new(output = output, r = r, i18n = i18n, module_id = id, thesaurus_id = input$thesaurus$key, category = "plus_module")

          colour_col <- create_datatable_cache_new(output = output, r = r, i18n = i18n, module_id = id, thesaurus_id = input$thesaurus$key, category = "colours_module")

          if (nrow(colour_col) > 0) r[[r_var]] <- r[[r_var]] %>%
            dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")

          count_items_rows <- tibble::tibble()
          count_patients_rows <- tibble::tibble()

          # Add count_items_rows in the cache & get it if already in the cache
          tryCatch(count_items_rows <- create_datatable_cache_new(output = output, r = r, i18n = i18n, thesaurus_id = thesaurus_id,
            datamart_id = r$chosen_datamart, category = "count_items_rows"),
              error = function(e) if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_load_datamart",
                error_name = paste0("modules - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n))

          # Add count_items_rows in the cache & get it if already in the cache
          tryCatch(count_patients_rows <- create_datatable_cache_new(output = output, r = r, i18n = i18n, thesaurus_id = thesaurus_id,
            datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
              error = function(e) if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_load_datamart",
                error_name = paste0("modules - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n))

          if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar_new(output, 1, "fail_load_datamart", "severeWarning", i18n = i18n, ns = ns)
          req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)

          # Transform count_rows cols to integer, to be sortable
          r[[r_var]] <- r[[r_var]] %>%
            dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
            dplyr::left_join(count_items_rows, by = "item_id") %>%
            dplyr::left_join(count_patients_rows, by = "item_id") %>%
            dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
            dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")

          # Filter on count_items_rows > 0
          r[[r_var]] <- r[[r_var]] %>% dplyr::filter(count_items_rows > 0)

          r[[paste0(r_var, "_temp")]] <- r[[r_var]] %>%
            dplyr::mutate(modified = FALSE) %>%
            dplyr::mutate_at("item_id", as.character)

          editable_cols <- c("display_name", "unit")
          searchable_cols <- c("item_id", "name", "display_name", "unit")
          factorize_cols <- c("unit")
          column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")
          sortable_cols <- c("id", "item_id", "name", "display_name", "count_patients_rows", "count_items_rows")
          centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
          col_names <- get_col_names_new(table_name = "modules_thesaurus_items_with_counts", i18n = i18n)
          hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")

          # Render datatable
          render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$module_element_thesaurus_items_temp,
            output_name = datatable, col_names =  col_names,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
            searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)

          # Create a proxy for datatatable
          r[[paste0(r_var, "_proxy")]] <- DT::dataTableProxy(datatable, deferUntilFlush = FALSE)

        })

        # Reload datatable

        observeEvent(r$module_element_thesaurus_items_temp, {

          # Reload data of datatable
          DT::replaceData(r$module_element_thesaurus_items_proxy, r$module_element_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
        })

        # Updates in datatable

        observeEvent(input$module_element_thesaurus_items_cell_edit, {

          edit_info <- input$module_element_thesaurus_items_cell_edit

          r$module_element_thesaurus_items_temp <- DT::editData(r$module_element_thesaurus_items_temp, edit_info, rownames = FALSE)
          r$module_element_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
        })

        # --- --- --- --- --- -
        ## Thesaurus items ----
        # --- --- --- --- --- -

        # When add button is clicked
        observeEvent(input$item_selected, {

          # Initiate r variable if doesn't exist
          if (length(r$module_element_thesaurus_selected_items) == 0){
            r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
              thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
              thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
          }

          # Get ID of chosen thesaurus item
          link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))

          # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown

          # value <- integer(1)
          # if (nrow(r$module_element_thesaurus_selected_items) > 0) value <- r$module_element_thesaurus_selected_items %>%
          #   dplyr::filter(thesaurus_id == input$thesaurus$key) %>% dplyr::pull(id)

          # if (link_id %not_in% value){

          # Get thesaurus name
          thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus$key) %>% dplyr::pull(name)

          # Get item informations from datatable / r$modules_thesaurus_items
          # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
          item <- r$module_element_thesaurus_items_temp %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", name))

          # display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)

          # Get mapped items
          thesaurus_mapped_items <- tibble::tibble()
          if (length(input$thesaurus_mapping) > 0){
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
              "WHERE m.thesaurus_id_1 = {as.integer(input$thesaurus$key)} AND m.item_id_1 = {as.integer(item$item_id)} AND m.relation_id IN ({input$thesaurus_mapping*}) ",
              "AND m.category = 'user_added_mapping' AND m.deleted IS FALSE ",
              "UNION ",
              "SELECT m.thesaurus_id_1 AS thesaurus_id, m.item_id_1 AS thesaurus_item_id, e.evaluation_id, ",
              "i.id, i.name AS thesaurus_item_name, i.display_name AS thesaurus_item_display_name, i.unit AS thesaurus_item_unit, ",
              "u.name AS user_thesaurus_item_name, u.display_name AS user_thesaurus_item_display_name, u.unit AS user_thesaurus_item_unit ",
              "FROM thesaurus_items_mapping m ",
              "INNER JOIN thesaurus_items_mapping_evals e ON m.id = e.mapping_id AND e.deleted IS FALSE ",
              "INNER JOIN thesaurus_items i ON m.thesaurus_id_1 = i.thesaurus_id AND m.item_id_1 = i.item_id AND i.deleted IS FALSE ",
              "LEFT JOIN thesaurus_items_users u ON m.thesaurus_id_1 = u.thesaurus_id AND m.item_id_1 = u.item_id AND u.deleted IS FALSE ",
              "WHERE m.thesaurus_id_2 = {as.integer(input$thesaurus$key)} AND m.item_id_2 = {as.integer(item$item_id)} AND m.relation_id IN ({input$thesaurus_mapping*}) ",
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
                thesaurus_item_unit, thesaurus_item_colour = as.character(input[[paste0("colour_", link_id)]]), 
                input_text = paste0(thesaurus_name, " - ", thesaurus_item_display_name, " (", tolower(i18n$t("mapped_item")), ")"),
                mapped_to_item_id = link_id, merge_items = input[[paste0(prefix, "_merge_mapped_items")]]
              ) %>%
              dplyr::anti_join(r$module_element_thesaurus_selected_items %>% dplyr::select(id), by = "id")
            # }
          }
          
          # Add item to selected items
          add_thesaurus_items <-
            # r$module_element_thesaurus_selected_items %>%
            # dplyr::bind_rows(
              tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name,
                ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text, ~mapped_to_item_id, ~merge_items,
                as.integer(link_id), as.integer(input$thesaurus$key), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
                as.character(item$unit), as.character(input[[paste0("colour_", link_id)]]), as.character(item$input_text),
                NA_integer_, FALSE)
            # )
          if (nrow(thesaurus_mapped_items) > 0) add_thesaurus_items <-
            add_thesaurus_items %>% dplyr::bind_rows(thesaurus_mapped_items)
          
          r$module_element_thesaurus_selected_items <-
          r$module_element_thesaurus_selected_items %>%
            dplyr::anti_join(add_thesaurus_items %>% dplyr::select(id), by = "id") %>%
            dplyr::bind_rows(add_thesaurus_items)

          # Update dropdown of selected items
          options <- convert_tibble_to_list(r$module_element_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
          value <- r$module_element_thesaurus_selected_items %>% dplyr::pull(id)
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
          # }

        })

        # When reset button is clicked
        observeEvent(input$reset_thesaurus_items, {
          # Reset r$modules_thesaurus_selected_items
          r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
            thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
            thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())

          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
        })

        # When dropdown is modified
        observeEvent(input$thesaurus_selected_items_trigger, {

          if (length(input$thesaurus_selected_items) == 0) r$module_element_thesaurus_selected_items <- r$module_element_thesaurus_selected_items %>% dplyr::slice(0)
          if (length(input$thesaurus_selected_items) > 0) {
            r$module_element_thesaurus_selected_items <- r$module_element_thesaurus_selected_items %>%
              dplyr::filter(id %in% input$thesaurus_selected_items)
            # Delete also mapped items
            r$module_element_thesaurus_selected_items <- r$module_element_thesaurus_selected_items %>%
              dplyr::filter(is.na(mapped_to_item_id) | mapped_to_item_id %in% r$module_element_thesaurus_selected_items$id)
          }
          
          options <- convert_tibble_to_list(r$module_element_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
          value <- r$module_element_thesaurus_selected_items %>% dplyr::pull(id)
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        })

      }
      # End of if(prefix == "patient_lvl")
      
      # --- --- --- --- --- --- --- ---
      ## Widget add button clicked ----
      # --- --- --- --- --- --- --- ---
      
      observeEvent(input$add_module_element_button, {

        new_data <- list()

        new_data$name <- coalesce2(type = "char", x = input$module_element_name)
        new_data$module_family <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_module")]]) %>% dplyr::pull(module_family_id)
        new_data$module_new_element <- r[[paste0(prefix, "_selected_module")]]
        new_data$plugin <- input$plugin$key

        # Check if name is not empty
        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "module_element_name", errorMessage = i18n$t("provide_valid_name"))
        else shiny.fluent::updateTextField.shinyInput(session, "module_element_name", errorMessage = NULL)
        req(!is.na(new_data$name))

        # Check if values required to be unique are unique

        table <- paste0(prefix, "_modules_elements")

        sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND module_id = {new_data$module_new_element}", .con = r$db)
        distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        if (new_data$name %in% distinct_values) show_message_bar_new(output, 2, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
        req(new_data$name %not_in% distinct_values)

        # Check if dropdowns are not empty (if all are required)
        dropdowns_check <- TRUE

        required_dropdowns <- c("plugin")

        for (dropdown in required_dropdowns){
          if (is.null(new_data[[dropdown]])) dropdowns_check <- FALSE
          else if (is.na(new_data[[dropdown]])) dropdowns_check <- FALSE
        }
        # sapply(required_dropdowns, function(dropdown){
        #   if (is.null(new_data[[dropdown]])) dropdowns_check <<- FALSE
        #   else if (is.na(new_data[[dropdown]])) dropdowns_check <<- FALSE
        # })

        if (!dropdowns_check) show_message_bar_new(output, 2, "dropdown_empty", "severeWarning", i18n = i18n, ns = ns)
        req(dropdowns_check)

        # Get last_row nb
        # last_row_modules_elements <- get_last_row(r$db, paste0(prefix, "_modules_elements"))
        group_id <- get_last_row(r$db, paste0(prefix, "_modules_elements")) + 1
        last_row_modules_elements_items <- get_last_row(r$db, paste0(prefix, "_modules_elements_items"))
        # group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table)) %>% dplyr::pull() %>% as.integer() + 1
        last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", paste0(prefix, "_modules_elements"), " WHERE module_id = ", new_data$module_new_element)) %>% dplyr::pull() %>% as.integer()

        new_data <- tibble::tribble(~id, ~name, ~module_id, ~plugin_id, ~display_order, ~creator_id, ~datetime, ~deleted,
          group_id, as.character(new_data$name), as.integer(new_data$module_new_element),
          as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
        
        DBI::dbAppendTable(r$db, paste0(prefix, "_modules_elements"), new_data)
        add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r[[paste0(prefix, "_modules_elements")]] <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::bind_rows(new_data)
        
        has_thesaurus_items <- TRUE

        if (prefix == "patient_lvl"){

          if (length(r$module_element_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          if (length(r$module_element_thesaurus_selected_items) > 0) if (nrow(r$module_element_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE

          if (has_thesaurus_items){
            
            new_data <-
              r$module_element_thesaurus_selected_items %>%
              dplyr::transmute(
                item_id = id,
                id = 1:dplyr::n() + last_row_modules_elements_items + 1,
                group_id = !!group_id,
            #     name = as.character(new_data$name),
            #     group_id = !!group_id,
            #     module_id = as.integer(new_data$module_new_element),
            #     plugin_id = as.integer(new_data$plugin),
                thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, thesaurus_item_colour,
                mapped_to_item_id, merge_items,
            #     display_order = last_display_order + 1,
                creator_id = r$user_id,
                datetime = as.character(Sys.time()),
                deleted = FALSE
              )
            
            new_data <- new_data %>%
              dplyr::left_join(
                new_data %>% dplyr::select(mapped_to_item_id = item_id, new_mapped_to_item_id = id),
                by = "mapped_to_item_id"
              ) %>%
              dplyr::mutate(mapped_to_item_id = new_mapped_to_item_id) %>%
              dplyr::select(-item_id, -new_mapped_to_item_id)
            
            DBI::dbAppendTable(r$db, paste0(prefix, "_modules_elements_items"), new_data)
            add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
            r[[paste0(prefix, "_modules_elements_items")]] <- r[[paste0(prefix, "_modules_elements_items")]] %>% dplyr::bind_rows(new_data)
            
            # Save thesaurus items for server code
            thesaurus_selected_items <- new_data %>%
              dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
            
            # Reset r$module_element_thesaurus_selected_items & dropdown
            r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
              thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
              thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
            shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
          }
          # if (!has_thesaurus_items){
          #   new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_name, ~thesaurus_item_id,
          #     ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~display_order, ~creator_id, ~datetime, ~deleted,
          #     last_row + 1, as.character(new_data$name), group_id, as.integer(new_data$module_new_element), as.integer(new_data$plugin),
          #     "None", 0L, "None", "", "", last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
          # }
        }

        # if (prefix == "aggregated") new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id,
        #   ~display_order, ~creator_id, ~datetime, ~deleted,
        #   last_row + 1, as.character(new_data$name), group_id, as.integer(new_data$module_new_element),
        #   as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
        
        show_message_bar_new(output = output, id = 3, message = paste0(get_singular(paste0(prefix, "_modules_elements")), "_added"), type = "success", i18n = i18n, ns = ns)

        # update_r_new(r = r, m = m, table = table, i18n = i18n)

        # Reset name textfield & dropdowns
        shiny.fluent::updateTextField.shinyInput(session, "module_element_name", value = "")
        # if (prefix == "patient_lvl" & has_thesaurus_items){
        # 
        #   # Save thesaurus items for server code first
        #   thesaurus_selected_items <-
        #     r$module_element_thesaurus_selected_items %>%
        #     dplyr::select(group_id, thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
        #       thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
        # 
        #   r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
        #     thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
        #     thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = lo)
        #   shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
        # }

        # Run server code

        trace_code <- paste0(prefix, "_", group_id, "_", m$chosen_study)
        # if (trace_code %in% r$server_modules_groups_loaded) print(trace_code)
        if (trace_code %not_in% r$server_modules_groups_loaded){
  
          # Add the trace_code to loaded plugins list
          r$server_modules_groups_loaded <- c(r$server_modules_groups_loaded, trace_code)
  
          # Get plugin code
  
          # Check if plugin has been deleted
          check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", input$plugin$key)) %>% dplyr::pull(deleted)
          if (!check_deleted_plugin){
  
            code_server_card <- r$code %>%
              dplyr::filter(link_id == input$plugin$key, category == "plugin_server") %>%
              dplyr::pull(code) %>%
              stringr::str_replace_all("%module_id%", as.character(r[[paste0(prefix, "_selected_module")]])) %>%
              stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
              stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
              stringr::str_replace_all("\r", "\n")
  
            # If it is an aggregated plugin, change %study_id% with current chosen study
            if (length(m$chosen_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$chosen_study))
          }
          else code_server_card <- ""
          
          # Create a session number, to inactivate older observers
          # Reset all older observers for this group_id
          
          session_code <- paste0("module_", r[[paste0(prefix, "_selected_module")]], "_group_", group_id)
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
  
          # Code for removing module element
  
          observeEvent(input[[paste0(prefix, "_remove_module_element_", group_id)]], {
            r[[paste0(prefix, "_selected_module_element")]] <- group_id
            r[[module_element_delete_variable]] <- TRUE
          })

        }

        # Prepare module element UI code
        
        module_id <- r[[paste0(prefix, "_selected_module")]]
        
        code_ui_card <- isolate(r$code) %>% dplyr::filter(link_id == input$plugin$key, category == "plugin_ui") %>% dplyr::pull(code)
        element_code <- div()
        module_element_name <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        tryCatch({
          code_ui_card <- code_ui_card %>%
            stringr::str_replace_all("%module_id%", as.character(module_id)) %>%
            stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
            stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
            stringr::str_replace_all("\r", "\n") %>%
            stringr::str_replace_all("%study_id%", as.character(isolate(m$chosen_study)))
          
          element_code <- div(
            make_card("",
              div(
                div(id = ns(paste0(prefix, "_module_element_plugin_ui_", group_id)), eval(parse(text = code_ui_card))),
                # shinyjs::hidden(
                #   div(id = ns(paste0(prefix, "_module_element_settings_", group_id)),
                #     p(span(i18n$t("widget_name"), style = "font-weight:bold;"), " : ", module_element_name),
                #     
                #   )
                # ),
                div(
                  id = ns(paste0(prefix, "_module_element_settings_remove_buttons_", group_id)),
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                    actionButton(ns(paste0(prefix, "_settings_module_element_", group_id)), "", icon = icon("cog")),
                    actionButton(ns(paste0(prefix, "_remove_module_element_", group_id)), "", icon = icon("trash-alt"))
                  ),
                  style = "position:absolute; top:8px; right: 10px;"
                )#,
                # shinyjs::hidden(
                #   div(
                #     id = ns(paste0(prefix, "_module_element_quit_button_", group_id)),
                #     actionButton(ns(paste0(prefix, "_quit_module_element_settings_", group_id)), "", icon = icon("times")),
                #     style = "position:absolute; top:8px; right: 10px;"
                #   )
                # )
              ),
              style = "position:relative;"
            )
          )
        },
        error = function(e){
          report_bug_new(r = r, output = output, error_message = i18n$t("error_run_plugin_ui_code"),
            error_name = paste0(id, " - run ui code - ", group_id), category = "Error", error_report = e, i18n = i18n)}
        )
        
        # Remove toggles UI for this module
        # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", module_id))))
        shinyjs::hide(paste0(prefix, "_toggles_", module_id))
        
        # Add the new toggles UI for this module
        
        toggles <- tagList()
        # update_r(r = r, table = paste0(prefix, "_modules_elements"))
        module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == !!module_id) %>%
          dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)
     
        # Get module element group_id
        distinct_groups <- unique(module_elements$group_id)
        
        # Reset opened cards
        r[[paste0(prefix, "_opened_cards")]] <- ""

        # Loop over distinct cards (modules elements), for this module
        
        sapply(distinct_groups, function(group_id){
          
          # Get name of module element
          module_element_name <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
          
          toggles <<- tagList(toggles,
            shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
            div(class = "toggle_title", module_element_name, style = "padding-top:10px;"))
          
          # Add to the list of opened cards
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
        })
        
        toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", module_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_module_element_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
              #paste0("module_id = ", module_id),
              div(style = "width:20px;"),
              toggles
            )
          )
        )
        
        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", module_id))
        
        # Show opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        # Add module toggles UI
        # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(prefix, "_toggles_", module_id))))
        output[[paste0(prefix, "_toggles_", module_id)]] <- renderUI(toggles_div)

        # Add module element UI
        insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_group_", group_id))))
        output[[paste0(prefix, "_group_", group_id)]] <- renderUI(element_code)
        
        # Hide Add module element div
        shinyjs::hide(paste0(prefix, "_add_module_element"))
        
        # Add this div to vector of cards
        r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_group_", group_id))
        
        # Reload UI menu
        r[[paste0(prefix, "_load_display_modules")]] <- Sys.time()
        
        # Reload UI menu (problem for displaying cards : blanks if we do not do that)
        # shinyjs::delay(300, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
        r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      })
      
      # --- --- --- --- --- -
      ## Delete a widget ----
      # --- --- --- --- --- -
      
      module_element_delete_prefix <- paste0(prefix, "_module_element")
      module_element_dialog_title <- paste0(prefix, "_modules_elements_delete")
      module_element_dialog_subtext <- paste0(prefix, "_modules_elements_delete_subtext")
      module_element_react_variable <- "module_element_delete_confirm"
      module_element_table <- paste0(prefix, "_modules_elements")
      module_element_id_var_sql <- "id"
      module_element_id_var_r <- paste0(prefix, "_selected_module_element")
      module_element_delete_message <- paste0(prefix, "_module_element_deleted")
      module_element_reload_variable <- paste0(prefix, "_load_ui")
      module_element_delete_variable <- paste0(module_element_delete_prefix, "_open_dialog")
      module_element_information_variable <- paste0(prefix, "_module_element_deleted")

      delete_element_new(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = module_element_delete_prefix, dialog_title = module_element_dialog_title, dialog_subtext = module_element_dialog_subtext,
        react_variable = module_element_react_variable, table = module_element_table, id_var_sql = module_element_id_var_sql, id_var_r = module_element_id_var_r,
        delete_message = module_element_delete_message, translation = TRUE, reload_variable = module_element_reload_variable,
        information_variable = module_element_information_variable)
      
      # When a module is element deleted, remove UI and reload toggles UI
      observeEvent(r[[module_element_information_variable]], {
        
        # table <- paste0(prefix, "_modules")
        # deleted_module_id <- r[[paste0(prefix, "_module_element_group_deleted")]]
        # sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_module_id}", .con = r$db)
        # module_deleted <- DBI::dbGetQuery(r$db, sql)
        
        group_id <- r[[paste0(prefix, "_module_element_deleted")]]
        
        # Remove UI card
        removeUI(selector = paste0("#", ns(paste0(prefix, "_group_", r[[paste0(prefix, "_module_element_deleted")]]))))
        
        sql <- glue::glue_sql("SELECT DISTINCT(module_id) FROM {`paste0(prefix, '_modules_elements')`} WHERE id = {group_id}", .con = r$db)
        module_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

        # Remove toggles UI for this module
        # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", module_id))))
        shinyjs::hide(paste0(prefix, "_toggles_", module_id))

        # Add the new toggles UI for this module

        toggles <- tagList()
        r[[paste0(prefix, "_modules_elements")]] <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(group_id != !!group_id)
        # update_r(r = r, table = paste0(prefix, "_modules_elements"))
        module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == !!module_id) %>% 
          dplyr::rename(group_id = id) %>% dplyr::arrange(display_order)

        # Get module element group_id
        distinct_groups <- unique(module_elements$group_id)

        # Reset opened cards
        r[[paste0(prefix, "_opened_cards")]] <- ""

        # Loop over distinct cards (modules elements), for this module
        sapply(distinct_groups, function(group_id){

          # Get name of module element
          module_element_name <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)

          toggles <<- tagList(toggles,
            shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
            div(class = "toggle_title", module_element_name, style = "padding-top:10px;"))

          # Add to the list of opened cards
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
        })
        # Does this module have sub-modules ?
        if (r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == module_id) %>% nrow() > 0) toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
              div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
            )
          )
        )
        
        else toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", module_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_module_element_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), i18n$t("remove_tab"), iconProps = list(iconName = "Delete"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_remove_module_trigger', Math.random())"))),
              #paste0("module_id = ", module_id),
              div(style = "width:20px;"),
              toggles
            )
          )
        )

        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", module_id))

        # Show opened cards
        
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        # Add module toggles UI
        # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(prefix, "_toggles_", module_id))))
        output[[paste0(prefix, "_toggles_", module_id)]] <- renderUI(toggles_div)
      })
      
      # --- --- --- --- --- --- --
      ## Debug - Execute code ----
      # --- --- --- --- --- --- --
      
      observeEvent(input$execute_code, {
        
        # print("cards = ")
        # print(r[[paste0(prefix, "_cards")]])
        # print(paste0("opened cards = ", r[[paste0(prefix, "_opened_cards")]]))
        # print(paste0("selected module = ", r[[paste0(prefix, "_selected_module")]]))
        # print(paste0("load_display_modules = ", r[[paste0(prefix, "_load_display_modules")]]))
        # print(paste0("first module shown = ", r[[paste0(prefix, "_first_module_shown")]] %>% dplyr::pull(id)))
        # print("display modules")
        # print("")
        # print(r[[paste0(prefix, "_display_modules")]])
        # print(paste0("load ui cards = ", r[[paste0(prefix, "_load_ui_cards")]]))
        # print(paste0("stage = ", r[[paste0(prefix, "_load_ui_stage")]]))
        # print("loaded studies")
        # print(r[[paste0(prefix, "_loaded_studies")]])
        
        code <- input$ace_edit_code %>% stringr::str_replace_all("\r", "\n")
        
        output$code_result <- renderText({
          
          options('cli.num_colors' = 1)
          
          # Capture console output of our code
          captured_output <- capture.output(
            tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
          
          # Restore normal value
          options('cli.num_colors' = NULL)
          
          # Display result
          paste(strwrap(captured_output), collapse = "\n")
        })
      })
  })
}
