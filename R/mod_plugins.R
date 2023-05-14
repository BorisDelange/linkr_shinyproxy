#' plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plugins_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card", "plugins_options_card", "import_plugin_card", "export_plugin_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  thesaurus_items_div <- ""
  if (id == "plugins_patient_lvl"){
    thesaurus_items_div <- div(
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
        make_combobox(i18n = i18n, ns = ns, label = "thesaurus", id = "thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
        make_dropdown(i18n = i18n, ns = ns, label = "items_mapping", id = "thesaurus_mapping", multiSelect = TRUE, width = "300px",
          options = list(
            list(key = 1, text = i18n$t("equivalent_to")),
            list(key = 2, text = i18n$t("included_in")),
            list(key = 3, text = i18n$t("include"))
          )
        ),
        conditionalPanel(condition = "input.thesaurus_mapping != null & input.thesaurus_mapping != ''", ns = ns, 
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(shiny.fluent::Toggle.shinyInput(ns("merge_mapped_items"), value = TRUE), style = "margin-top:45px;"),
              div(i18n$t("merge_mapped_items"), style = "font-weight:bold; margin-top:45px;")
            ),
            style = "margin-left:-28px;"
          )
        )
      ),
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 20),
        div(
          div(id = ns("thesaurus_selected_items_title"), class = "input_title", i18n$t("thesaurus_selected_items")),
          div(shiny.fluent::Dropdown.shinyInput(ns("thesaurus_selected_items"), value = NULL, options = list(), multiSelect = TRUE,
            onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-thesaurus_selected_items_trigger', Math.random())"))), style = "width:650px;")
        ),
        div(shiny.fluent::DefaultButton.shinyInput(ns("reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
      ),
      div(DT::DTOutput(ns("plugin_thesaurus_items")), class = "thesaurus_table"), 
      div(id = ns("blank_space"), br()),
      style = "position:relative; z-index:1; margin-bottom:-30px;"
    )
  }

  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
    shiny.fluent::reactOutput(ns("plugin_image_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t(id))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shiny.fluent::Pivot(
      id = ns("plugins_pivot"),
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "all_plugins_card", itemKey = "all_plugins_card", headerText = i18n$t("all_plugins")),
      shiny.fluent::PivotItem(id = "plugins_datatable_card", itemKey = "plugins_datatable_card", headerText = i18n$t("plugins_management")),
      shiny.fluent::PivotItem(id = "plugins_edit_code_card", itemKey = "plugins_edit_code_card", headerText = i18n$t("edit_plugin_code")),
      shiny.fluent::PivotItem(id = "plugins_options_card", itemKey = "plugins_options_card", headerText = i18n$t("plugin_options")),
      shiny.fluent::PivotItem(id = "import_plugin_card", itemKey = "import_plugin_card", headerText = i18n$t("import_plugin")),
      shiny.fluent::PivotItem(id = "export_plugin_card", itemKey = "export_plugin_card", headerText = i18n$t("export_plugin"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- -
    # Plugins catalog card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("all_plugins_card"),
        div(id = ns("all_plugins_document_cards"),
          make_card(i18n$t("all_plugins"),
            div(
              div(
                shiny.fluent::ChoiceGroup.shinyInput(ns("all_plugins_source"), value = "local", options = list(
                  list(key = "local", text = i18n$t("local_plural")),
                  list(key = "remote_git", text = i18n$t("git_remote_plugins"))
                ), className = "inline_choicegroup"),
                style = "width:320px;"
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
                conditionalPanel(condition = "input.all_plugins_source == 'remote_git'", ns = ns,
                  make_dropdown(i18n = i18n, ns = ns, label = "remote_git_repo", id = "remote_git_repo", width = "300px")),
                conditionalPanel(condition = "input.all_plugins_source == 'remote_git'", ns = ns, div(style = "width:20px;")),
                make_dropdown(i18n = i18n, ns = ns, label = "category", id = "plugins_category", width = "300px"),
              ),
              conditionalPanel(condition = "input.all_plugins_source == 'remote_git'", ns = ns,
                uiOutput(ns("all_plugins_remote_git"))),
              conditionalPanel(condition = "input.all_plugins_source == 'local'", ns = ns,
                uiOutput(ns("all_plugins_local"))), br(),
              div(shiny.fluent::DefaultButton.shinyInput(ns("reload_plugins_document_cards"), i18n$t("refresh")), style = "margin-top:2px; width:320px;")
            )
          )
        ),
        shinyjs::hidden(
          div(id = ns("all_plugins_plugin_details"),
            make_card(
              uiOutput(ns("all_plugins_plugin_details_title")),
              uiOutput(ns("all_plugins_plugin_details_content"))
            )
          )
        ), br(),
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Plugins management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_datatable_card"),
        make_card(i18n$t("plugins_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "plugin_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_plugin"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("plugins_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection")),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Edit plugin code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_edit_code_card"),
        make_card(i18n$t("edit_plugin_code"),
          div(
            make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "code_selected_plugin",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
            
            thesaurus_items_div, br(),
            
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
                  list(key = "ui", text = i18n$t("ui")),
                  list(key = "server", text = i18n$t("server")),
                  list(key = "translations", text = i18n$t("translations"))
                ), className = "inline_choicegroup"),
                div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
                div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
              ),
              style = "z-index:2"
            ),
            conditionalPanel(condition = "input.hide_editor == true", ns = ns, br()),
            
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              
              conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_ui"), "", mode = "r", 
                  code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                  ),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;")),
              conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_server"), "", mode = "r", 
                  code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                  ),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;")),
              conditionalPanel(condition = "input.edit_code_ui_server == 'translations'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_translations"), "", mode = "text",
                  code_hotkeys = list("r", list(save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"))),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;"))
            ),
            
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("save")), " ",
              shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code"))
            ), br(),
            div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"),
            shiny::uiOutput(ns("code_result_ui")), br(),
            div(verbatimTextOutput(ns("code_result_server")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    
    # --- --- --- --- -- -- --
    # Plugin options card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_options_card"),
        make_card(i18n$t("plugin_options"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "options_selected_plugin",
                width = "320px", allowFreeform = FALSE, multiSelect = FALSE),
              make_textfield(i18n = i18n, ns = ns, label = "author", id = "plugin_author", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "version", id = "plugin_version", width = "60px")
            ),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name_fr", id = "plugin_name_fr", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "name_en", id = "plugin_name_en", width = "320px")
            ),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "category_fr", id = "plugin_category_fr", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "category_en", id = "plugin_category_en", width = "320px")
            ),
            br(),
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
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_dropdown(i18n = i18n, ns = ns, label = "image_url", id = "plugin_image", width = "320px"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("delete_image"), i18n$t("delete_this_image")), style = "margin-top:39px;"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("import_image"), i18n$t("import_image")), style = "margin-top:39px;")
              )
            ), 
            br(),
            div(
              imageOutput(ns("render_image")), style = "border:solid #ECEBE9 1px; width:318px; height:200px;"),
            br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(paste0(i18n$t("description"), " :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("plugin_description_language"), value = "fr", options = list(
                list(key = "fr", text = i18n$t("description_french")),
                list(key = "en", text = i18n$t("description_english"))
              ), className = "inline_choicegroup")
            ),
            conditionalPanel(condition = "input.plugin_description_language == 'fr'", ns = ns,
              div(shinyAce::aceEditor(ns("plugin_description_fr"), "", mode = "markdown", 
                code_hotkeys = list("markdown", list(save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"))),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.plugin_description_language == 'en'", ns = ns,
              div(shinyAce::aceEditor(ns("plugin_description_en"), "", mode = "markdown", 
                code_hotkeys = list("markdown", list(save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"))),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_plugin_options"), i18n$t("save")),
            div(style = "display:none;", fileInput(ns("import_image_file"), label = "", multiple = FALSE, accept = c(".jpg", ".jpeg", ".png")))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- -
    # Import a plugin card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("import_plugin_card"),
        make_card(i18n$t("import_plugin"),
          div(br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
              make_toggle(i18n = i18n, ns = ns, label = "replace_already_existing_plugins", inline = TRUE)), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("import_plugins_browse"), i18n$t("choose_zip_file")),
              uiOutput(ns("import_plugins_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_plugins_button"), i18n$t("import_plugin"), iconProps = list(iconName = "Download")), br(),
            shinyjs::hidden(
                div(
                id = ns("imported_plugins_div"), br(),
                strong(i18n$t("imported_plugins")),
                div(DT::DTOutput(ns("imported_plugins")))
              )
            ),
            div(style = "display:none;", fileInput(ns("import_plugins_upload"), label = "", multiple = FALSE, accept = ".zip"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- -
    # Export a plugin card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("export_plugin_card"),
        make_card(i18n$t("export_plugin"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              make_dropdown(i18n = i18n, ns = ns, label = "plugins_to_export",
                multiSelect = TRUE, width = "400px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("export_plugins"), 
                i18n$t("export_plugins"), iconProps = list(iconName = "Upload")), style = "margin-top:38px;"),
              div(style = "visibility:hidden;", downloadButton(ns("export_plugins_download"), label = "")),
              style = "position:relative; z-index:1; width:700px;"
            ),
            div(DT::DTOutput(ns("plugins_to_export_datatable")), style = "margin-top:-30px; z-index:2")
          )
        ), br()
      )
    )
  ) -> result
  
  result
}
    
#' plugins Server Functions
#'
#' @noRd 
mod_plugins_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), o = shiny::reactiveValues(),
  language = "en", i18n = character(), app_folder = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) print(paste0(Sys.time(), " - mod_plugins - start"))
    
    col_types <- tibble::tribble(
      ~table, ~col_types,
      "plugins", "iccicl",
      "code", "icicicl"
    )
    
    # Prefix depending on page id
    if (id == "plugins_patient_lvl"){
      prefix <- "patient_lvl"
      tab_type_id <- 1 
    }
    if (id == "plugins_aggregated"){
      prefix <- "aggregated"
      tab_type_id <- 2
    }
    
    # r[[paste0(prefix, "_plugins_datatable_loaded")]] <- FALSE
 
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card",
      "plugins_options_card", "import_plugin_card", "export_plugin_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("all_plugins_card" %in% r$user_accesses) shinyjs::show("all_plugins_card")
    else shinyjs::show("all_plugins_card_forbidden")
    
    # --- --- --- --- --- --- --
    # Load page from header ----
    # --- --- --- --- --- --- --
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - ", id, " - observer shiny_router::change_page"))
      
      if (prefix == "aggregated" & shiny.router::get_page() == "plugins" & r$plugins_page == "plugins_patient_lvl") shiny.router::change_page("plugins_patient_lvl")
      else if (prefix == "patient_lvl" & shiny.router::get_page() == "plugins" & r$plugins_page == "plugins_aggregated") shiny.router::change_page("plugins_aggregated")
      
      # Close help pages when page changes
      r[[paste0("help_plugins_", prefix, "_open_panel")]] <- FALSE
      r[[paste0("help_plugins_", prefix, "_open_modal")]] <- FALSE
    })
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$plugins, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$plugins"))
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options)
    })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_plugins_", prefix, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_plugins_", prefix, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_plugins_", prefix, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_plugins_", prefix, "_open_modal")]] <- FALSE
      r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_plugins_", prefix, "_page_", i)]] <- Sys.time())
    })
    
    help_plugins(output = output, r = r, id = id, prefix = prefix, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- -- -
    # Plugins catalog ----
    # --- --- --- --- -- -
    
    # Update dropdown of remote git repos
    
    observeEvent(r$git_repos, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$git_repos"))
      
      shiny.fluent::updateDropdown.shinyInput(session, "remote_git_repo", 
        options = convert_tibble_to_list(r$git_repos %>% dplyr::filter(category == "plugin"), key_col = "id", text_col = "name"))
    })
    
    # Update plugins catalog when a remote git repo is selected
    
    observeEvent(input$remote_git_repo, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$remote_git_repo"))
      
      # Get URL of remote git repo
      # If there's not a slash at the end, add one
      url_address <- r$git_repos %>% dplyr::filter(id == input$remote_git_repo) %>% dplyr::pull(url_address)
      r$url_address <- url_address
      
      if (substr(url_address, nchar(url_address), nchar(url_address)) != "/") url_address <- paste0(url_address, "/")
      
      error_loading_remote_git <- TRUE
      
      plugins_file <- paste0(app_folder, "/temp_files/plugins.xml")
      
      if (r$has_internet){
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$has_internet"))
        
        tryCatch({
          xml2::download_xml(url = paste0(url_address, "plugins.xml"), file = plugins_file)
          error_loading_remote_git <- FALSE
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_connection_remote_git", 
          error_name = "plugins_catalog load plugins.xml", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      # remote_git plugins
      
      if (error_loading_remote_git) r$remote_git_plugins <- tibble::tibble(type = integer(), unique_id = character())
      
      else r$remote_git_plugins <-
        xml2::read_xml(plugins_file) %>%
        XML::xmlParse() %>%
        XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
        tibble::as_tibble()
      
      r$reload_plugins_document_cards <- Sys.time()
      
      r$error_loading_remote_git <- error_loading_remote_git
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$remote_git_repo"))
    })
    
    observeEvent(r$plugins, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$plugins"))
      r$reload_plugins_document_cards <- Sys.time()
    }, once = TRUE)
   
    observeEvent(input$reload_plugins_document_cards, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$reload_plugins_document_cards"))
      r$reload_plugins_document_cards <- Sys.time()
    })
    
    observeEvent(r$reload_plugins_document_cards, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$reload_plugins_document_cards"))
      
      if (length(r$remote_git_plugins) == 0) r$remote_git_plugins <- tibble::tibble(type = integer(), unique_id = character())
      
      plugins <- list()
      plugins$remote_git <- r$remote_git_plugins %>% dplyr::filter(type == tab_type_id) %>% dplyr::mutate(id = unique_id)
      plugins$local <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id, !deleted)
      r$plugins_images <- tibble::tibble(type = character(), id = character(), image_url = character())
      
      print(plugins$remote_git)
      
      for(type in c("remote_git", "local")){
        
        all_plugins <- tagList()
        all_plugins_document_cards <- tagList()
       
        if (type == "remote_git"){
          if (length(input$remote_git_repo) == 0) output$all_plugins_remote_git <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("choose_remote_git"), messageBarType = 5)))
          else if (r$error_loading_remote_git) output$all_plugins_remote_git <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("error_connection_remote_git"), messageBarType = 3)))
          else if (nrow(plugins$remote_git) == 0) output$all_plugins_remote_git <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("no_available_plugin"), messageBarType = 5)))
        }
        else if (type == "local" & nrow(plugins$local) == 0) output$all_plugins_local <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("no_available_plugin"), messageBarType = 5)))
        
        if (nrow(plugins[[type]]) > 0){
  
          i <- 0

          for(plugin_id in plugins[[type]] %>% dplyr::pull(id)){
            
            if (type == "local"){
              plugin <- list()
              plugin$name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)
              plugin$options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
              plugin$unique_id <- plugin$options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
              plugin$author <- plugin$options %>% dplyr::filter(name == "author") %>% dplyr::pull(value)
              plugin$version <- plugin$options %>% dplyr::filter(name == "version") %>% dplyr::pull(value)
              
              if (plugin$options %>% dplyr::filter(name == "image") %>% dplyr::pull(value) != ""){
                plugin$image_url <- paste0(app_folder, "/plugins/", prefix, "/",
                  plugin$options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value), "/",
                  plugin$options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
              }
              else plugin$image_url <- ""
            }
            
            if (type == "remote_git"){
              plugin <- plugins$remote_git %>% dplyr::filter(id == plugin_id)
              
              plugin$name <- plugin[[paste0("name_", language)]]
              
              if (plugin$image != "") plugin$image_url <- paste0(r$url_address, prefix, "/", plugin$unique_id, "/", plugin$image)
              else plugin$image_url <- ""
            }
            
            r$plugins_images <- r$plugins_images %>% dplyr::bind_rows(
              tibble::tribble(~type, ~id, ~image_url, type, as.character(plugin_id), plugin$image_url))
        
            if (type == "remote_git") image_div <- uiOutput(ns(paste0(plugin_id, "_image")))
            if (type == "local") image_div <- imageOutput(ns(paste0(plugin_id, "_image")))
            
            all_plugins_document_cards <- tagList(all_plugins_document_cards,

              shiny.fluent::Link(
                div(
                  div(image_div, style = "height:200px; background-color:#FAF9F8;"),
                  div(style = "height:20px;"),
                  div(shiny.fluent::Text(plugin$name, variant = "large"), style = "margin-left:10px; margin-top:-10px; margin-bottom:5px;"),
                  div(
                    div(shiny.fluent::Persona(imageInitials = gsub("([A-Z])[^A-Z]+", "\\1", plugin$author),
                      size = 2, initialsColor = sample(1:10, 1)), style = "float:left; margin-left:10px; margin-top:10px;"),
                    div(
                      div(shiny.fluent::Text(plugin$author, variant = "medium", style = "font-weight:bold;")),
                      div(shiny.fluent::Text(plugin$version, variant = "small"), style = "margin-top:-4px;"),
                      style = "float:left; margin-top:5px;"
                    )
                  ),
                  style = "height:290px; width:320px; border:solid 1px #ECEBE9; img:hover{border:5px;}"
                ),
                onClick = htmlwidgets::JS(paste0("function() { ",
                  "Shiny.setInputValue('", id, "-show_plugin_details', Math.random());",
                  "Shiny.setInputValue('", id, "-plugin_id', '", plugin_id, "');",
                  "}")),
                style = "height:272px; width:320px;"
              )
            )

            i <- i + 1

            if (i %% 3 == 0){
              all_plugins <- tagList(all_plugins, br(),
                div(
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                    all_plugins_document_cards
                  )
                ), br()
              )

              all_plugins_document_cards <- tagList()
            }
          }

          if (i %% 3 != 0){
            all_plugins <- tagList(all_plugins, br(),
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                  all_plugins_document_cards
                )
              )
            )
          }
          
          output[[paste0("all_plugins_", type)]] <- renderUI(tagList(all_plugins, br()))
        }
      }
      
      r$load_plugins_images <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$reload_plugins_document_cards"))
    })
    
    observeEvent(r$load_plugins_images, {
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$load_plugins_images"))
      
      req(nrow(r$plugins_images) > 0)
      sapply(1:nrow(r$plugins_images), function(i){
        
        row <- r$plugins_images[i, ]
        
        if (row$type == "remote_git"){
          output[[paste0(row$id, "_image")]] <- renderUI({
            req(row$image_url != "")
            tags$img(src = row$image_url, width = 318, height = 200)
          })
        }
        if (row$type == "local"){
          output[[paste0(row$id, "_image")]] <- renderImage({
            req(row$image_url != "")
            list(src = row$image_url, width = 318, height = 200)
          },
            deleteFile = FALSE)
        }
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$load_plugins_images"))
    })
    
    observeEvent(input$show_plugin_details, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$show_plugin_details"))
      r$show_plugin_details <- Sys.time()
    })
    
    observeEvent(r$show_plugin_details, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$show_plugin_details"))
      
      req(input$show_plugin_details)
      
      # Render markdown description
      
      dir <- paste0(app_folder, "/temp_files")
      
      markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", dir, "')\n",
        "knitr::opts_chunk$set(root.dir = '", dir, "', fig.path = '", dir, "')\n```\n")
      
      # For local plugins
      
      if (input$all_plugins_source == "local"){
        plugin <- r$plugins %>% dplyr::filter(id == input$plugin_id)
        plugin_options <- r$options %>% dplyr::filter(category == "plugin", link_id == input$plugin_id)
        
        plugin_description <- paste0(
          "**Auteur** : ", plugin_options %>% dplyr::filter(name == "author") %>% dplyr::pull(value), "<br />",
          "**Version** : ", plugin_options %>% dplyr::filter(name == "version") %>% dplyr::pull(value), "\n\n",
          plugin_options %>% dplyr::filter(name == paste0("description_", tolower(language))) %>% dplyr::pull(value)
        )
        
        plugin_folder <- paste0(app_folder, "/plugins/", prefix, "/", plugin_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      }
      
      # For remote_git plugins
      
      if (input$all_plugins_source == "remote_git"){
        plugin <- r$remote_git_plugins %>% dplyr::filter(unique_id == input$plugin_id)

        plugin$name <- plugin[[paste0("name_", language)]]
        
        plugin_description <- paste0(
          "**Auteur** : ", plugin$name, "<br />",
          "**Version** : ", plugin$version, "\n\n",
          plugin %>% dplyr::pull(paste0("description_", tolower(language)))
        )
        
        plugin_folder <- paste0(r$url_address, prefix, "/", plugin$unique_id)
      }
      
      # Change %plugin_folder% for images
      plugin_description <- plugin_description %>% stringr::str_replace_all("%plugin_folder%", plugin_folder)
      
      markdown_file <- paste0(markdown_settings, plugin_description)
      
      # Create temp dir
      file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      if (!dir.exists(dir)) dir.create(dir)
      
      # Create the markdown file
      knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      
      update_plugin_div <- ""
      
      if (input$all_plugins_source == "remote_git"){
        
        plugins <- 
          r$plugins %>%
          dplyr::filter(!deleted, tab_type_id == !!tab_type_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          ) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "version") %>% dplyr::select(id = link_id, version = value),
            by = "id"
          )
        
        plugins_join <- plugins %>% dplyr::inner_join(plugin %>% dplyr::select(unique_id), by = "unique_id")
        
        # Check if this plugin is already installed
        
        if (nrow(plugins_join) > 0){
          
          # Check if this plugin is up to date

          if (compareVersion(plugins_join %>% dplyr::pull(version), plugin$version) == 0) update_plugin_div <- shiny.fluent::DefaultButton.shinyInput(
            ns("update_plugin"), i18n$t("plugin_up_to_date"), disabled = TRUE)
          else if (compareVersion(plugins_join %>% dplyr::pull(version), plugin$version) == 1) update_plugin_div <- shiny.fluent::DefaultButton.shinyInput(
            ns("update_plugin"), i18n$t("local_plugin_newer"), disabled = TRUE)
          else update_plugin_div <- shiny.fluent::PrimaryButton.shinyInput(ns("update_plugin"), i18n$t("update_plugin"))
        }
        
        else update_plugin_div <- shiny.fluent::PrimaryButton.shinyInput(ns("install_plugin"), i18n$t("install_plugin"))
        
        update_plugin_div <- tagList(update_plugin_div, br())
      }
        
      output$all_plugins_plugin_details_title <- renderUI(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::ActionButton.shinyInput(ns("all_plugins_show_document_cards"), "", iconProps = list(iconName = "Back")), style = "position:relative; bottom:6px;"), 
          plugin$name
        )
      )
      
      output$all_plugins_plugin_details_content <- renderUI(
        tagList(
          update_plugin_div,
          div(
            div(class = "markdown", tagList(withMathJax(includeMarkdown(file)))),
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
          )
        )
      )
        
      shinyjs::hide("all_plugins_document_cards")
      shinyjs::show("all_plugins_plugin_details")

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$show_plugin_details"))
    })
    
    observeEvent(input$all_plugins_show_document_cards, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$all_plugins_show_document_cards"))
      shinyjs::show("all_plugins_document_cards")
      shinyjs::hide("all_plugins_plugin_details")
    })
    
    # Install of update a remote_git plugin
    
    observeEvent(input$install_plugin, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$install_plugin"))
      r$install_update_plugin <- "install"
      r$install_update_plugin_trigger <- Sys.time()
    })
    
    observeEvent(input$update_plugin, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$update_plugin"))
      r$install_update_plugin <- "update"
      r$install_update_plugin_trigger <- Sys.time()
    })
    
    observeEvent(r$install_update_plugin_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$install_update_plugin_trigger"))
      
      req(isTruthy(input$install_plugin) || isTruthy(input$update_plugin))
      
      plugin <- r$remote_git_plugins %>% dplyr::filter(unique_id == input$plugin_id)
      
      tryCatch({
        
        # Delete translations file
        # Create plugin folder in translations folder if doesn't exist
        translations_dir <- paste0(r$app_folder, "/translations/", plugin$unique_id)
        if (dir.exists(translations_dir)) unlink(translations_dir, recursive = TRUE)
        
        # Delete old rows
        
        if (r$install_update_plugin == "install") new_row <- get_last_row(r$db, "plugins") + 1
        
        if (r$install_update_plugin == "update"){
          
          new_row <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", value == plugin$unique_id) %>% 
            dplyr::select(id = link_id, value) %>%
            dplyr::inner_join(r$plugins %>% dplyr::filter(!deleted), by = "id") %>%
            dplyr::pull(id)
          
          sql <- glue::glue_sql("DELETE FROM plugins WHERE id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$plugins <- r$plugins %>% dplyr::filter(id != new_row)
          
          sql <- glue::glue_sql("DELETE FROM options WHERE category = 'plugin' AND link_id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$options <- r$options %>% dplyr::filter(category != "plugin" | (category == "plugin" & link_id != new_row))
          
          sql <- glue::glue_sql("DELETE FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$code <- r$code %>% dplyr::filter(category %not_in% c("plugin_ui", "plugin_server", "plugin_translations") | 
            (category %in% c("plugin_ui", "plugin_server", "plugin_translations") & link_id != new_row))
        }
        
        # Plugin table
        
        new_data <- tibble::tribble(
          ~id, ~name, ~description, ~tab_type_id, ~datetime, ~deleted,
          new_row, as.character(plugin[[paste0("name_", language)]]), "", as.integer(tab_type_id), as.character(Sys.time()), FALSE)
        print(new_data)
        DBI::dbAppendTable(r$db, "plugins", new_data)
        add_log_entry(r = r, category = paste0("plugins - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r$plugins <- r$plugins %>% dplyr::bind_rows(new_data)
        
        # Options table
        
        last_row_options <- get_last_row(r$db, "options")
        
        new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
          last_row_options + 1, "plugin", new_row, "users_allowed_read_group", "everybody", 1, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 2, "plugin", new_row, "user_allowed_read", "", r$user_id, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 3, "plugin", new_row, "version", plugin$version, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 4, "plugin", new_row, "unique_id", plugin$unique_id, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 5, "plugin", new_row, "author", plugin$author, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 6, "plugin", new_row, "image", plugin$image, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 7, "plugin", new_row, "description_fr", plugin$description_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 8, "plugin", new_row, "description_en", plugin$description_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 9, "plugin", new_row, "category_fr", plugin$category_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 10, "plugin", new_row, "category_en", plugin$category_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 11, "plugin", new_row, "name_fr", plugin$name_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
          last_row_options + 12, "plugin", new_row, "name_en", plugin$name_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE
        )
        
        DBI::dbAppendTable(r$db, "options", new_options)
        add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_options))
        r$options <- r$options %>% dplyr::bind_rows(new_options)
        
        # Code table
        
        plugin_ui_code <- 
          paste0("https://raw.remote_gitusercontent.com/BorisDelange/linkr-content/main/plugins/", prefix, "/", plugin$unique_id, "/ui.R") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n")
        
        plugin_server_code <-
          paste0("https://raw.remote_gitusercontent.com/BorisDelange/linkr-content/main/plugins/", prefix, "/", plugin$unique_id, "/server.R") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n")
        
        plugin_translations_code <-
          paste0("https://raw.remote_gitusercontent.com/BorisDelange/linkr-content/main/plugins/", prefix, "/", plugin$unique_id, "/translations.csv") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n")
        
        last_row_code <- get_last_row(r$db, "code")
        
        new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
          last_row_code + 1, "plugin_ui", new_row, plugin_ui_code, r$user_id, as.character(Sys.time()), FALSE,
          last_row_code + 2, "plugin_server", new_row, plugin_server_code, r$user_id, as.character(Sys.time()), FALSE,
          last_row_code + 3, "plugin_translations", new_row, plugin_translations_code, r$user_id, as.character(Sys.time()), FALSE)
        
        DBI::dbAppendTable(r$db, "code", new_code)
        add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_code))
        r$code <- r$code %>% dplyr::bind_rows(new_code)
        
        # update_r(r = r, table = "plugins")
        # update_r(r = r, table = "options")
        # update_r(r = r, table = "code")
        
        r$show_plugin_details <- Sys.time()
        # r[[paste0("reload_", prefix, "_plugin")]] <- Sys.time()
        
        # Reload datatable
        r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        
        show_message_bar(output,  "success_installing_remote_git_plugin", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_install_remote_git_plugin", 
        error_name = paste0("install_remote_git_plugin - id = ", plugin$unique_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$install_update_plugin_trigger"))
    })
    
    # --- --- --- --- -- -
    # Create a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$add_plugin"))
        
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$plugin_name)
      new_data$plugin_name <- new_data$name
      new_data$tab_type <- tab_type_id
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = "plugins", required_textfields = "plugin_name", req_unique_values = "name")
      
      # Reload datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$add_plugin"))
    })
    
    # --- --- --- --- --- ---
    # Plugins management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons_plugins_management <- c("delete", "edit_code", "options")
    action_buttons_export_plugins <- "add"
    
    editable_cols <- ""
    sortable_cols <- c("id", "name", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
    centered_cols <- c("id", "datetime", "action")
    searchable_cols <- c("name")
    hidden_cols <- c("id", "description", "tab_type_id", "deleted", "modified")
    col_names <- get_col_names("plugins", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$plugins, {
      
      if (nrow(r$plugins) == 0){
        r[[paste0(prefix, "_plugins_temp")]] <- tibble::tibble(id = integer(), name = character(), description = character(),
          tab_type_id = integer(), datetime = character(), deleted = integer(), modified = logical())
      }
      else r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
    }, once = TRUE)
    
    observeEvent(r[[paste0(prefix, "_plugins_temp")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..plugins_temp"))
      r[[paste0(prefix, "_plugins_reload_datatables")]] <- Sys.time()
    })
    observeEvent(r[[paste0(prefix, "_export_plugins_temp")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..export_plugins_temp"))
      r[[paste0(prefix, "_plugins_reload_datatables")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_plugins_reload_datatables")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..plugins_reload_datatables"))
      
      # Reset selected plugins for export and export_plugins var for datatable
      r[[paste0(prefix, "_export_plugins_temp")]] <- r[[paste0(prefix, "_plugins_temp")]]
      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::slice(0)
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]]) == 0){
        
        # Default data for datatable
        
        data_plugins_datatable <- tibble::tibble(id = integer(), name = character(), description = character(),
          tab_type_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
        data_export_plugins_datatable <- data_plugins_datatable
        
      }
      if (nrow(r[[paste0(prefix, "_plugins_temp")]]) > 0){
      
        # Prepare data for datatables
  
        r[[paste0(prefix, "_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_plugins_management, data_input = r[[paste0(prefix, "_plugins_temp")]])
        data_plugins_datatable <- r[[paste0(prefix, "_plugins_datatable_temp")]]
        
        r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_export_plugins, data_input = r[[paste0(prefix, "_export_plugins_temp")]])
        data_export_plugins_datatable <- r[[paste0(prefix, "_export_plugins_datatable_temp")]]
      }
      
      # Render datatables
      
      # If datatable already exists
      if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) > 0){
        DT::replaceData(r[[paste0(prefix, "_plugins_datatable_proxy")]], data_plugins_datatable, resetPaging = FALSE, rownames = FALSE)
        DT::replaceData(r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]], data_export_plugins_datatable, resetPaging = FALSE, rownames = FALSE)
      }
      
      # If datatable doesn't exist
      if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) == 0){

        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data_plugins_datatable,
          output_name = "plugins_datatable", col_names =  get_col_names(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, selection = "multiple")
  
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data_export_plugins_datatable,
          output_name = "plugins_to_export_datatable", col_names =  get_col_names(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
  
        # Create a proxy for datatables
  
        r[[paste0(prefix, "_plugins_datatable_proxy")]] <- DT::dataTableProxy("plugins_datatable", deferUntilFlush = FALSE)
        r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]] <- DT::dataTableProxy("plugins_to_export_datatable", deferUntilFlush = FALSE)
      }
      
      # Update dropdowns
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      if (length(input$options_selected_plugin) == 0) link_id <- NULL
      else if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$options_selected_plugin
      
      if (is.null(link_id)) value <- NULL
      if (!is.null(link_id)) value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..plugins_reload_datatables"))
    })
    
    observeEvent(r[[paste0(prefix, "_export_plugins_temp")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..export_plugins_temp"))

      # Reload datatable_temp variable
      if (nrow(r[[paste0(prefix, "_export_plugins_temp")]] == 0)){
        data <- tibble::tibble(id = integer(), name = character(), description = character(),
          tab_type_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      if (nrow(r[[paste0(prefix, "_export_plugins_temp")]]) > 0){
        r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_export_plugins, data_input = r[[paste0(prefix, "_export_plugins_temp")]])
        data <- r[[paste0(prefix, "_export_plugins_datatable_temp")]]
      }

      # Reload data of datatable
      if (length(r[[paste0(prefix, "_export_plugins_datatable_proxy")]]) > 0) DT::replaceData(r[[paste0(prefix, "_export_plugins_datatable_proxy")]], data, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..export_plugins_temp"))
    })

    # Updates on datatable data
    observeEvent(input$plugins_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_datatable_cell_edit"))

      edit_info <- input$plugins_datatable_cell_edit
      r[[paste0(prefix, "_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_plugins_temp")]], edit_info, rownames = FALSE)

      # Store that this row has been modified
      r[[paste0(prefix, "_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })

    observeEvent(input$plugins_to_export_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_to_export_datatable_cell_edit"))

      edit_info <- input$plugins_to_export_datatable_cell_edit
      r[[paste0(prefix, "_export_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_export_plugins_temp")]], edit_info, rownames = FALSE)

      # Store that this row has been modified
      r[[paste0(prefix, "_export_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_plugins_management, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_plugins_management"))
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      req(nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "plugins", r_table = paste0(prefix, "_plugins"), i18n = i18n, duplicates_allowed = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$save_plugins_management"))
    })
    
    # Delete a row or multiple rows in datatable

    plugin_delete_prefix <- paste0(prefix, "_plugin")
    plugin_dialog_title <- "plugins_delete"
    plugin_dialog_subtext <- "plugins_delete_subtext"
    plugin_react_variable <- "plugin_delete_confirm"
    plugin_table <- "plugins"
    plugin_id_var_sql <- "id"
    plugin_id_var_r <- "delete_plugins"
    plugin_delete_message <- "plugins_deleted"
    plugin_reload_variable <- paste0("reload" , prefix, "_plugins")
    plugin_information_variable <- "plugin_deleted"
    plugin_delete_variable <- paste0(plugin_delete_prefix, "_open_dialog")

    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = plugin_delete_prefix, dialog_title = plugin_dialog_title, dialog_subtext = plugin_dialog_subtext,
      react_variable = plugin_react_variable, table = plugin_table, id_var_sql = plugin_id_var_sql, id_var_r = plugin_id_var_r,
      delete_message = plugin_delete_message, translation = TRUE, reload_variable = plugin_reload_variable,
      information_variable = plugin_information_variable, app_folder = app_folder, prefix = prefix)

    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$deleted_pressed"))
      
      r$delete_plugins <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[plugin_delete_variable]] <- TRUE
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$delete_selection"))
      
      req(length(input$plugins_datatable_rows_selected) > 0)
      
      r$delete_plugins <- r[[paste0(prefix, "_plugins_temp")]][input$plugins_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[plugin_delete_variable]] <- TRUE
    })

    observeEvent(r[[plugin_reload_variable]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$reload..plugins"))
      
      # Reload datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reload remote_git description if opened
      r$show_plugin_details <- Sys.time()
      
      # Reload export plugin datatable
      
      # Reset "edit plugin code" fields and "plugin options" fields
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")

      sapply(c("code_selected_plugin", "thesaurus", "thesaurus_selected_items"), 
        function(name) shiny.fluent::updateComboBox.shinyInput(session, name, options = options, value = NULL))
      shiny.fluent::updateChoiceGroup.shinyInput(session, "edit_code_ui_server", value = "ui")
      shiny.fluent::updateToggle.shinyInput(session, "hide_editor", value = FALSE)
      sapply(c("ace_edit_code_ui", "ace_edit_code_server", "ace_edit_code_translations"),
        function(name) shinyAce::updateAceEditor(session, name, value = ""))

      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = NULL)
      sapply(c("plugin_author", "plugin_version", "plugin_name_fr", "plugin_name_en", "plugin_category_fr", "plugin_category_en"),
        function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group", value = "everybody")
      shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", options = list(), value = NULL)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "plugin_description_language", value = "fr")
      sapply(c("plugin_description_fr", "plugin_description_en"), function(name) shinyAce::updateAceEditor(session, name, value = ""))

      shiny.fluent::updateComboBox.shinyInput(session, "plugins_to_export", value = "")
    })
    
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$edit_code"))
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('edit_plugin_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('plugin_options')}\"]').click();"))
    })
    
    # --- --- --- -- -- -
    # Plugin options ----
    # --- --- --- -- -- -
    
    observeEvent(input$options_selected_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$options_selected_plugin"))

      if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$options_selected_plugin
      if (length(input$code_selected_plugin) > 0){
        if (length(input$code_selected_plugin) > 1) code_link_id <- input$code_selected_plugin$key
        else code_link_id <- input$code_selected_plugin
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id){
        options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      }
      
      # Plugin options
      
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
      req(nrow(options) > 0)
      
      # All users
      picker_options <-
        r$users %>%
        dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
        dplyr::transmute(
          key = id,
          imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
          text = paste0(firstname, " ", lastname),
          secondaryText = user_status)
      
      # Users who has access
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
      
      selected_items <- picker_options %>% dplyr::filter(key %in% value)
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Plugin version, author, image and descriptions
      
      plugin_folder <- paste0(app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG)$")
      shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
        options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
        value = options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
      
      for (field in c("version", "author", "name_fr", "name_en", "category_fr", "category_en")) shiny.fluent::updateTextField.shinyInput(session,
        paste0("plugin_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
      for (field in c("description_fr", "description_en")) shinyAce::updateAceEditor(session,
        paste0("plugin_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
      # Update plugin name if open in options
      # shiny.fluent::updateTextField.shinyInput(session, paste0("plugin_name_", language),
      #   value = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$options_selected_plugin"))
    })
    
    # Render selected image
    
    output$render_image <- renderImage({
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$render_image"))
      
      req(length(input$plugin_image) > 0)
      req(input$plugin_image != "")
      
      link_id <- input$options_selected_plugin$key
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
      plugin_folder <- paste0(app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      
      list(src = paste0(plugin_folder, "/", input$plugin_image), width = 318, height = 200)
    }, deleteFile = FALSE)
    
    # Save updates
    
    observeEvent(input$plugin_description_fr_save, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_description_fr_save"))
      r[[paste0(id, "_save_options")]] <- Sys.time()
    })
    observeEvent(input$plugin_description_en_save, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_description_en_save"))
      r[[paste0(id, "_save_options")]] <- Sys.time()
    })
    observeEvent(input$save_plugin_options, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_plugin_options"))
      r[[paste0(id, "_save_options")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(id, "_save_options")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..save_options"))
      
      plugin_name <- input[[paste0("plugin_name_", language)]]
      if (is.na(plugin_name) | plugin_name == "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("plugin_name_", language), errorMessage = i18n$t("provide_valid_name"))
      
      req(!is.na(plugin_name) | plugin_name != "")
      
      if (!is.na(plugin_name) | plugin_name != "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("plugin_name_", language), errorMessage = NULL)
      
      req(length(input$options_selected_plugin) > 0)
      if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$code_selected_plugin
      
      data <- list()
      data$users_allowed_read <- unique(input$users_allowed_read)
      for (field in c("users_allowed_read_group", "plugin_version", "plugin_author",
        "plugin_name_fr", "plugin_name_en", "plugin_category_fr", "plugin_category_en",
        "plugin_image", "plugin_description_fr", "plugin_description_en")) data[[stringr::str_replace(field, "plugin_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "plugin", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("users_allowed_read", "version", "author", "image", "description_fr", "description_en",
          "name_fr", "name_en", "category_fr", "category_en"))
      
      # Change plugin_name in plugins table
      sql <- glue::glue_sql("UPDATE plugins SET name = {plugin_name} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$plugins <- r$plugins %>% dplyr::mutate(name = dplyr::case_when(id == link_id ~ plugin_name, TRUE ~ name))
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      r$show_plugin_details <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..save_options"))
    })
    
    # Delete an image
    
    observeEvent(input$delete_image, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$delete_image"))
      r[[paste0(prefix, "_plugins_delete_image")]] <- TRUE
    })
    
    r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    output$plugin_image_delete_confirm <- shiny.fluent::renderReact({
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$plugin_image_delete_confirm"))
      
      shiny.fluent::Dialog(
        hidden = !r[[paste0(prefix, "_plugins_delete_image")]],
        onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", prefix, "_plugin_delete_image_hide_dialog', Math.random()); }")),
        dialogContentProps = list(
          type = 0,
          title = i18n$t("plugin_image_delete"),
          closeButtonAriaLabel = "Close",
          subText = tagList(i18n$t("plugin_image_delete_subtext"), br(), br())
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_plugin_delete_image_delete_confirmed")), text = i18n$t("delete")),
          shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "_plugin_delete_image_delete_canceled")), text = i18n$t("dont_delete"))
        )
      )
    })
    
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_hide_dialog")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_hide_dialog"))
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_delete_canceled")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_delete_canceled"))
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_delete_confirmed")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_delete_confirmed"))
      
      req(input$plugin_image != "")
      tryCatch({
        link_id <- input$options_selected_plugin$key
        
        plugin <- r$plugins %>% dplyr::filter(id == link_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          )
        
        plugin_folder <- paste0(app_folder, "/plugins/", prefix, "/", plugin$unique_id)
        unlink(paste0(plugin_folder, "/", input$plugin_image))
        
        files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG)$")
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
          value = "")
        
        show_message_bar(output,  "image_deleted", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_deleting_image",
        error_name = paste0(id, " - delete plugin image"), category = "Error", error_report = e, i18n = i18n))
      
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    
    # Import an image
    
    observeEvent(input$import_image, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_image"))
      shinyjs::click("import_image_file")
    })
    
    observeEvent(input$import_image_file, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_image_file"))
      
      tryCatch({
        
        # if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
        # else link_id <- input$code_selected_plugin
        link_id <- input$options_selected_plugin$key
        
        plugin <- r$plugins %>%
          dplyr::filter(id == link_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          )
        
        plugin_folder <- paste0(app_folder, "/plugins/", prefix, "/", plugin$unique_id)
        
        if (!dir.exists(plugin_folder)) dir.create(plugin_folder, recursive = TRUE)
        file.copy(input$import_image_file$datapath, paste0(plugin_folder, "/", input$import_image_file$name), overwrite = TRUE)
        
        # Update dropdown
        
        options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
        
        files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG)$")
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
          value = options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
        
        show_message_bar(output,  "image_imported", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_importing_image",
          error_name = paste0(id, " - import plugin image"), category = "Error", error_report = e, i18n = i18n))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$import_image_file"))
    })
    
    # --- --- --- --- --- -
    # Edit plugin code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_selected_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$code_selected_plugin"))
      
      if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
      else link_id <- input$code_selected_plugin
      if (length(input$options_selected_plugin) > 0){
        if (length(input$options_selected_plugin) > 1) options_link_id <- input$options_selected_plugin$key
        else options_link_id <- input$options_selected_plugin
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      }

      # Get code from database
      
      req(r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% nrow() > 0)
      
      code <- list()
      code$ui <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
      code$server <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code)
      code$translations <- r$code %>% dplyr::filter(category == "plugin_translations" & link_id == !!link_id) %>% dplyr::pull(code)
      
      shinyAce::updateAceEditor(session, "ace_edit_code_ui", value = code$ui)
      shinyAce::updateAceEditor(session, "ace_edit_code_server", value = code$server)
      shinyAce::updateAceEditor(session, "ace_edit_code_translations", value = code$translations)
      
      # Render UI of this edit_code card
      output$edit_code_card <- renderUI({
        render_settings_code_card(ns = ns, r = r, id = id, title = paste0("edit_plugins_code"), code = code, link_id = link_id, i18n = i18n)
      })

      # Reset code_result textOutput
      output$datetime_code_execution <- renderText("")
      output$code_result_ui <- renderUI("")
      output$code_result_server <- renderText("")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$code_selected_plugin"))
    })
    
    # Load thesaurus items
    
    if (prefix == "patient_lvl"){
      
      # --- --- --- --- --- --- -
      ## Thesaurus datatable ----
      # --- --- --- --- --- --- -
      
      # Load thesaurus attached to this dataset
      observeEvent(r$selected_dataset, {
        
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$selected_dataset"))
        
        req(!is.na(r$selected_dataset))
        
        data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id) %>% as.character()
        
        # Multiple cases
        # Only one ID, so it's the beginning and the end
        # Last ID, so it's the end
        # ID between begin and last, so separated by commas
        thesaurus <- r$thesaurus %>% 
          dplyr::filter(
            grepl(paste0("^", data_source, "$"), data_source_id) | 
            grepl(paste0(", ", data_source, "$"), data_source_id) | 
            grepl(paste0("^", data_source, ","), data_source_id) |
            grepl(paste0(", ", data_source, ","), data_source_id)
          ) %>% 
          dplyr::arrange(name)
        shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", i18n = i18n), value = NULL)
        
      })
      
      # Load thesaurus items
      observeEvent(input$thesaurus, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$thesaurus"))
        
        r$plugin_thesaurus_items <- create_datatable_cache(output = output, r = r, i18 = i18, tab_id = id, thesaurus_id = input$thesaurus$key, category = "plus_plugin")
        
        colour_col <- create_datatable_cache(output = output, r = r, i18n = i18n, tab_id = id, thesaurus_id = input$thesaurus$key, category = "colours_plugin")
        
        if (nrow(colour_col) > 0) r$plugin_thesaurus_items <- r$plugin_thesaurus_items %>%
          dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
        
        count_items_rows <- tibble::tibble()
        count_patients_rows <- tibble::tibble()
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
          dataset_id = r$selected_dataset, category = "count_items_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_dataset", 
            error_name = paste0("plugins - create_datatable_cache - count_items_rows - fail_load_dataset - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
          dataset_id = as.integer(r$selected_dataset), category = "count_patients_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_dataset", 
            error_name = paste0("plugins - create_datatable_cache - count_patients_rows - fail_load_dataset - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, "fail_load_dataset", "severeWarning", i18n = i18n, ns = ns)
        req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
        
        # Transform count_rows cols to integer, to be sortable
        r$plugin_thesaurus_items <- r$plugin_thesaurus_items %>%
          dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
          dplyr::left_join(count_items_rows, by = "item_id") %>%
          dplyr::left_join(count_patients_rows, by = "item_id") %>%
          dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
          dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
        
        # Filter on count_items_rows > 0
        r$plugin_thesaurus_items <- r$plugin_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
        
        r$plugin_thesaurus_items_temp <- r$plugin_thesaurus_items %>%
          dplyr::mutate(modified = FALSE) %>%
          dplyr::mutate_at("item_id", as.character)
        
        editable_cols <- c("display_name", "unit")
        searchable_cols <- c("item_id", "name", "display_name", "unit")
        factorize_cols <- c("unit")
        column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")
        sortable_cols <- c("id", "item_id", "name", "display_name", "count_patients_rows", "count_items_rows")
        centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        col_names <- get_col_names(table_name = "tabs_thesaurus_items_with_counts", i18n = i18n)
        hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
        
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$plugin_thesaurus_items_temp,
          output_name = "plugin_thesaurus_items", col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        # Create a proxy for datatatable
        r$plugin_thesaurus_items_proxy <- DT::dataTableProxy("plugin_thesaurus_items", deferUntilFlush = FALSE)
        
        shinyjs::hide("blank_space")
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$thesaurus"))
      })
      
      # Reload datatable
      
      observeEvent(r$plugin_thesaurus_items_temp, {
        
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$plugin_thesaurus_items_temp"))
        
        # Reload data of datatable
        DT::replaceData(r$plugin_thesaurus_items_proxy, r$plugin_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
      })
      
      # Updates in datatable
      
      observeEvent(input$plugin_thesaurus_items_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_thesaurus_items_cell_edit"))
        
        edit_info <- input$plugin_thesaurus_items_cell_edit
        
        r$plugin_thesaurus_items_temp <- DT::editData(r$plugin_thesaurus_items_temp, edit_info, rownames = FALSE)
        r$plugin_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      })
      
      # --- --- --- --- -- -
      # Thesaurus items ----
      # --- --- --- --- -- -
      
      # When add button is clicked
      observeEvent(input$item_selected, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$item_selected"))
        
        # Initiate r variable if doesn't exist
        if (length(r$plugin_thesaurus_selected_items) == 0){
          r$plugin_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
            thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
            thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
        }
        
        # Get ID of selected thesaurus item
        link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))
        
        # If this thesaurus item is not already selected, add it to the "thesaurus selected items" dropdown
        
        # value <- integer(1)
        if (nrow(r$plugin_thesaurus_selected_items) > 0) value <- r$plugin_thesaurus_selected_items %>% 
          dplyr::filter(thesaurus_id == input$thesaurus$key) %>% dplyr::pull(id)
        
        # if (link_id %not_in% value){
        
        # Get thesaurus name
        thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus$key) %>% dplyr::pull(name)
        
        # Get item informations from datatable / r$plugin_thesaurus_items
        # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
        item <- r$plugin_thesaurus_items_temp %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", name))
        
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
              thesaurus_item_unit, thesaurus_item_colour = as.character(input[[paste0("colours_", link_id)]]), 
              input_text = paste0(thesaurus_name, " - ", thesaurus_item_display_name, " (", tolower(i18n$t("mapped_item")), ")"),
              mapped_to_item_id = link_id, merge_items = input$merge_mapped_items
            ) %>%
            dplyr::anti_join(r$plugin_thesaurus_selected_items %>% dplyr::select(id), by = "id")
          # }
        }
        
        # Add item to selected items
        add_thesaurus_items <-
          # r$widget_thesaurus_selected_items %>%
          # dplyr::bind_rows(
          tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name,
            ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text, ~mapped_to_item_id, ~merge_items,
            as.integer(link_id), as.integer(input$thesaurus$key), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
            as.character(item$unit), as.character(input[[paste0("colours_", link_id)]]), as.character(item$input_text),
            NA_integer_, FALSE)
        # )
        if (nrow(thesaurus_mapped_items) > 0) add_thesaurus_items <-
          add_thesaurus_items %>% dplyr::bind_rows(thesaurus_mapped_items)
        
        r$plugin_thesaurus_selected_items <-
          r$plugin_thesaurus_selected_items %>%
          dplyr::anti_join(add_thesaurus_items %>% dplyr::select(id), by = "id") %>%
          dplyr::bind_rows(add_thesaurus_items)
        
        # Add item to selected items
        # r$plugin_thesaurus_selected_items <-
        #   tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text,
        #     as.integer(link_id), as.integer(input$thesaurus$key), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
        #     as.character(item$unit), as.character(input[[paste0("colour_", link_id)]]), as.character(item$input_text)) %>%
        #   dplyr::bind_rows(r$plugin_thesaurus_selected_items)
        
        # Update dropdown of selected items
        options <- convert_tibble_to_list(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", i18n = i18n)
        value <- r$plugin_thesaurus_selected_items %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        # }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$item_selected"))
        
      })
      
      # When reset button is clicked
      observeEvent(input$reset_thesaurus_items, {
        
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$reset_thesaurus_items"))
        
        # Reset r$plugin_thesaurus_selected_items
        r$plugin_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
          thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(),
          thesaurus_item_colour = character(), input_text = character(), mapped_to_item_id = integer(), merge_items = logical())
        
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
      
      # When dropdown is modified
      observeEvent(input$thesaurus_selected_items_trigger, {
        
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$thesaurus_selected_items_trigger"))
        
        if (length(input$thesaurus_selected_items) == 0) r$plugin_thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>% dplyr::slice(0)
        if (length(input$thesaurus_selected_items) > 0) {
          r$plugin_thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
            dplyr::filter(id %in% input$thesaurus_selected_items)
          # Delete also mapped items
          r$plugin_thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
            dplyr::filter(is.na(mapped_to_item_id) | mapped_to_item_id %in% r$plugin_thesaurus_selected_items$id)
        }
        
        # r$plugin_thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
        #   dplyr::filter(id %in% input$thesaurus_selected_items)
        options <- convert_tibble_to_list(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", i18n = i18n)
        value <- r$plugin_thesaurus_selected_items %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
    }
    
      # --- --- --- -- --
      ## Execute code ----
      # --- --- --- -- --
      
      observeEvent(input$execute_code, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$execute_code"))
        r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
        r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
        r[[paste0(id, "_trigger_code")]] <- Sys.time()
      })
      
      observeEvent(input$ace_edit_code_ui_run_selection, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_run_selection"))
        if(!shinyAce::is.empty(input$ace_edit_code_ui_run_selection$selection)) r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui_run_selection$selection
        else r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui_run_selection$line
        r[[paste0(id, "_server_code")]] <- ""
        r[[paste0(id, "_trigger_code")]] <- Sys.time()
      })
      
      observeEvent(input$ace_edit_code_server_run_selection, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_run_selection"))
        if(!shinyAce::is.empty(input$ace_edit_code_server_run_selection$selection)) r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server_run_selection$selection
        else r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server_run_selection$line
        r[[paste0(id, "_ui_code")]] <- ""
        r[[paste0(id, "_trigger_code")]] <- Sys.time()
      })
      
      observeEvent(input$ace_edit_code_ui_run_all, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_run_all"))
        r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
        r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
        r[[paste0(id, "_trigger_code")]] <- Sys.time()
      })
      
      observeEvent(input$ace_edit_code_server_run_all, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_run_all"))
        r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
        r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
        r[[paste0(id, "_trigger_code")]] <- Sys.time()
      })
      
      observeEvent(r[[paste0(id, "_trigger_code")]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..trigger_code"))
        
        var_check <- TRUE
        if (length(r$selected_dataset) == 0) var_check <- FALSE
        if (length(r$selected_dataset) > 0){
          if (is.na(r$selected_dataset) | is.na(m$selected_study)) var_check <- FALSE
          if (prefix == "patient_lvl" & is.na(m$selected_person)) var_check <- FALSE
        }

        if (!var_check) show_message_bar(output, message = "load_some_patient_data_plugin", i18n = i18n, ns = ns)

        req(var_check)
        
        # Get thesaurus items
        
        thesaurus_selected_items <- tibble::tibble(thesaurus_name = character(), item_id = integer(), display_name = character(),
          thesaurus_item_unit = character(), colour = character(), mapped_to_item_id = integer(), merge_items = logical())
        
        if (prefix == "patient_lvl"){
          
          # Check if some thesaurus items selected (not necessary)
          if (length(r$plugin_thesaurus_selected_items) > 0){
            
            # Get thesaurus items with thesaurus own item_id
            thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
              dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                thesaurus_item_unit, colour = thesaurus_item_colour, mapped_to_item_id, merge_items)
          }
        }
        
        if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
        else link_id <- input$code_selected_plugin
      
        ui_code <- r[[paste0(id, "_ui_code")]]
        server_code <- r[[paste0(id, "_server_code")]]
        
        # Replace %group_id% in ui_code with 1 for our example
        
        group_id <- get_last_row(r$db, paste0(prefix, "_widgets")) + 10^6 %>% as.integer()
        
        # Create a session number, to inactivate older observers
        # Reset all older observers
        
        session_code <- "plugin_test"
        if (length(o[[session_code]]) == 0) session_num <- 1L
        if (length(o[[session_code]]) > 0) session_num <- o[[session_code]] + 1
        o[[session_code]] <- session_num
        
        # NB : req(o[[session_code]] == session_num) must be put at the beginning of each observeEvent in plugins code
        
        ui_code <- ui_code %>% 
          stringr::str_replace_all("%tab_id%", "1") %>%
          stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
          stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
          stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
          stringr::str_replace_all("\r", "\n")
        
        if (prefix == "patient_lvl") ui_code <- ui_code %>% stringr::str_replace_all("%patient_id%", as.character(m$selected_person))
        
        server_code <- server_code %>% 
          stringr::str_replace_all("%tab_id%", "1") %>%
          stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
          stringr::str_replace_all("%widget_id%", as.character(group_id)) %>%
          stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
          stringr::str_replace_all("\r", "\n")
        
        if (prefix == "patient_lvl") server_code <- server_code %>% stringr::str_replace_all("%patient_id%", as.character(m$selected_person))
        
        output$code_result_ui <- renderUI(make_card("", tryCatch(result <- eval(parse(text = ui_code)), error = function(e) stop(e), warning = function(w) stop(w))))
        
        # Create translations file
        
        i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
        
        if (input$ace_edit_code_translations != ""){
          
          tryCatch({
            # Get plugin unique_id
            plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == !!link_id) %>% dplyr::pull(value)
            
            # Create plugin folder in translations folder if doesn't exist
            new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
            if (!dir.exists(new_dir)) dir.create(new_dir)
            
            writeLines(input$ace_edit_code_translations, paste0(new_dir, "/plugin_translations.csv"))
          },
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
            error_name = paste0(id, " - create translations files"), category = "Error", error_report = e, i18n = i18n, ns = ns))
          
          tryCatch({
            i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
            i18np$set_translation_language(language)},
            error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
              error_name = paste0(id, " - create i18n translator"), category = "Error", error_report = e, i18n = i18n, ns = ns))
        }
        
        # New environment, to authorize access to selected variables from shinyAce editor
        # We choose which vars to keep access to
        
        # Variables to hide
        new_env_vars <- list("r" = NA)
        # Variables to keep
        for (var in c("d", "m", "o", "thesaurus_selected_items", "session_code", "session_num", "i18n", "i18np")) new_env_vars[[var]] <- eval(parse(text = var))
        new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
        
        options('cli.num_colors' = 1)
        
        # Capture console output of our code
        captured_output <- capture.output(
          tryCatch(eval(parse(text = server_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
        
        # Restore normal value
        options('cli.num_colors' = NULL)
        
        output$code_result_server <- renderText(paste(paste(captured_output), collapse = "\n"))
        
        output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..trigger_code"))
      })
    
      # --- --- --- -- ---
      ## Save updates ----
      # --- --- --- -- ---
      
      observeEvent(input$ace_edit_code_ui_save, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_save"))
        r[[paste0(id, "_save_code")]] <- Sys.time()
      })
      observeEvent(input$ace_edit_code_server_save, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_save"))
        r[[paste0(id, "_save_code")]] <- Sys.time()
      })
      observeEvent(input$ace_edit_code_translations_save, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_translations_save"))
        r[[paste0(id, "_save_code")]] <- Sys.time()
      })
      observeEvent(input$save_code, {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_code"))
        r[[paste0(id, "_save_code")]] <- Sys.time()
      })
      
      observeEvent(r[[paste0(id, "_save_code")]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..save_code"))
        
        if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
        else link_id <- input$code_selected_plugin
      
        req(!is.null(link_id))
        
        # Update code
        # DON'T USE glue_sql, it adds some quotes in the code
        
        ui_code_id <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(id)
        ui_code <- stringr::str_replace_all(input$ace_edit_code_ui, "'", "''")
        server_code_id <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(id)
        server_code <- stringr::str_replace_all(input$ace_edit_code_server, "'", "''")
        translations_code_id <- r$code %>% dplyr::filter(category == "plugin_translations" & link_id == !!link_id) %>% dplyr::pull(id)
        translations_code <- stringr::str_replace_all(input$ace_edit_code_translations, "'", "''")
        
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", ui_code, "' WHERE id = ", ui_code_id)) -> query
        DBI::dbClearResult(query)
        
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", server_code, "' WHERE id = ", server_code_id)) -> query
        DBI::dbClearResult(query)
        
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", translations_code, "' WHERE id = ", translations_code_id)) -> query
        DBI::dbClearResult(query)
        
        r$code <- r$code %>% 
          dplyr::mutate(code = dplyr::case_when(id == ui_code_id ~ ui_code, TRUE ~ code)) %>%
          dplyr::mutate(code = dplyr::case_when(id == server_code_id ~ server_code, TRUE ~ code)) %>%
          dplyr::mutate(code = dplyr::case_when(id == translations_code_id ~ translations_code, TRUE ~ code))
        
        # Update datetime in plugins table
        
        new_datetime <- as.character(Sys.time())
        sql <- glue::glue_sql("UPDATE plugins SET datetime = {new_datetime} WHERE id = {link_id}", .con = r$db)
        DBI::dbSendStatement(r$db, sql) -> query
        DBI::dbClearResult(query)
        r$plugins <- r$plugins %>% dplyr::mutate(datetime = dplyr::case_when(id == link_id ~ new_datetime, TRUE ~ datetime))
        
        # Notify user
        show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..save_code"))
      })
    
    # --- --- --- --- -- -
    # Import a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$import_plugins_browse, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_plugins_browse"))
      shinyjs::click("import_plugins_upload")
    })
    
    output$import_plugins_status <- renderUI({
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$import_plugins_status"))
      
      tagList(div(
      span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"), 
      span(input$import_plugins_upload$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
    })
    
    observeEvent(input$import_plugins_button, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_plugins_button"))
      
      req(input$import_plugins_upload)
      
      tryCatch({

        # Extract ZIP file
        
        temp_dir <- paste0(app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        zip::unzip(input$import_plugins_upload$datapath, exdir = temp_dir)
        
        # Read XML file
        
        plugins <-
          xml2::read_xml(paste0(temp_dir, "/plugins/plugins.xml")) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
          tibble::as_tibble() %>%
          dplyr::left_join(
            r$plugins %>%
              dplyr::filter(tab_type_id == !!tab_type_id, !deleted) %>%
              dplyr::inner_join(
                r$options %>% dplyr::filter(category == "plugin", name == "unique_id", !deleted) %>% dplyr::select(id = link_id, unique_id = value),
                by = "id"
              ) %>%
            dplyr::select(id, unique_id),
            by = "unique_id"
          ) %>%
          dplyr::mutate(name = dplyr::case_when(
            language == "fr" ~ name_fr, TRUE ~ name_en
          )) %>%
          dplyr::relocate(id)
        
        if (!input$replace_already_existing_plugins) plugins <- plugins %>% dplyr::filter(is.na(id))
        
        # Loop over each plugin
        
        if (nrow(plugins) > 0){
          
          for (i in 1:nrow(plugins)){
  
            plugin <- plugins[i, ]
            
            # Delete old rows
            
            if (!is.na(plugin$id)){
  
              sql <- glue::glue_sql("DELETE FROM plugins WHERE id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$plugins <- r$plugins %>% dplyr::filter(id != plugin$id)
    
              sql <- glue::glue_sql("DELETE FROM options WHERE category = 'plugin' AND link_id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$options <- r$options %>% dplyr::filter(link_id != plugin$id | (link_id == plugin$id & category != "plugin"))
    
              sql <- glue::glue_sql("DELETE FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$code <- r$code %>% dplyr::filter(link_id != plugin$id | (link_id == plugin$id & category %not_in% c("plugin_ui", "plugin_server", "plugin_translations")))
            }
              
            # Plugin table
            
            new_row <- get_last_row(r$db, "plugins") + 1
  
            new_data <- tibble::tribble(
              ~id, ~name, ~description, ~tab_type_id, ~datetime, ~deleted,
              new_row, as.character(plugin[[paste0("name_", language)]]), "", as.integer(tab_type_id), as.character(Sys.time()), FALSE)
  
            DBI::dbAppendTable(r$db, "plugins", new_data)
            r$plugins <- r$plugins %>% dplyr::bind_rows(new_data)
            add_log_entry(r = r, category = paste0("plugins - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
  
            # Options table
  
            last_row_options <- get_last_row(r$db, "options")
  
            new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              last_row_options + 1, "plugin", new_row, "users_allowed_read_group", "everybody", 1, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 2, "plugin", new_row, "user_allowed_read", "", r$user_id, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 3, "plugin", new_row, "version", plugin$version, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 4, "plugin", new_row, "unique_id", plugin$unique_id, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 5, "plugin", new_row, "author", plugin$author, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 6, "plugin", new_row, "image", plugin$image, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 7, "plugin", new_row, "description_fr", plugin$description_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 8, "plugin", new_row, "description_en", plugin$description_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 9, "plugin", new_row, "category_fr", plugin$category_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 10, "plugin", new_row, "category_en", plugin$category_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 11, "plugin", new_row, "name_fr", plugin$name_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 12, "plugin", new_row, "name_en", plugin$name_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE
            )
  
            DBI::dbAppendTable(r$db, "options", new_options)
            r$options <- r$options %>% dplyr::bind_rows(new_options)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_options))
  
            # Code table
  
            plugin_ui_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/ui.R") %>% readLines(warn = FALSE) %>% paste(collapse = "\n")
            plugin_server_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/server.R") %>% readLines(warn = FALSE) %>% paste(collapse = "\n")
            plugin_translations_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/translations.csv") %>% readLines(warn = FALSE) %>% paste(collapse = "\n")
  
            last_row_code <- get_last_row(r$db, "code")
  
            new_code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
              last_row_code + 1, "plugin_ui", new_row, plugin_ui_code, r$user_id, as.character(Sys.time()), FALSE,
              last_row_code + 2, "plugin_server", new_row, plugin_server_code, r$user_id, as.character(Sys.time()), FALSE,
              last_row_code + 3, "plugin_translations", new_row, plugin_translations_code, r$user_id, as.character(Sys.time()), FALSE)
  
            DBI::dbAppendTable(r$db, "code", new_code)
            r$code <- r$code %>% dplyr::bind_rows(new_code)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_code))
            
            # Copy files
            # Create folder if doesn't exist
            plugin_dir <- paste0(app_folder, "/plugins/", prefix, "/", plugin$unique_id)
            if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)
            
            list_of_files <- list.files(paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id))
            
            # Copy files to temp dir
            file.copy(
              paste0(paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id), "/", list_of_files),
              paste0(plugin_dir, "/", list_of_files),
              overwrite = TRUE
            )
  
            r$show_plugin_details <- Sys.time()
            
            # Reload datatable
            r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
          }
        }
        
        # Show imported plugins
        
        col_names <- c(i18n$t("id"), i18n$t("type"), i18n$t("name"), i18n$t("version"), i18n$t("unique_id"), i18n$t("author"), i18n$t("image"),
          i18n$t("description_fr"), i18n$t("description_en"), i18n$t("app_version"), 
          i18n$t("name"), i18n$t("name"), i18n$t("category"), i18n$t("category"))
        centered_cols <- c("author", "version", "id")
        column_widths <- c("author" = "100px", "version" = "80px", "id" = "50px")
        hidden_cols <- c("id", "type", "unique_id", "image", "app_version", "description_fr", "description_en", 
         "name_en", "name_fr", "category_en", "category_fr")
        
        shinyjs::show("imported_plugins_div")
        
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = plugins,
          output_name = "imported_plugins", col_names = col_names, centered_cols = centered_cols, column_widths = column_widths,
          filter = FALSE, hidden_cols = hidden_cols)

        show_message_bar(output,  "success_importing_plugin", "success", i18n = i18n, time = 15000, ns = ns)
      },
      error = function(e) report_bug(r = r, output = output, error_message = "error_importing_plugin",
        error_name = paste0(id, " - import plugins"), category = "Error", error_report = e, i18n = i18n, ns = ns),
      warning = function(w) report_bug(r = r, output = output, error_message = "error_importing_plugin",
        error_name = paste0(id, " - import plugins"), category = "Error", error_report = w, i18n = i18n, ns = ns))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$import_plugins_button"))
    })
    
    # --- --- --- --- -- -
    # Export a plugin ----
    # --- --- --- --- -- -
    
    # When add button is clicked
    observeEvent(input$add_item, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$add_item"))
      
      # Get ID of selected plugin
      link_id <- as.integer(substr(input$add_item, nchar("add_item_") + 1, nchar(input$add_item)))

      # If this plugin is not already selected, add it to the selected items dropdown
      
      value <- integer(1)
      if (nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0) value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
      
      if (link_id %not_in% value){
        
        r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::filter(id == link_id) %>%
          dplyr::bind_rows(r[[paste0(prefix, "_export_plugins_selected")]])
        
        # Update dropdown of selected items
        options <- convert_tibble_to_list(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
        value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
    })
    
    # When dropdown is modified
    observeEvent(input$plugins_to_export, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_to_export"))

      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_selected")]] %>%
        dplyr::filter(id %in% input$plugins_to_export)

      options <- convert_tibble_to_list(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
      value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # Export plugins
    observeEvent(input$export_plugins, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$export_plugins"))
      
      req(nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0)
      
      shinyjs::click("export_plugins_download")
    })
    
    output$export_plugins_download <- downloadHandler(
      
      filename = function() paste0("linkr_export_plugins_", 
        Sys.time() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "_") %>% as.character(), ".zip"),
      
      content = function(file){
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - output$export_plugins_download"))
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        temp_dir <- paste0(app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        dir.create(paste0(temp_dir, "/plugins/", prefix), recursive = TRUE)
        
        for (plugin_id in r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)){
          
          plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
          options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
          code <- r$code %>% dplyr::filter(link_id == plugin_id, category %in% c("plugin_ui", "plugin_server", "plugin_translations"))
          
          # Create folder if doesn't exist
          plugin_dir <- paste0(app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)
          
          # Create ui.R & server.R
          sapply(c("ui", "server"), function(name) writeLines(code %>% dplyr::filter(category == paste0("plugin_", name)) %>% 
              dplyr::pull(code), paste0(plugin_dir, "/", name, ".R")))
          writeLines(code %>% dplyr::filter(category == "plugin_translations") %>% dplyr::pull(code), paste0(plugin_dir, "/translations.csv"))
          
          # Create XML file
          xml <- XML::newXMLDoc()
          plugins_node <- XML::newXMLNode("plugins", doc = xml)
          plugin_node <- XML::newXMLNode("plugin", parent = plugins_node, doc = xml)
          XML::newXMLNode("app_version", r$app_version, parent = plugin_node)
          XML::newXMLNode("type", tab_type_id, parent = plugin_node)
          sapply(c("version", "unique_id", "author", "image", "description_fr", "description_en",
            "name_fr", "name_en", "category_fr", "category_en"), function(name){
            XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = plugin_node)
          })
          XML::saveXML(xml, file = paste0(plugin_dir, "/plugin.xml"))
          
          list_of_files <- list.files(plugin_dir)
          
          # Copy files to temp dir
          temp_dir_copy <- paste0(temp_dir, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(temp_dir_copy)) dir.create(temp_dir_copy, recursive = TRUE)
          file.copy(
            paste0(plugin_dir, "/", list_of_files),
            paste0(temp_dir_copy, "/", list_of_files),
            overwrite = TRUE
          )
        }
        
        # Create XML file with all exported plugins
        
        plugins_dir <- paste0(temp_dir, "/plugins")
        
        plugins_tibble <- tibble::tibble(type = character(), name = character(), version = character(), unique_id = character(),
          author = character(), image = character(), description_fr = character(), description_en = character())
        
        for (category in c("patient_lvl", "aggregated")){
          
          dirs <- list.dirs(paste0(plugins_dir, "/", category), full.names = TRUE)
          
          for (dir in dirs){
            if (dir != paste0(plugins_dir, "/", category)){
              plugins_tibble <-
                plugins_tibble %>%
                dplyr::bind_rows(
                  xml2::read_xml(paste0(dir, "/plugin.xml")) %>%
                    XML::xmlParse() %>%
                    XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
                    tibble::as_tibble()
                )
            }
          }
        }
        
        plugins_xml <- XML::newXMLDoc()
        plugins_node <- XML::newXMLNode("plugins", doc = plugins_xml)
        
        plugins_nodes <- apply(plugins_tibble, 1, function(x) {
          plugin_node <- XML::newXMLNode("plugin")
          XML::addChildren(plugin_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
        })
        
        XML::xmlParent(plugins_nodes) <- plugins_node
        
        XML::saveXML(plugins_xml, file = paste0(plugins_dir, "/plugins.xml"))
        
        # Create a ZIP
        
        zip::zipr(file, paste0(temp_dir, "/plugins"))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - output$export_plugins_download"))
      }
    )
    
  })
}
