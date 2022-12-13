#' plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plugins_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card", "plugins_options_card", "import_plugin_card", "export_plugin_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card_new(ns = ns, name = card, i18n = i18n))
  })
  
  thesaurus_items_div <- ""
  if (id == "plugins_patient_lvl"){
    thesaurus_items_div <- div(
      make_combobox_new(i18n = i18n, ns = ns, label = "thesaurus", id = "thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 20),
        make_dropdown_new(i18n = i18n, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items",
          multiSelect = TRUE, width = "650px"),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), i18n$t("reset")), style = "margin-top:38px;")
      ),
      div(DT::DTOutput(ns("plugin_thesaurus_items")), class = "thesaurus_table"), br(),
      DT::DTOutput(ns("thesaurus_items"))
    )
  }

  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
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
                  list(key = "local", text = i18n$t("local_plugins")),
                  list(key = "github", text = i18n$t("github_plugins"))
                ), className = "inline_choicegroup")
              ), br(), 
              conditionalPanel(condition = "input.all_plugins_source == 'github'", ns = ns,
                uiOutput(ns("all_plugins_github"))
              ),
              conditionalPanel(condition = "input.all_plugins_source == 'local'", ns = ns,
                uiOutput(ns("all_plugins_local"))
              )
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
              make_textfield_new(i18n = i18n, ns = ns, label = "name", id = "plugin_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_plugin"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:absolute; z-index:1"
            ),
            div(DT::DTOutput(ns("plugins_datatable")), style = "margin-top:35px; z-index:2"),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_plugins_management"), i18n$t("save"))
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
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_combobox_new(i18n = i18n, ns = ns, label = "plugin", id = "code_chosen_plugin",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              div(
                div(class = "input_title", i18n$t("group_id")),
                div(shiny.fluent::SpinButton.shinyInput(ns("group_id"), min = 1, max = 100000000, step = 1), style = "width:300px;")
              )
            ),
            
            thesaurus_items_div, br(),
            
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
                list(key = "ui", text = i18n$t("ui")),
                list(key = "server", text = i18n$t("server"))
              ), className = "inline_choicegroup"),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
              div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            ),
            shinyjs::hidden(div(id = ns("div_br"), br())),
            
            conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
              div(shinyAce::aceEditor(ns("ace_edit_code_ui"), "", mode = "r", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
              div(shinyAce::aceEditor(ns("ace_edit_code_server"), "", mode = "r", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            
            shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(), br(),
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
              make_combobox_new(i18n = i18n, ns = ns, label = "plugin", id = "options_chosen_plugin",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              make_textfield_new(i18n = i18n, ns = ns, label = "author", id = "plugin_author", width = "300px"),
              make_textfield_new(i18n = i18n, ns = ns, label = "version", id = "plugin_version", width = "60px")
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
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                make_textfield_new(i18n = i18n, ns = ns, label = "image_url", id = "plugin_image_url", width = "700px"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("browse_image"), i18n$t("browse")), style = "margin-top:39px;"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("import_image"), i18n$t("import_image")), style = "margin-top:39px;"),
              )
            ), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(paste0(i18n$t("description"), " :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("plugin_description_language"), value = "fr", options = list(
                list(key = "fr", text = i18n$t("description_french")),
                list(key = "en", text = i18n$t("description_english"))
              ), className = "inline_choicegroup")
            ),
            conditionalPanel(condition = "input.plugin_description_language == 'fr'", ns = ns,
              div(shinyAce::aceEditor(ns("plugin_description_fr"), "", mode = "markdown", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.plugin_description_language == 'en'", ns = ns,
              div(shinyAce::aceEditor(ns("plugin_description_en"), "", mode = "markdown", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_plugin_options"), i18n$t("save"))
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
              make_toggle_new(i18n = i18n, ns = ns, label = "replace_already_existing_plugins", inline = TRUE)), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("import_plugins_browse"), i18n$t("choose_zip_file")),
              uiOutput(ns("import_plugins_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_plugins_button"), i18n$t("import_plugin"), iconProps = list(iconName = "Download")),
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
              make_dropdown_new(i18n = i18n, ns = ns, label = "plugins_to_export",
                multiSelect = TRUE, width = "650px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("export_plugins"), 
                i18n$t("export_plugins"), iconProps = list(iconName = "Upload")), style = "margin-top:38px;"),
              div(shiny.fluent::DefaultButton.shinyInput(ns("export_plugin_github"), 
                i18n$t("export_plugins_github"), iconProps = list(iconName = "Upload")), style = "margin-top:38px;"),
              div(style = "visibility:hidden;", downloadButton(ns("export_plugins_github_download"), label = "")),
              div(style = "visibility:hidden;", downloadButton(ns("export_plugins_download"), label = "")),
            ),
            DT::DTOutput(ns("plugins_to_export_datatable"))
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
mod_plugins_server <- function(id = character(), r = shiny::reactiveValues(), language = character(), i18n = R6::R6Class(), app_folder = character()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    col_types <- tibble::tribble(
      ~table, ~col_types,
      "plugins", "iccicl",
      "code", "icicicl"
    )
    
    # Prefix depending on page id
    if (id == "plugins_patient_lvl"){
      prefix <- "patient_lvl"
      module_type_id <- 1 
    }
    if (id == "plugins_aggregated"){
      prefix <- "aggregated"
      module_type_id <- 2
    }
    
    r[[paste0(prefix, "_plugins_datatable_loaded")]] <- FALSE
 
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card",
      "plugins_options_card", "import_plugin_card", "export_plugin_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("all_plugins_card" %in% r$user_accesses) shinyjs::show("all_plugins_card")
    else shinyjs::show("all_plugins_card_forbidden")
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$plugins, {
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options)
    })
    
    # --- --- --- --- -- -
    # Plugins catalog ----
    # --- --- --- --- -- -
    
    # Update plugins catalog
    
    observeEvent(r$plugins, {
      
      output$all_plugins_github <- renderUI(
        div(
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              shiny.fluent::DocumentCard(
                shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = "https://picsum.photos/318/196", width = 318, height = 200))),
                div(shiny.fluent::DocumentCardTitle(title = "Dygraph", shouldTruncate = TRUE), style = "margin-top:5px;"),
                div(shiny.fluent::DocumentCardActivity(activity = "2022-03-23", people = list(list(name = "Annie Lindqvist"))), style = "margin-top:-20px;"),
                onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-show_plugin_details', Math.random()); Shiny.setInputValue('", id, "-plugin_id', ", 13, "); }"))
              ),
              shiny.fluent::DocumentCard(
                shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = "https://picsum.photos/318/197", width = 318, height = 200))),
                div(shiny.fluent::DocumentCardTitle(title = "Flowchart", shouldTruncate = TRUE), style = "margin-top:5px;"),
                div(shiny.fluent::DocumentCardActivity(activity = "2022-01-22", people = list(list(name = "Boris Delange"))), style = "margin-top:-20px;")
              ),
              shiny.fluent::DocumentCard(
                shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = "https://picsum.photos/318/198", width = 318, height = 200))),
                div(shiny.fluent::DocumentCardTitle(title = "Datatable", shouldTruncate = TRUE), style = "margin-top:5px;"),
                div(shiny.fluent::DocumentCardActivity(activity = "2021-03-02", people = list(list(name = "John Doe"))), style = "margin-top:-20px;")
              )
            )
          ), br(),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              shiny.fluent::DocumentCard(
                shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = "https://picsum.photos/318/200", width = 318, height = 200))),
                shiny.fluent::DocumentCardTitle(title = "R code", shouldTruncate = TRUE),
                shiny.fluent::DocumentCardActivity(activity = "2020-03-23", people = list(list(name = "Annie Garden")))
              ),
              shiny.fluent::DocumentCard(
                shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = "https://picsum.photos/318/17", width = 318, height = 200))),
                shiny.fluent::DocumentCardTitle(title = "Features selection", shouldTruncate = TRUE),
                shiny.fluent::DocumentCardActivity(activity = "2022-01-22", people = list(list(name = "Boris Delange")))
              )
            )
          )
        )
      )
      
      plugins_temp <- r$plugins %>% dplyr::filter(name %in% c("Plugin avec options", "Plugin avec options 2", "abcd", "efgh", "ijkl", "mnop", "qrst"))
      
      if (nrow(plugins_temp %>% dplyr::filter(module_type_id == !!module_type_id, !deleted)) > 0){
        
        all_plugins_local <- tagList()
        all_plugins_local_document_cards <- tagList()
        i <- 0
        
        for(plugin_id in plugins_temp %>% dplyr::filter(module_type_id == !!module_type_id, !deleted) %>% dplyr::pull(id)){
          plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
          options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
          
          all_plugins_local_document_cards <- tagList(all_plugins_local_document_cards,
            shiny.fluent::DocumentCard(
              shiny.fluent::DocumentCardPreview(previewImages = list(list(previewImageSrc = paste0("https://picsum.photos/318/17", i), width = 318, height = 200))),
              div(shiny.fluent::DocumentCardTitle(title = plugin$name, shouldTruncate = TRUE, style = "margin-top:5px;")),
              div(shiny.fluent::DocumentCardActivity(
                activity = options %>% dplyr::filter(name == "version") %>% dplyr::pull(value),
                people = list(list(name = options %>% dplyr::filter(name == "author") %>% dplyr::pull(value)))),
                style = "margin-top:-20px;"
              ),
              onClick = htmlwidgets::JS(paste0("function() { ",
              "Shiny.setInputValue('", id, "-show_plugin_details', Math.random());",
              "Shiny.setInputValue('", id, "-plugin_id', ", plugin_id, ");",
              "}"))
            ),
          )
          
          i <- i + 1
          
          if (i %% 3 == 0){
            all_plugins_local <- tagList(all_plugins_local,
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                  all_plugins_local_document_cards
                )
              ), br()
            )
            
            all_plugins_local_document_cards <- tagList()
          }
        }
        
        if (i %% 3 != 0){ 
          all_plugins_local <- tagList(all_plugins_local,
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                all_plugins_local_document_cards
              )
            ), br()
          )
        }
      }
      
      if (nrow(plugins_temp %>% dplyr::filter(module_type_id == !!module_type_id, !deleted)) == 0){
        all_plugins_local <- shiny.fluent::MessageBar(i18n$t("no_plugin_available"), messageBarType = 0)
      }
      
      output$all_plugins_local <- renderUI(all_plugins_local)
    })
    
    observeEvent(input$show_plugin_details, {
      
      # Render markdown description
      
      dir <- paste0(app_folder, "/temp_files")
      
      markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", dir, "')\n",
        "knitr::opts_chunk$set(root.dir = '", dir, "', fig.path = '", dir, "')\n```\n")
      
      plugin <- r$plugins %>% dplyr::filter(id == input$plugin_id)
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == input$plugin_id)
      
      plugin_description <- paste0(
        "**Auteur** : ", options %>% dplyr::filter(name == "author") %>% dplyr::pull(value), "<br />",
        "**Version** : ", options %>% dplyr::filter(name == "version") %>% dplyr::pull(value), "\n\n",
        options %>% dplyr::filter(name == paste0("description_", tolower(language))) %>% dplyr::pull(value)
      )
      
      markdown_file <- paste0(markdown_settings, plugin_description)
      
      # Create temp dir
      file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      if (!dir.exists(dir)) dir.create(dir)
      
      # Create the markdown file
      knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      
      # If this is a local plugin
      
      if (input$all_plugins_source == "local"){
        
        output$all_plugins_plugin_details_title <- renderUI(
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
            div(shiny.fluent::ActionButton.shinyInput(ns("all_plugins_show_document_cards"), "", iconProps = list(iconName = "Back")), style = "position:relative; bottom:6px;"), 
            plugin$name
          )
        )
        
        output$all_plugins_plugin_details_content <- renderUI(
          div(
            div(class = "markdown", tagList(withMathJax(includeMarkdown(file)))),
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
          )
        )
      }
        
      # If this is a remote plugin
      
      if (input$all_plugins_source == "github"){
        
        # output$all_plugins_plugin_details_ui <- renderUI(
        #   make_card(
        #     tagList(
        #       shiny.fluent::ActionButton.shinyInput(ns("all_plugins_show_document_cards"), "", iconProps = list(iconName = "Back")), 
        #       plugin$name),
        #     div(br(),
        #       div(
        #         div(class = "markdown", withMathJax(includeMarkdown(file))), 
        #         style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
        #       ), br(),
        #       shiny.fluent::PrimaryButton.shinyInput(ns("update_plugin"), i18n$t("update_plugin"))
        #     )
        #   )
        # )
      }
        
      shinyjs::hide("all_plugins_document_cards")
      shinyjs::show("all_plugins_plugin_details")

    })
    
    observeEvent(input$all_plugins_show_document_cards, {
      shinyjs::show("all_plugins_document_cards")
      shinyjs::hide("all_plugins_plugin_details")
    })
    
    # --- --- --- --- -- -
    # Create a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_plugin, {
      
      if ("plugins_creation_card" %in% r$user_accesses){
        
        new_data <- list()
        new_data$name <- coalesce2(type = "char", x = input$plugin_name)
        new_data$plugin_name <- new_data$name
        new_data$module_type <- module_type_id
        
        add_settings_new_data_new(session = session, output = output, r = r, i18n = i18n, id = "settings_plugins", 
          data = new_data, table = "plugins", required_textfields = "plugin_name", req_unique_values = "name")
      }
      
      else {
        show_message_bar_new(output, 2, "unauthorized_action", "severeWarning", i18n = i18n)
      }
    })
    
    # --- --- --- --- --- ---
    # Plugins management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each module / page
    action_buttons_plugins_management <- c("delete", "edit_code", "options")
    action_buttons_export_plugins <- "add"
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
    centered_cols <- c("id", "datetime", "action")
    searchable_cols <- c("name")
    hidden_cols <- c("id", "description", "module_type_id", "deleted", "modified")
    col_names <- get_col_names_new("plugins", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$plugins, {
      
      if (nrow(r$plugins) == 0){
        r[[paste0(prefix, "_plugins_temp")]] <- tibble::tibble()
        r[[paste0(prefix, "_plugins_datatable_loaded")]] <- FALSE 
      }
      else r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reload group_id spinButton
      if (length(input$group_id) == 0) shiny.fluent::updateSpinButton.shinyInput(session,
        "group_id", value = get_last_row(r$db, paste0(prefix, "_modules_elements")) + 10000000)
    })
    
    observeEvent(r[[paste0(prefix, "_plugins_temp")]], {
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]]) == 0) render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(),
        output_name = "plugins_datatable", col_names =  get_col_names_new(table_name = "plugins", i18n = i18n),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
      
      req(nrow(r[[paste0(prefix, "_plugins_temp")]]) > 0)

      r[[paste0(prefix, "_export_plugins_temp")]] <- r[[paste0(prefix, "_plugins_temp")]]
      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::slice(0)

      # Prepare data for datatables

      r[[paste0(prefix, "_plugins_datatable_temp")]] <- prepare_data_datatable_new(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "plugins", action_buttons = action_buttons_plugins_management, data_input = r[[paste0(prefix, "_plugins_temp")]])

      r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable_new(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "plugins", action_buttons = action_buttons_export_plugins, data_input = r[[paste0(prefix, "_export_plugins_temp")]])

      # Render datatables
      
      if (!r[[paste0(prefix, "_plugins_datatable_loaded")]]){

        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0(prefix, "_plugins_datatable_temp")]],
          output_name = "plugins_datatable", col_names =  get_col_names_new(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
  
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0(prefix, "_export_plugins_datatable_temp")]],
          output_name = "plugins_to_export_datatable", col_names =  get_col_names_new(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
  
        # Create a proxy for datatable
  
        r[[paste0(prefix, "_plugins_datatable_proxy")]] <- DT::dataTableProxy("plugins_datatable", deferUntilFlush = FALSE)
        r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]] <- DT::dataTableProxy("plugins_to_export_datatable", deferUntilFlush = FALSE)

        # Indicate that datatable has been loaded
        r[[paste0(prefix, "_plugins_datatable_loaded")]] <- TRUE
      }
      
      else {
        # Reload data of datatable
          if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) > 0){
            DT::replaceData(r[[paste0(prefix, "_plugins_datatable_proxy")]], r[[paste0(prefix, "_plugins_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
            DT::replaceData(r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]], r[[paste0(prefix, "_export_plugins_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          }
      }
    })
    
    observeEvent(r[[paste0(prefix, "_export_plugins_temp")]], {
      
      # Reload datatable_temp variable
      r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable_new(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "plugins", action_buttons = action_buttons_export_plugins, 
        data_input = r[[paste0(prefix, "_export_plugins_temp")]])
      
      # Reload data of datatable
      if (length(r[[paste0(prefix, "_export_plugins_datatable_proxy")]]) > 0) DT::replaceData(r[[paste0(prefix, "_export_plugins_datatable_proxy")]], 
        r[[paste0(prefix, "_export_plugins_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$plugins_datatable_cell_edit, {
      
      edit_info <- input$plugins_datatable_cell_edit
      r[[paste0(prefix, "_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_plugins_temp")]], edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r[[paste0(prefix, "_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    observeEvent(input$plugins_to_export_datatable_cell_edit, {
      
      edit_info <- input$plugins_to_export_datatable_cell_edit
      r[[paste0(prefix, "_export_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_export_plugins_temp")]], edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r[[paste0(prefix, "_export_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_plugins_management, {
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) == 0) show_message_bar_new(output, 2, "modif_saved", "success", i18n = i18n)
      
      req(nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) > 0)
      
      save_settings_datatable_updates_new(output = output, r = r, ns = ns, 
        table = "plugins", r_table = paste0(prefix, "_plugins"), i18n = i18n, duplicates_allowed = FALSE)
      
    })
    
    # Delete a row in datatable

    plugin_delete_prefix <- paste0(prefix, "_plugin")
    plugin_dialog_title <- "plugins_delete"
    plugin_dialog_subtext <- "plugins_delete_subtext"
    plugin_react_variable <- "plugin_delete_confirm"
    plugin_table <- "plugins"
    plugin_id_var_sql <- "id"
    plugin_id_var_r <- "delete_plugin"
    plugin_delete_message <- "plugin_deleted"
    plugin_reload_variable <- "reload_plugins"
    plugin_information_variable <- "plugin_deleted"
    plugin_delete_variable <- paste0(plugin_delete_prefix, "_open_dialog")

    delete_element_new(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = plugin_delete_prefix, dialog_title = plugin_dialog_title, dialog_subtext = plugin_dialog_subtext,
      react_variable = plugin_react_variable, table = plugin_table, id_var_sql = plugin_id_var_sql, id_var_r = plugin_id_var_r,
      delete_message = plugin_delete_message, translation = TRUE, reload_variable = plugin_reload_variable,
      information_variable = plugin_information_variable)

    observeEvent(input$deleted_pressed, {

      r$delete_plugin <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[plugin_delete_variable]] <- TRUE

    })

    observeEvent(r$reload_plugins, {
      
      # Reload sidenav dropdown with reloading studies
      update_r(r = r, table = "plugins")

      # Reload datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)  %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(input$edit_code, {
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('edit_plugin_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('plugin_options')}\"]').click();"))
    })
    
    # --- --- --- -- -- -
    # Plugin options ----
    # --- --- --- -- -- -
    
    observeEvent(input$options_chosen_plugin, {

      if (length(input$options_chosen_plugin) > 1) link_id <- input$options_chosen_plugin$key
      else link_id <- input$options_chosen_plugin
      if (length(input$code_chosen_plugin) > 0){
        if (length(input$code_chosen_plugin) > 1) code_link_id <- input$code_chosen_plugin$key
        else code_link_id <- input$code_chosen_plugin
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id){
        options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options, value = value)
      }
      
      # Plugin options
      
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
      
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
        make_people_picker_new(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Plugin version, author, image and descriptions
      for (field in c("version", "author", "image")) shiny.fluent::updateTextField.shinyInput(session, 
        paste0("plugin_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
      for (field in c("description_fr", "description_en")) shinyAce::updateAceEditor(session,
        paste0("plugin_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
    })
    
    # Save updates
    
    observeEvent(input$save_plugin_options, {
      
      if (length(input$options_chosen_plugin) > 1) link_id <- input$options_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
      
      data <- list()
      data$users_allowed_read <- unique(input$users_allowed_read)
      for (field in c("users_allowed_read_group", "plugin_version", "plugin_author", 
        "plugin_image", "plugin_description_fr", "plugin_description_en")) data[[stringr::str_replace(field, "plugin_", "")]] <- input[[field]]
      
      save_settings_options_new(output = output, r = r, id = id, category = "plugin", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("users_allowed_read", "version", "author", "image", "description_fr", "description_en"))
      
    })
    
    # --- --- --- --- --- -
    # Edit plugin code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_chosen_plugin, {
      
      if (length(input$code_chosen_plugin) > 1) link_id <- input$code_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
      if (length(input$options_chosen_plugin) > 0){
        if (length(input$options_chosen_plugin) > 1) options_link_id <- input$options_chosen_plugin$key
        else options_link_id <- input$options_chosen_plugin
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options, value = value)
      }

      # Get code from database
      code <- list()
      code$ui <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
      code$server <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code)
      
      shinyAce::updateAceEditor(session, "ace_edit_code_ui", value = code$ui)
      shinyAce::updateAceEditor(session, "ace_edit_code_server", value = code$server)
      
      # Render UI of this edit_code card
      output$edit_code_card <- renderUI({
        render_settings_code_card_new(ns = ns, r = r, id = id, title = paste0("edit_plugins_code"), code = code, link_id = link_id, i18n = i18n)
      })

      # Reset code_result textOutput
      output$code_result_ui <- renderUI("")
      output$code_result_server <- renderText("")
      
    })
    
    # Load thesaurus items
    
    if (prefix == "patient_lvl"){
      
      # --- --- --- --- --- --- -
      ## Thesaurus datatable ----
      # --- --- --- --- --- --- -
      
      # Load thesaurus attached to this datamart
      observeEvent(r$chosen_datamart, {
        
        req(!is.na(r$chosen_datamart))
        
        data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id) %>% as.character()
        
        # Multiple cases
        # Only one ID, so it's the beginning and the end
        # Last ID, so it's the end
        # ID between begin and last, so separated by commas
        thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
          grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
        shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list_new(data = thesaurus, key_col = "id", text_col = "name", i18n = i18n), value = NULL)
        
      })
      
      # Load thesaurus items
      observeEvent(input$thesaurus, {
        
        r$plugin_thesaurus_items <- create_datatable_cache_new(output = output, r = r, i18 = i18, module_id = id, thesaurus_id = input$thesaurus$key, category = "plus_plugin")
        
        colour_col <- create_datatable_cache_new(output = output, r = r, i18n = i18n, module_id = id, thesaurus_id = input$thesaurus$key, category = "colours_plugin")
        
        if (nrow(colour_col) > 0) r$plugin_thesaurus_items <- r$plugin_thesaurus_items %>%
          dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
        
        count_items_rows <- tibble::tibble()
        count_patients_rows <- tibble::tibble()
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_items_rows <- create_datatable_cache_new(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
          datamart_id = r$chosen_datamart, category = "count_items_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("plugins - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n))
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_patients_rows <- create_datatable_cache_new(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
          datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug_new(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("plugins - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n))
        
        if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar_new(output, 1, "fail_load_datamart", "severeWarning", i18n = i18n)
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
        searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
        factorize_cols <- c("category", "unit")
        column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")
        sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
        centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        col_names <- get_col_names_new(table_name = "modules_thesaurus_items_with_counts", i18n = i18n)
        hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
        
        # Render datatable
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$plugin_thesaurus_items_temp,
          output_name = "plugin_thesaurus_items", col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        # Create a proxy for datatatable
        r$plugin_thesaurus_items_proxy <- DT::dataTableProxy("plugin_thesaurus_items", deferUntilFlush = FALSE)
        
      })
      
      # Reload datatable
      
      observeEvent(r$plugin_thesaurus_items_temp, {
        
        # Reload data of datatable
        DT::replaceData(r$plugin_thesaurus_items_proxy, r$plugin_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
      })
      
      # Updates in datatable
      
      observeEvent(input$plugin_thesaurus_items_cell_edit, {
        
        edit_info <- input$plugin_thesaurus_items_cell_edit
        
        r$plugin_thesaurus_items_temp <- DT::editData(r$plugin_thesaurus_items_temp, edit_info, rownames = FALSE)
        r$plugin_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      })
      
      # --- --- --- --- -- -
      # Thesaurus items ----
      # --- --- --- --- -- -
      
      # When add button is clicked
      observeEvent(input$item_selected, {
        
        # Initiate r variable if doesn't exist
        if (length(r$plugin_thesaurus_selected_items) == 0){
          r$plugin_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
            thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
            thesaurus_item_colour = character(), input_text = character()) 
        }
        
        # Get ID of chosen thesaurus item
        link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))
        
        # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown
        
        value <- integer(1)
        if (nrow(r$plugin_thesaurus_selected_items) > 0) value <- r$plugin_thesaurus_selected_items %>% 
          dplyr::filter(thesaurus_id == input$thesaurus$key) %>% dplyr::pull(id)
        
        if (link_id %not_in% value){
          
          # Get thesaurus name
          thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus$key) %>% dplyr::pull(name)
          
          # Get item informations from datatable / r$plugin_thesaurus_items
          # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
          item <- r$plugin_thesaurus_items_temp %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", name))
          
          # display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)
          
          # Add item to selected items
          r$plugin_thesaurus_selected_items <-
            tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text,
              as.integer(link_id), as.integer(input$thesaurus$key), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
              as.character(item$unit), as.character(input[[paste0("colour_", link_id)]]), as.character(item$input_text)) %>%
            dplyr::bind_rows(r$plugin_thesaurus_selected_items)
          
          # Update dropdown of selected items
          options <- convert_tibble_to_list_new(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", i18n = i18n)
          value <- r$plugin_thesaurus_selected_items %>% dplyr::pull(id)
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        }
        
      })
      
      # When reset button is clicked
      observeEvent(input$reset_thesaurus_items, {
        # Reset r$plugin_thesaurus_selected_items
        r$plugin_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
          thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
          thesaurus_item_colour = character(), input_text = character()) 
        
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
      
      # When dropdown is modified
      observeEvent(input$thesaurus_selected_items, {
        
        r$plugin_thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
          dplyr::filter(id %in% input$thesaurus_selected_items)
        options <- convert_tibble_to_list_new(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", i18n = i18n)
        value <- r$plugin_thesaurus_selected_items %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
    }
    
    # Execute code
    
    observeEvent(input$execute_code, {
      
      var_check <- TRUE
      if (length(r$chosen_datamart) == 0) var_check <- FALSE
      if (length(r$chosen_datamart) > 0){
        if (is.na(r$chosen_datamart) | is.na(r$chosen_study)) var_check <- FALSE
        if (prefix == "patient_lvl" & is.na(r$chosen_patient)) var_check <- FALSE
      }
      
      if (!var_check) show_message_bar_new(output = output, id = 3, message = "load_some_patient_data", i18n = i18n)
      
      req(var_check)
      
      # Get thesaurus items
      
      if (prefix == "patient_lvl"){
        
        # Check if some thesaurus items selected (not necessary)
        if (length(r$plugin_thesaurus_selected_items) > 0){
          
          # Get thesaurus items with thesaurus own item_id
          thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
            dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
              thesaurus_item_unit, colour = thesaurus_item_colour)
        }
      }
      
      if (length(input$code_chosen_plugin) > 1) link_id <- input$code_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
    
      ui_code <- input$ace_edit_code_ui
      server_code <- input$ace_edit_code_server
      
      # Replace %group_id% in ui_code with 1 for our example
      
      group_id <- get_last_row(r$db, paste0(prefix, "_modules_elements")) + 10000000
      if (length(input$group_id) > 0) group_id <- input$group_id
      
      ui_code <- ui_code %>% 
        stringr::str_replace_all("%module_id%", "1") %>%
        stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
        stringr::str_replace_all("%study_id%", as.character(r$chosen_study)) %>%
        stringr::str_replace_all("\r", "\n")
      
      if (prefix == "patient_lvl") ui_code <- ui_code %>% stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))
      
      server_code <- server_code %>% 
        stringr::str_replace_all("%module_id%", "1") %>%
        stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
        stringr::str_replace_all("%study_id%", as.character(r$chosen_study)) %>%
        stringr::str_replace_all("\r", "\n")
      
      if (prefix == "patient_lvl") server_code <- server_code %>% stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))
      
      output$code_result_ui <- renderUI(make_card("", tryCatch(result <- eval(parse(text = ui_code)), error = function(e) stop(e), warning = function(w) stop(w))))
      
      output$code_result_server <- renderText({
        
        options('cli.num_colors' = 1)
        
        # Capture console output of our code
        captured_output <- capture.output(
          tryCatch(eval(parse(text = server_code)), error = function(e) print(e), warning = function(w) print(w)))
        
        # Restore normal value
        options('cli.num_colors' = NULL)
        
        # Display result
        paste(strwrap(captured_output), collapse = "\n") -> result
      })
    })
    
    # Save updates
    
    observeEvent(input$save_code, {
      
      if (length(input$code_chosen_plugin) > 1) link_id <- input$code_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
    
      req(!is.null(link_id))
      
      # Update code
      # DON'T USE glue_sql, it adds some quotes in the code
      
      ui_code_id <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(id)
      server_code_id <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(id)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code_ui, "'", "''"), "' WHERE id = ", ui_code_id)) -> query
      DBI::dbClearResult(query)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code_server, "'", "''"), "' WHERE id = ", server_code_id)) -> query
      DBI::dbClearResult(query)
      
      # Update datetime in plugins table
      
      sql <- glue::glue_sql("UPDATE plugins SET datetime = {as.character(Sys.time())} WHERE id = {link_id}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      update_r(r = r, table = "code")
      update_r(r = r, table = "plugins")
      
      # Notify user
      show_message_bar_new(output, 4, "modif_saved", "success", i18n = i18n)
      
    })
    
    # Hide ace editor
    
    observeEvent(input$hide_editor, {
      if (input$hide_editor){
        sapply(c("ace_edit_code_ui", "ace_edit_code_server"), shinyjs::hide)
        shinyjs::show("div_br") 
      }
      else {
        sapply(c("ace_edit_code_ui", "ace_edit_code_server"), shinyjs::show)
        shinyjs::hide("div_br") 
      }
    })
    
    # --- --- --- --- -- -
    # Import a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$import_plugins_browse, shinyjs::click("import_plugins_upload"))
    
    output$import_plugins_status <- renderUI(tagList(div(
      span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"), 
      span(input$import_plugins_upload$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;")))
    
    observeEvent(input$import_plugins_button, {
      
      req(input$import_plugins_upload)
      
      tryCatch({
        
        exdir <- paste0(find.package("cdwtools"), "/data/temp/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"))
        dir.create(paste0(find.package("cdwtools"), "/data/"), showWarnings = FALSE)
        dir.create(paste0(find.package("cdwtools"), "/data/temp/"), showWarnings = FALSE)
        dir.create(exdir)

        zip::unzip(input$import_plugins_upload$datapath, exdir = exdir)
        csv_files <- zip::zip_list(input$import_plugins_upload$datapath)
        
        data <- list()

        lapply(csv_files$filename, function(file_name){

          # Name of the table
          table <- substr(file_name, 1, nchar(file_name) - 4)

          # Load CSV file
          col_types_temp <- col_types %>% dplyr::filter(table == !!table) %>% dplyr::pull(col_types)
          data[[table]] <<- readr::read_csv(paste0(exdir, "/", file_name), col_types = col_types_temp)
          
        })
        
        # Add plugins that do not already exist
        
        sql <- glue::glue_sql("SELECT * FROM plugins WHERE deleted IS FALSE", .con = r$db)
        all_plugins <- DBI::dbGetQuery(r$db, sql)
        
        new_plugins <- data$plugins %>% dplyr::anti_join(all_plugins %>% dplyr::select(name, module_type_id), by = c("name", "module_type_id"))
        new_plugins_code <- data$code %>% dplyr::inner_join(new_plugins %>% dplyr::select(link_id = id), by = "link_id")
        
        if (nrow(new_plugins) > 0){
          
          # Add new plugins & code
          new_plugins <- new_plugins %>% 
            dplyr::mutate(id = id + get_last_row(r$db, "plugins"), datetime = as.character(Sys.time()))
          new_plugins_code <- new_plugins_code %>% 
            dplyr::mutate(id = id + get_last_row(r$db, "code"), link_id = link_id + get_last_row(r$db, "plugins"),
              creator_id = r$user_id, datetime = as.character(Sys.time()))
          
          last_row_options <- get_last_row(r$db, "options")
          
          new_plugins_options <- new_plugins %>% dplyr::select(link_id = id) %>% 
            dplyr::mutate(id = 1:dplyr::n() + last_row_options, category = "plugin", name = "users_allowed_read_group", value = "everybody", value_num = 1, 
                          creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE) %>% 
            dplyr::relocate(link_id, .after = "category")
          
          last_row_options <- last_row_options + nrow(new_plugins_options)
          
          new_plugins_options <- new_plugins_options %>% 
            dplyr::bind_rows(
              new_plugins %>% dplyr::select(link_id = id) %>% 
                dplyr::mutate(id = 1:dplyr::n() + last_row_options, category = "plugin", name = "user_allowed_read", value = "", value_num = r$user_id, 
                  creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE) %>% 
                dplyr::relocate(link_id, .after = "category")
            )
          
          DBI::dbAppendTable(r$db, "plugins", new_plugins)
          DBI::dbAppendTable(r$db, "code", new_plugins_code)
          DBI::dbAppendTable(r$db, "options", new_plugins_options)
        }
        
        existing_plugins <- data$plugins %>% 
          dplyr::inner_join(all_plugins %>% dplyr::select(new_id = id, name, module_type_id), by = c("name", "module_type_id"))
        
        if (nrow(existing_plugins) > 0){
          
          existing_plugins_code <- data$code %>% 
            dplyr::inner_join(existing_plugins %>% dplyr::select(link_id = id, new_link_id = new_id), by = "link_id") %>%
            dplyr::select(-link_id) %>% dplyr::rename(link_id = new_link_id) %>% dplyr::relocate(link_id, .after = "category") %>%
            dplyr::mutate(creator_id = r$user_id, datetime = as.character(Sys.time()))
          
          existing_plugins <- existing_plugins %>%
            dplyr::select(-id) %>% dplyr::rename("id" = new_id) %>% dplyr::relocate(id) %>% 
            dplyr::mutate(datetime = as.character(Sys.time()))
          
          # Replace already existing plugins (based on module type & name)
          if (input$replace_already_existing_plugins){
            
            existing_plugins_code <- existing_plugins_code %>% dplyr::mutate(id = id + get_last_row(r$db, "code"))
            
            # Delete old rows
            sql <- glue::glue_sql("DELETE FROM plugins WHERE id IN ({existing_plugins %>% dplyr::pull(id)*})", .con = r$db)
            DBI::dbSendStatement(r$db, sql) -> query
            DBI::dbClearResult(query)
            
            sql <- glue::glue_sql("DELETE FROM code WHERE category IN ('plugin_ui', 'plugin_server') AND link_id IN ({existing_plugins %>% dplyr::pull(id)*})", .con = r$db)
            DBI::dbSendStatement(r$db, sql) -> query
            DBI::dbClearResult(query)
            
            # Insert new ones
            DBI::dbAppendTable(r$db, "plugins", existing_plugins)
            DBI::dbAppendTable(r$db, "code", existing_plugins_code)
          }
        }

        # Remove temp dir
        unlink(paste0(find.package("cdwtools"), "/data/temp"), recursive = TRUE, force = TRUE)

        # Load database, restored
        load_database_new(r = r, i18n = i18n)

        show_message_bar_new(output, 3, "plugin_imported", "success", i18n = i18n, time = 15000)
      },
      error = function(e) report_bug_new(r = r, output = output, error_message = "error_importing_plugins",
        error_name = paste0(id, " - import plugins"), category = "Error", error_report = e, i18n = i18n))
    })
    
    # --- --- --- --- -- -
    # Export a plugin ----
    # --- --- --- --- -- -
    
    # When add button is clicked
    observeEvent(input$add_item, {
      
      # Get ID of chosen plugin
      link_id <- as.integer(substr(input$add_item, nchar("add_item_") + 1, nchar(input$add_item)))

      # If this plugin is not already chosen, add it to the selected items dropdown
      
      value <- integer(1)
      if (nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0) value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
      
      if (link_id %not_in% value){
        
        r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::filter(id == link_id) %>%
          dplyr::bind_rows(r[[paste0(prefix, "_export_plugins_selected")]])
        
        # Update dropdown of selected items
        options <- convert_tibble_to_list_new(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
        value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
    })
    
    # When dropdown is modified
    observeEvent(input$plugins_to_export, {

      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_selected")]] %>%
        dplyr::filter(id %in% input$plugins_to_export)

      options <- convert_tibble_to_list_new(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
      value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # Export plugins
    observeEvent(input$export_plugins, {
      
      req(nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0)
      
      shinyjs::click("export_plugins_download")
    })
    
    output$export_plugins_download <- downloadHandler(
      
      filename = function() paste0("linkr_plugins_", as.character(stringr::str_replace(Sys.time(), " ", "_")), ".zip"),
      
      content = function(file){
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        temp_dir <- paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = '')
        dir.create(paste0(app_folder, "/temp_files/", temp_dir, "/plugins"))
        
        for (plugin_id in r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)){
          
          plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
          options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
          code <- r$code %>% dplyr::filter(link_id == plugin_id, category %in% c("plugin_ui", "plugin_server"))
          
          # Create folder if doesn't exist
          plugin_dir <- paste0(app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)
          
          # Create ui.R & server.R
          sapply(c("ui", "server"), function(name) writeLines(code %>% dplyr::filter(category == paste0("plugin_", name)) %>% 
              dplyr::pull(code), paste0(plugin_dir, "/", name, ".R")))
          
          # Create XML file
          xml <- XML::newXMLDoc()
          plugins_node <- XML::newXMLNode("plugins", doc = xml)
          plugin_node <- XML::newXMLNode("plugin", parent = plugins_node, doc = xml)
          XML::newXMLNode("type", module_type_id, parent = plugin_node)
          XML::newXMLNode("name", plugin$name, parent = plugin_node)
          sapply(c("version", "unique_id", "author", "image", "description_fr", "description_en"), function(name){
            XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = plugin_node)
          })
          XML::saveXML(xml, file = paste0(plugin_dir, "/plugin.xml"))
          
          # Copy files to temp dir
          temp_dir_copy <- paste0(app_folder, "/temp_files/", temp_dir, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          dir.create(temp_dir_copy)
          file.copy(
            c(paste0(plugin_dir, "/ui.R"), paste0(plugin_dir, "/server.R"), paste0(plugin_dir, "/plugin.xml")), 
            c(paste0(temp_dir_copy, "/ui.R"), paste0(temp_dir_copy, "/server.R"), paste0(temp_dir_copy, "/plugin.xml"))
          )
          
          # Add to files var
          # files <- c(files, plugin_dir)
        }
        
        # # plugins.csv
        # plugins <- 
        #   r[[paste0(prefix, "_export_plugins_selected")]] %>% 
        #   dplyr::select(-modified) %>%
        #   dplyr::mutate(new_id = 1:dplyr::n())
        # 
        # # code.csv
        # code <-
        #   r$code %>%
        #   dplyr::inner_join(plugins %>% dplyr::select(link_id = id, new_id), by = "link_id") %>%
        #   dplyr::filter(category %in% c("plugin_ui", "plugin_server")) %>%
        #   dplyr::select(-link_id) %>% dplyr::rename(link_id = new_id) %>% dplyr::relocate(link_id, .after = "category") %>%
        #   dplyr::mutate(new_id = 1:dplyr::n()) %>%
        #   dplyr::select(-id) %>% dplyr::rename(id = new_id) %>% dplyr::relocate(id)
        # 
        # plugins <- plugins %>% dplyr::select(-id) %>% dplyr::rename(id = new_id) %>% dplyr::relocate(id)
        # 
        # readr::write_csv(plugins, "plugins.csv")
        # readr::write_csv(code, "code.csv")
        # 
        # files <- c("plugins.csv", "code.csv")
        # 
        # print(files)
        zip::zipr(file, paste0(app_folder, "/temp_files/", temp_dir))
        # zip::zipr(file, paste0(app_folder, "/plugins/", prefix))
        # zip::zipr(file, files, recurse = TRUE, include_directories = TRUE, mode = "mirror")
      }
    )
    
  })
}
