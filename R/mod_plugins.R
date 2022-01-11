#' plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plugins_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  thesaurus_items_div <- ""
  if (id == "plugins_patient_lvl"){
    thesaurus_items_div <- div(
      make_combobox(language = language, ns = ns, label = "thesaurus", id = "thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px", words = words),
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 20),
        make_dropdown(language = language, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items",
          multiSelect = TRUE, width = "650px", words = words),
        div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), translate(language, "reset", words)), style = "margin-top:38px;")
      ),
      div(DT::DTOutput(ns("plugin_thesaurus_items")), class = "thesaurus_table"), br(),
      DT::DTOutput(ns("thesaurus_items"))
    )
  }

  div(class = "main",
      
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = translate(language, id, words))
    ), maxDisplayedItems = 3),
    # uiOutput(ns("plugins_pivot")),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "all_plugins_card", itemKey = "all_plugins", headerText = translate(language, "all_plugins", words)),
      shiny.fluent::PivotItem(id = "create_plugin_card", itemKey = "create_plugin", headerText = translate(language, "create_plugin", words)),
      shiny.fluent::PivotItem(id = "plugins_management_card", itemKey = "plugins_management", headerText = translate(language, "plugins_management", words)),
      shiny.fluent::PivotItem(id = "edit_plugin_code_card", itemKey = "edit_plugin_code", headerText = translate(language, "edit_plugin_code", words)),
      shiny.fluent::PivotItem(id = "plugin_options_card", itemKey = "plugin_options", headerText = translate(language, "plugin_options", words)),
      shiny.fluent::PivotItem(id = "import_plugin_card", itemKey = "import_plugin", headerText = translate(language, "import_plugin", words)),
      shiny.fluent::PivotItem(id = "export_plugin_card", itemKey = "export_plugin", headerText = translate(language, "export_plugin", words))
    ),
    div(
      id = ns("all_plugins_card"),
      make_card(translate(language, "all_plugins", words),
        div(
          br(),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
            shiny.fluent::DocumentCard(
              shiny.fluent::DocumentCardPreview(previewImages = list(
                list(
                  previewImageSrc = "https://cdn.thenewstack.io/media/2017/12/f67e69da-screen-shot-2017-12-28-at-4.17.36-pm.png",
                  width = 318,
                  height = 196
                ))
              ),
              shiny.fluent::DocumentCardTitle(
                title = "Dygraph",
                shouldTruncate = TRUE
              )#,
              # shiny.fluent::DocumentCardActivity(
              #   activity = "2020-05-21",
              #   people = list(list(name = "John Doe"))
              # )
            ),
            shiny.fluent::DocumentCard(
              shiny.fluent::DocumentCardPreview(previewImages = list(
                list(
                  previewImageSrc = "https://cran.r-project.org/web/packages/vistime/readme/man/figures/ward_movements.png",
                  width = 318,
                  height = 196
                ))
              ),
              shiny.fluent::DocumentCardTitle(
                title = "Vistime",
                shouldTruncate = TRUE
              ),
              shiny.fluent::DocumentCardActivity(
                activity = "2021-12-12",
                people = list(list(name = "Boris Delange"))
              )
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("create_plugin_card"),
        make_card(translate(language, "create_plugin", words),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield(language = language, ns = ns, label = "name", id = "plugin_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_plugin"), translate(language, "add", words))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("plugins_management_card"),
        make_card(translate(language, "plugins_management", words),
          div(
            DT::DTOutput(ns("plugins_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_plugins_management"), translate(language, "save", words))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("edit_plugin_code_card"),
        make_card(translate(language, "edit_plugin_code", words),
          div(
            make_combobox(language = language, ns = ns, label = "plugin", id = "code_chosen_plugin",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE),
            
            thesaurus_items_div, br(),
            
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
                list(key = "ui", text = translate(language, "ui", words)),
                list(key = "server", text = translate(language, "server", words))
              ), className = "inline_choicegroup"),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
              div(translate(language, "hide_editor", words), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            ),
            shinyjs::hidden(div(id = ns("div_br"), br())),
            
            conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
              div(shinyAce::aceEditor(ns("ace_edit_code_ui"), "", mode = "r", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
              div(shinyAce::aceEditor(ns("ace_edit_code_server"), "", mode = "r", 
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            
            shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), translate(language, "save", words)), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), translate(language, "execute_code", words)), br(), br(),
            shiny::uiOutput(ns("code_result_ui")), br(),
            div(verbatimTextOutput(ns("code_result_server")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("plugin_options_card"),
        make_card(translate(language, "plugin_options", words),
          div(
            make_combobox(language = language, ns = ns, label = "plugin", id = "options_chosen_plugin",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE)
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_plugin_card"),
        make_card(translate(language, "import_plugin", words),
          div("...")
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("export_plugin_card"),
        make_card(translate(language, "export_plugin", words),
          div("...")
        ), br()
      )
    )
  ) -> result
  
  result
}
    
#' plugins Server Functions
#'
#' @noRd 
mod_plugins_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "plugins_patient_lvl"){
      prefix <- "patient_lvl"
      module_type_id <- 1 
    }
    if (id == "plugins_aggregated"){
      prefix <- "aggregated"
      module_type_id <- 2
    }
    
    ##########################################
    # Pivot menu                             #
    ##########################################
    
    r$plugins_selected_pivot <- "all_plugins_card"
    
    # observeEvent(r$end_load_modules, {
    #   r$plugins_selected_pivot <- "create_plugin_card"
    # })
    
    observeEvent(input$current_tab, r$plugins_selected_pivot <- input$current_tab)
    
    # observeEvent(r$plugins_selected_pivot, {
      
    # output$plugins_pivot <- renderUI({
    #   shiny.fluent::Pivot(
    #     selectedKey = r$plugins_selected_pivot,
    #     onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
    #     shiny.fluent::PivotItem(id = "all_plugins_card", itemKey = "all_plugins_card", headerText = translate(language, "all_plugins", words)),
    #     shiny.fluent::PivotItem(id = "create_plugin_card", itemKey = "create_plugin_card", headerText = translate(language, "create_plugin", words)),
    #     shiny.fluent::PivotItem(id = "plugins_management_card", itemKey = "plugins_management_card", headerText = translate(language, "plugins_management", words)),
    #     shiny.fluent::PivotItem(id = "edit_plugin_code_card", itemKey = "edit_plugin_code_card", headerText = translate(language, "edit_plugin_code", words)),
    #     shiny.fluent::PivotItem(id = "plugin_options_card", itemKey = "plugin_options_card", headerText = translate(language, "plugin_options", words)),
    #     shiny.fluent::PivotItem(id = "import_plugin_card", itemKey = "import_plugin_card", headerText = translate(language, "import_plugin", words)),
    #     shiny.fluent::PivotItem(id = "export_plugin_card", itemKey = "export_plugin_card", headerText = translate(language, "export_plugin", words))
    #   )
    # })
    # })
 
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("all_plugins_card", "create_plugin_card", "plugins_management_card", "edit_plugin_code_card",
      "plugin_options_card", "import_plugin_card", "export_plugin_card")
    
    # observeEvent(r$plugins_selected_pivot, {
    # 
    #   sapply(cards %>% setdiff(., r$plugins_selected_pivot), shinyjs::hide)
    #   shinyjs::show(r$plugins_selected_pivot)
    # })
    
    observeEvent(input$current_tab, {

      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    ##########################################
    # Update dropdowns                       #
    ##########################################
    
    observeEvent(r$plugins, {
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options)
    })
    
    ##########################################
    # See all plugins                        #
    ##########################################
    
    ##########################################
    # Create a plugin                        #
    ##########################################
    
    observeEvent(input$add_plugin, {
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$plugin_name)
      new_data$plugin_name <- new_data$name
      new_data$module_type <- module_type_id
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = "settings_plugins", 
        data = new_data, table = "plugins", required_textfields = "plugin_name", req_unique_values = "name")
      
    })
    
    ##########################################
    # Plugins management                     #
    ##########################################
    
    # Action buttons for each module / page
    action_buttons <- c("delete", "edit_code", "options")
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
    centered_cols <- c("id", "datetime", "action")
    searchable_cols <- c("name")
    hidden_cols <- c("id", "description", "module_type_id", "deleted", "modified")
    col_names <- get_col_names("plugins", language)
    
    # Prepare data for datatable
    
    observeEvent(r$plugins, {
      
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Prepare data for datatable
      
      r[[paste0(prefix, "_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "plugins", action_buttons = action_buttons, 
        data_input = r[[paste0(prefix, "_plugins_temp")]], words = r$words)
      
      # Render datatable
      
      render_datatable(output = output, r = r, ns = ns, language = language, data = r[[paste0(prefix, "_plugins_datatable_temp")]],
        output_name = "plugins_datatable", col_names =  get_col_names(table_name = "plugins", language = language, words = r$words),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
      
      # Create a proxy for datatable
      
      r[[paste0(prefix, "_plugins_datatable_proxy")]] <- DT::dataTableProxy("plugins_datatable", deferUntilFlush = FALSE)
    })
    
    # Reload datatable
    observeEvent(r[[paste0(prefix, "_plugins_temp")]], {
      
      # Reload datatable_temp variable
      r[[paste0(prefix, "_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "plugins", action_buttons = action_buttons, 
        data_input = r[[paste0(prefix, "_plugins_temp")]], words = r$words)
      
      # Reload data of datatable
      if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) > 0) DT::replaceData(r[[paste0(prefix, "_plugins_datatable_proxy")]], 
        r[[paste0(prefix, "_plugins_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$plugins_datatable_cell_edit, {
      
      edit_info <- input$plugins_datatable_cell_edit
      r[[paste0(prefix, "_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_plugins_temp")]], edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r[[paste0(prefix, "_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_plugins_management, {
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) == 0) show_message_bar(output, 2, "modif_saved", "success", language)
      
      req(nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "plugins", r_table = paste0(prefix, "_plugins"), language = language, duplicates_allowed = FALSE)
      
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

    delete_element(r = r, input = input, output = output, session = session, ns = ns, language = language,
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
      
      # Change PivotItem
      r$plugins_selected_pivot <- "edit_plugin_code_card"
    })
    
    ##########################################
    # Plugin options                         #
    ##########################################
    
    # observeEvent(input$options_chosen_plugin, {
    #   
    #   link_id <- input$options_chosen_plugin
    #   
    #   options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
    #   shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_plugin", options = options, value = link_id)
    #   
    # })
    
    ##########################################
    # Edit plugin code                       #
    ##########################################
    
    observeEvent(input$code_chosen_plugin, {
      
      if (length(input$code_chosen_plugin) > 1) link_id <- input$code_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(module_type_id == !!module_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$plugins %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_plugin", options = options, value = link_id)

      # Get code from database
      code <- list()
      code$ui <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
      code$server <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code)
      
      shinyAce::updateAceEditor(session, "ace_edit_code_ui", value = code$ui)
      shinyAce::updateAceEditor(session, "ace_edit_code_server", value = code$server)
      
      # Render UI of this edit_code card
      # output$edit_code_card <- renderUI({
      #   render_settings_code_card(ns = ns, r = r, id = id, title = paste0("edit_plugins_code"), code = code, link_id = link_id, language = language)
      # })

      # Reset code_result textOutput
      output$code_result_ui <- renderUI("")
      output$code_result_server <- renderText("")
    })
    
    # Load thesaurus items
    
    if (prefix == "patient_lvl"){
      
      ##########################################
      # Thesaurus datatable                    #
      ##########################################
      
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
        shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
        
      })
      
      # Load thesaurus items
      observeEvent(input$thesaurus, {
        
        r$plugin_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus$key, category = "plus_plugin")
        
        colour_col <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus$key, category = "colours_plugin")
        
        if (nrow(colour_col) > 0) r$plugin_thesaurus_items <- r$plugin_thesaurus_items %>%
          dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
        
        count_items_rows <- tibble::tibble()
        count_patients_rows <- tibble::tibble()
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
          datamart_id = r$chosen_datamart, category = "count_items_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("plugins - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
          datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("plugins - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
        
        if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language, words = r$words)
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
        col_names <- get_col_names(table_name = "modules_thesaurus_items_with_counts", language = language, words = r$words)
        hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
        
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, language = language, data = r$plugin_thesaurus_items_temp,
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
      
      ##########################################
      # Thesaurus items                        #
      ##########################################
      
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
          options <- convert_tibble_to_list(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
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
        options <- convert_tibble_to_list(r$plugin_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
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
      
      if (!var_check) show_message_bar(output = output, id = 3, message = "load_some_patient_data", language = language, words = r$words)
      
      req(var_check)
      
      # Get thesaurus items
      
      # if (prefix == "patient_lvl"){
      #   
      #   r$plugin_thesaurus_selected_items
      #   
      #   # Get thesaurus items with thesaurus own item_id
      #   thesaurus_selected_items <- r$plugin_thesaurus_selected_items %>%
      #     
      #     dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
      #       thesaurus_item_unit, colour = thesaurus_item_colour)
      # }
      
      if (length(input$code_chosen_plugin) > 1) link_id <- input$code_chosen_plugin$key
      else link_id <- input$code_chosen_plugin
    
      ui_code <- input$ace_edit_code_ui
      server_code <- input$ace_edit_code_server
      
      # Replace %group_id% in ui_code with 1 for our example
      ui_code <- ui_code %>% 
        stringr::str_replace_all("%module_id%", "1") %>%
        stringr::str_replace_all("%group_id%", as.character(get_last_row(r$db, paste0(prefix, "_modules_elements")) + 10000000)) %>%
        stringr::str_replace_all("%study_id%", as.character(r$chosen_study)) %>%
        stringr::str_replace_all("\r", "\n")
      
      if (prefix == "patient_lvl") ui_code <- ui_code %>% stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))
      
      server_code <- server_code %>% 
        stringr::str_replace_all("%module_id%", "1") %>%
        stringr::str_replace_all("%group_id%", as.character(get_last_row(r$db, paste0(prefix, "_modules_elements")) + 10000000)) %>%
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
      
      ui_code_id <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(id)
      server_code_id <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(id)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code_ui, "'", "''"), "' WHERE id = ", ui_code_id)) -> query
      DBI::dbClearResult(query)
      
      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code_server, "'", "''"), "' WHERE id = ", server_code_id)) -> query
      DBI::dbClearResult(query)
      
      update_r(r = r, table = "code")
      
      # Notification to user
      show_message_bar(output, 4, "modif_saved", "success", language)
      
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
    
    ##########################################
    # Export a plugin                        #
    ##########################################
    
  })
}