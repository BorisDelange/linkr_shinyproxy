#' patient_and_aggregated_data_datamart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_datamart_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  # Prefix depending on page id
  if (id == "patient_level_data_datamart") prefix <- "patient_lvl"
  if (id == "aggregated_data_datamart") prefix <- "aggregated"
  
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::reactOutput(ns("thesaurus_item_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "datamart_main", text = translate(language, "datamart", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-datamart_current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "datamart_options_card", itemKey = "datamart_options", headerText = translate(language, "datamart_options", words)),
      shiny.fluent::PivotItem(id = "edit_datamart_code_card", itemKey = "edit_datamart_code", headerText = translate(language, "edit_datamart_code", words)),
      # shiny.fluent::PivotItem(id = "modules_families_card", itemKey = "modules_families", headerText = translate(language, "modules_families", words)),
      shiny.fluent::PivotItem(id = "create_study_card", itemKey = "create_study", headerText = translate(language, "create_study", words)),
      shiny.fluent::PivotItem(id = "studies_management_card", itemKey = "studies_management", headerText = translate(language, "studies_management", words)),
      shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study", headerText = translate(language, "import_study", words)),
      shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study", headerText = translate(language, "export_study", words)),
      shiny.fluent::PivotItem(id = "thesaurus_card", itemKey = "thesaurus", headerText = translate(language, "thesaurus", words))
    ),
    div(
      id = ns("datamart_options_card"),
      make_card(translate(language, "datamart_options", words),
        div(
          br(),
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 10),
            make_toggle(language = language, ns = ns, label = "show_only_aggregated_data", inline = TRUE, words = words)
          ), br(),
          div(
            div(class = "input_title", paste0(translate(language, "datamart_users_allowed_read", words), " :")),
            shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
              list(key = "everybody", text = translate(language, "everybody", words)),
              list(key = "people_picker", text = translate(language, "people_picker", words))
            ), className = "inline_choicegroup"),
            conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
              uiOutput(ns("users_allowed_read_div"))
            )
          ), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_datamart_options"), translate(language, "save", words))
        )
      ), br()
    ),
    shinyjs::hidden(
      div(
        id = ns("edit_datamart_code_card"),
        make_card(translate(language, "edit_datamart_code", words),
          div(
            div(shinyAce::aceEditor(ns("datamart_ace_editor"), "", mode = "r", 
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), translate(language, "save", words)), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), translate(language, "execute_code", words)), br(), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("create_study_card"),
        make_card(translate(language, "create_study", words), 
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield(language = language, ns = ns, label = "name", id = "study_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_study"), translate(language, "add", words))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("studies_management_card"),
        make_card(translate(language, "studies_management", words),
          div(
            DT::DTOutput(ns("studies_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_studies_management"), translate(language, "save", words))
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_study_card"),
        make_card(translate(language, "import_study", words),
          div("...")
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_card(translate(language, "export_study", words),
          div("...")
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thesaurus_card"),
        make_card(translate(language, "thesaurus", words),
          div(
            DT::DTOutput(ns("thesaurus")), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_thesaurus"), translate(language, "save", words))
          )
        ), br()
      )
    )
  )
}
    
#' patient_and_aggregated_data_datamart Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_datamart_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data_datamart") prefix <- "patient_lvl"
    if (id == "aggregated_data_datamart") prefix <- "aggregated"
 
    # When a datamart is chosen
    
    observeEvent(r$chosen_datamart, {
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      ##########################################
      # Load datamart data                     #
      ##########################################
      
      # Reset r variables (prevent bug later if datamart code doesn't work)
      r$patients <- tibble::tibble()
      r$stays <- tibble::tibble()
      r$labs_vitals <- tibble::tibble()
      r$text <- tibble::tibble()
      r$orders <- tibble::tibble()
      
      # Try to load datamart 
      tryCatch({
        run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language, quiet = TRUE)
        
        # A r variable to update Study dropdown, when the load of datamart is finished
        r$loaded_datamart <- r$chosen_datamart
        
        show_message_bar(output, 1, "import_datamart_success", "success", language, r$words)
      },
      error = function(e) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
        error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language))
      
      ##########################################
      # Load UI data                           #
      ##########################################

      # Datamart options

      options <- r$options %>% dplyr::filter(category == "datamart", link_id == r$chosen_datamart)

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

      shiny.fluent::updateToggle.shinyInput(session, "show_only_aggregated_data",
        value = options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num) %>% as.logical)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          language = language, ns = ns, id = "users_allowed_read", label = "blank", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;", words = words)
      })
      
      # shiny.fluent::updateNormalPeoplePicker.shinyInput(session, "users_allowed_read", options = picker_options, 
      #   value = value, selectedItems = selected_items)

      # Datamart code

      datamart_code <- r$code %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart) %>% dplyr::pull(code)
      shinyAce::updateAceEditor(session, "datamart_ace_editor", value = datamart_code)
      
    })
      
    ##########################################
    # Render datamart UI                     #
    ##########################################
    
    # Show datamart UI, hide other UIs
    r$datamart_page <- Sys.time()
    
    # Show or hide datamart cards
    
    datamarts_cards <- c("datamart_options_card", "edit_datamart_code_card", "create_study_card", "studies_management_card",
      "import_study_card", "export_study_card", "modules_families_card", "thesaurus_card")
    
    observeEvent(input$datamart_current_tab, {
      
      sapply(datamarts_cards %>% setdiff(., input$datamart_current_tab), shinyjs::hide)
      shinyjs::show(input$datamart_current_tab)
    })
    
    ##########################################
    # Datamart options                       #
    ##########################################
    
    observeEvent(input$save_datamart_options, {

      data <- list()
      data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
      data$users_allowed_read <- unique(input$users_allowed_read)
      data$users_allowed_read_group <- input$users_allowed_read_group

      save_settings_options(output = output, r = r, id = id, category = "datamart", code_id_input = paste0("options_", r$chosen_datamart),
        language = language, data = data, page_options = c("show_only_aggregated_data", "users_allowed_read"))

    })
    
    ##########################################
    # Edit datamart code                     #
    ##########################################

    # Execute code
    
    observeEvent(input$execute_code, {

      code <- input$datamart_ace_editor %>% 
        stringr::str_replace_all("\r", "\n") %>%
        stringr::str_replace_all("%datamart_id%", as.character(isolate(r$chosen_datamart)))
      
      # Change this option to display correctly tibble in textbox
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = as.character(code))), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      # Display result
      output$code_result <- renderText(paste(strwrap(captured_output), collapse = "\n"))
    })
    
    # Save updates
    
    observeEvent(input$save_code, {
      
      code_id <- r$code %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart) %>% dplyr::pull(id)

      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '",
        stringr::str_replace_all(input$datamart_ace_editor, "'", "''"), "' WHERE id = ", code_id)) -> query
      DBI::dbClearResult(query)
      update_r(r = r, table = "code")

      # Notification to user
      show_message_bar(output, 4, "modif_saved", "success", language)
      
    })
    
    ##########################################
    # Create a study                         #
    ##########################################
    
    observeEvent(input$add_study, {
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$study_name)
      new_data$study_name <- new_data$name
      new_data$description <- ""
      new_data$patient_lvl_module_family <- get_last_row(r$db, "patient_lvl_modules_families") + 1
      new_data$aggregated_module_family <- get_last_row(r$db, "aggregated_modules_families") + 1
      new_data$datamart <- r$chosen_datamart
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = "settings_studies", 
        data = new_data, table = "studies", required_textfields = "study_name", req_unique_values = "name")
      
    })
    
    ##########################################
    # Studies management                     #
    ##########################################
    
    # Action buttons for each module / page
    action_buttons <- c("delete")
    
    editable_cols <- c("name", "description")
    sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "datetime", "action")
    searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    factorize_cols <- c("datamart_id", "creator_id")
    hidden_cols <- c("id", "description", "datamart_id", "patient_lvl_module_family_id", "aggregated_module_family_id", "deleted", "modified")
    col_names <- get_col_names("studies", language)
    
    # Prepare data for datatable
    
    observeEvent(r$chosen_datamart, {
      
      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)  %>% dplyr::mutate(modified = FALSE)
      
      # Prepare data for datatable
      
      r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons, 
        data_input = r$studies_temp, words = r$words)
      
      # Render datatable
      
      render_datatable(output = output, r = r, ns = ns, language = language, data = r$studies_datatable_temp,
        output_name = "studies_datatable", col_names =  get_col_names(table_name = "studies", language = language, words = r$words),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatable
      
      r[[paste0(prefix, "_studies_datatable_proxy")]] <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
    })
    
    # Reload datatable
    observeEvent(r$studies_temp, {

      # Reload datatable_temp variable
      r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons, 
        data_input = r$studies_temp, words = r$words)

      # Reload data of datatable
      if (length(r[[paste0(prefix, "_studies_datatable_proxy")]]) > 0) DT::replaceData(r[[paste0(prefix, "_studies_datatable_proxy")]], 
        r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$studies_datatable_cell_edit, {
      
      edit_info <- input$studies_datatable_cell_edit
      r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_studies_management, {
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, table = "studies", language = language, duplicates_allowed = FALSE)
      
      # Update sidenav dropdown with the new study
      r$reload_studies <- Sys.time()
    })
    
    # Delete a row in datatable
    
    study_delete_prefix <- paste0(prefix, "_study")
    study_dialog_title <- "studies_delete"
    study_dialog_subtext <- "studies_delete_subtext"
    study_react_variable <- "study_delete_confirm"
    study_table <- "studies"
    study_id_var_sql <- "id"
    study_id_var_r <- "delete_study"
    study_delete_message <- "study_deleted"
    study_reload_variable <- "reload_studies"
    study_information_variable <- "study_deleted"
    study_delete_variable <- paste0(study_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, language = language,
      delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
      react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
      delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
      information_variable = study_information_variable)
    
    observeEvent(input$deleted_pressed, {
      
      r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[study_delete_variable]] <- TRUE
      
    })
    
    observeEvent(r$reload_studies, {
      
      # Reload sidenav dropdown
      r$studies_choices <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE deleted IS FALSE AND datamart_id = ", r$chosen_datamart))
      
      # Reload datatable
      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)  %>% dplyr::mutate(modified = FALSE)
      
      # Reset chosen study
      r$chosen_study <- NA_integer_
    })
    
    ##########################################
    # Import a study                         #
    ##########################################
    
    ##########################################
    # Export a study                         #
    ##########################################
    
    ##########################################
    # Thesaurus                              #
    ##########################################
    
  })
}