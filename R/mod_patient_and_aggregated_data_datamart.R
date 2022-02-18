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
  
  cards <- c("datamarts_options_card", "datamarts_edit_code_card", "studies_creation_card", "studies_datatable_card",
    "import_study_card", "export_study_card", "modules_families_card", "thesaurus_datamart_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = language, words = words))
  })
  
  div(
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::reactOutput(ns("thesaurus_item_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "datamart_main", text = paste0(translate(language, paste0(prefix, "_data"), words), " - ", translate(language, "datamart", words)))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "studies_creation_card", itemKey = "studies_creation_card", headerText = translate(language, "create_study", words)),
      shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = translate(language, "studies_management", words)),
      shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study_card", headerText = translate(language, "import_study", words)),
      shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study_card", headerText = translate(language, "export_study", words)),
      shiny.fluent::PivotItem(id = "thesaurus_datamart_card", itemKey = "thesaurus_datamart_card", headerText = translate(language, "thesaurus", words))
    ),
    forbidden_cards,
    shinyjs::hidden(
      div(
        id = ns("datamarts_options_card"),
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
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("datamarts_edit_code_card"),
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
        id = ns("studies_creation_card"),
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
        id = ns("studies_datatable_card"),
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
        make_card("",
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_card("",
          div(shiny.fluent::MessageBar(translate(language, "in_progress", words), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("thesaurus_datamart_card"),
        make_card(translate(language, "thesaurus", words),
          div(
            make_combobox(language = language, ns = ns, label = "thesaurus", width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            DT::DTOutput(ns("thesaurus_items")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_thesaurus_items"), translate(language, "save", words)), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("reload_thesaurus_cache"), translate(language, "reload_cache", words)),
            br(),
            uiOutput(ns("thesaurus_selected_item"))
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
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("datamarts_options_card", "datamarts_edit_code_card", "studies_creation_card", "studies_datatable_card",
      "import_study_card", "export_study_card", "modules_families_card", "thesaurus_datamart_card")
    show_hide_cards_new(r = r, input = input, session = session, id = id, cards = cards)

    # Show first card
    if ("studies_creation_card" %in% r$user_accesses) shinyjs::show("studies_creation_card")
    else shinyjs::show("studies_creation_card_forbidden")
    
    # observeEvent(input$current_tab, {
    # 
    #   sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
    #   shinyjs::show(input$current_tab)
    # })

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

      # Datamart code

      datamart_code <- r$code %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart) %>% dplyr::pull(code)
      shinyAce::updateAceEditor(session, "datamart_ace_editor", value = datamart_code)
      
    })
      
    ##########################################
    # Render datamart UI                     #
    ##########################################
    
    # Show datamart UI, hide other UIs
    r$datamart_page <- Sys.time()
    
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
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "datetime", "action")
    searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    factorize_cols <- c("datamart_id", "creator_id")
    hidden_cols <- c("id", "description", "datamart_id", "patient_lvl_module_family_id", "aggregated_module_family_id", "deleted", "modified")
    col_names <- get_col_names("studies", language)
    
    # Prepare data for datatable
    
    observeEvent(r$studies, {
      
      if(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) == 0){
        render_datatable(output = output, r = r, ns = ns, language = language,
          data = tibble::tribble(~name, ~creator_id, ~datetime, ~action), output_name = "studies_datatable")
      }
      
      req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)

      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart) %>% dplyr::mutate(modified = FALSE)

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
      
      req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)
      
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
      
      # Reload sidenav dropdown with reloading studies
      update_r(r = r, table = "studies")
      
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
    
    observeEvent(r$chosen_datamart, {
      
      data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      # Multiple cases
      # Only one ID, so it's the beginning and the end
      # Last ID, so it's the end
      # ID between begin and last, so separated by commas
      thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
        grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
      shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
      
      if (length(r$datamart_thesaurus_items_temp) > 0) r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items_temp %>% dplyr::slice(0)
      
      # Reset UI of selected item
      output$thesaurus_selected_item <- renderUI("")
    })
    
    observeEvent(input$thesaurus, {
      
      r$reload_thesaurus_datatable <- Sys.time()
    })
    
    observeEvent(r$reload_thesaurus_datatable, {
      
      req(length(input$thesaurus$key) > 0)
      
      r$datamart_thesaurus_items <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted
          FROM thesaurus_items t
          WHERE t.thesaurus_id = ", input$thesaurus$key, " AND t.deleted IS FALSE
          ORDER BY t.id")) %>% tibble::as_tibble() %>% dplyr::mutate(action = "")

      count_items_rows <- tibble::tibble()
      count_patients_rows <- tibble::tibble()
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
        datamart_id = r$chosen_datamart, category = "count_items_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0("modules - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
        datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0("modules - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
      
      if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language, words = r$words)
      req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
      
      # Transform count_rows cols to integer, to be sortable
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>%
        dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
        dplyr::left_join(count_items_rows, by = "item_id") %>%
        dplyr::left_join(count_patients_rows, by = "item_id") %>%
        dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
      
      # Filter on count_items_rows > 0
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
      
      r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("item_id", as.character)
      
      editable_cols <- c("display_name", "unit")
      searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
      factorize_cols <- c("category", "unit")
      column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px", "category" = "400px")
      sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
      centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
      col_names <- get_col_names(table_name = "datamart_thesaurus_items_with_counts", language = language, words = r$words)
      hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified", "action")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, language = language, data = r$datamart_thesaurus_items_temp,
        output_name = "thesaurus_items", col_names =  col_names,
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatatable
      r[[paste0(prefix, "_datamart_thesaurus_items_datatable_proxy")]] <- DT::dataTableProxy("thesaurus_items", deferUntilFlush = FALSE)
    })
    
    # Reload thesarus cache
    
    observeEvent(input$reload_thesaurus_cache, {
      
      req(length(input$thesaurus$key) > 0)
      
      # Delete old cache
      
      sql <- glue::glue_sql("SELECT t.id FROM thesaurus_items t WHERE t.thesaurus_id = {input$thesaurus$key} AND t.deleted IS FALSE" , .con = r$db)
      ids_to_del <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
      
      sql <- glue::glue_sql("DELETE FROM cache WHERE category IN ('count_patients_rows', 'count_items_rows') 
        AND link_id IN ({ids_to_del*}) AND link_id_bis = {r$chosen_datamart}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      r$reload_thesaurus_datatable <- Sys.time()
    })
    
    # Reload datatable
    observeEvent(r$datamart_thesaurus_items_temp, {
      
      if (length(r[[paste0(prefix, "_datamart_thesaurus_items_datatable_proxy")]]) > 0) DT::replaceData(
        r[[paste0(prefix, "_datamart_thesaurus_items_datatable_proxy")]], r$datamart_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$thesaurus_items_cell_edit, {
      
      edit_info <- input$thesaurus_items_cell_edit
      r$datamart_thesaurus_items_temp <- DT::editData(r$datamart_thesaurus_items_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$datamart_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_thesaurus_items, {
      
      req(input$thesaurus)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "thesaurus_items", r_table = "datamart_thesaurus_items", duplicates_allowed = TRUE, language = language)
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_items_rows_selected, {
      
      style <- "display:inline-block; width:200px; font-weight:bold;"
      
      thesaurus_item <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
      
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
      
      all_values <- r$labs_vitals %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
        dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% dplyr::select(value, value_num)
      values_num <- suppressMessages(all_values %>% dplyr::filter(!is.na(value_num)) %>% dplyr::slice_sample(n = 5) %>% dplyr::pull(value_num))
      values <- suppressMessages(all_values %>% dplyr::filter(!is.na(value)) %>% dplyr::slice_sample(n = 5) %>% dplyr::pull(value))
      values_text <- tagList(
        span(translate(language, "values", r$words), style = style), paste(values, collapse = " || "), br(),
        span(translate(language, "numeric_values", r$words), style = style), paste(values_num, collapse = " || "), br()
      )
      
      if (nrow(all_values) == 0){
        
        all_values <- r$orders %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
          dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% 
          dplyr::mutate(amount_text = paste0(amount, " ", amount_unit), rate_text = paste0(rate, " ", rate_unit)) %>%
          dplyr::select(amount, amount_text, rate, rate_text)
        amount <- suppressMessages(all_values %>% dplyr::filter(!is.na(amount)) %>% dplyr::slice_sample(n = 5) %>% dplyr::pull(amount_text))
        rate <- suppressMessages(all_values %>% dplyr::filter(!is.na(rate)) %>% dplyr::slice_sample(n = 5) %>% dplyr::pull(rate_text))
        
        values_text <- tagList(
          span(translate(language, "rate_values", r$words), style = style), paste(rate, collapse = " || "), br(),
          span(translate(language, "amount_values", r$words), style = style), paste(amount, collapse = " || "), br()
        )
        
        if (nrow(all_values) == 0) values_text <- ""
      }
      
      output$thesaurus_selected_item <- renderUI(tagList(br(), div(
        span(translate(language, "display_name", r$words), style = style), thesaurus_item$display_name, br(),
        span(translate(language, "thesaurus_id", r$words), style = style), thesaurus_item$thesaurus_id, br(),
        span(translate(language, "item_id", r$words), style = style), thesaurus_item$item_id, br(),
        values_text,
        style = "border:dashed 1px; padding:10px;"
      )))
    })
    
  })
}