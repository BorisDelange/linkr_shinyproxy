#' my_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_studies_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  # *** To be removed *** ----
  language <- "EN"
  
  cards <- c("datamarts_options_card", "datamarts_edit_code_card", 
    "study_messages_card", "studies_creation_card", "studies_datatable_card", "study_options_card",
    "import_study_card", "export_study_card", "modules_families_card", "thesaurus_datamart_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = language, words = words))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::reactOutput(ns("thesaurus_item_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "datamart_main", text = i18n$t("My studies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "study_messages_card", itemKey = "study_messages_card", headerText = i18n$t("Messages")),
          shiny.fluent::PivotItem(id = "studies_creation_card", itemKey = "studies_creation_card", headerText = i18n$t("Create a study")),
          shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = i18n$t("Studies management")),
          shiny.fluent::PivotItem(id = "study_options_card", itemKey = "study_options_card", headerText = i18n$t("Study options")),
          shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study_card", headerText = i18n$t("Import a study")),
          shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study_card", headerText = i18n$t("Export a study"))
        )
      )
    ),
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("Choose a damatart in the dropdown on the left-side of the page"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- -- -- --
    # Study messages card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("study_messages_card"),
        make_card(i18n$t("Messages"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p(tags$ul(
                    tags$li("Message indiquant qu'il faut choisir une étude pour afficher la page"),
                    tags$li("Affichage des messages, plus récent en bas, avec scroll infini"),
                    tags$li("Nécessite de bien avoir sécurisé les interfaces R Code (avec environnements...)")
                  )
                )
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- --
    # Create a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("studies_creation_card"),
        make_card(i18n$t("Create a study"), 
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield(language = language, ns = ns, label = "name", id = "study_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_study"), i18n$t("Add"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Studies management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("studies_datatable_card"),
        make_card(i18n$t("Studies management"),
          div(
            DT::DTOutput(ns("studies_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_studies_management"), i18n$t("Save"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- ---
    # Study options card ----
    # --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("study_options_card"),
        make_card(i18n$t("Study options"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Pareil que pour les datamarts, à qui donner l'accès à l'étude")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Import a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("import_study_card"),
        make_card(i18n$t("Import a study"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Importer une étude nécessite d'importer :",
                  tags$ul(
                    tags$li("Importer l'étude en elle-même (table études de la BDD)"),
                    tags$li("Importer les données relatives à l'étude (modules, données modifiées sur patients et sur modules)"),
                    tags$li("S'assurer que les plugins sont tous installés et à la bonne version")
                  )
                ),
                p("Comment faire pour les correspondances entre membres ?")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Export a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_card(i18n$t("Export a study"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Même principe que pour l'import d'une étude")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    )
  )
}
    
#' my_studies Server Functions
#'
#' @noRd 
mod_my_studies_server <- function(id = character(), r, language = "EN", i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("datamarts_options_card", "datamarts_edit_code_card", 
      "study_messages_card", "studies_creation_card", "studies_datatable_card", "study_options_card",
      "import_study_card", "export_study_card", "modules_families_card")#, "thesaurus_datamart_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)

    # --- --- --- --- --- --- --- --
    # When a datamart is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("study_messages_card" %in% r$user_accesses) shinyjs::show("study_messages_card")
        else shinyjs::show("studies_creation_card_forbidden")
      }
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
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
      
    })
    
    # --- --- --- --- --- -
    # Datamart options ----
    # --- --- --- --- --- -
    
    observeEvent(input$save_datamart_options, {

      data <- list()
      data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
      data$users_allowed_read <- unique(input$users_allowed_read)
      data$users_allowed_read_group <- input$users_allowed_read_group

      save_settings_options(output = output, r = r, id = id, category = "datamart", code_id_input = paste0("options_", r$chosen_datamart),
        language = language, data = data, page_options = c("show_only_aggregated_data", "users_allowed_read"))

    })
    
    # --- --- --- --- --- ---
    # Edit datamart code ----
    # --- --- --- --- --- ---

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
    
    # --- --- --- --- ---
    # Create a study ----
    # --- --- --- --- ---
    
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
    
    # --- --- --- --- --- ---
    # Studies management ----
    # --- --- --- --- --- ---
    
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

      r$studies_datatable_proxy <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
    })
    
    # Reload datatable
    observeEvent(r$studies_temp, {

      # Reload datatable_temp variable
      r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons, 
        data_input = r$studies_temp, words = r$words)

      # Reload data of datatable
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
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
    
    study_delete_prefix <- "study"
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
    
    # --- --- --- --- ---
    # Import a study ----
    # --- --- --- --- ---
    
    # --- --- --- --- ---
    # Export a study ----
    # --- --- --- --- ---
    
  })
}