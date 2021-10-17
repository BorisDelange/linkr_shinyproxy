#' settings_data_management UI Function
#'
#' @description Shiny module of settings / data management
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), language = "EN"){
  ns <- NS(id)
  result <- div()
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datamarts", "data_source",
    "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
    "settings_subsets", "study",
    "settings_thesaurus", "data_source")
  
  ##########################################
  # Data management / Data sources         #
  ##########################################
  
  if (id == "settings_data_sources"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_data_source"),
        list(key = "datatable_card", label = "data_sources_management"))),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_data_source",
        textfields = c("name", "description"), textfields_width = "300px"),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = "data_sources_management")
    ) -> result
  }
  
  ##########################################
  # Data management / Datamarts            #
  ##########################################
  
  if (id == "settings_datamarts"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_datamart"),
        list(key = "datatable_card", label = "datamarts_management"),
        list(key = "edit_code_card", label = "edit_datamart_code"),
        list(key = "options_card", label = "datamart_options"))),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_datamart",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns("options_card")),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = "datamarts_management")
    ) -> result
  }
  
  ##########################################
  # Data management / Studies              #
  ##########################################
  
  if (id == "settings_studies"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_study"),
        list(key = "datatable_card", label = "studies_management"),
        list(key = "options_card", label = "study_options")
      )),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_study",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      uiOutput(ns("options_card")),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = "studies_management")
    ) -> result
  }
  
  ##########################################
  # Data management / Subsets              #
  ##########################################
  
  if (id == "settings_subsets"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_subset"),
        list(key = "datatable_card", label = "subsets_management"),
        list(key = "edit_code_card", label = "edit_subset_code")
      )),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_subset",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      uiOutput(ns("edit_code_card")),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = "subsets_management")
    ) -> result
  }
  
  ##########################################
  # Data management / Thesaurus            #
  ##########################################
  
  if (id == "settings_thesaurus"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_thesaurus"),
        list(key = "datatable_card", label = "thesaurus_management_card"),
        list(key = "items_datatable_card", label = "thesaurus_items_management_card"),
        list(key = "edit_code_card", label = "edit_thesaurus_code")
      )),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_thesaurus",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = "thesaurus_management"),
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable_bis", title = "thesaurus_items_management"),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns("options_card"))
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

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Toggles IDs
    toggles <- c("creation_card", "datatable_card", "edit_code_card", "options_card")

    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "settings_data_sources", "",
      "settings_datamarts", "data_source",
      "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "settings_subsets", c("datamart", "study"),
      "settings_thesaurus", "data_source")
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    
    # Init delete_dialog variable
    # r[[paste0(table, "_delete_dialog")]] <- FALSE
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    sapply(c("data_sources", "datamarts", "studies", "subsets", "patient_lvl_module_families", "aggregated_module_families"), 
      function(data_var){
        observeEvent(r[[data_var]], {
          # Convert options to list
          options <- convert_tibble_to_list(data = r[[data_var]], key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
        })
      })
    
    # When add button is clicked
    observeEvent(input$add, {
      
      # Create a list with new data
      # If page = thesaurus, data_source is character, not integer (multiple choices)
      new_data <- list()
      if (id == "settings_thesaurus") new_data_var <- c("name" = "char", "description" = "char", "data_source" = "char")
      else new_data_var <- c("name" = "char", "description" = "char", "data_source" = "int", "datamart" = "int", 
        "study" = "int", "patient_lvl_module_family" = "int", "aggregated_module_family" = "int")
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
      })
      # Convert data_source to string, for page thesaurus
      if (id == "settings_thesaurus"){
        if (length(new_data$data_source) == 1) new_data$data_source <- coalesce2(type = "char", x = input$data_source)
        else new_data$data_source <- toString(new_data$data_source)
      }

      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, data = new_data,
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
    })
    
    ##########################################
    # Data management / Elements management  #
    ##########################################
      
      ##########################################
      # Generate datatable                     #
      ##########################################
    
        # If r$... variable changes
        observeEvent(r[[paste0(substr(id, nchar("settings_") + 1, nchar(id)), "_temp")]], {
          
          # Dropdowns for each module / page
          # Finally, some columns are not editable (commented lines below)
          dropdowns = switch(id,
            "settings_data_sources" = "",
            "settings_datamarts" = "",
            # "settings_datamarts" = c("data_source_id" = "data_sources"),
            # "settings_studies" = c("datamart_id" = "datamarts", "patient_lvl_module_family_id" = "patient_lvl_module_families", "aggregated_module_family_id" = "aggregated_module_families"),
            "settings_studies" = c("patient_lvl_module_family_id" = "patient_lvl_module_families", "aggregated_module_family_id" = "aggregated_module_families"),
            "settings_subsets" = "",
            # "settings_subsets" = c("study_id" = "studies"),
            "settings_thesaurus" = c("data_source_id" = "data_sources"))
          
          # Action buttons for each module / page
          action_buttons = switch(table,
            "data_sources" = "delete",
            "datamarts" = c("delete", "edit_code", "options"),
            "studies" = c("delete", "options"),
            "subsets" = c("delete", "edit_code"),
            "thesaurus" = c("delete", "edit_code", "sub_datatable")
          )
          
          # Sortable cols
          if (id == "settings_thesaurus") sortable_cols <- c("id", "name", "description", "creator_id", "datetime")
          if (id != "settings_thesaurus") sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
          
          # Column widths
          column_widths <- c("datetime" = "130px", "action" = "80px", "data_source_id" = "100px")
          
          # Centered columns
          centered_cols <- c("id", "data_source_id", "patient_lvl_module_family_id", "aggregated_module_family_id", "creator", "datetime", "action")
          
          # Restore datatable state
          page_length <- isolate(input$management_datatable_state$length)
          start <- isolate(input$management_datatable_state$start)
          # search_recorded <- ""
          
          render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id,
            col_names =  get_col_names(table), table = table, dropdowns = dropdowns, action_buttons = action_buttons,
            datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
            editable_cols = c("name", "description"), sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths)
        })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
    
      # Each time a row is updated, modify temp variable
        observeEvent(input$management_datatable_cell_edit, {
          edit_info <- input$management_datatable_cell_edit
          r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
          # Store that this row has been modified
          r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
        })
    
      # Each time a dropdown is updated, modify temp variable
        observeEvent(r[[table]], {
          update_settings_datatable(input = input, r = r, ns = ns, table = table, 
            dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), language = language)
        })
      
        observeEvent(input$management_save, {
          save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language)
        })
    
      ##########################################
      # Delete a row in datatable              #
      ##########################################

        # Create & show dialog box
        observeEvent(r[[paste0(table, "_delete_dialog")]] , {
          output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = table, language = language))
        })
        
        # Whether to close or not delete dialog box
        observeEvent(input$hide_dialog, r[[paste0(table, "_delete_dialog")]] <- FALSE)
        observeEvent(input$delete_canceled, r[[paste0(table, "_delete_dialog")]] <- FALSE)
        observeEvent(input$deleted_pressed, r[[paste0(table, "_delete_dialog")]] <- TRUE)
        
        # When the delete is confirmed...
        observeEvent(input$delete_confirmed, {

          # Get value of deleted row
          row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))

          # Delete row in DB table
          delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
        })
        
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      observeEvent(input$options, {
        # Show options toggle
        shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = TRUE)
        
        # Render UI of options card
        output$options_card <- renderUI({
          # Get category & link_id to get informations in options table
          category <- get_singular(word = id)
          link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
          render_settings_options_card(ns = ns, id = id, r = r, title = paste0(get_singular(id), "_options"), 
            category = category, link_id = link_id, language = language)
        })
      })
      
      observeEvent(input$options_save, {
        category <- get_singular(id)
        
        data <- list()
        data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
        data$users_allowed_read <- input$users_allowed_read

        save_settings_options(output = output, r = r, id = id, category = category,
          code_id_input = input$options, language = language, data = data)
      })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      # Button "Edit code" is clicked on the datatable
      observeEvent(input$edit_code, {
        
        # Display edit_code card
        shiny.fluent::updateToggle.shinyInput(session, "edit_code_card_toggle", value = TRUE)
        
        # Render UI of this edit_code card
        output$edit_code_card <- renderUI({
          
          # Get category & link_id variables, to update code table
          category <- get_singular(id, language)
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$thesaurus_id <- NA_integer_
          
          if (id == "settings_datamarts") r$datamart_id <- link_id
          if (id == "settings_thesaurus") r$thesaurus_id <- link_id
          if (id == "settings_subsets"){
            r$datamart_id <- 
              r$studies %>% 
              dplyr::filter(id == (r$subsets %>% dplyr::filter(id == !!link_id) %>% dplyr::pull(study_id))) %>%
              dplyr::pull(datamart_id)
            r$subset_id <- link_id
          } 
          
          # Get code from database
          code <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code)
          
          # Render UI
          render_settings_code_card(ns = ns, id = id, title = paste0("edit_", category, "_code"), code = code, link_id = link_id, language = language)
        })
        
        # Reset code_result textOutput
        output$code_result <- renderText("")
      })
      
      # When save button is clicked
      observeEvent(input$edit_code_save,
        save_settings_code(output = output, r = r, id = id, category = get_singular(id, language),
          code_id_input = input$edit_code, edited_code = input$ace_edit_code, language = "EN"))
      
      # When Execute code button is clicked
      observeEvent(input$execute_code, (
        output$code_result <- renderText(
          execute_settings_code(output = output, r = r, edited_code = isolate(input$ace_edit_code)))))
          
      
      ##########################################
      # Load sub datatable with action button  #
      ##########################################
      
      # observeEvent(input[[paste0(prefix, "_sub_datatable")]], {
      #   shiny.fluent::updateToggle.shinyInput(session, "items_datatable_card_toggle", value = TRUE)
      #   link_id_filter <- as.integer(substr(input[[paste0(prefix, "_sub_datatable")]], nchar(paste0(prefix, "_sub_datatable_")) + 1, nchar(input[[paste0(prefix, "_sub_datatable")]])))
      #   # if (prefix == "thesaurus_items"){
      #     r$thesaurus_items <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", link_id_filter, " AND deleted IS FALSE ORDER BY id"))
      #     r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
      #   # }
      # })
     
  })
}