#' settings_data_management UI Function
#'
#' @description Shiny module of settings / data management
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  result <- div()
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datamarts", "data_source",
    "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
    "settings_subsets", c("datamart", "study"),
    "settings_thesaurus", "data_source")
  
  ##########################################
  # Data management / Data sources         #
  ##########################################
  
  if (id == "settings_data_sources"){
      div(class = "main",
        render_settings_toggle_card(language = language, ns = ns, cards = list(
          list(key = "creation_card", label = "create_data_source"),
          list(key = "datatable_card", label = "data_sources_management")), words = words),
        render_settings_default_elements(ns = ns),
        render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_data_source",
        textfields = c("name", "description"), textfields_width = "300px", words = words),
        render_settings_datatable_card(language = language, ns = ns,
        div_id = "datatable_card", output_id = "management_datatable", title = "data_sources_management", words = words)
      ) -> result
    }
  
  ##########################################
  # Data management / Datamarts            #
  ##########################################
  
  if (id == "settings_datamarts"){
    div(class = "main",
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_datamart"),
        list(key = "datatable_card", label = "datamarts_management"),
        list(key = "edit_code_card", label = "edit_datamart_code"),
        list(key = "options_card", label = "datamart_options")), words = words),
      render_settings_default_elements(ns = ns),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_datamart",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px", words = words),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns("options_card")),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card", 
        output_id = "management_datatable", title = "datamarts_management", words = words)
    ) -> result
  }
  
  ##########################################
  # Data management / Studies              #
  ##########################################
  
  if (id == "settings_studies"){
    div(class = "main",
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_study"),
        list(key = "datatable_card", label = "studies_management"),
        list(key = "options_card", label = "study_options")
      ), words = words),
      render_settings_default_elements(ns = ns),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_study",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px", words = words),
      uiOutput(ns("options_card")),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card", 
        output_id = "management_datatable", title = "studies_management", words = words)
    ) -> result
  }
  
  ##########################################
  # Data management / Subsets              #
  ##########################################
  
  if (id == "settings_subsets"){
    div(class = "main",
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_subset"),
        list(key = "datatable_card", label = "subsets_management"),
        list(key = "edit_code_card", label = "edit_subset_code")
      ), words = words),
      render_settings_default_elements(ns = ns),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_subset",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px", words = words),
      uiOutput(ns("edit_code_card")),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card", 
        output_id = "management_datatable", title = "subsets_management", words = words)
    ) -> result
  }
  
  ##########################################
  # Data management / Thesaurus            #
  ##########################################
  
  if (id == "settings_thesaurus"){
    div(class = "main",
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_thesaurus"),
        list(key = "datatable_card", label = "thesaurus_management_card"),
        list(key = "sub_datatable_card", label = "thesaurus_items_management_card"),
        list(key = "edit_code_card", label = "edit_thesaurus_code")
      ), words = words),
      render_settings_default_elements(ns = ns),
      render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_thesaurus",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px", words = words),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns("options_card")),
      uiOutput(ns("sub_datatable_card")),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card",
        output_id = "management_datatable", title = "thesaurus_management", words = words)
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

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "settings_data_sources", "",
      "settings_datamarts", "data_source",
      "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "settings_subsets", c("datamart", "study"),
      "settings_thesaurus", "data_source")
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    # Toggles IDs
    toggles <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "sub_datatable_card")
    
    show_hide_cards(r = r, input = input, session = session, table = table, id = id, toggles = toggles)
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    
    uploaded_dropdowns <- dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist()
    
    sapply(uploaded_dropdowns, 
      function(data_var){
        
        # Create only needed observers for current page
        if (data_var != ""){
          data_var <- get_plural(data_var)
          observeEvent(r[[data_var]], {
            
            # Convert options to list
            if (table == "subsets" & data_var == "studies") options <- list()
            else options <- convert_tibble_to_list(data = r[[data_var]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        }
      })
    
    # Particular case for subsets, update study dropdown with current datamart
    if (table == "subsets"){
      observeEvent(input$datamart, {
        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
        options <- convert_tibble_to_list(data = studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        shiny.fluent::updateDropdown.shinyInput(session, "study", options = options)
      })
    }
    
    # When add button is clicked
    observeEvent(input$add, {
      
      # If user has access
      req(paste0(table, "_creation_card") %in% r$user_accesses)
      
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
        else new_data$data_source <- toString(as.integer(new_data$data_source))
      }
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, 
        data = new_data,
        table = substr(id, nchar("settings_") + 1, nchar(id)), 
        required_textfields = "name", req_unique_values = "name",
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
          
          # If user has access
          req(paste0(table, "_datatable_card") %in% r$user_accesses)
          
          # Dropdowns for each module / page
          # Finally, some columns are not editable (commented lines below)
          dropdowns_datatable <- switch(id,
            "settings_data_sources" = "",
            "settings_datamarts" = "",
            # "settings_datamarts" = c("data_source_id" = "data_sources"),
            # "settings_studies" = c("datamart_id" = "datamarts", "patient_lvl_module_family_id" = "patient_lvl_modules_families", "aggregated_module_family_id" = "aggregated_modules_families"),
            "settings_studies" = c("patient_lvl_module_family_id" = "patient_lvl_modules_families", "aggregated_module_family_id" = "aggregated_modules_families"),
            "settings_subsets" = "",
            # "settings_subsets" = c("study_id" = "studies"),
            "settings_thesaurus" = c("data_source_id" = "data_sources"))
          
          # Action buttons for each module / page
          if (paste0(table, "_delete_data") %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
          action_buttons = switch(table,
            "data_sources" = action_buttons,
            "datamarts" = c(action_buttons, "edit_code", "options"),
            "studies" = c(action_buttons, "options"),
            "subsets" = c(action_buttons, "edit_code"),
            "thesaurus" = c(action_buttons, "edit_code", "sub_datatable")
          )
          
          # Editable cols
          if (id != "settings_subsets") editable_cols <- c("name", "description")
          if (id == "settings_subsets") editable_cols <- "description"
          
          # Sortable cols
          if (id == "settings_thesaurus") sortable_cols <- c("id", "name", "description", "creator_id", "datetime")
          if (id != "settings_thesaurus") sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
          
          # Column widths
          column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
          
          # Centered columns
          centered_cols <- c("id", "data_source_id", "patient_lvl_module_family_id", "aggregated_module_family_id", "creator", "datetime", "action")
          
          # Searchable_cols
          if (id != "settings_thesaurus") searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
          if (id == "settings_thesaurus") searchable_cols <- c("name", "description", "creator_id")
          
          # Factorize_cols
          factorize_cols <- switch(id,
            "settings_data_sources" = "creator_id",
            "settings_datamarts" = c("data_source_id", "creator_id"),
            "settings_studies" = c("datamart_id", "creator_id"),
            "settings_subsets" = c("study_id", "creator_id"),
            "settings_thesaurus" = "creator_id")
          
          # Restore datatable state
          page_length <- isolate(input$management_datatable_state$length)
          start <- isolate(input$management_datatable_state$start)
          # search_recorded <- ""
          
          render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
            col_names =  get_col_names(table_name = table, language = language), table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons,
            datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
            filter = TRUE, searchable_cols = searchable_cols, factorize_cols = factorize_cols, column_widths = column_widths)
        })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
    
        # Hide save button if user has no access
        observeEvent(r$user_accesses, if (paste0(table, "_edit_data") %not_in% r$user_accesses) shinyjs::hide("management_save"))
    
        # Each time a row is updated, modify temp variable
        # Do that for main datatable (management_datatable) & sub_datatable
        observeEvent(input$management_datatable_cell_edit, {
          edit_info <- input$management_datatable_cell_edit
          r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
          # Store that this row has been modified
          r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
        })
    
        observeEvent(input$sub_datatable_cell_edit, {
          edit_info <- input$sub_datatable_cell_edit
          edit_info$col <- edit_info$col + 2 # We have removed id & thesaurus_id cols, so need to add two to col index
          r$sub_thesaurus_items_temp <- DT::editData(r$sub_thesaurus_items_temp, edit_info, rownames = FALSE)
          r$sub_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
        })
      
        # Each time a dropdown is updated, modify temp variable
        observeEvent(r[[table]], {
          update_settings_datatable(input = input, r = r, ns = ns, table = table, 
            dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), language = language)
        })
      
        # When save button is clicked
        # Do that for main datatable (management_datatable) & sub_datatable
        observeEvent(input$management_save, {
          duplicates_allowed <- FALSE
          if (table == "subsets") duplicates_allowed <- TRUE
          save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language, duplicates_allowed = duplicates_allowed)
        })
        observeEvent(input$sub_datatable_save, save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items", duplicates_allowed = TRUE, language = language))
    
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

          # If user has access
          req(paste0(table, "_datatable_card") %in% r$user_accesses)
          
          # Get value of deleted row
          row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))

          # Delete row in DB table
          delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
        })
        
        # The same for thesaurus_items / sub_datatable
        if (table == "thesaurus"){
          observeEvent(r$thesaurus_items_delete_dialog , {
            output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = "thesaurus_items", language = language))
          })
          
          # Whether to close or not delete dialog box
          observeEvent(input$thesaurus_items_hide_dialog, r$thesaurus_items_delete_dialog <- FALSE)
          observeEvent(input$thesaurus_items_delete_canceled, r$thesaurus_items_delete_dialog <- FALSE)
          observeEvent(input$thesaurus_items_deleted_pressed, r$thesaurus_items_delete_dialog <- TRUE)
          
          # When the delete is confirmed...
          observeEvent(input$thesaurus_items_delete_confirmed, {
            
            # Get value of deleted row
            row_deleted <- as.integer(substr(input$thesaurus_items_deleted_pressed, nchar("sub_delete_") + 1, nchar(input$thesaurus_items_deleted_pressed)))
            
            # Delete row in DB table
            # Link_id is ID of thesaurus which sub_datatable depends on
            # category is used to create the cache
            link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
          
            delete_settings_datatable_row(output = output, id = id, r = r, ns = ns, language = language,
              link_id = link_id, category = "delete", row_deleted = row_deleted, table = "thesaurus_items")
          })
        }
        
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      if (table %in% c("studies", "datamarts")){
          
        observeEvent(input$options, {
          # If user has access
          req(paste0(table, "_options_card") %in% r$user_accesses)
          
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
          # If user has access
          req(paste0(table, "_options_card") %in% r$user_accesses)
          
          category <- get_singular(id)
          
          data <- list()
          data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
          data$users_allowed_read <- input$users_allowed_read
          data$users_allowed_read_group <- input$users_allowed_read_group
  
          save_settings_options(output = output, r = r, id = id, category = category, code_id_input = input$options, language = language, data = data)
        })
      }
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      if (table %in% c("datamarts", "subsets", "thesaurus")){
        
        # Button "Edit code" is clicked on the datatable
        observeEvent(input$edit_code, {
          
          # If user has access
          req(paste0(table, "_edit_code_card") %in% r$user_accesses)
          
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
            code <- list()
            code$server <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code)
            
            # Render UI
            render_settings_code_card(ns = ns, r = r, id = id, title = paste0("edit_", category, "_code"), code = code, link_id = link_id, language = language)
          })
          
          # Reset code_result textOutput
          output$code_result <- renderText("")
        })
        
        # When save button is clicked
        observeEvent(input$edit_code_save,
          save_settings_code(output = output, r = r, id = id, category = get_singular(id, language),
            code_id_input = input$edit_code, edited_code = input$ace_edit_code, language = "EN"))
        
        # When Execute code button is clicked
          observeEvent(input$execute_code, {
            edited_code <- isolate(input$ace_edit_code) %>% stringr::str_replace_all("\r", "\n")
            
            output$code_result <- renderText(
              execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
                language = language, r = r, edited_code = edited_code))
          })
      }
      
      ##############################################
      # Generate sub datatable with action button  #
      ##############################################
      
      if (table == "thesaurus"){
          
        # Sub datatable is a datatable in thesaurus page, when we click on the subdatatable button of a thesaurus row
        # It opens a toggle with a datatable containing items of chosen thesaurus
        
        observeEvent(input$sub_datatable, {
  
          # If user has access
          req(paste0(table, "_sub_datatable_card") %in% r$user_accesses)
          
          # Get link id
          link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
          
          # Create a r var to export value to other observers
          r$thesaurus_link_id <- link_id
          
          # Get datamarts linked to this thesaurus
          data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
          datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)
          
          # Display sub_datatable card
          shiny.fluent::updateToggle.shinyInput(session, "sub_datatable_card_toggle", value = TRUE)
  
          # Render UI of this card
          output$sub_datatable_card <- renderUI({
            
            # Hide save button if user has no access
            save_button <- shiny.fluent::PrimaryButton.shinyInput(ns("sub_datatable_save"), translate(language, "save", words))
            if ("thesaurus_edit_data" %not_in% r$user_accesses) save_button <- ""
  
            div(id = ns("sub_datatable_card"),
              # Show current ID in the title
              make_card(tagList(translate(language, "thesaurus_items_management", words), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
                div(
                  shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 50),
                    make_dropdown(language = language, ns = ns, label = "datamart", id = "thesaurus_datamart", width = "300px",
                      options = convert_tibble_to_list(data = datamarts, key_col = "id", text_col = "name", null_value = TRUE), value = "", words = words),
                    conditionalPanel(condition = "input.datamart != ''", ns = ns,
                      div(strong(translate(language, "show_only_used_items", words), style = "display:block; padding-bottom:12px;"),
                        shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
                  ),
                  DT::DTOutput(ns("sub_datatable")),
                  save_button
                )
              )
            )
          })
          
          # Set r$thesaurus_refresh_thesaurus_items to "all_items", cause we havn't chosen yet the thesaurus or the datamart
          r$thesaurus_refresh_thesaurus_items <- paste0(link_id, "all_items")
          
        })
          
        observeEvent(input$show_only_used_items, {
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- "only_used_items"
          else r$thesaurus_refresh_thesaurus_items <- "all_items"
        })
        
        # When value of datamart changes, change value or r$thesaurus_refresh_thesaurus_items, depending on show_only_used_items
        # Add input$thesaurus_datamart in the value, to refresh even if the value doesn't change 
        # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...
        
        observeEvent(input$thesaurus_datamart, {
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "only_used_items")
          else r$thesaurus_refresh_thesaurus_items <- r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "all_items")
        })
        
        observeEvent(r$thesaurus_refresh_thesaurus_items, {
          
          req(r$thesaurus_link_id)
          
          # Get all items from the chosen thesaurus
          
          r$sub_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = r$thesaurus_link_id, category = "delete")
          
          if (length(input$thesaurus_datamart) > 0){
            if (input$thesaurus_datamart != ""){
              
              count_items_rows <- tibble::tibble()
              count_patients_rows <- tibble::tibble()
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_items_rows"),
                  error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language),
                  warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_patients_rows"),
                  error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language),
                  warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
              
              if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language)
              req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
              
              # Transform count_rows cols to integer, to be sortable
              r$sub_thesaurus_items <- r$sub_thesaurus_items %>%
                dplyr::left_join(count_items_rows, by = "item_id") %>%
                dplyr::left_join(count_patients_rows, by = "item_id") %>%
                dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
              
              # If r$thesaurus_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
              if (grepl("only_used_items", r$thesaurus_refresh_thesaurus_items)) r$sub_thesaurus_items <- r$sub_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
              
              r$sub_thesaurus_items_temp <- r$sub_thesaurus_items %>% dplyr::mutate(modified = FALSE)
            }
          }
          
          r$sub_thesaurus_items_temp <- r$sub_thesaurus_items %>% dplyr::mutate(modified = FALSE)
        })
  
        observeEvent(r$sub_thesaurus_items_temp, {
  
          # Transform item_id to character, to be filterable in datatable
          r$sub_thesaurus_items_temp <- r$sub_thesaurus_items_temp %>% dplyr::mutate_at("item_id", as.character)
  
          # Parameters for the datatable
          if ("thesaurus_delete_data" %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
          editable_cols <- c("display_name", "unit")
          searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
          factorize_cols <- c("category", "unit")
  
          # If we have count cols
          if ("count_patients_rows" %in% names(r$sub_thesaurus_items)){
            sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
            centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
            col_names <- get_col_names("thesaurus_items_with_counts", language = language)
          }
          else {
            sortable_cols <- c("id", "item_id", "name", "display_name", "category")
            centered_cols <- c("id", "item_id", "unit", "datetime", "action")
            col_names <- get_col_names("thesaurus_items", language = language)
          }
  
          # Restore datatable state
          page_length <- isolate(input$sub_datatable_state$length)
          start <- isolate(input$sub_datatable_state$start)
  
          # Render datatable
          render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "sub_datatable",
            col_names =  col_names, table = "sub_thesaurus_items", action_buttons = action_buttons,
            datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
            searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE)
        })
      }
  })
}