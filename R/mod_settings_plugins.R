#' settings_plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_plugins_ui <- function(id, language){
  ns <- NS(id)
  
  div(class = "main",
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "description_card", label = "plugins_description_card"),
      list(key = "creation_card", label = "plugins_creation_card"),
      list(key = "datatable_card", label = "plugins_management_card"),
      list(key = "options_card", label = "plugins_options_card"),
      list(key = "edit_code_card", label = "plugins_edit_code_card")
    )),
    render_settings_default_elements(ns = ns),
    div(
      id = ns("description_card"),
      make_card(
        translate(language, "plugins_description"),
        div(
          make_dropdown(language = language, ns = ns, label = "plugin", width = "300px"), br(),
          div(uiOutput(ns("plugin_description"),
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"))
        ))
    ),
    render_settings_creation_card(language = language, ns = ns, id = id, title = "plugins_creation",
      textfields = "name", textfields_width = "300px"),
    uiOutput(ns("edit_code_card")),
    uiOutput(ns("options_card")),
    render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card", output_id = "management_datatable", title = "plugins_management")
  ) -> result
  
  result
}
    
#' settings_plugins Server Functions
#'
#' @noRd 

mod_settings_plugins_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toggles <- c("description_card", "creation_card", "datatable_card", "options_card", "edit_code_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    # Depending on user_accesses
    # observeEvent(r$user_accesses, {
      # Hide toggles if user has no access
      # (Doesn't work anymore : we had a condition in server.R to not load server data if user has no access)
      # if ("plugins" %not_in% r$user_accesses) shinyjs::hide("toggles") else shinyjs::show("toggles")
    # })
    
    # Depending on toggles activated
    sapply(toggles, function(toggle){
      
      # If user has no access, hide card
      observeEvent(r$user_accesses, if (paste0("plugins_", toggle) %not_in% r$user_accesses) shinyjs::hide(toggle)) 
      
      # If user has access, show or hide card when toggle is clicked
      observeEvent(input[[paste0(toggle, "_toggle")]], {
        if (paste0("plugins_", toggle) %in% r$user_accesses){
          if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) 
          else shinyjs::hide(toggle)
        }
      })
    })
    
    ##########################################
    # Plugins descriptions                   #
    ##########################################
    
    # When r$plugins changes, update plugin dropdown
    observeEvent(r$plugins, {
      # If user has access
      req("plugins_description_card" %in% r$user_accesses)
      
      shiny.fluent::updateDropdown.shinyInput(session, "plugin", options = convert_tibble_to_list(data = r$plugins, key_col = "id", text_col = "name"))
    })
    
    # When a plugin is chosen, update description UI output
    observeEvent(input$plugin, {
      markdown_code <- r$options %>% dplyr::filter(category == 'plugin' & link_id == input$plugin & name == "markdown_description") %>% dplyr::pull(value)
      output$plugin_description <- renderUI(markdown(markdown_code))
    })
    
    ##########################################
    # Add a new plugin                       #
    ##########################################
    
    # When add button is clicked
    observeEvent(input$add, {
      
      # If user has access
      req("plugins_creation_card" %in% r$user_accesses)
      
      # Create a list with new data
      new_data <- list()
      new_data_var <- c("name" = "char", "module_type" = "int")
      sapply(names(new_data_var),
        function(input_name){
         new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
        })
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, data = new_data,
        table = "plugins", required_textfields = "name", req_unique_values = "name", dropdowns = "module_type")
    })
    
    ##########################################
    # Plugins data management                #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
      
      # If r$... variable changes
      observeEvent(r$plugins_temp, {
        
        # If user has access
        req("plugins_datatable_card" %in% r$user_accesses)

        dropdowns_datatable <- c("module_type_id" = "module_types")

        if ("plugins_delete_data" %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
        action_buttons <- c(action_buttons, "edit_code", "options")

        sortable_cols <- c("id", "name", "datetime")

        # Column widths
        column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")

        # Centered columns
        centered_cols <- c("id", "module_type", "datetime", "action")

        # Restore datatable state
        page_length <- isolate(input$management_datatable_state$length)
        start <- isolate(input$management_datatable_state$start)

        render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
          col_names =  get_col_names("plugins"), table = "plugins", dropdowns = dropdowns_datatable, action_buttons = action_buttons,
          datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
          editable_cols = c("name"), sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths)
      })
    
      ##########################################
      # Save changes in datatable              #
      #########################################

      # Hide save button if user has no access
      observeEvent(r$user_accesses, if ("plugins_edit_data" %not_in% r$user_accesses) shinyjs::hide("management_save"))
    
    
      # Each time a row is updated, modify temp variable
      observeEvent(input$management_datatable_cell_edit, {
        edit_info <- input$management_datatable_cell_edit
        r$plugins_temp <- DT::editData(r$plugins_temp, edit_info, rownames = FALSE)
        r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
      })

      # Each time a dropdown is updated, modify temp variable
      observeEvent(r$plugins, {
        update_settings_datatable(input = input, r = r, ns = ns, table = "plugins", dropdowns = "module_type", language = language)
      })

      observeEvent(input$management_save, {
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = "plugins", language = language)
      })

      ##########################################
      # Delete a row in datatable              #
      ##########################################
      
      # Create & show dialog box
      observeEvent(r$plugins_delete_dialog , {
        output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = "plugins", language = language))
      })
      
      # Whether to close or not delete dialog box
      observeEvent(input$hide_dialog, r$plugins_delete_dialog <- FALSE)
      observeEvent(input$delete_canceled, r$plugins_delete_dialog <- FALSE)
      observeEvent(input$deleted_pressed, r$plugins_delete_dialog <- TRUE)
      
      # When the delete is confirmed...
      observeEvent(input$delete_confirmed, {
        
        # If user has access
        req("plugins_datatable_card" %in% r$user_accesses)
        
        # Get value of deleted row
        row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))
        
        # Delete row in DB table
        delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = "plugins")
      })
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      observeEvent(input$options, {
        
        # If user has access
        req("plugins_options_card" %in% r$user_accesses)
        
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
        data$markdown_description <- as.character(input$markdown_description)
        data$users_allowed_read <- input$users_allowed_read
        data$users_allowed_read_group <- input$users_allowed_read_group
        
        save_settings_options(output = output, r = r, id = id, category = category, code_id_input = input$options, language = language, data = data)
      })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      # Button "Edit code" is clicked on the datatable
      observeEvent(input$edit_code, {
        
        # If user has access
        req("plugins_edit_code_card" %in% r$user_accesses)
        
        # Display edit_code card
        shiny.fluent::updateToggle.shinyInput(session, "edit_code_card_toggle", value = TRUE)
        
        # Get link_id to show in the card title
        link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
        
        # Get code from database
        code <- list()
        code$ui <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
        code$server <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code)
        
        # Render UI of this edit_code card
        output$edit_code_card <- renderUI({
          render_settings_code_card(ns = ns, r = r, id = id, title = paste0("edit_plugins_code"), code = code, link_id = link_id, language = language)
        })
        
        # Reset code_result textOutput
        output$code_result_ui <- renderUI("")
        output$code_result_server <- renderText("")
      })
      
      # When a datamart is chosen
      observeEvent(input$datamart, {
        
        # If user has access
        req("plugins_edit_code_card" %in% r$user_accesses)
        
        # Try to load datamart
        tryCatch(run_datamart_code(output, r, datamart_id = input$datamart, language = language),
          error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language), 
          warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
        
        # Update patient dropdown
        if (nrow(r$patients) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available"))
        
        if (nrow(r$patients) > 0){
          shiny.fluent::updateDropdown.shinyInput(session, "patient", 
            options = convert_tibble_to_list(data = r$patients %>% dplyr::mutate(name_display = paste0(patient_id, " - ", gender, " - ", age, " ", translate(language, "years"))), 
              key_col = "patient_id", text_col = "name_display"), value = NULL)}
        
        # Update also thesaurus dropdown, depending on data source
        # Reset thesaurus items dropdown
        data_source <- r$datamarts %>% dplyr::filter(id == input$datamart) %>% dplyr::pull(data_source_id) %>% as.character()
        
        # Multiple cases
        # Only one ID, so it's the beginning and the end
        # Last ID, so it's the end
        # ID between begin and last, so separated by commas
        thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
          grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id))
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name"), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_items", options = list(), value = NULL)
        
        # Reset outputs
        output$code_result_ui <- renderUI("")
        output$code_result_server <- renderText("")
        
        # Reset r$thesaurus_selected_items
        r$thesaurus_selected_items <- tibble::tribble(~thesaurus_id, ~thesaurus_name, ~item_id, ~text, ~colour)
        
      })
      
      # When a patient is chosen
      observeEvent(input$patient, {
        
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) == 0) shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL, errorMessage = translate(language, "no_stay_available"))
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) > 0){
          
          # Order stays by admission datetime
          stays <- r$stays %>% dplyr::filter(patient_id == input$patient) %>% dplyr::arrange(admission_datetime)
          
          # Load stays of the patient & update dropdown
          shiny.fluent::updateDropdown.shinyInput(session, "stay",
            options = convert_tibble_to_list(data = stays %>% dplyr::mutate(name_display = paste0(unit_name, " - ", 
              format(as.POSIXct(admission_datetime), format = "%Y-%m-%d"), " ", translate(language, "to"), " ",  format(as.POSIXct(discharge_datetime), format = "%Y-%m-%d"))),
            key_col = "stay_id", text_col = "name_display"), value = NULL)
        }
        
        # Reset outputs
        output$code_result_ui <- renderUI("")
        output$code_result_server <- renderText("")
      })
      
      # When a thesaurus is chosen
      observeEvent(input$thesaurus, {
        
        observeEvent(input$show_only_used_items, {
          
          # Get thesaurus items of current thesaurus
          thesaurus_items <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", input$thesaurus)) %>%
            dplyr::mutate(name = paste0(name, " - ", item_id)) %>%
            dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_id")
          
          # Show only items used by selected patient
          if (input$show_only_used_items){
            
            observeEvent(input$patient, {
              req(length(input$patient) > 0)
              
              # Create a variable with all items used by this patient
              patient_items <- tibble::tribble(~thesaurus_name, ~item_id)
              sapply(c("labs_vitals", "text", "orders"), function(data_var){
                data_patient <- tibble::tibble()
                if (nrow(r[[data_var]] > 0)) data_patient <- r[[data_var]] %>% dplyr::filter(patient_id == input$patient)
                if (nrow(data_patient) > 0){
                  patient_items <<- patient_items %>% dplyr::bind_rows(
                    data_patient %>% dplyr::group_by(thesaurus_name, item_id) %>%
                      dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(thesaurus_name, item_id)
                  )
                }
              })
            
              # Inner join between thesaurus_items & patient_items
              thesaurus_items <- thesaurus_items %>% dplyr::inner_join(patient_items, by = c("thesaurus_name", "item_id"))
              
              # Update dropdown with only selected thesaurus items
              shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_items", value = NULL,
                options = convert_tibble_to_list(data = thesaurus_items, key_col = "item_id", text_col = "name"))
            })
          }
          
          else {
            
            # Update dropdown with all thesaurus items
            shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_items", value = NULL,
              options = convert_tibble_to_list(data = thesaurus_items, key_col = "item_id", text_col = "name"))
          }
          
         
          # Reset r$thesaurus_selected_items
          # r$thesaurus_selected_items <- tibble::tibble(key = integer(), text = character(), colour = character())
          
          # Reset outputs
          output$code_result_ui <- renderUI("")
          output$code_result_server <- renderText("")
        })
      })
      
      # When add button is clicked
      observeEvent(input$add_thesaurus_item, {
        req(length(input$thesaurus_items$key) > 0)
        if (input$thesaurus_items$text %not_in% r$thesaurus_selected_items$text){
          
          # Get thesaurus name
          thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus) %>% dplyr::pull(name)
          
          # Add item to selected items
          r$thesaurus_selected_items <- r$thesaurus_selected_items %>% dplyr::bind_rows(
            tibble::tribble(~thesaurus_id, ~thesaurus_name, ~item_id, ~text, ~colour, 
              input$thesaurus, thesaurus_name, input$thesaurus_items$key, input$thesaurus_items$text, input$colour))}
      })
      
      # When remove button is clicked
      observeEvent(input$remove_thesaurus_item, {
        req(length(input$thesaurus_items$key) > 0)
        
        deleted_item <- tibble::tribble(~thesaurus_id, ~item_id, input$thesaurus, input$thesaurus_items$key)
        
        r$thesaurus_selected_items <- r$thesaurus_selected_items %>% dplyr::anti_join(deleted_item, by = c("thesaurus_id", "item_id"))
      })
      
      # Render result of selected items
      observeEvent(r$thesaurus_selected_items, {
        
        thesaurus_selected_items_text <- r$thesaurus_selected_items %>% dplyr::mutate(display_text = paste0(thesaurus_name, " - ", text)) %>% dplyr::pull(display_text)
        
        output$thesaurus_selected_items <- renderText(paste0(strong(translate(language, "thesaurus_selected_items")), " : ", toString(thesaurus_selected_items_text)))
        
        # Reset outputs
        output$code_result_ui <- renderUI("")
        output$code_result_server <- renderText("")
      })
      
      # When reset button is clicked
      observeEvent(input$reset_thesaurus_items, {
        # Reset r$thesaurus_selected_items
        r$thesaurus_selected_items <- tibble::tribble(~thesaurus_id, ~thesaurus_name, ~item_id, ~text, ~colour)
      })
      
      # When save button is clicked
      observeEvent(input$edit_code_save, {
        
        # There are two shinyAce editors, one for UI & one for server
        # Save two at once
        
        # UI
        save_settings_code(output = output, r = r, id = id, category = "plugin_ui",
          code_id_input = input$edit_code, edited_code = input$ace_edit_code_ui, language = "EN")
        
        # Server
        save_settings_code(output = output, r = r, id = id, category = "plugin_server",
          code_id_input = input$edit_code, edited_code = input$ace_edit_code_server, language = "EN")
      })
      
      # When Execute code button is clicked
      observeEvent(input$execute_code, {
        
        # If user has access
        req("plugins_edit_code_card" %in% r$user_accesses)
        
        if (length(input$datamart) == 0 | length(input$patient) == 0 | length(input$stay) == 0 | 
            length(input$thesaurus) == 0) show_message_bar(output, 2, "dropdown_empty", "severeWarning", language)
        else if (nrow(r$thesaurus_selected_items) == 0) show_message_bar(output, 3, "thesaurus_items_empty", "severeWarning", language)

        req(input$datamart, input$patient, input$stay, input$thesaurus)
        req(nrow(r$thesaurus_selected_items) > 0)
        
        # Create variables that will be available in patient-lvl data & aggregated data pages
        # For patient-lvl data page, we have these variables : labs_vitals, text & orders (for all stays of the patient)
        # and also these variables for current stay : labs_vitals_stay, text_stay & orders_stay
        # Filter on selected thesaurus items
        # Don't forget to convert item_id & patient_id to integers, may have been transformed to numeric before
        
        # Join with thesaurus items to get items names
        # First, get thesaurus attached to this datamart
        data_source <- r$datamarts %>% dplyr::filter(id == input$datamart) %>% dplyr::pull(data_source_id) %>% as.character()
        thesaurus_ids <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
          grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>%
          dplyr::pull(id)
        
        r$thesaurus_items <- DBI::dbGetQuery(r$db, paste0("SELECT thesaurus_id, item_id, display_name, unit AS unit_thesaurus FROM thesaurus_items ",
        "WHERE thesaurus_id IN ( ", paste(thesaurus_ids, collapse = ","), ") AND deleted IS FALSE"))
        
        thesaurus_selected_items <- r$thesaurus_selected_items %>% dplyr::left_join(r$thesaurus_items, by = c("thesaurus_id", "item_id"))
        
        # Initialize variables

        data <- list()
        data$labs_vitals <- tibble::tibble()
        data$text <- tibble::tibble()
        data$orders <- tibble::tibble()

        # Filter r variables with selected thesaurus items and with patient_id
        # Choose unit, priority to data, then thesaurus
        if (nrow(r$labs_vitals) > 0){
          data$labs_vitals <-
            r$labs_vitals %>%
            dplyr::filter(patient_id == input$patient) %>%
            dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) #%>%
            #dplyr::mutate(unit = dplyr::case_when(unit != "" ~ unit, TRUE ~ unit_thesaurus)) %>% dplyr::select(-unit_thesaurus)
        }
        if (nrow(r$text) > 0){
          data$text <-
            r$text %>%
            dplyr::filter(patient_id == input$patient) %>%
            dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
        }
        if (nrow(r$orders) > 0){
          data$orders <-
            r$orders %>%
            dplyr::filter(patient_id == input$patient) %>%
            dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
        }

        data$stay <- r$stays %>% dplyr::filter(stay_id == input$stay) %>% dplyr::select(admission_datetime, discharge_datetime)

        if (nrow(data$labs_vitals) > 0) data$labs_vitals_stay <- data$labs_vitals %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)
        if (nrow(data$text) > 0) data$text_stay <- data$text %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)
        if (nrow(data$orders) > 0) data$orders_stay <- data$orders %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)

        # Get link_id variable to get the code from database
        link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))

        # if current edited code is UI, load UI from shinyAce editor & server from database
        # if current edited code is server, load server ace editor & UI from database
        if (input$edit_code_ui_server == "ui"){
          ui_code <- isolate(input$ace_edit_code_ui)
          server_code <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code)
        }
        if (input$edit_code_ui_server == "server"){
          server_code <- isolate(input$ace_edit_code_server)
          ui_code <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
        }
        
        # Replace %group_id% in ui_code with 1 for our example
        ui_code <- ui_code %>% stringr::str_replace_all("%group_id%", "1") %>% stringr::str_replace_all("\r", "\n")
        server_code <- server_code %>% stringr::str_replace_all("%group_id%", "1") %>% stringr::str_replace_all("\r", "\n")

        # Render UI result
        output$code_result_ui <- renderUI(
          make_card("", 
            execute_settings_code(input = input, output = output, session = session,
            id = id, ns = ns, language = language, r = r, edited_code = ui_code, code_type = "ui", data = data)))
        
        # Warnings messages (render server result)
        output$code_result_server <- renderText(execute_settings_code(input = input, output = output, session = session,
          id = id, ns= ns, language = language, r = r, edited_code = server_code, code_type = "server", data = data))
      })
  })
}