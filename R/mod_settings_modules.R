#' settings_modules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_modules_ui <- function(id, language){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Patient-lvl modules                    #
  ##########################################
  
  # if (id == "settings_modules_patient_lvl"){
  
    if (grepl("patient_lvl", id)) prefix <- "patient_lvl"
    if (grepl("aggregated", id)) prefix <- "aggregated"

    # Three distinct pages in the settings/modules page : module family, module & module element
    # For each "sub page", create a creation & a management cards
    # Add an option card for module family

    pages <- c("modules", "modules_families", "modules_elements")
    cards <- tagList()

    # We create one module by "sub page"

    sapply(pages, function(page){

      cards <<- tagList(cards,
        div(id = ns(paste0(page, "_creation_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_creation"), language = language)),
        div(id = ns(paste0(page, "_management_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_management"), language = language)))

      if (page == "modules_families") cards <<- tagList(cards,
        div(id = ns(paste0(page, "_options_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_options"), language = language)))
    })

    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "modules_creation_card", label = "modules_creation_card"),
        list(key = "modules_management_card", label = "modules_management_card"),
        list(key = "modules_families_creation_card", label = "modules_families_creation_card"),
        list(key = "modules_families_management_card", label = "modules_families_management_card"),
        list(key = "modules_families_options_card", label = "modules_families_options_card"),
        list(key = "modules_elements_creation_card", label = "modules_elements_creation_card"),
        list(key = "modules_elements_management_card", label = "modules_elements_management_card")
      )),
      cards
    ) -> result
  # }
  
  
  result
}

mod_settings_sub_modules_ui <- function(id, language){
  ns <- NS(id)
  
  result <- ""
  
  ##########################################
  # Patient-lvl & aggregated sub modules   #
  ##########################################
    
    if (grepl("patient_lvl", id)) page <- substr(id, nchar("settings_modules_patient_lvl_") + 1, nchar(id))
    if (grepl("aggregated", id)) page <- substr(id, nchar("settings_modules_aggregated_") + 1, nchar(id))
    
    if (page == "modules_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = c("module_family", "parent_module"), dropdowns_width = "300px") -> result
    }

    if (page == "modules_families_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module_family",
        textfields = c("name", "description"), textfields_width = "300px") -> result
    }

    if (page == "modules_elements_creation"){
      if (grepl("patient_lvl", id)){
        div(id = ns("creation_card"),
          make_card(
            title = translate(language, "add_module_element"),
            content = div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_textfield(language = language, ns = ns, label = "name", id = "name", width = "300px"),
                make_textfield(language = language, ns = ns, label = "description", id = "description", width = "300px")
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "module", id = "module", width = "300px"),
                make_dropdown(language = language, ns = ns, label = "plugin", id = "plugin", width = "300px")
              ), 
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "datamart", 
                  options = list(list(key = "", text = translate(language, "none"))), value = "", width = "300px"),
                shiny::conditionalPanel(condition = "input.datamart != ''", ns = ns,
                  div(strong(translate(language, "show_only_used_items"), style = "display:block; padding-bottom:12px;"),
                    shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "thesaurus", id = "thesaurus", width = "300px"),
                make_dropdown(language = language, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items",
                  multiSelect = TRUE, width = "600px")
              ), br(),
              DT::DTOutput(ns("thesaurus_items")), br(),
              shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
            )
          )
        ) -> result
      }
      if (grepl("aggregated", id)) {
        render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module_element",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = c("module", "plugin"), dropdowns_width = "300px") -> result
      }
    }

    if (grepl("management", page)){
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = page) -> result
    }

    if (page == "modules_families_options"){
      uiOutput(ns("options_card")) -> result
    }
  
  tagList(render_settings_default_elements(ns = ns), result) 
}

    
#' settings_modules Server Functions
#'
#' @noRd 

mod_settings_modules_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (grepl("patient_lvl", id)) prefix <- "patient_lvl"
    if (grepl("aggregated", id)) prefix <- "aggregated"
    
    # if (id == "settings_modules_patient_lvl"){
      
      toggles <- c(
        "modules_creation_card", "modules_management_card", 
        "modules_families_creation_card", "modules_families_management_card", "modules_families_options_card",
        "modules_elements_creation_card", "modules_elements_management_card")
      
      # Current page
      page <- substr(id, nchar(paste0("settings_modules_", prefix, "_")) + 1, nchar(id))
      
      # Corresponding table
      # Corresponding table in the database
      if (grepl("creation", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_creation")))
      if (grepl("management", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_management")))
      if (grepl("options", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_options")))
      
      # Dropdowns used for creation card
      dropdowns <- ""
      if (page %in% c("modules_creation", "modules_management")) dropdowns <- c("module_family", "parent_module")
      
      ##########################################
      # Show or hide cards                     #
      ##########################################
      
      # Initiate vars
      
      # Used to communicate between modules and to show / hide cards
      r[[paste0(prefix, "_modules_toggle")]] <- 0L
      r[[paste0(prefix, "_modules_families_toggle")]] <- 0L
      r[[paste0(prefix, "_modules_elements_toggle")]] <- 0L
      
      # Used to send option link_id from one module to another
      r[[paste0(prefix, "_modules_families_options")]] <- 0L
      
      # Only for main users page (not for sub-pages)
      if (id == "settings_modules_patient_lvl" | id == "settings_modules_aggregated"){ 
        
        # Depending on user_accesses
        # observeEvent(r$user_accesses, {
        #   # Hide toggles if user has no access
        #   if ("users" %not_in% r$user_accesses) shinyjs::hide("toggles") else shinyjs::show("toggles")
        # })
        
        # Depending on toggles activated
        sapply(toggles, function(toggle){
          
          # If user has no access, hide card
          # observeEvent(r$user_accesses, if (toggle %not_in% r$user_accesses) shinyjs::hide(toggle))
          
          # If user has access, show or hide card when toggle is clicked
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            # if (toggle %in% r$user_accesses){
              if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
              else shinyjs::hide(toggle)
            # }
          })
        })
        
        # When a new user, a user status or a user access is added, close add card & show data management card
        sapply(c("modules", "modules_families", "modules_elements"), function(page){
          observeEvent(r[[paste0(prefix, "_", page, "_toggle")]], {
            if (r[[paste0(prefix, "_", page, "_toggle")]] != 0){
              shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_creation_card_toggle"), value = FALSE)
              shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_management_card_toggle"), value = TRUE)}
          })
        })
        
        observeEvent(r[[paste0(prefix, "_modules_families_options")]], {
          if (r[[paste0(prefix, "_modules_families_options")]] > 0){
            shiny.fluent::updateToggle.shinyInput(session, "modules_families_options_card_toggle", value = TRUE)
          }
        })
      }
      
      ##########################################
      # Add a new element                      #
      ##########################################
      
      # Only for creation subpages
      if (grepl("creation", id)){

        # Update dropdowns with reactive data (module_family & parent_module dropdowns)
        
        observeEvent(r[[paste0(prefix, "_modules_families")]], {
          options <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules_families")]], key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, "module_family", options = options)
        })
        
        observeEvent(r[[paste0(prefix, "_modules")]], {
          options_parent_module <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules")]], key_col = "id", text_col = "name", null_value = TRUE)
          options_module <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules")]], key_col = "id", text_col = "name", null_value = TRUE)
          shiny.fluent::updateDropdown.shinyInput(session, "parent_module", options = options_parent_module)
          shiny.fluent::updateDropdown.shinyInput(session, "module", options = options_module)
        })
        
        observeEvent(r$plugins, {
          module_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
          options <- convert_tibble_to_list(data = r$plugins %>% dplyr::filter(module_type_id == !!module_type_id), 
            key_col = "id", text_col = "name", null_value = TRUE)
          shiny.fluent::updateDropdown.shinyInput(session, "plugin", options = options)
        })
        
        observeEvent(r$datamarts, {
          options <- convert_tibble_to_list(data = r$datamarts, key_col = "id", text_col = "name", null_value = TRUE)
          shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = options)
        })
        
        observeEvent(r$thesaurus, {
          options <- convert_tibble_to_list(data = r$thesaurus, key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus", options = options)
        })
        
        ##########################################
        # Add a module element                   #
        ##########################################
        
        # For "Add module element", update thesaurus items datatable when a thesaurus item is chosen
        
        observeEvent(input$thesaurus, {
          
          # Load thesaurus items depending on chosen thesaurus
          r$thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus, category = "plus_minus")
          r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
          
          # Update datamart dropdown
          shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = convert_tibble_to_list(data = r$datamarts, key_col = "id", text_col = "name", null_value = TRUE))
          
          # If we choose a datamart, add count_rows cols
          # If we change value of show_only_used_items, reload r value
          
          observeEvent(input$show_only_used_items, {
            
            observeEvent(input$datamart, {
              
              # Reload original r variables (to remove previous count_rows cols)
              r$thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus, category = "plus_minus")
              r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
              
              if (input$datamart != ""){
                
                count_items_rows <- tibble::tibble()
                count_patients_rows <- tibble::tibble()
                
                # Add count_items_rows in the cache & get it if already in the cache
                tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus,
                  datamart_id = as.integer(input$datamart), category = "count_items_rows"), 
                    error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language), 
                    warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
                
                # Add count_items_rows in the cache & get it if already in the cache
                tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus,
                  datamart_id = as.integer(input$datamart), category = "count_patients_rows"), 
                    error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language), 
                    warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
                
                if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language)
                req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
                
                # Transform count_rows cols to integer, to be sortable
                r$thesaurus_items <- r$thesaurus_items %>% 
                  dplyr::left_join(count_items_rows, by = "item_id") %>% 
                  dplyr::left_join(count_patients_rows, by = "item_id") %>%
                  dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                  dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
                
                # If show_only_used_items is TRUE, filter on count_items_rows > 0
                if (input$show_only_used_items) r$thesaurus_items <- r$thesaurus_items %>% dplyr::filter(count_items_rows > 0)
                
                r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
              }
            })
          })
          
          observeEvent(r$thesaurus_items_temp, {
            
            # Transform category to factor, to be filtering in datatable
            r$thesaurus_items_temp <- r$thesaurus_items_temp %>% dplyr::mutate_at("category", as.factor)
            
            # Parameters for the datatable
            action_buttons <- "delete"
            editable_cols <- c("display_name", "unit")
            searchable_cols <- c("name", "display_name", "category", "unit")
            factorize_cols <- c("category", "unit")
            
            # If we have count cols
            if ("count_patients_rows" %in% names(r$thesaurus_items)){
              sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
              centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
              col_names <- get_col_names("thesaurus_items_with_counts")
            }
            else {
              sortable_cols <- c("id", "item_id", "name", "display_name", "category")
              centered_cols <- c("id", "item_id", "unit", "datetime", "action")
              col_names <- get_col_names("thesaurus_items")
            }
            
            # Restore datatable state
            page_length <- isolate(input$thesaurus_items_state$length)
            start <- isolate(input$thesaurus_items_state$start)
            
            # Render datatable
            render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "thesaurus_items",
              col_names =  col_names, table = "thesaurus_items", action_buttons = action_buttons,
              datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
              editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, 
              searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE)
          })
          
          # When thesaurus item add action button is clicked
          observeEvent(input$item_selected, {
            
            link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))

            # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown
            value <- input$thesaurus_selected_items
            if (link_id %not_in% value) value <- c(value, link_id)
            options <- tibble_to_list(r$thesaurus_items %>% dplyr::filter(id %in% value), "id", "name", rm_deleted_rows = TRUE)

            shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", 
              options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
          })

          # When thesaurus item remove action button is clicked
          observeEvent(input$item_removed, {
            
            link_id <- as.integer(substr(input$item_removed, nchar("select_") + 1, nchar(input$item_removed)))

            value <- value <- input$thesaurus_selected_items
            value <- value[!value %in% link_id]
            options <- tibble_to_list(r$thesaurus_items %>% dplyr::filter(id %in% value), "id", "name", rm_deleted_rows = TRUE)

            shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
              options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
          })
          
        })
        

        ##########################################
        # When add button is clicked             #
        ##########################################
        
        # When add button is clicked
        observeEvent(input$add, {

          # If user has access
          # req(paste0(table, "_creation_card") %in% r$user_accesses)

          new_data <- list()

          new_data_var <- c("name" = "char", "description" = "char", "module_family" = "int", "parent_module" = "int")

          # Transform values of textfields & dropdowns to chosen variable type
          sapply(names(new_data_var),
            function(input_name){
              new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
            })
          
          # Add a display order, depending on last display order from the module family
          if (table %in% c("patient_lvl_modules", "aggregated_modules") & length(input$module_family) > 0){
            
            if (length(input$parent_module) == 0){
              last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id IS NULL")) %>% dplyr::pull()
            }
            else {
              if (is.na(new_data$parent_module)) last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id IS NULL")) %>% dplyr::pull()
              
              else last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id = ", new_data$parent_module)) %>% dplyr::pull()
            }
            
            new_data$display_order <- last_display + 1
          }

          # Required textfields
          required_textfields <- "name"

          # Fields requiring unique value
          req_unique_values <- "name"
          
          # Required dropdowns
          required_dropdowns <- "all"
          if (page == "modules_creation") required_dropdowns <- "module_family"
          
          add_settings_new_data(session = session, output = output, r = r, language = language, id = id,
            data = new_data, table = table, required_textfields = required_textfields, req_unique_values = req_unique_values,
            required_dropdowns = required_dropdowns,
            dropdowns = dropdowns)

          r[[paste0(table, "_toggle")]] <- r[[paste0(table, "_toggle")]] + 1
        })
      }
      
      
      ##########################################
      # Management datatable                   #
      ##########################################
        
        ##########################################
        # Generate datatable                     #
        ##########################################
        
        # Only for data management subpages
        if (grepl("management", id)){
          
          # If r$... variable changes
          observeEvent(r[[paste0(table, "_temp")]], {
            
            # If user has access
            # req(paste0(table, "_management_card") %in% r$user_accesses)
            
            # Dropdowns for each module / page
            dropdowns_datatable <- switch(table,
              "patient_lvl_modules" = c("parent_module_id" = "patient_lvl_modules"),
              "aggregated_modules" = c("parent_module_id" = "aggregated_modules"))
            
            # Action buttons for each module / page
            if (grepl("modules$", table) | grepl("modules_elements", table)) action_buttons <- "delete"
            if (grepl("modules_families", table)) action_buttons <- c("options", "delete")
            
            # Sortable cols
            sortable_cols <- c("id", "name", "description", "display_order", "datetime", "module_family_id")
            
            # Column widths
            column_widths <- c("id" = "80px", "display_order" = "80px", "datetime" = "130px", "action" = "80px")
            
            # Editable cols
            editable_cols <- c("name", "description", "display_order")
            
            # Centered columns
            centered_cols <- c("id", "module_family_id", "parent_module_id", "display_order", "datetime", "action")
            
            # Searchable_cols
            searchable_cols <- c("name", "description", "module_family_id")
            
            # Factorized cols
            factorize_cols <- character()
            if (table %in% c("patient_lvl_modules", "aggregated_modules")) factorize_cols <- c("module_family_id")
            
            # Restore datatable state
            page_length <- isolate(input$management_datatable_state$length)
            start <- isolate(input$management_datatable_state$start)
            
            render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
              col_names =  get_col_names(table), table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons,
              datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
              editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
              filter = TRUE, searchable_cols = searchable_cols, factorize_cols = factorize_cols, column_widths = column_widths)
          })
        }
        
        ##########################################
        # Save changes in datatable              #
        ##########################################
        
        # Only for data management subpages
        if (grepl("management", id)){
          
          # Each time a row is updated, modify temp variable
          observeEvent(input$management_datatable_cell_edit, {
            edit_info <- input$management_datatable_cell_edit
            r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
            # Store that this row has been modified
            r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
          })
          
          dropdowns_update <- switch(table,
            "patient_lvl_modules" = "patient_lvl_module",
            "aggregated_modules" = "aggregated_module")
          
          # Each time a dropdown is updated, modify temp variable
          if (table %in% c("patient_lvl_modules", "aggregated_modules")){
            observeEvent(r[[table]], {
              update_settings_datatable(input = input, r = r, ns = ns, table = table, dropdowns = dropdowns_update, language = language)
            })
          }
          
          # When save button is clicked
          observeEvent(input$management_save, save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language))
        }
        
        ##########################################
        # Delete a row in datatable              #
        ##########################################
        
        # Only for data management subpages
        if (grepl("management", id)){
          
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
            # req(paste0(table, "_management_card") %in% r$user_accesses)
            
            # Get value of deleted row
            row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))
            
            # Delete row in DB table
            delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
          })
        }
    
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      # Only for accesses sub-page
      # We have to use same module than management, to get input from datatable
      if (grepl("modules_families", id)){
        
        observeEvent(input$options, {
          # Show options toggle
          r[[paste0(prefix, "_modules_families_options")]] <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
        })
      }
      
      if (grepl("options", id)){
        
        observeEvent(r[[paste0(prefix, "_modules_families_options")]], {
          req(r[[paste0(prefix, "_modules_families_options")]] > 0)
          
          # If user has access
          # req(paste0(table, "_options_card") %in% r$user_accesses)
          
          
          # Render UI of options card
          output$options_card <- renderUI({
            
            # Get category & link_id to get informations in options table
            
            make_card("title", "content")
            
            category <- get_singular(word = table)
            link_id <- r[[paste0(prefix, "_modules_families_options")]]

            render_settings_options_card(ns = ns, id = id, r = r, title = paste0(get_singular(table), "_options"),
              category = category, link_id = link_id, language = language)
          })
          
          
          # When save button is clicked
          observeEvent(input$options_save, {
            
            # If user has access
            # req(paste0(table, "_options_card") %in% r$user_accesses)
            
            category <- get_singular(word = table)
            
            data <- list()
            data$users_allowed_read_group <- input$users_allowed_read_group
            data$users_allowed_read <- input$users_allowed_read
            
            save_settings_options(output = output, r = r, id = id, category = category,
              code_id_input = paste0("options_", r[[paste0(prefix, "_modules_families_options")]]), language = language, data = data)
            
          })
        })
      }
      
  
    #   ##########################################
    #   # Edit options by selecting a row        #
    #   ##########################################
    #   
    #   observeEvent(input[[paste0(prefix, "_options")]], {
    #     # Show options toggle
    #     shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_options_card_toggle"), value = TRUE)
    #     
    #     # Get module ID
    #     module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #     
    #     # r variable with selected thesaurus items
    #     r[[paste0(prefix, "_thesaurus_items_selected")]] <- list()
    #     
    #     ##########################################
    #     # Render options card                    #
    #     ##########################################
    #     
    #     output[[paste0(prefix, "_options_card")]] <- renderUI({
    #       make_card(tagList(translate(language, "module_options"), span(paste0(" (ID = ", module_id, ")"), style = "font-size: 15px;")),
    #         div(
    #           shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_module_options_action")), value = "add", options = list(
    #             list(key = "add", text = translate(language, "add_module_element")),
    #             list(key = "datatable", text = translate(language, "datatable_module_elements"))
    #           ), className = "inline_choicegroup"),
    #           
    #           # Add a module element
    #           shiny::conditionalPanel(
    #             condition = paste0("input.", prefix, "_module_options_action == 'add'"), ns = ns,
    #             div(
    #               shiny.fluent::Stack(
    #                 horizontal = TRUE, tokens = list(childrenGap = 20),
    #                 make_textfield(language, ns, label = "name", id = paste0(prefix, "_module_options_add_name"), width = "300px"),
    #                 make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_add_plugin"), 
    #                   options = tibble_to_list(r$plugins %>% dplyr::filter(!deleted, module_type == paste0(prefix, "_data")), "id", "name", rm_deleted_rows =  TRUE),
    #                   width = "300px"),
    #                 make_dropdown(language, ns, label = "thesaurus", id = paste0(prefix, "_module_options_add_thesaurus"), width = "300px",
    #                   options = tibble_to_list(r$thesaurus %>% dplyr::filter(!deleted), "id", "name", rm_deleted_rows =  TRUE))
    #               ),
    #               make_dropdown(language, ns, label = "thesaurus_items", id = paste0(prefix, "_module_options_add_thesaurus_items_selected")),
    #               div(DT::DTOutput(ns(paste0(prefix, "_module_options_add_thesaurus_items"))), style = "margin-top: 15px; margin-bottom: -5px;"),
    #               shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_add")), translate(language, "add"))
    #             )
    #           ),
    #           
    #           # Management datatable
    #           shiny::conditionalPanel(
    #             condition = paste0("input.", prefix, "_module_options_action == 'datatable'"), ns = ns,
    #             div(
    #               shiny.fluent::Stack(
    #                 horizontal = TRUE, tokens = list(childrenGap = 20),
    #                   make_dropdown(language, ns, label = "module_element", id = paste0(prefix, "_module_options_management_module_element"), width = "300px"),
    #                   make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_management_plugin"), width = "300px"),
    #                   make_dropdown(language, ns, label = "display_order", id = paste0(prefix, "_module_options_management_display_order"), width = "300px")
    #               ),
    #               div(DT::DTOutput(ns(paste0(prefix, "_module_options_management_datatable"))), style = "margin-top: 15px; margin-bottom: -5px;"),
    #               shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_save")), translate(language, "save"))
    #             )
    #           )
    #         )
    #       )
    #     })
    #   })
    #   
    #     ##########################################
    #     # Options card / 1) Add module element   #
    #     ##########################################
    #     
    #       ##########################################
    #       # Options card / load thesaurus          #
    #       ##########################################
    #     
    #       # When the thesaurus is chosen
    #       observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus")]], {
    #         r[[paste0(prefix, "_thesaurus_items")]] <- settings_modules_thesaurus_cache(r, prefix, page_id = id, thesaurus_id = input[[paste0(prefix, "_module_options_add_thesaurus")]])
    #         
    #         # Reset items dropdown
    #         shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
    #         
    #         observeEvent(r[[paste0(prefix, "_thesaurus_items")]], {
    #           
    #           # Datatable state
    #           page_length <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$length)
    #           start <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$start)
    #           
    #           output[[paste0(prefix, "_module_options_add_thesaurus_items")]] <- DT::renderDT(
    #             settings_modules_datatable_data(ns, r, type = "thesaurus_items", prefix, data = r[[paste0(prefix, "_thesaurus_items")]],
    #               new_colnames = c(translate(language, "id"), translate(language, "thesaurus_id"), translate(language, "item_id"),
    #                 translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
    #                 translate(language, "datetime"), translate(language, "action"))),
    #             options = list(
    #               dom = "<'datatable_length'l><'top'ft><'bottom'p>",
    #               stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
    #               pageLength = page_length, displayStart = start,
    #               language = list(
    #                 paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    #                 search = translate(language, "DT_search"),
    #                 lengthMenu = translate(language, "DT_length")),
    #               columnDefs = list(
    #                 list(className = "dt-center", targets = c(0, 1, 2, -1))#,
    #                 # list(sortable = FALSE, targets = c())
    #             )),
    #             rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #             editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 5, 6, 7, 8))),
    #             callback = datatable_callback()
    #           )
    #         })
    #       })
    #     
    #       # When a cell of the thesaurus datatable is edited
    #       observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]], {
    #         edit_info <- input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]]
    #         r[[paste0(prefix, "_thesaurus_items")]] <- DT::editData(r[[paste0(prefix, "_thesaurus_items")]], edit_info, rownames = FALSE)
    #       })
    #     
    #       ##########################################
    #       # Options card / add & rm thesaurus item #
    #       ##########################################
    #       
    #         # When thesaurus item add action button is clicked
    #         observeEvent(input[[paste0(prefix, "_item_selected")]], {
    #           link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_selected")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_selected")]])))
    #           
    #           value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
    #           if (link_id_filter %not_in% value) value <- c(value, link_id_filter)
    #           options <- tibble_to_list(
    #             r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
    #             # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
    #             "id", "name", rm_deleted_rows = TRUE
    #           )
    #           
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
    #             options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    #         })
    #         
    #         # When thesaurus item remove action button is clicked
    #         observeEvent(input[[paste0(prefix, "_item_removed")]], {
    #           link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_removed")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_removed")]])))
    #   
    #           value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
    #           value <- value[!value %in% link_id_filter]
    #           options <- tibble_to_list(
    #             r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
    #             # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
    #             "id", "name", rm_deleted_rows = TRUE
    #           )
    #   
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
    #             options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    #         })
    #       
    #         ##########################################
    #         # Options card / Add module element      #
    #         ##########################################
    #     
    #           observeEvent(input[[paste0(prefix, "module_options_add")]], {
    #             
    #             fields_check <- TRUE
    #             if (is.null(input[[paste0(prefix, "_module_options_add_plugin")]]) | 
    #                 is.null(input[[paste0(prefix, "_module_options_add_thesaurus")]]) | 
    #                 is.null(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]])) fields_check <- FALSE
    #             
    #             if (!fields_check) show_message_bar(output, 1, "fields_empty", "severeWarning", language)
    #             req(fields_check)
    #             
    #             module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #             new_name <- input[[paste0(prefix, "_module_options_add_name")]]
    #             name_check <- FALSE
    #             if (!is.null(new_name)){
    #               if (new_name != "") name_check <- TRUE
    #             }
    #             if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = translate(language, "provide_valid_name"))
    #             if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = NULL)
    #             
    #             req(name_check)
    #             
    #             distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", prefix, "_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
    #             
    #             if (new_name %in% distinct_names) show_message_bar(output, 3, "name_already_used", "severeWarning", language)
    #             req(new_name %not_in% distinct_names)
    #             
    #             plugin_id <- as.integer(input[[paste0(prefix, "_module_options_add_plugin")]])
    #             last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
    #             last_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
    #             last_group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
    #             new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted)
    #             
    #             sapply(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]], function(item){
    #               last_id <<- last_id + 1
    #               thesaurus_item_id <- as.integer(item)
    #               thesaurus_item <- r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id == thesaurus_item_id)
    #               # If display name is empty, take original thesaurus name
    #               thesaurus_item_display_name <- thesaurus_item %>% 
    #                 dplyr::mutate(final_name = dplyr::case_when(display_name != "" ~ display_name, TRUE ~ name)) %>%
    #                 dplyr::select(final_name) %>% dplyr::pull() %>% as.character()
    #               thesaurus_item_unit <- thesaurus_item %>% dplyr::select(unit) %>% dplyr::pull() %>% as.character()
    # 
    #               new_data <<- new_data %>% dplyr::bind_rows(
    #               tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted,
    #                   last_id, new_name, last_group_id + 1, module_id, plugin_id, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE))
    #             })
    # 
    #             DBI::dbAppendTable(r$db, "patient_lvl_module_elements", new_data)
    #             
    #             # Reset dropdowns except thesaurus
    #             shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_plugin"), value = NULL)
    #             shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
    #             
    #             show_message_bar(output, 4, "module_element_added", "success", language)
    #           })
    # 
    #       ##########################################
    #       # Options card / 2) Elements mngt        #
    #       ##########################################
    #         
    #         observeEvent(input[[paste0(prefix, "_module_options_action")]], {
    #           req(input[[paste0(prefix, "_module_options_action")]] == "datatable")
    #           
    #           module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #           r[[paste0(prefix, "_module_elements")]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE ORDER BY id"))
    #           
    #           options <- tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(name) %>% dplyr::slice(1) %>% dplyr::select(name), "name", "name")
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_module_element"),
    #             options = options,
    #             value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    #         })
    #         
    #         observeEvent(input[[paste0(prefix, "_module_options_management_module_element")]], {
    #           
    #           data <- r[[paste0(prefix, "_module_elements")]] %>% dplyr::filter(name == input[[paste0(prefix, "_module_options_management_module_element")]])
    #           
    #           # Update plugin dropdown
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_plugin"),
    #             options = tibble_to_list(r$plugins %>% dplyr::select(name, id), "id", "name"),
    #             value = data %>% dplyr::slice(1) %>% dplyr::pull(plugin_id))
    #           
    #           # Update display order dropdown
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_display_order"),
    #             options = tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(display_order) %>% dplyr::slice(1), "display_order", "display_order"),
    #             value = data %>% dplyr::slice(1) %>% dplyr::pull(display_order))
    #           
    #           # Render thesaurus items datatable
    #           output[[paste0(prefix, "_module_options_management_datatable")]] <- DT::renderDT(
    #             settings_modules_datatable_data(ns, r, type = "elements_management", prefix, data = data,
    #               new_colnames = c(translate(language, "id"), translate(language, "thesaurus_item"), translate(language, "thesaurus_item_display_name"),
    #                 translate(language, "thesaurus_item_unit"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))),
    #             options = list(
    #               dom = "<'top't><'bottom'p>",
    #               # stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
    #               # pageLength = page_length, displayStart = start,
    #               language = list(
    #                 paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    #                 search = translate(language, "DT_search"),
    #                 lengthMenu = translate(language, "DT_length")),
    #               columnDefs = list(
    #                 list(className = "dt-center", targets = c(0, 4, -1, -2)),
    #                 list(sortable = FALSE, targets = c(-1))
    #               )),
    #             rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #             editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 4, 5, 6, 7, 8))),
    #             callback = datatable_callback()
    #           )
    #         })
    #         
  })
}