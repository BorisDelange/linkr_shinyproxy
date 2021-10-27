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
    
    if (grepl("patient_lvl", id)) page <- substr(id, nchar("settings_patient_lvl_modules_") + 1, nchar(id))
    if (grepl("aggregated", id)) page <- substr(id, nchar("settings_aggregated_modules_") + 1, nchar(id))
    
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
      if (id == "settings_patient_lvl_modules" | id == "settings_aggregated_modules"){ 
        
        # Depending on toggles activated
        sapply(toggles, function(toggle){
          
          # If user has no access, hide card
          observeEvent(r$user_accesses,
            if ((paste0(prefix, "_modules_creation_card") %not_in% r$user_accesses & grepl("creation_card", toggle))
               | (paste0(prefix, "_modules_management_card") %not_in% r$user_accesses & grepl("management_card", toggle))
               | (paste0(prefix, "_modules_options_card") %not_in% r$user_accesses & grepl("options_card", toggle))) shinyjs::hide(toggle))
          
          # If user has access, show or hide card when toggle is clicked
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            # if (toggle %in% r$user_accesses){
              if ((paste0(prefix, "_modules_creation_card") %in% r$user_accesses & grepl("creation_card", toggle))
                | (paste0(prefix, "_modules_management_card") %in% r$user_accesses & grepl("management_card", toggle))
                | (paste0(prefix, "_modules_options_card") %in% r$user_accesses & grepl("options_card", toggle))) {
                if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
                else shinyjs::hide(toggle)
              }
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
      
      ###########################################################
      # Add a new module / module family / module element       #
      ###########################################################
      
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
          
          # Filter on plugins user has access to
          data <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)
          
          if ("plugins_see_all_data" %not_in% r$user_accesses & nrow(data) > 0) data <- get_authorized_data(r = r, table = "plugins", data = data)
          
          options <- convert_tibble_to_list(data = data, key_col = "id", text_col = "name")
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
          
          if ("datamarts_see_all_data" %in% r$user_accesses) datamarts <- r$datamarts
          else datamarts <- get_authorized_data(r = r, table = "datamarts")
          
          # Update datamart dropdown
          shiny.fluent::updateDropdown.shinyInput(session, "datamart", 
            options = convert_tibble_to_list(data = datamarts, key_col = "id", text_col = "name", null_value = TRUE), value = "")
          
          # Set r$modules_refresh_thesaurus_items to "all_items", cause we havn't chosen yet the thesaurus or the datamart
          r$modules_refresh_thesaurus_items <- paste0(input$thesaurus, "all_items")
          
        })
        
        observeEvent(input$show_only_used_items, {
          if (input$show_only_used_items) r$modules_refresh_thesaurus_items <- "only_used_items"
          else r$modules_refresh_thesaurus_items <- "all_items"
        })
        
        # When value of datamart changes, change value or r$modules_refresh_thesaurus_items, depending on show_only_used_items
        # Add input$datamart in the value, to refresh even if the value doesn't change 
        # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...
        
        observeEvent(input$datamart, {
          if (input$show_only_used_items) r$modules_refresh_thesaurus_items <- paste0(input$datamart, "only_used_items")
          else r$modules_refresh_thesaurus_items <- r$modules_refresh_thesaurus_items <- paste0(input$datamart, "all_items")
        })
        
        observeEvent(r$modules_refresh_thesaurus_items, {
          
          req(input$thesaurus)
          
          # Get all items from the chosen thesaurus
          
          r$modules_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus, category = "plus_minus")
          
          if (length(input$datamart) > 0){
            if (input$datamart != ""){
              
              count_items_rows <- tibble::tibble()
              count_patients_rows <- tibble::tibble()
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$datamart), category = "count_items_rows"),
                  error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language),
                  warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$datamart), category = "count_patients_rows"),
                  error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language),
                  warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
              
              if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language)
              req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
              
              # Transform count_rows cols to integer, to be sortable
              r$modules_thesaurus_items <- r$modules_thesaurus_items %>%
                dplyr::left_join(count_items_rows, by = "item_id") %>%
                dplyr::left_join(count_patients_rows, by = "item_id") %>%
                dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
              
              # If r$modules_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
              if (grepl("only_used_items", r$modules_refresh_thesaurus_items)) r$modules_thesaurus_items <- r$modules_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
              
              r$modules_thesaurus_items_temp <- r$modules_thesaurus_items %>% dplyr::mutate(modified = FALSE)
            }
          }
          
          r$modules_thesaurus_items_temp <- r$modules_thesaurus_items %>% dplyr::mutate(modified = FALSE)
        })
          
        observeEvent(r$modules_thesaurus_items_temp, {
          
          # Transform category to factor, to be filtering in datatable
          r$modules_thesaurus_items_temp <- r$modules_thesaurus_items_temp %>% dplyr::mutate_at("category", as.factor)
          
          # Parameters for the datatable
          action_buttons <- ""
          if (paste0(prefix, "modules_delete_data") %in% r$user_accesses) action_buttons <- "delete"
          
          
          editable_cols <- c("display_name", "unit")
          searchable_cols <- c("name", "display_name", "category", "unit")
          factorize_cols <- c("category", "unit")
          column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "name" = "300px", "display_name" = "150px",
            "unit" = "100px")
          
          # If we have count cols
          if ("count_patients_rows" %in% names(r$modules_thesaurus_items)){
            sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
            centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
            col_names <- get_col_names(table_name = "modules_thesaurus_items_with_counts", language = language)
          }
          else {
            sortable_cols <- c("id", "item_id", "name", "display_name", "category")
            centered_cols <- c("id", "item_id", "unit", "datetime", "action")
            col_names <- get_col_names(table_name = "modules_thesaurus_items", language = language)
          }
          
          # Restore datatable state
          page_length <- isolate(input$thesaurus_items_state$length)
          start <- isolate(input$thesaurus_items_state$start)
          
          # Render datatable
          render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "thesaurus_items",
            col_names =  col_names, table = "modules_thesaurus_items", action_buttons = action_buttons,
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
          options <- tibble_to_list(r$modules_thesaurus_items %>% dplyr::filter(id %in% value), "id", "name", rm_deleted_rows = TRUE)

          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", 
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        })

        # When thesaurus item remove action button is clicked
        observeEvent(input$item_removed, {
          
          link_id <- as.integer(substr(input$item_removed, nchar("select_") + 1, nchar(input$item_removed)))

          value <- value <- input$thesaurus_selected_items
          value <- value[!value %in% link_id]
          options <- tibble_to_list(r$modules_thesaurus_items %>% dplyr::filter(id %in% value), "id", "name", rm_deleted_rows = TRUE)

          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        })
        

        ##########################################
        # When add button is clicked             #
        ##########################################
        
        # When add button is clicked
        observeEvent(input$add, {

          # If user has access
          req(paste0(prefix, "_modules_creation_card") %in% r$user_accesses)

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
            req(paste0(prefix, "_modules_management_card") %in% r$user_accesses)
            
            # Dropdowns for each module / page
            dropdowns_datatable <- switch(table,
              "patient_lvl_modules" = c("parent_module_id" = "patient_lvl_modules"),
              "aggregated_modules" = c("parent_module_id" = "aggregated_modules"))
            
            # Action buttons for each module / page
            action_buttons <- ""
            if (paste0(prefix, "_modules_delete_data") %in% r$user_accesses) action_buttons <- "delete"
            
            # if (grepl("modules$", table) | grepl("modules_elements", table)) action_buttons <- "delete"
            if (grepl("modules_families", table)) action_buttons <- c("options", action_buttons)
            
            # Sortable cols
            sortable_cols <- c("id", "name", "description", "display_order", "datetime", "module_family_id")
            
            # Column widths
            column_widths <- c("id" = "80px", "display_order" = "80px", "datetime" = "130px", "action" = "80px")
            
            # Editable cols
            editable_cols <- c("name", "description", "display_order")
            
            # Centered columns
            centered_cols <- c("id", "parent_module_id", "display_order", "datetime", "action")
            
            # Searchable_cols
            searchable_cols <- c("name", "description", "module_family_id")
            
            # Factorized cols
            factorize_cols <- character()
            if (table %in% c("patient_lvl_modules", "aggregated_modules")) factorize_cols <- c("module_family_id")
            
            # Restore datatable state
            page_length <- isolate(input$management_datatable_state$length)
            start <- isolate(input$management_datatable_state$start)
            
            render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
              col_names =  get_col_names(table_name = table, language = language), table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons,
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
          
          # Hide save button if user has no access
          observeEvent(r$user_accesses, if (paste0(prefix, "_modules_edit_data") %not_in% r$user_accesses) shinyjs::hide("management_save"))
          
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
            req(paste0(prefix, "_modules_management_card") %in% r$user_accesses)
            
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
          req(paste0(prefix, "_modules_options_card") %in% r$user_accesses)
          
          
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
            req(paste0(prefix, "_modules_options_card") %in% r$user_accesses)
            
            category <- get_singular(word = table)
            
            data <- list()
            data$users_allowed_read_group <- input$users_allowed_read_group
            data$users_allowed_read <- input$users_allowed_read
            
            save_settings_options(output = output, r = r, id = id, category = category,
              code_id_input = paste0("options_", r[[paste0(prefix, "_modules_families_options")]]), language = language, data = data)
            
          })
        })
      }  
  })
}