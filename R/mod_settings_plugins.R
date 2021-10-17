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
    render_settings_default_elements(ns = ns),
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "description_card", label = "plugins_description_card"),
      list(key = "creation_card", label = "plugins_creation_card"),
      list(key = "datatable_card", label = "plugins_management_card"),
      list(key = "edit_code_card", label = "plugins_edit_code_card")
    )),
    div(
      id = ns("description_card"),
      make_card(
        translate(language, "plugins_description"),
        "")
    ),
    render_settings_creation_card(language = language, ns = ns, id = id, title = "plugins_creation",
      textfields = "name", textfields_width = "300px"),
    uiOutput(ns("edit_code_card")),
    uiOutput(ns("options_card")),
    render_settings_datatable_card(language = language, ns = ns, output = "management_datatable", title = "plugins_management")
  ) -> result
  
  result
}
    
#' settings_plugins Server Functions
#'
#' @noRd 

mod_settings_plugins_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toggles <- c("description_card", "creation_card", "datatable_card", "edit_code_card")
    prefix <- "plugins"
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Add a new plugin                       #
    ##########################################
    
    # When add button is clicked
    observeEvent(input$add, {
      
      # Create a list with new data
      new_data <- list()
      new_data_var <- c("name" = "char", "module_type" = "char")
      sapply(names(new_data_var),
        function(input_name){
         new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
        })
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, data = new_data,
        dropdowns = "module_type")
    })
    
    ##########################################
    # Plugins data management                #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
      
      # If r$... variable changes
      observeEvent(r$plugins_temp, {
        
        dropdowns <- c("module_type" = "module_type")
        
        action_buttons <- c("delete", "edit_code", "options")
        
        sortable_cols <- c("id", "name", "datetime")
        
        # Column widths
        column_widths <- c("datetime" = "130px", "action" = "80px")
        
        # Centered columns
        centered_cols <- c("id", "datetime", "action")
        
        # Restore datatable state
        page_length <- isolate(input$management_datatable_state$length)
        start <- isolate(input$management_datatable_state$start)
        
        render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id,
          col_names =  get_col_names("plugins"), table = "plugins", dropdowns = dropdowns, action_buttons = action_buttons,
          datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
          editable_cols = c("name"), sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths)
      })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
      
        # Each time a row is updated, modify temp variable
        observeEvent(input$management_datatable_cell_edit, {
          edit_info <- input$management_datatable_cell_edit
          r$plugins_temp <- DT::editData(r$plugins_temp, edit_info, rownames = FALSE)
          r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
        })
        
        # Each time a dropdown is updated, modify temp variable
        observeEvent(r$plugins, {
          update_settings_datatable(input = input, r = r, ns = ns, table = "plugins", 
            dropdowns = "module_type", language = language)
        })
        
        observeEvent(input$management_save, {
          save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language)
        })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
      # 
      # # Each time a row is updated, modify temp variable
      # observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
      #   edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]
      #   r$plugins_temp <- DT::editData(r$plugins_temp, edit_info, rownames = FALSE)
      #   # Store that this row has been modified
      #   r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
      # })
      # 
      # # Each time a dropdown is updated, modify temp variable
      # observeEvent(r$plugins, {
      #   sapply(r$plugins %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
      #     observeEvent(input[[paste0(prefix, "_module_type", id)]], {
      #       r$plugins_temp[[which(r$plugins_temp["id"] == id), "module_type"]] <-
      #         input[[paste0(prefix, "_module_type", id)]]
      #       # Store that if this row has been modified
      #       r$plugins_temp[[which(r$plugins_temp["id"] == id), "modified"]] <- TRUE
      #     })
      #   })
      # })
      # 
      # observeEvent(input[[paste0(prefix, "_management_save")]], {
      #   # Make sure there's no duplicate in names
      #   data <- r$plugins_temp
      #   duplicates <- data %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
      #     dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      #   if (duplicates > 0) show_message_bar(output, 3, "modif_names_duplicates", "severeWarning", language)
      #   req(duplicates == 0)
      #   
      #   # Save changes in database
      #   ids_to_del <- r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::pull(id)
      #   DBI::dbSendStatement(r$db, paste0("DELETE FROM plugins WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
      #   DBI::dbClearResult(query)
      #   DBI::dbAppendTable(r$db, "plugins", r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::select(-modified))
      #   
      #   # Reload r variable
      #   r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins WHERE deleted IS FALSE")
      #   r$plugins_temp <- r$plugins %>% dplyr::mutate(modified = FALSE)
      #   
      #   # Notification to user
      #   show_message_bar(output, 3, "modif_saved", "severeWarning", language)
      # })
    
      ##########################################
      # Delete a row in datatable              #
      ##########################################
      
      #  # Indicate whether to close or not delete dialog box
      r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
      
      settings_delete_row(input, output, r = r, ns = ns, language = language, prefix = prefix, data_var = "plugins", message = "plugin_deleted")
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_edit_code")]], {
        req(input[[paste0(prefix, "_edit_code")]])
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_edit_code_card_toggle"), value = TRUE)
        
        if (is.null(input[[paste0(prefix, "_edit_code_choice_ui_server")]])) choice_ui_server <- "ui"
        if (!is.null(input[[paste0(prefix, "_edit_code_choice_ui_server")]])) choice_ui_server <- input[[paste0(prefix, "_edit_code_choice_ui_server")]]
        category_filter <- paste0("plugin_", choice_ui_server)
        link_id_filter <- as.integer(substr(input[[paste0(prefix, "_edit_code")]], nchar(paste0(prefix, "_edit_code_")) + 1, nchar(input[[paste0(prefix, "_edit_code")]])))
        code <- r$code %>% dplyr::filter(category == category_filter & link_id == link_id_filter) %>% dplyr::pull(code)
        
        output[[paste0(prefix, "_edit_code_card")]] <- renderUI({
          settings_edit_code_card(language, ns, type = "code", code = code, link_id = link_id_filter, title = paste0("edit_", category_filter, "_code"), prefix = prefix)
        })
      })
      
      observeEvent(input[[paste0(prefix, "_edit_code_choice_ui_server")]], {
        category_filter <- paste0("plugin_", input[[paste0(prefix, "_edit_code_choice_ui_server")]])
        link_id_filter <- as.integer(substr(input[[paste0(prefix, "_edit_code")]], nchar(paste0(prefix, "_edit_code_")) + 1, nchar(input[[paste0(prefix, "_edit_code")]])))
        code <- r$code %>% dplyr::filter(category == category_filter & link_id == link_id_filter) %>% dplyr::pull(code)
        shinyAce::updateAceEditor(session, paste0(prefix, "_ace_edit_code"), value = code)
      })
      
      observeEvent(input[[paste0(prefix, "_edit_code_save")]], {
        if (is.null(input[[paste0(prefix, "_edit_code_choice_ui_server")]])) choice_ui_server <- "ui"
        if (!is.null(input[[paste0(prefix, "_edit_code_choice_ui_server")]])) choice_ui_server <- input[[paste0(prefix, "_edit_code_choice_ui_server")]]
        category_filter <- paste0("plugin_", choice_ui_server)
        link_id_filter <- as.integer(substr(input[[paste0(prefix, "_edit_code")]], nchar(paste0(prefix, "_edit_code_")) + 1, nchar(input[[paste0(prefix, "_edit_code")]])))
        code_id <- r$code %>% dplyr::filter(category == category_filter, link_id == link_id_filter) %>% dplyr::pull(id)
        # Replace ' with '' and store in the database
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input[[paste0(prefix, "_ace_edit_code")]], "'", "''"), "' WHERE id = ", code_id)) -> query
        DBI::dbClearResult(query)
        r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE")

        show_message_bar(output, 3, "modif_saved", "success", language)
      })

      observeEvent(input[[paste0(prefix, "_execute_code")]], {
        output[[paste0(prefix, "_code_result")]] <- renderText({
          # Change this option to display correctly tibble in textbox
          eval(parse(text = "options('cli.num_colors' = 1)"))
          # Capture console output of our code
          captured_output <- capture.output(
            tryCatch(eval(parse(text = isolate(input[[paste0(prefix, "_ace_edit_code")]]))), error = function(e) print(e), warning = function(w) print(w))
          )
          # Restore normal value
          eval(parse(text = "options('cli.num_colors' = NULL)"))
          # Display result
          paste(captured_output, collapse = "\n")
        })
      })
  })
}
    
## To be copied in the UI
# mod_settings_plugins_ui("settings_plugins_ui_1")
    
## To be copied in the server
# mod_settings_plugins_server("settings_plugins_ui_1")
