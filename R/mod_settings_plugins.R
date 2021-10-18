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
      list(key = "options_card", label = "plugins_options_card"),
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
 
    toggles <- c("description_card", "creation_card", "datatable_card", "options_card", "edit_code_card")
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
      new_data_var <- c("name" = "char", "module_type" = "int")
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

        dropdowns <- c("module_type_id" = "module_types")

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
      #########################################

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
        
        # Get value of deleted row
        row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))
        
        # Delete row in DB table
        delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = "plugins")
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
        data$markdown_description <- as.character(input$markdown_description)
        data$visibility <- as.character(input$visibility)
        
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
      
      # When save button is clicked
      observeEvent(input$edit_code_save, {
        
        # There are two shinyAce editors, one for UI & one for server
        save_settings_code(output = output, r = r, id = id, category = paste0("plugin_", input$edit_code_ui_server),
          code_id_input = input$edit_code, edited_code = input[[paste0("ace_edit_code_", input$edit_code_ui_server)]], language = "EN")
      })
      
      # When Execute code button is clicked
      observeEvent(input$execute_code, {
        
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
        
        # Render result of executed code
        output$code_result_ui <- renderUI(execute_settings_code(input = input, output = output, session = session, 
          id = id, ns = ns, language = language, r = r, edited_code = ui_code, code_type = "ui"))
        output$code_result_server <- renderText(execute_settings_code(input = input, output = output, session = session, 
          id = id, ns= ns, language = language, r = r, edited_code = server_code, code_type = "server"))
      })
  })
}