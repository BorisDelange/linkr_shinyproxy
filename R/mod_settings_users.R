#' settings_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_users_ui <- function(id, language){
  ns <- NS(id)
  
  # Three distinct pages in the settings/users page : users, accesses & statuses
  # For each "sub page", create a creation & a management cards
  
  pages <- c("users", "accesses", "statuses")
  cards <- tagList()
  
  # We create one module by "sub page"
  
  sapply(pages, function(page){
    cards <<- tagList(cards,
      div(id = ns(paste0(page, "_creation_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_creation"), language = language)),
      div(id = ns(paste0(page, "_management_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_management"), language = language)))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "users_creation_card", label = "users_creation_card"),
      list(key = "users_management_card", label = "users_management_card"),
      list(key = "accesses_creation_card", label = "accesses_creation_card"),
      list(key = "accesses_management_card", label = "accesses_management_card"),
      list(key = "statuses_creation_card", label = "statuses_creation_card"),
      list(key = "statuses_management_card", label = "statuses_management_card")
    )),
    cards
  )
}

mod_settings_sub_users_ui <- function(id, language){
  ns <- NS(id)
  
  page <- substr(id, nchar("settings_users_") + 1, nchar(id))
  
  if (page == "users_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_user",
      textfields = c("username", "firstname", "lastname", "password"), textfields_width = "200px",
      dropdowns = c("user_access", "user_status"), dropdowns_width = "200px") -> result
  }
  
  if (page == "accesses_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_access",
      textfields = c("name", "description"), textfields_width = "300px") -> result
  }
  
  if (page == "statuses_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_status",
      textfields = c("name", "description"), textfields_width = "300px") -> result
  }
  
  if (grepl("management", page)){
    render_settings_datatable_card(language = language, ns = ns, output_id = page, title = page) -> result
  }
  
  tagList(render_settings_default_elements(ns = ns), result)
}
    
#' settings_users Server Functions
#'
#' @noRd 
mod_settings_users_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    toggles <- c(
      "users_creation_card", "users_management_card", 
      "accesses_creation_card", "accesses_management_card",
      "statuses_creation_card", "statuses_management_card")
    
    # Current page
    page <- substr(id, nchar("settings_users_") + 1, nchar(id))
    
    # Corresponding table in the database
    table <- switch(page, "users_creation" = "users", "accesses_creation" = "users_accesses", "statuses_creation" = "users_statuses")
    
    # Dropdowns used for creation card
    dropdowns <- ""
    if (page == "users_creation") dropdowns <- c("user_access", "user_status")
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    # Only for main users page (not for sub-pages)
    if (id == "settings_users"){
      sapply(toggles, function(toggle){
        observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
      })
    }
    
    ##########################################
    # Add a new element                      #
    ##########################################
    
    # Only for subpages (not for main users page)
    if (id != "settings_users")
    
    # Update dropdowns with reactive data
    sapply(c("users_accesses", "users_statuses"), 
      function(data_var){
        observeEvent(r[[data_var]], {
          # Convert options to list
          options <- convert_tibble_to_list(data = r[[data_var]], key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
        })
      })
    
    # When add button is clicked
    observeEvent(input$add, {
      
      new_data <- list()
      
      new_data_var <- c("username" = "char", "firstname" = "char", "lastname" = "char", "password" = "char",
        "user_access" = "int", "user_status" = "int", "name" = "char", "description" = "char")
      
      # Transform values of textfields & dropdowns to chosen variable type
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
        })
      
      # Required textfields
      required_textfields <- switch(table, 
        "users" = c("username", "firstname", "lastname", "password"),
        "users_accesses" = "name",
        "users_statuses" = "name")
      
      # Fields requiring unique value
      req_unique_values <- switch(table, "users" = "username", "users_accesses" = "name", "users_statuses" = "name")
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, 
        data = new_data, table = table, required_textfields = required_textfields, req_unique_values = req_unique_values, dropdowns = dropdowns)
    })
    
    ##########################################
    # Management datatable                   #
    ##########################################
      
      ##########################################
      # Generate datatable                     #
      ##########################################
      
      # If r$... variable changes
      observeEvent(r[[paste0(table, "_temp")]], {

        # Dropdowns for each module / page
        dropdowns <- switch(table, "users" = c("user_access_id" = "users_accesses", "user_status_id" = "users_statuses"),
          "users_accesses" = "", "users_statuses" = "")

        # Action buttons for each module / page
        action_buttons = switch(table, "users" = "delete", "users_accesses" = c("options", "delete"), "users_statuses" = "delete")

        # Sortable cols
        sortable_cols <- c("id", "name", "description", "username", "firstname", "lastname", "datetime")

        # Column widths
        column_widths <- c("datetime" = "130px", "action" = "80px")

        # Editable cols
        editable_cols <- switch(table, "users" = c("username", "firstname", "lastname"),
          "users_accesses" = c("name", "description"), "users_statuses" = c("name", "description"))

        # Centered columns
        centered_cols <- c("id", "user_access_id", "user_status_id", "datetime", "action")

        # Searchable_cols
        searchable_cols <- c("name", "description", "username", "firstname", "lastname")

        # Restore datatable state
        page_length <- isolate(input$management_datatable_state$length)
        start <- isolate(input$management_datatable_state$start)

        render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
          col_names =  get_col_names(table), table = table, dropdowns = dropdowns, action_buttons = action_buttons,
          datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
          filter = TRUE, searchable_cols = searchable_cols, column_widths = column_widths)
      })
      
    ##########################################
    # Save changes in datatable              #
    ##########################################
    
    # # Each time a row is updated, modify temp variable
    # # Do that for main datatable (management_datatable) & sub_datatable
    # observeEvent(input$management_datatable_cell_edit, {
    #   edit_info <- input$management_datatable_cell_edit
    #   r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
    #   # Store that this row has been modified
    #   r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
    # })
    # 
    # observeEvent(input$sub_datatable_cell_edit, {
    #   edit_info <- input$sub_datatable_cell_edit
    #   edit_info$col <- edit_info$col + 2 # We have removed id & thesaurus_id cols, so need to add two to col index
    #   r$thesaurus_items_temp <- DT::editData(r$thesaurus_items_temp, edit_info, rownames = FALSE)
    #   r$thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
    # })
    # 
    # # Each time a dropdown is updated, modify temp variable
    # observeEvent(r[[table]], {
    #   update_settings_datatable(input = input, r = r, ns = ns, table = table, 
    #                             dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), language = language)
    # })
    # 
    # # When save button is clicked
    # # Do that for main datatable (management_datatable) & sub_datatable
    # observeEvent(input$management_save, save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language))
    # observeEvent(input$sub_datatable_save, save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items", duplicates_allowed = TRUE, language = language))
    
    ##########################################
    # Delete a row in datatable              #
    ##########################################
    
    # # Create & show dialog box
    # observeEvent(r[[paste0(table, "_delete_dialog")]] , {
    #   output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = table, language = language))
    # })
    # 
    # # Whether to close or not delete dialog box
    # observeEvent(input$hide_dialog, r[[paste0(table, "_delete_dialog")]] <- FALSE)
    # observeEvent(input$delete_canceled, r[[paste0(table, "_delete_dialog")]] <- FALSE)
    # observeEvent(input$deleted_pressed, r[[paste0(table, "_delete_dialog")]] <- TRUE)
    # 
    # # When the delete is confirmed...
    # observeEvent(input$delete_confirmed, {
    #   
    #   # Get value of deleted row
    #   row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))
    #   
    #   # Delete row in DB table
    #   delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
    # })
    # 
    # # The same for thesaurus_items / sub_datatable
    # if (table == "thesaurus"){
    #   observeEvent(r$thesaurus_items_delete_dialog , {
    #     output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = "thesaurus_items", language = language))
    #   })
    #   
    #   # Whether to close or not delete dialog box
    #   observeEvent(input$thesaurus_items_hide_dialog, r$thesaurus_items_delete_dialog <- FALSE)
    #   observeEvent(input$thesaurus_items_delete_canceled, r$thesaurus_items_delete_dialog <- FALSE)
    #   observeEvent(input$thesaurus_items_deleted_pressed, r$thesaurus_items_delete_dialog <- TRUE)
    #   
    #   # When the delete is confirmed...
    #   observeEvent(input$thesaurus_items_delete_confirmed, {
    #     
    #     # Get value of deleted row
    #     row_deleted <- as.integer(substr(input$thesaurus_items_deleted_pressed, nchar("sub_delete_") + 1, nchar(input$thesaurus_items_deleted_pressed)))
    #     
    #     # Delete row in DB table
    #     # Link_id is ID of thesaurus which sub_datatable depends on
    #     # category is used to create the cache
    #     link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
    #     
    #     delete_settings_datatable_row(output = output, id = id, r = r, ns = ns, language = language,
    #                                   link_id = link_id, category = "delete", row_deleted = row_deleted, table = "thesaurus_items")
    #   })
    # }
    
    ##########################################
    # Edit options by selecting a row        #
    ##########################################
    # 
    # observeEvent(input$options, {
    #   # Show options toggle
    #   shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = TRUE)
    #   
    #   # Render UI of options card
    #   output$options_card <- renderUI({
    #     # Get category & link_id to get informations in options table
    #     category <- get_singular(word = id)
    #     link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
    #     
    #     render_settings_options_card(ns = ns, id = id, r = r, title = paste0(get_singular(id), "_options"), 
    #                                  category = category, link_id = link_id, language = language)
    #   })
    # })
    # 
    # observeEvent(input$options_save, {
    #   category <- get_singular(id)
    #   
    #   data <- list()
    #   data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
    #   data$users_allowed_read <- input$users_allowed_read
    #   
    #   save_settings_options(output = output, r = r, id = id, category = category,
    #                         code_id_input = input$options, language = language, data = data)
    # })
    
  })
}