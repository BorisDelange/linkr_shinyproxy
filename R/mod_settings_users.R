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
  result <- ""
  
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
  
  result
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
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Add a new element                      #
    ##########################################
    
    # Update dropdowns with reactive data
    sapply(c("user_accesses", "user_statuses"), 
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
      
      new_data_var <- c("username" = "char", "first_name" = "char", "last_name" = "char", "password" = "char",
        "user_access" = "int", "user_status" = "int", "name" = "char", "description" = "char")
      
      # Transform values of textfields & dropdowns to chosen variable type
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
        })
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, 
        data = new_data, table = table, dropdowns = dropdowns)
    })
    
    # 
    # ##########################################
    # # Show or hide cards                     #
    # ##########################################
    # 
    # sapply(toggles, function(toggle){
    #   observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    # })
    # 
    # ##########################################
    # # Update dropdowns with database values  #
    # ##########################################
    # 
    # observeEvent(r$users_accesses_statuses, {
    #   dropdowns_access <- c("add_user_user_access", "accesses_management_access")
    #   sapply(dropdowns_access, function(id){
    #     options <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "access"), "id", "name", rm_deleted_rows = TRUE)
    #     shiny.fluent::updateDropdown.shinyInput(session, id, options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    #   })
    #   dropdowns_status <- c("add_user_user_status")
    #   sapply(dropdowns_status, function(id){
    #     options <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "status"), "id", "name", rm_deleted_rows = TRUE)
    #     shiny.fluent::updateDropdown.shinyInput(session, id,
    #       options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    #   })
    # })
    # 
    # ##########################################
    # # Add a new user                         #
    # ##########################################
    # 
    # observeEvent(input$add_user_add, {
    #   sapply(c("username", "firstname", "lastname", "password", "user_access", "user_status"), 
    #   function(name){
    #     new_value <- isolate(input[[paste0("add_user_", name)]])
    #     assign(paste0("new_", name), new_value, envir = parent.env(environment()))
    #     
    #     assign(paste0(name, "_check"), FALSE, envir = parent.env(environment()))
    #     
    #     if (!is.null(new_value)){
    #       if (new_value != "") assign(paste0(name, "_check"), TRUE, envir = parent.env(environment()))
    #     }
    #     
    #     if (name %not_in% c("user_access", "user_status")){
    #       if (!eval(parse(text = paste0(name, "_check")))) shiny.fluent::updateTextField.shinyInput(session, 
    #         paste0("add_user_", name), errorMessage = translate(language, paste0("provide_valid_", name)))
    #       if (eval(parse(text = paste0(name, "_check")))) shiny.fluent::updateTextField.shinyInput(session,
    #         paste0("add_user_", name), errorMessage = NULL)
    #     }
    #   })
    # 
    #   if (!user_access_check | !user_status_check){
    #     output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "need_create_user_access"), messageBarType = 3), style = "margin-top:10px;"))
    #     shinyjs::show("warnings1")
    #     shinyjs::delay(3000, shinyjs::hide("warnings1"))
    #   }
    #   
    #   req(username_check, firstname_check, lastname_check, password_check, user_access_check, user_status_check)
    #   
    #   # Check if chosen username is already used
    #   distinct_usernames <- DBI::dbGetQuery(r$db, "SELECT DISTINCT(username) FROM users WHERE deleted IS FALSE") %>% dplyr::pull()
    #   
    #   if (new_username %in% distinct_usernames){
    #     output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "username_already_used"), messageBarType = 3), style = "margin-top:10px;"))
    #     shinyjs::show("warnings2")
    #     shinyjs::delay(3000, shinyjs::hide("warnings2"))
    #   }
    #   req(new_username %not_in% distinct_usernames)
    #   
    #   last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM users") %>% dplyr::pull()
    #   
    #   # Password is hashed
    #   new_data <- tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
    #                               last_row + 1, as.character(new_username), as.character(new_firstname), as.character(new_lastname),
    #                               as.character(rlang::hash(new_password)), as.character(new_user_access), as.character(new_user_status),
    #                               as.character(Sys.time()), FALSE)
    #   
    #   DBI::dbAppendTable(r$db, "users", new_data)
    #   
    #   r$users <- DBI::dbGetQuery(r$db, "SELECT * FROM users")
    #   r$users_temp <- r$users %>% dplyr::mutate(modified = FALSE)
    #   
    #   output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_user_added"), messageBarType = 4), style = "margin-top:10px;"))
    #   shinyjs::show("warnings1")
    #   shinyjs::delay(3000, shinyjs::hide("warnings1"))
    #   
    #   # Reset textfields
    #   sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, 
    #     paste0("add_user_", name), value = ""))
    # })
    # 
    # ##########################################
    # # Add an access or a status              #
    # ##########################################
    # 
    # sapply(c("access", "status"), function(add_type) observeEvent(input[[paste0("add_", add_type, "_add")]], {
    #   
    #   new_name <- isolate(input[[paste0("add_", add_type, "_name")]])
    #   new_description <- isolate(input[[paste0("add_", add_type, "_description")]])
    #   
    #   # Check if required fields are filled (name is required, description is not)
    #   # We can add other requirements (eg characters only)
    #   name_check <- FALSE
    #   if (!is.null(new_name)){
    #     if (new_name != "") name_check <- TRUE
    #   }
    #   if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0("add_", add_type, "_name"), errorMessage = translate(language, "provide_valid_name"))
    #   if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0("add_", add_type, "_name"), errorMessage = NULL)
    #   
    #   req(name_check)
    #   
    #   # Check if chosen name is already used
    #   distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM users_accesses_statuses WHERE type = '", add_type, "'
    #                                                  AND deleted IS NOT TRUE")) %>% dplyr::pull()
    #   if (new_name %in% distinct_names){
    #     output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
    #     shinyjs::show("warnings2")
    #     shinyjs::delay(3000, shinyjs::hide("warnings2"))
    #   }
    #   req(new_name %not_in% (distinct_names))
    # 
    #   last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM users_accesses_statuses") %>% dplyr::pull()
    # 
    #   new_data <- tibble::tribble(~id, ~type, ~name, ~description, ~datetime, ~deleted,
    #                               last_row + 1, add_type, as.character(new_name), as.character(new_description), as.character(Sys.time()), FALSE)
    # 
    #   DBI::dbAppendTable(r$db, "users_accesses_statuses", new_data)
    #   
    #   r$users_accesses_statuses <- DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses_statuses")
    #   r$users_accesses_statuses_temp <- r$users_accesses_statuses %>% dplyr::mutate(modified = FALSE)
    #   
    #   output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, paste0(add_type, "_added")), messageBarType = 4), style = "margin-top:10px;"))
    #   shinyjs::show("warnings1")
    #   shinyjs::delay(3000, shinyjs::hide("warnings1"))
    #   
    #   # Reset textfields
    #   sapply(c("name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, 
    #     paste0("add_", add_type, "_", name), value = ""))
    # }))
    # 
    # ##########################################
    # # Users & statuses management            #
    # ##########################################
    # 
    #   ##########################################
    #   # Generate datatable                     #
    #   ##########################################
    #   
    #   sapply(c("users", "users_accesses_statuses"), function(data_var){
    #     observeEvent(r[[data_var]], {
    #       # Get data
    #       data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var, " WHERE deleted IS FALSE"))
    #       if (nrow(data) != 0){
    #         data <- data %>% dplyr::select(-deleted)
    #         if (data_var == "users_accesses_statuses") data <- data %>% dplyr::filter(type == "status") %>% dplyr::select(-type)
    #       }
    # 
    #       # Render datatable
    #       category <- switch(data_var, "users" = "users", "users_accesses_statuses" = "statuses")
    #       output[[paste0(category, "_management_datatable")]] <- DT::renderDT(
    #         users_management_datatable(category, data, ns, r, language,
    #           dropdowns = switch(data_var,
    #                              "users" = c("user_access_id" = "users_accesses", "user_status_id" = "users_statuses"),
    #                              "users_accesses_statuses" = "")),
    #         options = list(dom = "t<'bottom'p>",
    #                        columnDefs = switch(category,
    #                          "users" = list(
    #                             list(className = "dt-center", targets = c(0, -1, -2)),
    #                             list(sortable = FALSE, targets = c(4, 5, 6, 8))),
    #                          "statuses" = list(
    #                            list(className = "dt-center", targets = c(0, -1, -2)),
    #                            list(sortable = FALSE, targets = c(4))),
    #                          )),
    #         rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #         editable = list(target = "cell", disable = switch(category,
    #                                                             "users" = list(columns = c(0, 4, 5, 6, 7, 8)),
    #                                                             "statuses" = list(columns = c(0, 3, 4)))),
    #         callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
    #           var $this = $(this.node());
    #           $this.attr('id', this.data()[0]);
    #           $this.addClass('shiny-input-container');
    #         });
    #         Shiny.unbindAll(table.table().node());
    #         Shiny.bindAll(table.table().node());")
    #       )
    #     })
    #   })
    # 
    #   ##########################################
    #   # Save changes in datatable              #
    #   ##########################################
    #   
    #   # Each time a row is updated, modify temp variable
    #   sapply(c("users", "users_accesses_statuses"), function(data_var){
    #     category <- switch(data_var, "users" = "users", "users_accesses_statuses" = "statuses")
    #     observeEvent(input[[paste0(category, "_management_datatable_cell_edit")]], {
    #       edit_info <- input[[paste0(category, "_management_datatable_cell_edit")]]
    #       edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1
    #       r[[paste0(data_var, "_temp")]] <- DT::editData(r[[paste0(data_var, "_temp")]], edit_info)
    #       # Store that this row has been modified
    #       r[[paste0(data_var, "_temp")]][[edit_info$row, "modified"]] <- TRUE
    #     })
    #   })
    #   
    #   # Each time a dropdown is updated, modify temp variable
    #   observeEvent(r$users, {
    #     sapply(r$users %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
    #       sapply(c("user_access_id", "user_status_id"), function(dropdown){
    #         observeEvent(input[[paste0(dropdown, id)]], {
    #           r$users_temp[[which(r$users_temp["id"] == id), dropdown]] <-
    #             input[[paste0(dropdown, id)]]
    #           # Store that if this row has been modified
    #           r$users_temp[[which(r$users_temp["id"] == id), "modified"]] <- TRUE
    #         })
    #       })
    #     })
    #   })
    #   
    #   sapply(c("users", "users_accesses_statuses"), function(data_var){
    #     observeEvent(input[[paste0(data_var, "_management_save")]], {
    # 
    #       # Make sure there's no duplicate in names
    #       data <- switch(data_var, 
    #                      "users" = r[[paste0(data_var, "_temp")]] %>% dplyr::rename(name = username), 
    #                      "users_accesses_statuses" = r[[paste0(data_var, "_temp")]])
    #       duplicates <- data %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
    #         dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    #       if (duplicates > 0){
    #         output$warnings1 <- renderUI({
    #           div(shiny.fluent::MessageBar(translate(language, "modif_names_duplicates"), messageBarType = 3), style = "margin-top:10px;")
    #         })
    #         shinyjs::show("warnings1")
    #         shinyjs::delay(3000, shinyjs::hide("warnings1"))
    #       }
    #       req(duplicates == 0)
    # 
    #       # Save changes in database
    #       ids_to_del <- r[[paste0(data_var, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
    #       DBI::dbSendStatement(r$db, paste0("DELETE FROM ", data_var, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
    #       DBI::dbClearResult(query)
    #       DBI::dbAppendTable(r$db, data_var, r[[paste0(data_var, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified))
    # 
    #       # Reload r variable
    #       r[[data_var]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var))
    #       r[[paste0(data_var, "_temp")]] <- r[[data_var]] %>% dplyr::filter(deleted == FALSE) %>% dplyr::mutate(modified = FALSE)
    # 
    #       # Notification to user
    #       output$warnings2 <- renderUI({
    #         div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
    #       })
    #       shinyjs::show("warnings2")
    #       shinyjs::delay(3000, shinyjs::hide("warnings2"))
    #     })
    #   })
    #   
    #   ##########################################
    #   # Delete a row in datatable              #
    #   ##########################################
    #   
    #   # In this case, two different datatables are in the same module (different as datamarts page or plugins page)
    #   # Need to make a loop
    #   sapply(c("users", "statuses"), function(name) {
    #     # Indicate whether to close or not delete dialog box
    #     r[[paste0("users_", name, "_delete_dialog")]] <<- FALSE
    #   
    #     # Create & show dialog box 
    #     output[[paste0("users_", name, "_delete_confirm")]] <- shiny.fluent::renderReact(settings_delete_react(paste0("users_", name), ns, language, r[[paste0("users_", name, "_delete_dialog")]]))
    # 
    #     # Whether to close or not delete dialog box
    #     observeEvent(input[[paste0("users_", name, "_hide_dialog")]], r[[paste0("users_", name, "_delete_dialog")]] <<- FALSE)
    #     observeEvent(input[[paste0("users_", name, "_delete_canceled")]], r[[paste0("users_", name, "_delete_dialog")]] <<- FALSE)
    #     observeEvent(input[[paste0("users_", name, "_deleted_pressed")]], r[[paste0("users_", name, "_delete_dialog")]] <<- TRUE)
    # 
    #     # When the delete is confirmed...
    #     observeEvent(input[[paste0("users_", name, "_delete_confirmed")]], {
    # 
    #       # Close dialog box
    #       r[[paste0("users_", name, "_delete_dialog")]] <<- FALSE
    # 
    #       data_var <- switch(name, "users" = "users", "statuses" = "users_accesses_statuses")
    #       
    #       # Get the ID of row deleted
    #       deleted_pressed_value <- isolate(input[[paste0("users_", name, "_deleted_pressed")]])
    #       row_deleted <- as.integer(substr(deleted_pressed_value, nchar(paste0(name, "_delete")) + 1, nchar(deleted_pressed_value)))
    #       # Delete row in database
    #       DBI::dbSendStatement(r$db, paste0("UPDATE ", data_var, " SET deleted = TRUE WHERE id = ", row_deleted))
    #       # Update r vars (including temp variable, used in management datatables)
    #       r[[data_var]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var))
    #       r[[paste0(data_var, "_temp")]] <- r[[data_var]] %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
    # 
    #       # Notification to user
    #       message <- switch(name, "users" = "user_deleted", "statuses" = "status_deleted")
    #       output$warnings3 <- renderUI({
    #         div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
    #       })
    #       shinyjs::show("warnings3")
    #       shinyjs::delay(3000, shinyjs::hide("warnings3"))
    #     })
    #   })
    
  })
}