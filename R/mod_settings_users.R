#' settings_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_users_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")),
      users_toggle_card(language, ns, activated = c("")),
      users_creation_card(language, ns, title = "add_user", card = "add_user", 
        textfields = c("username", "firstname", "lastname", "password"), textfields_width = "200px", 
        dropdowns = c("user_access", "user_status"), dropdowns_width = "200px"),
      users_datatable_card(language, ns, "users_management", "users_management"),
      users_creation_card(language, ns, title = "add_access", card = "add_access", 
        textfields = c("name", "description"), textfields_width = "300px"),
      users_edit_card(language, ns, "accesses_management", "accesses_management"),
      users_creation_card(language, ns, title = "add_status", card = "add_status", 
        textfields = c("name", "description"), textfields_width = "300px"),
      users_datatable_card(language, ns, "statuses_management", "statuses_management")
    ) -> result
  }
}
    
#' settings_users Server Functions
#'
#' @noRd 
mod_settings_users_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    toggles <- c("add_user_card", "users_management_card", "add_access_card", "accesses_management_card",
                 "add_status_card", "statuses_management_card")
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Update dropdowns with database values  #
    ##########################################
    
    observeEvent(r$users_accesses_statuses, {
      dropdowns_access <- c("add_user_user_access", "accesses_management_access")
      sapply(dropdowns_access, function(id){
        options <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "access"), "id", "name", rm_deleted_rows = TRUE)
        shiny.fluent::updateDropdown.shinyInput(session, id, options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
      })
      dropdowns_status <- c("add_user_user_status")
      sapply(dropdowns_status, function(id){
        options <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "status"), "id", "name", rm_deleted_rows = TRUE)
        shiny.fluent::updateDropdown.shinyInput(session, id,
          options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
      })
    })
    
    ##########################################
    # Add a new user                         #
    ##########################################
    
    observeEvent(input$add_user_add, {
      sapply(c("username", "firstname", "lastname", "password", "user_access", "user_status"), 
             function(name){
               new_value <- isolate(input[[paste0("add_user_", name)]])
               assign(paste0("new_", name), new_value, envir = parent.env(environment()))
               
               assign(paste0(name, "_check"), FALSE, envir = parent.env(environment()))
               
               if (!is.null(new_value)){
                 if (new_value != "") assign(paste0(name, "_check"), TRUE, envir = parent.env(environment()))
               }
               
               if (name %not_in% c("user_access", "user_status")){
                 if (!eval(parse(text = paste0(name, "_check")))) shiny.fluent::updateTextField.shinyInput(session, 
                   paste0("add_user_", name), errorMessage = translate(language, paste0("provide_valid_", name)))
                 if (eval(parse(text = paste0(name, "_check")))) shiny.fluent::updateTextField.shinyInput(session,
                   paste0("add_user_", name), errorMessage = NULL)
               }
             })

      if (!user_access_check | !user_status_check){
        output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "need_create_user_access"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings1")
        shinyjs::delay(3000, shinyjs::hide("warnings1"))
      }
      
      req(username_check, firstname_check, lastname_check, password_check, user_access_check, user_status_check)
      
      # Check if chosen username is already used
      distinct_usernames <- DBI::dbGetQuery(r$db, "SELECT DISTINCT(username) FROM users WHERE deleted IS FALSE") %>% dplyr::pull()
      
      if (new_username %in% distinct_usernames){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "username_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_username %not_in% distinct_usernames)
      
      last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM users") %>% dplyr::pull()
      
      # Password is hashed
      new_data <- tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access, ~user_status, ~datetime, ~deleted,
                                  last_row + 1, as.character(new_username), as.character(new_firstname), as.character(new_lastname),
                                  as.character(rlang::hash(new_password)), as.character(new_user_access), as.character(new_user_status),
                                  as.character(Sys.time()), FALSE)
      
      DBI::dbAppendTable(r$db, "users", new_data)
      
      r$users <- DBI::dbGetQuery(r$db, "SELECT * FROM users")
      r$users_temp <- DBI::dbGetQuery(r$db, "SELECT * FROM users")
      
      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_user_added"), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))
      
      # Reset textfields
      sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, 
        paste0("add_user_", name), value = ""))
    })
    
    ##########################################
    # Add an access or a status              #
    ##########################################
    
    sapply(c("access", "status"), function(add_type) observeEvent(input[[paste0("add_", add_type, "_add")]], {
      
      new_name <- isolate(input[[paste0("add_", add_type, "_name")]])
      new_description <- isolate(input[[paste0("add_", add_type, "_description")]])
      
      # Check if required fields are filled (name is required, description is not)
      # We can add other requirements (eg characters only)
      name_check <- FALSE
      if (!is.null(new_name)){
        if (new_name != "") name_check <- TRUE
      }
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0("add_", add_type, "_name"), errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0("add_", add_type, "_name"), errorMessage = NULL)
      
      req(name_check)
      
      # Check if chosen name is already used
      distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM users_accesses_statuses WHERE type = '", add_type, "'
                                                     AND deleted IS NOT TRUE")) %>% dplyr::pull()
      if (new_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_name %not_in% (distinct_names))

      last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM users_accesses_statuses") %>% dplyr::pull()

      new_data <- tibble::tribble(~id, ~type, ~name, ~description, ~datetime, ~deleted,
                                  last_row + 1, add_type, as.character(new_name), as.character(new_description), as.character(Sys.time()), FALSE)

      DBI::dbAppendTable(r$db, "users_accesses_statuses", new_data)
      
      r$users_accesses_statuses <- DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses_statuses")
      r$users_accesses_statuses_temp <- DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses_statuses")
      
      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, paste0(add_type, "_added")), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))
      
      # Reset textfields
      sapply(c("name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, 
        paste0("add_", add_type, "_", name), value = ""))
    }))
    
    ##########################################
    #               #
    ##########################################
    
  })
}
    
## To be copied in the UI
# mod_settings_users_ui("settings_users_ui_1")
    
## To be copied in the server
# mod_settings_users_server("settings_users_ui_1")
