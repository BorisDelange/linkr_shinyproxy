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
    
    if (page == "accesses") cards <<- tagList(cards,
      div(id = ns(paste0(page, "_options_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_options"), language = language)))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "users_creation_card", label = "users_creation_card"),
      list(key = "users_management_card", label = "users_management_card"),
      list(key = "accesses_creation_card", label = "accesses_creation_card"),
      list(key = "accesses_management_card", label = "accesses_management_card"),
      list(key = "accesses_options_card", label = "accesses_options_card"),
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
    render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = page) -> result
  }
  
  if (page == "accesses_options"){
    div(uiOutput(ns("options_card")), br(), tableOutput(ns("test"))) -> result
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
      "accesses_creation_card", "accesses_management_card", "accesses_options_card",
      "statuses_creation_card", "statuses_management_card")
    
    # Current page
    page <- substr(id, nchar("settings_users_") + 1, nchar(id))
    
    # Corresponding table in the database
    table <- switch(page, "users_creation" = "users", "users_management" = "users",
      "accesses_creation" = "users_accesses", "accesses_management" = "users_accesses",
      "statuses_creation" = "users_statuses", "statuses_management" = "users_statuses")
    
    # Dropdowns used for creation card
    dropdowns <- ""
    if (page == "users_creation") dropdowns <- c("user_access", "user_status")
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    # Initiate var
    r$users_statuses_options <- 0L
    r$users_toggle <- 0L
    r$accesses_toggle <- 0L
    r$statuses_toggle <- 0L
    
    # Only for main users page (not for sub-pages)
    if (id == "settings_users"){
      sapply(toggles, function(toggle){
        observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
      })
      
      # When a new user, a user status or a user access is added, close add card & show data management card
      sapply(c("users", "accesses", "statuses"), function(page){
        observeEvent(r[[paste0(page, "_toggle")]], {
          if (r[[paste0(page, "_toggle")]] != 0){
            shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_creation_card_toggle"), value = FALSE)
            shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_management_card_toggle"), value = TRUE)}
        })
      })
      
      observeEvent(r$users_statuses_options, {
        if (r$users_statuses_options > 0){
          shiny.fluent::updateToggle.shinyInput(session, "accesses_options_card_toggle", value = TRUE)
        }
      })
    }
    
    ##########################################
    # Add a new element                      #
    ##########################################
    
    # Only for creation subpages
    if (grepl("creation", id)){
      
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
        
        r_toggle <- switch(table, "users" = "users_toggle", "users_accesses" = "accesses_toggle", "users_statuses" = "statuses_toggle")
        r[[r_toggle]] <- r[[r_toggle]] + 1
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
  
          # Dropdowns for each module / page
          dropdowns <- switch(table, "users" = c("user_access_id" = "users_accesses", "user_status_id" = "users_statuses"),
            "users_accesses" = "", "users_statuses" = "")
  
          # Action buttons for each module / page
          action_buttons = switch(table, "users" = "delete", "users_accesses" = c("options", "delete"), "users_statuses" = "delete")
  
          # Sortable cols
          sortable_cols <- c("id", "name", "description", "username", "firstname", "lastname", "datetime")
  
          # Column widths
          column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
  
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
      
      # Each time a dropdown is updated, modify temp variable
      if (table == "users"){
        observeEvent(r[[table]], {
          update_settings_datatable(input = input, r = r, ns = ns, table = table, dropdowns = dropdowns, language = language)
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
    if (id == "settings_users_accesses_management"){
    
      observeEvent(input$options, {
        # Show options toggle
        r$users_statuses_options <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

      })
    }
    
    if (id == "settings_users_accesses_options"){
      observeEvent(r$users_statuses_options, {
        req(r$users_statuses_options > 0)

        options_toggles <- tibble::tribble(
          ~name, ~toggles,
          "app_db", c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card", "db_restore_card"),
          "users", c("users_creation_card", "users_management_card",
             "accesses_creation_card", "accesses_management_card", "accesses_options_card",
             "statuses_creation_card", "statuses_management_card"),
          "r_console", "",
          "data_sources", c("create_data_source", "data_sources_management"),
          "datamarts", c("create_datamart", "datamarts_management", "datamart_options", "edit_datamart_code"),
          "studies", c("create_study", "studies_management", "study_options"),
          "subsets", c("create_subset", "subsets_management", "edit_subset_code"),
          "thesaurus", c("create_thesaurus", "thesaurus_management_card", "thesaurus_items_management_card", "edit_thesaurus_code"),
          "plugins", c("plugins_description_card", "plugins_creation_card", "plugins_management_card", "plugins_options_card", "plugins_edit_code_card"),
          "modules_patient_lvl", c("patient_modules_creation_card", "patient_modules_management_card", "patient_modules_options_card"),
          "modules_aggregated", c("aggregated_modules_creation_card", "aggregated_modules_management_card", "aggregated_modules_options_card"),
          "log", c("all_users", "only_me")
        )
        
        # Load current data
        data <- DBI::dbGetQuery(r$db, paste0("SELECT name, value_num FROM options WHERE category = 'users_accesses' AND link_id = ", r$users_statuses_options))

        options_toggles_result <- tagList()

        sapply(1:nrow(options_toggles), function(i){

          sub_results <- tagList()
          
          if (options_toggles[[i, "toggles"]] != ""){
            j <<- 0
            sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
              sub_results <<- tagList(sub_results, make_toggle(language = language, ns = ns, label = toggle, inline = TRUE,
                value = data %>% dplyr::filter(name == toggle) %>% dplyr::pull(value_num) %>% as.logical()))
            })
          }
          
          label <- options_toggles[[i, "name"]]

          options_toggles_result <<- tagList(options_toggles_result, br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(language = language, ns = ns, label = label, inline = TRUE, 
                value = data %>% dplyr::filter(name == label) %>% dplyr::pull(value_num) %>% as.logical())),
            conditionalPanel(condition = paste0("input.", label, " == 1"), ns = ns,
              br(), shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), sub_results)), hr()
          )
        })

        # Render UI of options card
        output$options_card <- renderUI({

          make_card(tagList(translate(language, "accesses_options"), span(paste0(" (ID = ", r$users_statuses_options, ")"), style = "font-size: 15px;")),
            div(br(),
                
              # Basically, it's one toggle by page, and one toggle by card inside a page
              options_toggles_result, br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                shiny.fluent::DefaultButton.shinyInput(ns("select_all"), translate(language, "select_all")),
                shiny.fluent::DefaultButton.shinyInput(ns("unselect_all"), translate(language, "unselect_all")),
                shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), translate(language, "save")))
            )
          )
        })

        observeEvent(input$select_all,{
          sapply(1:nrow(options_toggles), function(i){
            shiny.fluent::updateToggle.shinyInput(session, options_toggles[[i, "name"]], value = TRUE)
            if (options_toggles[[i, "toggles"]] != ""){
              sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
                shiny.fluent::updateToggle.shinyInput(session, toggle, value = TRUE)
              })
            }
          })
        })
        
        observeEvent(input$unselect_all,{
          sapply(1:nrow(options_toggles), function(i){
            shiny.fluent::updateToggle.shinyInput(session, options_toggles[[i, "name"]], value = FALSE)
            if (options_toggles[[i, "toggles"]] != ""){
              sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
                shiny.fluent::updateToggle.shinyInput(session, toggle, value = FALSE)
              })
            }
          })
        })

        observeEvent(input$options_save, {
          
          data <- tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted)
          
          sapply(1:nrow(options_toggles), function(i){
            data <<- data %>% dplyr::bind_rows(
              tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                "user_access", r$users_statuses_options, options_toggles[[i, "name"]], "", as.integer(input[[options_toggles[[i, "name"]]]]),
                r$user_id, as.character(Sys.time()), FALSE)
              )
            if (options_toggles[[i, "toggles"]] != ""){
              sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
                value_num <- as.integer(input[[toggle]])
                
                # If category toggle is FALSE, set children to FALSE
                if (!input[[options_toggles[[i, "name"]]]]) value_num <- 0
                
                data <<- data %>% dplyr::bind_rows(
                  tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                    "users_accesses", r$users_statuses_options, toggle, "", value_num,
                    r$user_id, as.character(Sys.time()), FALSE)
                )
              })
            }
          })
          
          # Delete old data from options
          
          query <- DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE category = 'users_accesses' AND link_id = ", r$users_statuses_options))
          DBI::dbClearResult(query)
          
          # Attribute id values
          
          last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull())
          data$id <- seq.int(nrow(data)) + last_row
          
          # Add new values to database
          DBI::dbAppendTable(r$db, "options", data)
          
          # Notificate the user
          show_message_bar(output = output, id = 1, message = "modif_saved", type = "success", language = language)
          
          output$test <- renderTable(data)

        })
      })
    }
    
  })
}