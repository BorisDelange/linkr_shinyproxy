#' settings_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_users_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  # Three distinct pages in the settings/users page : users, accesses & statuses
  # For each "sub page", create a creation & a management cards
  
  pages <- c("users", "users_accesses", "users_statuses")
  cards <- tagList()
  
  # We create one module by "sub page"
  
  sapply(pages, function(page){
    
    cards <<- tagList(cards,
      div(id = ns(paste0(page, "_creation_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_creation"), language = language, words = words)),
      div(id = ns(paste0(page, "_management_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_management"), language = language, words = words)))
    
    if (page == "users_accesses") cards <<- tagList(cards,
      div(id = ns(paste0(page, "_options_card")), mod_settings_sub_users_ui(id = paste0("settings_users_", page, "_options"), language = language, words = words)))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "users", text = translate(language, "users", words))
    ), maxDisplayedItems = 3),
    div(id = ns("pivot"),
      shiny.fluent::Pivot(
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "users_creation_card", itemKey = "users_creation_card", headerText = translate(language, "users_creation_card", words)),
        shiny.fluent::PivotItem(id = "users_management_card", itemKey = "users_management_card", headerText = translate(language, "users_management_card", words)),
        shiny.fluent::PivotItem(id = "users_accesses_creation_card", itemKey = "users_accesses_creation_card", headerText = translate(language, "users_accesses_creation_card", words)),
        shiny.fluent::PivotItem(id = "users_accesses_management_card", itemKey = "users_accesses_management_card", headerText = translate(language, "users_accesses_management_card", words)),
        shiny.fluent::PivotItem(id = "users_accesses_options_card", itemKey = "users_accesses_options_card", headerText = translate(language, "users_accesses_options_card", words)),
        shiny.fluent::PivotItem(id = "users_statuses_creation_card", itemKey = "users_statuses_creation_card", headerText = translate(language, "users_statuses_creation_card", words)),
        shiny.fluent::PivotItem(id = "users_statuses_management_card", itemKey = "users_statuses_management_card", headerText = translate(language, "users_statuses_management_card", words))
      )
    ),
    cards
  )
}

mod_settings_sub_users_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  page <- substr(id, nchar("settings_users_") + 1, nchar(id))
  
  if (page == "users_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_user",
      textfields = c("username", "firstname", "lastname", "password"), textfields_width = "200px",
      dropdowns = c("user_access", "user_status"), dropdowns_width = "200px", words = words) -> result
  }
  
  if (page == "users_accesses_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_access",
      textfields = c("name", "description"), textfields_width = "300px", words = words) -> result
  }
  
  if (page == "users_statuses_creation"){
    render_settings_creation_card(language = language, ns = ns, id = id, title = "add_status",
      textfields = c("name", "description"), textfields_width = "300px", words = words) -> result
  }
  
  if (grepl("management", page)){
    render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = page, words = words) -> result
  }
  
  if (page == "users_accesses_options"){
    uiOutput(ns("options_card")) -> result
  }
  
  tagList(render_settings_default_elements(ns = ns), result)
}
    
#' settings_users Server Functions
#'
#' @noRd 
mod_settings_users_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    cards <- c(
      "users_creation_card", "users_management_card", 
      "users_accesses_creation_card", "users_accesses_management_card", "users_accesses_options_card",
      "users_statuses_creation_card", "users_statuses_management_card")
    
    sapply(cards, shinyjs::hide)
    
    # Current page
    page <- substr(id, nchar("settings_users_") + 1, nchar(id))
    
    # Corresponding table in the database
    if (grepl("creation", page)) table <- substr(page, 1, nchar(page) - nchar("_creation"))
    if (grepl("management", page)) table <- substr(page, 1, nchar(page) - nchar("_management"))
    if (grepl("options", page)) table <- substr(page, 1, nchar(page) - nchar("_options"))
    
    # Dropdowns used for creation card
    dropdowns <- ""
    if (page %in% c("users_creation", "users_management")) dropdowns <- c("user_access", "user_status")
    
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    # Initiate vars
    
    # Used to communicate between modules and to show / hide cards
    r$users_toggle <- 0L
    r$users_accesses_toggle <- 0L
    r$users_statuses_toggle <- 0L
    
    # Used to send option link_id from one module to another
    r$users_statuses_options <- 0L
    
    # Only for main users page (not for sub-pages)
    if (id == "settings_users"){
      
      # Depending on user_accesses
      observeEvent(r$user_accesses, {
        # Hide Pivot if user has no access
        if ("users" %not_in% r$user_accesses) shinyjs::hide("pivot") else shinyjs::show("pivot")
      })

      # Depending on cards activated
      
      show_hide_cards_new(r = r, input = input, session = session, id = id, cards = cards)
      
      # When a new user, a user status or a user access is added, close add card & show data management card
      sapply(c("users", "users_accesses", "users_statuses"), function(page){
        observeEvent(r[[paste0(page, "_toggle")]], {
          if (r[[paste0(page, "_toggle")]] != 0){
            shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_creation_card_toggle"), value = FALSE)
            shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_management_card_toggle"), value = TRUE)}
        })
      })
      
      observeEvent(r$users_statuses_options, {
        if (r$users_statuses_options > 0){
          shinyjs::show("users_accesses_options_card")
          # shiny.fluent::updateToggle.shinyInput(session, "users_accesses_options_card_toggle", value = TRUE)
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
            options <- convert_tibble_to_list(data = r[[data_var]], key_col = "id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        })
      
      # When add button is clicked
      observeEvent(input$add, {
        
        # If user has access
        req(paste0(table, "_creation_card") %in% r$user_accesses)
        
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
        
        if (r$perf_monitoring) print(paste0(Sys.time(), " _ --- BEGIN load", table, " management datatable"))
        
        # Dropdowns for each module / page
        dropdowns_datatable <- switch(table, "users" = c("user_access_id" = "users_accesses", "user_status_id" = "users_statuses"),
          "users_accesses" = "", "users_statuses" = "")
        
        # Action buttons for each module / page
        if ("users_delete_data" %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
        action_buttons = switch(table, "users" = action_buttons, "users_accesses" = c("options", action_buttons), "users_statuses" =action_buttons)
        
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
        
        # If r variable already created, or not
        if (length(r[[paste0(table, "_datatable_temp")]]) == 0) data_output <- tibble::tibble()
        else data_output <- r[[paste0(table, "_datatable_temp")]]
        
        # Prepare data for datatable (add code for dropdowns etc)
        r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
          table = table, dropdowns = dropdowns_datatable,
          action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]], data_output = data_output, words = r$words)
        
        hidden_cols <- c("id", "password", "deleted", "modified")
        
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, language = language, data = r[[paste0(table, "_datatable_temp")]],
          output_name = "management_datatable", col_names =  get_col_names(table_name = table, language = language, words = r$words),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
        
        # Reload datatable
        observeEvent(r[[paste0(table, "_temp")]], {
          
          # Reload datatable_temp variable
          r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
            table = table, dropdowns = dropdowns_datatable,
            action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]], data_output = data_output, words = r$words)
          
          # Reload data of datatable
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
        })
        
        if (r$perf_monitoring) print(paste0(Sys.time(), " _ --- END load ", table, " management datatable"))
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
          observeEvent(r$users, {
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
          
          # If user has access
          req(paste0(table, "_management_card") %in% r$user_accesses)
  
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
    if (page == "users_accesses_management"){
    
      observeEvent(input$options, {
        # Show options toggle
        r$users_statuses_options <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

      })
    }
    
    if (page == "users_accesses_options"){
      
      observeEvent(r$users_statuses_options, {
        req(r$users_statuses_options > 0)
        
        # If user has access
        req(paste0(table, "_options_card") %in% r$user_accesses)

        # All toggles displayed
        options_toggles <- tibble::tribble(
          ~name, ~toggles,
          "patient_lvl_data", c(""),
          "general_settings", "change_password_card",
          "app_db", c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card", "db_restore_card"),
          "users", c("users_delete_data", "users_creation_card", "users_management_card",
             "users_accesses_creation_card", "users_accesses_management_card", "users_accesses_options_card",
             "users_statuses_creation_card", "users_statuses_management_card"),
          "r_console", "r_console_edit_code_card",
          "data_sources", c("data_sources_see_all_data", "data_sources_edit_data", "data_sources_delete_data", "data_sources_creation_card", "data_sources_datatable_card"),
          "datamarts", c("datamarts_see_all_data", "datamarts_edit_data", "datamarts_delete_data", "datamarts_creation_card", "datamarts_datatable_card", "datamarts_options_card", "datamarts_edit_code_card"),
          "studies", c("studies_see_all_data", "studies_edit_data", "studies_delete_data", "studies_creation_card", "studies_datatable_card", "studies_options_card"),
          "subsets", c("subsets_see_all_data", "subsets_edit_data", "subsets_delete_data", "subsets_creation_card", "subsets_datatable_card", "subsets_edit_code_card"),
          "thesaurus", c("thesaurus_see_all_data", "thesaurus_edit_data", "thesaurus_delete_data", "thesaurus_creation_card", "thesaurus_datatable_card", "thesaurus_sub_datatable_card", "thesaurus_edit_code_card"),
          "plugins", c("plugins_see_all_data", "plugins_edit_data", "plugins_delete_data", "plugins_description_card", "plugins_creation_card", "plugins_datatable_card", "plugins_options_card", "plugins_edit_code_card"),
          "patient_lvl_modules", c("patient_lvl_modules_see_all_data", "patient_lvl_modules_edit_data", "patient_lvl_modules_delete_data", "patient_lvl_modules_creation_card", "patient_lvl_modules_management_card", "patient_lvl_modules_options_card"),
          "aggregated_modules", c("aggregated_modules_see_all_data", "aggregated_modules_edit_data", "aggregated_modules_delete_data", "aggregated_modules_creation_card", "aggregated_modules_management_card", "aggregated_modules_options_card"),
          "log", c("all_users", "only_me")
        )
        
        # Get current data
        current_data <- DBI::dbGetQuery(r$db, paste0("SELECT name, value_num FROM options WHERE category = 'users_accesses' AND link_id = ", r$users_statuses_options))
        
        options_toggles_result <- tagList()

        sapply(1:nrow(options_toggles), function(i){

          sub_results <- tagList()
          
          if (options_toggles[[i, "toggles"]] != ""){
            j <<- 0
            sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
              
              # Get current value
              value <- current_data %>% dplyr::filter(name == toggle)
              if (nrow(value) == 0) value <- FALSE
              else value <- current_data %>% dplyr::filter(name == toggle) %>% dplyr::pull(value_num) %>% as.logical()
              
              # Create toggle
              sub_results <<- tagList(sub_results, make_toggle(language = language, ns = ns, label = toggle, inline = TRUE, value = value, words = words))
            })
          }
          
          label <- options_toggles[[i, "name"]]
          
          # Get current value
          value <- current_data %>% dplyr::filter(name == label)
          if (nrow(value) == 0) value <- FALSE
          else value <- current_data %>% dplyr::filter(name == label) %>% dplyr::pull(value_num) %>% as.logical()

          # Create final toggle
          options_toggles_result <<- tagList(options_toggles_result, br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(language = language, ns = ns, label = label, inline = TRUE, value = value, words = words)),
            conditionalPanel(condition = paste0("input.", label, " == 1"), ns = ns,
              br(), shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), sub_results)), hr()
          )
        })

        # Render UI of options card
        output$options_card <- renderUI({

          make_card(tagList(translate(language, "accesses_options", words), span(paste0(" (ID = ", r$users_statuses_options, ")"), style = "font-size: 15px;")),
            div(br(),
                
              # Basically, it's one toggle by page, and one toggle by card inside a page
              options_toggles_result, br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                shiny.fluent::DefaultButton.shinyInput(ns("select_all"), translate(language, "select_all", words)),
                shiny.fluent::DefaultButton.shinyInput(ns("unselect_all"), translate(language, "unselect_all", words)),
                shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), translate(language, "save", words)))
            )
          )
        })

        # When select all button is clicked, put all toggles to TRUE
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
        
        # When unselect all button is clicked, put all toggles to FALSE
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

        # When save button is clicked
        observeEvent(input$options_save, {
          
          # If user has access
          req(paste0(table, "_options_card") %in% r$user_accesses)
          
          # Create a data variable to insert data in database
          data <- tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted)
          
          # Loop over all toggles, set 0 to value_num is toggle is FALSE, 1 else
          sapply(1:nrow(options_toggles), function(i){
            data <<- data %>% dplyr::bind_rows(
              tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                "users_accesses", isolate(r$users_statuses_options), options_toggles[[i, "name"]], "", as.integer(isolate(input[[options_toggles[[i, "name"]]]])),
                r$user_id, as.character(Sys.time()), FALSE)
              )
            if (options_toggles[[i, "toggles"]] != ""){
              sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
                
                value_num <- as.integer(isolate(input[[toggle]]))
                
                # If category toggle is FALSE, set children to FALSE
                if (isolate(input[[options_toggles[[i, "name"]]]] == 0)) value_num <- 0
                
                data <<- data %>% dplyr::bind_rows(
                  tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                    "users_accesses", isolate(r$users_statuses_options), toggle, "", value_num,
                    r$user_id, as.character(Sys.time()), FALSE)
                )
              })
            }
          })
          
          # Delete old data from options
          
          sql <- paste0("DELETE FROM options WHERE category = 'users_accesses' AND link_id = ", r$users_statuses_options)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          add_log_entry(r = r, category = "SQL query", name = "Update users accesses", value = sql)
          
          # Attribute id values
          
          last_row <- get_last_row(con = r$db, table = "options")
          data$id <- seq.int(nrow(data)) + last_row
          
          # Add new values to database
          DBI::dbAppendTable(r$db, "options", data)
          add_log_entry(r = r, category = "SQL query", name = "Update users accesses", value = toString(data))
          
          # Notificate the user
          show_message_bar(output = output, id = 1, message = "modif_saved", type = "success", language = language)

        })
      })
    }
    
  })
}