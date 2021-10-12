#' settings_plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_plugins_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    prefix <- "plugins"
    div(class = "main",
      settings_default_elements(ns, prefix),
      settings_toggle_card(language, ns, cards = list(
        list(key = paste0(prefix, "_description_card"), label = "plugins_description_card"),
        list(key = paste0(prefix, "_creation_card"), label = "plugins_creation_card"),
        list(key = paste0(prefix, "_datatable_card"), label = "plugins_management_card"),
        list(key = paste0(prefix, "_edit_code_card"), label = "plugins_edit_code_card")
      )),
      div(
        id = ns(paste0(prefix, "_description_card")),
        make_card(
          translate(language, "plugins_description"),
          "")
      ),
      div(
        id = ns(paste0(prefix, "_creation_card")),
        make_card(
          translate(language, "plugins_creation"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(language, ns, label = "name", id = paste0(prefix, "_name"), width = "300px"),
              make_textfield(language, ns, label = "description", id = paste0(prefix, "_description"), width = "300px"),
              make_dropdown(language, ns, label = "module_type", id = paste0(prefix, "_module_type"), width = "300px", options = list(
                list(key = "patient_lvl_data", text = translate(language, "patient_level_data")),
                list(key = "aggregated_data", text = translate(language, "aggregated_data"))
              ), value = "patient_lvl_data")
            ),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_add")), translate(language, "add"))
          )
        )
      ),
      div(
        shiny::uiOutput(ns(paste0(prefix, "_edit_code_card")))
      ),
      div(
        id = ns(paste0(prefix, "_datatable_card")),
        make_card(
          translate(language, "plugins_management"),
          div(
            DT::DTOutput(ns(paste0(prefix, "_management_datatable"))),
            shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "save"), style = "top:-20px;")
          )
        )
      )
    ) -> result
  }
  
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
      observeEvent(input[[paste0(prefix, "_", toggle, "_toggle")]], if(input[[paste0(prefix, "_", toggle, "_toggle")]]) shinyjs::show(paste0(prefix, "_", toggle)) else shinyjs::hide(paste0(prefix, "_", toggle)))
    })
    
    ##########################################
    # Add a new plugin                       #
    ##########################################
    
    observeEvent(input[[paste0(prefix, "_add")]], {
      new_name <- input[[paste0(prefix, "_name")]]
      name_check <- FALSE
      
      if (!is.null(new_name)){
        if (new_name != "") name_check <- TRUE
      }
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = NULL)
      
      req(name_check)
      
      # Check if chosen name is already used
      distinct_names <- DBI::dbGetQuery(r$db, "SELECT DISTINCT(name) FROM plugins WHERE deleted IS FALSE") %>% dplyr::pull()
      
      if (new_name %in% distinct_names) output$message_bar2 <- message_bar(2, "name_already_used", "severeWarning", language)
      req(new_name %not_in% distinct_names)
      
      last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM plugins") %>% dplyr::pull()

      new_data <- tibble::tribble(~id, ~name, ~description, ~module_type, ~datetime,  ~deleted,
        last_row + 1,
        as.character(new_name),
        coalesce2("char", input[[paste0(prefix, "_description")]]),
        coalesce2("char", input[[paste0(prefix, "_module_type")]]),
        as.character(Sys.time()),
        FALSE)
      
      DBI::dbAppendTable(r$db, "plugins", new_data)
      
      # Reload r variable
      r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins WHERE deleted IS FALSE")
      r$plugins_temp <- r$plugins %>% dplyr::mutate(modified = FALSE)
      
      # Add a row in code table
      last_row_code <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM code") %>% dplyr::pull()
        DBI::dbAppendTable(r$db, "code",
        tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
          last_row_code + 1, "plugin_ui", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
          last_row_code + 2, "plugin_server", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
      
      # Reload r variable
      r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE")
        
      output$message_bar1 <- message_bar(1, "new_plugin_added", "success", language)
      
      # Reset textfields
      shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), value = "")
      shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_description"), value = "")
    })
    
    ##########################################
    # Plugins management                     #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
      
      observeEvent(r$plugins_temp, {
        # Datatable state
        page_length <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$length)
        start <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$start)
        
        # Render datatable
        output[[paste0(prefix, "_management_datatable")]] <- DT::renderDT(
          plugins_management_datatable(settings_datatable_data(prefix, r), ns, r, language, prefix,
            dropdowns = c("module_type" = "modules_types")),
          options = list(dom = "<'datatable_length'l><'top'ft><'bottom'p>",
            stateSave = TRUE, stateDuration = 30, pageLength = page_length, displayStart = start,
            columnDefs = list(list(className = "dt-center", targets = c(0, 4, 5)),
              list(sortable = FALSE, targets = c(3, 5)))),
          rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
          editable = list(target = "cell", disable = list(columns = c(0, 3, 4, 5))),
          callback = datatable_callback()
        )
      })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
      
      # Each time a row is updated, modify temp variable
      observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
        edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]
        r$plugins_temp <- DT::editData(r$plugins_temp, edit_info, rownames = FALSE)
        # Store that this row has been modified
        r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
      })
    
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r$plugins, {
        sapply(r$plugins %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
          observeEvent(input[[paste0(prefix, "_module_type", id)]], {
            r$plugins_temp[[which(r$plugins_temp["id"] == id), "module_type"]] <-
              input[[paste0(prefix, "_module_type", id)]]
            # Store that if this row has been modified
            r$plugins_temp[[which(r$plugins_temp["id"] == id), "modified"]] <- TRUE
          })
        })
      })
      
      observeEvent(input[[paste0(prefix, "_management_save")]], {
        # Make sure there's no duplicate in names
        data <- r$plugins_temp
        duplicates <- data %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        if (duplicates > 0) output$message_bar1 <- message_bar(3, "modif_names_duplicates", "severeWarning", language)
        req(duplicates == 0)
        
        # Save changes in database
        ids_to_del <- r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::pull(id)
        DBI::dbSendStatement(r$db, paste0("DELETE FROM plugins WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
        DBI::dbClearResult(query)
        DBI::dbAppendTable(r$db, "plugins", r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::select(-modified))
        
        # Reload r variable
        r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins WHERE deleted IS FALSE")
        r$plugins_temp <- r$plugins %>% dplyr::mutate(modified = FALSE)
        
        # Notification to user
        output$message_bar2 <- message_bar(3, "modif_saved", "severeWarning", language)
      })
    
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

        output$message_bar3 <- message_bar(3, "modif_saved", "success", language)
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
