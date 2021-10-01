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
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
      shiny.fluent::reactOutput(ns("plugins_delete_confirm")), 
      settings_toggle_card(language, ns, cards = list(
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
      div(
        id = ns("creation_card"),
        make_card(
          translate(language, "plugins_creation"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(language, ns, "name", width = "300px"),
              make_textfield(language, ns, "description", width = "300px"),
              make_dropdown(language, ns, "module_type", width = "300px", options = list(
                list(key = "patient_lvl_data", text = translate(language, "patient_level_data")),
                list(key = "aggregated_data", text = translate(language, "aggregated_data"))
              ), value = "patient_lvl_data")
            ),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
          )
        )
      ),
      div(
        id = ns("datatable_card"),
        make_card(
          translate(language, "plugins_management"),
          div(
            DT::DTOutput(ns("management_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save"), style = "top:-20px;")
          )
        )
      ),
      div(
        shiny::uiOutput(ns("edit_code_card")),
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
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Add a new plugin                       #
    ##########################################
    
    observeEvent(input$add, {
      new_name <- input$name
      name_check <- FALSE
      
      if (!is.null(new_name)){
        if (new_name != "") name_check <- TRUE
      }
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)
      
      req(name_check)
      
      # Check if chosen name is already used
      distinct_names <- DBI::dbGetQuery(r$db, "SELECT DISTINCT(name) FROM plugins WHERE deleted IS FALSE") %>% dplyr::pull()
      
      if (new_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_name %not_in% distinct_names)
      
      last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM plugins") %>% dplyr::pull()

      new_data <- tibble::tribble(~id, ~name, ~description, ~module_type, ~datetime,  ~deleted,
                                  last_row + 1, as.character(new_name), 
                                  ifelse(is.null(input$description), "", as.character(input$description)), 
                                  as.character(input$module_type), 
                                  as.character(Sys.time()), FALSE)
      
      DBI::dbAppendTable(r$db, "plugins", new_data)
      
      r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins")
      r$plugins_temp <- r$plugins %>% dplyr::mutate(modified = FALSE)
      
      # Add a row in code table
      last_row_code <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM code") %>% dplyr::pull()
        DBI::dbAppendTable(r$db, "code",
        tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
          last_row_code + 1, "plugin", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
      
      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_plugin_added"), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))
      
      # Reset textfields
      shiny.fluent::updateTextField.shinyInput(session, "name", value = "")
      shiny.fluent::updateTextField.shinyInput(session, "description", value = "")
    })
    
    ##########################################
    # Plugins management                     #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
      
      observeEvent(r$plugins, {
        # Get data
        data <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins WHERE deleted IS FALSE")
        if (nrow(data) != 0){
          data <- data %>% dplyr::select(-deleted)
        }
        
        # Render datatable
        output$management_datatable <- DT::renderDT(
          plugins_management_datatable(data, ns, r, language,
                               dropdowns = c("module_type" = "modules_types")),
          options = list(dom = "t<'bottom'p>",
                         columnDefs = list(list(className = "dt-center", targets = c(0, 4, 5)),
                                           list(sortable = FALSE, targets = c(3, 5)))),
          rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
          editable = list(target = "cell", disable = list(columns = c(0, 3, 4, 5))),
          callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
              var $this = $(this.node());
              $this.attr('id', this.data()[0]);
              $this.addClass('shiny-input-container');
            });
            Shiny.unbindAll(table.table().node());
            Shiny.bindAll(table.table().node());")
        )
      })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
      
      # Each time a row is updated, modify temp variable
      observeEvent(input$management_datatable_cell_edit, {
        edit_info <- input$management_datatable_cell_edit
        edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1
        r$plugins_temp <- DT::editData(r$plugins_temp, edit_info)
        # Store that this row has been modified
        r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
      })
    
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r$plugins, {
        sapply(r$plugins %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
          observeEvent(input[[paste0("module_type", id)]], {
            r$plugins_temp[[which(r$plugins_temp["id"] == id), "module_type"]] <-
              input[[paste0("module_type", id)]]
            # Store that if this row has been modified
            r$plugins_temp[[which(r$plugins_temp["id"] == id), "modified"]] <- TRUE
          })
        })
      })
      
      observeEvent(input$management_save, {
        # Make sure there's no duplicate in names
        data <- r$plugins_temp
        duplicates <- data %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        if (duplicates > 0){
          output$warnings1 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, "modif_names_duplicates"), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings1")
          shinyjs::delay(3000, shinyjs::hide("warnings1"))
        }
        req(duplicates == 0)
        
        # Save changes in database
        ids_to_del <- r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::pull(id)
        DBI::dbSendStatement(r$db, paste0("DELETE FROM plugins WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
        DBI::dbClearResult(query)
        DBI::dbAppendTable(r$db, "plugins", r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::select(-modified))
        
        # Reload r variable
        r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins")
        r$plugins_temp <- r$plugins %>% dplyr::filter(deleted == FALSE) %>% dplyr::mutate(modified = FALSE)
        
        # Notification to user
        output$warnings2 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
        })
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      })
    
      ##########################################
      # Delete a row in datatable              #
      ##########################################
      
      # Indicate whether to close or not delete dialog box
      r$plugins_delete_dialog <- FALSE
      
      # Create & show dialog box 
      output$plugins_delete_confirm <- shiny.fluent::renderReact(settings_delete_react("plugins", ns, language, r$plugins_delete_dialog))
      
      # Whether to close or not delete dialog box
      observeEvent(input$plugins_hide_dialog, r$plugins_delete_dialog <- FALSE)
      observeEvent(input$plugins_delete_canceled, r$plugins_delete_dialog <- FALSE)
      observeEvent(input$plugins_deleted_pressed, r$plugins_delete_dialog <- TRUE)
      
      # When the delete is confirmed...
      observeEvent(input$plugins_delete_confirmed, {
        
        # Close dialog box
        r$plugins_delete_dialog <- FALSE
        
        data_var <- switch(id, "users" = "users", "statuses" = "users_accesses_statuses")
        
        # Get the ID of row deleted
        deleted_pressed_value <- isolate(input$plugins_deleted_pressed)
        row_deleted <- as.integer(substr(deleted_pressed_value, nchar("delete") + 1, nchar(deleted_pressed_value)))
        # Delete row in database
        DBI::dbSendStatement(r$db, paste0("UPDATE plugins SET deleted = TRUE WHERE id = ", row_deleted))
        # Update r vars (including temp variable, used in management datatables)
        r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins")
        r$plugins_temp <- r$plugins %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
        
        # Notification to user
        output$warnings3 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "plugin_deleted"), messageBarType = 3), style = "margin-top:10px;")
        })
        shinyjs::show("warnings3")
        shinyjs::delay(3000, shinyjs::hide("warnings3"))
      })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      observeEvent(input$edit_code, {
        req(input$edit_code)
        shiny.fluent::updateToggle.shinyInput(session, "edit_code_card_toggle", value = TRUE)
        output$edit_code_card <- renderUI({
          category_filter <- "plugin"
          link_id_filter <- as.integer(substr(input$edit_code, nchar("edit_code") + 1, nchar(input$edit_code)))
          code <- r$code %>% dplyr::filter(category == category_filter & link_id == link_id_filter) %>% dplyr::pull(code)
          settings_edit_code_card(language, ns, type = "code", code = code, link_id = link_id_filter, title = paste0("edit_", category_filter, "_code"), prefix = prefix)
        })
        output$code_result <- renderText("")
      })
      
      observeEvent(input$edit_code_save, {
        category_filter <- "plugin"
        link_id_filter <- as.integer(substr(input$edit_code, nchar("edit_code") + 1, nchar(input$edit_code)))
        code_id <- r$code %>% dplyr::filter(category == category_filter, link_id == link_id_filter) %>% dplyr::pull(id)
        # Replace ' with '' and store in the database
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code, "'", "''"), "' WHERE id = ", code_id)) -> query
        DBI::dbClearResult(query)
        r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code")

        output$warnings3 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
        })
        shinyjs::show("warnings3")
        shinyjs::delay(3000, shinyjs::hide("warnings3"))
      })

      observeEvent(input$execute_code, {
        output$code_result <- renderText({
          # Change this option to display correctly tibble in textbox
          eval(parse(text = "options('cli.num_colors' = 1)"))
          # Capture console output of our code
          captured_output <- capture.output(
            tryCatch(eval(parse(text = isolate(input$ace_edit_code))), error = function(e) print(e), warning = function(w) print(w))
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
