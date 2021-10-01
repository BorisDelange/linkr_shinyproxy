#' settings_modules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_modules_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
      shiny.fluent::reactOutput(ns("modules_delete_confirm")), 
      settings_toggle_card(language, ns, creation_card = "modules_creation_card", datatable_card = "modules_management_card",
                           options_card = "modules_options_card", activated = c("")),
      div(
        id = ns("creation_card"),
        make_card(
          translate(language, "modules_creation"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("creation_module_type"), value = "module", options = list(
              list(key = "module", text = translate(language, "module")),
              list(key = "family", text = translate(language, "module_family"))
            ), className = "inline_choicegroup"),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(language, ns, "name", width = "300px"),
              make_textfield(language, ns, "description", width = "300px")
            ),
            shiny::conditionalPanel(
              condition = "input.creation_module_type == 'module'", ns = ns,
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 20),
                make_dropdown(language, ns, "module_family", width = "300px"),
                make_dropdown(language, ns, "module_parent", width = "300px")
              )
            ),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
          )
        )
      ),
      div(
        id = ns("datatable_card"),
        make_card(
          translate(language, "modules_management"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("management_module_type"), value = "module", options = list(
              list(key = "module", text = translate(language, "module")),
              list(key = "family", text = translate(language, "module_family"))
            ), className = "inline_choicegroup"),
            DT::DTOutput(ns("management_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save"), style = "top:-20px;")
          )
        )
      ),
      div(
        shiny::uiOutput(ns("options_card")),
      )
    ) -> result
  }
  
  result
}
    
#' settings_modules Server Functions
#'
#' @noRd 
mod_settings_modules_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toggles <- c("creation_card", "datatable_card", "options_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Add a new module                       #
    ##########################################
    
    # Update dropdowns with reactive data
    data_var_families <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", 
                           "settings_aggregated_modules" = "aggregated_module_families")
    data_var_modules <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", 
                               "settings_aggregated_modules" = "aggregated_modules")
    
    observeEvent(r[[data_var_families]], {
      options <- tibble_to_list(r[[data_var_families]], "id", "name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "module_family",
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    
    observeEvent(input$module_family, {
      module_parents <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var_modules, " WHERE module_family_id = ", input$module_family))
      options <- tibble_to_list(module_parents, "id", "name", rm_deleted_rows = TRUE, null_value = TRUE, language = language)
      shiny.fluent::updateDropdown.shinyInput(session, "module_parent",
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    
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
      if (input$creation_module_type == "module") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", "settings_aggregated_modules" = "aggregated_modules")
      if (input$creation_module_type == "family") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", "settings_aggregated_modules" = "aggregated_module_families")
      
      distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", table, " WHERE deleted IS FALSE")) %>% dplyr::pull()

      if (new_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_name %not_in% distinct_names)

      last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", table)) %>% dplyr::pull()

      new_data <- switch(input$creation_module_type,
        "module" = tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~creator_id, ~datetime, ~deleted,
          last_row + 1, as.character(new_name), ifelse(is.null(input$description), "", as.character(input$description)),
          as.integer(input$module_family), ifelse(is.null(input$module_parent), NA_integer_, as.integer(input$module_parent)),
          r$creator_id, as.character(Sys.time()), FALSE),
        "family" = tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
          last_row + 1, as.character(new_name), ifelse(is.null(input$description), "", as.character(input$description)),
          r$creator_id, as.character(Sys.time()), FALSE))

      DBI::dbAppendTable(r$db, table, new_data)

      r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table))
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)

      # Add a row in options table
      # last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
      # DBI::dbAppendTable(r$db, "options",
      #   tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      #     last_row_options + 1, table, last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))

      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_plugin_added"), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))

      # Reset textfields
      shiny.fluent::updateTextField.shinyInput(session, "name", value = "")
      shiny.fluent::updateTextField.shinyInput(session, "description", value = "")
    })
    
    ##########################################
    # Modules management                     #
    ##########################################
    
    ##########################################
    # Generate datatable                     #
    ##########################################

    observeEvent(c(input$management_module_type, r$patient_lvl_module_families, r$patient_lvl_modules,
                   r$aggregated_module_families, r$aggregated_modules), {
      # Get data
      req(input$management_module_type)
      if (input$management_module_type == "module") data_var <- data_var_modules
      if (input$management_module_type == "family") data_var <- data_var_families
      data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var, " WHERE deleted IS FALSE"))
      if (nrow(data) != 0){
        data <- data %>% dplyr::select(-deleted)
      }

      # Render datatable
      output$management_datatable <- DT::renderDT(
        data,
        # settings_datatable(
        #   ns = ns, r = r, id = id, name = substr(id, nchar("settings_") + 1, nchar(id)), 
        #   data = data, 
        #   data_variables = c("patient_lvl_modules", "patient_lvl_module_families", "aggregated_modules", "aggregated_module_families"), 
        #   dropdowns = c("module_family_id" = "patient_lvl_module_families"), 
        #   action_buttons = "", 
        #   new_colnames = "", 
        #   subpages = FALSE),
        # settings_management_datatable(data, ns, r, language,
        #                              dropdowns = c("module_type" = "modules_types"))#,
        options = list(dom = "t<'bottom'p>")#,
        #                columnDefs = list(list(className = "dt-center", targets = c(0, 4, 5)),
        #                                  list(sortable = FALSE, targets = c(3, 5)))),
        # rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
        # editable = list(target = "cell", disable = list(columns = c(0, 3, 4, 5))),
        # callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
        #       var $this = $(this.node());
        #       $this.attr('id', this.data()[0]);
        #       $this.addClass('shiny-input-container');
        #     });
        #     Shiny.unbindAll(table.table().node());
        #     Shiny.bindAll(table.table().node());")
      )
    })
    
    ##########################################
    # Save changes in datatable              #
    ##########################################
    
    # Each time a row is updated, modify temp variable
    # observeEvent(input$management_datatable_cell_edit, {
    #   edit_info <- input$management_datatable_cell_edit
    #   edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1
    #   r$plugins_temp <- DT::editData(r$plugins_temp, edit_info)
    #   # Store that this row has been modified
    #   r$plugins_temp[[edit_info$row, "modified"]] <- TRUE
    # })
    # 
    # # Each time a dropdown is updated, modify temp variable
    # observeEvent(r$plugins, {
    #   sapply(r$plugins %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
    #     observeEvent(input[[paste0("module_type", id)]], {
    #       r$plugins_temp[[which(r$plugins_temp["id"] == id), "module_type"]] <-
    #         input[[paste0("module_type", id)]]
    #       # Store that if this row has been modified
    #       r$plugins_temp[[which(r$plugins_temp["id"] == id), "modified"]] <- TRUE
    #     })
    #   })
    # })
    # 
    # observeEvent(input$management_save, {
    #   # Make sure there's no duplicate in names
    #   data <- r$plugins_temp
    #   duplicates <- data %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
    #     dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    #   if (duplicates > 0){
    #     output$warnings1 <- renderUI({
    #       div(shiny.fluent::MessageBar(translate(language, "modif_names_duplicates"), messageBarType = 3), style = "margin-top:10px;")
    #     })
    #     shinyjs::show("warnings1")
    #     shinyjs::delay(3000, shinyjs::hide("warnings1"))
    #   }
    #   req(duplicates == 0)
    #   
    #   # Save changes in database
    #   ids_to_del <- r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::pull(id)
    #   DBI::dbSendStatement(r$db, paste0("DELETE FROM plugins WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
    #   DBI::dbClearResult(query)
    #   DBI::dbAppendTable(r$db, "plugins", r$plugins_temp %>% dplyr::filter(modified) %>% dplyr::select(-modified))
    #   
    #   # Reload r variable
    #   r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins")
    #   r$plugins_temp <- r$plugins %>% dplyr::filter(deleted == FALSE) %>% dplyr::mutate(modified = FALSE)
    #   
    #   # Notification to user
    #   output$warnings2 <- renderUI({
    #     div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
    #   })
    #   shinyjs::show("warnings2")
    #   shinyjs::delay(3000, shinyjs::hide("warnings2"))
    # })
    
    ##########################################
    # Delete a row in datatable              #
    ##########################################
    
    # Indicate whether to close or not delete dialog box
    # r$plugins_delete_dialog <- FALSE
    # 
    # # Create & show dialog box 
    # output$plugins_delete_confirm <- shiny.fluent::renderReact(settings_delete_react("plugins", ns, language, r$plugins_delete_dialog))
    # 
    # # Whether to close or not delete dialog box
    # observeEvent(input$plugins_hide_dialog, r$plugins_delete_dialog <- FALSE)
    # observeEvent(input$plugins_delete_canceled, r$plugins_delete_dialog <- FALSE)
    # observeEvent(input$plugins_deleted_pressed, r$plugins_delete_dialog <- TRUE)
    # 
    # # When the delete is confirmed...
    # observeEvent(input$plugins_delete_confirmed, {
    #   
    #   # Close dialog box
    #   r$plugins_delete_dialog <- FALSE
    #   
    #   data_var <- switch(id, "users" = "users", "statuses" = "users_accesses_statuses")
    #   
    #   # Get the ID of row deleted
    #   deleted_pressed_value <- isolate(input$plugins_deleted_pressed)
    #   row_deleted <- as.integer(substr(deleted_pressed_value, nchar("delete") + 1, nchar(deleted_pressed_value)))
    #   # Delete row in database
    #   DBI::dbSendStatement(r$db, paste0("UPDATE plugins SET deleted = TRUE WHERE id = ", row_deleted))
    #   # Update r vars (including temp variable, used in management datatables)
    #   r$plugins <- DBI::dbGetQuery(r$db, "SELECT * FROM plugins")
    #   r$plugins_temp <- r$plugins %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
    #   
    #   # Notification to user
    #   output$warnings3 <- renderUI({
    #     div(shiny.fluent::MessageBar(translate(language, "plugin_deleted"), messageBarType = 3), style = "margin-top:10px;")
    #   })
    #   shinyjs::show("warnings3")
    #   shinyjs::delay(3000, shinyjs::hide("warnings3"))
    # })
    
  })
}