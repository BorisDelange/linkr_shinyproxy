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
  if (page == "settings/modules_patient_lvl") prefix <- "patient_lvl"
  if (page == "settings/modules_aggregated") prefix <- "aggregated"
  
  if (page_style == "fluent"){
    div(class = "main",
      settings_default_elements(ns, prefix),
      settings_toggle_card(language, ns, cards = list(
        list(key = paste0(prefix, "_creation_card"), label = "modules_creation_card"),
        list(key = paste0(prefix, "_datatable_card"), label = "modules_management_card"),
        list(key = paste0(prefix, "_options_card"), label = "modules_options_card")
      )),
      # Don't use settings_creation_card because of the conditionnal panel
      div(
        id = ns(paste0(prefix, "_creation_card")),
        make_card(
          translate(language, "modules_creation"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_creation_module_type")), value = "module", options = list(
              list(key = "module", text = translate(language, "module")),
              list(key = "family", text = translate(language, "module_family"))
            ), className = "inline_choicegroup"),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(language, ns, id = paste0(prefix, "_name"), label = "name", width = "300px"),
              make_textfield(language, ns, id = paste0(prefix, "_description"), label = "description", width = "300px")
            ),
            shiny::conditionalPanel(
              condition = paste0("input.", prefix, "_creation_module_type == 'module'"), ns = ns,
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 20),
                make_dropdown(language, ns, id = paste0(prefix, "_module_family"), label = "module_family", width = "300px"),
                make_dropdown(language, ns, id = paste0(prefix, "_module_parent"), label = "module_parent", width = "300px")
              )
            ),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_add")), translate(language, "add"))
          )
        )
      ),
      div(
        id = ns(paste0(prefix, "_datatable_card")),
        make_card(
          translate(language, "modules_management"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_management_module_type")), value = "module", options = list(
              list(key = "module", text = translate(language, "modules")),
              list(key = "family", text = translate(language, "module_families"))
            ), className = "inline_choicegroup"),
            DT::DTOutput(ns(paste0(prefix, "_management_datatable"))),
            shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "save"), style = "top:-20px;")
          )
        )
      ),
      shiny::uiOutput(ns(paste0(prefix, "_options_card")))
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
    
    if (id == "settings_patient_lvl_modules") prefix <- "patient_lvl"
    if (id == "settings_aggregated_modules") prefix <- "aggregated"

    # ##########################################
    # # Show or hide cards   #
    # ##########################################

    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(prefix, "_", toggle, "_toggle")]], if(input[[paste0(prefix, "_", toggle, "_toggle")]]) shinyjs::show(paste0(prefix, "_", toggle)) else shinyjs::hide(paste0(prefix, "_", toggle)))
    })

    # ##########################################
    # # Add a new module                       #
    # ##########################################

    # Update dropdowns with reactive data
    data_var_families <- switch(id,
      "settings_patient_lvl_modules" = "patient_lvl_module_families",
      "settings_aggregated_modules" = "aggregated_module_families")
    data_var_modules <- switch(id,
      "settings_patient_lvl_modules" = "patient_lvl_modules",
      "settings_aggregated_modules" = "aggregated_modules")

    observeEvent(r[[data_var_families]], {
      options <- tibble_to_list(r[[data_var_families]], "id", "name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_family"),
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })

    observeEvent(c(input[[paste0(prefix, "_module_family")]], r$patient_lvl_modules, r$aggregated_modules), {
      # Prevent bug if input is empty
      req(input[[paste0(prefix, "_module_family")]])
      module_parents <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var_modules, " WHERE module_family_id = ", input[[paste0(prefix, "_module_family")]]))
      options <- tibble_to_list(module_parents, "id", "name", rm_deleted_rows = TRUE, null_value = TRUE, language = language)
      shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_parent"),
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })

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
      if (input[[paste0(prefix, "_creation_module_type")]] == "module") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", "settings_aggregated_modules" = "aggregated_modules")
      if (input[[paste0(prefix, "_creation_module_type")]] == "family") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", "settings_aggregated_modules" = "aggregated_module_families")

      distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", table, " WHERE deleted IS FALSE")) %>% dplyr::pull()

      if (new_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_name %not_in% distinct_names)

      # Check if module family is not empty
      module_family_check <- TRUE
      if (input[[paste0(prefix, "_creation_module_type")]] == "module" & input[[paste0(prefix, "_module_family")]] == "") module_family_check <- FALSE
      
      if (!module_family_check){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "req_module_family"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      
      req(module_family_check)
      
      last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", table)) %>% dplyr::pull()

      new_data <- switch(input[[paste0(prefix, "_creation_module_type")]],
        "module" = tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~creator_id, ~datetime, ~deleted,
          last_row + 1,
          as.character(new_name),
          coalesce2("char", input[[paste0(prefix, "_description")]]),
          coalesce2("int", input[[paste0(prefix, "_module_family")]]),
          coalesce2("int", input[[paste0(prefix, "_module_parent")]]),
          r$user_id,
          as.character(Sys.time()),
          FALSE),
        "family" = tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
          last_row + 1,
          as.character(new_name),
          coalesce2("char", input[[paste0(prefix, "_description")]]),
          r$user_id,
          as.character(Sys.time()),
          FALSE))

      DBI::dbAppendTable(r$db, table, new_data)

      r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table))
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)

      # Add a row in options table
      # last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
      # DBI::dbAppendTable(r$db, "options",
      #   tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      #     last_row_options + 1, table, last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))

      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_module_added"), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))
      
      # Show management card
      shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_datatable_card_toggle"), value = TRUE)

      # Reset textfields
      shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), value = "")
      shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_description"), value = "")
    })
    
    ##########################################
    # Modules management                     #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
  
      observeEvent(c(input[[paste0(prefix, "_management_module_type")]],
        r$patient_lvl_module_families, r$patient_lvl_modules, r$aggregated_module_families, r$aggregated_modules), {
  
        # Get data
        req(input[[paste0(prefix, "_management_module_type")]])
        module_type <- input[[paste0(prefix, "_management_module_type")]]
        if (module_type == "module") data_var <- data_var_modules
        if (module_type == "family") data_var <- data_var_families
        data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var, " WHERE deleted IS FALSE"))
        if (nrow(data) != 0){
          data <- data %>% dplyr::select(-deleted)
        }
  
        # Render datatable
        output[[paste0(prefix, "_management_datatable")]] <- DT::renderDT(
          # data,
          settings_datatable(
            ns = ns, r = r, id = id, prefix = prefix,
            data = data,
            data_variables = c("patient_lvl_modules", "patient_lvl_module_families", "aggregated_modules", "aggregated_module_families"),
            dropdowns = settings_modules_get_dropdowns(prefix, module_type),
            action_buttons = switch(module_type, "module" =  c("delete", "options"), "family" = "delete"),
            new_colnames =
            switch(prefix,
            "patient_lvl" =
              switch(module_type,
                "module" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "module_family"), 
                  translate(language, "module_parent"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action")),
                "family" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
                  translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))),
            "aggregated" =
              switch(module_type,
                "module" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "module_family"), 
                  translate(language, "module_parent"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action")),
                "family" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
                  translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))))
          ),
          options = list(dom = "t<'bottom'p>",
            columnDefs = list(list(className = "dt-center", targets = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5))),
              list(sortable = FALSE, targets = switch(module_type, "module" = c(3, 4, 7), "family" = "")))),
          rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
          editable = list(target = "cell", disable = list(columns = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5)))),
          callback = datatable_callback()
        )
      })
      
      ##########################################
      # Save changes in datatable              #
      ##########################################

      # Each time a row is updated, modify temp variable
      observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
        edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]
        edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1

        table <- settings_modules_get_table(prefix, input[[paste0(prefix, "_management_module_type")]])

        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })

      # Each time a dropdown is updated, modify temp variable
      observeEvent(c(input[[paste0(prefix, "_management_module_type")]], 
        r$patient_lvl_module_families, r$patient_lvl_modules, r$aggregated_module_families, r$aggregated_modules), {
          
        req(input[[paste0(prefix, "_management_module_type")]])
        
        table <- settings_modules_get_table(prefix, input[[paste0(prefix, "_management_module_type")]])
        dropdowns <- settings_modules_get_dropdowns(prefix, input[[paste0(prefix, "_management_module_type")]])

        sapply(r[[table]] %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
          sapply(names(dropdowns), function(name){
            observeEvent(input[[paste0(dropdowns[[name]], id)]], {
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), name]] <-
                input[[paste0(dropdowns[[name]], id)]]
              # Store that this row has been modified
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            })
          })
        })
      })

      observeEvent(input[[paste0(prefix, "_management_save")]], {
        
        # Make sure there's no duplicate in names
        duplicates <- 0

        module_type <- input[[paste0(prefix, "_management_module_type")]]
        table <- settings_modules_get_table(prefix, module_type)
        
        duplicates <- r[[paste0(table, "_temp")]] %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()

        if (duplicates > 0){
          output$warnings1 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, "modif_names_duplicates"), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings1")
          shinyjs::delay(3000, shinyjs::hide("warnings1"))
        }
        req(duplicates == 0)
        
        # Make sure parent module is not the module itself
        parent_is_itself <- FALSE
        if (module_type == "module"){
          if (nrow(r[[paste0(table, "_temp")]] %>% 
              dplyr::select(name, parent_module_id) %>%
              dplyr::left_join(r[[paste0(table, "_temp")]] %>% dplyr::select(parent_module_id = id, parent_name = name), by = "parent_module_id") %>%
              dplyr::filter(!is.na(parent_name)) %>% dplyr::filter(name == parent_name)) != 0) parent_is_itself <- TRUE
        }
        if (parent_is_itself){
          output$warnings2 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, "parent_module_is_itself"), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings2")
          shinyjs::delay(3000, shinyjs::hide("warnings2"))
        }
        req(!parent_is_itself)

        # Save changes in database
        ids_to_del <- r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
        DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
        DBI::dbClearResult(query)
        DBI::dbAppendTable(r$db, table, r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified))

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
      r[[paste0(prefix, "_delete_dialog")]] <<- FALSE

      # Create & show dialog box
      output[[paste0(prefix, "_delete_confirm")]] <- shiny.fluent::renderReact({
        settings_delete_react(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), ns, language, r[[paste0(prefix, "_delete_dialog")]])})

      # Whether to close or not delete dialog box
      observeEvent(input[[paste0(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), "_hide_dialog")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
      observeEvent(input[[paste0(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), "_delete_canceled")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
      observeEvent(input[[paste0(prefix, "_deleted_pressed")]], r[[paste0(prefix, "_delete_dialog")]] <<- TRUE)

      # When the delete is confirmed...
      observeEvent(input[[paste0(prefix, "_management_module_type")]], {
        module_type <- input[[paste0(prefix, "_management_module_type")]]
        observeEvent(input[[paste0(paste0("modules_", module_type), "_delete_confirmed")]], {
  
          # Close dialog box
          r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
  
          table <- settings_modules_get_table(prefix, module_type)
  
          # Get the ID of row deleted
          deleted_pressed_value <- input[[paste0(prefix, "_deleted_pressed")]]
          row_deleted <- as.integer(substr(deleted_pressed_value, nchar(paste0(prefix, "_delete_")) + 1, nchar(deleted_pressed_value)))
          
          # Prevent bug when changing module_type
          req(nrow(r[[table]] %>% dplyr::filter(!deleted & id == row_deleted)) > 0)
          
          # Delete row in database
          DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted))
          # Update r vars (including temp variable, used in management datatables)
          r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table))
          r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
  
          # Notification to user
          message <- paste0("modules_", module_type, "_deleted")
          output$warnings3 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings3")
          shinyjs::delay(3000, shinyjs::hide("warnings3"))
        })
      })

  })
}