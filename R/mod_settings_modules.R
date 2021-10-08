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
      shiny::uiOutput(ns(paste0(prefix, "_options_card"))),
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
            shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "save"), style = "top:-30px;")
          )
        )
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
    
    if (id == "settings_patient_lvl_modules") prefix <- "patient_lvl"
    if (id == "settings_aggregated_modules") prefix <- "aggregated"

    ##########################################
    # Show or hide cards   #
    ##########################################

    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(prefix, "_", toggle, "_toggle")]], if(input[[paste0(prefix, "_", toggle, "_toggle")]]) shinyjs::show(paste0(prefix, "_", toggle)) else shinyjs::hide(paste0(prefix, "_", toggle)))
    })

    ##########################################
    # Add a new module                       #
    ##########################################

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
      module_parents <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var_modules, " WHERE module_family_id = ", input[[paste0(prefix, "_module_family")]], " ORDER BY id"))
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

      r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
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
  
      observeEvent(input[[paste0(prefix, "_management_module_type")]], {
  
        # Get data
        req(input[[paste0(prefix, "_management_module_type")]])
        module_type <- input[[paste0(prefix, "_management_module_type")]]
        if (module_type == "module") data_var <- data_var_modules
        if (module_type == "family") data_var <- data_var_families
        
        observeEvent(r[[paste0(data_var, "_temp")]], {
          
          # Datatable state
          page_length <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$length)
          start <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$start)
          
          # Render datatable
          output[[paste0(prefix, "_management_datatable")]] <- DT::renderDT(
            # data,
            settings_datatable(
              ns = ns, r = r, id = id, prefix = prefix,
              data = settings_datatable_data(data_var, r),
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
            options = list(dom = "<'datatable_length'l><'top'ft><'bottom'p>",
              stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
              pageLength = page_length, displayStart = start,
              columnDefs = list(list(className = "dt-center", targets = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5))),
                list(sortable = FALSE, targets = switch(module_type, "module" = c(3, 4, 7), "family" = ""))),
              language = list(
                paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
                search = translate(language, "DT_search"),
                lengthMenu = translate(language, "DT_length"))),
            rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
            editable = list(target = "cell", disable = list(columns = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5)))),
            callback = datatable_callback()
          )
        })
      })
      
      ##########################################
      # Save changes in datatable              #
      ##########################################

      # Each time a row is updated, modify temp variable
      observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
        edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]

        table <- settings_modules_get_table(prefix, input[[paste0(prefix, "_management_module_type")]])

        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
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
                coalesce2("int", input[[paste0(dropdowns[[name]], id)]])
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
          r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
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
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_options")]], {
        # Show options toggle
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_options_card_toggle"), value = TRUE)
        
        # Get module ID
        module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
        
        # r variable with selected thesaurus items
        r[[paste0(prefix, "_thesaurus_items_selected")]] <- list()
        
        ##########################################
        # Render options card                    #
        ##########################################
        
        output[[paste0(prefix, "_options_card")]] <- renderUI({
          make_card(tagList(translate(language, "module_options"), span(paste0(" (ID = ", module_id, ")"), style = "font-size: 15px;")),
            div(
              shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_module_options_action")), value = "add", options = list(
                list(key = "add", text = translate(language, "add_module_element")),
                list(key = "datatable", text = translate(language, "datatable_module_elements"))
              ), className = "inline_choicegroup"),
              
              # Add a module element
              shiny::conditionalPanel(
                condition = paste0("input.", prefix, "_module_options_action == 'add'"), ns = ns,
                div(
                  shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 20),
                    make_textfield(language, ns, label = "name", id = paste0(prefix, "_module_options_add_name"), width = "300px"),
                    make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_add_plugin"), 
                      options = tibble_to_list(r$plugins %>% dplyr::filter(!deleted, module_type == paste0(prefix, "_data")), "id", "name", rm_deleted_rows =  TRUE),
                      width = "300px"),
                    make_dropdown(language, ns, label = "thesaurus", id = paste0(prefix, "_module_options_add_thesaurus"), width = "300px",
                      options = tibble_to_list(r$thesaurus %>% dplyr::filter(!deleted), "id", "name", rm_deleted_rows =  TRUE))
                  ),
                  make_dropdown(language, ns, label = "thesaurus_items", id = paste0(prefix, "_module_options_add_thesaurus_items_selected")),
                  div(DT::DTOutput(ns(paste0(prefix, "_module_options_add_thesaurus_items"))), style = "margin-top: 15px; margin-bottom: -5px;"),
                  shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_add")), translate(language, "add"))
                )
              ),
              
              # Management datatable
              shiny::conditionalPanel(
                condition = paste0("input.", prefix, "_module_options_action == 'datatable'"), ns = ns,
                div(
                  shiny.fluent::Stack(
                    horizontal = TRUE, tokens = list(childrenGap = 20),
                      make_dropdown(language, ns, label = "module_element", id = paste0(prefix, "_module_options_management_module_element"), width = "300px"),
                      make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_management_plugin"), width = "300px"),
                      make_dropdown(language, ns, label = "display_order", id = paste0(prefix, "_module_options_management_display_order"), width = "300px")
                  ),
                  div(DT::DTOutput(ns(paste0(prefix, "_module_options_management_datatable"))), style = "margin-top: 15px; margin-bottom: -5px;"),
                  shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_save")), translate(language, "save"))
                )
              )
            )
          )
        })
      })
      
        ##########################################
        # Options card / 1) Add module element   #
        ##########################################
        
          ##########################################
          # Options card / load thesaurus          #
          ##########################################
        
          # When the thesaurus is chosen
          observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus")]], {
            r[[paste0(prefix, "_thesaurus_items")]] <- settings_modules_thesaurus_cache(r, prefix, page_id = id, thesaurus_id = input[[paste0(prefix, "_module_options_add_thesaurus")]])
            
            # Reset items dropdown
            shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
            
            observeEvent(r[[paste0(prefix, "_thesaurus_items")]], {
              
              # Datatable state
              page_length <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$length)
              start <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$start)
              
              output[[paste0(prefix, "_module_options_add_thesaurus_items")]] <- DT::renderDT(
                settings_modules_datatable_data(ns, r, type = "thesaurus_items", prefix, data = r[[paste0(prefix, "_thesaurus_items")]],
                  new_colnames = c(translate(language, "id"), translate(language, "thesaurus_id"), translate(language, "item_id"),
                    translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
                    translate(language, "datetime"), translate(language, "action"))),
                options = list(
                  dom = "<'datatable_length'l><'top'ft><'bottom'p>",
                  stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
                  pageLength = page_length, displayStart = start,
                  language = list(
                    paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
                    search = translate(language, "DT_search"),
                    lengthMenu = translate(language, "DT_length")),
                  columnDefs = list(
                    list(className = "dt-center", targets = c(0, 1, 2, -1))#,
                    # list(sortable = FALSE, targets = c())
                )),
                rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
                editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 5, 6, 7, 8))),
                callback = datatable_callback()
              )
            })
          })
        
          # When a cell of the thesaurus datatable is edited
          observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]], {
            edit_info <- input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]]
            r[[paste0(prefix, "_thesaurus_items")]] <- DT::editData(r[[paste0(prefix, "_thesaurus_items")]], edit_info, rownames = FALSE)
          })
        
          ##########################################
          # Options card / add & rm thesaurus item #
          ##########################################
          
            # When thesaurus item add action button is clicked
            observeEvent(input[[paste0(prefix, "_item_selected")]], {
              link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_selected")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_selected")]])))
              
              value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
              if (link_id_filter %not_in% value) value <- c(value, link_id_filter)
              options <- tibble_to_list(
                r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
                # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
                "id", "name", rm_deleted_rows = TRUE
              )
              
              shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
                options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
            })
            
            # When thesaurus item remove action button is clicked
            observeEvent(input[[paste0(prefix, "_item_removed")]], {
              link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_removed")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_removed")]])))
      
              value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
              value <- value[!value %in% link_id_filter]
              options <- tibble_to_list(
                r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
                # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
                "id", "name", rm_deleted_rows = TRUE
              )
      
              shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
                options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
            })
          
            ##########################################
            # Options card / Add module element      #
            ##########################################
        
              observeEvent(input[[paste0(prefix, "module_options_add")]], {
                
                fields_check <- TRUE
                if (is.null(input[[paste0(prefix, "_module_options_add_plugin")]]) | 
                    is.null(input[[paste0(prefix, "_module_options_add_thesaurus")]]) | 
                    is.null(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]])) fields_check <- FALSE
                
                if (!fields_check){
                  output$warnings1 <- renderUI( div(shiny.fluent::MessageBar(translate(language, "fields_empty"), messageBarType = 3), style = "margin-top:10px;"))
                  shinyjs::show("warnings1")
                  shinyjs::delay(3000, shinyjs::hide("warnings1"))
                }
                req(fields_check)
                
                module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
                new_name <- input[[paste0(prefix, "_module_options_add_name")]]
                name_check <- FALSE
                if (!is.null(new_name)){
                  if (new_name != "") name_check <- TRUE
                }
                if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = translate(language, "provide_valid_name"))
                if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = NULL)
                
                req(name_check)
                
                distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", prefix, "_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
                
                if (new_name %in% distinct_names){
                  output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
                  shinyjs::show("warnings2")
                  shinyjs::delay(3000, shinyjs::hide("warnings2"))
                }
                req(new_name %not_in% distinct_names)
                
                plugin_id <- as.integer(input[[paste0(prefix, "_module_options_add_plugin")]])
                last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
                last_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
                last_group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
                new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted)
                
                sapply(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]], function(item){
                  last_id <<- last_id + 1
                  thesaurus_item_id <- as.integer(item)
                  thesaurus_item <- r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id == thesaurus_item_id)
                  # If display name is empty, take original thesaurus name
                  thesaurus_item_display_name <- thesaurus_item %>% 
                    dplyr::mutate(final_name = dplyr::case_when(display_name != "" ~ display_name, TRUE ~ name)) %>%
                    dplyr::select(final_name) %>% dplyr::pull() %>% as.character()
                  thesaurus_item_unit <- thesaurus_item %>% dplyr::select(unit) %>% dplyr::pull() %>% as.character()
  
                  new_data <<- new_data %>% dplyr::bind_rows(
                  tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted,
                      last_id, new_name, last_group_id + 1, module_id, plugin_id, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE))
                })
  
                DBI::dbAppendTable(r$db, "patient_lvl_module_elements", new_data)
                
                # Reset dropdowns except thesaurus
                shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_plugin"), value = NULL)
                shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
                
                output$warnings4 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "module_element_added"), messageBarType = 4), style = "margin-top:10px;"))
                shinyjs::show("warnings4")
                shinyjs::delay(3000, shinyjs::hide("warnings4"))
              })

          ##########################################
          # Options card / 2) Elements mngt        #
          ##########################################
            
            observeEvent(input[[paste0(prefix, "_module_options_action")]], {
              req(input[[paste0(prefix, "_module_options_action")]] == "datatable")
              
              module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
              r[[paste0(prefix, "_module_elements")]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE ORDER BY id"))
              
              options <- tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(name) %>% dplyr::slice(1) %>% dplyr::select(name), "name", "name")
              shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_module_element"),
                options = options,
                value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
            })
            
            observeEvent(input[[paste0(prefix, "_module_options_management_module_element")]], {
              
              data <- r[[paste0(prefix, "_module_elements")]] %>% dplyr::filter(name == input[[paste0(prefix, "_module_options_management_module_element")]])
              
              # Update plugin dropdown
              shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_plugin"),
                options = tibble_to_list(r$plugins %>% dplyr::select(name, id), "id", "name"),
                value = data %>% dplyr::slice(1) %>% dplyr::pull(plugin_id))
              
              # Update display order dropdown
              shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_display_order"),
                options = tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(display_order) %>% dplyr::slice(1), "display_order", "display_order"),
                value = data %>% dplyr::slice(1) %>% dplyr::pull(display_order))
              
              # Render thesaurus items datatable
              output[[paste0(prefix, "_module_options_management_datatable")]] <- DT::renderDT(
                settings_modules_datatable_data(ns, r, type = "elements_management", prefix, data = data,
                  new_colnames = c(translate(language, "id"), translate(language, "thesaurus_item"), translate(language, "thesaurus_item_display_name"),
                    translate(language, "thesaurus_item_unit"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))),
                options = list(
                  dom = "<'top't><'bottom'p>",
                  # stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
                  # pageLength = page_length, displayStart = start,
                  language = list(
                    paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
                    search = translate(language, "DT_search"),
                    lengthMenu = translate(language, "DT_length")),
                  columnDefs = list(
                    list(className = "dt-center", targets = c(0, 4, -1, -2)),
                    list(sortable = FALSE, targets = c(-1))
                  )),
                rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
                editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 4, 5, 6, 7, 8))),
                callback = datatable_callback()
              )
            })
            
  })
}