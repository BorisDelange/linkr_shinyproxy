#' settings_data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), language = "EN"){
  ns <- NS(id)
  result <- div()
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datamarts", "data_source",
    "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
    "settings_subsets", "study",
    "settings_thesaurus", "data_source")
  
  ##########################################
  # Data management / Data sources         #
  ##########################################
  
  if (id == "settings_data_sources"){
    prefix <- "data_sources"
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_data_source"),
        list(key = "datatable_card", label = "data_sources_management"))),
      render_settings_creation_card(
        language = language, ns = ns, title = "create_data_source",
        textfields = c("name", "description"), textfields_width = "300px"),
      settings_datatable_card(language, ns, title = "data_sources_management", prefix = prefix)
    ) -> result
  }
  
  ##########################################
  # Data management / Datamarts            #
  ##########################################
  
  if (id == "settings_datamarts"){
    prefix <- "datamarts"
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_datamart"),
        list(key = "datatable_card", label = "datamarts_management"),
        list(key = "edit_code_card", label = "edit_datamart_code"),
        list(key = "options_card", label = "datamart_options"))),
      render_settings_creation_card(
        language = language, ns = ns, title = "create_datamart",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns(paste0(prefix, "_options_card"))),
      settings_datatable_card(language, ns, title = "datamarts_management", prefix = prefix)
    ) -> result
  }
  
  ##########################################
  # Data management / Studies              #
  ##########################################
  
  if (id == "settings_studies"){
    prefix <- "studies"
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_study"),
        list(key = "datatable_card", label = "studies_management"),
        list(key = "options_card", label = "study_options")
      )),
      render_settings_creation_card(
        language = language, ns = ns, title = "create_study",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      settings_datatable_card(language, ns, title = "studies_management", prefix = prefix),
      uiOutput(ns(paste0(prefix, "_options_card")))
    ) -> result
  }
  
  ##########################################
  # Data management / Subsets              #
  ##########################################
  
  if (id == "settings_subsets"){
    prefix <- "subsets"
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_subset"),
        list(key = "datatable_card", label = "subsets_management"),
        list(key = "edit_code_card", label = "edit_subset_code")
      )),
      render_settings_creation_card(
        language = language, ns = ns, title = "create_subset",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      uiOutput(ns("edit_code_card")),
      settings_datatable_card(language, ns, title = "subsets_management", prefix = prefix)
    ) -> result
  }
  
  ##########################################
  # Data management / Thesaurus            #
  ##########################################
  
  if (id == "settings_thesaurus"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "creation_card", label = "create_thesaurus"),
        list(key = "datatable_card", label = "thesaurus_management_card"),
        list(key = "items_datatable_card", label = "thesaurus_items_management_card"),
        list(key = "edit_code_card", label = "edit_thesaurus_code")
      )),
      render_settings_creation_card(
        language = language, ns = ns, title = "create_thesaurus",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
      settings_datatable_card(language, ns, title = "thesaurus_management", prefix = "thesaurus"),
      settings_datatable_card(language, ns, title = "thesaurus_items_management", prefix = "thesaurus_items"),
      uiOutput(ns("edit_code_card")),
      uiOutput(ns("thesaurus_options_card"))
    ) -> result
  }
  
  result
}
    
#' settings_studies Server Functions
#'
#' @param id ID of the module (character)
#' @param r Shiny reactive value
#' @param language Language used (character)
#' @noRd 

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Toggles IDs
    toggles <- c("creation_card", "datatable_card", "edit_code_card", "options_card")
    
    # To delete asap...
    data_management_elements <- c("data_sources", "datamarts", "studies", "subsets", "patient_lvl_module_families", "aggregated_module_families")
    
    # To move, after remove prefix
    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "data_sources", "",
      "datamarts", "data_source",
      "studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "subsets", c("datamart", "study"),
      "thesaurus", "data_source")
    
    # Prefix used for inputs
    # It also corresponds to database tables names
    prefix <- switch(substr(id, nchar("settings_") + 1, nchar(id)),
      "data_sources" = "data_sources", "datamarts" = "datamarts", "studies" = "studies", "subsets" = "subsets",
      "thesaurus" = "thesaurus")
    
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    # To remove asap
    sapply(prefix, function(prefix){
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    sapply(c("data_sources", "datamarts", "studies", "subsets", "patient_lvl_module_families", "aggregated_module_families"), 
      function(data_var){
        observeEvent(r[[data_var]], {
          # Convert options to list
          options <- convert_tibble_to_list(data = r[[data_var]], key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var),
            options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
        })
      })
    
    # When add button is clicked
    observeEvent(input$add, {
      
      # Create a list with new data
      # If page = thesaurus, data_source is character, not integer (multiple choices)
      new_data <- list()
      if (id == "settings_thesaurus") new_data_var <- c("name" = "char", "description" = "char", "data_source" = "char")
      else new_data_var <- c("name" = "char", "description" = "char", "data_source" = "int", "datamart" = "int", 
        "study" = "int", "patient_lvl_module_family" = "int", "aggregated_module_family" = "int")
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
      })

      add_settings_new_data(session = session, output = output, r = r, language = language, id = id, data = new_data,
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
    })
    
    ##########################################
    # Data management / Elements management  #
    ##########################################
    
      # Datatable states
      # observeEvent(r[[prefix]], {
      #   r[[paste0(id, "_", prefix, "_management_datatable_state")]] <- list(
      #     length = 10, start = 0, search = ""
      #   )
      # })
      
      ##########################################
      # Generate datatable                     #
      ##########################################
    
        # If r$... variable changes
        observeEvent(r[[paste0(prefix, "_temp")]], {
          # Restore datatable state
          page_length <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$length)
          start <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$start)
          search_recorded <- ""
          
          output[[paste0(prefix, "_management_datatable")]] <- DT::renderDT(
            # This function generates the data for the datatable
            settings_datatable(
              id = id, prefix = prefix,
              data = settings_datatable_data(prefix, isolate(r)), ns = ns, r = r, data_variables = data_management_elements,
              dropdowns = switch(prefix,
                "data_sources" = "",
                "datamarts" = "",
                # "datamarts" = c("data_source_id" = "data_sources"),
                # "studies" = c("datamart_id" = "datamarts", "patient_lvl_module_family_id" = "patient_lvl_module_families", "aggregated_module_family_id" = "aggregated_module_families"),
                "studies" = c("patient_lvl_module_family_id" = "patient_lvl_module_families", "aggregated_module_family_id" = "aggregated_module_families"),
                "subsets" = "",
                # "subsets" = c("study_id" = "studies"),
                "thesaurus" = c("data_source_id" = "data_sources"),
                "thesaurus_items" = ""),
              action_buttons = switch(prefix,
                "data_sources" = "delete",
                "datamarts" = c("delete", "edit_code", "options"),
                "studies" = c("delete", "options"),
                "subsets" = c("delete", "edit_code"),
                "thesaurus" = c("delete", "edit_code", "sub_datatable"),
                "thesaurus_items" = ""
                ),
              new_colnames = id_get_other_name(prefix, "colnames_text_version", language = language)),
            # Options of the datatable
            # We use a function (data_management_datatable_options) for this module
            options = list(dom = "<'datatable_length'l><'top'ft><'bottom'p>",
              stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
              pageLength = page_length, displayStart = start, #search = list(search = ""),
              columnDefs = list(
                # -1 : action column / -2 : datetime column
                list(width = "80px", targets = -1), list(width = "130px", targets = -2),
                list(sortable = FALSE, targets = data_management_datatable_options(settings_datatable_data(prefix, r), prefix, "non_sortable"))),
              language = list(
                paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
                search = translate(language, "DT_search"),
                lengthMenu = translate(language, "DT_length"))
            ),
            rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
            editable = list(target = "cell", disable = list(columns = data_management_datatable_options(settings_datatable_data(prefix, r), id, "disable"))),
            callback = datatable_callback()
          )
        })
      
      ##########################################
      # Datatable state changed                #
      ##########################################

      # observeEvent(input[[paste0(prefix, "_management_datatable_state")]], {
      #   r[[paste0(id, "_", prefix, "_management_datatable_state")]] <- list(
      #     length = input[[paste0(prefix, "_management_datatable_state")]]$length,
      #     start = input[[paste0(prefix, "_management_datatable_state")]]$start,
      #     search = input[[paste0(prefix, "_management_datatable_state")]]$search[1]
      #   )
      # })
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
    
      # Each time a row is updated, modify temp variable
        observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
          edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]
          # edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1
          r[[paste0(prefix, "_temp")]] <- DT::editData(r[[paste0(prefix, "_temp")]], edit_info, rownames = FALSE)
          # Store that this row has been modified
          r[[paste0(prefix, "_temp")]][[edit_info$row, "modified"]] <- TRUE
        })
    
      # Each time a dropdown is updated, modify temp variable
        if (prefix %not_in% c("thesaurus", "thesaurus_items")){
          observeEvent(r[[prefix]], {
            sapply(r[[prefix]] %>% dplyr::pull(id), function(id){
              sapply(dropdowns %>% dplyr::filter(id == prefix) %>% dplyr::pull(dropdowns) %>% unlist(), function(dropdown){
                observeEvent(input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]], {
                  # When we load a page, every dropdown triggers the event
                  # Change temp variable only if new value is different than old value
                  old_value <- r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), paste0(dropdown, "_id")]]
                  # If thesaurus, data_source_id can accept multiple values (converting to string)
                  new_value <- ifelse(prefix == "thesaurus", toString(input[[paste0("data_sources", id)]]),
                    as.integer(input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]]))
                  if (new_value != old_value){
                    r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), paste0(dropdown, "_id")]] <- new_value
                    # Store that this row has been modified
                    r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), "modified"]] <- TRUE
                  }
                })
              })
            })
          })
        }
      
        observeEvent(input[[paste0(prefix, "_management_save")]], {
          # Make sure there's no duplicate in names
          duplicates <- 0
          # Duplicates are allowed in thesaurus_items
          if (prefix != "thesaurus_items"){
            duplicates <- r[[paste0(prefix, "_temp")]] %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
              dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
          }
          if (duplicates > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
          req(duplicates == 0)
          
          # Save changes in database
          ids_to_del <- r[[paste0(prefix, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
          DBI::dbSendStatement(r$db, paste0("DELETE FROM ", prefix, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
          DBI::dbClearResult(query)
          DBI::dbAppendTable(r$db, prefix, r[[paste0(prefix, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified))
          
          # Reload r variable
          # r[[prefix]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, " WHERE deleted IS FALSE ORDER BY id))
          # r[[paste0(prefix, "_temp")]] <- r[[prefix]] %>% dplyr::mutate(modified = FALSE)
          
          # Notification to user
          show_message_bar(output, 2, "modif_saved", "success", language)
        })
    
      ##########################################
      # Delete a row in datatable              #
      ##########################################
      
        # Indicate whether to close or not delete dialog box
        r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
        
        # Create & show dialog box 
        output[[paste0(prefix, "_delete_confirm")]] <- shiny.fluent::renderReact(settings_delete_react(prefix, ns, language, r[[paste0(prefix, "_delete_dialog")]]))
        
        # Whether to close or not delete dialog box
        observeEvent(input[[paste0(prefix, "_hide_dialog")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
        observeEvent(input[[paste0(prefix, "_delete_canceled")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
        observeEvent(input[[paste0(prefix, "_deleted_pressed")]], r[[paste0(prefix, "_delete_dialog")]] <<- TRUE)
        
        # When the delete is confirmed...
        observeEvent(input[[paste0(prefix, "_delete_confirmed")]], {
          
          # Close dialog box
          r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
          
          # Get the ID of row deleted
          deleted_pressed_value <- input[[paste0(prefix, "_deleted_pressed")]]
          row_deleted <- as.integer(substr(deleted_pressed_value, nchar(paste0(prefix, "_delete_")) + 1, nchar(deleted_pressed_value)))
          # Delete row in database
          DBI::dbSendStatement(r$db, paste0("UPDATE ", prefix, " SET deleted = TRUE WHERE id = ", row_deleted))
          # Update r vars (including temp variable, used in management datatables)
          r[[prefix]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, " WHERE deleted IS FALSE ORDER BY id"))
          r[[paste0(prefix, "_temp")]] <- r[[prefix]] %>% dplyr::mutate(modified = FALSE)
          
          # Notification to user
          show_message_bar(output, 3, paste0(id_get_other_name(prefix, "singular_form"), "_deleted"), "severeWarning", language)
        })
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
        observeEvent(input[[paste0(prefix, "_options")]], {
          # Show options toggle
          shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = TRUE)
          output[[paste0(prefix, "_options_card")]] <- renderUI({
            category <- id_get_other_name(id, "singular_form")
            link_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
            settings_options_card(language, ns, id, r, category_filter = category, link_id_filter = link_id,
              title = paste0(id_get_other_name(id, "singular_form"), "_options"), prefix = prefix)
          })
        })
        
        observeEvent(input[[paste0(prefix, "_options_save")]], {
          category_filter <- id_get_other_name(id, "singular_form")
          link_id_filter <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
          
          options <- r$options %>% dplyr::filter(category == category_filter, link_id == link_id_filter)
          options_by_cat <- id_get_other_name(id, "options_by_cat")
          
          if("show_only_aggregated_data" %in% options_by_cat){
            option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
            DBI::dbSendStatement(r$db, paste0("UPDATE options SET value_num = ", input[[paste0(prefix, "_", category_filter, "_show_only_aggregated_data")]], "
                                          WHERE id = ", option_id)) -> query
            DBI::dbClearResult(query)
            r$options <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE deleted IS FALSE ORDER BY id")
          }
          
          if ("user_allowed_read" %in% options_by_cat){
            users_already_allowed <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(value_num)
            users_allowed <- input[[paste0(prefix, "_", category_filter, "_users_allowed_read")]]
            last_row <- max(r[[prefix]]["id"])
            
            # Delete users not in the selected list
            rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
            rows_to_del <-
              options %>%
              dplyr::filter(name == "user_allowed_read" & value_num %not_in% users_allowed) %>%
              dplyr::pull(id)
            DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE id IN (", paste(rows_to_del, collapse = ","),")")) -> query
            DBI::dbClearResult(query)
            r$options <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE deleted IS FALSE ORDER BY id")
            
            # Add users in the selected list
            users_allowed <- users_allowed[!users_allowed %in% users_already_allowed]
            if (length(users_allowed) != 0){
              DBI::dbAppendTable(r$db, "options",
                tibble::tibble(id = ((last_row + 1) + (1:length(users_allowed))), category = category_filter, link_id = link_id_filter,
                  name = "user_allowed_read", value = "", value_num = users_allowed, creator_id = as.integer(r$user_id),
                  datetime = as.character(Sys.time()), deleted = FALSE))
            }
          }
          
          show_message_bar(output, 4, "modif_saved", "success", language)
        })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      # Button "Edit code" is clicked on the datatable
      observeEvent(input$edit_code, {
        
        # Display edit_code card
        shiny.fluent::updateToggle.shinyInput(session, "edit_code_card_toggle", value = TRUE)
        
        # Render UI of this edit_code card
        output$edit_code_card <- renderUI({
          
          # Get category & link_id variables, to update code table
          category <- get_singular(id, language)
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$thesaurus_id <- NA_integer_
          
          if (id == "settings_datamarts") r$datamart_id <- link_id
          if (id == "settings_thesaurus") r$thesaurus_id <- link_id
          if (id == "settings_subsets"){
            r$datamart_id <- 
              r$studies %>% 
              dplyr::filter(id == (r$subsets %>% dplyr::filter(id == !!link_id) %>% dplyr::pull(study_id))) %>%
              dplyr::pull(datamart_id)
            r$subset_id <- link_id
          } 
          
          # Get code from database
          code <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code)
          
          # Render UI
          render_settings_code_card(ns = ns, id = id, title = paste0("edit_", category, "_code"), code = code, link_id = link_id, language = language)
        })
        
        # Reset code_result textOutput
        output$code_result <- renderText("")
      })
      
      # When save button is clicked
      observeEvent(input$edit_code_save,
        save_settings_code(output = output, r = r, id = id, category = get_singular(id, language),
          code_id_input = input$edit_code, edited_code = input$ace_edit_code, language = "EN"))
      
      # When Execute code button is clicked
      observeEvent(input$execute_code, (
        output$code_result <- renderText(
          execute_settings_code(output = output, r = r, edited_code = isolate(input$ace_edit_code)))))
          
      
      ##########################################
      # Load sub datatable with action button  #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_sub_datatable")]], {
        shiny.fluent::updateToggle.shinyInput(session, "items_datatable_card_toggle", value = TRUE)
        link_id_filter <- as.integer(substr(input[[paste0(prefix, "_sub_datatable")]], nchar(paste0(prefix, "_sub_datatable_")) + 1, nchar(input[[paste0(prefix, "_sub_datatable")]])))
        # if (prefix == "thesaurus_items"){
          r$thesaurus_items <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", link_id_filter, " AND deleted IS FALSE ORDER BY id"))
          r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
        # }
      })
      
    })
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
