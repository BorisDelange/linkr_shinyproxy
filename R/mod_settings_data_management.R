#' settings_data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    
    # Dropdowns shown in datatable for each page
    dropdowns <- tibble::tribble(~page_name, ~dropdowns,
      "settings/data_sources", "",
      "settings/datamarts", "data_source",
      "settings/studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "settings/subsets", "study",
      "settings/thesaurus", "data_source")
    
    ##########################################
    # Settings / Data sources                #
    ##########################################
    
    if (page == "settings/data_sources"){
      prefix <- "data_sources"
      div(class = "main",
        settings_default_elements(ns, prefix),
        settings_toggle_card(language, ns, cards = list(
          list(key = paste0(prefix, "_creation_card"), label = "create_data_source"),
          list(key = paste0(prefix, "_datatable_card"), label = "data_sources_management"))),
        settings_creation_card(
          language, ns, title = "create_data_source", prefix = prefix,
          textfields = c("name", "description"), textfields_width = "300px"),
        settings_datatable_card(language, ns, title = "data_sources_management", prefix = prefix)
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      prefix <- "datamarts"
      div(class = "main",
        settings_default_elements(ns, prefix),
        settings_toggle_card(language, ns, cards = list(
          list(key = paste0(prefix, "_creation_card"), label = "create_datamart"),
          list(key = paste0(prefix, "_datatable_card"), label = "datamarts_management"),
          list(key = paste0(prefix, "_edit_code_card"), label = "edit_datamart_code"),
          list(key = paste0(prefix, "_options_card"), label = "datamart_options"))),
        settings_creation_card(
          language, ns, title = "create_datamart", prefix = prefix,
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
        uiOutput(ns(paste0(prefix, "_edit_code_card"))),
        uiOutput(ns(paste0(prefix, "_options_card"))),
        settings_datatable_card(language, ns, title = "datamarts_management", prefix = prefix)
      ) -> result
    }
    
    ##########################################
    # Settings / Studies                     #
    ##########################################
    
    if (page == "settings/studies"){
      prefix <- "studies"
      div(class = "main",
        settings_default_elements(ns, prefix),
        settings_toggle_card(language, ns, cards = list(
          list(key = paste0(prefix, "_creation_card"), label = "create_study"),
          list(key = paste0(prefix, "_datatable_card"), label = "studies_management"),
          list(key = paste0(prefix, "_options_card"), label = "study_options")
        )),
        settings_creation_card(
          language, ns, title = "create_study", prefix = "studies",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(),
          dropdowns_width = "300px"),
        settings_datatable_card(language, ns, title = "studies_management", prefix = prefix),
        uiOutput(ns(paste0(prefix, "_options_card")))
      ) -> result
    }
    
    ##########################################
    # Settings / Subsets                     #
    ##########################################
    
    if (page == "settings/subsets"){
      prefix <- "subsets"
      div(class = "main",
        settings_default_elements(ns, prefix),
        settings_toggle_card(language, ns, cards = list(
          list(key = paste0(prefix, "_creation_card"), label = "create_subset"),
          list(key = paste0(prefix, "_datatable_card"), label = "subsets_management"),
          list(key = paste0(prefix, "_edit_code_card"), label = "edit_subset_code")
        )),
        settings_creation_card(
          language, ns, title = "create_subset", prefix = prefix,
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
        uiOutput(ns(paste0(prefix, "_edit_code_card"))),
        settings_datatable_card(language, ns, title = "subsets_management", prefix = prefix)
      ) -> result
    }
    
    ##########################################
    # Settings / Thesaurus                   #
    ##########################################
    
    if (page == "settings/thesaurus"){
      div(class = "main",
        settings_default_elements(ns, "thesaurus"),
        settings_toggle_card(language, ns, cards = list(
          list(key = "thesaurus_creation_card", label = "create_thesaurus"),
          list(key = "thesaurus_datatable_card", label = "thesaurus_management_card"),
          list(key = "thesaurus_items_datatable_card", label = "thesaurus_items_management_card"),
          list(key = "thesaurus_edit_code_card", label = "edit_thesaurus_code")
        )),
        settings_creation_card(
          language, ns, title = "create_thesaurus", prefix = "thesaurus",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
        settings_datatable_card(language, ns, title = "thesaurus_management", prefix = "thesaurus"),
        settings_datatable_card(language, ns, title = "thesaurus_items_management", prefix = "thesaurus_items"),
        uiOutput(ns("thesaurus_edit_code_card")),
        uiOutput(ns("thesaurus_options_card"))
      ) -> result
    }
  }
  
  ##########################################
  # Fluid                                  #
  ##########################################
  
  if (page_style == "fluid"){
    
  }
  
  result
}
    
#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#' settings_studies Server Functions
#'
#' @noRd 
# mod_settings_data_management_server <- function(id, page_style, page){

mod_settings_data_management_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Toggles ids
    toggles <- c("creation_card", "datatable_card", "edit_code_card", "options_card")
    
    data_management_elements <- c("data_sources", "datamarts", "studies", "subsets", "patient_lvl_module_families", "aggregated_module_families")
    
    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~page_name, ~dropdowns,
      "data_sources", "",
      "datamarts", "data_source",
      "studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "subsets", c("datamart", "study"),
      "thesaurus", "data_source")
    
    # Prefix used for inputs
    # It also corresponds to database tables names
    prefix <- switch(substr(id, nchar("settings_") + 1, nchar(id)),
      "data_sources" = "data_sources", "datamarts" = "datamarts", "studies" = "studies", "subsets" = "subsets",
      "thesaurus" = c("thesaurus", "thesaurus_items"))
    
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    sapply(prefix, function(prefix){
    
      sapply(toggles, function(toggle){
        observeEvent(input[[paste0(prefix, "_", toggle, "_toggle")]], if(input[[paste0(prefix, "_", toggle, "_toggle")]]) shinyjs::show(paste0(prefix, "_", toggle)) else shinyjs::hide(paste0(prefix, "_", toggle)))
      })
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
      # Update dropdowns with reactive data
      sapply(c(data_management_elements), function(data_management_element){
        observeEvent(r[[data_management_element]], {
          options <- tibble_to_list(r[[data_management_element]], "id", "name", rm_deleted_rows = TRUE)
          shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_", id_get_other_name(data_management_element, "singular_form")),
            options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
        })
      })
      
      # When add button is used
      observeEvent(input[[paste0(prefix, "_add")]], {
        
        # Check if required fields are filled (name is required, description is not)
        # We can add other requirements (eg characters only)
        new_name <- input[[paste0(prefix, "_name")]]
        name_check <- FALSE
        if (!is.null(new_name)){
          if (new_name != "") name_check <- TRUE
        }
        if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = translate(language, "provide_valid_name"))
        if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = NULL)
        
        req(name_check)
        
        distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", prefix, " WHERE deleted IS FALSE")) %>% dplyr::pull()

        if (new_name %in% distinct_names){
          message_bar(output, 2, "name_already_used", "severeWarning", language)
        }
        req(new_name %not_in% distinct_names)

        # Check if dropdowns are not empty
        dropdowns_check <- TRUE
        sapply(dropdowns %>% dplyr::filter(page_name == prefix) %>% dplyr::pull(dropdowns) %>% unlist(), function(dropdown){
          if (!is.null(input[[paste0(prefix, "_", dropdown)]])){
            if (input[[paste0(prefix, "_", dropdown)]] == "") dropdowns_check <<- FALSE
          }
        })
        if (!dropdowns_check) message_bar(output, 2, "dropdown_empty", "severeWarning", language)
        req(dropdowns_check)

        last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", prefix)) %>% dplyr::pull()

        new_data <- settings_new_data(prefix, data = list(
          id = last_row + 1,
          name = as.character(new_name),
          description = coalesce2("char", input[[paste0(prefix, "_description")]]),
          creator_id = as.numeric(r$user_id),
          datetime = as.character(Sys.time()),
          deleted = FALSE,
          # If prefix == thesaurus, data_source_id is character (multiSelect = TRUE)
          data_source_id = ifelse(prefix == "thesaurus", coalesce2("char", toString(input[[paste0(prefix, "_data_source")]])), coalesce2("int", input[[paste0(prefix, "_data_source")]])),
          datamart_id = coalesce2("int", input[[paste0(prefix, "_datamart")]]),
          study_id = coalesce2("int", input[[paste0(prefix, "_study")]]),
          patient_lvl_module_family_id = coalesce2("int", input[[paste0(prefix, "_patient_lvl_module_family")]]),
          aggregated_module_family_id = coalesce2("int", input[[paste0(prefix, "_aggregated_module_family")]])
        ))

        DBI::dbAppendTable(r$db, prefix, new_data)
        r[[prefix]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, " WHERE deleted IS FALSE ORDER BY id"))
        r[[paste0(prefix, "_temp")]] <- r[[prefix]]  %>% dplyr::mutate(modified = FALSE)

        # Add new rows in code table & options table
        # Add default subsets when creating a new study
        last_row_code <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM code") %>% dplyr::pull()
        last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
        last_row_subsets <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM subsets") %>% dplyr::pull()

        if (prefix %in% c("datamarts", "thesaurus")){
          DBI::dbAppendTable(r$db, "code",
          tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
            last_row_code + 1, prefix, last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
          # Update r$code
          update_r(r, "code", language)
        }

        if (prefix == "datamarts"){
          DBI::dbAppendTable(r$db, "options",
            tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              last_row_options + 1, "datamart", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
              last_row_options + 2, "datamart", last_row + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE))
          # Update r$options
          update_r(r, "options", language)
        }

        if (prefix == "studies"){
          DBI::dbAppendTable(r$db, "options",
            tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              last_row_options + 1, "study", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))
          # Add rows in subsets table, for inclusion / exclusion subsets
          # Add also code corresponding to each subset
          DBI::dbAppendTable(r$db, "subsets",
            tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id,  ~datetime, ~deleted,
              last_row_subsets + 1, translate(language, "subset_all_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
              last_row_subsets + 2, translate(language, "subset_included_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
              last_row_subsets + 3, translate(language, "subset_excluded_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE))
          DBI::dbAppendTable(r$db, "code",
            tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
              last_row_code + 1, "subset", last_row_subsets + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
              last_row_code + 2, "subset", last_row_subsets + 2, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
              last_row_code + 3, "subset", last_row_subsets + 3, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))

          # Update r$options, r$code & r$subsets
          update_r(r, "options", language)
          update_r(r, "subsets", language)
          update_r(r, "code", language)
        }

        # Hide creation card & options card, show management card
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_options_card_toggle"), value = FALSE)
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_creation_card_toggle"), value = FALSE)
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_datatable_card_toggle"), value = TRUE)

        message_bar(output, 1, paste0(id_get_other_name(prefix, "singular_form"), "_added"), "success", language)

        # Reset textfields
        sapply(c("name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_", name), value = ""))
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
        # observeEvent(input[[paste0(prefix, "_dropdown_updated")]], {
        #   value <- input[[paste0(prefix, "_dropdown_updated")]]
        #   colname <- paste0(id_get_other_name(gsub("[0-9]", "", value), "singular_form"), "_id")
        #   id <- gsub("[a-zA-Z_]", "", value)
        #   output$test <- renderText(paste0("value = ", value, " // colname = ", colname, " // id = ", id,
        #   " // data = ", r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), colname]],
        #   " // new_value = ", input[[value]]))
        #   
        #   r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), colname]] <- input[[value]]
        #   r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), "modified"]] <- TRUE
        # })  
      
        if (prefix %not_in% c("thesaurus", "thesaurus_items")){
          observeEvent(r[[prefix]], {
            sapply(r[[prefix]] %>% dplyr::pull(id), function(id){
              sapply(dropdowns %>% dplyr::filter(page_name == prefix) %>% dplyr::pull(dropdowns) %>% unlist(), function(dropdown){
                observeEvent(input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]], {
                  # When we load a page, every dropdown triggers the event
                  # Change temp variable only if new value is different than old value
                  old_value <- r[[paste0(prefix, "_temp")]][[which(r[[paste0(prefix, "_temp")]]["id"] == id), paste0(dropdown, "_id")]]
                  # If thesaurus, data_source_id can accept multiple values (converting to string)
                  new_value <- ifelse(prefix == "thesaurus", toString(input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]]),
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
          if (duplicates > 0) message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
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
          message_bar(output, 2, "modif_saved", "success", language)
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
          message_bar(output, 3, paste0(id_get_other_name(prefix, "singular_form"), "_deleted"), "severeWarning", language)
        })
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
        observeEvent(input[[paste0(prefix, "_options")]], {
          # Show options toggle
          shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_options_card_toggle"), value = TRUE)
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
          
          message_bar(output, 4, "modif_saved", "success", language)
        })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_edit_code")]], {
        shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_edit_code_card_toggle"), value = TRUE)
        output[[paste0(prefix, "_edit_code_card")]] <- renderUI({
          category_filter <- id_get_other_name(id, "singular_form")
          link_id_filter <- as.integer(substr(input[[paste0(prefix, "_edit_code")]], nchar(paste0(prefix, "_edit_code_")) + 1, nchar(input[[paste0(prefix, "_edit_code")]])))
          # Save ID value in r variable, to get this during code execution
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$thesaurus_id <- NA_integer_
          if (prefix == "datamarts") r$datamart_id <- link_id_filter
          if (prefix == "subsets"){
            # Get subset_id & datamart_id
            r$datamart_id <- r$studies %>% 
              dplyr::filter(id == (r$subsets %>% dplyr::filter(id == link_id_filter) %>% dplyr::pull(study_id))) %>%
              dplyr::pull(datamart_id)
            r$subset_id <- link_id_filter
          } 
          if (prefix == "thesaurus") r$thesaurus_id <- link_id_filter
          
          code <- r$code %>% dplyr::filter(category == category_filter & link_id == link_id_filter) %>% dplyr::pull(code)
          settings_edit_code_card(language, ns, type = "code", code = code, link_id = link_id_filter, title = paste0("edit_", category_filter, "_code"), prefix = prefix)
        })
        output[[paste0(prefix, "_code_result")]] <- renderText("")
      })
      
      observeEvent(input[[paste0(prefix, "_edit_code_save")]], {
        category_filter <- id_get_other_name(id, "singular_form")
        link_id_filter <- as.integer(substr(input[[paste0(prefix, "_edit_code")]], nchar(paste0(prefix, "_edit_code_")) + 1, nchar(input[[paste0(prefix, "_edit_code")]])))
        # Reload r$code before querying
        r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
        code_id <- r$code %>% dplyr::filter(category == category_filter, link_id == link_id_filter) %>% dplyr::pull(id)
        # Replace ' with '' and store in the database
        DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input[[paste0(prefix, "_ace_edit_code")]], "'", "''"), "' WHERE id = ", code_id)) -> query
        DBI::dbClearResult(query)
        r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
        
        message_bar(output, 4, "modif_saved", "success", language)
      })
      
      observeEvent(input[[paste0(prefix, "_execute_code")]], {
        output[[paste0(prefix, "_code_result")]] <- renderText({
          # Replace %datamart_id% from code to real r$datamart_id
          code <- isolate(input[[paste0(prefix, "_ace_edit_code")]]) %>%
            stringr::str_replace_all("%datamart_id%", as.character(isolate(r$datamart_id))) %>%
            stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
            stringr::str_replace_all("%thesaurus_id%", as.character(isolate(r$thesaurus_id)))
          # Change this option to display correctly tibble in textbox
          eval(parse(text = "options('cli.num_colors' = 1)"))
          # Capture console output of our code
          captured_output <- capture.output(
            tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))
          )
          # Restore normal value
          eval(parse(text = "options('cli.num_colors' = NULL)"))
          # Display result
          paste(captured_output, collapse = "\n")
        })
      })
      
      ##########################################
      # Load sub datatable with action button  #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_sub_datatable")]], {
        shiny.fluent::updateToggle.shinyInput(session, "thesaurus_items_datatable_card_toggle", value = TRUE)
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
