#' settings_modules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_modules_ui <- function(id, language){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Patient-lvl modules                    #
  ##########################################
  
  # if (id == "settings_modules_patient_lvl"){
  
    if (grepl("patient_lvl", id)) prefix <- "patient_lvl"
    if (grepl("aggregated", id)) prefix <- "aggregated"

    # Three distinct pages in the settings/modules page : module family, module & module element
    # For each "sub page", create a creation & a management cards
    # Add an option card for module family

    pages <- c("modules", "modules_families", "modules_elements")
    cards <- tagList()

    # We create one module by "sub page"

    sapply(pages, function(page){

      cards <<- tagList(cards,
        div(id = ns(paste0(page, "_creation_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_creation"), language = language)),
        div(id = ns(paste0(page, "_management_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_management"), language = language)))

      if (page == "modules_families") cards <<- tagList(cards,
        div(id = ns(paste0(page, "_options_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_options"), language = language)))
    })

    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "modules_creation_card", label = "modules_creation_card"),
        list(key = "modules_management_card", label = "modules_management_card"),
        list(key = "modules_families_creation_card", label = "modules_families_creation_card"),
        list(key = "modules_families_management_card", label = "modules_families_management_card"),
        list(key = "modules_families_options_card", label = "modules_families_options_card"),
        list(key = "modules_elements_creation_card", label = "modules_elements_creation_card"),
        list(key = "modules_elements_management_card", label = "modules_elements_management_card")
      )),
      cards
    ) -> result
  # }
  
  
  result
}

mod_settings_sub_modules_ui <- function(id, language){
  ns <- NS(id)
  
  result <- ""
  
  ##########################################
  # Patient-lvl & aggregated sub modules   #
  ##########################################
    
    if (grepl("patient_lvl", id)) page <- substr(id, nchar("settings_modules_patient_lvl_") + 1, nchar(id))
    if (grepl("aggregated", id)) page <- substr(id, nchar("settings_modules_aggregated_") + 1, nchar(id))
    
    if (page == "modules_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = c("module_family", "parent_module"), dropdowns_width = "300px") -> result
    }

    if (page == "modules_families_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module_family",
        textfields = c("name", "description"), textfields_width = "300px") -> result
    }

    if (page == "modules_elements_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module_element",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = c(), dropdowns_width = "300px") -> result
    }

    if (grepl("management", page)){
      render_settings_datatable_card(language = language, ns = ns, output_id = "management_datatable", title = page) -> result
    }

    if (page == "modules_families_options"){
      uiOutput(ns("options_card")) -> result
    }
  
  tagList(render_settings_default_elements(ns = ns), result) 
}

    
#' settings_modules Server Functions
#'
#' @noRd 

mod_settings_modules_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (grepl("patient_lvl", id)) prefix <- "patient_lvl"
    if (grepl("aggregated", id)) prefix <- "aggregated"
    
    # if (id == "settings_modules_patient_lvl"){
      
      toggles <- c(
        "modules_creation_card", "modules_management_card", 
        "modules_families_creation_card", "modules_families_management_card", "modules_families_options_card",
        "modules_elements_creation_card", "modules_elements_management_card")
      
      # Current page
      page <- substr(id, nchar(paste0("settings_modules_", prefix, "_")) + 1, nchar(id))
      
      # Corresponding table
      # Corresponding table in the database
      if (grepl("creation", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_creation")))
      if (grepl("management", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_management")))
      if (grepl("options", page)) table <- paste0(prefix, "_", substr(page, 1, nchar(page) - nchar("_options")))
      
      # Corresponding table in the database
      # if (grepl("creation", page)) table <- substr(page, 1, nchar(page) - nchar("_creation"))
      # if (grepl("management", page)) table <- substr(page, 1, nchar(page) - nchar("_management"))
      # if (grepl("options", page)) table <- substr(page, 1, nchar(page) - nchar("_options"))
      
      # Dropdowns used for creation card
      dropdowns <- ""
      if (page %in% c("modules_creation", "modules_management")) dropdowns <- c("module_family", "parent_module")
      
      ##########################################
      # Show or hide cards                     #
      ##########################################
      
      # Initiate vars
      
      # Used to communicate between modules and to show / hide cards
      r[[paste0(prefix, "_modules_toggle")]] <- 0L
      r[[paste0(prefix, "_modules_families_toggle")]] <- 0L
      r[[paste0(prefix, "_modules_elements_toggle")]] <- 0L
      
      # Used to send option link_id from one module to another
      r[[paste0(prefix, "_modules_families_options")]] <- 0L
      
      # Only for main users page (not for sub-pages)
      if (id == "settings_modules_patient_lvl" | id == "settings_modules_aggregated"){ 
        
        # Depending on user_accesses
        # observeEvent(r$user_accesses, {
        #   # Hide toggles if user has no access
        #   if ("users" %not_in% r$user_accesses) shinyjs::hide("toggles") else shinyjs::show("toggles")
        # })
        
        # Depending on toggles activated
        sapply(toggles, function(toggle){
          
          # If user has no access, hide card
          # observeEvent(r$user_accesses, if (toggle %not_in% r$user_accesses) shinyjs::hide(toggle))
          
          # If user has access, show or hide card when toggle is clicked
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            # if (toggle %in% r$user_accesses){
              if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
              else shinyjs::hide(toggle)
            # }
          })
        })
        
        # When a new user, a user status or a user access is added, close add card & show data management card
        sapply(c("modules", "modules_families", "modules_elements"), function(page){
          observeEvent(r[[paste0(prefix, "_", page, "_toggle")]], {
            if (r[[paste0(prefix, "_", page, "_toggle")]] != 0){
              shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_creation_card_toggle"), value = FALSE)
              shiny.fluent::updateToggle.shinyInput(session, paste0(page, "_management_card_toggle"), value = TRUE)}
          })
        })
        
        # observeEvent(r$users_statuses_options, {
        #   if (r$users_statuses_options > 0){
        #     shiny.fluent::updateToggle.shinyInput(session, "users_accesses_options_card_toggle", value = TRUE)
        #   }
        # })
      }
      
      ##########################################
      # Add a new element                      #
      ##########################################
      
      # Only for creation subpages
      if (grepl("creation", id)){

        # Update dropdowns with reactive data (module_family & parent_module dropdowns)
        
        observeEvent(r[[paste0(prefix, "_modules_families")]], {
          options <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules_families")]], key_col = "id", text_col = "name")
          shiny.fluent::updateDropdown.shinyInput(session, "module_family", options = options)
        })
        
        observeEvent(r[[paste0(prefix, "_modules")]], {
          options <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules")]], key_col = "id", text_col = "name", null_value = TRUE)
          shiny.fluent::updateDropdown.shinyInput(session, "parent_module", options = options)
        })

        # When add button is clicked
        observeEvent(input$add, {

          # If user has access
          # req(paste0(table, "_creation_card") %in% r$user_accesses)

          new_data <- list()

          new_data_var <- c("name" = "char", "description" = "char", "module_family" = "int", "parent_module" = "int")

          # Transform values of textfields & dropdowns to chosen variable type
          sapply(names(new_data_var),
            function(input_name){
              new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
            })
          
          # Add a display order, depending on last display order from the module family
          if (table %in% c("patient_lvl_modules", "aggregated_modules")){
            
            if (is.na(new_data$parent_module)) last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
              " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id IS NULL")) %>% dplyr::pull()
            
            else last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
              " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id = ", new_data$parent_module)) %>% dplyr::pull()
            
            new_data$display_order <- last_display + 1
          }

          # Required textfields
          required_textfields <- "name"

          # Fields requiring unique value
          req_unique_values <- "name"
          
          # Required dropdowns
          required_dropdowns <- "all"
          if (page == "modules_creation") required_dropdowns <- "module_family"
          
          add_settings_new_data(session = session, output = output, r = r, language = language, id = id,
            data = new_data, table = table, required_textfields = required_textfields, req_unique_values = req_unique_values,
            required_dropdowns = required_dropdowns,
            dropdowns = dropdowns)

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
        
        # If r$... variable changes
        observeEvent(r[[paste0(table, "_temp")]], {
          
          # If user has access
          # req(paste0(table, "_management_card") %in% r$user_accesses)
          
          # Dropdowns for each module / page
          dropdowns_datatable <- switch(table,
            "patient_lvl_modules" = c("module_family_id" = "patient_lvl_modules_families", "parent_module_id" = "patient_lvl_modules"),
            "aggregated_modules" = c("module_family_id" = "aggregated_modules_families", "parent_module_id" = "aggregated_modules"))
          
          # Action buttons for each module / page
          if (grepl("modules$", table) | grepl("modules_elements", table)) action_buttons <- "delete"
          if (grepl("modules_families", table)) action_buttons <- c("options", "delete")
          
          # Sortable cols
          sortable_cols <- c("id", "name", "description", "display_order", "datetime")
          
          # Column widths
          column_widths <- c("id" = "80px", "display_order" = "80px", "datetime" = "130px", "action" = "80px")
          
          # Editable cols
          editable_cols <- c("name", "description", "display_order")
          
          # Centered columns
          centered_cols <- c("id", "module_family_id", "parent_module_id", "display_order", "datetime", "action")
          
          # Searchable_cols
          searchable_cols <- c("name", "description")
          
          # Restore datatable state
          page_length <- isolate(input$management_datatable_state$length)
          start <- isolate(input$management_datatable_state$start)
          
          render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
            col_names =  get_col_names(table), table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons,
            datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
            filter = TRUE, searchable_cols = searchable_cols, column_widths = column_widths)
        })
      }
      
    
 
    # toggles <- c("creation_card", "datatable_card", "options_card")
    # 
    # if (id == "settings_patient_lvl_modules") prefix <- "patient_lvl"
    # if (id == "settings_aggregated_modules") prefix <- "aggregated"
    # 
    # ##########################################
    # # Show or hide cards   #
    # ##########################################
    # 
    # sapply(toggles, function(toggle){
    #   observeEvent(input[[paste0(prefix, "_", toggle, "_toggle")]], if(input[[paste0(prefix, "_", toggle, "_toggle")]]) shinyjs::show(paste0(prefix, "_", toggle)) else shinyjs::hide(paste0(prefix, "_", toggle)))
    # })
    # 
    # ##########################################
    # # Add a new module                       #
    # ##########################################
    # 
    # # Update dropdowns with reactive data
    # data_var_families <- switch(id,
    #   "settings_patient_lvl_modules" = "patient_lvl_module_families",
    #   "settings_aggregated_modules" = "aggregated_module_families")
    # data_var_modules <- switch(id,
    #   "settings_patient_lvl_modules" = "patient_lvl_modules",
    #   "settings_aggregated_modules" = "aggregated_modules")
    # 
    # observeEvent(r[[data_var_families]], {
    #   options <- tibble_to_list(r[[data_var_families]], "id", "name", rm_deleted_rows = TRUE)
    #   shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_family"),
    #     options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    # })
    # 
    # observeEvent(c(input[[paste0(prefix, "_module_family")]], r$patient_lvl_modules, r$aggregated_modules), {
    #   # Prevent bug if input is empty
    #   req(input[[paste0(prefix, "_module_family")]])
    #   module_parents <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var_modules, " WHERE module_family_id = ", input[[paste0(prefix, "_module_family")]], " ORDER BY id"))
    #   options <- tibble_to_list(module_parents, "id", "name", rm_deleted_rows = TRUE, null_value = TRUE, language = language)
    #   shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_parent"),
    #     options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    # })
    # 
    # observeEvent(input[[paste0(prefix, "_add")]], {
    #   new_name <- input[[paste0(prefix, "_name")]]
    #   name_check <- FALSE
    # 
    #   if (!is.null(new_name)){
    #     if (new_name != "") name_check <- TRUE
    #   }
    #   if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = translate(language, "provide_valid_name"))
    #   if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), errorMessage = NULL)
    # 
    #   req(name_check)
    # 
    #   # Check if chosen name is already used
    #   if (input[[paste0(prefix, "_creation_module_type")]] == "module") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", "settings_aggregated_modules" = "aggregated_modules")
    #   if (input[[paste0(prefix, "_creation_module_type")]] == "family") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", "settings_aggregated_modules" = "aggregated_module_families")
    # 
    #   distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", table, " WHERE deleted IS FALSE")) %>% dplyr::pull()
    # 
    #   if (new_name %in% distinct_names) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    #   req(new_name %not_in% distinct_names)
    # 
    #   # Check if module family is not empty
    #   module_family_check <- TRUE
    #   if (input[[paste0(prefix, "_creation_module_type")]] == "module" & input[[paste0(prefix, "_module_family")]] == "") module_family_check <- FALSE
    #   
    #   if (!module_family_check) show_message_bar(output, 2, "req_module_family", "severeWarning", language)
    #   req(module_family_check)
    #   
    #   last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", table)) %>% dplyr::pull()
    # 
    #   new_data <- switch(input[[paste0(prefix, "_creation_module_type")]],
    #     "module" = tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~creator_id, ~datetime, ~deleted,
    #       last_row + 1,
    #       as.character(new_name),
    #       coalesce2("char", input[[paste0(prefix, "_description")]]),
    #       coalesce2("int", input[[paste0(prefix, "_module_family")]]),
    #       coalesce2("int", input[[paste0(prefix, "_module_parent")]]),
    #       r$user_id,
    #       as.character(Sys.time()),
    #       FALSE),
    #     "family" = tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
    #       last_row + 1,
    #       as.character(new_name),
    #       coalesce2("char", input[[paste0(prefix, "_description")]]),
    #       r$user_id,
    #       as.character(Sys.time()),
    #       FALSE))
    # 
    #   DBI::dbAppendTable(r$db, table, new_data)
    # 
    #   r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    #   r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    # 
    #   # Add a row in options table
    #   # last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
    #   # DBI::dbAppendTable(r$db, "options",
    #   #   tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
    #   #     last_row_options + 1, table, last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))
    # 
    #   show_message_bar(output, 3, "new_module_added", "success", language)
    #   
    #   # Show management card
    #   shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_datatable_card_toggle"), value = TRUE)
    # 
    #   # Reset textfields
    #   shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_name"), value = "")
    #   shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_description"), value = "")
    # })
    # 
    # ##########################################
    # # Modules management                     #
    # ##########################################
    # 
    #   ##########################################
    #   # Generate datatable                     #
    #   ##########################################
    # 
    #   observeEvent(input[[paste0(prefix, "_management_module_type")]], {
    # 
    #     # Get data
    #     req(input[[paste0(prefix, "_management_module_type")]])
    #     module_type <- input[[paste0(prefix, "_management_module_type")]]
    #     if (module_type == "module") data_var <- data_var_modules
    #     if (module_type == "family") data_var <- data_var_families
    #     
    #     observeEvent(r[[paste0(data_var, "_temp")]], {
    #       
    #       # Datatable state
    #       page_length <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$length)
    #       start <- isolate(input[[paste0(prefix, "_management_datatable_state")]]$start)
    #       
    #       # Render datatable
    #       output[[paste0(prefix, "_management_datatable")]] <- DT::renderDT(
    #         # data,
    #         settings_datatable(
    #           ns = ns, r = r, id = id, prefix = prefix,
    #           data = settings_datatable_data(data_var, r),
    #           data_variables = c("patient_lvl_modules", "patient_lvl_module_families", "aggregated_modules", "aggregated_module_families"),
    #           dropdowns = settings_modules_get_dropdowns(prefix, module_type),
    #           action_buttons = switch(module_type, "module" =  c("delete", "options"), "family" = "delete"),
    #           new_colnames =
    #             switch(prefix,
    #               "patient_lvl" =
    #                 switch(module_type,
    #                 "module" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "module_family"), 
    #                   translate(language, "module_parent"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action")),
    #                 "family" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
    #                   translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))),
    #               "aggregated" =
    #                 switch(module_type,
    #                   "module" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "module_family"), 
    #                     translate(language, "module_parent"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action")),
    #                   "family" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
    #                     translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))))
    #         ),
    #         options = list(dom = "<'datatable_length'l><'top'ft><'bottom'p>",
    #           stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
    #           pageLength = page_length, displayStart = start,
    #           columnDefs = list(list(className = "dt-center", targets = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5))),
    #             list(sortable = FALSE, targets = switch(module_type, "module" = c(3, 4, 7), "family" = ""))),
    #           language = list(
    #             paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    #             search = translate(language, "DT_search"),
    #             lengthMenu = translate(language, "DT_length"))),
    #         rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #         editable = list(target = "cell", disable = list(columns = switch(module_type, "module" = c(0, 3, 4, 5, 6, 7), "family" = c(0, 3, 4, 5)))),
    #         callback = datatable_callback()
    #       )
    #     })
    #   })
    #   
    #   ##########################################
    #   # Save changes in datatable              #
    #   ##########################################
    # 
    #   # Each time a row is updated, modify temp variable
    #   observeEvent(input[[paste0(prefix, "_management_datatable_cell_edit")]], {
    #     edit_info <- input[[paste0(prefix, "_management_datatable_cell_edit")]]
    # 
    #     table <- settings_modules_get_table(prefix, input[[paste0(prefix, "_management_module_type")]])
    # 
    #     r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
    #     # Store that this row has been modified
    #     r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
    #   })
    # 
    #   # Each time a dropdown is updated, modify temp variable
    #   observeEvent(c(input[[paste0(prefix, "_management_module_type")]], 
    #     r$patient_lvl_module_families, r$patient_lvl_modules, r$aggregated_module_families, r$aggregated_modules), {
    #       
    #     req(input[[paste0(prefix, "_management_module_type")]])
    #     
    #     table <- settings_modules_get_table(prefix, input[[paste0(prefix, "_management_module_type")]])
    #     dropdowns <- settings_modules_get_dropdowns(prefix, input[[paste0(prefix, "_management_module_type")]])
    # 
    #     sapply(r[[table]] %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
    #       sapply(names(dropdowns), function(name){
    #         observeEvent(input[[paste0(dropdowns[[name]], id)]], {
    #           r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), name]] <-
    #             coalesce2("int", input[[paste0(dropdowns[[name]], id)]])
    #           # Store that this row has been modified
    #           r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
    #         })
    #       })
    #     })
    #   })
    # 
    #   observeEvent(input[[paste0(prefix, "_management_save")]], {
    # 
    #     # Make sure there's no duplicate in names
    #     duplicates <- 0
    # 
    #     module_type <- input[[paste0(prefix, "_management_module_type")]]
    #     table <- settings_modules_get_table(prefix, module_type)
    # 
    #     duplicates <- r[[paste0(table, "_temp")]] %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
    #       dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    # 
    #     if (duplicates > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
    #     req(duplicates == 0)
    # 
    #     # Make sure parent module is not the module itself
    #     parent_is_itself <- FALSE
    #     if (module_type == "module"){
    #       if (nrow(r[[paste0(table, "_temp")]] %>%
    #           dplyr::select(name, parent_module_id) %>%
    #           dplyr::left_join(r[[paste0(table, "_temp")]] %>% dplyr::select(parent_module_id = id, parent_name = name), by = "parent_module_id") %>%
    #           dplyr::filter(!is.na(parent_name)) %>% dplyr::filter(name == parent_name)) != 0) parent_is_itself <- TRUE
    #     }
    #     if (parent_is_itself) show_message_bar(output, 2, "parent_module_is_itself", "severeWarning", language)
    #     req(!parent_is_itself)
    # 
    #     # Save changes in database
    #     ids_to_del <- r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
    #     DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
    #     DBI::dbClearResult(query)
    #     DBI::dbAppendTable(r$db, table, r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified))
    # 
    #     # Notification to user
    #     show_message_bar(output, 2, "modif_saved", "success", language)
    #   })
    #   
    #   ##########################################
    #   # Delete a row in datatable              #
    #   ##########################################
    # 
    #   # Indicate whether to close or not delete dialog box
    #   r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
    # 
    #   # Create & show dialog box
    #   output[[paste0(prefix, "_delete_confirm")]] <- shiny.fluent::renderReact({
    #     settings_delete_react(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), ns, language, r[[paste0(prefix, "_delete_dialog")]])})
    # 
    #   # Whether to close or not delete dialog box
    #   observeEvent(input[[paste0(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), "_hide_dialog")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
    #   observeEvent(input[[paste0(paste0("modules_", input[[paste0(prefix, "_management_module_type")]]), "_delete_canceled")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
    #   observeEvent(input[[paste0(prefix, "_deleted_pressed")]], r[[paste0(prefix, "_delete_dialog")]] <<- TRUE)
    # 
    #   # When the delete is confirmed...
    #   observeEvent(input[[paste0(prefix, "_management_module_type")]], {
    #     module_type <- input[[paste0(prefix, "_management_module_type")]]
    #     observeEvent(input[[paste0(paste0("modules_", module_type), "_delete_confirmed")]], {
    # 
    #       # Close dialog box
    #       r[[paste0(prefix, "_delete_dialog")]] <<- FALSE
    # 
    #       table <- settings_modules_get_table(prefix, module_type)
    # 
    #       # Get the ID of row deleted
    #       deleted_pressed_value <- input[[paste0(prefix, "_deleted_pressed")]]
    #       row_deleted <- as.integer(substr(deleted_pressed_value, nchar(paste0(prefix, "_delete_")) + 1, nchar(deleted_pressed_value)))
    #       
    #       # Prevent bug when changing module_type
    #       req(nrow(r[[table]] %>% dplyr::filter(!deleted & id == row_deleted)) > 0)
    #       
    #       # Delete row in database
    #       DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted))
    #       # Update r vars (including temp variable, used in management datatables)
    #       r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    #       r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
    # 
    #       # Notification to user
    #       show_message_bar(output, 3, paste0("modules_", module_type, "_deleted"), "severeWarning", language)
    #     })
    #   })
    #   
    #   ##########################################
    #   # Edit options by selecting a row        #
    #   ##########################################
    #   
    #   observeEvent(input[[paste0(prefix, "_options")]], {
    #     # Show options toggle
    #     shiny.fluent::updateToggle.shinyInput(session, paste0(prefix, "_options_card_toggle"), value = TRUE)
    #     
    #     # Get module ID
    #     module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #     
    #     # r variable with selected thesaurus items
    #     r[[paste0(prefix, "_thesaurus_items_selected")]] <- list()
    #     
    #     ##########################################
    #     # Render options card                    #
    #     ##########################################
    #     
    #     output[[paste0(prefix, "_options_card")]] <- renderUI({
    #       make_card(tagList(translate(language, "module_options"), span(paste0(" (ID = ", module_id, ")"), style = "font-size: 15px;")),
    #         div(
    #           shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_module_options_action")), value = "add", options = list(
    #             list(key = "add", text = translate(language, "add_module_element")),
    #             list(key = "datatable", text = translate(language, "datatable_module_elements"))
    #           ), className = "inline_choicegroup"),
    #           
    #           # Add a module element
    #           shiny::conditionalPanel(
    #             condition = paste0("input.", prefix, "_module_options_action == 'add'"), ns = ns,
    #             div(
    #               shiny.fluent::Stack(
    #                 horizontal = TRUE, tokens = list(childrenGap = 20),
    #                 make_textfield(language, ns, label = "name", id = paste0(prefix, "_module_options_add_name"), width = "300px"),
    #                 make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_add_plugin"), 
    #                   options = tibble_to_list(r$plugins %>% dplyr::filter(!deleted, module_type == paste0(prefix, "_data")), "id", "name", rm_deleted_rows =  TRUE),
    #                   width = "300px"),
    #                 make_dropdown(language, ns, label = "thesaurus", id = paste0(prefix, "_module_options_add_thesaurus"), width = "300px",
    #                   options = tibble_to_list(r$thesaurus %>% dplyr::filter(!deleted), "id", "name", rm_deleted_rows =  TRUE))
    #               ),
    #               make_dropdown(language, ns, label = "thesaurus_items", id = paste0(prefix, "_module_options_add_thesaurus_items_selected")),
    #               div(DT::DTOutput(ns(paste0(prefix, "_module_options_add_thesaurus_items"))), style = "margin-top: 15px; margin-bottom: -5px;"),
    #               shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_add")), translate(language, "add"))
    #             )
    #           ),
    #           
    #           # Management datatable
    #           shiny::conditionalPanel(
    #             condition = paste0("input.", prefix, "_module_options_action == 'datatable'"), ns = ns,
    #             div(
    #               shiny.fluent::Stack(
    #                 horizontal = TRUE, tokens = list(childrenGap = 20),
    #                   make_dropdown(language, ns, label = "module_element", id = paste0(prefix, "_module_options_management_module_element"), width = "300px"),
    #                   make_dropdown(language, ns, label = "plugin", id = paste0(prefix, "_module_options_management_plugin"), width = "300px"),
    #                   make_dropdown(language, ns, label = "display_order", id = paste0(prefix, "_module_options_management_display_order"), width = "300px")
    #               ),
    #               div(DT::DTOutput(ns(paste0(prefix, "_module_options_management_datatable"))), style = "margin-top: 15px; margin-bottom: -5px;"),
    #               shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "module_options_save")), translate(language, "save"))
    #             )
    #           )
    #         )
    #       )
    #     })
    #   })
    #   
    #     ##########################################
    #     # Options card / 1) Add module element   #
    #     ##########################################
    #     
    #       ##########################################
    #       # Options card / load thesaurus          #
    #       ##########################################
    #     
    #       # When the thesaurus is chosen
    #       observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus")]], {
    #         r[[paste0(prefix, "_thesaurus_items")]] <- settings_modules_thesaurus_cache(r, prefix, page_id = id, thesaurus_id = input[[paste0(prefix, "_module_options_add_thesaurus")]])
    #         
    #         # Reset items dropdown
    #         shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
    #         
    #         observeEvent(r[[paste0(prefix, "_thesaurus_items")]], {
    #           
    #           # Datatable state
    #           page_length <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$length)
    #           start <- isolate(input[[paste0(prefix, "_module_options_add_thesaurus_items_state")]]$start)
    #           
    #           output[[paste0(prefix, "_module_options_add_thesaurus_items")]] <- DT::renderDT(
    #             settings_modules_datatable_data(ns, r, type = "thesaurus_items", prefix, data = r[[paste0(prefix, "_thesaurus_items")]],
    #               new_colnames = c(translate(language, "id"), translate(language, "thesaurus_id"), translate(language, "item_id"),
    #                 translate(language, "name"), translate(language, "display_name"), translate(language, "category"), translate(language, "unit"),
    #                 translate(language, "datetime"), translate(language, "action"))),
    #             options = list(
    #               dom = "<'datatable_length'l><'top'ft><'bottom'p>",
    #               stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
    #               pageLength = page_length, displayStart = start,
    #               language = list(
    #                 paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    #                 search = translate(language, "DT_search"),
    #                 lengthMenu = translate(language, "DT_length")),
    #               columnDefs = list(
    #                 list(className = "dt-center", targets = c(0, 1, 2, -1))#,
    #                 # list(sortable = FALSE, targets = c())
    #             )),
    #             rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #             editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 5, 6, 7, 8))),
    #             callback = datatable_callback()
    #           )
    #         })
    #       })
    #     
    #       # When a cell of the thesaurus datatable is edited
    #       observeEvent(input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]], {
    #         edit_info <- input[[paste0(prefix, "_module_options_add_thesaurus_items_cell_edit")]]
    #         r[[paste0(prefix, "_thesaurus_items")]] <- DT::editData(r[[paste0(prefix, "_thesaurus_items")]], edit_info, rownames = FALSE)
    #       })
    #     
    #       ##########################################
    #       # Options card / add & rm thesaurus item #
    #       ##########################################
    #       
    #         # When thesaurus item add action button is clicked
    #         observeEvent(input[[paste0(prefix, "_item_selected")]], {
    #           link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_selected")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_selected")]])))
    #           
    #           value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
    #           if (link_id_filter %not_in% value) value <- c(value, link_id_filter)
    #           options <- tibble_to_list(
    #             r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
    #             # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
    #             "id", "name", rm_deleted_rows = TRUE
    #           )
    #           
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
    #             options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    #         })
    #         
    #         # When thesaurus item remove action button is clicked
    #         observeEvent(input[[paste0(prefix, "_item_removed")]], {
    #           link_id_filter <- as.integer(substr(input[[paste0(prefix, "_item_removed")]], nchar(paste0(prefix, "_select_")) + 1, nchar(input[[paste0(prefix, "_item_removed")]])))
    #   
    #           value <- input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]]
    #           value <- value[!value %in% link_id_filter]
    #           options <- tibble_to_list(
    #             r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id %in% value),
    #             # DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE id IN (", paste(value, collapse = ","), ")")),
    #             "id", "name", rm_deleted_rows = TRUE
    #           )
    #   
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"),
    #             options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    #         })
    #       
    #         ##########################################
    #         # Options card / Add module element      #
    #         ##########################################
    #     
    #           observeEvent(input[[paste0(prefix, "module_options_add")]], {
    #             
    #             fields_check <- TRUE
    #             if (is.null(input[[paste0(prefix, "_module_options_add_plugin")]]) | 
    #                 is.null(input[[paste0(prefix, "_module_options_add_thesaurus")]]) | 
    #                 is.null(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]])) fields_check <- FALSE
    #             
    #             if (!fields_check) show_message_bar(output, 1, "fields_empty", "severeWarning", language)
    #             req(fields_check)
    #             
    #             module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #             new_name <- input[[paste0(prefix, "_module_options_add_name")]]
    #             name_check <- FALSE
    #             if (!is.null(new_name)){
    #               if (new_name != "") name_check <- TRUE
    #             }
    #             if (!name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = translate(language, "provide_valid_name"))
    #             if (name_check) shiny.fluent::updateTextField.shinyInput(session, paste0(prefix, "_module_options_add_name"), errorMessage = NULL)
    #             
    #             req(name_check)
    #             
    #             distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", prefix, "_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
    #             
    #             if (new_name %in% distinct_names) show_message_bar(output, 3, "name_already_used", "severeWarning", language)
    #             req(new_name %not_in% distinct_names)
    #             
    #             plugin_id <- as.integer(input[[paste0(prefix, "_module_options_add_plugin")]])
    #             last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE")) %>% dplyr::pull()
    #             last_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
    #             last_group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM patient_lvl_module_elements")) %>% dplyr::pull()
    #             new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted)
    #             
    #             sapply(input[[paste0(prefix, "_module_options_add_thesaurus_items_selected")]], function(item){
    #               last_id <<- last_id + 1
    #               thesaurus_item_id <- as.integer(item)
    #               thesaurus_item <- r[[paste0(prefix, "_thesaurus_items")]] %>% dplyr::filter(id == thesaurus_item_id)
    #               # If display name is empty, take original thesaurus name
    #               thesaurus_item_display_name <- thesaurus_item %>% 
    #                 dplyr::mutate(final_name = dplyr::case_when(display_name != "" ~ display_name, TRUE ~ name)) %>%
    #                 dplyr::select(final_name) %>% dplyr::pull() %>% as.character()
    #               thesaurus_item_unit <- thesaurus_item %>% dplyr::select(unit) %>% dplyr::pull() %>% as.character()
    # 
    #               new_data <<- new_data %>% dplyr::bind_rows(
    #               tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~display_order, ~creator_id, ~datetime, ~deleted,
    #                   last_id, new_name, last_group_id + 1, module_id, plugin_id, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE))
    #             })
    # 
    #             DBI::dbAppendTable(r$db, "patient_lvl_module_elements", new_data)
    #             
    #             # Reset dropdowns except thesaurus
    #             shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_plugin"), value = NULL)
    #             shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_add_thesaurus_items_selected"), value = NULL, options = list())
    #             
    #             show_message_bar(output, 4, "module_element_added", "success", language)
    #           })
    # 
    #       ##########################################
    #       # Options card / 2) Elements mngt        #
    #       ##########################################
    #         
    #         observeEvent(input[[paste0(prefix, "_module_options_action")]], {
    #           req(input[[paste0(prefix, "_module_options_action")]] == "datatable")
    #           
    #           module_id <- as.integer(substr(input[[paste0(prefix, "_options")]], nchar(paste0(prefix, "_options_")) + 1, nchar(input[[paste0(prefix, "_options")]])))
    #           r[[paste0(prefix, "_module_elements")]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_module_elements WHERE module_id = ", module_id, " AND deleted IS FALSE ORDER BY id"))
    #           
    #           options <- tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(name) %>% dplyr::slice(1) %>% dplyr::select(name), "name", "name")
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_module_element"),
    #             options = options,
    #             value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    #         })
    #         
    #         observeEvent(input[[paste0(prefix, "_module_options_management_module_element")]], {
    #           
    #           data <- r[[paste0(prefix, "_module_elements")]] %>% dplyr::filter(name == input[[paste0(prefix, "_module_options_management_module_element")]])
    #           
    #           # Update plugin dropdown
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_plugin"),
    #             options = tibble_to_list(r$plugins %>% dplyr::select(name, id), "id", "name"),
    #             value = data %>% dplyr::slice(1) %>% dplyr::pull(plugin_id))
    #           
    #           # Update display order dropdown
    #           shiny.fluent::updateDropdown.shinyInput(session, paste0(prefix, "_module_options_management_display_order"),
    #             options = tibble_to_list(r[[paste0(prefix, "_module_elements")]] %>% dplyr::group_by(display_order) %>% dplyr::slice(1), "display_order", "display_order"),
    #             value = data %>% dplyr::slice(1) %>% dplyr::pull(display_order))
    #           
    #           # Render thesaurus items datatable
    #           output[[paste0(prefix, "_module_options_management_datatable")]] <- DT::renderDT(
    #             settings_modules_datatable_data(ns, r, type = "elements_management", prefix, data = data,
    #               new_colnames = c(translate(language, "id"), translate(language, "thesaurus_item"), translate(language, "thesaurus_item_display_name"),
    #                 translate(language, "thesaurus_item_unit"), translate(language, "creator"), translate(language, "datetime"), translate(language, "action"))),
    #             options = list(
    #               dom = "<'top't><'bottom'p>",
    #               # stateSave = TRUE, stateDuration = 30, autoFill = list(enable = FALSE),
    #               # pageLength = page_length, displayStart = start,
    #               language = list(
    #                 paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    #                 search = translate(language, "DT_search"),
    #                 lengthMenu = translate(language, "DT_length")),
    #               columnDefs = list(
    #                 list(className = "dt-center", targets = c(0, 4, -1, -2)),
    #                 list(sortable = FALSE, targets = c(-1))
    #               )),
    #             rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
    #             editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 4, 5, 6, 7, 8))),
    #             callback = datatable_callback()
    #           )
    #         })
    #         
  })
}