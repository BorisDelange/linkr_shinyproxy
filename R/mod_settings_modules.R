#' settings_modules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_modules_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Patient-lvl modules                    #
  ##########################################
  
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
        div(id = ns(paste0(page, "_creation_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_creation"), language = language, words = words)),
        div(id = ns(paste0(page, "_management_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_management"), language = language, words = words)))

      if (page == "modules_families") cards <<- tagList(cards,
        div(id = ns(paste0(page, "_options_card")), mod_settings_sub_modules_ui(id = paste0("settings_modules_", prefix, "_", page, "_options"), language = language, words = words)))
    })

    div(class = "main",
      render_settings_default_elements(ns = ns),
      render_settings_toggle_card(language = language, ns = ns, cards = list(
        list(key = "modules_families_creation_card", label = "modules_families_creation_card"),
        list(key = "modules_families_management_card", label = "modules_families_management_card"),
        list(key = "modules_families_options_card", label = "modules_families_options_card"),
        list(key = "modules_creation_card", label = "modules_creation_card"),
        list(key = "modules_management_card", label = "modules_management_card"),
        list(key = "modules_elements_creation_card", label = "modules_elements_creation_card"),
        list(key = "modules_elements_management_card", label = "modules_elements_management_card")
      ), words = words),
      cards
    ) -> result
  
  result
}

mod_settings_sub_modules_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  result <- ""
  
  ##########################################
  # Patient-lvl & aggregated sub modules   #
  ##########################################
    
    if (grepl("patient_lvl", id)) page <- substr(id, nchar("settings_patient_lvl_modules_") + 1, nchar(id))
    if (grepl("aggregated", id)) page <- substr(id, nchar("settings_aggregated_modules_") + 1, nchar(id))
    
    if (page == "modules_families_options"){
      uiOutput(ns("options_card")) -> result
    }

    if (page == "modules_families_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module_family",
        textfields = c("name", "description"), textfields_width = "300px", words = words) -> result
    }
  
    if (page == "modules_creation"){
      render_settings_creation_card(language = language, ns = ns, id = id, title = "add_module",
        textfields = c("name", "description"), textfields_width = "300px",
        dropdowns = c("module_family", "parent_module"), dropdowns_width = "300px", words = words) -> result
    }
  
    if (page %in% c("modules_management", "modules_families_management")){
      render_settings_datatable_card(language = language, ns = ns, title = page, words = words) -> result
    }
  
    # For modules management, add a dropdown to select module family
    # if (page == "modules_management"){
    #   div(id = ns("datatable_card"),
    #     make_card(translate(language, page, words),
    #       div(
    #         make_dropdown(language = language, ns = ns, label = "module_family", width = "300px"),
    #         DT::DTOutput(ns("management_datatable")),
    #         shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save", words))
    #       )
    #     )
    #   ) -> result
    # }

    if (page == "modules_elements_creation"){
      if (grepl("patient_lvl", id)){
        div(id = ns("creation_card"),
          make_card(
            title = translate(language, "add_module_element", words),
            content = div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_textfield(language = language, ns = ns, label = "name", id = "name", width = "300px", words = words)
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "module_family", id = "module_family", width = "300px", words = words),
                make_dropdown(language = language, ns = ns, label = "module", id = "module_new_element", width = "300px", words = words),
                make_dropdown(language = language, ns = ns, label = "plugin", id = "plugin", width = "300px", words = words)
              ), 
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "thesaurus", id = "thesaurus", width = "300px", words = words),
                make_dropdown(language = language, ns = ns, label = "datamart", 
                  options = list(list(key = "", text = translate(language, "none", words))), value = "", width = "300px", words = words),
                shiny::conditionalPanel(condition = "input.datamart != ''", ns = ns,
                  div(strong(translate(language, "show_only_used_items", words), style = "display:block; padding-bottom:12px;"),
                    shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
              ), 
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 50),
                make_dropdown(language = language, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items",
                  multiSelect = TRUE, width = "650px", words = words),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), translate(language, "reset", words)), style = "margin-top:38px;")
              ),
              br(),
              DT::DTOutput(ns("thesaurus_items")), br(),
              shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element"), translate(language, "add", words))
            )
          )
        ) -> result
      }
      if (grepl("aggregated", id)) {
        div(id = ns("creation_card"),
            make_card(
              title = translate(language, "add_module_element", words),
              content = div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                  make_textfield(language = language, ns = ns, label = "name", id = "name", width = "300px", words = words)
                ),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                  make_dropdown(language = language, ns = ns, label = "module_family", id = "module_family", width = "300px", words = words),
                  make_dropdown(language = language, ns = ns, label = "module", id = "module_new_element", width = "300px", words = words),
                  make_dropdown(language = language, ns = ns, label = "plugin", id = "plugin", width = "300px", words = words)
                ), br(),
                shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element"), translate(language, "add", words))
              )
            )
        ) -> result
      }
    }

    if (grepl("modules_elements_management", id)){
        
      delete_module_element <- ""
        
      if (id == "settings_modules_patient_lvl_modules_elements_management"){
        
        delete_module_element <- div(
          hr(), span(translate(language, "delete_module_element"), style = "font-family: 'Segoe UI'; font-size: 18px;"), br(),
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 30),
            div(make_dropdown(language = language, ns = ns, label = "module_family", id = "dme_module_family", width = "200px", words = words)),
            div(make_dropdown(language = language, ns = ns, label = "module", id = "dme_module", width = "200px", words = words)),
            div(make_dropdown(language = language, ns = ns, label = "module_element", id = "dme_module_element", width = "200px", words = words)),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("dme_delete"), translate(language, "delete", words)), style = "margin-top:38px;")
          ), br()
        )
      }
      
      div(div(id = ns("datatable_card"),
        make_card(translate(language, "modules_elements_management", words),
          div(
            DT::DTOutput(ns("management_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save", words)), br(), br(),
            hr(), span(translate(language, "change_display_order_module_element", words), style = "font-family: 'Segoe UI'; font-size: 18px;"), br(),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 30),
              div(make_dropdown(language = language, ns = ns, label = "module_family", id = "cdo_module_family", width = "200px", words = words)),
              div(make_dropdown(language = language, ns = ns, label = "module", id = "cdo_module", width = "200px", words = words)),
              div(make_dropdown(language = language, ns = ns, label = "module_element", id = "cdo_module_element", width = "200px", words = words)),
              div(make_dropdown(language = language, ns = ns, label = "display_order", id = "cdo_display_order", width = "100px", words = words)),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("cdo_save"), translate(language, "save", words)), style = "margin-top:38px;")
            ), br(),
            delete_module_element
          )
        )
      ), br()) -> result
    }
  
  tagList(render_settings_default_elements(ns = ns), result) 
}

    
#' settings_modules Server Functions
#'
#' @noRd 

mod_settings_modules_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # To prevent refresh of dropdowns when change display order is saved
    r$patient_lvl_cdo_saved <- 0L
    r$aggregated_cdo_saved <- 0L
    
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
      if (id == "settings_patient_lvl_modules" | id == "settings_aggregated_modules"){ 
        
        # Depending on toggles activated
        sapply(toggles, function(toggle){
          
          # Reset toggles when we load the page (restart reactivity, sometimes frozen)
          observeEvent(shiny.router::get_query_param(), {
            shiny.fluent::updateToggle.shinyInput(session, paste0(toggle, "_toggle"), value = FALSE)
            # If this toggles was activated, reactivate it
            if (paste0(id, toggle) %in% r$activated_toggles) shiny.fluent::updateToggle.shinyInput(session, paste0(toggle, "_toggle"), value = TRUE)
          })
          
          # If user has no access, hide card
          observeEvent(r$user_accesses,
            if ((paste0(prefix, "_modules_creation_card") %not_in% r$user_accesses & grepl("creation_card", toggle))
               | (paste0(prefix, "_modules_management_card") %not_in% r$user_accesses & grepl("management_card", toggle))
               | (paste0(prefix, "_modules_options_card") %not_in% r$user_accesses & grepl("options_card", toggle))) shinyjs::hide(toggle))
          
          # If user has access, show or hide card when toggle is clicked
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            # if (toggle %in% r$user_accesses){
              if ((paste0(prefix, "_modules_creation_card") %in% r$user_accesses & grepl("creation_card", toggle))
                | (paste0(prefix, "_modules_management_card") %in% r$user_accesses & grepl("management_card", toggle))
                | (paste0(prefix, "_modules_options_card") %in% r$user_accesses & grepl("options_card", toggle))) {
                if(input[[paste0(toggle, "_toggle")]]){
                  shinyjs::show(toggle)
                  r$activated_toggles <- c(r$activated_toggles, paste0(id, toggle))
                }
                else {
                  shinyjs::hide(toggle)
                  r$activated_toggles <- r$activated_toggles[r$activated_toggles != paste0(id, toggle)]
                }
              }
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
        
        observeEvent(r[[paste0(prefix, "_modules_families_options")]], {
          if (r[[paste0(prefix, "_modules_families_options")]] > 0){
            shiny.fluent::updateToggle.shinyInput(session, "modules_families_options_card_toggle", value = TRUE)
          }
        })
      }
      
      ###########################################################
      # Add a new module / module family / module element       #
      ###########################################################
      
      # Only for creation subpages
      if (grepl("creation", id)){

        # Update dropdowns with reactive data (module_family & parent_module dropdowns)
        # Depends on which data the user has access
        
        observeEvent(r[[paste0(prefix, "_modules_families")]], {
          table <- paste0(prefix, "_modules_families")
          
          options <- convert_tibble_to_list(data = r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "module_family", options = options)
        })
        
        # Update parent_module dropdown only when a module family is chosen
        observe({
          req(input$module_family)
          options_parent_module <- convert_tibble_to_list(data = r[[paste0(prefix, "_modules")]] %>%
            dplyr::filter(module_family_id == input$module_family) %>% dplyr::arrange(name), key_col = "id", text_col = "name", null_value = TRUE, words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "parent_module", options = options_parent_module)
        })
        
        observeEvent(r$plugins, {
          
          module_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
          
          # Filter on plugins user has access to
          plugins <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)
          
          options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "plugin", options = options)
        })
        
        observeEvent(r$datamarts, {
          
          options <- convert_tibble_to_list(data = r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name", null_value = TRUE, words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = options)
        })
        
        observeEvent(r$thesaurus, {
          options <- convert_tibble_to_list(data = r$thesaurus %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus", options = options)
        })
        
        ##########################################
        # Add a module element                   #
        ##########################################
        
        if (grepl("modules_elements", table)){
          
          # Update modules depending on chosen module family
          observeEvent(input$module_family, {
            req(input$module_family)
            modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == input$module_family) %>% dplyr::arrange(name)
            
            # Exclude modules who has children
            modules <- modules %>% 
              dplyr::left_join(modules %>% dplyr::select(has_children = id, id = parent_module_id), by = "id") %>%
              dplyr::filter(is.na(has_children))
            
            shiny.fluent::updateDropdown.shinyInput(session, "module_new_element",
              options = convert_tibble_to_list(data = modules, key_col = "id", text_col = "name", words = r$words))
          })
        
          ##########################################
          # Thesaurus & items dropdown / datatable #
          ########################################## 
          
          # Only for patient_lvl_modules_elements
          if (grepl("patient_lvl", table)){
            
            # Update thesaurus items datatable when a thesaurus item is chosen
            observeEvent(input$thesaurus, {
              
              datamarts <- r$datamarts
              
              # Get datamarts linked to this thesaurus
              data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == input$thesaurus) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
              datamarts <- datamarts %>% dplyr::filter(data_source_id %in% data_sources) %>% dplyr::arrange(name)
              
              # Update datamart dropdown
              shiny.fluent::updateDropdown.shinyInput(session, "datamart", 
                options = convert_tibble_to_list(data = datamarts, key_col = "id", text_col = "name", null_value = TRUE, words = r$words), value = "")
              
              # Set r$modules_refresh_thesaurus_items to "all_items", cause we havn't chosen yet the thesaurus or the datamart
              r$modules_refresh_thesaurus_items <- paste0(input$thesaurus, "all_items")
              
            })
            
            observeEvent(input$show_only_used_items, {
              if (input$show_only_used_items) r$modules_refresh_thesaurus_items <- "only_used_items"
              else r$modules_refresh_thesaurus_items <- "all_items"
            })
            
            # When value of datamart changes, change value or r$modules_refresh_thesaurus_items, depending on show_only_used_items
            # Add input$datamart in the value, to refresh even if the value doesn't change 
            # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...
            
            observeEvent(input$datamart, {
              if (input$show_only_used_items) r$modules_refresh_thesaurus_items <- paste0(input$datamart, "only_used_items")
              else r$modules_refresh_thesaurus_items <- r$modules_refresh_thesaurus_items <- paste0(input$datamart, "all_items")
            })
            
            observeEvent(r$modules_refresh_thesaurus_items, {
              
              req(input$thesaurus)
              
              # Get all items from the chosen thesaurus
              
              r$modules_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus, category = "plus_minus")
              colour_col <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus, category = "colours")
              if (nrow(colour_col) > 0) r$modules_thesaurus_items <- r$modules_thesaurus_items %>%
                dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
              
              if (length(input$datamart) > 0){
                if (input$datamart != ""){
                  
                  count_items_rows <- tibble::tibble()
                  count_patients_rows <- tibble::tibble()
                  
                  # Add count_items_rows in the cache & get it if already in the cache
                  tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus,
                    datamart_id = as.integer(input$datamart), category = "count_items_rows"),
                    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
                      error_name = paste0("modules - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", input$datamart), category = "Error", error_report = toString(e), language = language))
                  
                  # Add count_items_rows in the cache & get it if already in the cache
                  tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus,
                    datamart_id = as.integer(input$datamart), category = "count_patients_rows"),
                    error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
                      error_name = paste0("modules - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", input$datamart), category = "Error", error_report = toString(e), language = language))
                  
                  if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language)
                  req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
                  
                  # Transform count_rows cols to integer, to be sortable
                  r$modules_thesaurus_items <- r$modules_thesaurus_items %>%
                    dplyr::left_join(count_items_rows, by = "item_id") %>%
                    dplyr::left_join(count_patients_rows, by = "item_id") %>%
                    dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                    dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
                  
                  # If r$modules_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
                  if (grepl("only_used_items", r$modules_refresh_thesaurus_items)) r$modules_thesaurus_items <- r$modules_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
                  
                  r$modules_thesaurus_items_temp <- r$modules_thesaurus_items %>% dplyr::mutate(modified = FALSE)
                }
              }
              
              r$modules_thesaurus_items_temp <- r$modules_thesaurus_items %>% dplyr::mutate(modified = FALSE)
            })
              
            observeEvent(r$modules_thesaurus_items_temp, {
              
              # Transform category to factor, to be filtering in datatable
              r$modules_thesaurus_items_temp <- r$modules_thesaurus_items_temp %>% 
                dplyr::mutate_at("category", as.factor) %>%
                dplyr::mutate_at("item_id", as.character)
              
              # Render datatable
              render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "thesaurus_items",
                col_names =  col_names, table = "modules_thesaurus_items", action_buttons = action_buttons,
                datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = page_length, start = start,
                editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, 
                searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE)
            })
          }
              
          ##############################################
          # Module element / Thesaurus items selection #
          ############################################## 
          
          # Only for patient_lvl_modules_elements
          if (grepl("patient_lvl", table)){
          
            ##############################################
            # Module element / Save changes in datatable #
            ##############################################
              
            observeEvent(input$thesaurus_items_cell_edit, {
              edit_info <- input$thesaurus_items_cell_edit
              edit_info$col <- edit_info$col + 2 # Cause id & thesaurus_id cols removed
              r$modules_thesaurus_items_temp <- DT::editData(r$modules_thesaurus_items_temp, edit_info, rownames = FALSE)
              r$modules_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
            })
          
            ##########################################
            # Module element / Add & remove items    #
            ##########################################
            
            # When add button is clicked
            observeEvent(input$item_selected, {
              
              # Initiate r variable if doesn't exist
              if (length(r$modules_thesaurus_selected_items) == 0){
                r$modules_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
                  thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
                  thesaurus_item_colour = character(), input_text = character()) 
              }
              
              # Get ID of chosen thesaurus item
              link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))
              
              # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown
              
              value <- integer(1)
              if (nrow(r$modules_thesaurus_selected_items) > 0) value <- r$modules_thesaurus_selected_items %>% 
                dplyr::filter(thesaurus_id == input$thesaurus) %>% dplyr::pull(id)
              
              if (link_id %not_in% value){
                
                # Get thesaurus name
                thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus) %>% dplyr::pull(name)
  
                # Get item informations from datatable / r$modules_thesaurus_items
                # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
                item <- r$modules_thesaurus_items_temp %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", name))
                
                display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)
                
                # Add item to selected items
                r$modules_thesaurus_selected_items <- r$modules_thesaurus_selected_items %>% dplyr::bind_rows(
                  tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text,
                    as.integer(link_id), as.integer(input$thesaurus), as.character(thesaurus_name), as.integer(item$item_id), as.character(display_name), 
                    as.character(item$unit), as.character(input[[paste0("colour_", link_id)]]), as.character(item$input_text)))
  
                # Update dropdown of selected items
                options <- convert_tibble_to_list(r$modules_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
                value <- r$modules_thesaurus_selected_items %>% dplyr::pull(id)
                shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
                  options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
              }
            
            })
          
            # When remove button is clicked
            observeEvent(input$item_removed, {
              
              # Initiate r variable if doesn't exist
              if (length(r$modules_thesaurus_selected_items) == 0){
                r$modules_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
                  thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
                  thesaurus_item_colour = character(), input_text = character())
              }
              
              # Get ID of chosen thesaurus item
              link_id <- as.integer(substr(input$item_removed, nchar("remove_") + 1, nchar(input$item_removed)))
              
              value <- integer(1)
              if (nrow(r$modules_thesaurus_selected_items) > 0) value <- r$modules_thesaurus_selected_items %>% dplyr::filter(thesaurus_id == input$thesaurus) %>% dplyr::pull(id)
              
              if (link_id %in% value){
                
                r$modules_thesaurus_selected_items <- r$modules_thesaurus_selected_items %>%
                  dplyr::anti_join(tibble::tribble(~thesaurus_id, ~id, input$thesaurus, link_id), by = c("thesaurus_id", "id"))
                
                # Update dropdown of selected items
                options <- convert_tibble_to_list(r$modules_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
                value <- r$modules_thesaurus_selected_items %>% dplyr::pull(id)
                shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
                  options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
              }
            })
            
            # When reset button is clicked
            observeEvent(input$reset_thesaurus_items, {
              # Reset r$modules_thesaurus_selected_items
              r$modules_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
                thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
                thesaurus_item_colour = character(), input_text = character()) 
              
              shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
            })
          }
        }

        ##########################################
        # When add button is clicked             #
        # For modules & modules families         #
        ##########################################
        
        # When add button is clicked
        observeEvent(input$add, {

          # If user has access
          req(paste0(prefix, "_modules_creation_card") %in% r$user_accesses)

          new_data <- list()

          new_data_var <- c("name" = "char", "description" = "char", "module_family" = "int", "parent_module" = "int")

          # Transform values of textfields & dropdowns to chosen variable type
          sapply(names(new_data_var),
            function(input_name){
              new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
            })
          
          # Add a display order, depending on last display order from the module family
          if (table %in% c("patient_lvl_modules", "aggregated_modules") & length(input$module_family) > 0){
            
            if (length(input$parent_module) == 0){
              last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id IS NULL")) %>% dplyr::pull()
            }
            else {
              if (is.na(new_data$parent_module)) last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id IS NULL")) %>% dplyr::pull()
              
              else last_display <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", table,
                " WHERE module_family_id = ", new_data$module_family, " AND parent_module_id = ", new_data$parent_module)) %>% dplyr::pull()
            }
            
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
        
        ##########################################
        # When add button is clicked             #
        # For modules elements                   #
        ##########################################
        
        observeEvent(input$add_module_element, {
          
          new_data <- list()
          
          new_data_var <- c("name" = "char", "module_family" = "int", "module_new_element" = "int", "plugin" = "int")
          
          # Transform values of textfields & dropdowns to chosen variable type
          sapply(names(new_data_var),
            function(input_name){
              new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
            })
          
          # Check if name is not empty
          if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name", words))
          else shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)
          req(!is.na(new_data$name))

          # Check if values required to be unique are unique
          # sapply(req_unique_values, function(field){

          sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND module_id = {new_data$module_new_element}", .con = r$db)
          distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
          if (new_data$name %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
          req(new_data$name %not_in% distinct_values)

          # Check if dropdowns are not empty (if all are required)
          dropdowns_check <- TRUE

          required_dropdowns <- c("module_family", "module_new_element", "plugin")

          sapply(required_dropdowns, function(dropdown){
            if (is.null(new_data[[dropdown]])) dropdowns_check <<- FALSE
            else if (is.na(new_data[[dropdown]])) dropdowns_check <<- FALSE
          })

          if (!dropdowns_check) show_message_bar(output, 2, "dropdown_empty", "severeWarning", language)
          req(dropdowns_check)

          # Check if list of items is not empty (only for patient_lvl)
          if (grepl("patient_lvl", table)){
            if (length(r$modules_thesaurus_selected_items) == 0) show_message_bar(output, 2, "thesaurus_items_empty", "severeWarning", language)
            req(length(r$modules_thesaurus_selected_items) > 0)
            if (nrow(r$modules_thesaurus_selected_items) == 0) show_message_bar(output, 2, "thesaurus_items_empty", "severeWarning", language)
            req(nrow(r$modules_thesaurus_selected_items) > 0)
          }

          # Get last_row nb
          last_row <- get_last_row(r$db, table)
          last_row_group <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table)) %>% dplyr::pull() %>% as.integer()
          last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table, " WHERE module_id = ", new_data$module_new_element)) %>% dplyr::pull() %>% as.integer()

          if (grepl("patient_lvl", table)) new_data <- r$modules_thesaurus_selected_items %>%
            dplyr::transmute(
              id = 1:dplyr::n() + last_row + 1,
              name = as.character(new_data$name),
              group_id = last_row_group + 1,
              module_id = as.integer(new_data$module_new_element),
              plugin_id = as.integer(new_data$plugin),
              thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, thesaurus_item_colour,
              display_order = last_display_order + 1,
              creator_id = r$user_id,
              datetime = as.character(Sys.time()),
              deleted = FALSE
              )
          
          if (grepl("aggregated", table)) new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, 
            ~display_order, ~creator_id, ~datetime, ~deleted,
            last_row + 1, as.character(new_data$name), last_row_group + 1, as.integer(new_data$module_new_element),
            as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
          
          DBI::dbAppendTable(r$db, table, new_data)
          add_log_entry(r = r, category = paste0(table, " - ", translate(language, "insert_new_data", words)), name = translate(language, "sql_query", words), value = toString(new_data))
          
          show_message_bar(output = output, id = 3, message = paste0(get_singular(table), "_added"), type = "success", language = language)
          
          update_r(r = r, table = table, language = language)
          
          # Reset name textfield & dropdowns
          shiny.fluent::updateTextField.shinyInput(session, "name", value = "")
          
          modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == input$module_family) %>% dplyr::arrange(name)
          shiny.fluent::updateDropdown.shinyInput(session, "module_new_element",
            options = convert_tibble_to_list(data = modules, key_col = "id", text_col = "name", words = r$words), value = NULL)
          
          module_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
          plugins <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)
          options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
          shiny.fluent::updateDropdown.shinyInput(session, "plugin", options = options, value = NULL)
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
          
          # Create r var for datatable
          
          # Action buttons for each module / page
          action_buttons <- ""
          if (paste0(prefix, "_modules_delete_data") %in% r$user_accesses) action_buttons <- "delete"
          if (grepl("modules_families", table)) action_buttons <- c("options", action_buttons)
          
          # Dropdowns for each module / page
          dropdowns_datatable <- switch(table,
            "patient_lvl_modules" = c("parent_module_id" = "patient_lvl_modules"),
            "aggregated_modules" = c("parent_module_id" = "aggregated_modules"))
          
          # If r variable already created, or not
          if (length(r[[paste0(table, "_datatable_temp")]]) == 0) data_output <- tibble::tibble()
          else data_output <- r[[paste0(table, "_datatable_temp")]]
          
          # Prepare data for datatable (add code for dropdowns etc)
          r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
            table = table, dropdowns = dropdowns_datatable, dropdowns_null_value = "parent_module_id",
            action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]], data_output = data_output)
          
          # Editable cols
          if (table == "patient_lvl_modules_elements") editable_cols <- c("thesaurus_item_display_name", "thesaurus_item_unit", "display_order")
          else if (table == "aggregated_modules_elements") editable_cols <- c("name", "display_order")
          else editable_cols <- c("name", "description", "display_order")
          
          # Sortable cols
          sortable_cols <- c("id", "name", "description", "display_order", "datetime", "module_family_id", "module_id", "plugin_id", "thesaurus_name")

          # Column widths
          column_widths <- c("id" = "80px", "display_order" = "80px", "datetime" = "130px", "action" = "80px")
          
          # Searchable_cols
          searchable_cols <- c("name", "description", "module_family_id", "module_id", "plugin_id", "thesaurus_name")
          
          # Centered columns
          centered_cols <- c("id", "parent_module_id", "display_order", "datetime", "action")
          
          # Factorized cols
          factorize_cols <- character()
          if (table %in% c("patient_lvl_modules", "aggregated_modules")) factorize_cols <- c("module_family_id")
          if (table == "patient_lvl_modules_elements") factorize_cols <- c("module_family_id", "module_id", "plugin_id", "thesaurus_name")
          if (table == "aggregated_modules_elements") factorize_cols <- c("module_family_id", "module_id", "plugin_id")
          
          # Hidden cols
          hidden_cols <- c("description", "deleted", "modified")
          
          # Render datatable
          render_datatable(output = output, r = r, ns = ns, language = language, data = r[[paste0(table, "_datatable_temp")]],
            output_name = "management_datatable", col_names =  get_col_names(table_name = table, language = language, words = r$words),
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
            searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
          
          # Create a proxy for datatatable
          r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
          
          # Reload datatable
          observeEvent(r[[paste0(table, "_temp")]], {
            
            # Reload datatable_temp variable
            # r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
            #   table = table, dropdowns = dropdowns_datatable, dropdowns_null_value = "parent_module_id",
            #   action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]], data_output = data_output)
            
            
            # Reload data of datatable
            DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE)
          })
          
          
          # Reload datatable
          # observeEvent(r[[paste0(table, "_temp")]], {
          #   
          #   # Update input module_family
          #   options <- convert_tibble_to_list(r[[paste0(prefix, "_modules_families")]], key_col = "id", text_col = "name")
          #   shiny.fluent::updateDropdown.shinyInput(session, "module_family", options = options)
          #   
          #   # If user has access
          #   req(paste0(prefix, "_modules_management_card") %in% r$user_accesses)
          #   
          #   # Dropdowns for each module / page
          #   dropdowns_datatable <- switch(table,
          #     "patient_lvl_modules" = c("parent_module_id" = "patient_lvl_modules"),
          #     "aggregated_modules" = c("parent_module_id" = "aggregated_modules"))
          #   
          #   # Action buttons for each module / page
          #   action_buttons <- ""
          #   if (paste0(prefix, "_modules_delete_data") %in% r$user_accesses) action_buttons <- "delete"
          #   
          #   # if (grepl("modules$", table) | grepl("modules_elements", table)) action_buttons <- "delete"
          #   if (grepl("modules_families", table)) action_buttons <- c("options", action_buttons)
          #   
          #   # Sortable cols
          #   sortable_cols <- c("id", "name", "description", "display_order", "datetime", "module_family_id", "module_id", "plugin_id", "thesaurus_name")
          #   
          #   # Column widths
          #   column_widths <- c("id" = "80px", "display_order" = "80px", "datetime" = "130px", "action" = "80px")
          #   
          #   # Editable cols
          #   if (table == "patient_lvl_modules_elements") editable_cols <- c("thesaurus_item_display_name", "thesaurus_item_unit", "display_order")
          #   else if (table == "aggregated_modules_elements") editable_cols <- c("name", "display_order")
          #   else editable_cols <- c("name", "description", "display_order")
          #   
          #   # Centered columns
          #   centered_cols <- c("id", "parent_module_id", "display_order", "datetime", "action")
          #   
          #   # Searchable_cols
          #   searchable_cols <- c("name", "description", "module_family_id", "module_id", "plugin_id", "thesaurus_name")
          #   
          #   # Factorized cols
          #   factorize_cols <- character()
          #   if (table %in% c("patient_lvl_modules", "aggregated_modules")) factorize_cols <- c("module_family_id")
          #   if (table == "patient_lvl_modules_elements") factorize_cols <- c("module_family_id", "module_id", "plugin_id", "thesaurus_name")
          #   if (table == "aggregated_modules_elements") factorize_cols <- c("module_family_id", "module_id", "plugin_id")
          #   
          #   # Restore datatable state
          #   page_length <- isolate(input$management_datatable_state$length)
          #   start <- isolate(input$management_datatable_state$start)
          #   order <- isolate(input$management_datatable_state$order)
          #   columns <- isolate(input$management_datatable_state$columns)
          #   str(columns)
          #   # str(isolate(input$management_datatable_state))
          #   
          #   # Filter data on module family, for modules datatable
          #   # data <- r[[paste0(table, "_temp")]]
          #   # if (length(input$module_family) > 0) data <- r[[paste0(prefix, "_modules_temp")]] %>% dplyr::filter(module_family_id == input$module_family)
          #   
          #   render_settings_datatable(output = output, r = r, ns = ns, language = language, id = id, output_name = "management_datatable",
          #     col_names =  get_col_names(table_name = table, language = language), table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons,
          #     page_length = page_length, start = start, order = order, columns = columns,
          #     editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols,
          #     filter = TRUE, searchable_cols = searchable_cols, factorize_cols = factorize_cols, column_widths = column_widths)
          #     # data = data)
          # })
            
        }
        
        ##########################################
        # Save changes in datatable              #
        ##########################################
        
        # Only for data management subpages
        if (grepl("management", id)){
          
          # Hide save button if user has no access
          observeEvent(r$user_accesses, if (paste0(prefix, "_modules_edit_data") %not_in% r$user_accesses) shinyjs::hide("management_save"))
          
          # Each time a row is updated, modify temp variable
          observeEvent(input$management_datatable_cell_edit, {
            edit_info <- input$management_datatable_cell_edit
            
            if (table == "patient_lvl_modules_elements") edit_info$col <- edit_info$col + 2 # Cause some cols have been removed in datatable
            if (table == "aggregated_modules_elements") edit_info$col <- edit_info$col + 1
            
            r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
            # Store that this row has been modified
            r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
          })
          
          dropdowns_update <- switch(table,
            "patient_lvl_modules" = "patient_lvl_module",
            "aggregated_modules" = "aggregated_module")
          
          # Each time a dropdown is updated, modify temp variable
          if (table %in% c("patient_lvl_modules", "aggregated_modules")){
            observeEvent(r[[table]], {
              update_settings_datatable(input = input, r = r, ns = ns, table = table, dropdowns = dropdowns_update, language = language)
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
            req(paste0(prefix, "_modules_delete_data") %in% r$user_accesses)
            
            # Get value of deleted row
            row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))
            
            # Delete row in DB table
            delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
          })
        }
      
        #################################################
        # Modules elements / change display order       #
        #################################################
        
        if (grepl("modules_elements", id)){
          
          # Update the four dropdowns
          observeEvent(r[[paste0(prefix, "_modules_elements_temp")]], {
            
            if (r[[paste0(prefix, "_cdo_saved")]] != 1){
              cdo_modules_families <- r[[paste0(prefix, "_modules_elements_temp")]] %>%
                dplyr::distinct(module_id) %>% dplyr::left_join(r[[paste0(prefix, "_modules")]] %>% dplyr::select(module_id = id, module_family_id), by = "module_id") %>%
                dplyr::left_join(r[[paste0(prefix, "_modules_families")]] %>% dplyr::select(module_family_id = id, name), by = "module_family_id") %>%
                dplyr::group_by(name) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::arrange(name)
              
              options <- list()
              if (nrow(cdo_modules_families) > 0) options <- convert_tibble_to_list(data = cdo_modules_families, key_col = "module_family_id", text_col = "name", words = r$words)
              
              shiny.fluent::updateDropdown.shinyInput(session, "cdo_module_family", options = options, value = NULL)
              shiny.fluent::updateDropdown.shinyInput(session, "cdo_module", options = list())
              shiny.fluent::updateDropdown.shinyInput(session, "cdo_module_element", options = list())
              shiny.fluent::updateDropdown.shinyInput(session, "cdo_display_order", options = list())
            }
          })
          
          observeEvent(input$cdo_module_family, {
            req(input$cdo_module_family)
            cdo_modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == input$cdo_module_family)
            options <- list()
            if (nrow(cdo_modules) > 0) options <- convert_tibble_to_list(data = cdo_modules, key_col = "id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_module", options = options)
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_module_element", options = list())
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_display_order", options = list())
          })
          
          observeEvent(input$cdo_module, {
            req(input$cdo_module)
            cdo_modules_elements_groups <- r[[paste0(prefix, "_modules_elements_temp")]] %>% dplyr::filter(module_id == input$cdo_module) %>% 
              dplyr::group_by(group_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
            options <- list()
            if (nrow(cdo_modules_elements_groups) > 0) options <- convert_tibble_to_list(data = cdo_modules_elements_groups, key_col = "group_id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_module_element", options = options)
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_display_order", options = list())
          })
          
          observeEvent(input$cdo_module_element, {
            req(input$cdo_module_element, input$cdo_module)
            cdo_modules_elements_display_orders <- r[[paste0(prefix, "_modules_elements_temp")]] %>% dplyr::filter(module_id == input$cdo_module) %>%
              dplyr::group_by(display_order) %>% dplyr::slice(1) %>% dplyr::ungroup()
            value <- r[[paste0(prefix, "_modules_elements_temp")]] %>% dplyr::filter(group_id == input$cdo_module_element) %>% dplyr::slice(1) %>% dplyr::pull(display_order)
            options <- convert_tibble_to_list(data = cdo_modules_elements_display_orders, key_col = "display_order", text_col = "display_order", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, "cdo_display_order", options = options, value = value)
          })
          
          observeEvent(input$cdo_save, {
            
            if (length(input$cdo_display_order) == 0) show_message_bar(output = output, id = 1, "dropdown_empty", type = "severeWarning", language = language)
            
            req(length(input$cdo_display_order) > 0)
            
            # The group which had selected display order
            original_group_id <- r[[paste0(prefix, "_modules_elements_temp")]] %>% dplyr::filter(module_id == input$cdo_module, display_order == input$cdo_display_order) %>%
              dplyr::slice(1) %>% dplyr::pull(group_id)
            
            # The ID of the new group, which will have selected display order
            new_group_id <- input$cdo_module_element
            
            # If they are the same, nothing changes
            # Else, update display_orders in database
            
            if (original_group_id != new_group_id){
              
              # Display order of the new_group_id will become display order of original_group_id
              new_group_display_order <- r[[paste0(prefix, "_modules_elements_temp")]] %>% dplyr::filter(group_id == new_group_id) %>% dplyr::slice(1) %>% dplyr::pull(display_order)
              
              sql <- paste0("UPDATE ", prefix, "_modules_elements SET display_order = ", input$cdo_display_order, " WHERE group_id = ", input$cdo_module_element)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              
              sql <- paste0("UPDATE ", prefix, "_modules_elements SET display_order = ", new_group_display_order, " WHERE group_id = ", original_group_id)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              
              update_r(r = r, table = paste0(prefix, "_modules_elements"))
              
              # To prevent refresh of dropdowns
              r[[paste0(prefix, "_cdo_saved")]] <- 1L
            }
            
            show_message_bar(output = output, id = 4, "modif_saved", type ="success", language = language)
            
          })
        }
      
        #################################################
        # Patient-lvl modules elements / delete a group #
        #################################################
        
        if (id == "settings_modules_patient_lvl_modules_elements_management"){
      
          # Update the three dropdowns
          observeEvent(r$patient_lvl_modules_elements_temp, {

            dme_modules_families <- r$patient_lvl_modules_elements_temp %>%
              dplyr::distinct(module_id) %>% dplyr::left_join(r$patient_lvl_modules %>% dplyr::select(module_id = id, module_family_id), by = "module_id") %>%
              dplyr::left_join(r$patient_lvl_modules_families %>% dplyr::select(module_family_id = id, name), by = "module_family_id") %>%
              dplyr::group_by(name) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::arrange(name)
            
            options <- list()
            if (nrow(dme_modules_families) > 0) options <- convert_tibble_to_list(data = dme_modules_families, key_col = "module_family_id", text_col = "name", words = r$words)

            shiny.fluent::updateDropdown.shinyInput(session, "dme_module_family", options = options, value = NULL)
            shiny.fluent::updateDropdown.shinyInput(session, "dme_module", options = list())
            shiny.fluent::updateDropdown.shinyInput(session, "dme_module_element", options = list())
          })
          
          observeEvent(input$dme_module_family, {
            req(input$dme_module_family)
            dme_modules <- r$patient_lvl_modules %>% dplyr::filter(module_family_id == input$dme_module_family)
            options <- list()
            if (nrow(dme_modules) > 0) options <- convert_tibble_to_list(data = dme_modules, key_col = "id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, "dme_module", options = options)
            shiny.fluent::updateDropdown.shinyInput(session, "dme_module_element", options = list())
          })
          
          observeEvent(input$dme_module, {
            req(input$dme_module)
            dme_modules_elements_groups <- r$patient_lvl_modules_elements_temp %>% dplyr::filter(module_id == input$dme_module) %>% 
              dplyr::group_by(group_id) %>% dplyr::slice(1) %>% dplyr::ungroup()
            options <- list()
            if (nrow(dme_modules_elements_groups) > 0) options <- convert_tibble_to_list(data = dme_modules_elements_groups, key_col = "group_id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, "dme_module_element", options = options)
          })
          
          # Create & show dialog box
          observeEvent(r$patient_lvl_modules_elements_group_delete_dialog, {
            
            dialogContentProps <- list(
              type = 0,
              title = translate(language, paste0(table, "_group_delete"), r$words),
              closeButtonAriaLabel = "Close",
              subText = translate(language, paste0(table, "_group_delete_subtext"), r$words)
            )
            
            shiny.fluent::Dialog(
              hidden = !r$patient_lvl_modules_elements_group_delete_dialog,
              onDismiss = htmlwidgets::JS("function() { Shiny.setInputValue('modules_elements_group_hide_dialog', Math.random()); }"),
              dialogContentProps = dialogContentProps,
              modalProps = list(),
              shiny.fluent::DialogFooter(
                shiny.fluent::PrimaryButton.shinyInput(ns("modules_elements_group_delete_confirmed"), text = translate(language, "delete", r$words)),
                shiny.fluent::DefaultButton.shinyInput(ns("modules_elements_group_delete_canceled"), text = translate(language, "dont_delete", r$words))
              )
            ) -> render_react
            
            output$delete_confirm <- shiny.fluent::renderReact(render_react)
          })
          
          # Whether to close or not delete dialog box
          observeEvent(input$modules_elements_group_hide_dialog, r$patient_lvl_modules_elements_group_delete_dialog <- FALSE)
          observeEvent(input$modules_elements_group_delete_canceled, r$patient_lvl_modules_elements_group_delete_dialog <- FALSE)
          observeEvent(input$dme_delete, {
            req(length(input$dme_module_element) > 0)
            r$patient_lvl_modules_elements_group_delete_dialog <- TRUE 
          })
          
          # When the delete is confirmed...
          observeEvent(input$modules_elements_group_delete_confirmed, {
            
            # If user has access
            req(paste0(prefix, "_modules_delete_data") %in% r$user_accesses)
            
            r$patient_lvl_modules_elements_group_delete_dialog <- FALSE
            
            sql <- paste0("UPDATE patient_lvl_modules_elements SET deleted = TRUE WHERE group_id = ", input$dme_module_element)
            query <- DBI::dbSendStatement(r$db, sql)
            DBI::dbClearResult(query)
            
            show_message_bar(output = output, id = 4, "module_element_deleted", type ="severeWarning", language = language)
            
            update_r(r = r, table = table, language = language)
            
          })
        }
    
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      # Only for accesses sub-page
      # We have to use same module than management, to get input from datatable
      if (grepl("modules_families", id)){
        
        observeEvent(input$options, {
          # Show options toggle
          r[[paste0(prefix, "_modules_families_options")]] <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
        })
      }
      
      if (grepl("options", id)){
        
        observeEvent(r[[paste0(prefix, "_modules_families_options")]], {
          req(r[[paste0(prefix, "_modules_families_options")]] > 0)
          
          # If user has access
          req(paste0(prefix, "_modules_options_card") %in% r$user_accesses)
          
          
          # Render UI of options card
          output$options_card <- renderUI({
            
            # Get category & link_id to get informations in options table
            
            make_card("title", "content")
            
            category <- get_singular(word = table)
            link_id <- r[[paste0(prefix, "_modules_families_options")]]

            render_settings_options_card(ns = ns, id = id, r = r, title = paste0(get_singular(table), "_options"),
              category = category, link_id = link_id, language = language)
          })
          
          
          # When save button is clicked
          observeEvent(input$options_save, {
            
            # If user has access
            req(paste0(prefix, "_modules_options_card") %in% r$user_accesses)
            
            category <- get_singular(word = table)
            
            data <- list()
            data$users_allowed_read_group <- input$users_allowed_read_group
            data$users_allowed_read <- input$users_allowed_read
            
            save_settings_options(output = output, r = r, id = id, category = category,
              code_id_input = paste0("options_", r[[paste0(prefix, "_modules_families_options")]]), language = language, data = data)
            
          })
        })
      }  
  })
}