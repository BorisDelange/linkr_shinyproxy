#' patient_and_aggregated_data_study UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_study_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  # Prefix depending on page id
  if (id == "patient_level_data_study"){
    prefix <- "patient_lvl"
    page_name <- "patient_level_data"
  } 
  if (id == "aggregated_data_study"){
    prefix <- "aggregated"
    page_name <- "aggregated_data"
  }
  
  ##########################################
  # Module creation card                   #
  ##########################################
  
  module_creation_options <- list(
    list(key = "same_level", text = translate(language, "same_level", words)),
    list(key = "level_under", text = translate(language, "level_under", words))
  )
  
  module_creation_card <- make_card(
    title = translate(language, "add_module", words),
    content = div(
      actionButton(ns(paste0(prefix, "_close_add_module")), "", icon = icon("times"), style = "position:absolute; top:10px; right: 10px;"),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
        make_textfield(language = language, ns = ns, label = "name", id = "module_name", width = "300px", words = words),
        div(shiny.fluent::ChoiceGroup.shinyInput(ns("add_module_type"), value = "same_level", 
          options = module_creation_options, className = "inline_choicegroup"), style = "padding-top:35px;")
      ), br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("add_module_button"), translate(language, "add", words)), br(),
    )
  )
  
  ##########################################
  # Module element creation card           #
  ##########################################
  
  if (prefix == "patient_lvl"){
    module_element_creation_card <- make_card(
      title = translate(language, "add_module_element", words),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_module_element")), "", icon = icon("times"), style = "position:absolute; top:10px; right: 10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(language = language, ns = ns, label = "name", id = "module_element_name", width = "300px", words = words),
          make_combobox(language = language, ns = ns, label = "plugin", id = "plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px", words = words)
        ),
        make_combobox(language = language, ns = ns, label = "thesaurus", id = "thesaurus", allowFreeform = FALSE, multiSelect = FALSE, width = "300px", words = words),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          make_dropdown(language = language, ns = ns, label = "thesaurus_selected_items", id = "thesaurus_selected_items",
            multiSelect = TRUE, width = "650px", words = words),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("reset_thesaurus_items"), translate(language, "reset", words)), style = "margin-top:38px;")
        ),
        div(DT::DTOutput(ns("module_element_thesaurus_items")), class = "thesaurus_table"), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element_button"), translate(language, "add", words)), br(),
        DT::DTOutput(ns("thesaurus_items"))
      )
    )
  }
  
  if (prefix == "aggregated"){
    module_element_creation_card <- make_card(
      title = translate(language, "add_module_element", words),
      content = div(
        actionButton(ns(paste0(prefix, "_close_add_module_element")), "", icon = icon("times"), style = "position:absolute; top:10px; right: 10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
          make_textfield(language = language, ns = ns, label = "name", id = "module_element_name", width = "300px", words = words),
          make_combobox(language = language, ns = ns, label = "plugin", id = "plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "300px", words = words)
        ),
        br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add_module_element_button"), translate(language, "add", words)),
      )
    )
  }
  
  div(
    div(
      class = "message_bars",
      shiny::uiOutput(ns("message_bar1")), shiny::uiOutput(ns("message_bar2")), shiny::uiOutput(ns("message_bar3")), 
      shiny::uiOutput(ns("message_bar4")), shiny::uiOutput(ns("message_bar5"))
    ), 
    shiny.fluent::reactOutput(ns("module_delete_confirm")), shiny.fluent::reactOutput(ns("module_element_delete_confirm")),
    uiOutput(ns("study_menu")),
    uiOutput(ns("study_cards")),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_module_element")),
        module_element_creation_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_module")),
        module_creation_card,
        style = "position:relative;"
      )
    )
  )
}
    
#' patient_and_aggregated_data_study Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_study_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data_study"){
      prefix <- "patient_lvl"
      page_name <- "patient_level_data"
    } 
    if (id == "aggregated_data_study"){
      prefix <- "aggregated"
      page_name <- "aggregated_data"
    }
    
    ##########################################
    # STUDY - INITIATE UI                    #
    ##########################################
    
    observeEvent(r$chosen_study, {
      
      req(!is.na(r$chosen_study))
      
      # Reset selected key
      r[[paste0(prefix, "_selected_module")]] <- NA_integer_
      
      # Reset shown modules
      r[[paste0(prefix, "_opened_cards")]] <- ""
      
      # Hide Add module element card & Add module
      shinyjs::hide("add_module_element")
      shinyjs::hide("add_module")
      
      r[[paste0(prefix, "_load_ui")]] <- Sys.time()
      r[[paste0(prefix, "_load_server")]] <- Sys.time()
      
      # Load modules variables
      update_r(r = r, table = paste0(prefix, "_modules_families"))
      update_r(r = r, table = paste0(prefix, "_modules"))
      update_r(r = r, table = paste0(prefix, "_modules_elements"))
      
    })
    
    # observeEvent(r$study_page, {
    #   
    #   req(!is.na(r$chosen_study))
    #   
    #   # Show datamart UI, hide other UIs
    #   shinyjs::show("study_main")
    #   sapply(c("datamart_main", "subset_main"), shinyjs::hide)
    # })
    
    ##########################################
    # DELETE A MODULE                        #
    ##########################################
    
    module_delete_prefix <- paste0(prefix, "_module")
    module_dialog_title <- paste0(prefix, "_modules_delete")
    module_dialog_subtext <- paste0(prefix, "_modules_delete_subtext")
    module_react_variable <- "module_delete_confirm"
    module_table <- paste0(prefix, "_modules")
    module_id_var_sql <- "id"
    module_id_var_r <- paste0(prefix, "_selected_module")
    module_delete_message <- paste0(prefix, "_module_deleted")
    module_reload_variable <- paste0(prefix, "_load_ui")
    module_information_variable <- paste0(prefix, "_module_deleted")
    module_delete_variable <- paste0(module_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, language = language,
      delete_prefix = module_delete_prefix, dialog_title = module_dialog_title, dialog_subtext = module_dialog_subtext,
      react_variable = module_react_variable, table = module_table, id_var_sql = module_id_var_sql, id_var_r = module_id_var_r, 
      delete_message = module_delete_message, translation = TRUE, reload_variable = module_reload_variable, 
      information_variable = module_information_variable)
    
    # When a module is deleted, change current module variable
    
    observeEvent(r[[module_information_variable]], {
      
      table <- paste0(prefix, "_modules")
      deleted_module_id <- r[[paste0(prefix, "_module_deleted")]]
      sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_module_id}", .con = r$db)
      module_deleted <- DBI::dbGetQuery(r$db, sql)
      
      # If we are at level one, take first module of level one
      if (is.na(module_deleted$parent_module_id)){
        show_module_id <- r[[table]] %>% 
          dplyr::filter(module_family_id == module_deleted$module_family_id & is.na(parent_module_id) & id != module_deleted$id) %>%
          dplyr::arrange(display_order) %>%
          dplyr::slice(1) %>%
          dplyr::pull(id)
      }
      
      # Else, take first module of the same level
      if (!is.na(module_deleted$parent_module_id)){
        show_module <- r[[table]] %>% 
          dplyr::filter(parent_module_id == module_deleted$parent_module_id & id != module_deleted$id)
        
        # If not any module in this level, take lower level
        if (nrow(show_module) == 0) show_module <- r[[table]] %>% 
            dplyr::filter(id == module_deleted$parent_module_id)
        
        show_module_id <- show_module %>%
          dplyr::arrange(display_order) %>%
          dplyr::slice(1) %>%
          dplyr::pull(id)
      }
      
      r[[paste0(prefix, "_selected_module")]] <- paste0("show_module_", show_module_id)
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::show(paste0(prefix, "_toggles_", show_module_id))
      
    })
    
    ##########################################
    # DELETE A MODULE ELEMENT                #
    ##########################################
    
    module_element_delete_prefix <- paste0(prefix, "_module_element")
    module_element_dialog_title <- paste0(prefix, "_modules_elements_group_delete")
    module_element_dialog_subtext <- paste0(prefix, "_modules_elements_group_delete_subtext")
    module_element_react_variable <- "module_element_delete_confirm"
    module_element_table <- paste0(prefix, "_modules_elements")
    module_element_id_var_sql <- "group_id"
    module_element_id_var_r <- paste0(prefix, "_selected_module_element")
    module_element_delete_message <- paste0(prefix, "_module_element_group_deleted")
    module_element_reload_variable <- paste0(prefix, "_load_ui")
    module_element_delete_variable <- paste0(module_element_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, language = language,
      delete_prefix = module_element_delete_prefix, dialog_title = module_element_dialog_title, dialog_subtext = module_element_dialog_subtext,
      react_variable = module_element_react_variable, table = module_element_table, id_var_sql = module_element_id_var_sql, id_var_r = module_element_id_var_r, 
      delete_message = module_element_delete_message, translation = TRUE, reload_variable = module_element_reload_variable)
    
    
    ##########################################
    # ADD A MODULE                           #
    ##########################################
    
    observeEvent(input$study_current_tab, {
      
      input$study_current_tab
      
      req(grepl("add_module", input$study_current_tab))
      
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::hide(paste0(prefix, "_add_module_element"))
      shinyjs::show(paste0(prefix, "_add_module"))
    })
    
    # Close creation div
    
    observeEvent(input[[paste0(prefix, "_close_add_module")]], {
      
      # Show opened cards before opening Add module element div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide Add module element div
      shinyjs::hide(paste0(prefix, "_add_module"))
    })
    
    # Add button clicked
    
    observeEvent(input$add_module_button, {
      
      study <- r$studies %>% dplyr::filter(id == r$chosen_study)
      module <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_module")]])
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$module_name)
      new_data$description <- ""
      
      # Required textfields
      required_textfields <- "name"
      
      # Fields requiring unique value
      req_unique_values <- "name"
      
      # Get module_family_id
      new_data$module_family <- study %>% dplyr::pull(paste0(prefix, "_module_family_id"))
      
      # If it is the first module to be created
      if (nrow(module) == 0){
        new_data$parent_module <- NA_integer_
        new_data$display_order <- 1
      }
      
      # If already existing modules
      if (nrow(module) > 0){
        
        # If module is at the same level of current module, get common parent_module_id
        # Calculate display order
        
        if (input$add_module_type == "same_level") new_data$parent_module <- module %>% dplyr::pull(parent_module_id)
        if (input$add_module_type == "level_under") new_data$parent_module <- module %>% dplyr::pull(id)
        
        # Calculate display order
        if (!is.na(new_data$parent_module)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_modules')`}
          WHERE module_family_id = {new_data$module_family} AND parent_module_id = {new_data$parent_module}", .con = r$db)
        if (is.na(new_data$parent_module)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_modules')`}
          WHERE module_family_id = {new_data$module_family} AND parent_module_id IS NULL", .con = r$db)
        
        new_data$display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() + 1
        
        # Can't add a module at the level under if there are modules elements attached to current module
        if (input$add_module_type == "level_under"){
          modules_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == r[[paste0(prefix, "_selected_module")]], !deleted)
          if (nrow(modules_elements) > 0) show_message_bar(output = output, id = 3, message = "add_module_has_modules_elements", language = language)
          req(nrow(modules_elements) == 0)
        }
      }
      
      if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "module_name", errorMessage = translate(language, "provide_valid_name", r$words))
      req(!is.na(new_data$name))
      
      add_settings_new_data(session = session, output = output, r = r, language = language, id = id,
        data = new_data, table = paste0(prefix, "_modules"), required_textfields = required_textfields, req_unique_values = req_unique_values)
      
      # Reset fields
      
      shiny.fluent::updateTextField.shinyInput(session, "module_name", value = "")
      shiny.fluent::updateChoiceGroup.shinyInput(session, "add_module_type", value = "same_level")
      
      # Reload UI, with new module opened
      
      r[[paste0(prefix, "_selected_module")]] <- get_last_row(r$db, paste0(prefix, "_modules"))
      r[[paste0(prefix, "_load_ui")]] <- Sys.time()
      
      # Hide currently opened cards
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      
      # Code to make Add module element button work
      observeEvent(input[[paste0(prefix, "_add_module_element_", r[[paste0(prefix, "_selected_module")]])]], {
        
        # Hide opened cards
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        
        # Show Add module element div
        shinyjs::show(paste0(prefix, "_add_module_element"))
        
      }, ignoreInit = TRUE)
      
      # Code to make Remove module button work
      observeEvent(input[[paste0(prefix, "_remove_module_", r[[paste0(prefix, "_selected_module")]])]], r[[module_delete_variable]] <- TRUE)
      
    })
    
    ##########################################
    # ADD A MODULE ELEMENT                   #
    ##########################################
    
    observeEvent(r$plugins, {
      
      module_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
      
      plugins <- r$plugins %>% dplyr::filter(module_type_id == !!module_type_id)
      
      options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
      shiny.fluent::updateComboBox.shinyInput(session, "plugin", options = options)
    })
    
    # Only for patient-lvl data
    
    if (prefix == "patient_lvl"){
      
      ##########################################
      # THESAURUS DATATABLE                    #
      ##########################################
      
      # Load thesaurus attached to this datamart
      observeEvent(r$chosen_datamart, {
        
        req(!is.na(r$chosen_datamart))
        
        data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id) %>% as.character()
        
        # Multiple cases
        # Only one ID, so it's the beginning and the end
        # Last ID, so it's the end
        # ID between begin and last, so separated by commas
        thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
          grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
        shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
      })
      
      # Load thesaurus items
      observeEvent(input$thesaurus, {
        
        r$module_element_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus$key, category = "plus_module")
        
        colour_col <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = input$thesaurus$key, category = "colours_module")
        
        if (nrow(colour_col) > 0) r$module_element_thesaurus_items <- r$module_element_thesaurus_items %>%
          dplyr::left_join(colour_col %>% dplyr::select(id, colour), by = "id") %>% dplyr::relocate(colour, .before = "datetime")
        
        count_items_rows <- tibble::tibble()
        count_patients_rows <- tibble::tibble()
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
          datamart_id = r$chosen_datamart, category = "count_items_rows"),
            error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0("modules - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = input$thesaurus$key,
          datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
            error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0("modules - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = language))
        
        if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language, words = r$words)
        req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
        
        # Transform count_rows cols to integer, to be sortable
        r$module_element_thesaurus_items <- r$module_element_thesaurus_items %>%
          dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
          dplyr::left_join(count_items_rows, by = "item_id") %>%
          dplyr::left_join(count_patients_rows, by = "item_id") %>%
          dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
          dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
        
        # Filter on count_items_rows > 0
        r$module_element_thesaurus_items <- r$module_element_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
        
        r$module_element_thesaurus_items_temp <- r$module_element_thesaurus_items %>%
          dplyr::mutate(modified = FALSE) %>%
          dplyr::mutate_at("item_id", as.character)
        
        editable_cols <- c("display_name", "unit")
        searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
        factorize_cols <- c("category", "unit")
        column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px")#, 
        # "category" = "300px", "colour" = "100px")
        sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
        centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        col_names <- get_col_names(table_name = "modules_thesaurus_items_with_counts", language = language, words = r$words)
        hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
        
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, language = language, data = r$module_element_thesaurus_items_temp,
          output_name = "module_element_thesaurus_items", col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        # Create a proxy for datatatable
        r$module_element_thesaurus_items_proxy <- DT::dataTableProxy("module_element_thesaurus_items", deferUntilFlush = FALSE)
        
      })
      
      # Reload datatable
      
      observeEvent(r$module_element_thesaurus_items_temp, {
        
        # Reload data of datatable
        DT::replaceData(r$module_element_thesaurus_items_proxy, r$module_element_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
      })
      
      # Updates in datatable
      
      observeEvent(input$module_element_thesaurus_items_cell_edit, {
        
        edit_info <- input$module_element_thesaurus_items_cell_edit
        
        r$module_element_thesaurus_items_temp <- DT::editData(r$module_element_thesaurus_items_temp, edit_info, rownames = FALSE)
        r$module_element_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      })
      
      ##########################################
      # THESAURUS ITEMS                        #
      ##########################################
      
      # When add button is clicked
      observeEvent(input$item_selected, {
        
        # Initiate r variable if doesn't exist
        if (length(r$module_element_thesaurus_selected_items) == 0){
          r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
            thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
            thesaurus_item_colour = character(), input_text = character()) 
        }
        
        # Get ID of chosen thesaurus item
        link_id <- as.integer(substr(input$item_selected, nchar("select_") + 1, nchar(input$item_selected)))
        
        # If this thesaurus item is not already chosen, add it to the "thesaurus selected items" dropdown
        
        value <- integer(1)
        if (nrow(r$module_element_thesaurus_selected_items) > 0) value <- r$module_element_thesaurus_selected_items %>% 
          dplyr::filter(thesaurus_id == input$thesaurus$key) %>% dplyr::pull(id)
        
        if (link_id %not_in% value){

          # Get thesaurus name
          thesaurus_name <- r$thesaurus %>% dplyr::filter(id == input$thesaurus$key) %>% dplyr::pull(name)

          # Get item informations from datatable / r$modules_thesaurus_items
          # NB : the thesaurus_item_id saved in the database is the thesaurus ITEM_ID, no its ID in the database (in case thesaurus is deleted or re-uploaded)
          item <- r$module_element_thesaurus_items_temp %>% dplyr::filter(id == link_id) %>% dplyr::mutate(input_text = paste0(thesaurus_name, " - ", name))

          # display_name <- ifelse((item$display_name == "" | is.na(item$display_name)), item$name, item$display_name)

          # Add item to selected items
          r$module_element_thesaurus_selected_items <-
            tibble::tribble(~id, ~thesaurus_id, ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~input_text,
              as.integer(link_id), as.integer(input$thesaurus$key), as.character(thesaurus_name), as.integer(item$item_id), as.character(item$display_name),
              as.character(item$unit), as.character(input[[paste0("colour_", link_id)]]), as.character(item$input_text)) %>%
            dplyr::bind_rows(r$module_element_thesaurus_selected_items)

          # Update dropdown of selected items
          options <- convert_tibble_to_list(r$module_element_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
          value <- r$module_element_thesaurus_selected_items %>% dplyr::pull(id)
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
            options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
        }
        
      })
      
      # When reset button is clicked
      observeEvent(input$reset_thesaurus_items, {
        # Reset r$modules_thesaurus_selected_items
        r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
          thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
          thesaurus_item_colour = character(), input_text = character()) 
        
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
      
      # When dropdown is modified
      observeEvent(input$thesaurus_selected_items, {
        
        r$module_element_thesaurus_selected_items <- r$module_element_thesaurus_selected_items %>%
          dplyr::filter(id %in% input$thesaurus_selected_items)
        options <- convert_tibble_to_list(r$module_element_thesaurus_selected_items %>% dplyr::arrange(thesaurus_item_display_name), key_col = "id", text_col = "input_text", words = r$words)
        value <- r$module_element_thesaurus_selected_items %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      })
      
    }
    # End of if(prefix == "patient_lvl")
    
      ##########################################
      # Add button clicked                     #
      ##########################################
      
      observeEvent(input$add_module_element_button, {
        
        new_data <- list()
        
        new_data$name <- coalesce2(type = "char", x = input$module_element_name)
        new_data$module_family <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_module")]]) %>% dplyr::pull(module_family_id)
        new_data$module_new_element <- r[[paste0(prefix, "_selected_module")]]
        new_data$plugin <- input$plugin$key
        
        # Check if name is not empty
        if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "module_element_name", errorMessage = translate(language, "provide_valid_name", words))
        else shiny.fluent::updateTextField.shinyInput(session, "module_element_name", errorMessage = NULL)
        req(!is.na(new_data$name))
        
        # Check if values required to be unique are unique
        
        table <- paste0(prefix, "_modules_elements")
        
        sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND module_id = {new_data$module_new_element}", .con = r$db)
        distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        if (new_data$name %in% distinct_values) show_message_bar(output, 2, "name_already_used", "severeWarning", language, words = r$words)
        req(new_data$name %not_in% distinct_values)
        
        # Check if dropdowns are not empty (if all are required)
        dropdowns_check <- TRUE
        
        required_dropdowns <- c("plugin")
        
        sapply(required_dropdowns, function(dropdown){
          if (is.null(new_data[[dropdown]])) dropdowns_check <<- FALSE
          else if (is.na(new_data[[dropdown]])) dropdowns_check <<- FALSE
        })
        
        if (!dropdowns_check) show_message_bar(output, 2, "dropdown_empty", "severeWarning", language, words = r$words)
        req(dropdowns_check)
        
        # Get last_row nb
        last_row <- get_last_row(r$db, table)
        group_id <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table)) %>% dplyr::pull() %>% as.integer() + 1
        last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(group_id), 0) FROM ", table, " WHERE module_id = ", new_data$module_new_element)) %>% dplyr::pull() %>% as.integer()
        
        has_thesaurus_items <- TRUE
        
        if (prefix == "patient_lvl"){
          
          if (length(r$module_element_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          if (length(r$module_element_thesaurus_selected_items) > 0) if (nrow(r$module_element_thesaurus_selected_items) == 0) has_thesaurus_items <- FALSE
          
          if (has_thesaurus_items){
            new_data <- 
              r$module_element_thesaurus_selected_items %>%
              dplyr::transmute(
                id = 1:dplyr::n() + last_row + 1,
                name = as.character(new_data$name),
                group_id = !!group_id,
                module_id = as.integer(new_data$module_new_element),
                plugin_id = as.integer(new_data$plugin),
                thesaurus_name, thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit, thesaurus_item_colour,
                display_order = last_display_order + 1,
                creator_id = r$user_id,
                datetime = as.character(Sys.time()),
                deleted = FALSE
              )
          }
          if (!has_thesaurus_items){
            new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, ~thesaurus_name, ~thesaurus_item_id,
              ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~display_order, ~creator_id, ~datetime, ~deleted,
              last_row + 1, as.character(new_data$name), group_id, as.integer(new_data$module_new_element), as.integer(new_data$plugin),
              "None", 0L, "None", "", "", last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
          }
        }
        
        if (prefix == "aggregated") new_data <- tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, 
          ~display_order, ~creator_id, ~datetime, ~deleted,
          last_row + 1, as.character(new_data$name), group_id, as.integer(new_data$module_new_element),
          as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
        
        DBI::dbAppendTable(r$db, table, new_data)
        add_log_entry(r = r, category = paste0(table, " - ", translate(language, "insert_new_data", words)), name = translate(language, "sql_query", words), value = toString(new_data))
        
        show_message_bar(output = output, id = 3, message = paste0(get_singular(table), "_added"), type = "success", language = language, words = r$words)
        
        update_r(r = r, table = table, language = language)
        
        # Reset name textfield & dropdowns
        shiny.fluent::updateTextField.shinyInput(session, "module_element_name", value = "")
        if (prefix == "patient_lvl" & has_thesaurus_items){
          
          # Save thesaurus items for server code first
          thesaurus_selected_items <-
            r$module_element_thesaurus_selected_items %>%
            dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
              thesaurus_item_unit, colour = thesaurus_item_colour)
          
          r$module_element_thesaurus_selected_items <- tibble::tibble(id = integer(), thesaurus_id = integer(), thesaurus_name = character(),
            thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
            thesaurus_item_colour = character(), input_text = character()) 
          shiny.fluent::updateDropdown.shinyInput(session, "thesaurus_selected_items", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
        }
        
        # Run server code
        
        trace_code <- paste0(prefix, "_", group_id, "_", r$chosen_study)
        if (trace_code %not_in% r$server_modules_groups_loaded){
          
          # Add the trace_code to loaded plugins list
          r$server_modules_groups_loaded <- c(r$server_modules_groups_loaded, trace_code)
          
          # Get plugin code
          
          # Check if plugin has been deleted
          check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", input$plugin$key)) %>% dplyr::pull(deleted)
          if (!check_deleted_plugin){
            
            code_server_card <- r$code %>%
              dplyr::filter(link_id == input$plugin$key, category == "plugin_server") %>%
              dplyr::pull(code) %>%
              stringr::str_replace_all("%module_id%", as.character(r[[paste0(prefix, "_selected_module")]])) %>%
              stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
              stringr::str_replace_all("\r", "\n")
          
            # If it is an aggregated plugin, change %study_id% with current chosen study
            if (length(r$chosen_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(r$chosen_study))
          }
          else code_server_card <- ""
            
          tryCatch(eval(parse(text = code_server_card)),
            error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
              error_name = paste0(id, " - run server code - ", group_id), category = "Error", error_report = e, language = language)
          )
          
          # Code for toggle reactivity
          toggle <- paste0(prefix, "_group_", group_id)
          
          observeEvent(input[[paste0(toggle, "_toggle")]], {
            if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
            else shinyjs::hide(toggle)
          })
          
          # Code for removing module element
          
          observeEvent(input[[paste0(prefix, "_remove_module_element_", group_id)]], {
            r[[paste0(prefix, "_selected_module_element")]] <- group_id
            r[[module_element_delete_variable]] <- TRUE
          })
          
        }
        
        # Reload UI code
        r[[paste0(prefix, "_load_ui")]] <- Sys.time()
        
        shinyjs::hide(paste0(prefix, "_add_module_element"))
        
      })
    
      ##########################################
      # Close creation div                     #
      ##########################################
      
      observeEvent(input[[paste0(prefix, "_close_add_module_element")]], {
        
        # Show opened cards before opening Add module element div
        sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
        
        # Hide Add module element div
        shinyjs::hide(paste0(prefix, "_add_module_element"))
      })
    
    
    
    
    ##########################################
    # LOAD UI                                #
    ##########################################
    
      ##########################################
      # Load modules                           #
      ##########################################
      
      observeEvent(r[[paste0(prefix, "_load_ui")]], {
        
        req(!is.na(r$chosen_study))
        
        # Show study UI, hide other UIs
        # r$study_page <- Sys.time()
        
        # Load study informations
        # For one study, you choose ONE patient_lvl or aggregated data module family
        study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))
        
        # Load modules belonging to this module family
        display_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, "_modules WHERE module_family_id = ",
          study_infos[[paste0(prefix, "_module_family_id")]], " AND deleted IS FALSE"))
        
        # Modules without parent are set to level 1
        display_modules <- display_modules %>% 
          dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))
        
        # Prevent infinite loop, max loops = 7
        i <- 1
        
        # Creating levels for distinct modules
        while(nrow(display_modules %>% dplyr::filter(is.na(level))) > 0 & i <= 7){
          display_modules <-
            display_modules %>%
            dplyr::left_join(display_modules %>%
              dplyr::filter(!is.na(level)) %>%
              dplyr::transmute(parent_module_id = id, parent_level = level), by = "parent_module_id") %>%
            dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
            dplyr::select(-parent_level)
          i <- i + 1
        }
        
        # Exclude modules without level
        display_modules <- display_modules %>% dplyr::filter(!is.na(level))
        
        # Now we have a level for each module, order them by display order
        r[[paste0(prefix, "_display_modules")]] <- display_modules %>% dplyr::arrange(level, display_order)
        
        # If this is not just a change of pivot order
        if (!grepl("change_pivot_order", r[[paste0(prefix, "_load_ui")]])) r[[paste0(prefix, "_load_ui_cards")]] <- Sys.time()
        
      })
      
      ##########################################
      # Render menu                            #
      ##########################################
      
      # Render menu
      output$study_menu <- renderUI({
        
        req(isolate(r[[paste0(prefix, "_display_modules")]]))
        r[[paste0(prefix, "_load_ui_cards")]]
        
        # Check if users has access only to aggregated data
        r$options %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart & name == "show_only_aggregated_data") %>%
          dplyr::pull(value_num) -> show_only_aggregated_data
        
        if (prefix == "patient_lvl" & show_only_aggregated_data == 1) show_message_bar(output, 1, "only_aggregated_data_authorized", "severeWarning", language)
        req((prefix == "patient_lvl" & show_only_aggregated_data != 1) | prefix == "aggregated")
        
        display_modules <- isolate(r[[paste0(prefix, "_display_modules")]])
        
        # If no module to show, notificate user
        if (nrow(display_modules) == 0 | "level" %not_in% names(display_modules)){
          return(tagList(
            shiny.fluent::Breadcrumb(items = list(
              list(key = "main", text = paste0(translate(language, paste0(prefix, "_data"), words), " - ", translate(language, "study", words)), href = paste0("#!/", page_name), isCurrentItem = TRUE,
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', 0)")))),
              maxDisplayedItems = 3),
            shiny.fluent::Pivot(
              onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', item.props.id)")),
              selectedKey = r[[paste0(prefix, "_selected_module")]],
              shiny.fluent::PivotItem(id = paste0(prefix, "_add_module_", 0), itemIcon = "Add")
            )
          ))
        }
        
        req(nrow(display_modules > 0) & "level" %in% names(display_modules) & !is.na(r$chosen_study))
        
        # First module shown
        first_module_shown <- display_modules %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
        if (max(display_modules$level) >= 2){
          sapply(2:max(display_modules$level), function(current_level){
            children <- display_modules %>% dplyr::filter(level == current_level, parent_module_id == first_module_shown$id) %>% dplyr::slice(1)
            if (nrow(children) > 0) first_module_shown <<- children
          })
        }
        
        shown_modules_temp <- tibble::tibble()
        
        # If we are at level one, show all levels one
        if (first_module_shown$level == 1){
          shown_modules <- display_modules %>% dplyr::filter(level == 1)
        }
        
        # Else, show only current & those who has same level & same parent
        if (first_module_shown$level > 1){
          shown_modules_temp <- display_modules %>% dplyr::filter(level == first_module_shown$level & parent_module_id == first_module_shown$parent_module_id)
          if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
          if (nrow(shown_modules_temp) == 0) shown_modules <- first_module_shown
        }
        
        r[[paste0(prefix, "_first_module_shown")]] <- first_module_shown$id
        
        if (length(input$study_current_tab) > 0){
          
          # If value = 0, go back to first level
          if (input$study_current_tab == 0){
            shown_modules <- display_modules %>% dplyr::filter(level == 1)
            r[[paste0(prefix, "_selected_module")]] <- first_module_shown$id
          } 
          else {
            
            if (grepl("show_module", isolate(r[[paste0(prefix, "_selected_module")]]))){
              study_current_tab <- as.integer(substr(isolate(r[[paste0(prefix, "_selected_module")]]), nchar("show_module_") + 1, 100))
              shown_modules_temp <- display_modules %>% dplyr::filter(parent_module_id == study_current_tab)
            }
            else if (grepl("add_module", input$study_current_tab)){
              shown_modules_temp <- tibble::tibble()
              study_current_tab <- isolate(r[[paste0(prefix, "_selected_module")]])
            }
            else {
              study_current_tab <- input$study_current_tab
              shown_modules_temp <- display_modules %>% dplyr::filter(parent_module_id == study_current_tab)
            }
            
            # If current tab has children
            if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
            
            # If current tab has no children
            if (nrow(shown_modules_temp) == 0){
              current_module <- display_modules %>% dplyr::filter(id == study_current_tab)
              if (nrow(current_module) > 0) shown_modules <- display_modules %>% dplyr::filter(parent_module_id == current_module$parent_module_id & level == current_module$level)
              else show_modules <- tibble::tibble()
              
              # If not any "brother", we are at level one
              if (nrow(shown_modules) == 0){
                shown_modules <- display_modules %>% dplyr::filter(level == 1)
              }
            }
          }
        }
        
        # Currently selected tab
        
        # We have just deleted a module
        if (grepl("show_module", isolate(r[[paste0(prefix, "_selected_module")]]))) r[[paste0(prefix, "_selected_module")]] <- 
          as.integer(substr(isolate(r[[paste0(prefix, "_selected_module")]]), nchar("show_module_") + 1, 100))
        
        # First existing module
        else if (length(input$study_current_tab) == 0) r[[paste0(prefix, "_selected_module")]] <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
        
        # We have clicked on a tab
        else if (length(input$study_current_tab) > 0){
          
          # Current module has children, take the first of this level of modules
          if (nrow(shown_modules_temp) > 0) r[[paste0(prefix, "_selected_module")]] <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
          
          # Take the input as current module
          else if (!grepl("add_module", input$study_current_tab) & input$study_current_tab != 0) r[[paste0(prefix, "_selected_module")]] <- input$study_current_tab
        }
        
        nb_levels <- max(shown_modules$level)
        
        # First level
        is_current_item <- FALSE
        if (nb_levels == 1) is_current_item <- TRUE
        
        items <- list(
          list(key = "main", text = translate(language, page_name, words), href = paste0("#!/", page_name), isCurrentItem = is_current_item,
            onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', 0)"))))
        
        # Other levels
        if (nb_levels >= 2){
          
          # Remove last level
          modules_tree <- display_modules %>% dplyr::filter(level < nb_levels)
          
          current_parent <- NA_integer_
          sapply(nb_levels:1, function(current_level){
            if (!is.na(current_parent)){
              modules_tree <<- modules_tree %>% dplyr::filter(level != current_level | id == current_parent)
              current_parent <<- display_modules %>% dplyr::filter(id == current_parent) %>% dplyr::pull(parent_module_id)
            }
            if (is.na(current_parent)) current_parent <<- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(parent_module_id)
          })
          modules_tree <- modules_tree %>% dplyr::arrange(level)
          sapply(1:nrow(modules_tree), function(i){
            is_current_item <- FALSE
            if (modules_tree[[i, "level"]] == nb_levels) is_current_item <- TRUE
            items <<- rlist::list.append(items, list(
              key = modules_tree[[i, "name"]], text = modules_tree[[i, "name"]], href = paste0("#!/", page_name), isCurrentItem = is_current_item,
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', ", modules_tree[[i, "id"]], ")"))
            ))
          })
        }
        
        shown_tabs <- tagList()
        
        sapply(1:nrow(shown_modules), function(i){
          shown_tabs <<- tagList(shown_tabs, shiny.fluent::PivotItem(id = shown_modules[[i, "id"]], itemKey = shown_modules[[i, "id"]], headerText = shown_modules[[i, "name"]]))
        })
        
        # Add an add button, to add a new module
        shown_tabs <- tagList(shown_tabs, shiny.fluent::PivotItem(id = paste0(prefix, "_add_module_", r[[paste0(prefix, "_selected_module")]]), itemIcon = "Add"))
        
        tagList(
          shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3),
          shiny.fluent::Pivot(
            id = paste0(prefix, "_study_pivot"),
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-study_current_tab', item.props.id)")),
            selectedKey = r[[paste0(prefix, "_selected_module")]],
            shown_tabs
          ),
          # A script to use sortable with PivotItems
          tags$script(paste0('$("#', prefix, '_study_pivot").children().first().attr("id", "', prefix, '_pivot_tabs");')),
          sortable::sortable_js(paste0(prefix, "_pivot_tabs"), options = sortable::sortable_options(onUpdate = htmlwidgets::JS(paste0("function(evt) { Shiny.setInputValue('", id, "-study_pivot_order', evt.from.innerText);}"))))
        )
        
      })
      
      ##########################################
      # Render modules elements                #
      ##########################################
      
      output$study_cards <- renderUI({
        
        req(isolate(r[[paste0(prefix, "_display_modules")]]))
        r[[paste0(prefix, "_load_ui_cards")]]
        
        r[[paste0(prefix, "_cards")]] <- character()
        r[[paste0(prefix, "_cards_to_open")]] <- character()
        
        distinct_modules <- isolate(r[[paste0(prefix, "_display_modules")]]) %>% dplyr::pull(id)
        
        code_ui <- tagList("")
        
        all_groups <- NA_integer_
        
        # Loop over distinct modules, for this study
        
        selected_module <- isolate(r[[paste0(prefix, "_selected_module")]])
        
        if (grepl("show_module", selected_module)) selected_module <- as.integer(substr(selected_module, nchar("show_module_") + 1, 100))
        
        sapply(distinct_modules, function(module_id){
          
          toggles <- tagList()
          
          module_elements <- isolate(r[[paste0(prefix, "_modules_elements")]]) %>% dplyr::filter(module_id == !!module_id) %>% dplyr::arrange(display_order)
          
          if (nrow(module_elements) > 0){
            
            # Get module element group_id
            distinct_groups <- unique(module_elements$group_id)
            
            # Loop over distinct cards, for this module
            
            sapply(distinct_groups, function(group_id){
              
              if (module_id != isolate(r[[paste0(prefix, "_first_module_shown")]])) all_groups <- c(all_groups, group_id)
              
              plugin_id <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              if (length(plugin_id) != 0){
                code_ui_card <- isolate(r$code) %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
              }
              
              # Check if plugin has been deleted
              check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id)) %>% dplyr::pull(deleted)
              if (check_deleted_plugin){
                code_ui_card <- paste0("div(shiny.fluent::MessageBar('", translate(language, "plugin_deleted", r$words), "', messageBarType = 3), style = 'margin-top:10px;')")
              }
              
              # Get name of module element
              
              module_element_name <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
              
              # Append a toggle to our cards list
              
              r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_group_", group_id))
              
              toggles <<- tagList(toggles,
                shiny.fluent::Toggle.shinyInput(ns(paste0(paste0(prefix, "_group_", group_id), "_toggle")), value = TRUE, style = "margin-top:10px;"),
                div(class = "toggle_title", module_element_name, style = "padding-top:12px;"))
              
              # Try to run plugin UI code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                code_ui_card <- code_ui_card %>%
                  stringr::str_replace_all("%module_id%", as.character(module_id)) %>%
                  stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("\r", "\n")
                
                if (length(r$chosen_study) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%study_id%", as.character(isolate(r$chosen_study)))
                
                # Module element card
                
                element_code <- shinyjs::hidden(
                  div(id = ns(paste0(prefix, "_group_", group_id)),
                    make_card("",
                      div(eval(parse(text = code_ui_card)),
                        # actionButton(ns(paste0(prefix, "_settings_module_element_", group_id)), "", icon = icon("cog"), style = "position:absolute; top:8px; right: 41px;"),
                        actionButton(ns(paste0(prefix, "_remove_module_element_", group_id)), "", icon = icon("trash-alt"), style = "position:absolute; top:8px; right: 10px;")
                      ),
                      style = "position:relative;"
                    )
                  )
                )
                
                if (!is.na(selected_module)) if (module_id == selected_module) r[[paste0(prefix, "_cards_to_open")]] <- 
                  c(isolate(r[[paste0(prefix, "_cards_to_open")]]), paste0(prefix, "_group_", group_id))
                
                code_ui <<- tagList(code_ui, element_code)
              },
              error = function(e){
                report_bug(r = r, output = output, error_message = translate(language, "error_run_plugin_ui_code", words),
                  error_name = paste0(id, " - run ui code - ", group_id), category = "Error", error_report = e, language = language)
              })
            })
          }
          
          # Put all div together
          
          r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_toggles_", module_id))
          
          toggles_div <- shinyjs::hidden(
            div(id = ns(paste0(prefix, "_toggles_", module_id)),
              make_card("",
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_", module_id)), translate(language, "new_module_element", isolate(r$words)), iconProps = list(iconName = "Add")),
                  shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_", module_id)), translate(language, "remove_module", isolate(r$words)), iconProps = list(iconName = "Delete")),
                  div(style = "width:20px;"),
                  toggles
                )
              )
            )
          )
          
          if (!is.na(selected_module)) if (module_id == selected_module) r[[paste0(prefix, "_cards_to_open")]] <- 
            c(isolate(r[[paste0(prefix, "_cards_to_open")]]), paste0(prefix, "_toggles_", module_id))
          
          code_ui <<- tagList(toggles_div, code_ui)
          
        })
        
        r[[paste0(prefix, "_hide_all_cards")]] <- Sys.time()
        
        # Final result
        tagList(code_ui, br())
      })
      
      ############################################
      # Show / hide div when pivot item selected #
      ############################################
      
      observeEvent(r[[paste0(prefix, "_selected_module")]], {
        
        req(!grepl("show_module", r[[paste0(prefix, "_selected_module")]]))
        
        # Hide opened cards
        # sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
        
        # Hide all cards
        sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
        
        # Hide Add module element card & Add module card
        sapply(c(paste0(prefix, "_add_module"), paste0(prefix, "_add_module_element")), shinyjs::hide)
        
        shinyjs::show(paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_module")]]))
        
        # Add to the list of open cards and reset the list
        r[[paste0(prefix, "_opened_cards")]] <- paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_module")]])
        
        module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == r[[paste0(prefix, "_selected_module")]])
        distinct_groups <- unique(module_elements$group_id)
        
        sapply(distinct_groups, function(group_id){
          
          # If toggle is ON
          if (length(input[[paste0(paste0(prefix, "_group_", group_id), "_toggle")]]) > 0){
            
            if (input[[paste0(paste0(prefix, "_group_", group_id), "_toggle")]]){
            
              # Show card
              shinyjs::show(paste0(prefix, "_group_", group_id))
            
              # Add to the list of open cards
              r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
            }
          }
          else {
            shinyjs::show(paste0(prefix, "_group_", group_id))
            r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_group_", group_id))
          }
          
        })
        
      })
    
      ############################################
      # Sortable / change PivotItems order       #
      ############################################
    
      observeEvent(input$study_pivot_order, {
        
        new_pivot_order <- tibble::tibble(name = stringr::str_split(input$study_pivot_order, "\n") %>% unlist()) %>%
          dplyr::mutate(display_order = 1:dplyr::n())
        
        table <- paste0(prefix, "_modules")
        sql <- glue::glue_sql("SELECT module_family_id FROM {`table`} WHERE id = {r[[paste0(prefix, '_selected_module')]]}", .con = r$db)
        module_family_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        
        if (is.na(module_family_id)) sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE module_family_id IS NULL AND deleted IS FALSE", .con = r$db)
        else sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE module_family_id = {module_family_id} AND deleted IS FALSE", .con = r$db)
        
        all_modules <- DBI::dbGetQuery(r$db, sql) %>%
          dplyr::select(-display_order) %>%
          dplyr::inner_join(new_pivot_order, by = "name") %>%
          dplyr::relocate(display_order, .after = "parent_module_id")
        
        sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id IN ({all_modules %>% dplyr::pull(id)*})", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        DBI::dbAppendTable(r$db, table, all_modules)
        update_r(r = r, table = table)
        
        r[[paste0(prefix, "_load_ui")]] <- paste0("change_pivot_order_", Sys.time())
      })
    
    
    ##########################################
    # LOAD SERVER                            #
    ##########################################
    
    observeEvent(r[[paste0(prefix, "_load_server")]], {
      
      req(!is.na(r$chosen_study))
      
      # Get modules elements, arrange them by display_order
      
      module_family <- r$studies %>% dplyr::filter(id == r$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
      modules <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(module_family_id == module_family) %>% dplyr::select(module_id = id)
      module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::inner_join(modules, by = "module_id")
      
      ##########################################
      # Delete module & create module element  #
      ##########################################
      
      # Loop over modules
      
      sapply(modules$module_id, function(module_id){
        
        ##########################################
        # Create a new module element            #
        ##########################################
        
        observeEvent(input[[paste0(prefix, "_add_module_element_", module_id)]], {
          
          # Hide opened cards
          sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
          
          # Show Add module element div
          shinyjs::show(paste0(prefix, "_add_module_element"))
          
        })
        
        ##########################################
        # Delete a module                        #
        ##########################################
        
        observeEvent(input[[paste0(prefix, "_remove_module_", module_id)]], {
          r[[module_delete_variable]] <- TRUE
        })
        
      })
      
      ##########################################
      # Run server code for cards              #
      ##########################################
      
      # If no thesaurus elements to show in this module, notify the user
      # if (nrow(module_elements) == 0) show_message_bar(output = output, id = 2, message = "no_module_element_to_show", type = "severeWarning", language = language)
      
      if (nrow(module_elements) > 0){
        
        # Get module element group_id
        distinct_groups <- unique(module_elements$group_id)
        
        toggles <- c()
        
        # Loop over distinct cards
        sapply(distinct_groups, function(group_id){
          
          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(prefix, "_", group_id, "_", r$chosen_study)
          if (trace_code %not_in% r$server_modules_groups_loaded){
            
            # Add the trace_code to loaded plugins list
            r$server_modules_groups_loaded <- c(r$server_modules_groups_loaded, trace_code)
            
            # Get name of module element
            # module_element_name_escaping <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% 
            # dplyr::pull(name) %>% stringr::str_replace_all(c("-" = "_", "/" = "_", "\\(" = "_", "\\)" = "_"))
            
            toggles <<- c(toggles, paste0(prefix, "_group_", group_id))
            
            if (prefix == "patient_lvl"){
              
              # Get thesaurus items with thesaurus own item_id
              thesaurus_selected_items <- module_elements %>% dplyr::filter(group_id == !!group_id) %>%
                dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                  thesaurus_item_unit, colour = thesaurus_item_colour)
            }
            
            # Get plugin code
            
            ids <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, module_id)
            
            # Check if plugin has been deleted
            check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", ids$plugin_id)) %>% dplyr::pull(deleted)
            if (!check_deleted_plugin){
                
              code_server_card <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
                dplyr::pull(code) %>%
                stringr::str_replace_all("%module_id%", as.character(ids$module_id)) %>%
                stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                stringr::str_replace_all("\r", "\n")
              
              # If it is an aggregated plugin, change %study_id% with current chosen study
              if (length(r$chosen_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(r$chosen_study))
            }
            else code_server_card <- ""
              
            tryCatch(eval(parse(text = code_server_card)),
              error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
                error_name = paste0(id, " - run server code - ", group_id), category = "Error", error_report = e, language = language)
            )
            
            ##########################################
            # Delete a module element                #
            ##########################################
            
            observeEvent(input[[paste0(prefix, "_remove_module_element_", group_id)]], {
              
              r[[paste0(prefix, "_selected_module_element")]] <- group_id
              
              r[[module_element_delete_variable]] <- TRUE
            })
            
          }
        })
        
        
        ##########################################
        # Server code for toggles                #
        ##########################################
        
        if (length(toggles) > 0){
          # Load toggles server code  
          # shinyjs::delay(10000, {
            sapply(toggles, function(toggle){
              observeEvent(input[[paste0(toggle, "_toggle")]], {
                if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
                else shinyjs::hide(toggle)
              }, ignoreInit = TRUE)
            })
          # })
            r[[paste0(prefix, "_hide_all_cards")]] <- Sys.time()
        }
      }
      
    })
    
    observeEvent(r[[paste0(prefix, "_hide_all_cards")]], {
      shinyjs::delay(100, {
        sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
        if (length(r[[paste0(prefix, "_cards")]]) > 0) r[[paste0(prefix, "reopen_cards")]] <- Sys.time()
      })
    })
    
    observeEvent(r[[paste0(prefix, "reopen_cards")]], shinyjs::delay(1000, {
      sapply(r[[paste0(prefix, "_cards_to_open")]], shinyjs::show)
    }))
  })
}