#' settings_data_management UI Function
#'
#' @description Shiny module of settings / data management
#'
#' @param id ID of the module (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  result <- div()
  
  # Delete in a future version
  language <- "EN"
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datamarts", "data_source",
    "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
    "settings_subsets", c("datamart", "study"),
    "settings_thesaurus", "data_source")
  
  cards <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "sub_datatable_card")
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = language, words = words))
  })
  
  # --- --- --- --- --- --
  # Data sources card ----
  # --- --- --- --- --- --
  
  if (id == "settings_data_sources"){
      div(class = "main",
        render_settings_default_elements(ns = ns),
        shiny.fluent::Breadcrumb(items = list(
          list(key = "data_sources", text = i18n$t("Data sources"))
        ), maxDisplayedItems = 3),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "creation_card", itemKey = "creation_card", headerText = i18n$t("Create a data source")),
          shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("Data sources management")),
          shiny.fluent::PivotItem(id = "create_items_card", itemKey = "create_items_card", headerText = i18n$t("Create items"))
        ),
        forbidden_cards,
        render_settings_creation_card(
        language = language, ns = ns, id = id, title = "create_data_source",
        textfields = "name", textfields_width = "300px", words = words),
        render_settings_datatable_card(language = language, ns = ns,
        div_id = "datatable_card", output_id = "management_datatable", title = "data_sources_management", words = words)
      ) -> result
    }
  
  # --- --- --- --- ---
  # Datamarts card ----
  # --- --- --- --- ---
  
  if (id == "settings_datamarts"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "datamarts", text = i18n$t("Datamarts"))
      ), maxDisplayedItems = 3),
      shiny.fluent::Pivot(
        id = ns("datamarts_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "creation_card", itemKey = "creation_card", headerText = i18n$t("Create a datamart")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("Datamarts management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("Edit datamart code")),
        shiny.fluent::PivotItem(id = "options_card", itemKey = "options_card", headerText = i18n$t("Datamart options"))
      ),
      forbidden_cards,
      div(id = ns("creation_card"),
        make_card(
          i18n$t("Create a datamart"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield(language = language, ns = ns, label = "name", width = "300px", words = words),
              make_dropdown(language = language, ns = ns, label = "data_source", multiSelect = FALSE, width = "300px", words = words),
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add"))
          )
        )
      ),
      div(id = ns("edit_code_card"), 
        make_card(i18n$t("Edit datamart code"),
          div(
            make_combobox(language = language, ns = ns, label = "datamart", id = "code_chosen",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(shinyAce::aceEditor(ns("ace_edit_code"), "", mode = "r", 
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("Run code")), br(), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      ),
      div(id = ns("options_card"),
        make_card(i18n$t("Datamart options"),
          div(
            make_combobox(language = language, ns = ns, label = "datamart", id = "options_chosen",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(), br(),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(language = language, ns = ns, label = "show_only_aggregated_data", inline = TRUE, words = words)
            ), br(),
            div(
              div(class = "input_title", paste0(i18n$t("Grant access to"), " :")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                list(key = "everybody", text = i18n$t("Everybody")),
                list(key = "people_picker", text = i18n$t("Choose users"))
              ), className = "inline_choicegroup"),
              conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                uiOutput(ns("users_allowed_read_div"))
              )
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save"))
          )
        ), br()
      ),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card", 
        output_id = "management_datatable", title = "datamarts_management", words = words)
    ) -> result
  }
  
  # --- --- --- --- ---
  # Thesaurus card ----
  # --- --- --- --- ---
  
  if (id == "settings_thesaurus"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "thesaurus", text = i18n$t("Thesaurus"))
      ), maxDisplayedItems = 3),
      shiny.fluent::Pivot(
        id = ns("thesaurus_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "creation_card", itemKey = "creation_card", headerText = i18n$t("Create a thesaurus")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("Thesaurus management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("Edit thesaurus code")),
        shiny.fluent::PivotItem(id = "sub_datatable_card", itemKey = "sub_datatable_card", headerText = i18n$t("All items")),
        shiny.fluent::PivotItem(id = "categories_card", itemKey = "categories_card", headerText = i18n$t("Categories")),
        shiny.fluent::PivotItem(id = "conversions_card", itemKey = "conversions_card", headerText = i18n$t("Conversions")),
        shiny.fluent::PivotItem(id = "mapping_card", itemKey = "mapping_card", headerText = i18n$t("Mapping")),
      ),
      forbidden_cards,
      div(id = ns("creation_card"),
        make_card(
          i18n$t("Create a thesaurus"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield_new(i18n = i18n, ns = ns, label = "name", width = "300px"),
              make_dropdown_new(i18n = i18n, ns = ns, label = "data_source", multiSelect = TRUE, width = "300px"),
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add"))
          )
        )
      ),
      div(id = ns("sub_datatable_card"),
        make_card(i18n$t("All items"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 50),
              make_combobox(language = language, ns = ns, label = "thesaurus", id = "items_chosen",
                width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE),
              make_dropdown(language = language, ns = ns, label = "datamart", id = "thesaurus_datamart", width = "300px", words = words),
              conditionalPanel(condition = "input.datamart != ''", ns = ns,
                div(strong(i18n$t("Show only items used in this datamart"), style = "display:block; padding-bottom:12px;"),
                  shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;"))
            ),
            DT::DTOutput(ns("sub_datatable")), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("sub_datatable_save"), i18n$t("save"))
          )
        )
      ),
      div(id = ns("edit_code_card"), 
        make_card(i18n$t("Edit thesaurus code"),
          div(
            make_combobox(language = language, ns = ns, label = "thesaurus", id = "code_chosen",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(shinyAce::aceEditor(ns("ace_edit_code"), "", mode = "r", 
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("Run code")), br(), br(),
            div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      ),
      render_settings_datatable_card(language = language, ns = ns, div_id = "datatable_card",
        output_id = "management_datatable", title = "thesaurus_management", words = words)
    ) -> result
  }
  result
}
    
#' settings_data_management Server Functions
#'
#' @param id ID of the module (character)
#' @param r Shiny reactive value
#' @param language Language used (character)
#' @noRd 

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # ** To be deleted ** ----
    language <- "EN"

    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "settings_data_sources", "",
      "settings_datamarts", "data_source",
      "settings_studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
      "settings_subsets", c("datamart", "study"),
      "settings_thesaurus", "data_source")
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    
    r[[paste0(table, "_datatable_loaded")]] <- FALSE
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    if (table %in% c("datamarts", "thesaurus")){
      observeEvent(r[[table]], {
        
        options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        
        if (table == "datamarts") shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options)
        if (table == "thesaurus") shiny.fluent::updateComboBox.shinyInput(session, "items_chosen", options = options)
        shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options)
      })
    }
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    # Toggles IDs
    cards <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "sub_datatable_card", "create_items_card")
    sapply(cards, shinyjs::hide)
    
    show_hide_cards(r = r, input = input, session = session, table = table, id = id, cards = cards)
    
    # Show first card
    if (paste0(table, "_creation_card") %in% r$user_accesses) shinyjs::show("creation_card")
    else shinyjs::show("creation_card_forbidden")
    
    # --- --- --- --- --- --
    # Add a new element ----
    # --- --- --- --- --- --
    
    # Update dropdowns with reactive data
    
    uploaded_dropdowns <- dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist()
    
    sapply(uploaded_dropdowns, 
      function(data_var){
        
        # Create only needed observers for current page
        if (data_var != ""){
          data_var <- get_plural(data_var)
          observeEvent(r[[data_var]], {
            
            # Convert options to list
            if (table == "subsets" & data_var == "studies") options <- list()
            else options <- convert_tibble_to_list(data = r[[data_var]] %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        }
      })
    
    # Particular case for subsets, update study dropdown with current datamart
    if (table == "subsets"){
      observeEvent(input$datamart, {
        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
        options <- convert_tibble_to_list(data = studies %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words)
        shiny.fluent::updateDropdown.shinyInput(session, "study", options = options)
      })
    }
    
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
      
      # Convert data_source to string, for page thesaurus
      if (id == "settings_thesaurus"){
        if (length(new_data$data_source) == 1) new_data$data_source <- coalesce2(type = "char", x = input$data_source)
        else new_data$data_source <- toString(as.integer(new_data$data_source))
      }
      
      add_settings_new_data_new(session = session, output = output, r = r, i18n = i18n, id = id, 
        data = new_data,
        table = substr(id, nchar("settings_") + 1, nchar(id)), 
        required_textfields = "name", req_unique_values = "name",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
    })
    
    # --- --- --- --- --- ---
    # Generate datatable ----
    # --- --- --- --- --- ---
  
    table <- paste0(substr(id, nchar("settings_") + 1, nchar(id)))
    
    # observeEvent(r[[table]], {
      
    dropdowns_datatable <- switch(id,
      "settings_data_sources" = "",
      "settings_datamarts" = "",
      # "settings_datamarts" = c("data_source_id" = "data_sources"),
      # "settings_studies" = c("datamart_id" = "datamarts", "patient_lvl_module_family_id" = "patient_lvl_modules_families", "aggregated_module_family_id" = "aggregated_modules_families"),
      "settings_studies" = c("patient_lvl_module_family_id" = "patient_lvl_modules_families", "aggregated_module_family_id" = "aggregated_modules_families"),
      "settings_subsets" = "",
      # "settings_subsets" = c("study_id" = "studies"),
      "settings_thesaurus" = c("data_source_id" = "data_sources"))
    
    # Dropdowns with multiSelect
    dropdowns_multiselect <- ""
    if (table == "thesaurus") dropdowns_multiselect <- "data_source_id"
    
    # Action buttons for each module / page
    if (paste0(table, "_delete_data") %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
    action_buttons = switch(table,
      "data_sources" = action_buttons,
      "datamarts" = c(action_buttons, "edit_code", "options"),
      "studies" = c(action_buttons, "options"),
      "subsets" = c(action_buttons, "edit_code"),
      "thesaurus" = c(action_buttons, "edit_code", "sub_datatable")
    )
    
    # Editable cols
    if (id != "settings_subsets") editable_cols <- c("name", "description")
    if (id == "settings_subsets") editable_cols <- "description"
    
    # Sortable cols
    if (id == "settings_thesaurus") sortable_cols <- c("id", "name", "description", "creator_id", "datetime")
    if (id != "settings_thesaurus") sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    
    # Column widths
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
    
    # Centered columns
    centered_cols <- c("id", "creator", "datetime", "action")
    
    # Searchable_cols
    if (id != "settings_thesaurus") searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    if (id == "settings_thesaurus") searchable_cols <- c("name", "description", "creator_id")
    
    # Factorize_cols
    factorize_cols <- switch(id,
      "settings_data_sources" = "creator_id",
      "settings_datamarts" = c("data_source_id", "creator_id"),
      "settings_studies" = c("datamart_id", "creator_id"),
      "settings_subsets" = c("study_id", "creator_id"),
      "settings_thesaurus" = "creator_id")
    
    hidden_cols <- c("id", "description", "deleted", "modified")
    
    # Reload datatable
    
    observeEvent(r[[table]], {
      if (nrow(r[[table]]) == 0){
        r[[paste0(table, "_temp")]] <- tibble::tibble()
        r[[paste0(table, "_datatable_loaded")]] <- FALSE 
      }
      else r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(r[[paste0(table, "_temp")]], {
      
      if (nrow(r[[paste0(table, "_temp")]]) == 0) render_datatable(output = output, r = r, ns = ns, language = language, data = tibble::tibble(),
        output_name = "management_datatable", col_names =  get_col_names(table_name = table, language = language, words = r$words),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      req(nrow(r[[paste0(table, "_temp")]]) > 0)
      
      # Prepare data for datatable (add code for dropdowns etc)
      r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = table, dropdowns = dropdowns_datatable, dropdowns_multiselect = dropdowns_multiselect, factorize_cols = factorize_cols,
        action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]], words = r$words)
    
      if (!r[[paste0(table, "_datatable_loaded")]]){
        
        # Render datatable
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0(table, "_datatable_temp")]],
          output_name = "management_datatable", col_names =  get_col_names_new(table_name = table, i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
        
        r[[paste0(table, "_datatable_loaded")]] <- TRUE
      }
      
      else {
        
        # Reload data of datatable
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
      }
      
    })
    
    # --- --- --- --- --- --- --- --
    # Save updates in datatable ----
    # --- --- --- --- --- --- --- --
  
      # Hide save button if user has no access
      observeEvent(r$user_accesses, if (paste0(table, "_edit_data") %not_in% r$user_accesses) shinyjs::hide("management_save"))
  
      # Each time a row is updated, modify temp variable
      # Do that for main datatable (management_datatable) & sub_datatable
      observeEvent(input$management_datatable_cell_edit, {
        edit_info <- input$management_datatable_cell_edit
        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })
  
      observeEvent(input$sub_datatable_cell_edit, {
        edit_info <- input$sub_datatable_cell_edit
        # edit_info$col <- edit_info$col + 2 # We have removed id & thesaurus_id cols, so need to add two to col index
        r$sub_thesaurus_items_temp <- DT::editData(r$sub_thesaurus_items_temp, edit_info, rownames = FALSE)
        r$sub_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
      })
    
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r[[table]], {
        update_settings_datatable(input = input, r = r, ns = ns, table = table, 
          dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), language = language)
      })
    
      # When save button is clicked
      # Do that for main datatable (management_datatable) & sub_datatable
      observeEvent(input$management_save, {
        duplicates_allowed <- FALSE
        if (table == "subsets") duplicates_allowed <- TRUE
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, language = language, duplicates_allowed = duplicates_allowed)
      })
      observeEvent(input$sub_datatable_save, {
        req(input$items_chosen)
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items", 
          r_table = "sub_thesaurus_items", duplicates_allowed = TRUE, language = language)
      })
      
      # --- --- --- --- --- --- --- --
      # Delete a row in datatable ----
      # --- --- --- --- --- --- --- --
  
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
        req(paste0(table, "_datatable_card") %in% r$user_accesses)
        
        # Get value of deleted row
        row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, nchar(input$deleted_pressed)))

        # Delete row in DB table
        delete_settings_datatable_row(output = output, r = r, ns = ns, language = language, row_deleted = row_deleted, table = table)
      })
      
      # The same for thesaurus_items / sub_datatable
      if (table == "thesaurus"){
        observeEvent(r$thesaurus_items_delete_dialog , {
          output$delete_confirm <- shiny.fluent::renderReact(render_settings_delete_react(r = r, ns = ns, table = "thesaurus_items", language = language))
        })
        
        # Whether to close or not delete dialog box
        observeEvent(input$thesaurus_items_hide_dialog, r$thesaurus_items_delete_dialog <- FALSE)
        observeEvent(input$thesaurus_items_delete_canceled, r$thesaurus_items_delete_dialog <- FALSE)
        observeEvent(input$thesaurus_items_deleted_pressed, r$thesaurus_items_delete_dialog <- TRUE)
        
        # When the delete is confirmed...
        observeEvent(input$thesaurus_items_delete_confirmed, {
          
          # Get value of deleted row
          row_deleted <- as.integer(substr(input$thesaurus_items_deleted_pressed, nchar("sub_delete_") + 1, nchar(input$thesaurus_items_deleted_pressed)))
          
          # Delete row in DB table
          # Link_id is ID of thesaurus which sub_datatable depends on
          # category is used to create the cache
          link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
        
          delete_settings_datatable_row(output = output, id = id, r = r, ns = ns, language = language,
            link_id = link_id, category = "delete", row_deleted = row_deleted, table = "thesaurus_items")
        })
      }
      
      # --- --- --- --- --- --- --- --- -- -
      # Edit options by selecting a row ----
      # --- --- --- --- --- --- --- --- -- -
        
      if (table == "datamarts"){
          
        observeEvent(input$options, {
          
          # Get link_id variable, to update options div
          link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
          options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
          
          # Set current pivot to options_card
          shinyjs::runjs(glue::glue("$('#{id}-datamarts_pivot button[name=\"{i18n$t('Datamart')}\"]').click();"))
        })
        
        observeEvent(input$options_chosen, {
          
          if (length(input$options_chosen) > 1) link_id <- input$options_chosen$key
          else link_id <- input$options_chosen
          if (length(input$code_chosen) > 0){
            if (length(input$code_chosen) > 1) code_link_id <- input$code_chosen$key
            else code_link_id <- input$code_chosen
          }
          else code_link_id <- 0L
          
          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options, value = value)
          }
          
          category <- get_singular(word = id)
          
          options <- r$options %>% dplyr::filter(category == "datamart", link_id == !!link_id)
          
          picker_options <-
            r$users %>%
            dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
            dplyr::transmute(
              key = id, 
              imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
              text = paste0(firstname, " ", lastname), 
              secondaryText = user_status)
          
          value <-
            picker_options %>%
            dplyr::mutate(n = 1:dplyr::n()) %>%
            dplyr::inner_join(
              options %>%
                dplyr::filter(name == "user_allowed_read") %>%
                dplyr::select(key = value_num),
              by = "key"
            ) %>%
            dplyr::pull(key)
          
          # Users allowed read group
          value_group <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value)
          
          selected_items <- picker_options %>% dplyr::filter(key %in% value)
          
          shiny.fluent::updateToggle.shinyInput(session, "show_only_aggregated_data",
            value = options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num) %>% as.logical)
          shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
            value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
          output$users_allowed_read_div <- renderUI({
            make_people_picker(
              language = language, ns = ns, id = "users_allowed_read", label = "blank", options = picker_options, value = value,
              width = "100%", style = "padding-bottom:10px;", words = words)
          })
          
        })
        
        observeEvent(input$options_save, {
          
          req(input$options_chosen)
          
          if (length(input$options_chosen) > 1) link_id <- input$options_chosen$key
          else link_id <- input$options_chosen
          
          category <- get_singular(id)
          
          data <- list()
          data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
          data$users_allowed_read <- input$users_allowed_read
          data$users_allowed_read_group <- input$users_allowed_read_group
  
          save_settings_options(output = output, r = r, id = id, category = category, code_id_input = paste0("options_", link_id), 
            language = language, data = data, page_options = c("show_only_aggregated_data", "users_allowed_read"))
        })
      }
      
      # --- --- --- --- --- --- --- -- --
      # Edit code by selecting a row ----
      # --- --- --- --- --- --- --- -- --
      
      if (table %in% c("datamarts", "thesaurus")){
        
        # Button "Edit code" is clicked on the datatable
        observeEvent(input$edit_code, {

          # Get link_id variable, to update code editor
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options, value = value)
          if (table == "datamarts") shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
          if (table == "thesaurus") shiny.fluent::updateComboBox.shinyInput(session, "items_chosen", options = options, value = value)
          
          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(table, '_pivot')} button[name=\"{i18n$t(paste0('edit_', get_singular(table), '_code'))}\"]').click();"))
          
        })
        
        observeEvent(input$code_chosen, {

          if (length(input$code_chosen) > 1) link_id <- input$code_chosen$key
          else link_id <- input$code_chosen
          
          if (table == "datamarts"){
            if (length(input$options_chosen) > 0){
              if (length(input$options_chosen) > 1) options_link_id <- input$options_chosen$key
              else options_link_id <- input$options_chosen
            }
            else options_link_id <- 0L

            if (link_id != options_link_id){
              options <- convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$datamarts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
            }
          }
          
          if (table == "thesaurus"){
            if (length(input$items_chosen) > 0){
              if (length(input$items_chosen) > 1) items_link_id <- input$items_chosen$key
              else items_link_id <- input$items_chosen
            }
            else items_link_id <- 0L
            
            if (link_id != items_link_id){
              options <- convert_tibble_to_list(r$thesaurus %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "items_chosen", options = options, value = value)
            }
          }

          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$datamart_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$thesaurus_id <- NA_integer_

          if (id == "settings_datamarts") r$datamart_id <- link_id
          if (id == "settings_thesaurus") r$thesaurus_id <- link_id

          category <- get_singular(id, language)

          # Get code from database
          code <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code)
          shinyAce::updateAceEditor(session, "ace_edit_code", value = code)

          # Reset code_result textOutput
          output$code_result <- renderText("")
        })
        
        # When save button is clicked
        observeEvent(input$edit_code_save, {
          
          req(input$code_chosen)
          
          if (length(input$code_chosen) > 1) link_id <- input$code_chosen$key
          else link_id <- input$code_chosen
          
          save_settings_code(output = output, r = r, id = id, category = get_singular(id, language),
            code_id_input = paste0("edit_code_", link_id), edited_code = input$ace_edit_code, language = language)
        })
        
        # When Execute code button is clicked
          observeEvent(input$execute_code, {
            edited_code <- isolate(input$ace_edit_code) %>% stringr::str_replace_all("\r", "\n")
            
            output$code_result <- renderText(
              execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
                language = language, r = r, edited_code = edited_code))
          })
      }
      
      # --- --- --- --- --- --- --- --- --- --- --- --
      # Generate sub datatable with action button ----
      # --- --- --- --- --- --- --- --- --- --- --- --
        
      if (table == "thesaurus"){
          
        # Sub datatable is a datatable in thesaurus page, when we click on the subdatatable button of a thesaurus row
        # It opens a toggle with a datatable containing items of chosen thesaurus
        
        observeEvent(input$sub_datatable, {
          
          # Get link id
          link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
          
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "items_chosen", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options, value = value)
          
          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-thesaurus_pivot button[name=\"{i18n$t('thesaurus_items_management_card')}\"]').click();"))
        })
        
        observeEvent(input$items_chosen, {
          
          if (length(input$items_chosen) > 1) link_id <- input$items_chosen$key
          else link_id <- input$items_chosen
          if (length(input$code_chosen) > 0){
            if (length(input$code_chosen) > 1) code_link_id <- input$code_chosen$key
            else code_link_id <- input$code_chosen
          }
          else code_link_id <- 0L

          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$thesaurus %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_chosen", options = options, value = value)
          }
          
          # Create a r var to export value to other observers
          r$thesaurus_link_id <- link_id
          
          # Get datamarts linked to this thesaurus
          data_sources <- stringr::str_split(r$thesaurus %>% dplyr::filter(id == link_id) %>% dplyr::pull(data_source_id), ", ") %>% unlist() %>% as.integer()
          datamarts <- r$datamarts %>% dplyr::filter(data_source_id %in% data_sources)
          
          options <- convert_tibble_to_list(datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          shiny.fluent::updateComboBox.shinyInput(session, "thesaurus_datamart", options = options)
          
          # Set r$thesaurus_refresh_thesaurus_items to "all_items", cause we havn't chosen yet the thesaurus or the datamart
          r$thesaurus_refresh_thesaurus_items <- paste0(link_id, "all_items")
        })
          
        observeEvent(input$show_only_used_items, {
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- "only_used_items"
          else r$thesaurus_refresh_thesaurus_items <- "all_items"
        })
        
        # When value of datamart changes, change value or r$thesaurus_refresh_thesaurus_items, depending on show_only_used_items
        # Add input$thesaurus_datamart in the value, to refresh even if the value doesn't change 
        # (if I change datamart and keep "all_items"), it won't active observer cause value hasn't changed...
        
        observeEvent(input$thesaurus_datamart, {
          if (input$show_only_used_items) r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "only_used_items")
          else r$thesaurus_refresh_thesaurus_items <- r$thesaurus_refresh_thesaurus_items <- paste0(input$thesaurus_datamart, "all_items")
        })
        
        observeEvent(r$thesaurus_refresh_thesaurus_items, {
          
          req(r$thesaurus_link_id)
          
          # Get all items from the chosen thesaurus
          
          r$sub_thesaurus_items <- create_datatable_cache(output = output, r = r, language = language, module_id = id, thesaurus_id = r$thesaurus_link_id, category = "delete")
          
          if (length(input$thesaurus_datamart) > 0){
            if (input$thesaurus_datamart != ""){
              
              count_items_rows <- tibble::tibble()
              count_patients_rows <- tibble::tibble()
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_items_rows"),
                error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
                  error_name = paste0(id, " - count_items_rows"), category = "Error", error_report = toString(e), language = language))
              
              # Add count_items_rows in the cache & get it if already in the cache
              tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = language, thesaurus_id = r$thesaurus_link_id,
                datamart_id = as.integer(input$thesaurus_datamart), category = "count_patients_rows"),
                error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
                  error_name = paste0(id, " - count_patients_rows"), category = "Error", error_report = toString(e), language = language))
              
              if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language)
              req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
              
              # Transform count_rows cols to integer, to be sortable
              r$sub_thesaurus_items <- r$sub_thesaurus_items %>%
                dplyr::left_join(count_items_rows, by = "item_id") %>%
                dplyr::left_join(count_patients_rows, by = "item_id") %>%
                dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
                dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
              
              # If r$thesaurus_refresh_thesaurus_items is set to "only_used_items", filter on count_items_rows > 0
              if (grepl("only_used_items", r$thesaurus_refresh_thesaurus_items)) r$sub_thesaurus_items <- r$sub_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
              
              # r$sub_thesaurus_items_temp <- r$sub_thesaurus_items %>% dplyr::mutate(modified = FALSE)
            }
          }
          
          r$sub_thesaurus_items_temp <- r$sub_thesaurus_items %>% 
            dplyr::mutate(modified = FALSE) %>%
            dplyr::mutate_at("category", as.factor) %>%
            dplyr::mutate_at("item_id", as.character)
          
          if ("thesaurus_delete_data" %in% r$user_accesses) action_buttons <- "delete" else action_buttons <- ""
          
          editable_cols <- c("display_name", "unit")
          searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
          factorize_cols <- c("category", "unit")
          column_widths <- c("id" = "80px", "action" = "80px")# "name" = "300px", "display_name" = "300px", "unit" = "100px", 
          # "category" = "300px", "colour" = "100px")
          
          if ("count_patients_rows" %in% names(r$sub_thesaurus_items)){
            sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
            centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
            col_names <- get_col_names(table_name = "thesaurus_items_with_counts", language = language, words = r$words)
          }
          else {
            sortable_cols <- c("id", "item_id", "name", "display_name", "category")
            centered_cols <- c("id", "item_id", "unit", "datetime", "action")
            col_names <- get_col_names(table_name = "thesaurus_items", language = language, words = r$words)
          }
          
          hidden_cols <- c("id", "thesaurus_id", "item_id", "datetime", "deleted", "modified")
          
          # Render datatable
          render_datatable(output = output, r = r, ns = ns, language = language, data = r$sub_thesaurus_items_temp,
            output_name = "sub_datatable", col_names =  col_names,
            editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
            searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
          
          # Create a proxy for datatatable
          r$sub_thesaurus_datatable_proxy <- DT::dataTableProxy("sub_datatable", deferUntilFlush = FALSE)
          
          # Reload datatable
          observeEvent(r$sub_thesaurus_items_temp, {
            
            # Reload data of datatable
            DT::replaceData(r$sub_thesaurus_datatable_proxy, r$sub_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
          })
          
        })
      }
  })
}
