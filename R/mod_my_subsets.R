#' my_subsets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_subsets_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("datatable_card", "management_card", "edit_code_card")
  language <- "en"
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "subset_main", text = i18n$t("my_subsets"))
    ), maxDisplayedItems = 3),
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("subsets_management")),
          shiny.fluent::PivotItem(id = "management_card", itemKey = "management_card", headerText = i18n$t("subset_management")),
          shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_subset_code"))
        )
      )
    ),
    forbidden_cards,
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_study_and_datamart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    
    # --- --- --- --- --- -- --- -
    # Subsets management card ----
    # --- --- --- --- --- -- --- -
    
    shinyjs::hidden(
      div(
        id = ns("datatable_card"),
        make_card(i18n$t("subsets_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "subset_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_subset"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("subsets_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_subsets_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        )
      )
    ),
    
    # --- --- --- --- --- -- -- -
    # Subset management card ----
    # --- --- --- --- --- -- -- -
    
    shinyjs::hidden(
      div(
        id = ns("management_card"),
        make_card(i18n$t("subset_management"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("On choisit un subset."),
                p("On peut :",
                  tags$ul(
                    tags$li("Filter des patients sur des paramètres"),
                    tags$li("Ajouter des patients"),
                    tags$li("Supprimer des patients")
                  )
                )
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    
    # --- --- --- --- --- --- --
    # Subset edit code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("edit_code_card"),
        make_card(i18n$t("edit_subset_code"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Création de subsets avec du code, directement.")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ), br()
  )
}
    
#' my_subsets Server Functions
#'
#' @noRd 
mod_my_subsets_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = R6::R6Class(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    # if (id == "patient_level_data_subsets") prefix <- "patient_lvl"
    # if (id == "aggregated_data_subsets") prefix <- "aggregated"
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("management_card", "edit_code_card", "datatable_card")
    # show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    observeEvent(input$current_tab, {
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, 1, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- --
    # Render subsets UI ----
    # --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      shinyjs::show("choose_a_study_card")
      sapply(c("datatable_card", "management_card", "edit_code_card", "menu"), shinyjs::hide)
    })
    
    observeEvent(m$chosen_study, {
      req(!is.na(m$chosen_study))
      
      # Show first card & hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        shinyjs::show("datatable_card")
        # if ("subset_datatable_card" %in% r$user_accesses) shinyjs::show("subset_datatable_card")
        # else shinyjs::show("subset_datatable_card_forbidden")
      }
      else shinyjs::show(input$current_tab)
      
      r$reload_subsets_datatable <- Sys.time()
    })
    
    observeEvent(m$chosen_subset, {
      
      req(!is.na(m$chosen_subset))
      
      # Update subset options combobox
      # options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      # value <- list(key = m$chosen_study, text = r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(name))
      # shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
    })
    
    # --- --- --- --- -- -
    # Create a subset ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_subset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$add_subset"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$subset_name)
      new_data$subset_name <- new_data$name
      new_data$description <- ""
      new_data$study <- m$chosen_study
      
      add_settings_new_data(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "my_subsets", 
        data = new_data, table = "subsets", required_textfields = "subset_name", req_unique_values = "name")
      
      # Reload datatable
      m$subsets_temp <- m$subsets %>% dplyr::filter(study_id == m$chosen_study) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_subsets - observer input$add_subset"))
    })
    
    # --- --- --- --- --- ---
    # Subsets management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each module / page
    action_buttons <- c("options", "edit_code", "delete")
    
    subsets_management_editable_cols <- c("name")
    subsets_management_sortable_cols <- c("id", "name", "description", "study_id", "creator_id", "datetime")
    subsets_management_column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    subsets_management_centered_cols <- c("id", "creator", "datetime", "action")
    subsets_management_searchable_cols <- c("name", "description", "creator_id")
    subsets_management_factorize_cols <- c("study_id", "creator_id")
    subsets_management_hidden_cols <- c("id", "description", "study_id","deleted", "modified")
    subsets_management_col_names <- get_col_names("subsets", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$reload_subsets_datatable, {

      if (nrow(m$subsets) == 0) {

        data <- tibble::tibble(id = integer(), name = character(), description = character(), study_id = factor(),
          creator_id = factor(), datetime = character(), deleted = integer(), modified = logical(), action = character())
      }

      if (nrow(m$subsets) > 0){

        m$subsets_temp <- m$subsets %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)

        # Prepare data for datatable

        m$subsets_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "subsets", factorize_cols = subsets_management_factorize_cols, action_buttons = action_buttons, data_input = m$subsets_temp)
        data <- m$subsets_datatable_temp
      }

      if (length(m$subsets_datatable_proxy) == 0){

        # Render datatable

        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
          output_name = "subsets_datatable", col_names = get_col_names("subsets", i18n),
          editable_cols = subsets_management_editable_cols, sortable_cols = subsets_management_sortable_cols, centered_cols = subsets_management_centered_cols,
          column_widths = subsets_management_column_widths, searchable_cols = subsets_management_searchable_cols,
          filter = TRUE, factorize_cols = subsets_management_factorize_cols, hidden_cols = subsets_management_hidden_cols,
          selection = "multiple")

        # Create a proxy for datatable

        m$subsets_datatable_proxy <- DT::dataTableProxy("subsets_datatable", deferUntilFlush = FALSE)
      }

      if (length(m$subsets_datatable_proxy) > 0) DT::replaceData(m$subsets_datatable_proxy, data, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Reload datatable
    observeEvent(m$subsets_temp, {

      # Reload datatable_temp variable
      if (nrow(m$subsets_temp) == 0) m$subsets_datatable_temp <- tibble::tibble(id = integer(), name = character(), description = character(), study_id = factor(),
        creator_id = factor(), datetime = character(), deleted = integer(), modified = logical(), action = character())

      if (nrow(m$subsets_temp) > 0) m$subsets_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "subsets", factorize_cols = subsets_management_factorize_cols, action_buttons = action_buttons, data_input = m$subsets_temp)

      # Reload data of datatable
      if (length(m$subsets_datatable_proxy) > 0) DT::replaceData(m$subsets_datatable_proxy,
        m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$subsets_datatable_cell_edit, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subsets_datatable_cell_edit"))

      edit_info <- input$subsets_datatable_cell_edit
      m$subsets_temp <- DT::editData(m$subsets_temp, edit_info, rownames = FALSE)

      # Store that this row has been modified
      m$subsets_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_subsets_management, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$save_subsets_management"))

      req(nrow(m$subsets %>% dplyr::filter(study_id == m$chosen_study)) > 0)

      save_settings_datatable_updates(output = output, r = r, m = m, ns = ns,
        table = "subsets", r_table = "subsets", i18n = i18n, duplicates_allowed = FALSE)

      # Update sidenav dropdown with the new study
      r$reload_subsets <- Sys.time()
    })
    
    # Delete a row in datatable
    
    subset_delete_prefix <- "subset"
    subset_dialog_title <- "subsets_delete"
    subset_dialog_subtext <- "subsets_delete_subtext"
    subset_react_variable <- "delete_confirm"
    subset_table <- "subsets"
    subset_id_var_sql <- "id"
    subset_id_var_r <- "delete_subset"
    subset_delete_message <- "subsets_deleted"
    subset_reload_variable <- "reload_subsets"
    subset_information_variable <- "subset_deleted"
    subset_delete_variable <- paste0(subset_delete_prefix, "_open_dialog")

    delete_element(r = r, m = m, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = subset_delete_prefix, dialog_title = subset_dialog_title, dialog_subtext = subset_dialog_subtext,
      react_variable = subset_react_variable, table = subset_table, id_var_sql = subset_id_var_sql, id_var_r = subset_id_var_r,
      delete_message = subset_delete_message, translation = TRUE, reload_variable = subset_reload_variable,
      information_variable = subset_information_variable)

    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$deleted_pressed"))

      r$delete_subset <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[subset_delete_variable]] <- TRUE

      # Reload datatable (to unselect rows)
      DT::replaceData(m$subsets_datatable_proxy, m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$delete_selection"))

      req(length(input$subsets_datatable_rows_selected) > 0)

      r$delete_subset <- m$subsets_temp[input$subsets_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[subset_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_subsets, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer r$reload_subsets"))

      m$subsets_temp <- m$subsets %>% dplyr::filter(study_id == m$chosen_study) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)

      # Reset chosen subset
      m$chosen_subset <- NA_integer_
    })
    
  })
}
