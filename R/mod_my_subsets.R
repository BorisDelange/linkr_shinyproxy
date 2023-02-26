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
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study"), messageBarType = 5), style = "margin-top:10px;"))
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
mod_my_subsets_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class()){
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
    
    cards <- c("management_card", "edit_code_card", "creation_card", "datatable_card")
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
      
      r$reload_subsets_datatable <- Sys.time()
    })
    
    observeEvent(m$chosen_subset, {
      
      req(!is.na(m$chosen_subset))
      
      # Update subset options combobox
      # options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      # value <- list(key = m$chosen_study, text = r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(name))
      # shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
    })
    
    # --- --- --- --- --- ---
    # Studies management ----
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

        r$subsets_temp <- m$subsets %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)

        # Prepare data for datatable

        r$subsets_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "subsets", factorize_cols = subsets_management_factorize_cols, action_buttons = action_buttons, data_input = r$subsets_temp)
        data <- r$subsets_datatable_temp
      }

      if (length(r$subsets_datatable_proxy) == 0){

        # Render datatable

        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
          output_name = "subsets_datatable", col_names = get_col_names("subsets", i18n),
          editable_cols = subsets_management_editable_cols, sortable_cols = subsets_management_sortable_cols, centered_cols = subsets_management_centered_cols,
          column_widths = subsets_management_column_widths, searchable_cols = subsets_management_searchable_cols,
          filter = TRUE, factorize_cols = subsets_management_factorize_cols, hidden_cols = subsets_management_hidden_cols,
          selection = "multiple")

        # Create a proxy for datatable

        r$subsets_datatable_proxy <- DT::dataTableProxy("subsets_datatable", deferUntilFlush = FALSE)
      }

      if (length(r$subsets_datatable_proxy) > 0) DT::replaceData(r$subsets_datatable_proxy, data, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Reload datatable
    observeEvent(r$subsets_temp, {

      # Reload datatable_temp variable
      if (nrow(r$subsets_temp) == 0) r$subsets_datatable_temp <- tibble::tibble(id = integer(), name = character(), description = character(), study_id = factor(),
        creator_id = factor(), datetime = character(), deleted = integer(), modified = logical(), action = character())

      if (nrow(r$subsets_temp) > 0) r$subsets_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "subsets", factorize_cols = subsets_management_factorize_cols, action_buttons = action_buttons, data_input = r$subsets_temp)

      # Reload data of datatable
      if (length(r$subsets_datatable_proxy) > 0) DT::replaceData(r$subsets_datatable_proxy,
        r$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    # observeEvent(input$studies_datatable_cell_edit, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$studies_datatable_cell_edit"))
    #   
    #   edit_info <- input$studies_datatable_cell_edit
    #   r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
    #   
    #   # Store that this row has been modified
    #   r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    # })
    
    # Save updates
    # observeEvent(input$save_studies_management, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$save_studies_management"))
    #   
    #   req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)
    #   
    #   save_settings_datatable_updates(output = output, r = r, ns = ns, 
    #     table = "studies", r_table = "studies", i18n = i18n, duplicates_allowed = FALSE)
    #   
    #   # Update sidenav dropdown with the new study
    #   r$reload_studies <- Sys.time()
    # })
    
    # Delete a row in datatable
    
    # study_delete_prefix <- "study"
    # study_dialog_title <- "studies_delete"
    # study_dialog_subtext <- "studies_delete_subtext"
    # study_react_variable <- "study_delete_confirm"
    # study_table <- "studies"
    # study_id_var_sql <- "id"
    # study_id_var_r <- "delete_study"
    # study_delete_message <- "studies_deleted"
    # study_reload_variable <- "reload_studies"
    # study_information_variable <- "study_deleted"
    # study_delete_variable <- paste0(study_delete_prefix, "_open_dialog")
    # 
    # delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
    #   delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
    #   react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
    #   delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
    #   information_variable = study_information_variable)
    
    # Delete one row (with icon on DT)
    
    # observeEvent(input$deleted_pressed, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$deleted_pressed"))
    #   
    #   r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
    #   r[[study_delete_variable]] <- TRUE
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })
    
    # Delete multiple rows (with "Delete selection" button)
    
    # observeEvent(input$delete_selection, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$delete_selection"))
    #   
    #   req(length(input$studies_datatable_rows_selected) > 0)
    #   
    #   r$delete_study <- r$studies_temp[input$studies_datatable_rows_selected, ] %>% dplyr::pull(id)
    #   r[[study_delete_variable]] <- TRUE
    # })
    
    # observeEvent(r$reload_studies, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_studies"))
    #   
    #   # Reload sidenav dropdown with reloading studies
    #   update_r(r = r, table = "studies")
    #   r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
    #   
    #   # Reset chosen study
    #   m$chosen_study <- NA_integer_
    # })
    
  })
}
