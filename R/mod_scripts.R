#' scripts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scripts_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("datamart_scripts_card", "scripts_datatable_card", "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "scripts_thesaurus_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card_new(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    #shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t("scripts"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datamart_scripts_card", itemKey = "datamart_scripts_card", headerText = i18n$t("Choose datamart scripts")),
          shiny.fluent::PivotItem(id = "scripts_creation_card", itemKey = "scripts_creation_card", headerText = i18n$t("Create a script")),
          shiny.fluent::PivotItem(id = "scripts_datatable_card", itemKey = "scripts_datatable_card", headerText = i18n$t("Scripts management")),
          shiny.fluent::PivotItem(id = "scripts_edit_code_card", itemKey = "scripts_edit_code_card", headerText = i18n$t("Edit script code")),
          shiny.fluent::PivotItem(id = "scripts_options_card", itemKey = "scripts_options_card", headerText = i18n$t("Script options"))#,
          #shiny.fluent::PivotItem(id = "scripts_thesaurus_card", itemKey = "scripts_thesaurus_card", headerText = i18n$t("Thesaurus items"))
        )
      )
    ),
    
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("Choose a damatart in the dropdown on the left-side of the page"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- --
    # Datamart scripts card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("datamart_scripts_card"),
        div(
          class = glue::glue("card ms-depth-8 ms-sm{12} ms-xl{12}"),
          shiny.fluent::Text(variant = "large", i18n$t("Choose datamart scripts"), block = TRUE),
          div(uiOutput(ns("datamart_scripts_bucket_list"))),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_datamart_scripts"), i18n$t("save"))
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Scripts management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_datatable_card"),
        make_card(i18n$t("Scripts management"),
          div(
            DT::DTOutput(ns("scripts_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_management"), i18n$t("save"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- -
    # Create a script card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("scripts_creation_card"),
        make_card(i18n$t("Create a script"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield_new(i18n = i18n, ns = ns, label = "Name", id = "script_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_script"), i18n$t("Add"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Edit script code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_edit_code_card"),
        make_card(i18n$t("Edit script code"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "Script", id = "code_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE)
            
            # thesaurus_items_div, br(),
            # 
            # shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            #   shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
            #     list(key = "ui", text = i18n$t("UI")),
            #     list(key = "server", text = i18n$t("Server"))
            #   ), className = "inline_choicegroup"),
            #   div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
            #   div(i18n$t("Hide editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            # ),
            # shinyjs::hidden(div(id = ns("div_br"), br())),
            # 
            # conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
            #   div(shinyAce::aceEditor(ns("ace_edit_code_ui"), "", mode = "r", 
            #     autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            # conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
            #   div(shinyAce::aceEditor(ns("ace_edit_code_server"), "", mode = "r", 
            #     autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            # 
            # shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("save")), " ",
            # shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("Run code")), br(), br(),
            # shiny::uiOutput(ns("code_result_ui")), br(),
            # div(verbatimTextOutput(ns("code_result_server")), 
            #   style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    
    # --- --- --- --- -- -- --
    # Script options card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_options_card"),
        make_card(i18n$t("Script options"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "Script", id = "code_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            
            shiny.fluent::PrimaryButton.shinyInput(ns("save_script_options"), i18n$t("save"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- ---
    # Scripts thesaurus card ----
    # --- --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("scripts_thesaurus_card"),
        make_card("",
          div(
            
          )
        ), br()
      )
    )
  ) -> result
  
  result
}
    
#' scripts Server Functions
#'
#' @noRd 
mod_scripts_server <- function(id = character(), r = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("datamart_scripts_card", "scripts_datatable_card", "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "scripts_thesaurus_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- --- --- --
    # When a datamart is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      
      # Create empty var for r$scripts, if there's an error loading the datamart
      r$scripts <- tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = integer(), creator_id = integer(),
        datetime = character(), deleted = logical())
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("datamart_scripts_card" %in% r$user_accesses) shinyjs::show("datamart_scripts_card")
        else shinyjs::show("datamart_scripts_card_forbidden")
      }
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      # Reset r variables (prevent bug later if datamart code doesn't work)
      r$patients <- tibble::tibble()
      r$stays <- tibble::tibble()
      r$labs_vitals <- tibble::tibble()
      r$text <- tibble::tibble()
      r$orders <- tibble::tibble()
      
      # Try to load datamart 
      tryCatch({
        run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language, quiet = TRUE)
        
        # A r variable to update Study dropdown, when the load of datamart is finished
        r$loaded_datamart <- r$chosen_datamart
        
        show_message_bar(output, 1, "import_datamart_success", "success", language, r$words)
      },
        error = function(e) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language))
      
      # Show list of scripts for this datamart
      
      output$datamart_scripts_bucket_list <- renderUI({
        
        available_scripts <- r$scripts %>% dplyr::pull(name)
        chosen_scripts <- NULL
        
        result <- sortable::bucket_list(
          header = NULL,
          group_name = "bucket_list_group",
          orientation = "horizontal",
          sortable::add_rank_list(text = i18n$t("chosen_scripts"), labels = chosen_scripts, input_id = ns("datamart_chosen_scripts")),
          sortable::add_rank_list(text = i18n$t("available_scripts"), labels = available_scripts, input_id = ns("datamart_available_scripts"))
        )
        
        result
      })
      
    })
    
    # --- --- --- -- -- --
    # Create a script ----
    # --- --- --- -- -- --
    
    observeEvent(input$add_script, {
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$script_name)
      new_data$script_name <- new_data$name
      new_data$data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      add_settings_new_data_new(session = session, output = output, r = r, i18n = i18n, id = "scripts", 
        data = new_data, table = "scripts", required_textfields = "script_name", req_unique_values = "name")
      
    })
      
    # --- --- --- --- --- ---
    # Scripts management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each module / page
    action_buttons <- c("delete")
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "data_source_id", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "datetime", "action")
    searchable_cols <- c("name", "description", "data_source_id", "creator_id")
    factorize_cols <- c("creator_id")
    hidden_cols <- c("id", "description", "data_source_id", "deleted", "modified")
    col_names <- get_col_names_new("scripts", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$scripts, {
      
      data_source_id <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)

      if(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) == 0){
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n,
          data = tibble::tribble(~name, ~creator_id, ~datetime, ~action), output_name = "scripts_datatable")
      }

      req(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) > 0)

      r$scripts_temp <- r$scripts %>% dplyr::filter(data_source_id == !!data_source_id) %>% dplyr::mutate(modified = FALSE)

      # Prepare data for datatable

      r$scripts_datatable_temp <- prepare_data_datatable_new(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons,
        data_input = r$scripts_temp, words = r$words)

      # Render datatable

      render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$scripts_datatable_temp,
        output_name = "scripts_datatable", col_names =  get_col_names_new(table_name = "scripts", i18n = i18n),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)

      # Create a proxy for datatable

      r$scripts_datatable_proxy <- DT::dataTableProxy("scripts_datatable", deferUntilFlush = FALSE)
    })
    # 
    # # Reload datatable
    # observeEvent(r$studies_temp, {
    #   
    #   # Reload datatable_temp variable
    #   r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
    #     table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons, 
    #     data_input = r$studies_temp, words = r$words)
    #   
    #   # Reload data of datatable
    #   if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
    #     r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # # Updates on datatable data
    # observeEvent(input$studies_datatable_cell_edit, {
    #   
    #   edit_info <- input$studies_datatable_cell_edit
    #   r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
    #   
    #   # Store that this row has been modified
    #   r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    # })
    # 
    # # Save updates
    # observeEvent(input$save_studies_management, {
    #   
    #   req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)
    #   
    #   save_settings_datatable_updates(output = output, r = r, ns = ns, table = "studies", language = language, duplicates_allowed = FALSE)
    #   
    #   # Update sidenav dropdown with the new study
    #   r$reload_studies <- Sys.time()
    # })
    # 
    # # Delete a row in datatable
    # 
    # study_delete_prefix <- "study"
    # study_dialog_title <- "studies_delete"
    # study_dialog_subtext <- "studies_delete_subtext"
    # study_react_variable <- "study_delete_confirm"
    # study_table <- "studies"
    # study_id_var_sql <- "id"
    # study_id_var_r <- "delete_study"
    # study_delete_message <- "study_deleted"
    # study_reload_variable <- "reload_studies"
    # study_information_variable <- "study_deleted"
    # study_delete_variable <- paste0(study_delete_prefix, "_open_dialog")
    # 
    # delete_element(r = r, input = input, output = output, session = session, ns = ns, language = language,
    #   delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
    #   react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
    #   delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
    #   information_variable = study_information_variable)
    # 
    # observeEvent(input$deleted_pressed, {
    #   
    #   r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
    #   r[[study_delete_variable]] <- TRUE
    #   
    # })
    # 
    # observeEvent(r$reload_studies, {
    #   
    #   # Reload sidenav dropdown with reloading studies
    #   update_r(r = r, table = "studies")
    #   
    #   # Reload datatable
    #   r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)  %>% dplyr::mutate(modified = FALSE)
    #   
    #   # Reset chosen study
    #   r$chosen_study <- NA_integer_
    # })
    
  })
}