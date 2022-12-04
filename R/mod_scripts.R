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
  
  cards <- c("scripts_descriptions_card", "datamart_scripts_card", "scripts_datatable_card", 
    "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card_new(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("script_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t("scripts"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("scripts_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datamart_scripts_card", itemKey = "datamart_scripts_card", headerText = i18n$t("choose_datamart_scripts")),
          shiny.fluent::PivotItem(id = "scripts_descriptions_card", itemKey = "scripts_descriptions_card", headerText = i18n$t("scripts_descriptions_card")),
          shiny.fluent::PivotItem(id = "scripts_creation_card", itemKey = "scripts_creation_card", headerText = i18n$t("create_script")),
          shiny.fluent::PivotItem(id = "scripts_datatable_card", itemKey = "scripts_datatable_card", headerText = i18n$t("scripts_management")),
          shiny.fluent::PivotItem(id = "scripts_edit_code_card", itemKey = "scripts_edit_code_card", headerText = i18n$t("edit_script_code")),
          shiny.fluent::PivotItem(id = "scripts_options_card", itemKey = "scripts_options_card", headerText = i18n$t("script_options"))
        )
      )
    ),
    
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;")),
      br()
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- --- --
    # Scripts descriptions card ----
    # --- --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        
        id = ns("scripts_descriptions_card"),
        make_card(i18n$t("scripts_descriptions_card"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "script", id = "scripts_description_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(id = ns("scripts_description_markdown_output"),
              uiOutput(ns("scripts_description_markdown_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;")
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Datamart scripts card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("datamart_scripts_card"),
        div(
          class = glue::glue("card ms-depth-8 ms-sm{12} ms-xl{12}"),
          shiny.fluent::Text(variant = "large", i18n$t("choose_datamart_scripts"), block = TRUE),
          div(uiOutput(ns("datamart_scripts_bucket_list"))),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_datamart_scripts"), i18n$t("save"))
        ), 
        make_card(i18n$t("scripts_cache_memory"),
          div(
            br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Scripts management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_datatable_card"),
        make_card(i18n$t("scripts_management"),
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
        make_card(i18n$t("create_script"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield_new(i18n = i18n, ns = ns, label = "name", id = "script_name", width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add_script"), i18n$t("add"))
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
        make_card(i18n$t("edit_script_code"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "script", id = "code_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_code_editor"), value = FALSE), style = "margin-top:9px;"),
              div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            ),
            shinyjs::hidden(div(id = ns("div_br"), br())),

            div(shinyAce::aceEditor(ns("ace_edit_code"), "", mode = "r",
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),

            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("save")), " ",
              shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
              div(i18n$t("output"), " : ", style = "margin:5px 0px 0px 30px; font-weight:bold;"),
              div(shiny.fluent::ChoiceGroup.shinyInput(ns("output_type"), value = "console", 
                options = list(
                  list(key = "console", text = i18n$t("console")),
                  list(key = "table", text = i18n$t("table"))
                ), 
              className = "inline_choicegroup"), 
              style = "margin:-3px 0px 0px 20px;")
            ),
            br(), br(),
            div(id = ns("console_output"), verbatimTextOutput(ns("console_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
            shinyjs::hidden(div(id = ns("table_output"), DT::DTOutput(ns("table_result")), style = "width: 99%; margin-right: 5px;"))
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
        make_card(i18n$t("script_options"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "script", id = "options_chosen_script",
              width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
            strong(i18n$t("script_description")), br(),
            
            div(shinyAce::aceEditor(ns("ace_options_description"), "", mode = "markdown",
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            
            shiny.fluent::PrimaryButton.shinyInput(ns("save_options_description"), i18n$t("save")), " ",
            shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("run_code")),
            br(), br(),
            div(id = ns("description_markdown_output"),
              uiOutput(ns("description_markdown_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;")
          )
        ), br()
      )
    ),
    
    # --- --- --- ---
    # To do list ----
    # --- --- --- ---
    
    div(shiny.fluent::MessageBar(
      div(
        strong("A faire"),
        tags$ul(
          tags$li("Afficher description des scripts depuis page 'Configurer le datamart', en cliquant sur une case"),
          tags$li("Créer un script depuis la page Gestion des scripts, en inline"),
          tags$li("Pouvoir supprimer les CSV des scripts d'un datamart"),
          tags$li("Modifier colonne datetime_start (enlever Z & T)"),
          tags$li("Make all columns sortable"),
          tags$li("Scripts in red if bug noticed / in green if OK / in grey if never runned since last update of code")
        )
      ),
      messageBarType = 0)
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
    
    cards <- c("scripts_descriptions_card", "datamart_scripts_card", "scripts_datatable_card",
      "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar_new(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n))
    observeEvent(r$show_message_bar2, show_message_bar_new(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n))
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$scripts, {
      
      options <- convert_tibble_to_list(r$scripts%>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_chosen_script", options = options)
      
    })
    
    # --- --- --- --- -
    # Reset fields ----
    # --- --- --- --- -
    
    reset_scripts_fields <- function(session){
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_chosen_script", value = NULL)
      
      output$scripts_description_markdown_result <- renderUI("")
      shinyAce::updateAceEditor(session, "ace_edit_code", value = "")
      output$console_result <- renderText("")
      blank_data <- data <- tibble::tribble(~id)
      names(blank_data) <- c("")
      output$table_result <- DT::renderDT(blank_data, options = list(dom = "<'datatable_length'l><'top't><'bottom'p>"), 
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE)
      shinyAce::updateAceEditor(session, "ace_options_description", value = "")
      output$description_markdown_result <- renderUI("")
    }
    
    # --- --- --- --- --- --- --- --
    # When a datamart is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      
      # Reset fields
      reset_scripts_fields(session = session)
      
      # Create empty var for r$scripts, if there's an error loading the datamart
      r$scripts <- tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = integer(), creator_id = integer(),
        datetime = character(), deleted = logical())
      
      update_r(r = r, table = "scripts")
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("datamart_scripts_card" %in% r$user_accesses) shinyjs::show("datamart_scripts_card")
        else shinyjs::show("datamart_scripts_card_forbidden")
      }
      # Show list of scripts for this datamart
      
      output$datamart_scripts_bucket_list <- renderUI({
        
        all_scripts <- r$scripts
        chosen_scripts <- r$scripts %>% dplyr::inner_join(
          r$options %>%
          dplyr::filter(category == "datamart_scripts", link_id == r$chosen_datamart) %>%
          dplyr::select(id = value_num),
          by = "id"
        )
          
        available_scripts <- all_scripts %>% dplyr::anti_join(chosen_scripts, by = "id")
        
        result <- sortable::bucket_list(
          header = NULL,
          group_name = ns("all_datamart_scripts"),
          orientation = "horizontal",
          sortable::add_rank_list(text = i18n$t("chosen_scripts"), labels = chosen_scripts$name, input_id = ns("datamart_chosen_scripts")),
          sortable::add_rank_list(text = i18n$t("available_scripts"), labels = available_scripts$name, input_id = ns("datamart_available_scripts"))
        )
        
        result
      })
      
    })
    
    # --- --- --- --- --- -
    # Datamart scripts ----
    # --- --- --- --- --- -
    
    observeEvent(input$save_datamart_scripts, {
      
      # Delete rows in options table concerning the scripts for this datamart
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'datamart_scripts' AND link_id = {r$chosen_datamart}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      # Add in options table informations concerning the scripts for this datamart
      
      if(length(input$datamart_chosen_scripts) > 0){
        
        data_insert <- tibble::tibble(category = character(), link_id = integer(), name = character(), value = character(),
          value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical())
        
        sapply(input$datamart_chosen_scripts, function(script){
          data_insert <<- data_insert %>%
            dplyr::bind_rows(
              tibble::tibble(category = "datamart_scripts", link_id = r$chosen_datamart, name = "", value = "",
                value_num = r$scripts %>% dplyr::filter(name == script) %>% dplyr::pull(id),
                creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE)
            )
        })
        
        data_insert$id <- seq.int(nrow(data_insert)) + get_last_row(r$db, "options")
        data_insert <- data_insert %>% dplyr::relocate(id)

        DBI::dbAppendTable(r$db, "options", data_insert)
      }
      
      update_r(r = r, table = "options")
      
      show_message_bar_new(output, 4, "modif_saved", "success", i18n)
        
    })
    
    # --- --- --- --- --- --- -
    # Scripts descriptions ----
    # --- --- --- --- --- --- -
    
    observeEvent(input$scripts_description_chosen_script, {
      
      if (length(input$scripts_description_chosen_script) > 1) link_id <- input$scripts_description_chosen_script$key
      else link_id <- input$scripts_description_chosen_script
      
      # Get description from database
      script_description <- r$options %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(value) %>%
        stringr::str_replace_all("\r", "\n")
      
      tryCatch({
        
        # Clear temp dir
        unlink(paste0(path.expand("~"), "/linkr_temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          path.expand("~"), "/linkr_temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/linkr_temp_files', fig.path = '", path.expand("~"), "/linkr_temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, script_description)
        
        # Create temp dir
        dir <- paste0(path.expand("~"), "/linkr_temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$scripts_description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
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
    action_buttons <- c("delete", "edit_code", "options")
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "data_source_id", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "datetime", "action")
    searchable_cols <- c("name", "description", "creator_id", "data_source_id", "creator_id")
    factorize_cols <- c("creator_id")
    hidden_cols <- c("id", "description", "data_source_id", "deleted", "modified")
    col_names <- get_col_names_new("scripts", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$scripts, {
      
      # Reset fields
      
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

    # Reload datatable
    observeEvent(r$scripts_temp, {
      
      # Reload datatable_temp variable
      if (nrow(r$scripts_temp) > 0) r$scripts_datatable_temp <- prepare_data_datatable_new(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$scripts_temp)

      # Reload data of datatable
      if (length(r$scripts_datatable_proxy) > 0) DT::replaceData(r$scripts_datatable_proxy,
        r$scripts_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })

    # Updates on datatable data
    observeEvent(input$scripts_datatable_cell_edit, {

      edit_info <- input$scripts_datatable_cell_edit
      r$scripts_temp <- DT::editData(r$scripts_temp, edit_info, rownames = FALSE)

      # Store that this row has been modified
      r$scripts_temp[[edit_info$row, "modified"]] <- TRUE
    })

    # Save updates
    observeEvent(input$save_scripts_management, {

      req(nrow(r$scripts) > 0)

      save_settings_datatable_updates_new(output = output, r = r, ns = ns, table = "scripts", i18n = i18n, duplicates_allowed = FALSE)

      # Update sidenav dropdown with the new study
      r$reload_scripts <- Sys.time()
    })

    # Delete a row in datatable

    script_delete_prefix <- "script"
    script_dialog_title <- "scripts_delete"
    script_dialog_subtext <- "scripts_delete_subtext"
    script_react_variable <- "script_delete_confirm"
    script_table <- "scripts"
    script_id_var_sql <- "id"
    script_id_var_r <- "delete_script"
    script_delete_message <- "script_deleted"
    script_reload_variable <- "reload_scripts"
    script_information_variable <- "script_deleted"
    script_delete_variable <- paste0(script_delete_prefix, "_open_dialog")

    delete_element_new(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = script_delete_prefix, dialog_title = script_dialog_title, dialog_subtext = script_dialog_subtext,
      react_variable = script_react_variable, table = script_table, id_var_sql = script_id_var_sql, id_var_r = script_id_var_r,
      delete_message = script_delete_message, translation = TRUE, reload_variable = script_reload_variable,
      information_variable = script_information_variable)

    observeEvent(input$deleted_pressed, {

      r$delete_script <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[script_delete_variable]] <- TRUE

      reset_scripts_fields(session = session)
    })

    observeEvent(r$reload_scripts, {

      # Reload sidenav dropdown with reloading scripts
      update_r(r = r, table = "scripts")

      # Reload datatable
      r$scripts_temp <- r$scripts %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(input$edit_code, {
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('edit_script_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('script_options')}\"]').click();"))
    })
    
    # --- --- --- --- --- -
    # Edit script code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_chosen_script, {
      
      if (length(input$code_chosen_script) > 1) link_id <- input$code_chosen_script$key
      else link_id <- input$code_chosen_script
      if (length(input$options_chosen_script) > 0){
        if (length(input$options_chosen_script) > 1) options_link_id <- input$options_chosen_script$key
        else options_link_id <- input$options_chosen_script
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options, value = value)
      }
      
      # Get code from database
      code <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(code)
      
      shinyAce::updateAceEditor(session, "ace_edit_code", value = code)
      
    })
    
    # Save updates
    
    observeEvent(input$save_code, {
      
      if (length(input$code_chosen_script) > 1) link_id <- input$code_chosen_script$key
      else link_id <- input$code_chosen_script
      
      req(!is.null(link_id))
      
      # Update code
      # DON'T USE glue_sql, it adds some quotes in the code
      
      code_id <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)

      DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(input$ace_edit_code, "'", "''"), "' WHERE id = ", code_id)) -> query
      DBI::dbClearResult(query)

      # Update datetime in plugins table

      sql <- glue::glue_sql("UPDATE scripts SET datetime = {as.character(Sys.time())} WHERE id = {link_id}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      update_r(r = r, table = "code")
      update_r(r = r, table = "scripts")
      
      # Notify user
      show_message_bar_new(output, 4, "modif_saved", "success", i18n)
      
    })
    
    # Execute code
    
    observeEvent(input$execute_code, {
      
      edited_code <- isolate(input$ace_edit_code %>% stringr::str_replace_all("\r", "\n"))
      
      if (input$output_type == "console"){
        shinyjs::show("console_output")
        shinyjs::hide("table_output")
        
        output$console_result <- renderText(
          execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, r = r,
            edited_code = edited_code, code_type = "server"))
      }
      
      if (input$output_type == "table"){
        shinyjs::show("table_output")
        shinyjs::hide("console_output")
        
        tryCatch({
          
          data <- eval(parse(text = as.character(isolate(input$ace_edit_code)) %>% stringr::str_replace_all("\r", "\n")))
          
          render_datatable_new(
            data = data,
            output = output,
            r = r,
            ns = ns,
            i18n = i18n,
            output_name = "table_result",
            filter = TRUE,
            searchable_cols = names(data),
            sortable_cols = names(data)
          )
        }, error = function(e) render_datatable_new(
          data = tibble::tibble(), output = output, r = r, ns = ns, i18n = i18n, output_name = "table_result", filter = FALSE
        ))
      }
    })
    
    # Hide ace editor
    
    observeEvent(input$hide_code_editor, {
      if (input$hide_code_editor){
        shinyjs::hide("ace_edit_code")
        shinyjs::show("div_br") 
      }
      else {
        shinyjs::show("ace_edit_code")
        shinyjs::hide("div_br") 
      }
    })
    
    # --- --- --- --- -- -- --
    # Edit script options ----
    # --- --- --- --- -- -- --
    
    observeEvent(input$options_chosen_script, {
      
      if (length(input$options_chosen_script) > 1) link_id <- input$options_chosen_script$key
      else link_id <- input$options_chosen_script
      if (length(input$code_chosen_script) > 0){
        if (length(input$code_chosen_script) > 1) code_link_id <- input$code_chosen_script$key
        else code_link_id <- input$code_chosen_script
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id){
        options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options, value = value)
      }
      
      # Get description from database
      description <- r$options %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(value)

      shinyAce::updateAceEditor(session, "ace_options_description", value = description)
    })
    
    # Save updates
    
    observeEvent(input$save_options_description, {
      
      if (length(input$options_chosen_script) > 1) link_id <- input$options_chosen_script$key
      else link_id <- input$options_chosen_script

      req(!is.null(link_id))

      # Update options
      # DON'T USE glue_sql, it adds some quotes in the code

      option_id <- r$options %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)

      DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", stringr::str_replace_all(input$ace_options_description, "'", "''"), "' WHERE id = ", option_id)) -> query
      DBI::dbClearResult(query)

      # Update datetime in plugins table

      sql <- glue::glue_sql("UPDATE scripts SET datetime = {as.character(Sys.time())} WHERE id = {link_id}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)

      update_r(r = r, table = "options")
      update_r(r = r, table = "scripts")

      # Notify user
      show_message_bar_new(output, 4, "modif_saved", "success", i18n)
      
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      
      options_description <- isolate(input$ace_options_description %>% stringr::str_replace_all("\r", "\n"))
      
      tryCatch({
        
        # Clear temp dir
        unlink(paste0(path.expand("~"), "/linkr_temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          path.expand("~"), "/linkr_temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/linkr_temp_files', fig.path = '", path.expand("~"), "/linkr_temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(path.expand("~"), "/linkr_temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
    })
    
  })
}
