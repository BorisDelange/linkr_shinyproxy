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
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
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
          # shiny.fluent::PivotItem(id = "scripts_creation_card", itemKey = "scripts_creation_card", headerText = i18n$t("create_script")),
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
            make_combobox(i18n = i18n, ns = ns, label = "script", id = "scripts_description_chosen_script",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
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
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, id = "activate_scripts_cache", label = "activate_scripts_cache", inline = TRUE)),
            conditionalPanel(condition = "input.activate_scripts_cache == true", ns = ns, uiOutput(ns("scripts_cache_infos"))), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
              shiny.fluent::PrimaryButton.shinyInput(ns("save_cache_settings"), i18n$t("save")),
              conditionalPanel(condition = "input.activate_scripts_cache == true", ns = ns, 
                shiny.fluent::DefaultButton.shinyInput(ns("reload_cache"), i18n$t("reload_cache")))
            )
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
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("scripts_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
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
            make_combobox(i18n = i18n, ns = ns, label = "script", id = "code_chosen_script",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(shiny.fluent::Toggle.shinyInput(ns("hide_code_editor"), value = FALSE), style = "margin-top:9px;"),
              div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
            ),
            shinyjs::hidden(div(id = ns("div_br"), br())),

            div(shinyAce::aceEditor(ns("ace_edit_code"), "", mode = "r",
              code_hotkeys = list(
                "r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                )
              ),
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),

            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_code"), i18n$t("save")), " ",
              shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code"))#,
              # div(i18n$t("output"), " : ", style = "margin:5px 0px 0px 30px; font-weight:bold;"),
              # div(shiny.fluent::ChoiceGroup.shinyInput(ns("output_type"), value = "console", 
              #   options = list(
              #     list(key = "console", text = i18n$t("console")),
              #     list(key = "table", text = i18n$t("table"))
              #   ), 
              # className = "inline_choicegroup"), 
              # style = "margin:-3px 0px 0px 20px;")
            ), br(),
            div(id = ns("console_output"), verbatimTextOutput(ns("console_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")#,
            # shinyjs::hidden(div(id = ns("table_output"), DT::DTOutput(ns("table_result")), style = "width: 99%; margin-right: 5px;"))
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
            make_combobox(i18n = i18n, ns = ns, label = "script", id = "options_chosen_script",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
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
    )
  ) -> result
  
  result
}
    
#' scripts Server Functions
#'
#' @noRd 
mod_scripts_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  language = "en", i18n = R6::R6Class(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_scripts - start"))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("scripts_descriptions_card", "datamart_scripts_card", "scripts_datatable_card",
      "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, 1, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - show_message_bars"))
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts"))
      
      options <- convert_tibble_to_list(r$scripts%>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_chosen_script", options = options)
      
    })
    
    # --- --- --- --- -
    # Reset fields ----
    # --- --- --- --- -
    
    reset_scripts_fields <- function(session){
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - function reset_scripts_fields"))
      
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
      output$scripts_cache_infos <- renderUI("")
    }
    
    # --- --- --- --- --- --- --- --
    # When a datamart is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$chosen_datamart"))
      
      # Reset fields
      reset_scripts_fields(session = session)
      
      # activate_scripts_cache option
      value <- r$options %>% 
        dplyr::filter(category == "datamart", name == "activate_scripts_cache", link_id == r$chosen_datamart) %>% dplyr::pull(value_num)
      shiny.fluent::updateToggle.shinyInput(session, "activate_scripts_cache", value = as.logical(value))
      
      # Create empty var for r$scripts, if there's an error loading the datamart
      # r$scripts <- tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = integer(), creator_id = integer(),
      #   datetime = character(), deleted = logical())
      
      # Load scripts for this datamart
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$chosen_datamart"))
    })
    
    # --- --- --- --- --- -
    # Datamart scripts ----
    # --- --- --- --- --- -
    
    # Save datamart scripts
    
    observeEvent(input$save_datamart_scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_datamart_scripts"))
      
      # Delete rows in options table concerning the scripts for this datamart
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'datamart_scripts' AND link_id = {r$chosen_datamart}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::filter(category != "datamart_scripts" | (category == "datamart_scripts" & link_id != r$chosen_datamart))
      
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
        r$options <- r$options %>% dplyr::bind_rows(data_insert)
      }
      
      # update_r(r = r, table = "options")
      
      show_message_bar(output, 4, "modif_saved", "success", i18n, ns = ns)
        
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input_save_datamart_scripts"))
    })
    
    # Save cache settings
    
    observeEvent(input$save_cache_settings, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_cache_settings"))
      
      sql <- glue::glue_sql(paste0("UPDATE options SET value_num = {as.integer(input$activate_scripts_cache)} ",
        "WHERE category = 'datamart' AND name = 'activate_scripts_cache' AND link_id = {r$chosen_datamart} AND deleted IS FALSE"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value_num = dplyr::case_when(
        category == "datamart" & name == "activate_scripts_cache" & link_id == r$chosen_datamart & !deleted ~ as.numeric(input$activate_scripts_cache),
        TRUE ~ value_num
      ))
      
      show_message_bar(output, 4, "modif_saved", "success", i18n, ns = ns)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_cache_settings"))
    })
    
    # Update scripts_cache_infos UI
    
    observeEvent(r$update_scripts_cache_card, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$update_scripts_cache_card"))
      
      loaded_scripts_file_path <- paste0(r$app_folder, "/datamarts/", r$chosen_datamart, "/loaded_scripts.csv")
      if (file.exists(loaded_scripts_file_path)) datamart_loaded_scripts <- readr::read_csv(loaded_scripts_file_path, show_col_types = FALSE)
      if (!file.exists(loaded_scripts_file_path)) datamart_loaded_scripts <- tibble::tibble()
      
      if (nrow(datamart_loaded_scripts) > 0){
        
        datetime <- datamart_loaded_scripts %>% dplyr::slice(1) %>% dplyr::pull(datetime)
        if (tolower(language) == "fr") datetime <- format(as.POSIXct(datetime), format = "%d-%m-%Y %H:%M")
        if (tolower(language) == "en") datetime <- format(as.POSIXct(datetime), format = "%Y-%m-%d %H:%M")
        
        datamart_loaded_scripts <- datamart_loaded_scripts %>%
          dplyr::left_join(r$scripts %>% dplyr::select(id, name), by = "id") %>%
          dplyr::mutate(name = dplyr::case_when(is.na(name) ~ i18n$t("deleted_script"), TRUE ~ name))

        loaded_scripts <- list()
        loaded_scripts$success <- datamart_loaded_scripts %>% dplyr::filter(status == "success")
        loaded_scripts$failure <- datamart_loaded_scripts %>% dplyr::filter(status == "failure")

        for (status in c("success", "failure")){
          if (nrow(loaded_scripts[[status]]) == 0) loaded_scripts[[status]] <- "/"
          else {
            my_list <- tagList()
            for (i in 1:nrow(loaded_scripts[[status]])){
              row <- loaded_scripts[[status]][i, ]
              my_list <- tagList(my_list, tags$li(row$name))
            }
            loaded_scripts[[status]] <- tagList(tags$ul(my_list))
          }
        }

        output$scripts_cache_infos <- renderUI({
          if (debug) print(paste0(Sys.time(), " - mod_scripts - output$scripts_cache_infos"))
          
          tagList(
            br(), div(
              strong(i18n$t("last_cache_load_datetime")), " : ", datetime, br(), br(),
              span(i18n$t("scripts_loaded_successfully"), style = "font-weight:bold; color:#0078D5;"), " : ", loaded_scripts$success,
              span(i18n$t("scripts_with_load_failure"), style = "font-weight:bold; color:#CB181D;"), " : ", loaded_scripts$failure,
              style = "border:solid 1px #DDDCDE; padding:8px; margin:0px 10px 0px 10px;"
            )
          )
        })
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$update_scripts_cache_card"))
    })
    
    # Reload script cache
    
    observeEvent(input$reload_cache, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$reload_cache"))
      
      r$load_scripts <- Sys.time()
      r$force_reload_scripts_cache <- TRUE
    })
    
    # --- --- --- --- --- --- -
    # Scripts descriptions ----
    # --- --- --- --- --- --- -
    
    observeEvent(input$scripts_description_chosen_script, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$scripts_description_chosen_script"))
      
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
          "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/linkr_temp_files/', fig.path = '", path.expand("~"), "/linkr_temp_files/')\n```\n")
        
        markdown_file <- paste0(markdown_settings, script_description)
        
        # Create temp dir
        dir <- paste0(path.expand("~"), "/linkr_temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$scripts_description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$scripts_description_chosen_script"))
    })
    
    # --- --- --- -- -- --
    # Create a script ----
    # --- --- --- -- -- --
    
    observeEvent(input$add_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$add_script"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$script_name)
      new_data$script_name <- new_data$name
      new_data$data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = "scripts",
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
    col_names <- get_col_names("scripts", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts"))
      
      # Reset fields
      
      data_source_id <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)

      if(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) == 0){
        render_datatable(output = output, r = r, ns = ns, i18n = i18n,
          data = tibble::tribble(~name, ~creator_id, ~datetime, ~action), output_name = "scripts_datatable")
      }

      req(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) > 0)

      r$scripts_temp <- r$scripts %>% dplyr::filter(data_source_id == !!data_source_id) %>% dplyr::mutate(modified = FALSE)

      # Prepare data for datatable

      r$scripts_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$scripts_temp)

      # Render datatable

      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$scripts_datatable_temp,
        output_name = "scripts_datatable", col_names =  get_col_names(table_name = "scripts", i18n = i18n),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)

      # Create a proxy for datatable

      r$scripts_datatable_proxy <- DT::dataTableProxy("scripts_datatable", deferUntilFlush = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$scripts"))
    })

    # Reload datatable
    observeEvent(r$scripts_temp, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts_temp"))
      
      # Reload datatable_temp variable
      if (nrow(r$scripts_temp) > 0) r$scripts_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$scripts_temp)

      # Reload data of datatable
      if (length(r$scripts_datatable_proxy) > 0) DT::replaceData(r$scripts_datatable_proxy,
        r$scripts_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })

    # Updates on datatable data
    observeEvent(input$scripts_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$scripts_datatable_cell_edit"))

      edit_info <- input$scripts_datatable_cell_edit
      r$scripts_temp <- DT::editData(r$scripts_temp, edit_info, rownames = FALSE)

      # Store that this row has been modified
      r$scripts_temp[[edit_info$row, "modified"]] <- TRUE
    })

    # Save updates
    observeEvent(input$save_scripts_management, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_scripts_management"))

      req(nrow(r$scripts) > 0)

      save_settings_datatable_updates(output = output, r = r, ns = ns, table = "scripts", i18n = i18n, duplicates_allowed = FALSE)

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

    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = script_delete_prefix, dialog_title = script_dialog_title, dialog_subtext = script_dialog_subtext,
      react_variable = script_react_variable, table = script_table, id_var_sql = script_id_var_sql, id_var_r = script_id_var_r,
      delete_message = script_delete_message, translation = TRUE, reload_variable = script_reload_variable,
      information_variable = script_information_variable)

    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$deleted_pressed"))

      r$delete_script <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[script_delete_variable]] <- TRUE

      reset_scripts_fields(session = session)
    })

    observeEvent(r$reload_scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$reload_scripts"))

      # Reload sidenav dropdown with reloading scripts
      # update_r(r = r, table = "scripts")

      # Reload datatable
      r$scripts_temp <- r$scripts %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$edit_code"))
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_chosen_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('edit_script_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options"))
      
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
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$code_chosen_script"))
      
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
    
    observeEvent(input$ace_edit_code_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_save"))
      r$script_save_code <- Sys.time()
    })
    observeEvent(input$save_code, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_code"))
      r$script_save_code <- Sys.time()
    })
    
    observeEvent(r$script_save_code, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$script_save_code"))
      
      if (length(input$code_chosen_script) > 1) link_id <- input$code_chosen_script$key
      else link_id <- input$code_chosen_script
      
      req(!is.null(link_id))
      
      # Update code
      # DON'T USE glue_sql, it adds some quotes in the code
      
      code_id <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)

      ace_edit_code <- stringr::str_replace_all(input$ace_edit_code, "'", "''")
      sql <- glue::glue_sql("UPDATE code SET code = {ace_edit_code} WHERE id = {code_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$code <- r$code %>% dplyr::mutate(code = dplyr::case_when(id == code_id ~ ace_edit_code, TRUE ~ code))

      # Update datetime in plugins table

      sql <- glue::glue_sql("UPDATE scripts SET datetime = {as.character(Sys.time())} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$scripts <- r$scripts %>% dplyr::mutate(datetime = dplyr::case_when(id == link_id ~ as.character(Sys.time()), TRUE ~ datetime))
      
      # update_r(r = r, table = "code")
      # update_r(r = r, table = "scripts")
      
      # Notify user
      show_message_bar(output, 4, "modif_saved", "success", i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$scripts_save_code"))
    })
    
    # Execute code
    
    observeEvent(input$execute_code, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_code"))
      r$script_code <- input$ace_edit_code
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_run_selection"))
      if(!shinyAce::is.empty(input$ace_edit_code_run_selection$selection)) r$script_code <- input$ace_edit_code_run_selection$selection
      else r$script_code <- input$ace_edit_code_run_selection$line
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_run_all"))
      r$script_code <- input$ace_edit_code
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(r$script_code_trigger, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$script_code_trigger"))
      
      # Create thesaurus for scripts if doesn't exist
      create_scripts_thesaurus(output = output, r = r, 
        data_source_id = r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id), i18n = i18n, ns = ns)
      
      edited_code <- r$script_code %>% stringr::str_replace_all("\r", "\n")
      
      # Variables to hide
      new_env_vars <- list()
      # Variables to keep
      for (var in c("d", "m", "r", "output", "i18n",)) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      # if (input$output_type == "console"){
      # shinyjs::show("console_output")
      # shinyjs::hide("table_output")
      
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = edited_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      output$console_result <- renderText(paste(paste(captured_output), collapse = "\n"))
    })
    
    # Hide ace editor
    
    observeEvent(input$hide_code_editor, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$hide_code_editor"))
      
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
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options_chosen_script"))
      
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_options_description"))
      
      if (length(input$options_chosen_script) > 1) link_id <- input$options_chosen_script$key
      else link_id <- input$options_chosen_script

      req(!is.null(link_id))

      # Update options
      # DON'T USE glue_sql, it adds some quotes in the code

      option_id <- r$options %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)

      new_description <- stringr::str_replace_all(input$ace_options_description, "'", "''")
      sql <- glue::glue_sql("UPDATE options SET value = {new_description} WHERE id = {option_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value = dplyr::case_when(id == option_id ~ new_description, TRUE ~ value))

      # Update datetime in plugins table

      new_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE scripts SET datetime = {new_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$scripts <- r$scripts %>% dplyr::mutate(datetime = dplyr::case_when(id == link_id ~ new_datetime, TRUE ~ datetime))

      # update_r(r = r, table = "options")
      # update_r(r = r, table = "scripts")

      # Notify user
      show_message_bar(output, 4, "modif_saved", "success", i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_options_description"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_options_description"))
      
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$execute_options_description"))
    })
    
  })
}
