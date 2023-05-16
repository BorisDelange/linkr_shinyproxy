#' scripts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scripts_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("scripts_descriptions_card", "dataset_scripts_card", "scripts_datatable_card", 
    "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
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
          shiny.fluent::PivotItem(id = "dataset_scripts_card", itemKey = "dataset_scripts_card", headerText = i18n$t("choose_dataset_scripts")),
          shiny.fluent::PivotItem(id = "scripts_descriptions_card", itemKey = "scripts_descriptions_card", headerText = i18n$t("scripts_descriptions_card")),
          shiny.fluent::PivotItem(id = "scripts_datatable_card", itemKey = "scripts_datatable_card", headerText = i18n$t("scripts_management")),
          shiny.fluent::PivotItem(id = "scripts_edit_code_card", itemKey = "scripts_edit_code_card", headerText = i18n$t("edit_script_code")),
          shiny.fluent::PivotItem(id = "scripts_options_card", itemKey = "scripts_options_card", headerText = i18n$t("script_options"))
        )
      )
    ),
    
    div(
      id = ns("choose_a_dataset_card"),
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
            make_combobox(i18n = i18n, ns = ns, label = "script", id = "scripts_description_selected_script",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(id = ns("scripts_description_markdown_output"),
              uiOutput(ns("scripts_description_markdown_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Dataset scripts card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("dataset_scripts_card"),
        div(
          class = glue::glue("card ms-depth-8 ms-sm{12} ms-xl{12}"),
          shiny.fluent::Text(variant = "large", i18n$t("choose_dataset_scripts"), block = TRUE),
          div(uiOutput(ns("dataset_scripts_bucket_list"))),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_dataset_scripts"), i18n$t("save"))
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
            make_combobox(i18n = i18n, ns = ns, label = "script", id = "code_selected_script",
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
              shiny.fluent::DefaultButton.shinyInput(ns("execute_code"), i18n$t("run_code"))
            ), br(),
            div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
            div(id = ns("console_output"), verbatimTextOutput(ns("console_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
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
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_combobox(i18n = i18n, ns = ns, label = "script", id = "options_selected_script",
                width = "320px", allowFreeform = FALSE, multiSelect = FALSE),
              make_textfield(i18n = i18n, ns = ns, label = "author", id = "script_author", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "version", id = "script_version", width = "60px")
            ), 
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name_fr", id = "script_name_fr", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "name_en", id = "script_name_en", width = "320px")
            ),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "category_fr", id = "script_category_fr", width = "320px"),
              make_textfield(i18n = i18n, ns = ns, label = "category_en", id = "script_category_en", width = "320px")
            ), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(paste0(i18n$t("description"), " :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("script_description_language"), value = "fr", options = list(
                list(key = "fr", text = "FR"),
                list(key = "en", text = "EN")
              ), className = "inline_choicegroup")
            ),
            conditionalPanel(condition = "input.script_description_language == 'fr'", ns = ns,
              div(shinyAce::aceEditor(ns("script_description_fr"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.script_description_language == 'en'", ns = ns,
              div(shinyAce::aceEditor(ns("script_description_en"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 21),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_options_description"), i18n$t("save")), " ",
              shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("run_code"))
            ),
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
  language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_scripts - start"))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("scripts_descriptions_card", "dataset_scripts_card", "scripts_datatable_card",
      "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - show_message_bars"))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_scripts_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_scripts_open_panel <- FALSE)
    
    r$help_scripts_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_scripts_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_scripts_open_modal <- FALSE
      r$help_scripts_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r$help_scripts_open_panel <- FALSE
      r$help_scripts_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_scripts_page_", i)]] <- Sys.time())
    })
    
    help_scripts(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts 1"))
      
      options <- convert_tibble_to_list(r$scripts%>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_selected_script", options = options)
      
    })
    
    # --- --- --- --- -
    # Reset fields ----
    # --- --- --- --- -
    
    reset_scripts_fields <- function(session){
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - function reset_scripts_fields"))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_selected_script", value = NULL)
      
      output$scripts_description_markdown_result <- renderUI("")
      shinyAce::updateAceEditor(session, "ace_edit_code", value = "")
      output$console_result <- renderText("")
      output$datetime_code_execution <- renderText("")
      blank_data <- data <- tibble::tribble(~id)
      names(blank_data) <- c("")
      output$table_result <- DT::renderDT(blank_data, options = list(dom = "<'datatable_length'l><'top't><'bottom'p>"), 
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE)
      shinyAce::updateAceEditor(session, "ace_options_description", value = "")
      output$description_markdown_result <- renderUI("")
      output$scripts_cache_infos <- renderUI("")
    }
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$selected_dataset"))
      
      # Reset fields
      reset_scripts_fields(session = session)
      
      # activate_scripts_cache option
      value <- r$options %>% 
        dplyr::filter(category == "dataset", name == "activate_scripts_cache", link_id == r$selected_dataset) %>% dplyr::pull(value_num)
      shiny.fluent::updateToggle.shinyInput(session, "activate_scripts_cache", value = as.logical(value))
      
      # Create empty var for r$scripts, if there's an error loading the dataset
      # r$scripts <- tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = integer(), creator_id = integer(),
      #   datetime = character(), deleted = logical())
      
      # Load scripts for this dataset
      update_r(r = r, table = "scripts")
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("dataset_scripts_card" %in% r$user_accesses) shinyjs::show("dataset_scripts_card")
        else shinyjs::show("dataset_scripts_card_forbidden")
      }
      # Show list of scripts for this dataset
      
      output$dataset_scripts_bucket_list <- renderUI({
        
        all_scripts <- r$scripts
        selected_scripts <- r$scripts %>% dplyr::inner_join(
          r$options %>%
          dplyr::filter(category == "dataset_scripts", link_id == r$selected_dataset) %>%
          dplyr::select(id = value_num),
          by = "id"
        )
          
        available_scripts <- all_scripts %>% dplyr::anti_join(selected_scripts, by = "id")
        
        result <- sortable::bucket_list(
          header = NULL,
          group_name = ns("all_dataset_scripts"),
          orientation = "horizontal",
          sortable::add_rank_list(text = i18n$t("selected_scripts"), labels = selected_scripts$name, input_id = ns("dataset_selected_scripts")),
          sortable::add_rank_list(text = i18n$t("available_scripts"), labels = available_scripts$name, input_id = ns("dataset_available_scripts"))
        )
        
        result
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$selected_dataset"))
    })
    
    # --- --- --- --- --- -
    # Dataset scripts ----
    # --- --- --- --- --- -
    
    # Save dataset scripts
    
    observeEvent(input$save_dataset_scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_dataset_scripts"))
      
      # Delete rows in options table concerning the scripts for this dataset
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'dataset_scripts' AND link_id = {r$selected_dataset}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::filter(category != "dataset_scripts" | (category == "dataset_scripts" & link_id != r$selected_dataset))
      
      # Add in options table informations concerning the scripts for this dataset
      
      if(length(input$dataset_selected_scripts) > 0){
        
        data_insert <- tibble::tibble(category = character(), link_id = integer(), name = character(), value = character(),
          value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical())
        
        sapply(input$dataset_selected_scripts, function(script){
          data_insert <<- data_insert %>%
            dplyr::bind_rows(
              tibble::tibble(category = "dataset_scripts", link_id = r$selected_dataset, name = "", value = "",
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
      
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
        
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input_save_dataset_scripts"))
    })
    
    # Save cache settings
    
    observeEvent(input$save_cache_settings, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_cache_settings"))
      
      sql <- glue::glue_sql(paste0("UPDATE options SET value_num = {as.integer(input$activate_scripts_cache)} ",
        "WHERE category = 'dataset' AND name = 'activate_scripts_cache' AND link_id = {r$selected_dataset} AND deleted IS FALSE"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value_num = dplyr::case_when(
        category == "dataset" & name == "activate_scripts_cache" & link_id == r$selected_dataset & !deleted ~ as.numeric(input$activate_scripts_cache),
        TRUE ~ value_num
      ))
      
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_cache_settings"))
    })
    
    # Update scripts_cache_infos UI
    
    observeEvent(r$update_scripts_cache_card, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$update_scripts_cache_card"))
      
      loaded_scripts_file_path <- paste0(r$app_folder, "/datasets/", r$selected_dataset, "/loaded_scripts.csv")
      if (file.exists(loaded_scripts_file_path)) dataset_loaded_scripts <- readr::read_csv(loaded_scripts_file_path, show_col_types = FALSE)
      if (!file.exists(loaded_scripts_file_path)) dataset_loaded_scripts <- tibble::tibble()
      
      if (nrow(dataset_loaded_scripts) > 0){
        
        datetime <- dataset_loaded_scripts %>% dplyr::slice(1) %>% dplyr::pull(datetime) %>% format_datetime(language, sec = FALSE)
        
        dataset_loaded_scripts <- dataset_loaded_scripts %>%
          dplyr::left_join(r$scripts %>% dplyr::select(id, name), by = "id") %>%
          dplyr::mutate(name = dplyr::case_when(is.na(name) ~ i18n$t("deleted_script"), TRUE ~ name))

        loaded_scripts <- list()
        loaded_scripts$success <- dataset_loaded_scripts %>% dplyr::filter(status == "success")
        loaded_scripts$failure <- dataset_loaded_scripts %>% dplyr::filter(status == "failure")

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
    
    observeEvent(input$scripts_description_selected_script, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$scripts_description_selected_script"))
      
      if (length(input$scripts_description_selected_script) > 1) link_id <- input$scripts_description_selected_script$key
      else link_id <- input$scripts_description_selected_script
      
      # Get description from database
      script_description <- r$options %>% dplyr::filter(category == "script" & name == "markdown_description" & link_id == !!link_id) %>% dplyr::pull(value) %>%
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$scripts_description_selected_script"))
    })
    
    # --- --- --- -- -- --
    # Create a script ----
    # --- --- --- -- -- --
    
    observeEvent(input$add_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$add_script"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$script_name)
      new_data$script_name <- new_data$name
      new_data$data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = "scripts",
        data = new_data, table = "scripts", required_textfields = "script_name", req_unique_values = "name")
      
    })
      
    # --- --- --- --- --- ---
    # Scripts management ----
    # --- --- --- --- --- ---
    
    action_buttons <- c("delete", "edit_code", "options")
    sortable_cols <- c("id", "name", "data_source_id", "creator_id", "creation_datetime", "update_datetime")
    column_widths <- c("id" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "creation_datetime", "update_datetime", "action")
    searchable_cols <- c("name", "creator_id", "data_source_id")
    factorize_cols <- c("creator_id")
    hidden_cols <- c("id", "description", "data_source_id", "deleted", "modified")
    col_names <- get_col_names("scripts", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts 2"))
      
      # Reset fields
      
      data_source_id <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)

      if(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) == 0){
        render_datatable(output = output, r = r, ns = ns, i18n = i18n,
          data = tibble::tibble(id = integer(), name = character(), data_source_id = integer(), creator_id = factor(),
            creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character()), 
          col_names = col_names, output_name = "scripts_datatable", selection = "multiple",
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      }

      req(nrow(r$scripts %>% dplyr::filter(data_source_id == !!data_source_id)) > 0)

      r$scripts_temp <- r$scripts %>% 
        dplyr::filter(data_source_id == !!data_source_id) %>% 
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE)

      # req(length(r$scripts_datatable_proxy) == 0)
      
      # Prepare data for datatable

      r$scripts_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$scripts_temp)

      # Render datatable

      if (length(r$scripts_datatable_proxy) == 0){
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$scripts_datatable_temp,
          output_name = "scripts_datatable", col_names = col_names, selection = "multiple",
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
  
        # Create a proxy for datatable
  
        r$scripts_datatable_proxy <- DT::dataTableProxy("scripts_datatable", deferUntilFlush = FALSE)
      }
        
      else DT::replaceData(r$scripts_datatable_proxy, r$scripts_datatable_temp, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$scripts"))
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
    script_id_var_r <- "delete_scripts"
    script_delete_message <- "script_deleted"
    script_reload_variable <- "reload_scripts"
    script_information_variable <- "script_deleted"
    script_delete_variable <- paste0(script_delete_prefix, "_open_dialog")

    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = script_delete_prefix, dialog_title = script_dialog_title, dialog_subtext = script_dialog_subtext,
      react_variable = script_react_variable, table = script_table, id_var_sql = script_id_var_sql, id_var_r = script_id_var_r,
      delete_message = script_delete_message, translation = TRUE, reload_variable = script_reload_variable,
      information_variable = script_information_variable)

    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$deleted_pressed"))
      
      r$delete_scripts <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[script_delete_variable]] <- TRUE
      reset_scripts_fields(session = session)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$delete_selection"))
      
      req(length(input$scripts_datatable_rows_selected) > 0)
      
      r$delete_scripts <- r$scripts_temp[input$scripts_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[script_delete_variable]] <- TRUE
      reset_scripts_fields(session = session)
    })
    
    # observeEvent(input$deleted_pressed, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$deleted_pressed"))
    # 
    #   r$delete_script <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
    #   r[[script_delete_variable]] <- TRUE
    # 
    #   reset_scripts_fields(session = session)
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$scripts_datatable_proxy, r$scripts_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })

    observeEvent(r$reload_scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$reload_scripts"))

      # Reload sidenav dropdown with reloading scripts
      # update_r(r = r, table = "scripts")

      # Reload datatable
      r$scripts_temp <- r$scripts %>% 
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$edit_code"))
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('edit_script_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('script_options')}\"]').click();"))
    })
    
    # --- --- --- --- --- -
    # Edit script code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_selected_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$code_selected_script"))
      
      if (length(input$code_selected_script) > 1) link_id <- input$code_selected_script$key
      else link_id <- input$code_selected_script
      if (length(input$options_selected_script) > 0){
        if (length(input$options_selected_script) > 1) options_link_id <- input$options_selected_script$key
        else options_link_id <- input$options_selected_script
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
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
      
      if (length(input$code_selected_script) > 1) link_id <- input$code_selected_script$key
      else link_id <- input$code_selected_script
      
      req(!is.null(link_id))
      
      # Update code
      
      code_id <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)

      ace_edit_code <- stringr::str_replace_all(input$ace_edit_code, "'", "''")
      sql <- glue::glue_sql("UPDATE code SET code = {ace_edit_code} WHERE id = {code_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$code <- r$code %>% dplyr::mutate(code = dplyr::case_when(id == code_id ~ ace_edit_code, TRUE ~ code))

      # Update datetime in plugins table

      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE scripts SET update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$scripts <- r$scripts %>% dplyr::mutate(update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      
      # Notify user
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
      
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
        data_source_id = r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id), i18n = i18n, ns = ns)
      
      edited_code <- r$script_code %>% stringr::str_replace_all("\r", "\n")
      
      # Variables to hide
      new_env_vars <- list("r" = NA)
      # Variables to keep
      for (var in c("d", "m", "r", "output", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = edited_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
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
    
    observeEvent(input$options_selected_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options_selected_script"))
      
      if (length(input$options_selected_script) > 1) link_id <- input$options_selected_script$key
      else link_id <- input$options_selected_script
      if (length(input$code_selected_script) > 0){
        if (length(input$code_selected_script) > 1) code_link_id <- input$code_selected_script$key
        else code_link_id <- input$code_selected_script
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id)  
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", 
          options = convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
          value = list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name)))
      
      # Get description from database
      # description <- r$options %>% dplyr::filter(category == "script" & name == "markdown_description" & link_id == !!link_id) %>% dplyr::pull(value)
      # shinyAce::updateAceEditor(session, "ace_options_description", value = description)
      
      options <- r$options %>% dplyr::filter(category == "script", link_id == !!link_id)
      
      for (field in c("version", "author", "name_fr", "name_en", "category_fr", "category_en")) shiny.fluent::updateTextField.shinyInput(session,
        paste0("script_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
      for (field in c("description_fr", "description_en")) shinyAce::updateAceEditor(session,
        paste0("script_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value))
      
    })
    
    # Save updates
    
    observeEvent(input$script_description_fr_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_fr_save"))
      r$script_save_options <- Sys.time()
    })
    observeEvent(input$script_description_en_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_en_save"))
      r$script_save_options <- Sys.time()
    })
    observeEvent(input$save_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_options_description"))
      r$script_save_options <- Sys.time()
    })
    
    observeEvent(r$script_save_options, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_options_description"))
      
      script_name <- input[[paste0("script_name_", language)]]
      
      if (is.na(script_name) | script_name == "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("script_name_", language), errorMessage = i18n$t("provide_valid_name"))
      
      req(!is.na(script_name) & script_name != "")
      
      if (!is.na(script_name) & script_name != "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("script_name_", language), errorMessage = NULL)
      
      req(length(input$options_selected_script) > 0)
      if (length(input$options_selected_script) > 1) link_id <- input$options_selected_script$key
      else link_id <- input$code_selected_script
      
      data <- list()
      for (field in c("script_version", "script_author",
        "script_name_fr", "script_name_en", "script_category_fr", "script_category_en",
        "script_description_fr", "script_description_en")) data[[stringr::str_replace(field, "script_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "script", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("version", "author", "description_fr", "description_en",
          "name_fr", "name_en", "category_fr", "category_en"))
      
      # Change script_name & update_datetime in scripts table
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE scripts SET name = {script_name}, update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$scripts <- r$scripts %>% dplyr::mutate(
        name = dplyr::case_when(id == link_id ~ script_name, TRUE ~ name),
        update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      r$scripts_temp <- r$scripts %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_options_description"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_options_description"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$script_description_fr_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_fr_run_all"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$script_description_en_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_en_run_all"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(r$script_options_description_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_options_description"))
      
      options_description <- isolate(input[[paste0("script_description_", input$script_description_language)]] %>% stringr::str_replace_all("\r", "\n"))
      
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
