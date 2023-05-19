#' my_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_studies_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("studies_messages_card", "studies_description_card", "studies_creation_card", "studies_datatable_card", "studies_options_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::reactOutput(ns("conversation_delete_confirm")),
    shiny.fluent::reactOutput(ns("message_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "dataset_main", text = i18n$t("my_studies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("studies_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "studies_messages_card", itemKey = "studies_messages_card", headerText = i18n$t("messages")),
          shiny.fluent::PivotItem(id = "studies_description_card", itemKey = "studies_description_card", headerText = i18n$t("study_description")),
          shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = i18n$t("studies_management")),
          shiny.fluent::PivotItem(id = "studies_options_card", itemKey = "studies_options_card", headerText = i18n$t("study_options"))
        )
      )
    ),
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- -- -- --
    # Study messages card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("studies_messages_card"),
        div(id = ns("study_messages_content"),
          make_card(i18n$t("messages"),
            div(
              shiny.fluent::Pivot(
                id = ns("study_messages_pivot"),
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-messages_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "all_messages", itemKey = "all_messages", headerText = i18n$t("all_messages")),
                shiny.fluent::PivotItem(id = "new_conversation", itemKey = "new_conversation", headerText = i18n$t("new_conversation"))
              ),
              conditionalPanel(condition = "input.messages_current_tab == null || input.messages_current_tab == 'all_messages'", ns = ns,
                DT::DTOutput(ns("study_conversations")),
                uiOutput(ns("conversation_object")), br(),
                shinyjs::hidden(
                  div(
                    id = ns("conversation_new_message_div"),
                    shiny.fluent::DefaultButton.shinyInput(ns("conversation_new_message"), i18n$t("new_message")),
                    shinyjs::hidden(
                      div(id = ns("conversation_hide_new_message_div"),
                        shiny.fluent::DefaultButton.shinyInput(ns("conversation_hide_new_message"), i18n$t("hide_editor")))
                    ),
                    shinyjs::hidden(
                      div(
                        id = ns("new_message_text_div"),
                        div(
                          shinyAce::aceEditor(
                            ns("new_message_text"), "", mode = "markdown",
                            code_hotkeys = list("markdown", list(run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"))),
                            autoScrollEditorIntoView = TRUE, minLines = 10, maxLines = 1000
                          ), style = "width: 100%;"),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                          shiny.fluent::PrimaryButton.shinyInput(ns("send_new_message"), i18n$t("send")), " ",
                          shiny.fluent::DefaultButton.shinyInput(ns("preview_new_message"), i18n$t("preview")),
                          shiny.fluent::Toggle.shinyInput(ns("new_message_as_rmarkdown"), value = FALSE, style = "margin-top:6px;"),
                          div(class = "toggle_title", i18n$t("rmarkdown"), style = "padding-top:6px;")
                        ),
                        uiOutput(ns("new_message_preview")),
                        style = "float:left; width:100%;"
                      )
                    ), br(), br()
                  )
                ),
                uiOutput(ns("selected_conversation"))
              ),
              conditionalPanel(condition = "input.messages_current_tab == 'new_conversation'", ns = ns,
                make_textfield(i18n = i18n, ns = ns, label = "object", id = "new_conversation_name"),
                div(shinyAce::aceEditor(
                  ns("new_conversation_text"), "", mode = "markdown", 
                  code_hotkeys = list("markdown", list(run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"))),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;"),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::PrimaryButton.shinyInput(ns("send_new_conversation"), i18n$t("send")), " ",
                  shiny.fluent::DefaultButton.shinyInput(ns("preview_new_conversation"), i18n$t("preview")),
                  shiny.fluent::Toggle.shinyInput(ns("new_conversation_as_rmarkdown"), value = FALSE, style = "margin-top:6px;"),
                  div(class = "toggle_title", i18n$t("rmarkdown"), style = "padding-top:6px;")
                ),
                uiOutput(ns("new_conversation_preview"))
              )
            )
          )
        ),
        div(
          id = ns("choose_a_study_card_messages"),
          make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study_left_side"), messageBarType = 5), style = "margin-top:10px;"))
        )
      )
    ),
    
    # --- --- --- --- --- --- ---
    # Study description card ----
    # --- --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("studies_description_card"),
        div(id = ns("studies_description_content"),
          make_card(i18n$t("studies_description_card"),
            uiOutput(ns("studies_description_markdown_result"))
          ), br()
        ),
        div(
          id = ns("choose_a_study_card_description"),
          make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study_left_side"), messageBarType = 5), style = "margin-top:10px;"))
        )
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Studies management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("studies_datatable_card"),
        make_card(i18n$t("studies_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "study_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_study"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("studies_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_studies_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- ---
    # Study options card ----
    # --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("studies_options_card"),
        make_card(i18n$t("study_options"),
          div(
            make_combobox(i18n = i18n, ns = ns, label = "study", id = "options_selected", width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(
              div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                list(key = "everybody", text = i18n$t("everybody_who_has_access_to_dataset")),
                list(key = "people_picker", text = i18n$t("choose_users"))
              ), className = "inline_choicegroup"),
              conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                uiOutput(ns("users_allowed_read_div"))
              )
            ), br(),
            strong(i18n$t("study_description")),
            div(shinyAce::aceEditor(ns("ace_options_description"), "", mode = "markdown",
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save")),
              shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("preview"))
            ),
            br(),
            div(id = ns("description_markdown_output"),
              uiOutput(ns("description_markdown_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Import a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("import_study_card"),
        make_card(i18n$t("import_study"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Importer une étude nécessite d'importer :",
                  tags$ul(
                    tags$li("Importer l'étude en elle-même (table études de la BDD)"),
                    tags$li("Importer les données relatives à l'étude (tabs, données modifiées sur patients et sur tabs)"),
                    tags$li("S'assurer que les plugins sont tous installés et à la bonne version")
                  )
                ),
                p("Comment faire pour les correspondances entre membres ?")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Export a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_card(i18n$t("export_study"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Même principe que pour l'import d'une étude")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    br()
  )
}
    
#' my_studies Server Functions
#'
#' @noRd 
mod_my_studies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) print(paste0(Sys.time(), " - mod_my_studies - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    messages_timer <- reactiveTimer(10000, session)
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("studies_messages_card", "studies_description_card", "studies_datatable_card", "studies_options_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)

    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_my_studies_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_my_studies_open_panel <- FALSE)
    
    r$help_my_studies_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_my_studies_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_my_studies_open_modal <- FALSE
      r$help_my_studies_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r$help_my_studies_open_panel <- FALSE
      r$help_my_studies_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_my_studies_page_", i)]] <- Sys.time())
    })
    
    help_my_studies(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$selected_dataset"))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("studies_messages_card" %in% r$user_accesses) shinyjs::show("studies_messages_card")
        else shinyjs::show("studies_messages_card_forbidden")
      }
      else{
        if (input$current_tab %in% r$user_accesses) shinyjs::show(input$current_tab)
        else shinyjs::show(paste0(input$current_tab, "_forbidden"))
      }
      
      # Hide messages card & reset fields
      sapply(c("choose_a_study_card_messages", "choose_a_study_card_description"), shinyjs::show)
      sapply(c("study_messages_content", "conversation_new_message_div", "studies_description_content"), shinyjs::hide)
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      shinyAce::updateAceEditor(session, "new_message_text", value = "")
      output$conversation_object <- renderUI("")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      
      # The dataset is loaded here, and not in sidenav
      # Placed in sidenav, the dataset is loaded multiple times (each time a page loads its own sidenav)
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      # Reset d variables
      
      visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
        "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
      person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
      subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
      main_tables <- c(subset_tables, "location", "care_site", "provider")
      
      sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
      sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

      # Reset selected_study variable
      m$selected_study <- NA_integer_
      m$selected_person <- NA_integer_ # To prevent bug when execute plugin code from plugin page
      
      # A r variable to update study dropdown, when the load of dataset is finished
      r$loaded_dataset <- r$selected_dataset
      
      # Load studies & scripts related to this dataset
      update_r(r = r, m = m, table = "studies")
      update_r(r = r, m = m, table = "scripts")
      
      r$force_reload_scripts_cache <- FALSE
      
      # Try to load dataset
      tryCatch({

        capture.output(run_dataset_code(output, r = r, d = d, dataset_id = r$selected_dataset, i18n = i18n))
  
        r$show_message_bar <- tibble::tibble(message = "import_dataset_success", type = "success", trigger = Sys.time())
        
        r$load_scripts <- Sys.time()
      },
      error = function(e){
        r$show_message_bar <- tibble::tibble(message = "fail_load_dataset", type = "severeWarning", trigger = Sys.time())
        report_bug(r = r, output = output, error_message = "fail_load_dataset",
          error_name = paste0(id, " - run server code"), category = "Error", error_report = e, i18n = i18n)
      })
      
      r$reload_studies_datatable <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$selected_dataset"))
    })
    
    # Load scripts
    
    observeEvent(r$load_scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$load_scripts"))
      
      # Try to run the scripts associated with this dataset
      # Save runned scripts and success status
      
      r$dataset_loaded_scripts <- tibble::tibble(id = integer(), status = character(), datetime = character())
      
      if (nrow(r$scripts) > 0){
        
        scripts <- r$scripts %>% dplyr::inner_join(
          r$options %>% dplyr::filter(category == "dataset_scripts", link_id == r$selected_dataset) %>% dplyr::select(id = value_num),
          by = "id"
        ) %>%
          dplyr::inner_join(r$code %>% dplyr::filter(category == "script") %>% dplyr::select(id = link_id, code), by = "id")
        
        if (nrow(scripts) > 0){
          for (i in 1:nrow(scripts)){
            
            script <- scripts[i, ]
            
            r$dataset_loaded_scripts <- r$dataset_loaded_scripts %>% dplyr::bind_rows(
              tibble::tibble(id = script$id, status = "failure", datetime = as.character(Sys.time())))
            
            # Execute script code
            captured_output <- capture.output(
              tryCatch({
                eval(parse(text = script$code %>% stringr::str_replace_all("\r", "\n")))
                r$dataset_loaded_scripts <- r$dataset_loaded_scripts %>% dplyr::mutate(status = dplyr::case_when(
                  id == script$id ~ "success", TRUE ~ status
                ))
              },
                error = function(e){
                  r$show_message_bar <- tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
                  report_bug(r = r, output = output, error_message = "fail_load_scripts",
                    error_name = paste0(id, " - run server code"), category = "Error", error_report = e, i18n = i18n)})
            )
          }
        }
        
        if (nrow(r$dataset_loaded_scripts) > 0) r$reload_scripts_cache <- Sys.time()
      }
      
      # Join d$person, d$visit_occurrence & d$visit_detail with r$dataset_all_concepts
      
      # r$merge_concepts_and_d_vars <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$load_scripts"))
    })
    
    # Reload scripts cache
    
    observeEvent(r$reload_scripts_cache, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_scripts_cache"))
      
      req(!is.na(r$selected_dataset))
      
      # If activate_scripts_cache option activated and if cache doesn't exists, save data as CSV files
      if(r$options %>% dplyr::filter(category == "dataset", name == "activate_scripts_cache", link_id == r$selected_dataset) %>% dplyr::pull(value_num) == 1){
        
        tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
          "drug_era", "dose_era", "condition_era", "person", "observation_period", "visit_occurrence", "visit_detail",
          "location", "care_site", "provider")
        
        dataset_file_path <- paste0(r$app_folder, "/datasets/", r$selected_dataset)
        loaded_scripts_file_path <- paste0(r$app_folder, "/datasets/", r$selected_dataset, "/loaded_scripts.csv")
        
        # If dataset folder doesn't exist, create it
        if (!dir.exists(dataset_file_path)) dir.create(dataset_file_path)
        
        # If cache doesn't exist, create cache
        if (!file.exists(loaded_scripts_file_path) | r$force_reload_scripts_cache){
          
          # Save data as CSV files
          for (table in tables){
            if (nrow(d[[table]]) > 0){
              readr::write_csv(d[[table]], paste0(r$app_folder, "/datasets/", r$selected_dataset, "/", table, "_with_scripts.csv"))
            }
          }
          
          # Save a CSV file for informations on loaded scripts
          readr::write_csv(r$dataset_loaded_scripts, paste0(r$app_folder, "/datasets/", r$selected_dataset, "/loaded_scripts.csv"))
        }
        
        # Load cache if already exists
        
        if (file.exists(loaded_scripts_file_path)){
          for (table in tables){
            table_file_path <- paste0(r$app_folder, "/datasets/", r$selected_dataset, "/", table, "_with_scripts.csv")
            
            if (file.exists(table_file_path)){
              
              omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
              
              col_types <- switch(table, 
                "person" = "iiiiiTTiiiiiccicici",
                "observation_period" = "iiDDi",
                "visit_occurrence" = "iiiDTDTiiiciicici",
                "visit_detail" = "iiiDTDTiiiciciciiii",
                "condition_occurrence" = "iiiDTDTiiciiicic",
                "drug_exposure" = "iiiDTDTDiciniciciiicicc",
                "procedure_occurrence" = "iiiDTiiiiiicic",
                "device_exposure" = "iiiDTDTiciiiici",
                "measurement" = "iiiDTciiniinniiicicc",
                "observation" = "iiiDTinciiiiiicicciiT",
                "death" = "iDTiici",
                "note" = "iiiiDTiicciiiiic",
                "note_nlp" = "iiiccciicDTccc",
                "specimen" = "iiiiDTniiiccccc",
                "fact_relationship" = "iiiii",
                "location" = "icccccccnn",
                "location_hisTory" = "iiciDD",
                "care_site" = "iciicc",
                "provider" = "iccciiiiccici",
                "payer_plan_period" = "iiiDDiciiciiciicicici",
                "cost" = "iiiiiiicinDDDiicci",
                "drug_era" = "iiiTTii",
                "dose_era" = "iiiinTT",
                "condition_era" = "iiiTTi"
              )
              if (table == "person" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiiiTiiiiiccicici"
              if (table == "observation" & omop_version == "5.3") col_types <-  "iiiDTinciiiiiicicc"
              if (table == "observation" & omop_version == "5.4") col_types <-  "iiiDTinciiiiiicicccii"
              if (table == "location" & omop_version == "5.3") col_types <-  "iccccccc"
              if (table == "drug_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDii"
              if (table == "dose_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiinDD"
              if (table == "condition_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDi"
              
              d[[table]] <- readr::read_csv(table_file_path, col_types = col_types)
            }
          }
        }
      }
      
      if (nrow(r$dataset_loaded_scripts %>% dplyr::filter(status == "failure")) > 0) r$show_message_bar <- 
          tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
      else r$show_message_bar <- tibble::tibble(message = "run_scripts_success", type = "success", trigger = Sys.time())
      
      r$force_reload_scripts_cache <- FALSE
      r$update_scripts_cache_card <- Sys.time()
      
      # Join d$person, d$visit_occurrence & d$visit_detail with r$dataset_all_concepts
      
      r$merge_concepts_and_d_vars <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$reload_scripts_cache"))
    })
    
    # Once the dataset is loaded, load studies & scripts
    observeEvent(r$loaded_dataset, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$loaded_dataset"))
      
      # Load studies datatable
      # r$reload_studies_datatable <- Sys.time()
      
      # Update dropdown for study options
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected", options = options)
    })
    
    # --- --- --- --- --- --- --- --
    # When a study is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$selected_study, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$selected_study"))
      
      req(!is.na(m$selected_study))
      # Show first card & hide "choose a dataset" card
      sapply(c("choose_a_study_card_messages", "choose_a_study_card_description"), shinyjs::hide)
      sapply(c("study_messages_content", "studies_description_content"), shinyjs::show)
      
      # Reset d variables
      
      visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
        "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
      person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
      subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
      main_tables <- c(subset_tables, "location", "care_site", "provider")
      
      sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
      
      # Reset new conversation fields
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      shinyAce::updateAceEditor(session, "new_message_text", value = "")
      output$conversation_object <- renderUI("")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      shinyjs::hide("conversation_new_message_div")
      
      # Update study options combobox
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = m$selected_study, text = r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected", options = options, value = value)
      
      # Subsets depending on the selected study
      update_r(r = r, m = m, table = "subsets")

      # Reset selected_subset
      m$selected_subset <- NA_integer_
      
      # Select patients belonging to subsets of this study
      update_r(r = r, m = m, table = "subsets_persons")
      
      # Load patients options
      sql <- glue::glue_sql("SELECT * FROM persons_options WHERE study_id = {m$selected_study}", .con = m$db)
      m$persons_options <- DBI::dbGetQuery(m$db, sql)
      
      # Load study description
      
      # Get description from database
      study_description <- r$options %>% dplyr::filter(category == "study" & name == "markdown_description" & link_id == m$selected_study) %>% 
        dplyr::pull(value) %>% stringr::str_replace_all("\r", "\n")
      
      tryCatch({
        
        # Clear temp dir
        unlink(paste0(path.expand("~"), "/linkr_temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          path.expand("~"), "/linkr_temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", path.expand("~"), "/linkr_temp_files/', fig.path = '", path.expand("~"), "/linkr_temp_files/')\n```\n")
        
        markdown_file <- paste0(markdown_settings, study_description)
        
        # Create temp dir
        dir <- paste0(path.expand("~"), "/linkr_temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$studies_description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
    })
    
    # --- --- --- --- --- --- --- --
    # When a subset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$selected_subset, {
      if (!is.na(m$selected_subset)){
        
        # Reset d variables

        visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
        person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")

        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        # Select patients who belong to this subset
        update_r(r = r, m = m, table = "subset_persons")
        
        # If this subset contains no patient, maybe the code has not been run yet
        if (nrow(m$subset_persons) == 0){
          subset_code <- r$code %>% dplyr::filter(category == "subset" & link_id == m$selected_subset) %>% dplyr::pull(code) %>%
            stringr::str_replace_all("%dataset_id%", as.character(r$selected_dataset)) %>%
            stringr::str_replace_all("%subset_id%", as.character(m$selected_subset)) %>%
            stringr::str_replace_all("\r", "\n")
          
          tryCatch(eval(parse(text = subset_code)),
            error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_execute_subset_code", 
              error_name = paste0("sidenav - execute_subset_code  - id = ", m$selected_subset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
          )
          
          update_r(r = r, m = m, table = "subset_persons")
        }
      }
    })
    
    # --- --- --- -
    # Messages ----
    # --- --- --- -
      
      # --- --- --- --- --
      ## All messages ----
      # --- --- --- --- --

      observeEvent(m$selected_study, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer m$selected_study"))

        req(!is.na(m$selected_study))
        
        # All users of the study have access to the study conversations
        # Remove conversations deleted by the user (still visible for other users)

        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read, m.deleted ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$selected_study} ",
          "AND m.conversation_id NOT IN (",
          " SELECT DISTINCT(udc.conversation_id) FROM user_deleted_conversations udc WHERE udc.user_id = {r$user_id}",
          ")"), .con = r$db)

        r$study_messages <- DBI::dbGetQuery(r$db, sql) %>%
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime))

        if (nrow(r$study_messages) > 0) r$study_conversations <- r$study_messages %>%
          dplyr::group_by(conversation_id) %>%
          dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ 0, TRUE ~ 1)) %>%
          dplyr::select(-read) %>%
          dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime)) %>%
          dplyr::relocate(unread_messages, .after = "datetime") %>%
          dplyr::mutate_at("datetime", as.character) %>%
          dplyr::mutate(
            datetime = stringr::str_replace_all(datetime, "T|Z", ""),
            action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
              onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
          ) %>%
          dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id)))

        if (nrow(r$study_messages) == 0) r$study_conversations <- tibble::tibble(conversation_id = integer(),
          conversation_name = character(), datetime = character(), unread_messages = integer(), action = character())
        
        r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
        
        r$study_reload_conversations_datatable_clear_selection <- "all"
        r$study_reload_conversations_datatable <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer m$selected_study"))
      })
    
      conv_searchable_cols <- c("conversation_name", "unread_messages")
      conv_factorize_cols <- c("unread_messages")
      conv_column_widths <- c("datetime" = "150px", "unread_messages" = "150px", "action" = "80px")
      conv_sortable_cols <- c("id", "conversation_name", "datetime", "unread_messages")
      conv_centered_cols <- c("id", "datetime", "unread_messages", "action")
      conv_col_names <- get_col_names(table_name = "study_conversations", i18n = i18n)
      conv_hidden_cols <- c("conversation_id", "modified", "unread_messages")
    
      # Reload conversations
      
      observeEvent(r$study_reload_conversations_datatable, {
        
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$study_reload_conversations_datatable"))
        
        # Render datatable
        if (length(r$study_conversations_datatable_proxy) == 0){
          render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$study_conversations_temp,
            output_name = "study_conversations", col_names = conv_col_names,
            sortable_cols = conv_sortable_cols, centered_cols = conv_centered_cols, column_widths = conv_column_widths,
            searchable_cols = conv_searchable_cols, filter = TRUE, factorize_cols = conv_factorize_cols, 
            hidden_cols = conv_hidden_cols, bold_rows = c("unread_messages" = 1))

          # Create a proxy for datatatable
          r$study_conversations_datatable_proxy <- DT::dataTableProxy("study_conversations", deferUntilFlush = FALSE)
        }

        if (length(r$study_conversations_datatable_proxy) > 0) DT::replaceData(r$study_conversations_datatable_proxy,
          r$study_conversations_temp, resetPaging = FALSE, rownames = FALSE, clearSelection = r$study_reload_conversations_datatable_clear_selection)
      })
      
      # Timer to update conversations & messages
      
      observe({
        
        # if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer to update messages"))
        
        messages_timer()
        req(!is.na(m$selected_study))
        
        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read, m.deleted ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$selected_study} ",
          "AND m.conversation_id NOT IN (",
          " SELECT DISTINCT(udc.conversation_id) FROM user_deleted_conversations udc WHERE udc.user_id = {r$user_id}",
          ")"), .con = r$db)
        
        study_messages <- DBI::dbGetQuery(r$db, sql) %>%
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime))
        
        if (study_messages %>% dplyr::select(id, deleted) %>% dplyr::anti_join(r$study_messages %>% dplyr::select(id, deleted), by = c("id", "deleted")) %>% nrow() > 0 & nrow(study_messages) > 0){
          
          r$study_messages <- study_messages
          
          r$study_conversations <- r$study_messages %>%
            dplyr::group_by(conversation_id) %>%
            dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ 0, TRUE ~ 1)) %>%
            dplyr::select(-read) %>%
            dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime)) %>%
            dplyr::relocate(unread_messages, .after = "datetime") %>%
            dplyr::mutate_at("datetime", as.character) %>%
            dplyr::mutate(datetime = stringr::str_replace_all(datetime, "T|Z", "")) %>%
            dplyr::mutate(
              datetime = stringr::str_replace_all(datetime, "T|Z", ""),
              action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
            ) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id)))
          
          r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
          
          # Unselect rows
          r$study_reload_conversations_datatable_clear_selection <- "all"
          
          # Reload datatable
          r$study_reload_conversations_datatable <- Sys.time()
          
          r$study_reload_conversation <- Sys.time()
          r$study_reload_conversation_type <- "refresh_conversation"
        }
        
        # if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer to update messages"))
      })
      
      # --- --- --- --- --- --- ---
      ## Delete a conversation ----
      # --- --- --- --- --- --- ---
      
      r$study_delete_conversation_open_dialog <- FALSE
      
      output$conversation_delete_confirm <- shiny.fluent::renderReact({
        
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - output$conversation_delete_confirm"))
        
        shiny.fluent::Dialog(
          hidden = !r$study_delete_conversation_open_dialog,
          onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('study_delete_conversation_hide_dialog', Math.random()); }")),
          dialogContentProps = list(
            type = 0,
            title = i18n$t("study_delete_conversation_title"),
            closeButtonAriaLabel = "Close",
            subText = tagList(i18n$t("study_delete_conversation_subtext"), br(), br())
          ),
          modalProps = list(),
          shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("study_delete_conversation_delete_confirmed"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("study_delete_conversation_delete_canceled"), text = i18n$t("dont_delete"))
          )
        )
      })
      
      # Whether to close or not delete dialog box
      observeEvent(input$study_delete_conversation_hide_dialog, {
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_conversation_hide_dialog"))
        r$study_delete_conversation_open_dialog <- FALSE 
      })
      observeEvent(input$study_delete_conversation_delete_canceled, {
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_conversation_delete_canceled"))
        r$study_delete_conversation_open_dialog <- FALSE
      })
      
      # When the deletion is confirmed
      
      observeEvent(input$study_delete_conversation_delete_confirmed, {
        
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_conversation_delete_confirmed"))
        
        link_id <- substr(input$conversation_deletion, nchar(paste0(id, "-delete_conversation_")) + 1, nchar(input$conversation_deletion)) %>% as.integer()
        
        new_data <- tibble::tibble(
          id = get_last_row(r$db, "user_deleted_conversations") + 1, conversation_id = link_id,
          user_id = r$user_id, datetime = as.character(Sys.time()))
        DBI::dbAppendTable(r$db, "user_deleted_conversations", new_data)
        
        r$study_conversations <- r$study_conversations %>% dplyr::filter(conversation_id != link_id)
        r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
        
        r$study_delete_conversation_open_dialog <- FALSE
        
        r$study_reload_conversations_datatable <- Sys.time()
        
        output$conversation_object <- renderUI("")
        output$selected_conversation <- renderUI("")
        shinyjs::hide("conversation_new_message_div")
      })
      
      observeEvent(input$conversation_deletion, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$conversation_deletion"))
        r$study_delete_conversation_open_dialog <- TRUE
      })
      
      # --- --- --- --- --- --
      ## Delete a message ----
      # --- --- --- --- --- --
      
      r$study_delete_message_open_dialog <- FALSE
      
      output$message_delete_confirm <- shiny.fluent::renderReact({
        
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - output$message_delete_confirm"))
        
        shiny.fluent::Dialog(
          hidden = !r$study_delete_message_open_dialog,
          onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('study_delete_message_hide_dialog', Math.random()); }")),
          dialogContentProps = list(
            type = 0,
            title = i18n$t("study_delete_message_title"),
            closeButtonAriaLabel = "Close",
            subText = tagList(i18n$t("study_delete_message_subtext"), br(), br())
          ),
          modalProps = list(),
          shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("study_delete_message_delete_confirmed"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("study_delete_message_delete_canceled"), text = i18n$t("dont_delete"))
          )
        )
      })
      
      # Whether to close or not delete dialog box
      observeEvent(input$study_delete_message_hide_dialog, {
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_message_hide_dialog"))
        r$study_delete_message_open_dialog <- FALSE 
      })
      observeEvent(input$study_delete_message_delete_canceled, {
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_message_delete_canceled"))
        r$study_delete_message_open_dialog <- FALSE
      })
      
      # When the deletion is confirmed
      
      observeEvent(input$study_delete_message_delete_confirmed, {
        
        if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$study_delete_message_delete_confirmed"))
        
        link_id <- substr(input$message_deletion, nchar(paste0(id, "-delete_message_")) + 1, nchar(input$message_deletion)) %>% as.integer()
        print(link_id)
        
        sql <- glue::glue_sql("UPDATE messages SET deleted = TRUE WHERE id = {link_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        r$study_messages <- r$study_messages %>% dplyr::mutate(deleted = ifelse(id == link_id, TRUE, deleted))
        r$study_messages_temp <- r$study_messages %>% dplyr::mutate(modified = FALSE)
        
        r$study_delete_message_open_dialog <- FALSE
        
        # Reload conversation
        r$study_reload_conversation <- Sys.time()
        r$study_reload_conversation_type <- "refresh_conversation"
      })
      
      observeEvent(input$message_deletion, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$message_deletion"))
        r$study_delete_message_open_dialog <- TRUE
      })
      
      # --- --- --- --- --- --- --- --- --- -
      ## When a conversation is selected ----
      # --- --- --- --- --- --- --- --- --- -
      
      observeEvent(input$study_conversations_rows_selected, {
        
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$study_conversations_rows_selected"))

        # Show conversation messages
        
        r$study_reload_conversation <- Sys.time()
        r$study_reload_conversation_type <- "select_conversation"
        r$study_selected_conversation <- r$study_conversations_temp[input$study_conversations_rows_selected, ]
        
        # Mark conversation as read
        r$study_conversations_temp <- r$study_conversations_temp %>%
          dplyr::mutate(unread_messages = dplyr::case_when(
            conversation_id == r$study_conversations_temp[input$study_conversations_rows_selected, "conversation_id"] %>% dplyr::pull() ~ 0,
            TRUE ~ unread_messages
          ))
        
        # Reload datatable
        r$study_reload_conversations_datatable_clear_selection <- "none"
        r$study_reload_conversations_datatable <- Sys.time()
      })

      observeEvent(r$study_reload_conversation, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$study_reload_conversation"))

        req(r$study_selected_conversation)
        
        conversation_messages <- r$study_messages %>% dplyr::inner_join(r$study_selected_conversation %>% dplyr::select(conversation_id), by = "conversation_id") %>%
          dplyr::arrange(dplyr::desc(datetime))

        conversation_messages_ui <- tagList()

        colours <- c("#f94144","#f3722c","#f8961e","#f9844a","#f9c74f","#90be6d","#43aa8b","#4d908e","#577590","#277da1")
        conversation_messages_users <- conversation_messages %>% dplyr::group_by(creator_id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
          dplyr::select(creator_id) %>%
          dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, initials = paste0(toupper(substr(firstname, 1, 1)), toupper(substr(lastname, 1, 1)))), by = "creator_id") %>%
          dplyr::mutate(colour = "")
        conversation_messages_users$colour <- sample(colours, nrow(conversation_messages_users), replace = TRUE, prob = rep(0.1, 10))

        for (i in 1:nrow(conversation_messages)){

          study_message <- conversation_messages[i, ]

          if (study_message$deleted) message_div <- div(tags$em(i18n$t("deleted_message")))
          else message_div <- div(HTML(study_message$message))

          if (study_message$filepath != "" & !study_message$deleted){
            
            fail_load_message <- TRUE
            
            tryCatch({
              message_div <- div(class = "markdown_messages", withMathJax(includeMarkdown(study_message$filepath)))
              fail_load_message <- FALSE
            }, error = function(e) report_bug(r = r, output = output, error_message = "fail_load_message",
              error_name = paste0(id, " - load message - message_id = ", study_message$id), category = "Error", error_report = e, i18n = i18n, ns = ns),
              warning = function(w) report_bug(r = r, output = output, error_message = "fail_load_message",
                error_name = paste0(id, " - load message - message_id = ", study_message$id), category = "Error", error_report = w, i18n = i18n, ns = ns))
            
            if (fail_load_message) message_div <- div(
              tags$em(i18n$t("fail_load_message_show_raw_message")), br(), br(), HTML(study_message$message))
          }

          date <- study_message$datetime %>% as.Date()
          if (language == "fr") date <- date %>% format(format = "%d-%m-%Y")
          
          if (!study_message$deleted & study_message$creator_id == r$user_id) deletion_div <- div(
              actionButton(ns(paste0("delete_message_", study_message$id)), "X", style = "padding:0px 5px 0px 5px;",
              onclick = paste0("Shiny.setInputValue('", id, "-message_deletion', this.id, {priority: 'event'})")), 
            style = "float:right; margin-top:-10px; margin-right:-10px;")
          else deletion_div <- ""

          if (study_message$creator_id == r$user_id){
            study_message_ui <- div(
              div(
                deletion_div,
                div(
                  div(paste0(date, ", ", format(study_message$datetime, "%H:%M"))),
                  style = "font-size:12px; margin-bottom:10px; color:#878787"
                ),
                message_div,
                style = "background-color:#E6F1F8; margin-top:10px; padding:15px; border-radius:10px; width:80%; float:right;"
              )
            )
          }
          else {
            if (is.na(study_message$read) | study_message$read) study_message_style <- "background-color:#F5F5F5; margin-top:10px; padding:15px; border-radius:10px;"
            else study_message_style <- "background-color:#ECF8E7; margin-top:10px; padding:15px; border-radius:10px;"

            creator_name <- r$users %>% dplyr::filter(id == study_message$creator_id) %>%
              dplyr::mutate(creator_name = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator_name)

            author_span <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
              div(creator_name), div(paste0(date, ", ", format(study_message$datetime, "%H:%M")))
            )
            
            if (!study_message$deleted & study_message$creator_id == r$user_id) deletion_div <- div(
                actionButton(ns(paste0("delete_message_", study_message$id)), "X", style = "padding:0px 5px 0px 5px;",
                onclick = paste0("Shiny.setInputValue('", id, "-message_deletion', this.id, {priority: 'event'})")), 
              style = "float:right; margin-top:-10px; margin-right:-10px;")
            else deletion_div <- ""

            study_message_ui <-
            div(
              deletion_div,
              div(
                div(
                  shiny.fluent::Persona(size = 2,
                    imageInitials = conversation_messages_users %>% dplyr::filter(creator_id == study_message$creator_id) %>% dplyr::pull(initials),
                    initialsColor = conversation_messages_users %>% dplyr::filter(creator_id == study_message$creator_id) %>% dplyr::pull(colour)
                  ),
                  style = "float:left; margin-left:-40px; margin-top:10px;"
                ),
                div(
                  div(
                    author_span,
                    style = "font-size:12px; margin-bottom:10px; color:#878787"
                  ),
                  message_div,
                  style = study_message_style
                )
              ),
              style = "width:80%; float:left; margin-left:30px;"
            )
          }

          conversation_messages_ui <- tagList(conversation_messages_ui, study_message_ui)
        }

        output$selected_conversation <- renderUI(conversation_messages_ui)
        shinyjs::show("conversation_new_message_div")

        if (r$study_reload_conversation_type != "refresh_conversation"){
          output$new_message_preview <- renderUI("")
          output$conversation_object <- renderUI(tagList(i18n$t("object"), " : ", strong(r$study_selected_conversation$conversation_name)))
          shinyAce::updateAceEditor(session, "new_message_text", value = "")
          sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
          shinyjs::show("conversation_new_message")
        }

        # Update inbox_messages
        # Create a row if user hasn't a row (admin who has access to all studies or an account created after the conversation was sent)
        
        sql <- glue::glue_sql("SELECT * FROM inbox_messages WHERE receiver_id = {r$user_id} AND message_id IN ({conversation_messages$id*})", .con = r$db)
        has_inbox_messages <- DBI::dbGetQuery(r$db, sql)
        if (nrow(has_inbox_messages) == 0){
          new_data <- conversation_messages %>% dplyr::select(message_id = id) %>%
            dplyr::mutate(id = 1:dplyr::n() + get_last_row(r$db, "inbox_messages"), .before = "message_id") %>%
            dplyr::mutate(receiver_id = r$user_id, read = TRUE, datetime = as.character(Sys.time()), deleted = FALSE)
          DBI::dbAppendTable(r$db, "inbox_messages", new_data)
        }
        
        sql <- glue::glue_sql("UPDATE inbox_messages SET read = TRUE WHERE receiver_id = {r$user_id} AND message_id IN ({conversation_messages$id*})", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)

        # Update r$study_messages
        r$study_messages <- r$study_messages %>%
          dplyr::left_join(conversation_messages %>% dplyr::transmute(id, new_read = TRUE), by = "id") %>%
          dplyr::mutate(read = dplyr::case_when(
            as.logical(read) ~ TRUE, new_read ~ TRUE, TRUE ~ FALSE
          )) %>%
          dplyr::select(-new_read) %>%
          dplyr::arrange(dplyr::desc(datetime))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$study_reload_conversation"))
      })

      # --- --- --- --- -
      ## New message ----
      # --- --- --- --- -

      observeEvent(input$conversation_new_message, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$conversation_new_message"))
        sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::show(name))
        shinyjs::hide("conversation_new_message")
      })

      observeEvent(input$conversation_hide_new_message, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$conversation_hide_new_message"))
        sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
        shinyjs::show("conversation_new_message")
      })

      # --- --- --- --- --- --- --- --- --- --- -- -
      ## Preview for new message & conversation ----
      # --- --- --- --- --- --- --- --- --- --- -- -

      observeEvent(input$preview_new_conversation, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$preview_new_conversation"))
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "conversation"
      })

      observeEvent(input$preview_new_message, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$preview_new_message"))
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "message"
      })

      observeEvent(r$study_preview_trigger, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$study_preview_trigger"))

        req(r$study_preview_type %in% c("message", "conversation"))

        type <- r$study_preview_type
        if (type == "conversation") background_color <- "#E6F1F8"
        if (type == "message") background_color <- "#ECF8E7"

        if (input[[paste0("new_", type, "_text")]] == "") output[[paste0("new_", type, "_preview")]] <- renderUI("")

        req(input[[paste0("new_", type, "_text")]] != "")

        if (input[[paste0("new_", type, "_as_rmarkdown")]]){
 
          tryCatch({

            new_text <- input[[paste0("new_", type, "_text")]] %>% stringr::str_replace_all("\r", "\n")

            # Clear temp dir
            unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)

            markdown_settings <- paste0("```{r setup, include=FALSE}\n",
              "knitr::opts_knit$set(root.dir = '", r$app_folder, "/temp_files/')\n",
              "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/', fig.path = '", r$app_folder, "/temp_files/',",
              "dpi = 600, out.width='600px')\n",
              "```\n")

            markdown_file <- paste0(markdown_settings, new_text)

            # Create temp dir
            dir <- paste0(r$app_folder, "/temp_files")
            new_file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
            if (!dir.exists(dir)) dir.create(dir)

            # Variables to hide
            new_env_vars <- list("r" = NA)
            # Variables to keep
            for (var in c("d", "m", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
            new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
            
            # Create the markdown file
            knitr::knit(text = markdown_file, envir = new_env, output = new_file, quiet = TRUE)

            output[[paste0("new_", type, "_preview")]] <- renderUI(
              div(
                div(
                  div(paste0(i18n$t("today"), ", ", format(Sys.time(), "%H:%M"))),
                  style = "font-size:12px; margin-bottom:10px; color:#878787"
                ),
                div(class = "markdown_messages", withMathJax(includeMarkdown(new_file))),
                style = paste0("background-color:", background_color, "; margin-top:10px; padding:15px 15px 0px 15px; border-radius:10px; float:left; width:80%;")
              )
            )
          }, error = function(e) "")
        }

        else {

          new_text <- input[[paste0("new_", type, "_text")]] %>% stringr::str_replace_all("\n", "<br />")

          output[[paste0("new_", type, "_preview")]] <- renderUI(
            div(
              div(
                div(paste0(i18n$t("today"), ", ", format(Sys.time(), "%H:%M"))),
                style = "font-size:12px; margin-bottom:10px; color:#878787"
              ),
              div(HTML(new_text)),
              style = paste0("background-color:", background_color, "; margin-top:10px; padding:15px; border-radius:10px; float:left; width:80%;")
            )
          )
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$study_preview_trigger"))
      })

      # --- --- --- --- --- --- --- --- --- --
      ## Save new message or conversation ----
      # --- --- --- --- --- --- --- --- --- --

      observeEvent(input$send_new_conversation, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$send_new_conversation"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "conversation"
      })

      observeEvent(input$send_new_message, {
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$send_new_message"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "message"
      })

      observeEvent(r$study_save_message_conversation_trigger, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$study_save_message_conversation_trigger"))

        req(r$study_save_message_conversation_type %in% c("message", "conversation"))
        type <- r$study_save_message_conversation_type

        # Is conversation name empty ?
        if (type == "conversation"){
          if (is.na(input$new_conversation_name) | input$new_conversation_name == "")
            shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", errorMessage = i18n$t("provide_valid_object"))
          else shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", errorMessage = NULL)

          req(!is.na(input$new_conversation_name) & input$new_conversation_name != "")
        }

        # Is text is empty ?
        req(input[[paste0("new_", type, "_text")]] != "")

        # If Rmarkdown toggle is TRUE, save the file

        if (input[[paste0("new_", type, "_as_rmarkdown")]]){

          tryCatch({

            new_text <- input[[paste0("new_", type, "_text")]] %>% stringr::str_replace_all("\r", "\n")

            unique_id <- paste0(Sys.time() %>% stringr::str_replace_all(":| |-", "") , paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
            new_dir <- paste0(r$app_folder, "/messages/", unique_id)

            markdown_settings <- paste0("```{r setup, include=FALSE}\n",
              "knitr::opts_knit$set(root.dir = '", new_dir, "/')\n",
              "knitr::opts_chunk$set(root.dir = '", new_dir, "/', fig.path = '", new_dir, "/',",
              "dpi = 600, out.width='600px')\n",
              "```\n")

            markdown_file <- paste0(markdown_settings, new_text)

            # Create new dir
            new_file <- paste0(new_dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":| |-", ""), ".Md")
            if (!dir.exists(new_dir)) dir.create(new_dir)
            
            # Variables to hide
            new_env_vars <- list("r" = NA)
            # Variables to keep
            for (var in c("d", "m", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
            new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))

            # Create the markdown file
            knitr::knit(text = markdown_file, envir = new_env, output = new_file, quiet = TRUE)

          }, error = function(e) "")
        }
        else {
          new_text <- input[[paste0("new_", type, "_text")]]
          new_file <- ""
        }

        # Get list of users authorized to see this study

        users_allowed_read_group <- r$options %>% dplyr::filter(category == "study", link_id == m$selected_study, name == "users_allowed_read_group") %>% dplyr::pull(value)
        if (users_allowed_read_group == "everybody") receivers_ids <- r$users %>% dplyr::pull(id)
        if (users_allowed_read_group == "people_picker") receivers_ids <- r$options %>% dplyr::filter(category == "study", link_id == m$selected_study, name == "user_allowed_read") %>% dplyr::pull(value_num)

        # Add data to database

        if (type == "conversation"){
          conversation_id <- get_last_row(r$db, "conversations") + 1
          conversation_name <- input$new_conversation_name
        }
        if (type == "message"){
          conversation_id <- r$study_selected_conversation$conversation_id
          conversation_name <- r$study_selected_conversation$conversation_name
        }

        new_message_id <- get_last_row(r$db, "messages") + 1

        if (type == "conversation"){
          # Conversations table
          new_data <- tibble::tribble(
            ~id, ~name, ~datetime, ~deleted,
            conversation_id, input$new_conversation_name, as.character(Sys.time()), FALSE)
          DBI::dbAppendTable(r$db, "conversations", new_data)
        }

        # Messages table
        new_data <- tibble::tribble(
          ~id, ~conversation_id, ~study_id, ~category, ~message, ~filepath, ~creator_id, ~datetime, ~deleted,
          new_message_id, conversation_id, m$selected_study, "study_message",
          new_text, new_file, r$user_id, as.character(Sys.time()), FALSE)
        DBI::dbAppendTable(r$db, "messages", new_data)

        # Inbox_messages table
        new_data <- tibble::tibble(
          id = seq(get_last_row(r$db, "inbox_messages") + 1, get_last_row(r$db, "inbox_messages") + length(receivers_ids), 1),
          message_id = new_message_id, receiver_id = receivers_ids, read = FALSE, datetime = as.character(Sys.time()), deleted = FALSE) %>%
          dplyr::mutate(read = dplyr::case_when(receiver_id == r$user_id ~ TRUE, TRUE ~ read))
        DBI::dbAppendTable(r$db, "inbox_messages", new_data)

        # Reset fields
        if (type == "conversation") shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
        shinyAce::updateAceEditor(session, paste0("new_", type, "_text"), value = "")
        output[[paste0("new_", type, "_preview")]] <- renderUI("")

        if (type == "conversation"){

          # Reload conversations datatable
          
          r$study_conversations <- r$study_conversations %>%
            dplyr::bind_rows(
              tibble::tibble(
                conversation_id = conversation_id, conversation_name = conversation_name, 
                datetime = as.character(Sys.time()), unread_messages = 0, action = ""
              )
            ) %>%
            dplyr::mutate(
              action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
            ) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id))) %>%
            dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime))
          
          r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
          
          r$study_reload_conversations_datatable_clear_selection <- "all"
          r$study_reload_conversations_datatable <- Sys.time()
        }

        # Reload conversation

        r$study_messages <- r$study_messages %>%
          dplyr::bind_rows(
            tibble::tribble(
              ~id, ~conversation_id, ~conversation_name, ~message, ~filepath, ~creator_id, ~datetime, ~read, ~deleted,
              new_message_id, conversation_id, conversation_name, new_text, new_file, r$user_id, Sys.time(), 0, FALSE
            )
          ) %>%
          dplyr::arrange(dplyr::desc(datetime))

        if (type == "message"){
          r$study_reload_conversation <- Sys.time()
          r$study_reload_conversation_type <- "add_message"
        } 

        # Notify user only for conversation
        # Set current pivot to messages
        # Hide messages for selected conversation
        if (type == "conversation"){
          show_message_bar(output, message = "new_conversation_added", type = "success", i18n = i18n, ns = ns)
          shinyjs::runjs(glue::glue("$('#{id}-study_messages_pivot button[name=\"{i18n$t('all_messages')}\"]').click();"))
          output$selected_conversation <- renderUI("")
          shinyjs::hide("conversation_new_message_div")
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$study_save_message_conversation_trigger"))
      })

    # --- --- --- --- ---
    # Create a study ----
    # --- --- --- --- ---
    
    observeEvent(input$add_study, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$add_study"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$study_name)
      new_data$study_name <- new_data$name
      new_data$description <- ""
      new_data$patient_lvl_tab_group <- get_last_row(r$db, "patient_lvl_tabs_groups") + 1
      new_data$aggregated_tab_group <- get_last_row(r$db, "aggregated_tabs_groups") + 1
      new_data$dataset <- r$selected_dataset
      
      add_settings_new_data(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "my_studies", 
        data = new_data, table = "studies", required_textfields = "study_name", req_unique_values = "name")
      
      # Reload datatable
      r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$add_study"))
    })
    
    # --- --- --- --- --- ---
    # Studies management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons <- c("options", "delete")
    
    studies_management_editable_cols <- c("name")
    studies_management_sortable_cols <- c("id", "name", "description", "dataset_id", "data_source_id", "study_id", "creator_id", "datetime")
    studies_management_column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    studies_management_centered_cols <- c("id", "creator", "datetime", "action")
    studies_management_searchable_cols <- c("name", "description", "data_source_id", "dataset_id", "study_id", "creator_id")
    studies_management_factorize_cols <- c("dataset_id", "creator_id")
    studies_management_hidden_cols <- c("id", "description", "dataset_id", "patient_lvl_tab_group_id", "aggregated_tab_group_id", "deleted", "modified")
    studies_management_col_names <- get_col_names("studies", i18n)
    
    # Prepare data for datatable
    # This is on a different observer, because r$studies is loaded just before r$reload_studies_datatable is set to Sys.time()
    # If we put this code in the observer of r$selected_dataset, it has no time to execute update_r for studies
    # So r$studies is not updated
    
    observeEvent(r$reload_studies_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_studies_datatable"))
      
      if (nrow(r$studies) == 0) {
        
        data <- tibble::tibble(id = integer(), name = character(), description = character(), dataset_id = factor(),
          patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(), datetime = character(),
          deleted = integer(), modified = logical(), action = character())
      }
      
      if (nrow(r$studies) > 0){
        
        r$studies_temp <- r$studies %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        
        # Prepare data for datatable
        
        r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)
        data <- r$studies_datatable_temp
      }
        
      if (length(r$studies_datatable_proxy) == 0){
        
        # Render datatable
        
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = data,
          output_name = "studies_datatable", col_names = get_col_names("studies", i18n),
          editable_cols = studies_management_editable_cols, sortable_cols = studies_management_sortable_cols, centered_cols = studies_management_centered_cols, 
          column_widths = studies_management_column_widths, searchable_cols = studies_management_searchable_cols, 
          filter = TRUE, factorize_cols = studies_management_factorize_cols, hidden_cols = studies_management_hidden_cols,
          selection = "multiple")
        
        # Create a proxy for datatable
        
        r$studies_datatable_proxy <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, data, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$reload_studies_datatable"))
    })
    
    # Reload datatable
    observeEvent(r$studies_temp, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$studies_temp"))

      # Reload datatable_temp variable
      if (nrow(r$studies_temp) == 0) r$studies_datatable_temp <- tibble::tibble(id = integer(), name = character(), description = character(), dataset_id = factor(),
        patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(), datetime = character(),
        deleted = integer(), modified = logical(), action = character())
      
      if (nrow(r$studies_temp) > 0) r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)

      # Reload data of datatable
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
        r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$studies_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$studies_datatable_cell_edit"))
      
      edit_info <- input$studies_datatable_cell_edit
      r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_studies_management, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$save_studies_management"))
      
      req(nrow(r$studies %>% dplyr::filter(dataset_id == r$selected_dataset)) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "studies", r_table = "studies", i18n = i18n, duplicates_allowed = FALSE)
      
      # Update sidenav dropdown with the new study
      r$reload_studies <- Sys.time()
    })
    
    # Delete a row in datatable
    
    study_delete_prefix <- "study"
    study_dialog_title <- "studies_delete"
    study_dialog_subtext <- "studies_delete_subtext"
    study_react_variable <- "study_delete_confirm"
    study_table <- "studies"
    study_id_var_sql <- "id"
    study_id_var_r <- "delete_study"
    study_delete_message <- "studies_deleted"
    study_reload_variable <- "reload_studies"
    study_information_variable <- "study_deleted"
    study_delete_variable <- paste0(study_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
      react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
      delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
      information_variable = study_information_variable)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$deleted_pressed"))
      
      r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[study_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$delete_selection"))
      
      req(length(input$studies_datatable_rows_selected) > 0)
      
      r$delete_study <- r$studies_temp[input$studies_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[study_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_studies, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_studies"))
      
      r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reset selected study
      m$selected_study <- NA_integer_
    })
    
    # --- --- --- --- --
    # Study options ----
    # --- --- --- --- --
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$studies %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))

      shiny.fluent::updateComboBox.shinyInput(session, "options_selected", options = options, value = value)

      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)

      # Set current pivot to options_card
      button_name <- gsub("'", "\\\\'", i18n$t('study_options'))
      shinyjs::runjs(glue::glue("$('#{id}-studies_pivot button[name=\"{button_name}\"]').click();"))
    })
    
    observeEvent(input$options_selected, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options_selected"))
      
      if (length(input$options_selected) > 1) link_id <- input$options_selected$key
      else link_id <- input$options_selected
      
      options <- r$options %>% dplyr::filter(category == "study", link_id == !!link_id)
      
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
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Study description
      description <- r$options %>% dplyr::filter(category == "study" & name == "markdown_description" & link_id == !!link_id) %>% dplyr::pull(value)
      shinyAce::updateAceEditor(session, "ace_options_description", value = description)
    })
    
    observeEvent(input$options_save, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options_save"))

      req(input$options_selected)

      if (length(input$options_selected) > 1) link_id <- input$options_selected$key
      else link_id <- input$options_selected
      
      data <- list()
      data$users_allowed_read <- input$users_allowed_read
      data$users_allowed_read_group <- input$users_allowed_read_group
      data$markdown_description <- input$ace_options_description
      save_settings_options(output = output, r = r, id = id, category = "study", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("users_allowed_read", "markdown_description"))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$options_save"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$execute_options_description"))
      
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$execute_options_description"))
    })
    
    # --- --- --- --- ---
    # Import a study ----
    # --- --- --- --- ---
    
    # --- --- --- --- ---
    # Export a study ----
    # --- --- --- --- ---
    
    # --- --- --- --- --- --- --
    ## Debug - Execute code ----
    # --- --- --- --- --- --- --
    
    # observeEvent(input$execute_code, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$execute_code"))
    #   
    #   code <- input$ace_edit_code %>% stringr::str_replace_all("\r", "\n")
    #   
    #   output$code_result <- renderText({
    #     
    #     options('cli.num_colors' = 1)
    #     
    #     # Capture console output of our code
    #     captured_output <- capture.output(
    #       tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
    #     
    #     # Restore normal value
    #     options('cli.num_colors' = NULL)
    #     
    #     # Display result
    #     paste(strwrap(captured_output), collapse = "\n")
    #   })
    # })
    
  })
}
