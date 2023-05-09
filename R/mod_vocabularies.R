#' thesaurus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vocabularies_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("vocabularies_concepts_card", "vocabularies_mapping_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("dataset_all_concepts_reload_cache")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "vocabularies_main", text = i18n$t("vocabularies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "vocabularies_concepts_card", itemKey = "vocabularies_concepts_card", headerText = i18n$t("concepts")),
          shiny.fluent::PivotItem(id = "vocabularies_mapping_card", itemKey = "vocabularies_mapping_card", headerText = i18n$t("concepts_mapping"))
        )
      )
    ),
    
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- ---
    # Items card ----
    # --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("vocabularies_concepts_card"),
        make_card(i18n$t("concepts"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "vocabulary", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              div(style = "width:20px;"),
              make_dropdown(i18n = i18n, ns = ns, label = "columns", id = "vocabulary_table_cols", width = "300px", multiSelect = TRUE,
                options = list(
                  list(key = 1, text = i18n$t("concept_id_1")),
                  list(key = 2, text = i18n$t("concept_name_1")),
                  list(key = 3, text = i18n$t("concept_display_name_1")),
                  list(key = 4, text = i18n$t("relationship_id")),
                  list(key = 5, text = i18n$t("concept_id_2")),
                  list(key = 6, text = i18n$t("concept_name_2")),
                  list(key = 7, text = i18n$t("domain_id")),
                  list(key = 9, text = i18n$t("concept_class_id")),
                  list(key = 10, text = i18n$t("standard_concept")),
                  list(key = 11, text = i18n$t("concept_code")),
                  list(key = 12, text = i18n$t("valid_start_date")),
                  list(key = 13, text = i18n$t("valid_end_date")),
                  list(key = 14, text = i18n$t("invalid_reason")),
                  list(key = 15, text = i18n$t("num_rows")),
                  list(key = 16, text = i18n$t("num_patients"))
                ),
                value = c(1, 2, 3, 15, 16)
              ),
              div(style = "width:10px;"),
              div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_mapped_concepts"), value = FALSE), style = "margin-top:45px;"),
              div(i18n$t("show_mapped_concepts"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
            ),
            conditionalPanel(
              condition = "input.vocabulary_concepts_pivot == null | input.vocabulary_concepts_pivot == 'vocabulary_concepts_table_view'", ns = ns,
              DT::DTOutput(ns("vocabulary_concepts")),
              conditionalPanel("input.vocabulary == null", ns = ns, br()),
              conditionalPanel(
                condition = "input.vocabulary != null", ns = ns,
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::PrimaryButton.shinyInput(ns("save_vocabulary_concepts"), i18n$t("save")),
                  shiny.fluent::DefaultButton.shinyInput(ns("reload_vocabulary_concepts_cache"), i18n$t("reload_cache"))
                ), br(),
                div(
                  div(uiOutput(ns("vocabulary_datatable_selected_item")), style = "display:relative; float:left; width:50%;"),
                  div(
                    div(shiny.fluent::Dropdown.shinyInput(ns("vocabulary_datatable_selected_item_plot_variable")), style = "width:300px; margin-left:42px;"),
                    plotly::plotlyOutput(ns("vocabulary_datatable_selected_item_plot"), height = "280px"), 
                    style = "display:relative; float:right; width:50%;"
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # --- --- --- -- --
    # Mapping card ----
    # --- --- --- -- --
    
    shinyjs::hidden(
      div(
        id = ns("vocabularies_mapping_card"),
        make_card(i18n$t("concepts_mapping"),
          div(
            shiny.fluent::reactOutput(ns("mappings_delete_confirm")),
            div(
              shiny.fluent::Pivot(
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-mapping_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "vocabularies_mapping_add", itemKey = "vocabularies_mapping_add", headerText = i18n$t("add")),
                shiny.fluent::PivotItem(id = "vocabularies_mapping_management", itemKey = "vocabularies_mapping_management", headerText = i18n$t("evaluate_and_edit"))
              )
            ),
            conditionalPanel(condition = "input.mapping_current_tab == null || input.mapping_current_tab == 'vocabularies_mapping_add'", ns = ns,
              div(
                div(
                  div(
                    make_combobox(i18n = i18n, ns = ns, label = "vocabulary1", id = "vocabulary_mapping1", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    DT::DTOutput(ns("vocabulary_mapping1_dt"))
                  ),
                  div(
                    make_combobox(i18n = i18n, ns = ns, label = "vocabulary2", id = "vocabulary_mapping2", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    DT::DTOutput(ns("vocabulary_mapping2_dt"))
                  ),
                  style = "width:100%; display:grid; grid-template-columns:1fr 1fr; grid-gap:20px;"
                ), br(),
                conditionalPanel(condition = "input.vocabulary_mapping1 != null && input.vocabulary_mapping2 != null", ns = ns, 
                  br(),
                  div(
                    div(uiOutput(ns("vocabulary_selected_item_mapping1")), style = "border:dashed 1px; padding:10px;"),
                    div(
                      make_dropdown(i18n = i18n, ns = ns, label = "item1_is", id = "mapping_type", width = "300px", multiSelect = FALSE,
                        options = list(
                          list(key = 1, text = i18n$t("equivalent_to")),
                          list(key = 2, text = i18n$t("included_in")),
                          list(key = 3, text = i18n$t("include"))
                        ),
                        value = 1),
                      br(), div(i18n$t("to_item2"), style = "font-weight:bold;"), br(),
                      shiny.fluent::PrimaryButton.shinyInput(ns("add_mapping"), i18n$t("add"))
                    ),
                    div(uiOutput(ns("thesaurus_selected_item_mapping2")), style = "border:dashed 1px; padding:10px;"),
                    style = "width:100%; display:grid; grid-template-columns:2fr 1fr 2fr; grid-gap:20px;"
                  ), br(),
                  DT::DTOutput(ns("thesaurus_added_mappings"))
                )
              )    
            ),
            conditionalPanel(condition = "input.mapping_current_tab == 'thesaurus_mapping_management'", ns = ns,
              div(DT::DTOutput(ns("thesaurus_evaluate_mappings")), style = "z-index:2"),
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::PrimaryButton.shinyInput(ns("save_mappings_evaluation"), i18n$t("save")),
                  shiny.fluent::DefaultButton.shinyInput(ns("mapping_delete_selection"), i18n$t("delete_selection"))
                ),
                style = "position:relative; z-index:1; margin-top:-30px; width:500px;"), br(),
              div(verbatimTextOutput(ns("thesaurus_mapping_details")), style = "border:dashed 1px; padding:10px;"), br(),
            )
          )
        )
      )
    ), br()
  )
}

#' thesaurus Server Functions
#'
#' @noRd 
mod_vocabularies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_vocabularies - start"))
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("vocabularies_concepts_card", "vocabularies_mapping_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_vocabularies_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_vocabularies_open_panel <- FALSE)
    
    r$help_vocabularies_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_vocabularies_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_vocabularies_open_modal <- FALSE
      r$help_vocabularies_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r$help_vocabularies_open_panel <- FALSE
      r$help_vocabularies_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_vocabularies_page_", i)]] <- Sys.time())
    })
    
    help_vocabularies(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- -- -
    # Thesaurus items ----
    # --- --- --- --- --- -- -
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$selected_dataset 2"))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("vocabularies_concepts_card" %in% r$user_accesses) shinyjs::show("vocabularies_concepts_card")
        else shinyjs::show("vocabularies_concepts_card_forbidden")
      }
      
      data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)
      
      # Multiple cases
      # Only one ID, so it's the beginning and the end
      # Last ID, so it's the end
      # ID between begin and last, so separated by commas
      r$dataset_vocabularies <- r$vocabulary %>% 
        dplyr::filter(
          grepl(paste0("^", data_source, "$"), data_source_id) | 
            grepl(paste0(", ", data_source, "$"), data_source_id) | 
            grepl(paste0("^", data_source, ","), data_source_id) |
            grepl(paste0(", ", data_source, ","), data_source_id)
        ) %>% dplyr::arrange(vocabulary_name)
      vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "id", text_col = "vocabulary_name", i18n = i18n)
      
      for (var in c("vocabulary", "vocabulary_mapping1", "vocabulary_mapping2")) shiny.fluent::updateComboBox.shinyInput(session, var, options = vocabulary_options, value = NULL)
      
      r$load_dataset_all_concepts <- Sys.time()
      
      # Reset UI of selected item
      output$vocabulary_datatable_selected_item <- renderUI("")
      output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(plotly::plotly_empty())
      shinyjs::hide("vocabulary_datatable_selected_item_plot_variable")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$selected_dataset 2"))
    })
    
    # Load all concepts
    
    observeEvent(r$load_dataset_all_concepts, {
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$load_dataset_all_concepts"))
      
      # Load csv file if it exists
      
      dataset_all_concepts_filename <- paste0(r$app_folder, "/datasets/", r$selected_dataset, "/dataset_all_concepts.csv")
      
      if (file.exists(dataset_all_concepts_filename)) r$dataset_all_concepts <- vroom::vroom(dataset_all_concepts_filename, col_types = "iicccicccccccccii", progress = FALSE)
      
      # Create csv file if it doesn't exist
      
      if (!file.exists(dataset_all_concepts_filename)){
        
        # Load all concepts for this dataset, with rows count
        
        sql <- glue::glue_sql(paste0(
          "SELECT * ",
          "FROM concept ",
          "WHERE vocabulary_id IN ({r$dataset_vocabularies %>% dplyr::pull(vocabulary_id)*}) ",
          "ORDER BY concept_id"), .con = m$db)
        r$dataset_all_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate(concept_display_name = NA_character_, .after = "concept_name")
        
        # Count rows
        
        omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
        
        count_rows <- tibble::tibble()
        secondary_concepts_count_rows <- tibble::tibble()
        
        tables <- c("person", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
          "measurement", "observation", "specimen", "drug_era", "dose_era", "condition_era", "note", "specimen")
        if (omop_version %in% c("5.3", "5.0")) tables <- c(tables, "death")
        
        main_cols <- c(
          "condition_occurrence" = "condition",
          "drug_exposure" = "drug",
          "procedure_occurrence" = "procedure",
          "device_exposure" = "device",
          "measurement" = "measurement",
          "observation" = "observation",
          "specimen" = "specimen",
          "drug_era" = "drug",
          "dose_era" = "drug",
          "condition_era" = "condition"
        )
        
        secondary_cols <- list(
          "person" = c("gender", "race", "ethnicity"),
          "condition_occurrence" = c("condition_type", "condition_status"),
          "drug_exposure" = c("drug_type", "route"),
          "procedure_occurrence" = "procedure_type",
          "device_exposure" = c("device_type"),
          "measurement" = c("measurement_type", "value_as", "unit"),
          "observation" = c("observation_type", "qualifier", "value_as", "unit"),
          "note" = c("note_type", "note_class", "encoding", "language"),
          "specimen" = c("specimen_type", "unit", "anatomic_site", "disease_status"),
          "dose_era" = "unit"
        )
        
        if (omop_version == "5.3") secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("admitting_source", "discharge_to", "visit", "visit_type"))
        else if (omop_version %in% c("5.4", "6.0")) secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("admitted_from", "discharge_to", "visit", "visit_type"))
        
        if (omop_version %in% c("5.3", "5.0")) secondary_cols <- rlist::list.append(secondary_cols, "death" = c("death_type", "cause"))
        
        for(table in tables){
          print(table)
          if (nrow(d[[table]]) > 0){
            
            if (table %in% names(main_cols)) count_rows <- 
              count_rows %>% 
              dplyr::bind_rows(
                d[[table]] %>% 
                  dplyr::group_by_at(paste0(main_cols[[table]], "_concept_id")) %>%
                  dplyr::summarize(count_persons_rows = dplyr::n_distinct(person_id), count_concepts_rows = dplyr::n(), count_secondary_concepts_rows = 0L) %>% 
                  dplyr::ungroup() %>% 
                  dplyr::rename(concept_id = paste0(main_cols[[table]], "_concept_id"))
              )
            
            if (table %in% names(secondary_cols)){
              for (col in secondary_cols[[table]]){
                count_rows <- 
                  count_rows %>%
                  dplyr::bind_rows(
                    d[[table]] %>% 
                      dplyr::group_by_at(paste0(col, "_concept_id")) %>%
                      dplyr::summarize(count_persons_rows = 0L, count_concepts_rows = 0L, count_secondary_concepts_rows = dplyr::n()) %>% 
                      dplyr::ungroup() %>% 
                      dplyr::rename(concept_id = paste0(col, "_concept_id"))
                  )
              } 
            }
          }
        }
        
        # Merge count_rows, transform count_rows cols to integer, to be sortable
        if (nrow(count_rows) > 0) r$dataset_all_concepts <- r$dataset_all_concepts %>% 
          dplyr::left_join(count_rows, by = "concept_id") %>%
          dplyr::mutate_at(c("count_concepts_rows", "count_persons_rows", "count_secondary_concepts_rows"), as.integer) %>%
          dplyr::filter(count_concepts_rows > 0 | count_secondary_concepts_rows > 0)
        
        if (nrow(count_rows) == 0) r$dataset_all_concepts <- r$dataset_all_concepts %>% dplyr::slice(0)
        
        r$dataset_all_concepts <- r$dataset_all_concepts %>%
          dplyr::rename(concept_id_1 = concept_id, concept_name_1 = concept_name, concept_display_name_1 = concept_display_name) %>%
          dplyr::mutate(relationship_id = NA_character_, concept_id_2 = NA_integer_, concept_name_2 = NA_character_, .after = "concept_display_name_1")
        
        # Load r$concept & r$concept_relationship if not already loaded from mod_settings_data_management.R
        # Convert cols to char and arrange cols as done in mod_settings_data_management.R
        
        tables <- c("concept", "concept_relationship")
        
        cols_to_char <- list()
        cols_to_char$concept = "concept_id"
        cols_to_char$relationship = "relationship_concept_id"
        
        cols_order <- list()
        cols_order$concept <- "concept_id"
        cols_order$concept_relationship <- "concept_id_1"
        
        for (table in tables){
          if (length(r$table) == 0){
            sql <- glue::glue_sql("SELECT * FROM {`table`}", .con = m$db)
            r[[table]] <- DBI::dbGetQuery(m$db, sql) %>%
              dplyr::arrange(cols_order[[table]]) %>%
              dplyr::mutate_at(cols_to_char[[table]], as.character) %>%
              dplyr::mutate(modified = FALSE)
          }
        }
        
        # Merge mapped concepts
        
        if (nrow(r$concept_relationship) > 0 & nrow(r$concept) > 0 & nrow(r$dataset_all_concepts) > 0){
          r$dataset_all_concepts <- r$dataset_all_concepts %>%
            dplyr::bind_rows(
              r$dataset_all_concepts %>%
                dplyr::select(-relationship_id, -concept_id_2, -concept_name_2, -concept_display_name_1) %>%
                dplyr::rename(concept_id_2 = concept_id_1, concept_name_2 = concept_name_1) %>%
                dplyr::left_join(
                  r$concept_relationship %>% 
                    dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
                    dplyr::select(concept_id_1, concept_id_2, relationship_id),
                  by = "concept_id_2"
                ) %>%
                dplyr::filter(concept_id_1 != concept_id_2) %>%
                dplyr::left_join(
                  r$concept %>%
                    dplyr::mutate_at("concept_id", as.integer) %>%
                    dplyr::select(concept_id_1 = concept_id, concept_name_1 = concept_name),
                  by = "concept_id_1"
                ) %>% 
                dplyr::arrange(dplyr::desc(count_concepts_rows)) %>%
                dplyr::mutate(concept_display_name_1 = NA_character_, .after = "concept_name_1")
            )
          
          readr::write_csv(r$dataset_all_concepts, dataset_all_concepts_filename, progress = FALSE)
        }
      }
      
      # Get user's modifications on items names & concept_display_names
      
      sql <- glue::glue_sql(paste0(
        "SELECT id, concept_id, concept_name, concept_display_name ",
        "FROM concept_user ",
        "WHERE user_id = {r$user_id} AND vocabulary_id IN ({r$dataset_vocabularies %>% dplyr::pull(vocabulary_id)*})"), .con = m$db)
      dataset_user_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
      
      # Merge tibbles
      if (nrow(dataset_user_concepts) > 0) r$dataset_all_concepts <-
        r$dataset_all_concepts %>%
        dplyr::left_join(
          dataset_user_concepts %>% dplyr::select(concept_id_1 = concept_id, new_concept_name_1 = concept_name, new_concept_display_name_1 = concept_display_name),
          by = "concept_id_1"
        ) %>%
        dplyr::mutate(
          concept_name_1 = dplyr::case_when(!is.na(new_concept_name_1) ~ new_concept_name_1, TRUE ~ concept_name_1),
          concept_display_name_1 = dplyr::case_when(!is.na(new_concept_display_name_1) ~ new_concept_display_name_1, TRUE ~ concept_display_name_1)
        ) %>%
        dplyr::left_join(
          dataset_user_concepts %>% dplyr::select(concept_id_2 = concept_id, new_concept_name_2 = concept_name),
          by = "concept_id_2"
        ) %>%
        dplyr::mutate(
          concept_name_2 = dplyr::case_when(!is.na(new_concept_name_2) ~ new_concept_name_2, TRUE ~ concept_name_2)
        ) %>%
        dplyr::select(-new_concept_name_1, -new_concept_display_name_1, -new_concept_name_2)

      if (length(r$dataset_vocabulary_concepts_datatable_proxy) > 0) DT::replaceData(r$dataset_vocabulary_concepts_datatable_proxy,
        r$dataset_vocabulary_concepts %>% dplyr::slice(0), resetPaging = FALSE, rownames = FALSE)
      
      # Update vocabulary dropdown
      shiny.fluent::updateComboBox.shinyInput(session, "vocabulary", 
        options = convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "id", text_col = "vocabulary_name", i18n = i18n), value = NULL)
      
      # Join d$person, d$visit_occurrence & d$visit_detail with r$dataset_all_concepts
      
      r$merge_concepts_and_d_vars <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$load_dataset_all_concepts"))
    })
    
    # Join concepts cols of d$ vars with r$dataset_all_concepts
    
    observeEvent(r$merge_concepts_and_d_vars, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$merge_concepts_and_d_vars"))
      
      req(nrow(d$person) > 0)
      
      omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
      
      # Don't reload if already done
      
      if ("gender_concept_name" %not_in% colnames(d$person)){
        
        cols <- list(
          "person" = c("gender", "race", "ethnicity"),
          "condition_occurrence" = c("condition_type", "condition_status"),
          "drug_exposure" = c("drug_type", "route"),
          "procedure_occurrence" = "procedure_type",
          "device_exposure" = c("device_type"),
          "measurement" = c("measurement_type", "value_as", "unit"),
          "observation" = c("observation_type", "qualifier", "value_as", "unit"),
          "note" = c("note_type", "note_class", "encoding", "language"),
          "specimen" = c("specimen_type", "unit", "anatomic_site", "disease_status"),
          "dose_era" = "unit"
        )
        
        if (omop_version == "5.3") cols <- rlist::list.append(cols, "visit_occurrence" = c("admitting_source", "discharge_to", "visit", "visit_type"))
        else if (omop_version %in% c("5.4", "6.0")) cols <- rlist::list.append(cols, "visit_occurrence" = c("admitted_from", "discharge_to", "visit", "visit_type"))
        
        if (omop_version %in% c("5.3", "5.0")) cols <- rlist::list.append(cols, "death" = c("death_type", "cause"))
        
        for (table in names(cols)){
          table_cols <- cols[[table]]
          for (col in table_cols){
            if (nrow(d[[table]]) > 0){
              if (grepl("unit", col)) merge_col <- c("concept_code", "concept_code") else merge_col <- c("concept_name", "concept_name_1")
              
              d[[table]] <- d[[table]] %>%
                dplyr::left_join(
                  r$dataset_all_concepts %>% dplyr::select(!!paste0(col, "_concept_id") := concept_id_1, !!paste0(col, "_", merge_col[1]) := !!merge_col[2]), by = paste0(col, "_concept_id")
                ) %>%
                dplyr::relocate(!!paste0(col, "_", merge_col[1]), .after = !!paste0(col, "_concept_id"))
            } 
          }
        }
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$merge_concepts_and_d_vars"))
    })
    
    # When a vocabulary is selected, filter on it
    
    observeEvent(input$vocabulary, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary"))
      
      r$reload_vocabulary_datatable <- Sys.time()
    })
    
    # Update which cols are hidden
    observeEvent(input$vocabulary_table_cols, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_table_cols"))
      
      req(length(r$dataset_vocabulary_concepts_datatable_proxy) > 0)
      
      r$dataset_vocabulary_concepts_datatable_proxy %>%
        DT::showCols(1:17) %>%
        DT::hideCols(setdiff(1:17, input$vocabulary_table_cols))
    })
    
    # Show mapped concepts in the datatable
    
    observeEvent(input$vocabulary_show_mapped_concepts, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_show_mapped_concepts"))
      
      req(length(input$vocabulary) > 0)
      
      r$reload_vocabulary_datatable <- Sys.time()
    })
    
    # Reload datatable
    
    observeEvent(r$reload_vocabulary_datatable, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$reload_vocabulary_datatable"))
      
      # Reset row details
      output$vocabulary_datatable_selected_item <- renderUI("")
      output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(plotly::plotly_empty())
      shinyjs::hide("vocabulary_datatable_selected_item_plot_variable")
      
      vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input$vocabulary$key) %>% dplyr::pull(vocabulary_id)
      
      # Filter only used concepts in d vars
      r$dataset_vocabulary_concepts <- r$dataset_all_concepts %>% dplyr::filter(count_concepts_rows > 0) %>% dplyr::select(-count_secondary_concepts_rows)
      
      if (input$vocabulary_show_mapped_concepts) r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>% dplyr::filter(vocabulary_id == !!vocabulary_id)
      else r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>% dplyr::filter(vocabulary_id == !!vocabulary_id, is.na(relationship_id))
      
      r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("concept_id_1", as.character)
      
      # if (length(r$dataset_vocabulary_concepts_datatable_proxy) == 0){
        editable_cols <- c("concept_name_1", "concept_display_name_1")
        searchable_cols <- c("concept_id_1", "concept_name_1", "relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id")
        column_widths <- c("count_persons_rows" = "80px", "count_concepts_rows" = "80px")
        sortable_cols <- c("concept_id_1", "concept_name_1", "relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id", "count_persons_rows", "count_concepts_rows")
        factorize_cols <- c("relationship_id", "domain_id", "concept_class_id", "standard_concept", "invalid_reason")
        centered_cols <- c("concept_id_1", "relationship_id", "concept_id_2", "domain_id", "count_persons_rows", "count_concepts_rows")
        col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_with_counts", i18n = i18n)
        hidden_cols <- c("id", "concept_id_2", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", 
          "valid_start_date", "valid_end_date", "invalid_reason", "modified")
        value_show_cols <- c(1, 2, 3, 4, 6, 15, 16)
        
        if (!input$vocabulary_show_mapped_concepts){
          hidden_cols <- c(hidden_cols, "relationship_id", "concept_name_2")
          value_show_cols <- c(1, 2, 3, 15, 16)
        }
        
        shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_table_cols", value = value_show_cols)
        
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$dataset_vocabulary_concepts,
          output_name = "vocabulary_concepts", col_names = col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, factorize_cols = factorize_cols)
        
        # Create a proxy for datatatable
        r$dataset_vocabulary_concepts_datatable_proxy <- DT::dataTableProxy("vocabulary_concepts", deferUntilFlush = FALSE)
      # }
      
      # else DT::replaceData(r$dataset_vocabulary_concepts_datatable_proxy, r$dataset_vocabulary_concepts, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Reload cache
    
    observeEvent(input$reload_vocabulary_concepts_cache, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$reload_vocabulary_concepts_cache"))
      
      r$dataset_all_concepts_reload_cache_open_dialog <- TRUE
    })
    
    r$dataset_all_concepts_reload_cache_open_dialog <- FALSE
    
    output$dataset_all_concepts_reload_cache <- shiny.fluent::renderReact({
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - output$dataset_all_concepts_reload_cache"))
      
      shiny.fluent::Dialog(
        hidden = !r$dataset_all_concepts_reload_cache_open_dialog,
        onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('vdataset_all_concepts_reload_cache_hide_dialog', Math.random()); }")),
        dialogContentProps = list(
          type = 0,
          title = i18n$t("dataset_all_concepts_reload_cache"),
          closeButtonAriaLabel = "Close",
          subText = tagList( i18n$t("dataset_all_concepts_reload_cache_subtext"), br(), br())
        ),
        shiny.fluent::DialogFooter(
          shiny.fluent::PrimaryButton.shinyInput(ns("dataset_all_concepts_reload_cache_confirmed"), text = i18n$t("yes")),
          shiny.fluent::DefaultButton.shinyInput(ns("dataset_all_concepts_reload_cache_canceled"), text = i18n$t("no"))
        )
      )
    })
    
    observeEvent(input$dataset_all_concepts_reload_cache_canceled, r$dataset_all_concepts_reload_cache_open_dialog <- FALSE)
    
    observeEvent(input$dataset_all_concepts_reload_cache_confirmed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$dataset_all_concepts_reload_cache_confirmed"))
      
      # Close dialog box
      r$dataset_all_concepts_reload_cache_open_dialog <- FALSE
      
      file.remove(paste0(r$app_folder, "/datasets/", r$selected_dataset, "/dataset_all_concepts.csv"))
      
      r$load_dataset_all_concepts <- Sys.time()
      
      show_message_bar(output, "cache_reloaded", type = "success", i18n = i18n, ns = ns)
    })
    
    # Updates on datatable data
    observeEvent(input$vocabulary_concepts_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_concepts_cell_edit"))
      
      edit_info <- input$vocabulary_concepts_cell_edit
      r$dataset_vocabulary_concepts <- DT::editData(r$dataset_vocabulary_concepts, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$dataset_vocabulary_concepts[[edit_info$row, "modified"]] <- TRUE
      concept_id <- r$dataset_vocabulary_concepts[[edit_info$row, "concept_id_1"]]
      
      # Save this update in r$dataset_all_concepts also
      r$dataset_all_concepts <- r$dataset_all_concepts %>% dplyr::mutate(
        concept_name_1 = dplyr::case_when(
          concept_id_1 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_name_1"]],
          TRUE ~ concept_name_1),
        concept_display_name_1 = dplyr::case_when(
          concept_id_1 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_display_name_1"]],
          TRUE ~ concept_display_name_1),
        concept_name_2 = dplyr::case_when(
          concept_id_2 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_name_1"]],
          TRUE ~ concept_name_2)
      )
    })
    
    # Save updates
    observeEvent(input$save_vocabulary_concepts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$save_vocabulary_concepts"))
      
      vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input$vocabulary$key) %>% dplyr::pull(vocabulary_id)
      
      r$dataset_vocabulary_concept_user <- 
        r$dataset_vocabulary_concepts %>%
        dplyr::filter(modified) %>%
        dplyr::transmute(user_id = r$user_id, concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1, vocabulary_id = !!vocabulary_id)
      r$dataset_vocabulary_concept_user_temp <- r$dataset_vocabulary_concept_user %>% dplyr::mutate(modified = TRUE)
      
      save_settings_datatable_updates(output = output, r = r, m = m, ns = ns, table = "concept_user", r_table = "dataset_vocabulary_concept_user", duplicates_allowed = TRUE, i18n = i18n)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$save_vocabulary_concepts"))
    })
    
    vocabulary_concepts_row_details <- tibble::tribble(
      ~domain_id, ~table, ~concept_id, ~cols,
      "Measurement", "measurement", "measurement_concept_id", list("value_as_number" = "values_as_number", "value_as_concept_id" = "values_as_concept_id"),
      "Observation", "observation", "observation_concept_id", list("value_as_number" = "values_as_number", "value_as_string" = "values_as_string", "value_as_concept_id" = "values_as_concept_id"),
      "Drug", "drug_exposure", "drug_concept_id", list("quantity" = "quantities"),
      "Procedure", "procedure_occurrence", "procedure_concept_id", list("quantity" = "quantities"),
      "Specimen", "specimen", "specimen_concept_id", list("quantity" = "quantities")
    )
    
    # When a row is selected
    observeEvent(input$vocabulary_concepts_rows_selected, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_concepts_rows_selected"))
      
      selected_concept <- r$dataset_vocabulary_concepts[input$vocabulary_concepts_rows_selected, ]
      r$vocabulary_selected_concept <- selected_concept
      
      concept_values <- tagList()
      
      # for (i in 1:nrow(vocabulary_concepts_row_details)){
      row <- vocabulary_concepts_row_details %>% dplyr::filter(domain_id == selected_concept$domain_id)
        # if (selected_concept$domain_id == row$domain_id){
      if (nrow(d[[row$table]]) > 0){
        if (!is.na(selected_concept$concept_id_2)) concept_id <- selected_concept$concept_id_2
        else concept_id <- selected_concept$concept_id_1
        
        values <- d[[row$table]] %>% dplyr::filter(get(row$concept_id) == !!concept_id)
        
        if (nrow(values) > 0){
          
          values <- values %>% dplyr::slice_sample(n = 5, replace = TRUE)
          
          cols <- row$cols %>% unlist()
          
          selected_item_plot_variable_options <- list()
          
          i <- FALSE
          
          for (name in names(cols)){
            col <- cols[[name]]
            
            selected_item_plot_variable_options <- rlist::list.append(selected_item_plot_variable_options, list(key = name, text = i18n$t(col)))
            if (!i) selected_item_plot_variable_value <- name
            
            if (nrow(values %>% dplyr::filter(!is.na(get(name)))) == 0) concept_values <- tagList(concept_values, br(), strong(i18n$t(col)), " : /")
            else {
              if (name == "value_as_number") concept_values <- tagList(concept_values, br(),
                strong(i18n$t("values_as_number")), " : ", values %>% 
                  dplyr::mutate(result = paste0(value_as_number, " ", unit_concept_code)) %>% dplyr::pull(result) %>% toString())
              
              else if (name == "value_as_concept_id") concept_values <- tagList(concept_values, br(),
                strong(i18n$t("values_as_concept_id")), " : ", values %>% dplyr::slice_sample(n = 5, replace = TRUE) %>% 
                  dplyr::left_join(
                    r$dataset_all_concepts %>% dplyr::select(value_as_concept_id = concept_id, value_as_concept_name = concept_name),
                    by = "value_as_concept_id"
                  ) %>% dplyr::pull(value_as_concept_name) %>% toString())
              
              else concept_values <- tagList(concept_values, br(),
                strong(i18n$t(col)), " : ", values %>% dplyr::slice_sample(n = 5, replace = TRUE) %>% 
                  dplyr::pull(name) %>% toString())
            }
            
            i <- TRUE
          }
          
          # Update dropdown for plotly var
          shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_datatable_selected_item_plot_variable",
            options = selected_item_plot_variable_options, value = selected_item_plot_variable_value)
          
          shinyjs::show("vocabulary_datatable_selected_item_plot_variable")
        }
      }
        # }
      # }
      
      concept_info <- tagList(
        strong(i18n$t("concept_id")), " : ", selected_concept$concept_id_1, br(),
        strong(i18n$t("concept_name")), " : ", selected_concept$concept_name_1, br(),
        strong(i18n$t("concept_display_name")), " : ", selected_concept$concept_display_name_1, br(),
        strong(i18n$t("domain_id")), " : ", selected_concept$domain_id, br(),
        strong(i18n$t("standard_concept")), " : ", selected_concept$standard_concept, br(),
        strong(i18n$t("concept_code")), " : ", selected_concept$concept_code, br(),
        strong(i18n$t("valid_start_date")), " : ", selected_concept$valid_start_date, br(),
        strong(i18n$t("valid_end_date")), " : ", selected_concept$valid_end_date, br(),
        strong(i18n$t("invalid_reason")), " : ", selected_concept$invalid_reason, br(),
        concept_values,
        br(), br()
      )
      
      output$vocabulary_datatable_selected_item <- renderUI(concept_info)
    })
    
    # Update plotly
    
    observeEvent(input$vocabulary_datatable_selected_item_plot_variable, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_datatable_selected_item_plot_variable"))
      
      # if (selected_concept$domain_id == "Measurement"){

      concept_plot <- d$measurement %>% dplyr::filter(measurement_concept_id == selected_concept$concept_id_1) %>%
        ggplot2::ggplot(ggplot2::aes(x = value_as_number, y = 100 * ..count.. / sum(..count..),
          text = paste(i18n$t("value"), " : ", value_as_number))) +
        ggplot2::geom_histogram(fill = "#4F86C6") +
        ggplot2::labs(x = selected_concept$concept_name_1, y = "Propotion (%)") +
        ggplot2::theme(axis.title = ggplot2::element_text(size = 10), axis.text = ggplot2::element_text(size = 10))

      output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(
        plotly::ggplotly(concept_plot, tooltip = "text") %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::style(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
        plotly::layout(xaxis = list(tickfont = list(size = 12)), yaxis = list(tickfont = list(size = 12)))
        )
      # }
    })
    
    # --- --- --- --- --
    # Items mapping ----
    # --- --- --- --- --
    
    # --- --- --- --- --- --
    ## Create a mapping ----
    # --- --- --- --- --- --
    
    observeEvent(input$vocabulary_mapping1, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_mapping1"))
      r$vocabulary_mapping_reload <- paste0(Sys.time(), "_mapping1")
    })
    observeEvent(input$vocabulary_mapping2, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_mapping2"))
      r$vocabulary_mapping_reload <- paste0(Sys.time(), "_mapping2")
    })
    
    observeEvent(r$vocabulary_mapping_reload, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$vocabulary_mapping_reload"))
      
      if (grepl("mapping1", r$vocabulary_mapping_reload)) mapping <- "mapping1"
      else if (grepl("mapping2", r$vocabulary_mapping_reload)) mapping <- "mapping2"
      
      req(length(input[[paste0("vocabulary_", mapping)]]$key) > 0)
      
      vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input[[paste0("vocabulary_", mapping)]]$key) %>% dplyr::pull(vocabulary_id)
      
      sql <- glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = {vocabulary_id} ORDER BY concept_id", .con = m$db)
      r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- DBI::dbGetQuery(m$db, sql)
      
      # Merge count_rows from r$dataset_all_concepts
      # Convert concept_id to character to be sortable
      r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- 
        r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>%
        dplyr::left_join(
          r$dataset_all_concepts %>% dplyr::select(concept_id = concept_id_1, count_persons_rows, count_concepts_rows),
          by = "concept_id"
        ) %>%
        dplyr::mutate_at("concept_id", as.character)
      
      searchable_cols <- c("concept_id", "concept_name")
      column_widths <- c("count_persons_rows" = "80px", "count_concepts_rows" = "80px")
      sortable_cols <- c("concept_id", "concept_name", "count_persons_rows", "count_concepts_rows")
      centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows")
      col_names <- get_col_names(table_name = "mapping_vocabulary_concepts_with_counts", i18n = i18n)
      hidden_cols <- c("id", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
        "valid_start_date", "valid_end_date", "invalid_reason")

      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0("dataset_vocabulary_concepts_", mapping)]],
        output_name = paste0("vocabulary_", mapping, "_dt"), col_names = col_names, datatable_dom = "<'top't><'bottom'p>",
        sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$vocabulary_mapping_reload"))
    })
    
    # When a row is selected
    observeEvent(input$vocabulary_mapping1_dt_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_mapping1_dt_rows_selected"))
      r$vocabulary_mapping_item_info <- paste0(Sys.time(), "_mapping1")
    })
    observeEvent(input$vocabulary_mapping2_dt_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_mapping2_dt_rows_selected"))
      r$vocabulary_mapping_item_info <- paste0(Sys.time(), "_mapping2")
    })
    
    observeEvent(r$vocabulary_mapping_item_info, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$vocabulary_mapping_item_info"))
      
      if (grepl("mapping1", r$vocabulary_mapping_item_info)) mapping <- "mapping1"
      else if (grepl("mapping2", r$vocabulary_mapping_item_info)) mapping <- "mapping2"
      
      style <- "display:inline-block; width:200px; font-weight:bold;"
      
      concept <- r[[paste0("dataset_vocabulary_concepts_", mapping)]][input[[paste0("vocabulary_", mapping, "_dt_rows_selected")]], ] %>% 
        dplyr::mutate_at("concept_id", as.integer)
      
      # output[[paste0("thesaurus_selected_item_", mapping)]] <- renderUI(tagList(div(
      #   span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
      #   span(i18n$t("concept_id"), style = style), thesaurus_item$concept_id, br(),
      #   span(i18n$t("name"), style = style), thesaurus_item$name, br(),
      #   span(i18n$t("concept_display_name"), style = style), thesaurus_item$display_name, br(),
      #   span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
      #   values_text
      # )))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$vocabulary_mapping_item_info"))
    })
    
    # When a mapping id added
    
    observeEvent(input$add_mapping, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$add_mapping"))
      
      req(length(input$vocabulary_mapping1_dt_rows_selected) > 0)
      req(length(input$vocabulary_mapping2_dt_rows_selected) > 0)
      req(nrow(r$dataset_vocabulary_concepts_mapping1_temp[input$vocabulary_mapping1_dt_rows_selected, ]) > 0)
      req(nrow(r$dataset_vocabulary_concepts_mapping2_temp[input$vocabulary_mapping2_dt_rows_selected, ]) > 0)
      
      item_1 <- r$dataset_vocabulary_concepts_mapping1_temp[input$vocabulary_mapping1_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
      item_2 <- r$dataset_vocabulary_concepts_mapping2_temp[input$vocabulary_mapping2_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
      
      # Check if mapping already added in database
      
      check_duplicates <- FALSE
      
      sql <- glue::glue_sql(paste0("SELECT * FROM vocabulary_concepts_mapping WHERE ",
        "thesaurus_id_1 = {item_1$thesaurus_id} AND concept_id_1 = {item_1$concept_id} AND ",
        "thesaurus_id_2 = {item_2$thesaurus_id} AND concept_id_2 = {item_2$concept_id} AND ",
        "relation_id = {as.integer(input$mapping_type)} AND ",
        "category = 'user_added_mapping' AND deleted IS FALSE"), .con = r$db)
      existing_mapping <- DBI::dbGetQuery(r$db, sql)
      
      if (nrow(existing_mapping) > 0) show_message_bar(output,  "thesaurus_mapping_already_exists", "severeWarning", i18n, ns = ns)
      req(nrow(existing_mapping) == 0)
      
      if (item_1$thesaurus_id == item_2$thesaurus_id & item_1$concept_id == item_2$concept_id) show_message_bar(output,  "thesaurus_mapping_same_items", "severeWarning", i18n, ns = ns)
      req(item_1$thesaurus_id != item_2$thesaurus_id | item_1$concept_id != item_2$concept_id)
      
      last_row <- get_last_row(r$db, "vocabulary_concepts_mapping")
      
      # Add new mapping to r$thesaurus_added_mappings
      
      new_row <- tibble::tribble(~id, ~category, ~thesaurus_id_1, ~concept_id_1, ~thesaurus_id_2, ~concept_id_2, ~relation_id, ~creator_id, ~datetime, ~deleted,
        last_row + 1, "user_added_mapping", item_1$thesaurus_id, item_1$concept_id, item_2$thesaurus_id, item_2$concept_id,
        as.integer(input$mapping_type), r$user_id, as.character(Sys.time()), FALSE)
      
      r$thesaurus_added_mappings <- r$thesaurus_added_mappings %>% dplyr::bind_rows(new_row)
      
      # Add new mapping to database
      
      DBI::dbAppendTable(r$db, "vocabulary_concepts_mapping", new_row)
      
      # Notify user
      show_message_bar(output,  "thesaurus_mapping_added", "success", i18n, ns = ns)
      
      # Update datatables
      r$reload_thesaurus_added_mappings_datatable <- Sys.time()
      # r$reload_thesaurus_evaluate_mappings_datatable <- Sys.time()
      
      r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
        dplyr::bind_rows(
          tibble::tibble(
            id = last_row + 1,
            thesaurus_name_1 = r$thesaurus %>% dplyr::filter(id == item_1$thesaurus_id) %>% dplyr::pull(name),
            concept_id_1 = as.character(item_1$concept_id),
            relation = dplyr::case_when(as.integer(input$mapping_type) == 1 ~ i18n$t("equivalent_to"),
              as.integer(input$mapping_type) == 2 ~ i18n$t("included_in"), as.integer(input$mapping_type) == 3 ~ i18n$t("include")),
            thesaurus_name_2 = r$thesaurus %>% dplyr::filter(id == item_2$thesaurus_id) %>% dplyr::pull(name),
            concept_id_2 = as.character(item_2$concept_id),
            creator_name = r$users %>% dplyr::filter(id == r$user_id) %>%
              dplyr::mutate(creator_name = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator_name),
            datetime = as.character(Sys.time()),
            deleted = 0L,
            positive_evals = 0L,
            negative_evals = 0L,
            action = as.character(tagList(
              shiny::actionButton(paste0("positive_eval_", last_row + 1), "", icon = icon("thumbs-up"),
                onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_positive', this.id, {priority: 'event'})"),
                style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;"),
              shiny::actionButton(paste0("negative_eval_", last_row + 1), "", icon = icon("thumbs-down"),
                onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_negative', this.id, {priority: 'event'})"),
                style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;"),
              shiny::actionButton(paste0("remove_", last_row + 1), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_deleted_pressed', this.id, {priority: 'event'})"),
                style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;")
            )),
            user_evaluation_id = NA_integer_
          )
        ) %>% dplyr::mutate(modified = FALSE) %>%
        dplyr::arrange(dplyr::desc(id))
      
      DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$add_mapping"))
    })
    
    # Table to summarize added mappings
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$selected_dataset 1"))
      
      r$thesaurus_added_mappings <- tibble::tibble(id = integer(), category = character(), thesaurus_id_1 = integer(), concept_id_1 = integer(), 
        thesaurus_id_2 = integer(), concept_id_2 = integer(), relation_id = integer(), creator_id = integer(), datetime = character(), deleted = logical())
      
      output$thesaurus_selected_item_mapping1 <- renderText("")
      output$thesaurus_selected_item_mapping2 <- renderText("")
      
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "vocabulary_mapping1_dt", datatable_dom = "")
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "vocabulary_mapping2_dt", datatable_dom = "")
      
      r$reload_thesaurus_added_mappings_datatable <- Sys.time()
      r$reload_thesaurus_evaluate_mappings_datatable <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$selected_dataset 1"))
    })
    
    observeEvent(r$reload_thesaurus_added_mappings_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$reload_thesaurus_added_mappings_datatable"))
      
      r$thesaurus_added_mappings_temp <- r$thesaurus_added_mappings %>%
        dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character) %>%
        dplyr::mutate(relation = dplyr::case_when(relation_id == 1 ~ i18n$t("equivalent_to"), relation_id == 2 ~ i18n$t("included_in"), relation_id == 3 ~ i18n$t("include"))) %>%
        dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_1 = id, thesaurus_name_1 = name), by = "thesaurus_id_1") %>%
        dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_2 = id, thesaurus_name_2 = name), by = "thesaurus_id_2") %>%
        dplyr::relocate(thesaurus_name_1, .after = "thesaurus_id_1") %>%
        dplyr::relocate(thesaurus_name_2, .after = "thesaurus_id_2") %>%
        dplyr::select(-thesaurus_id_1, -thesaurus_id_2, -relation_id) %>%
        dplyr::relocate(relation, .after = concept_id_1) %>%
        dplyr::arrange(dplyr::desc(datetime))
      
      centered_cols <- c("id", "concept_id_1", "thesaurus_name_1", "concept_id_2", "thesaurus_name_2", "relation")
      col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_mapping", i18n = i18n)
      hidden_cols <- c("id", "creator_id", "datetime", "deleted", "category")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$thesaurus_added_mappings_temp, datatable_dom = "<'top't><'bottom'p>",
        output_name = "thesaurus_added_mappings", col_names = col_names, centered_cols = centered_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatatable
      r$thesaurus_added_mappings_datatable_proxy <- DT::dataTableProxy("thesaurus_added_mappings", deferUntilFlush = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$reload_thesaurus_added_mappings_datatable"))
    })
    
    # --- --- --- --- - --
    ## Manage mapping ----
    # --- --- --- --- - --
    
    # Reload datatable  
    
    # observeEvent(r$reload_thesaurus_evaluate_mappings_datatable, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$reload_thesaurus_evaluate_mappings_datatable"))
    #   
    #   # Get all items mappings
    #   
    #   data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)
    #   
    #   thesaurus_ids <- r$thesaurus %>% 
    #     dplyr::filter(
    #       grepl(paste0("^", data_source, "$"), data_source_id) | 
    #         grepl(paste0(", ", data_source, "$"), data_source_id) | 
    #         grepl(paste0("^", data_source, ","), data_source_id) |
    #         grepl(paste0(", ", data_source, ","), data_source_id)
    #     ) %>% dplyr::pull(id)
    #   
    #   sql <- glue::glue_sql(paste0("SELECT * FROM vocabulary_concepts_mapping WHERE (thesaurus_id_1 IN ({thesaurus_ids*}) OR thesaurus_id_2 IN ({thesaurus_ids*})) ",
    #     "AND category = 'user_added_mapping' AND deleted IS FALSE"), .con = r$db)
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- DBI::dbGetQuery(r$db, sql)
    #   
    #   action_col <- tibble::tibble()
    #   
    #   # Join with evaluations
    #   
    #   sql <- glue::glue_sql(paste0("SELECT * FROM vocabulary_concepts_mapping_evals WHERE mapping_id IN ({r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::pull(id)*}) ",
    #     "AND deleted IS FALSE"), .con = r$db)
    #   thesaurus_mapping_evals <- DBI::dbGetQuery(r$db, sql)
    #   
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::left_join(thesaurus_mapping_evals %>% dplyr::select(eval_id = id, id = mapping_id, evaluation_id), by = "id") %>%
    #     dplyr::group_by(id, thesaurus_id_1, concept_id_1, thesaurus_id_2, concept_id_2, relation_id, creator_id, datetime, deleted) %>%
    #     dplyr::summarize(
    #       positive_evals = sum(evaluation_id == 1, na.rm = TRUE),
    #       negative_evals = sum(evaluation_id == 2, na.rm = TRUE)
    #     ) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::mutate(
    #       positive_evals = ifelse(positive_evals > 0, positive_evals, 0),
    #       negative_evals = ifelse(negative_evals > 0, negative_evals, 0)
    #     ) %>%
    #     dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character) %>%
    #     dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, creator_name = paste0(firstname, " ", lastname)), by = "creator_id") %>%
    #     dplyr::relocate(creator_name, .after = "creator_id") %>%
    #     dplyr::select(-creator_id) %>%
    #     dplyr::left_join(thesaurus_mapping_evals %>% 
    #         dplyr::filter(creator_id == r$user_id) %>%
    #         dplyr::select(id = mapping_id, user_evaluation_id = evaluation_id), by = "id")
    #   
    #   # Create or get cache for action column
    #   tryCatch(action_col <- create_datatable_cache(output = output, r = r, i18n = i18n, tab_id = id, thesaurus_id = thesaurus_ids, category = "thumbs_and_delete"))
    #   
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::left_join(action_col %>% dplyr::select(id, action), by = "id") %>%
    #     dplyr::relocate(action, .after = "negative_evals")
    #   
    #   # Update action buttons with user evaluations
    #   
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::mutate(
    #       positive_eval_button_background_color = dplyr::case_when(
    #         user_evaluation_id == 1 ~ "#5FBAFF",
    #         user_evaluation_id == 2 ~ "#E8E9EC"
    #       ),
    #       positive_eval_button_color = dplyr::case_when(
    #         user_evaluation_id == 1 ~ "white",
    #         user_evaluation_id == 2 ~ "black"
    #       ),
    #       negative_eval_button_background_color = dplyr::case_when(
    #         user_evaluation_id == 1 ~ "#E8E9EC",
    #         user_evaluation_id == 2 ~ "#FF434C"
    #       ),
    #       negative_eval_button_color = dplyr::case_when(
    #         user_evaluation_id == 1 ~ "black",
    #         user_evaluation_id == 2 ~ "white"
    #       )
    #     ) %>%
    #     dplyr::rowwise() %>%
    #     dplyr::mutate(action = dplyr::case_when(
    #       !is.na(user_evaluation_id) ~ as.character(tagList(
    #         shiny::actionButton(paste0("positive_eval_", id), "", icon = icon("thumbs-up"),
    #           onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_positive', this.id, {priority: 'event'})"),
    #           style = paste0("background-color:", positive_eval_button_background_color, "; color:", positive_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
    #         shiny::actionButton(paste0("negative_eval_", id), "", icon = icon("thumbs-down"),
    #           onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_negative', this.id, {priority: 'event'})"),
    #           style = paste0("background-color:", negative_eval_button_background_color, "; color:", negative_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
    #         shiny::actionButton(paste0("remove_", id), "", icon = icon("trash-alt"),
    #           onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_deleted_pressed', this.id, {priority: 'event'})"),
    #           style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;")
    #       )),
    #       TRUE ~ action
    #     )) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::select(-positive_eval_button_background_color, -positive_eval_button_color, -negative_eval_button_background_color, -negative_eval_button_color)
    #   
    #   # Get thesaurus names instead of IDs
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::mutate(relation = dplyr::case_when(relation_id == 1 ~ i18n$t("equivalent_to"), relation_id == 2 ~ i18n$t("included_in"), relation_id == 3 ~ i18n$t("include"))) %>%
    #     dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_1 = id, thesaurus_name_1 = name), by = "thesaurus_id_1") %>%
    #     dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_2 = id, thesaurus_name_2 = name), by = "thesaurus_id_2") %>%
    #     dplyr::relocate(thesaurus_name_1, .after = "thesaurus_id_1") %>%
    #     dplyr::relocate(thesaurus_name_2, .after = "thesaurus_id_2") %>%
    #     dplyr::select(-thesaurus_id_1, -thesaurus_id_2, -relation_id) %>%
    #     dplyr::relocate(relation, .after = concept_id_1)
    #   
    #   # Select only mappings without evaluation
    #   
    #   # Render datatable
    #   
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::arrange(dplyr::desc(id)) %>% dplyr::mutate(modified = FALSE)
    #   
    #   searchable_cols <- c("thesaurus_name_1", "concept_id_1", "thesaurus_name_2", "concept_id_2", "relation", "creator_name", "positive_evals", "negative_evals")
    #   factorize_cols <- c("thesaurus_name_1", "thesaurus_name_2", "relation", "creator_name")
    #   sortable_cols <- c("thesaurus_name_1", "concept_id_1", "thesaurus_name_2", "concept_id_2", "relation", "creator_name", "datetime", "positive_evals", "negative_evals")
    #   centered_cols <- c("id", "datetime", "action", "thesaurus_name_1", "concept_id_1", "thesaurus_name_2", "concept_id_2", "creator_name", "relation")
    #   col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_mapping_evals", i18n = i18n)
    #   hidden_cols <- c("id", "deleted", "modified", "user_evaluation_id")
    #   column_widths <- c("action" = "80px", "datetime" = "130px")
    #   
    #   # Render datatable
    #   render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$dataset_vocabulary_concepts_evaluate_mappings,
    #     output_name = "thesaurus_evaluate_mappings", hidden_cols = hidden_cols, centered_cols = centered_cols, searchable_cols = searchable_cols,
    #     col_names = col_names, filter = TRUE, factorize_cols = factorize_cols, sortable_cols = sortable_cols, column_widths = column_widths,
    #     selection = "multiple"
    #   )
    #   
    #   # Create a proxy for datatatable
    #   r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy <- DT::dataTableProxy("thesaurus_evaluate_mappings", deferUntilFlush = FALSE)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$reload_thesaurus_evaluate_mappings_datatable"))
    # })
    
    # When an evaluation button is clicked
    
    observeEvent(input$item_mapping_evaluated_positive, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$item_mapping_evaluated_positive"))
      r$item_mapping_evaluation_type <- "positive"
      r$item_mapping_evaluation_update <- Sys.time()
    })
    
    observeEvent(input$item_mapping_evaluated_negative, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$item_mapping_evaluated_positive"))
      r$item_mapping_evaluation_type <- "negative"
      r$item_mapping_evaluation_update <- Sys.time()
    })
    
    observeEvent(r$item_mapping_evaluation_update, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$item_mapping_evaluation_update"))
      
      prefix <- r$item_mapping_evaluation_type
      new_evaluation_id <- switch(r$item_mapping_evaluation_type, "positive" = 1L, "negative" = 2L)
      
      link_id <- as.integer(substr(input[[paste0("item_mapping_evaluated_", prefix)]], nchar(paste0(prefix, "_eval_")) + 1, nchar(input[[paste0("item_mapping_evaluated_", prefix)]])))
      
      # If we cancel current evaluation
      current_evaluation_id <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
        dplyr::filter(id == link_id) %>% dplyr::pull(user_evaluation_id)
      
      if (!is.na(current_evaluation_id)) if ((current_evaluation_id == 1 & new_evaluation_id == 1) | (current_evaluation_id == 2 & new_evaluation_id == 2)) new_evaluation_id <- NA_integer_
      
      # Change actionButtons style
      if (is.na(new_evaluation_id)){
        positive_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
        negative_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
      }
      else if (prefix == "positive"){
        positive_eval_button_style <- list(background_color = "#5FBAFF", color = "white")
        negative_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
      } 
      else if (prefix == "negative"){
        positive_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
        negative_eval_button_style <- list(background_color = "#FF434C", color = "white")
      }
      
      # Update temp variable
      
      r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
        dplyr::mutate(user_evaluation_id = dplyr::case_when(
          id == link_id ~ new_evaluation_id,
          TRUE ~ user_evaluation_id
        )) %>%
        dplyr::mutate(
          positive_evals = dplyr::case_when(
            id == link_id & is.na(current_evaluation_id) & new_evaluation_id == 1 ~ positive_evals + 1,
            id == link_id & current_evaluation_id == 1 & is.na(new_evaluation_id) ~ positive_evals - 1,
            id == link_id & current_evaluation_id == 2 & new_evaluation_id == 1 ~ positive_evals + 1,
            id == link_id & current_evaluation_id == 1 & new_evaluation_id == 2 ~ positive_evals - 1,
            TRUE ~ positive_evals
          ),
          negative_evals = dplyr::case_when(
            id == link_id & is.na(current_evaluation_id) & new_evaluation_id == 2 ~ negative_evals + 1,
            id == link_id & current_evaluation_id == 2 & is.na(new_evaluation_id) ~ negative_evals - 1,
            id == link_id & current_evaluation_id == 1 & new_evaluation_id == 2 ~ negative_evals + 1,
            id == link_id & current_evaluation_id == 2 & new_evaluation_id == 1 ~ negative_evals - 1,
            TRUE ~ negative_evals
          )
        ) %>%
        dplyr::mutate(action = dplyr::case_when(
          id == link_id ~ as.character(tagList(
            shiny::actionButton(paste0("positive_eval_", link_id), "", icon = icon("thumbs-up"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_positive', this.id, {priority: 'event'})"),
              style = paste0("background-color:", positive_eval_button_style$background_color, "; color:", positive_eval_button_style$color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
            shiny::actionButton(paste0("negative_eval_", link_id), "", icon = icon("thumbs-down"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_negative', this.id, {priority: 'event'})"),
              style = paste0("background-color:", negative_eval_button_style$background_color, "; color:", negative_eval_button_style$color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
            shiny::actionButton(paste0("remove_", link_id), "", icon = icon("trash-alt"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_deleted_pressed', this.id, {priority: 'event'})"),
              style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;")
          )),
          TRUE ~ action
        )) %>%
        dplyr::mutate(modified = dplyr::case_when(
          id == link_id ~ TRUE,
          TRUE ~ modified
        ))
      
      # Reload datatable
      DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$item_mapping_evaluation_update"))
    })
    
    # Delete a row or multiple rows in datatable
    
    mappings_delete_prefix <- "mappings"
    mappings_dialog_title <- "mapping_delete"
    mappings_dialog_subtext <- "mapping_delete_subtext"
    mappings_react_variable <- "mappings_delete_confirm"
    mappings_table <- "vocabulary_concepts_mapping"
    mappings_r_table <- "dataset_vocabulary_concepts_evaluate_mappings"
    mappings_id_var_sql <- "id"
    mappings_id_var_r <- "delete_mappings"
    mappings_delete_message <- "mapping_deleted"
    mappings_reload_variable <- "reload_mappings_evals"
    mappings_information_variable <- "mappings_deleted"
    mappings_delete_variable <- paste0(mappings_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = mappings_delete_prefix, dialog_title = mappings_dialog_title, dialog_subtext = mappings_dialog_subtext,
      react_variable = mappings_react_variable, table = mappings_table, r_table = mappings_r_table, id_var_sql = mappings_id_var_sql, id_var_r = mappings_id_var_r,
      delete_message = mappings_delete_message, translation = TRUE, reload_variable = mappings_reload_variable,
      information_variable = mappings_information_variable)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$item_mapping_deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$item_mapping_deleted_pressed"))
      
      r$delete_mappings <- as.integer(substr(input$item_mapping_deleted_pressed, nchar("delete_") + 1, 100))
      r[[mappings_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$mapping_delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$mapping_delete_selection"))
      
      req(length(input$thesaurus_evaluate_mappings_rows_selected) > 0)
      
      r$delete_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings[input$thesaurus_evaluate_mappings_rows_selected, ] %>% dplyr::pull(id)
      r[[mappings_delete_variable]] <- TRUE
    })
    
    # Reload data
    
    observeEvent(r[[mappings_reload_variable]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$reload_mappings_evals"))
      
      # Reload datatable
      DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Save updates
    
    observeEvent(input$save_mappings_evaluation, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$save_mappings_evaluation"))
      
      # Update database
      
      if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      req(nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified)) > 0)
      
      sql <- glue::glue_sql(paste0("DELETE FROM vocabulary_concepts_mapping_evals WHERE creator_id = {r$user_id} ",
        "AND mapping_id IN ({r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified) %>% dplyr::pull(id)*})"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified, !is.na(user_evaluation_id))) > 0){
        new_data <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
          dplyr::filter(modified, !is.na(user_evaluation_id)) %>%
          dplyr::select(mapping_id = id, evaluation_id = user_evaluation_id) %>%
          dplyr::mutate(id = get_last_row(r$db, "vocabulary_concepts_mapping_evals") + 1:dplyr::n(), .before = "mapping_id") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE) %>%
          dplyr::relocate(evaluation_id, .after = "creator_id")
        
        DBI::dbAppendTable(r$db, "vocabulary_concepts_mapping_evals", new_data)
      }
      
      show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$save_mappings_evaluation"))
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_evaluate_mappings_rows_selected, {
      
    })
  })
}
