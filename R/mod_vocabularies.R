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
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "vocabulary", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
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
                value = c(1, 2, 3, 4, 5, 15, 16)
              )
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
                ),
                uiOutput(ns("vocabulary_datatable_selected_item"))
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
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$selected_dataset 2"))
    })
    
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
            dataset_user_concepts %>% dplyr::select(concept_id, new_concept_name = concept_name, new_concept_display_name = concept_display_name),
            by = "concept_id"
          ) %>%
          dplyr::mutate(
            concept_name = dplyr::case_when(!is.na(new_concept_name) ~ new_concept_name, TRUE ~ concept_name),
            concept_display_name = dplyr::case_when(!is.na(new_concept_display_name) ~ new_concept_display_name, TRUE ~ concept_display_name)
          ) %>%
          dplyr::select(-new_concept_name, -new_concept_display_name)
        
        # Count rows
        
        count_rows <- tibble::tibble()
        
        var_names <- c(
          "condition_occurrence" = "condition_concept_id",
          "drug_exposure" = "drug_concept_id",
          "procedure_occurrence" = "procedure_concept_id",
          "device_exposure" = "device_concept_id",
          "measurement" = "measurement_concept_id",
          "observation" = "observation_concept_id",
          "specimen" = "specimen_concept_id",
          "drug_era" = "drug_concept_id",
          "dose_era" = "drug_concept_id",
          "condition_era" = "condition_concept_id"
        )
        
        for(var_name in names(var_names)){
          if (nrow(d[[var_name]]) > 0){
            count_rows <- 
              count_rows %>% 
              dplyr::bind_rows(
                d[[var_name]] %>% 
                  dplyr::group_by_at(var_names[[var_name]]) %>%
                  dplyr::summarize(count_concepts_rows = dplyr::n(), count_persons_rows = dplyr::n_distinct(person_id)) %>% 
                  dplyr::ungroup() %>% 
                  dplyr::rename(concept_id = var_names[[var_name]])
              )
          }
        }
        
        # Merge count_rows, transform count_rows cols to integer, to be sortable
        if (nrow(count_rows) != 0) r$dataset_all_concepts <- r$dataset_all_concepts %>% 
          dplyr::left_join(count_rows, by = "concept_id") %>%
          dplyr::mutate_at(c("count_concepts_rows", "count_persons_rows"), as.integer) %>%
          dplyr::filter(count_concepts_rows > 0) %>%
          dplyr::rename(concept_id_1 = concept_id, concept_name_1 = concept_name, concept_display_name_1 = concept_display_name) %>%
          dplyr::mutate(relationship_id = NA_character_, concept_id_2 = NA_integer_, concept_name_2 = NA_character_, .after = "concept_display_name_1")
        
        if (nrow(count_rows) == 0) r$dataset_all_concepts <- r$dataset_all_concepts %>% dplyr::slice(0)
        
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
        
        else r$dataset_all_concepts <- r$dataset_all_concepts %>% 
          dplyr::rename(concept_id_1 = concept_id, concept_name_1 = concept_name, concept_display_name_1 = concept_display_name) %>%
          dplyr::mutate(relationship_id = NA_integer_, concept_id_2 = NA_integer_, concept_name_2 = NA_character_, 
            count_concepts_rows = NA_integer_, count_persons_rows = NA_integer_, .after = "concept_display_name_1")
      }
      
      # Reload datatable
      if (length(r$dataset_vocabulary_concepts_datatable_proxy) > 0) DT::replaceData(r$dataset_vocabulary_concepts_datatable_proxy,
        r$dataset_vocabulary_concepts %>% dplyr::slice(0), resetPaging = FALSE, rownames = FALSE)
      
      # Update vocabulary dropdown
      shiny.fluent::updateComboBox.shinyInput(session, "vocabulary", 
        options = convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "id", text_col = "vocabulary_name", i18n = i18n), value = NULL)
      
      # Update datatable columns dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_table_cols", value = c(1, 2, 3, 4, 5, 15, 16))
        
      # Join d$person, d$visit_occurrence & d$visit_detail with r$dataset_all_concepts
      
      r$merge_concepts_and_d_vars <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$load_dataset_all_concepts"))
    })
    
    # Join d$person, d$visit_occurrence & d$visit_detail with r$dataset_all_concepts
    
    observeEvent(r$merge_concepts_and_d_vars, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$merge_concepts_and_d_vars"))
      
      omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)

      # Don't reload if already done
      
      if ("gender_concept_name" %not_in% colnames(d$person)){
      
        person_cols <- c("gender", "race", "ethnicity")
  
        for (col in person_cols) d$person <- d$person %>%
          dplyr::left_join(
            r$dataset_all_concepts %>%
              dplyr::select(!!paste0(col, "_concept_id") := concept_id_1, !!paste0(col, "_concept_name") := concept_name_1), by = paste0(col, "_concept_id")
          ) %>%
          dplyr::relocate(!!paste0(col, "_concept_name"), .after = !!paste0(col, "_concept_id"))
  
        if (omop_version == "5.3") visit_cols <- c("admitting_source", "discharge_to")
        else if (omop_version %in% c("5.4", "6.0")) visit_cols <- c("admitted_from", "discharge_to")
  
        for (table in c("visit_occurrence", "visit_detail")){
  
          if (table == "visit_occurrence") sub_cols <- c("visit", "visit_type")
          else sub_cols <- c("visit_detail", "visit_detail_type")
  
          for (col in sub_cols) d[[table]] <- d[[table]] %>%
            dplyr::left_join(
              r$dataset_all_concepts %>% dplyr::select(!!paste0(col, "_concept_id") := concept_id_1, !!paste0(col, "_concept_name") := concept_name_1), by = paste0(col, "_concept_id")
            ) %>%
            dplyr::relocate(!!paste0(col, "_concept_name"), .after = !!paste0(col, "_concept_id"))
  
          for (col in visit_cols) d[[table]] <- d[[table]] %>%
            dplyr::left_join(
              r$dataset_all_concepts %>% dplyr::select(!!paste0(col, "_concept_id") := concept_id_1, !!paste0(col, "_concept_name") := concept_name_1), by = paste0(col, "_concept_id")
            ) %>%
            dplyr::relocate(!!paste0(col, "_concept_name"), .after = !!paste0(col, "_concept_id"))
        }
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$merge_concepts_and_d_vars"))
    })
    
    observeEvent(input$vocabulary, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary"))
      
      vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input$vocabulary$key) %>% dplyr::pull(vocabulary_id)
      
      r$dataset_vocabulary_concepts <- r$dataset_all_concepts %>%
        dplyr::filter(vocabulary_id == !!vocabulary_id) %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("concept_id_1", as.character)
      
      editable_cols <- c("concept_name_1", "concept_display_name_1")
      searchable_cols <- c("concept_id_1", "concept_name_1", "relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id")
      column_widths <- c("count_persons_rows" = "80px", "count_concepts_rows" = "80px")
      sortable_cols <- c("concept_id_1", "concept_name_1", "relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id", "count_persons_rows", "count_concepts_rows")
      factorize_cols <- c("relationship_id")
      centered_cols <- c("concept_id_1", "relationship_id", "concept_id_2", "domain_id", "count_persons_rows", "count_concepts_rows")
      col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_with_counts", i18n = i18n)
      hidden_cols <- c("id", "concept_id_2", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", 
        "valid_start_date", "valid_end_date", "invalid_reason", "modified")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$dataset_vocabulary_concepts,
        output_name = "vocabulary_concepts", col_names = col_names,
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, factorize_cols = factorize_cols)
      
      # Create a proxy for datatatable
      r$dataset_vocabulary_concepts_datatable_proxy <- DT::dataTableProxy("vocabulary_concepts", deferUntilFlush = FALSE)
      
      # Update datatable columns dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_table_cols", value = c(1, 2, 3, 4, 5, 15, 16))
    })
    
    # Update which cols are hidden
    observeEvent(input$vocabulary_table_cols, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_table_cols"))
      
      req(length(r$dataset_vocabulary_concepts_datatable_proxy) > 0)
      
      r$dataset_vocabulary_concepts_datatable_proxy %>%
        DT::showCols(1:17) %>%
        DT::hideCols(setdiff(1:17, input$vocabulary_table_cols))
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
    
    # When a row is selected
    observeEvent(input$vocabulary_concepts_rows_selected, {
      
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$vocabulary_concepts_rows_selected"))
      
      # r$vocabulary_concepts_selected_concept_id <- r$dataset_vocabulary_concepts_temp[input$vocabulary_concepts_rows_selected, ] %>% dplyr::pull(concept_id)
      # r$vocabulary_concepts_selected_item_trigger <- Sys.time()
    })
    
    # observeEvent(r$vocabulary_concepts_selected_item_trigger, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$vocabulary_concepts_selected_item_trigger"))
    #   
    #   style <- "display:inline-block; width:200px; font-weight:bold;"
    #   
    #   if (is.null(input$vocabulary_concepts_pivot)) prefix <- "datatable"
    #   else if (input$vocabulary_concepts_pivot == "vocabulary_concepts_table_view") prefix <- "datatable"
    #   else if (input$vocabulary_concepts_pivot == "vocabulary_concepts_tree_view") prefix <- "tree"
    # 
    #   if (length(r$vocabulary_concepts_selected_concept_id) == 0) output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI("") 
    #   req(length(r$vocabulary_concepts_selected_concept_id) > 0)
    # 
    #   if (prefix == "datatable") thesaurus_item <- r$dataset_vocabulary_concepts_temp %>% dplyr::filter(concept_id == r$vocabulary_concepts_selected_concept_id)
    #   if (prefix == "tree") thesaurus_item <- r$dataset_vocabulary_concepts %>% dplyr::filter(concept_id == r$vocabulary_concepts_selected_concept_id)
    # 
    #   if (nrow(thesaurus_item) == 0) output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI("")    
    #   req(nrow(thesaurus_item) > 0)
    #   
    #   thesaurus_item <- thesaurus_item %>% dplyr::mutate_at("concept_id", as.integer)
    # 
    #   thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
    # 
    #   # Which columns contain data
    #   r$dataset_vocabulary_concepts_d_var <- ""
    #   r$dataset_vocabulary_concepts_cols_not_empty <- ""
    #   plots <- tagList()
    #   values_num <- ""
    #   values <- ""
    #   amounts <- ""
    #   rates <- ""
    #   
    #   for (var in c("labs_vitals", "orders", "text", "diagnoses")){
    #     
    #     values_text <- ""
    #     
    #     if (nrow(d[[var]]) > 0){
    #       all_values_temp <- d[[var]] %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
    #         dplyr::inner_join(thesaurus_item %>% dplyr::select(concept_id), by = "concept_id")
    #       
    #       if (nrow(all_values_temp) > 0){
    #         all_values <- all_values_temp
    #         r$dataset_vocabulary_concepts_d_var <- var 
    #       }
    #     }
    #   }
    #   
    #   if (r$dataset_vocabulary_concepts_d_var == "labs_vitals"){
    # 
    #     if (nrow(all_values %>% dplyr::filter(!is.na(value_num))) > 0){
    #       values_num <- suppressMessages(
    #         all_values %>% 
    #           dplyr::mutate(value_num_text = dplyr::case_when(!is.na(unit) ~ paste0(value_num, " ", unit), TRUE ~ as.character(value_num))) %>%
    #           dplyr::filter(!is.na(value_num)) %>%
    #           dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value_num_text)
    #         )
    #       plots <- tagList(plots, shinyjs::hidden(plotOutput(ns(paste0(prefix, "_value_num_plot")))))
    #       r$dataset_vocabulary_concepts_cols_not_empty <- c(r$dataset_vocabulary_concepts_cols_not_empty, "value_num")
    #     }
    # 
    #     if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
    #       
    #       values <- suppressMessages(
    #         all_values %>% 
    #           dplyr::filter(!is.na(value)) %>%
    #           dplyr::slice_sample(n = 5, replace = TRUE) %>% 
    #           dplyr::pull(value)
    #         )
    #       plots <- tagList(plots, shinyjs::hidden(plotOutput(ns(paste0(prefix, "_value_plot")))))
    #       r$dataset_vocabulary_concepts_cols_not_empty <- c(r$dataset_vocabulary_concepts_cols_not_empty, "value")
    #     }
    #     
    #     values_text <- tagList(
    #       span(i18n$t("numeric_values"), style = style), paste(values_num, collapse = " || "), br(),
    #       span(i18n$t("values"), style = style), paste(values, collapse = " || "), br())
    #   }
    #   
    #   if (r$dataset_vocabulary_concepts_d_var == "orders"){
    #     
    #     if (nrow(all_values %>% dplyr::filter(!is.na(amount))) > 0){
    #       
    #       amounts <- suppressMessages(
    #         all_values %>% 
    #           dplyr::mutate(amount_text = dplyr::case_when(!is.na(amount_unit) ~ paste0(amount, " ", amount_unit), TRUE ~ as.character(amount))) %>%
    #           dplyr::filter(!is.na(amount)) %>%
    #           dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(amount_text)
    #       )
    #       plots <- tagList(plots, plotOutput(ns(paste0(prefix, "_amount_plot"))))
    #       r$dataset_vocabulary_concepts_cols_not_empty <- c(r$dataset_vocabulary_concepts_cols_not_empty, "amount")
    #     }
    #     
    #     if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0){
    #       
    #       rates <- suppressMessages(
    #         all_values %>% 
    #           dplyr::mutate(rate_text = dplyr::case_when(!is.na(rate_unit) ~ paste0(rate, " ", rate_unit), TRUE ~ as.character(rate))) %>%
    #           dplyr::filter(!is.na(rate)) %>%
    #           dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text)
    #       )
    #       plots <- tagList(plots, plotOutput(ns(paste0(prefix, "_rate_plot"))))
    #       r$dataset_vocabulary_concepts_cols_not_empty <- c(r$dataset_vocabulary_concepts_cols_not_empty, "rate")
    #     }
    #     
    #     values_text <- tagList(
    #       span(i18n$t("rate"), style = style), paste(rates, collapse = " || "), br(),
    #       span(i18n$t("amount"), style = style), paste(amounts, collapse = " || "), br())
    #   }
    #   
    #   if (r$dataset_vocabulary_concepts_d_var == "text"){
    #     
    #     if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
    #       
    #       values <- suppressMessages(
    #         all_values %>% 
    #           dplyr::filter(!is.na(value)) %>%
    #           dplyr::slice_sample(n = 5, replace = TRUE) %>% 
    #           dplyr::pull(value)
    #       )
    #       plots <- tagList(plots, shinyjs::hidden(plotOutput(ns("value_plot"))))
    #       r$dataset_vocabulary_concepts_cols_not_empty <- c(r$dataset_vocabulary_concepts_cols_not_empty, "value")
    #     }
    #     
    #     values_text <- tagList(
    #       span(i18n$t("rate"), style = style), paste(rate, collapse = " || "), br(),
    #       span(i18n$t("amount"), style = style), paste(amount, collapse = " || "), br())
    #   }
    #   
    #   dataset_vocabulary_concepts_d_var <- ifelse(r$dataset_vocabulary_concepts_d_var == "", i18n$t("none_fem"), paste0("d$", r$dataset_vocabulary_concepts_d_var))
    #     
    #   # print(plots)
    #   
    #   output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI(tagList(br(), div(
    #     span(i18n$t("var_containing_item"), style = style), dataset_vocabulary_concepts_d_var, br(),
    #     span(i18n$t("thesaurus_id"), style = style), thesaurus_item$thesaurus_id, br(),
    #     span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
    #     span(i18n$t("concept_id"), style = style), thesaurus_item$concept_id, br(),
    #     span(i18n$t("name"), style = style), thesaurus_item$name, br(),
    #     span(i18n$t("concept_display_name_1"), style = style), thesaurus_item$display_name, br(),
    #     span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
    #     values_text, br(),
    #     shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "_show_plots")), i18n$t("show_plots")),
    #     conditionalPanel(condition = paste0("input.", prefix, "_show_plots == true"), ns = ns, br(), plots),
    #     style = "border:dashed 1px; padding:10px;"
    #   )))
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$vocabulary_concepts_seeclted_item_trigger"))
    # })
    
    # observeEvent(input$datatable_show_plots, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$datatable_show_plots"))
    #   
    #   if (is.null(input$vocabulary_concepts_pivot)) prefix <- "datatable"
    #   else if (input$vocabulary_concepts_pivot == "vocabulary_concepts_table_view") prefix <- "datatable"
    #   else if (input$vocabulary_concepts_pivot == "vocabulary_concepts_tree_view") prefix <- "tree"
    #   
    #   thesaurus_item <- r$dataset_vocabulary_concepts %>% dplyr::filter(concept_id == r$vocabulary_concepts_selected_concept_id)
    #   
    #   # thesaurus_item <- r$dataset_vocabulary_concepts_temp[input$vocabulary_concepts_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
    #   thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
    #   
    #   all_values <- d$labs_vitals %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
    #     dplyr::inner_join(thesaurus_item %>% dplyr::select(concept_id), by = "concept_id") %>% dplyr::select(value, value_num)
    #   values_num <- all_values %>% dplyr::filter(!is.na(value_num))
    #   values <- all_values %>% dplyr::filter(!is.na(value))
    #   
    #   # print(values_num)
    #   
    #   shinyjs::show(paste0(prefix, "_value_num_plot"))
    #   
    #   output[[paste0(prefix, "_value_num_plot")]] <- renderPlot({
    #     values_num %>%
    #       ggplot2::ggplot(ggplot2::aes(x = value_num)) +
    #       ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
    #       ggplot2::labs(x = "", y = "") +
    #       ggplot2::theme_bw()
    #   })
    #   
    #   # output[[paste0(prefix, "_value_plot")]] <- renderPlot({
    #   #   values %>%
    #   #     ggplot2::ggplot(ggplot2::aes(x = value)) +
    #   #     ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), stat = "count", colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
    #   #     # ggplot2::labs(x = i18n$t(col), y = "") +
    #   #     ggplot2::theme_bw()
    #   # })
    #   
    #   # for (column_name in c("value", "value_num", "amount", "rate")){
    #   #   if (col %in% r$dataset_vocabulary_concepts_cols_not_empty){
    #   #     print(col)
    #   #     column_name <- col
    #   #     output[[paste0(col, "_plot")]] <- renderPlot({
    #   #       all_values %>%
    #   #         ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(column_name))) +
    #   #         ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
    #   #         # ggplot2::scale_x_continuous(breaks = ) +
    #   #         ggplot2::labs(x = i18n$t(col), y = "") +
    #   #         ggplot2::theme_bw()
    #   #     })
    #   #   }
    #   # }
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$datatable_show_plots"))
    # })
    
    # --- --- --- --- --
    # Items mapping ----
    # --- --- --- --- --
    
    # --- --- --- --- --- --
    ## Create a mapping ----
    # --- --- --- --- --- --
    
    observeEvent(input$thesaurus_mapping1, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$thesaurus_mapping1"))
      r$thesaurus_mapping_reload <- paste0(Sys.time(), "_mapping1")
    })
    observeEvent(input$thesaurus_mapping2, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$thesaurus_mapping2"))
      r$thesaurus_mapping_reload <- paste0(Sys.time(), "_mapping2")
    })
    
    observeEvent(r$thesaurus_mapping_reload, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$thesaurus_mapping_reload"))
      
      if (grepl("mapping1", r$thesaurus_mapping_reload)) mapping <- "mapping1"
      else if (grepl("mapping2", r$thesaurus_mapping_reload)) mapping <- "mapping2"
      
      req(length(input[[paste0("thesaurus_", mapping)]]$key) > 0)
      
      r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.concept_id, t.name, t.display_name, t.unit, t.datetime, t.deleted
            FROM vocabulary_concepts t
            WHERE t.thesaurus_id = ", input[[paste0("thesaurus_", mapping)]]$key, " AND t.deleted IS FALSE
            ORDER BY t.concept_id")) %>% tibble::as_tibble() %>% dplyr::mutate(action = "")
      
      # Get user's modifications on items names & concept_display_names
      
      r[[paste0("dataset_vocabulary_user_concepts_", mapping)]] <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.concept_id, t.name, t.display_name, t.deleted
            FROM vocabulary_concepts_users t
            WHERE t.thesaurus_id = ", input[[paste0("thesaurus_", mapping)]]$key, " AND t.user_id = ", r$user_id ," AND t.deleted IS FALSE
            ORDER BY t.concept_id")) %>% tibble::as_tibble()
      
      # Merge tibbles
      r[[paste0("dataset_vocabulary_concepts_", mapping)]] <-
        r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>%
        dplyr::left_join(
          r[[paste0("dataset_vocabulary_user_concepts_", mapping)]] %>% dplyr::select(concept_id, new_name = name, new_display_name = display_name),
          by = "concept_id"
        ) %>%
        dplyr::mutate(
          name = dplyr::case_when(!is.na(new_name) ~ new_name, TRUE ~ name),
          display_name = dplyr::case_when(!is.na(new_display_name) ~ new_display_name, TRUE ~ display_name)
        ) %>%
        dplyr::select(-new_name, -new_display_name)
      
      count_concepts_rows <- tibble::tibble()
      count_persons_rows <- tibble::tibble()
      
      # Add count_concepts_rows in the cache & get it if already in the cache
      tryCatch(count_concepts_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input[[paste0("thesaurus_", mapping)]]$key,
        dataset_id = r$selected_dataset, category = "count_concepts_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_vocabulary", 
          error_name = paste0("thesaurus - create_datatable_cache - count_concepts_rows - fail_load_vocabulary - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      # Add count_concepts_rows in the cache & get it if already in the cache
      tryCatch(count_persons_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input[[paste0("thesaurus_", mapping)]]$key,
        dataset_id = as.integer(r$selected_dataset), category = "count_persons_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_vocabulary", 
          error_name = paste0("thesaurus - create_datatable_cache - count_persons_rows - fail_load_vocabulary - id = ", r$selected_dataset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      if (nrow(count_concepts_rows) == 0 | nrow(count_persons_rows) == 0) show_message_bar(output, "fail_load_vocabulary", "severeWarning", i18n = i18n, ns = ns)
      req(nrow(count_concepts_rows) != 0, nrow(count_persons_rows) != 0)
      
      # Transform count_rows cols to integer, to be sortable
      r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>%
        dplyr::left_join(count_concepts_rows, by = "concept_id") %>%
        dplyr::left_join(count_persons_rows, by = "concept_id") %>%
        dplyr::mutate_at(c("count_concepts_rows", "count_persons_rows"), as.integer) %>%
        dplyr::relocate(count_persons_rows, .before = "action") %>% dplyr::relocate(count_concepts_rows, .before = "action") %>%
        dplyr::arrange(name)
      
      r[[paste0("dataset_vocabulary_concepts_", mapping, "_temp")]] <- r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("concept_id", as.character)
      
      editable_cols <- c("name", "display_name")
      searchable_cols <- c("concept_id", "name", "display_name", "unit")
      factorize_cols <- c("unit")
      column_widths <- c("id" = "80px", "action" = "80px", "unit" = "100px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px")
      sortable_cols <- c("id", "name", "display_name", "count_persons_rows", "count_concepts_rows")
      centered_cols <- c("id", "concept_id", "unit", "datetime", "count_persons_rows", "count_concepts_rows", "action")
      col_names <- get_col_names(table_name = "mapping_vocabulary_concepts_with_counts", i18n = i18n)
      hidden_cols <- c("id", "thesaurus_id", "concept_id", "display_name", "domain_id", "unit", "count_persons_rows", "datetime", "deleted", "modified", "action")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0("dataset_vocabulary_concepts_", mapping, "_temp")]],
        output_name = paste0("thesaurus_", mapping, "_dt"), col_names = col_names, datatable_dom = "<'top't><'bottom'p>",
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$thesaurus_mapping_reload"))
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_mapping1_dt_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$thesaurus_mapping1_dt_rows_selected"))
      r$thesaurus_mapping_item_info <- paste0(Sys.time(), "_mapping1")
    })
    observeEvent(input$thesaurus_mapping2_dt_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$thesaurus_mapping2_dt_rows_selected"))
      r$thesaurus_mapping_item_info <- paste0(Sys.time(), "_mapping2")
    })
    
    observeEvent(r$thesaurus_mapping_item_info, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer r$thesaurus_mapping_item_info"))
      
      if (grepl("mapping1", r$thesaurus_mapping_item_info)) mapping <- "mapping1"
      else if (grepl("mapping2", r$thesaurus_mapping_item_info)) mapping <- "mapping2"
      
      style <- "display:inline-block; width:200px; font-weight:bold;"
      
      thesaurus_item <- r[[paste0("dataset_vocabulary_concepts_", mapping, "_temp")]][input[[paste0("thesaurus_", mapping, "_dt_rows_selected")]], ] %>% dplyr::mutate_at("concept_id", as.integer)
      
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
      
      # Which columns contain data
      r[[paste0("dataset_vocabulary_concepts_", mapping, "_d_var")]] <- ""
      r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- ""
      plots <- tagList()
      values_num <- ""
      values <- ""
      amounts <- ""
      rates <- ""
      
      for (var in c("labs_vitals", "orders", "text", "diagnoses")){
        
        values_text <- ""
        
        if (nrow(d[[var]]) > 0){
          all_values_temp <- d[[var]] %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
            dplyr::inner_join(thesaurus_item %>% dplyr::select(concept_id), by = "concept_id")
          
          if (nrow(all_values_temp) > 0){
            all_values <- all_values_temp
            r[[paste0("dataset_vocabulary_concepts_", mapping, "_d_var")]] <- var 
          }
        }
      }
      
      if (r[[paste0("dataset_vocabulary_concepts_", mapping, "_d_var")]] == "labs_vitals"){
        if (nrow(all_values %>% dplyr::filter(!is.na(value_num))) > 0){
          values_num <- suppressMessages(
            all_values %>% 
              dplyr::mutate(value_num_text = dplyr::case_when(!is.na(unit) ~ paste0(value_num, " ", unit), TRUE ~ as.character(value_num))) %>%
              dplyr::filter(!is.na(value_num)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value_num_text)
          )
          # plots <- tagList(plots, plotOutput(ns("value_num_plot")))
          r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- c(r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]], "value_num")
        }
        
        if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
          
          values <- suppressMessages(
            all_values %>% 
              dplyr::filter(!is.na(value)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% 
              dplyr::pull(value)
          )
          # plots <- tagList(plots, plotOutput(ns("value_plot")))
          r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- c(r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]], "value")
        }
        
        values_text <- tagList(
          span(i18n$t("numeric_values"), style = style), paste(values_num, collapse = " || "), br(),
          span(i18n$t("values"), style = style), paste(values, collapse = " || "), br())
      }
      
      if (r[[paste0("dataset_vocabulary_concepts_", mapping, "_d_var")]] == "orders"){
        
        if (nrow(all_values %>% dplyr::filter(!is.na(amount))) > 0){
          
          amounts <- suppressMessages(
            all_values %>% 
              dplyr::mutate(amount_text = dplyr::case_when(!is.na(amount_unit) ~ paste0(amount, " ", amount_unit), TRUE ~ as.character(amount))) %>%
              dplyr::filter(!is.na(amount)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(amount_text)
          )
          # plots <- tagList(plots, plotOutput(ns("amount_plot")))
          r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- c(r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]], "amount")
        }
        
        if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0){
          
          rates <- suppressMessages(
            all_values %>% 
              dplyr::mutate(rate_text = dplyr::case_when(!is.na(rate_unit) ~ paste0(rate, " ", rate_unit), TRUE ~ as.character(rate))) %>%
              dplyr::filter(!is.na(rate)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text)
          )
          # plots <- tagList(plots, plotOutput(ns("rate_plot")))
          r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- c(r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]], "rate")
        }
        
        values_text <- tagList(
          span(i18n$t("rate"), style = style), paste(rates, collapse = " || "), br(),
          span(i18n$t("amount"), style = style), paste(amounts, collapse = " || "), br())
      }
      
      if (r[[paste0("dataset_vocabulary_concepts_", mapping, "_d_var")]] == "text"){
        
        if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
          
          values <- suppressMessages(
            all_values %>% 
              dplyr::filter(!is.na(value)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% 
              dplyr::pull(value)
          )
          # plots <- tagList(plots, plotOutput(ns("value_plot")))
          r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]] <- c(r[[paste0("dataset_vocabulary_concepts_", mapping, "_cols_not_empty")]], "value")
        }
        
        values_text <- tagList(
          span(i18n$t("rate"), style = style), paste(rate, collapse = " || "), br(),
          span(i18n$t("amount"), style = style), paste(amount, collapse = " || "), br())
      }
      
      output[[paste0("thesaurus_selected_item_", mapping)]] <- renderUI(tagList(div(
        span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
        span(i18n$t("concept_id"), style = style), thesaurus_item$concept_id, br(),
        span(i18n$t("name"), style = style), thesaurus_item$name, br(),
        span(i18n$t("concept_display_name"), style = style), thesaurus_item$display_name, br(),
        span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
        values_text
      )))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$thesaurus_mapping_item_info"))
    })
    
    # When a mapping id added
    
    observeEvent(input$add_mapping, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_vocabularies - observer input$add_mapping"))
      
      req(length(input$thesaurus_mapping1_dt_rows_selected) > 0)
      req(length(input$thesaurus_mapping2_dt_rows_selected) > 0)
      req(nrow(r$dataset_vocabulary_concepts_mapping1_temp[input$thesaurus_mapping1_dt_rows_selected, ]) > 0)
      req(nrow(r$dataset_vocabulary_concepts_mapping2_temp[input$thesaurus_mapping2_dt_rows_selected, ]) > 0)
      
      item_1 <- r$dataset_vocabulary_concepts_mapping1_temp[input$thesaurus_mapping1_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
      item_2 <- r$dataset_vocabulary_concepts_mapping2_temp[input$thesaurus_mapping2_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
      
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
      
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "thesaurus_mapping1_dt", datatable_dom = "")
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "thesaurus_mapping2_dt", datatable_dom = "")
      
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
    #   tryCatch(action_col <- create_datatable_cache(output = output, r = r, i18n = i18n, module_id = id, thesaurus_id = thesaurus_ids, category = "thumbs_and_delete"))
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