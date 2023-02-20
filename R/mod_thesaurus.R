#' thesaurus UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_thesaurus_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("thesaurus_items_card", "thesaurus_categories_card", "thesaurus_conversions_card", "thesaurus_mapping_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "thesaurus_main", text = i18n$t("thesaurus"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "thesaurus_items_card", itemKey = "thesaurus_items_card", headerText = i18n$t("items")),
          # shiny.fluent::PivotItem(id = "thesaurus_categories_card", itemKey = "thesaurus_categories_card", headerText = i18n$t("categories")),
          # shiny.fluent::PivotItem(id = "thesaurus_conversions_card", itemKey = "thesaurus_conversions_card", headerText = i18n$t("conversions")),
          shiny.fluent::PivotItem(id = "thesaurus_mapping_card", itemKey = "thesaurus_mapping_card", headerText = i18n$t("items_mapping"))
        )
      )
    ),
    
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- ---
    # Items card ----
    # --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("thesaurus_items_card"),
        make_card(i18n$t("items"),
          div(
            shiny.fluent::Pivot(
              onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-thesaurus_items_pivot', item.props.id)")),
              shiny.fluent::PivotItem(id = "thesaurus_items_table_view", itemKey = "thesaurus_items_table_view", headerText = i18n$t("datatable_view")),
              shiny.fluent::PivotItem(id = "thesaurus_items_tree_view", itemKey = "thesaurus_items_tree_view", headerText = i18n$t("tree_view"))
            ),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_combobox(i18n = i18n, ns = ns, label = "thesaurus", id = "thesaurus", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              conditionalPanel(
                condition = "input.thesaurus != null", ns = ns,
                div(strong(i18n$t("show_only_used_items"), style = "display:block; padding-bottom:12px;"),
                  shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;")#,
                # conditionalPanel(
                #   condition = "input.thesaurus_items_pivot == 'thesaurus_items_tree_view'", ns = ns,
                #   div(strong(i18n$t("unroll_tree"), style = "display:block; padding-bottom:12px;"),
                #     shiny.fluent::Toggle.shinyInput(ns("unroll_tree"), value = TRUE), style = "margin-top:15px;")
                # )
              ),
              style = "position:relative; z-index:1; width:800px;"
            ),
            conditionalPanel(
              condition = "input.thesaurus_items_pivot == null | input.thesaurus_items_pivot == 'thesaurus_items_table_view'", ns = ns,
              div(DT::DTOutput(ns("thesaurus_items")), style = "margin-top:-30px; z-index:2"),
              conditionalPanel("input.thesaurus == null", ns = ns, br(), br()),
              conditionalPanel(
                condition = "input.thesaurus != null", ns = ns,
                shiny.fluent::PrimaryButton.shinyInput(ns("save_thesaurus_items"), i18n$t("save")),
                uiOutput(ns("thesaurus_datatable_selected_item"))
              )
            ),
            conditionalPanel(
              condition = "input.thesaurus_items_pivot == 'thesaurus_items_tree_view'", ns = ns, br(),
              div(
                div(
                  shinyTree::shinyTree(
                    ns("thesaurus_items_tree"),
                    search = FALSE,
                    checkbox = FALSE,
                    dragAndDrop = FALSE,
                    theme = "proton",
                    themeIcons = FALSE,
                    wholerow = FALSE,
                    stripes = FALSE,
                    animation = 100,
                    contextmenu = FALSE,
                    unique = FALSE,
                    types =
                      "{
                        '#': { 'max_children' : 2, 'max_depth' : 6, 'valid_children' : ['root'] },
                        'root' : { 'valid_children' : ['file'] },
                        'default' : { 'valid_children' : ['default','file'] },
                        'file' : { 'icon' : 'fa fa-file', 'valid_children' : [] }
                      }"
                  ),
                  style = "width:45%; float:left;"
                ),
                div(uiOutput(ns("thesaurus_tree_selected_item")), style = "width:50%; float:right;")
              )
            )
          )
        ), br(),
        #shinyjs::hidden(
          div(shinyTree::shinyTree(ns("tree")))
        #)
      )
    ),
    
    # --- --- --- --- -- -
    # Categories card ----
    # --- --- --- --- -- -
    
    # shinyjs::hidden(
    #   div(
    #     id = ns("thesaurus_categories_card"),
    #     make_card(i18n$t("categories"),
    #       div(
    #         
    #       )
    #     ), br()
    #   )
    # ),
    
    # --- --- --- --- --- -
    # Conversions card ----
    # --- --- --- --- --- -
    
    # shinyjs::hidden(
    #   div(
    #     id = ns("thesaurus_conversions_card"),
    #     make_card(i18n$t("conversions"),
    #       div(
    #         div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
    #         div(shiny.fluent::MessageBar(
    #           div(
    #             strong("A faire"),
    #             p("Il sera possible de convertir les variables dans différentes unités"),
    #             p("Faut-il laisser possible le changement de l'unité, en changeant le texte, dans \"Tous les items\" ?")
    #           ),
    #           messageBarType = 0)
    #         )
    #       )
    #     ), br()
    #   )
    # ),
    
    # --- --- --- -- --
    # Mapping card ----
    # --- --- --- -- --
    
    shinyjs::hidden(
      div(
        id = ns("thesaurus_mapping_card"),
        make_card(i18n$t("items_mapping"),
          div(
            shiny.fluent::reactOutput(ns("mappings_delete_confirm")),
            div(
              shiny.fluent::Pivot(
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-mapping_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "thesaurus_mapping_add", itemKey = "thesaurus_mapping_add", headerText = i18n$t("add")),
                shiny.fluent::PivotItem(id = "thesaurus_mapping_management", itemKey = "thesaurus_mapping_management", headerText = i18n$t("evaluate_and_edit"))
              )
            ),
            conditionalPanel(condition = "input.mapping_current_tab == null || input.mapping_current_tab == 'thesaurus_mapping_add'", ns = ns,
              div(
                div(
                  div(
                    make_combobox(i18n = i18n, ns = ns, label = "thesaurus1", id = "thesaurus_mapping1", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    DT::DTOutput(ns("thesaurus_mapping1_dt"))
                  ),
                  div(
                    make_combobox(i18n = i18n, ns = ns, label = "thesaurus2", id = "thesaurus_mapping2", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    DT::DTOutput(ns("thesaurus_mapping2_dt"))
                  ),
                  style = "width:100%; display:grid; grid-template-columns:1fr 1fr; grid-gap:20px;"
                ), br(),
                conditionalPanel(condition = "input.thesaurus_mapping1 != null && input.thesaurus_mapping2 != null", ns = ns, 
                  br(),
                  div(
                    div(uiOutput(ns("thesaurus_selected_item_mapping1")), style = "border:dashed 1px; padding:10px;"),
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
        ), br()
      )
    )
  )
}
    
#' thesaurus Server Functions
#'
#' @noRd 
mod_thesaurus_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), 
  i18n = R6::R6Class(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_thesaurus - start"))
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
 
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("thesaurus_items_card", "thesaurus_categories_card", "thesaurus_conversions_card", "thesaurus_mapping_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n, ns = ns))
    observeEvent(r$show_message_bar2, show_message_bar(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- -- -
    # Thesaurus items ----
    # --- --- --- --- --- -- -
    
    # Delete when "thesaurus_items_card" will be added in r$user_accesses
    
    # observeEvent(input$current_tab, {
    #   sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
    #   shinyjs::show(input$current_tab)
    # })
    
    observeEvent(r$chosen_datamart, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$chosen_datamart 2"))
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("thesaurus_items_card" %in% r$user_accesses) shinyjs::show("thesaurus_items_card")
        else shinyjs::show("thesaurus_items_card_forbidden")
      }
      
      data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      # Multiple cases
      # Only one ID, so it's the beginning and the end
      # Last ID, so it's the end
      # ID between begin and last, so separated by commas
      thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
        grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
      thesaurus_options <- convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", i18n = i18n)
      for (var in c("thesaurus", "thesaurus_mapping1", "thesaurus_mapping2")) shiny.fluent::updateComboBox.shinyInput(session, var, options = thesaurus_options, value = NULL)
      
      if (length(r$datamart_thesaurus_items_temp) > 0) r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items_temp %>% dplyr::slice(0)
      
      # Reset UI of selected item
      output$thesaurus_datatable_selected_item <- renderUI("")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$chosen_datamart 2"))
    })
    
    observeEvent(input$show_only_used_items, {
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$show_only_used_items"))
      r$reload_thesaurus_datatable <- Sys.time()
    })
    observeEvent(input$thesaurus, {
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus"))
      r$reload_thesaurus_data <- Sys.time()
    })
    
    observeEvent(r$reload_thesaurus_data, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$reload_thesaurus_data"))
      
      req(length(input$thesaurus$key) > 0)
      
      sql <- glue::glue_sql(paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted ",
        "FROM thesaurus_items t ",
        "WHERE t.thesaurus_id = {input$thesaurus$key} AND t.deleted IS FALSE ",
        "ORDER BY t.item_id"), .con = r$db)
      
      datamart_thesaurus_items <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      r$datamart_thesaurus_items <- datamart_thesaurus_items %>% dplyr::mutate(action = "")
      
      # Get user's modifications on items names & abbreviations
      
      sql <- glue::glue_sql(paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.deleted
          FROM thesaurus_items_users t
          WHERE t.thesaurus_id = {input$thesaurus$key} AND t.user_id = {r$user_id} AND t.deleted IS FALSE
          ORDER BY t.item_id"), .con = r$db)
      r$datamart_thesaurus_user_items <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      # Get thesaurus items relationships
      
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items_mapping WHERE thesaurus_id_1 = {input$thesaurus$key} AND ",
        "thesaurus_id_2 = {input$thesaurus$key} AND category = 'import_thesaurus_mapping' AND relation_id = 2 AND deleted IS FALSE"), .con = r$db)
      thesaurus_items_mappings <- DBI::dbGetQuery(r$db, sql)
      
      # Merge tibbles
      if (nrow(r$datamart_thesaurus_user_items) > 0) r$datamart_thesaurus_items <-
        r$datamart_thesaurus_items %>%
        dplyr::left_join(
          r$datamart_thesaurus_user_items %>% dplyr::select(item_id, new_name = name, new_display_name = display_name),
          by = "item_id"
        ) %>%
        dplyr::mutate(
          name = dplyr::case_when(!is.na(new_name) ~ new_name, TRUE ~ name),
          display_name = dplyr::case_when(!is.na(new_display_name) ~ new_display_name, TRUE ~ display_name)
        ) %>%
        dplyr::select(-new_name, -new_display_name)
      
      if (nrow(thesaurus_items_mappings) > 0) r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>%
        dplyr::left_join(
          thesaurus_items_mappings %>%
            dplyr::select(item_id = item_id_1, parent_item_id = item_id_2),
          by = "item_id"
        ) %>%
        dplyr::relocate(parent_item_id, .after = item_id)
      else r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::mutate(parent_item_id = NA_integer_, .after = item_id)
      
      count_items_rows <- tibble::tibble()
      count_patients_rows <- tibble::tibble()
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
        datamart_id = r$chosen_datamart, category = "count_items_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus", 
          error_name = paste0("thesaurus - create_datatable_cache - count_items_rows - fail_load_thesaurus - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input$thesaurus$key,
        datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
        error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus", 
          error_name = paste0("thesaurus - create_datatable_cache - count_patients_rows - fail_load_thesaurus - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_thesaurus", "severeWarning", i18n = i18n, ns = ns)
      req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
      
      # Transform count_rows cols to integer, to be sortable
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>%
        dplyr::left_join(count_items_rows, by = "item_id") %>%
        dplyr::left_join(count_patients_rows, by = "item_id") %>%
        dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
      
      # Order by name
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::arrange(name)
      
      # Prepare data for shinyTree
      
      datamart_thesaurus_items_tree <-
        r$datamart_thesaurus_items %>%
        dplyr::select(-action) %>%
        get_thesaurus_items_levels() %>%
        get_thesaurus_items_paths()
      
      # Create a first file to save the list without count_items_rows selected
      
      not_filtered_list_file <- paste0(r$app_folder, "/thesaurus/thesaurus_", input$thesaurus$key, "_datamart_", r$chosen_datamart, "_not_filtered.rds")
      filtered_list_file <- paste0(r$app_folder, "/thesaurus/thesaurus_", input$thesaurus$key, "_datamart_", r$chosen_datamart, "_filtered.rds")
      
      # If file exists, load it
      if (file.exists(filtered_list_file) & file.exists(not_filtered_list_file)) tryCatch({
        
        #if (input$show_only_used_items) 
        r$datamart_thesaurus_items_tree_filtered <- rlist::list.load(filtered_list_file)
        r$datamart_thesaurus_items_tree_not_filtered <- rlist::list.load(not_filtered_list_file)
        
      }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_list_file", 
        error_name = paste0("thesaurus - shiny_tree - load_list_file - thesaurus_id = ", input$thesaurus$key), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      if (!file.exists(filtered_list_file)) tryCatch({
        
        r$datamart_thesaurus_items_tree_not_filtered <- datamart_thesaurus_items_tree %>%
          # dplyr::sample_n(400) %>%
          dplyr::arrange(path, dplyr::desc(count_items_rows), name) %>%
          prepare_data_shiny_tree() 
        
        r$datamart_thesaurus_items_tree_filtered <- datamart_thesaurus_items_tree %>%
          # dplyr::sample_n(400) %>%
          dplyr::filter(has_children | (!has_children & count_items_rows > 0)) %>%
          dplyr::arrange(path, dplyr::desc(count_items_rows), name) %>%
          prepare_data_shiny_tree()
        
        r$datamart_thesaurus_items_tree_not_filtered %>% rlist::list.save(not_filtered_list_file)
        r$datamart_thesaurus_items_tree_filtered %>% rlist::list.save(filtered_list_file)
        
        # r$datamart_thesaurus_items_tree <- datamart_thesaurus_items_tree_filtered
        # else r$datamart_thesaurus_items_tree <- datamart_thesaurus_items_tree_not_filtered
        
      }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_save_list_file", 
        error_name = paste0("thesaurus - shiny_tree - save_list_file - thesaurus_id = ", input$thesaurus$key), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      r$reload_thesaurus_datatable <- Sys.time()
      r$reload_thesaurus_tree <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$reload_thesaurus_data"))
    })
    
    observeEvent(r$reload_thesaurus_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$reload_thesaurus_datatable"))
      
      req(nrow(r$datamart_thesaurus_items) > 0)
      
      r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("item_id", as.character)
      
      if (input$show_only_used_items) r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items %>%
        dplyr::filter(count_items_rows > 0) %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("item_id", as.character)
      
      editable_cols <- c("name", "display_name")
      searchable_cols <- c("item_id", "name", "display_name", "unit")
      factorize_cols <- c("unit")
      column_widths <- c("action" = "80px", "unit" = "100px", "count_patients_rows" = "80px", "count_items_rows" = "80px")
      sortable_cols <- c("id", "name", "display_name", "count_patients_rows", "count_items_rows")
      centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
      col_names <- get_col_names(table_name = "datamart_thesaurus_items_with_counts", i18n = i18n)
      hidden_cols <- c("id", "thesaurus_id", "item_id", "datetime", "deleted", "modified", "action", "parent_item_id")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$datamart_thesaurus_items_temp,
        output_name = "thesaurus_items", col_names = col_names,
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatatable
      r$datamart_thesaurus_items_datatable_proxy <- DT::dataTableProxy("thesaurus_items", deferUntilFlush = FALSE)
 
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$reload_thesaurus_datatable"))
    })
    
    # Render shinyTree
    output$thesaurus_items_tree <- shinyTree::renderTree({
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - output$thesaurus_items_tree"))
      
      r$reload_thesaurus_tree
      
      # r$datamart_thesaurus_items_tree
      if (input$show_only_used_items) r$datamart_thesaurus_items_tree_filtered
      else r$datamart_thesaurus_items_tree_not_filtered
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - output$thesauurs_items_tree"))
    })
    
    # Updates on datatable data
    observeEvent(input$thesaurus_items_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_items_cell_edit"))
      
      edit_info <- input$thesaurus_items_cell_edit
      r$datamart_thesaurus_items_temp <- DT::editData(r$datamart_thesaurus_items_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$datamart_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_thesaurus_items, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$save_thesaurus_items"))
      
      req(input$thesaurus)
      
      # Reset datamart_thesaurus_user_items variable, with updates
      r$datamart_thesaurus_user_items <- r$datamart_thesaurus_items %>% dplyr::mutate(user_id = r$user_id, .after = id) %>% dplyr::select(-parent_item_id)
      r$datamart_thesaurus_user_items_temp <- r$datamart_thesaurus_items_temp %>% dplyr::mutate(user_id = r$user_id, .after = id) %>% dplyr::select(-parent_item_id)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, table = "thesaurus_items_users", r_table = "datamart_thesaurus_user_items", duplicates_allowed = TRUE, i18n = i18n)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer input$save_thesaurus_items"))
    })
    
    # When a tree element is selected
    observeEvent(input$thesaurus_items_tree, {
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_items_tree"))
      
      r$thesaurus_items_selected_item_id <- get_selected(input$thesaurus_items_tree, format = "classid") %>%
        lapply(attr, "stid") %>% unlist()
      r$thesaurus_items_selected_item_trigger <- Sys.time()
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_items_rows_selected, {
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_items_rows_selected"))
      
      r$thesaurus_items_selected_item_id <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::pull(item_id)
      r$thesaurus_items_selected_item_trigger <- Sys.time()
    })
    
    observeEvent(r$thesaurus_items_selected_item_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$thesaurus_items_selected_item_trigger"))
      
      style <- "display:inline-block; width:200px; font-weight:bold;"
      
      if (is.null(input$thesaurus_items_pivot)) prefix <- "datatable"
      else if (input$thesaurus_items_pivot == "thesaurus_items_table_view") prefix <- "datatable"
      else if (input$thesaurus_items_pivot == "thesaurus_items_tree_view") prefix <- "tree"
 
      if (length(r$thesaurus_items_selected_item_id) == 0) output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI("") 
      req(length(r$thesaurus_items_selected_item_id) > 0)

      if (prefix == "datatable") thesaurus_item <- r$datamart_thesaurus_items_temp %>% dplyr::filter(item_id == r$thesaurus_items_selected_item_id)
      if (prefix == "tree") thesaurus_item <- r$datamart_thesaurus_items %>% dplyr::filter(item_id == r$thesaurus_items_selected_item_id)

      if (nrow(thesaurus_item) == 0) output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI("")    
      req(nrow(thesaurus_item) > 0)
      
      thesaurus_item <- thesaurus_item %>% dplyr::mutate_at("item_id", as.integer)

      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)

      # Which columns contain data
      r$datamart_thesaurus_items_d_var <- ""
      r$datamart_thesaurus_items_cols_not_empty <- ""
      plots <- tagList()
      values_num <- ""
      values <- ""
      amounts <- ""
      rates <- ""
      
      for (var in c("labs_vitals", "orders", "text", "diagnoses")){
        
        values_text <- ""
        
        if (nrow(d[[var]]) > 0){
          all_values_temp <- d[[var]] %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
            dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id")
          
          if (nrow(all_values_temp) > 0){
            all_values <- all_values_temp
            r$datamart_thesaurus_items_d_var <- var 
          }
        }
      }
      
      if (r$datamart_thesaurus_items_d_var == "labs_vitals"){

        if (nrow(all_values %>% dplyr::filter(!is.na(value_num))) > 0){
          values_num <- suppressMessages(
            all_values %>% 
              dplyr::mutate(value_num_text = dplyr::case_when(!is.na(unit) ~ paste0(value_num, " ", unit), TRUE ~ as.character(value_num))) %>%
              dplyr::filter(!is.na(value_num)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value_num_text)
            )
          plots <- tagList(plots, shinyjs::hidden(plotOutput(ns(paste0(prefix, "_value_num_plot")))))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "value_num")
        }

        if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
          
          values <- suppressMessages(
            all_values %>% 
              dplyr::filter(!is.na(value)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% 
              dplyr::pull(value)
            )
          plots <- tagList(plots, shinyjs::hidden(plotOutput(ns(paste0(prefix, "_value_plot")))))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "value")
        }
        
        values_text <- tagList(
          span(i18n$t("numeric_values"), style = style), paste(values_num, collapse = " || "), br(),
          span(i18n$t("values"), style = style), paste(values, collapse = " || "), br())
      }
      
      if (r$datamart_thesaurus_items_d_var == "orders"){
        
        if (nrow(all_values %>% dplyr::filter(!is.na(amount))) > 0){
          
          amounts <- suppressMessages(
            all_values %>% 
              dplyr::mutate(amount_text = dplyr::case_when(!is.na(amount_unit) ~ paste0(amount, " ", amount_unit), TRUE ~ as.character(amount))) %>%
              dplyr::filter(!is.na(amount)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(amount_text)
          )
          plots <- tagList(plots, plotOutput(ns(paste0(prefix, "_amount_plot"))))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "amount")
        }
        
        if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0){
          
          rates <- suppressMessages(
            all_values %>% 
              dplyr::mutate(rate_text = dplyr::case_when(!is.na(rate_unit) ~ paste0(rate, " ", rate_unit), TRUE ~ as.character(rate))) %>%
              dplyr::filter(!is.na(rate)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text)
          )
          plots <- tagList(plots, plotOutput(ns(paste0(prefix, "_rate_plot"))))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "rate")
        }
        
        values_text <- tagList(
          span(i18n$t("rate"), style = style), paste(rates, collapse = " || "), br(),
          span(i18n$t("amount"), style = style), paste(amounts, collapse = " || "), br())
      }
      
      if (r$datamart_thesaurus_items_d_var == "text"){
        
        if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
          
          values <- suppressMessages(
            all_values %>% 
              dplyr::filter(!is.na(value)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% 
              dplyr::pull(value)
          )
          plots <- tagList(plots, shinyjs::hidden(plotOutput(ns("value_plot"))))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "value")
        }
        
        values_text <- tagList(
          span(i18n$t("rate"), style = style), paste(rate, collapse = " || "), br(),
          span(i18n$t("amount"), style = style), paste(amount, collapse = " || "), br())
      }
      
      datamart_thesaurus_items_d_var <- ifelse(r$datamart_thesaurus_items_d_var == "", i18n$t("none_fem"), paste0("d$", r$datamart_thesaurus_items_d_var))
        
      # print(plots)
      
      output[[paste0("thesaurus_", prefix, "_selected_item")]] <- renderUI(tagList(br(), div(
        span(i18n$t("var_containing_item"), style = style), datamart_thesaurus_items_d_var, br(),
        span(i18n$t("thesaurus_id"), style = style), thesaurus_item$thesaurus_id, br(),
        span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
        span(i18n$t("item_id"), style = style), thesaurus_item$item_id, br(),
        span(i18n$t("name"), style = style), thesaurus_item$name, br(),
        span(i18n$t("abbreviation"), style = style), thesaurus_item$display_name, br(),
        span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
        values_text, br(),
        shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "_show_plots")), i18n$t("show_plots")),
        conditionalPanel(condition = paste0("input.", prefix, "_show_plots == true"), ns = ns, br(), plots),
        style = "border:dashed 1px; padding:10px;"
      )))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$thesaurus_items_seeclted_item_trigger"))
    })
    
    observeEvent(input$datatable_show_plots, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$datatable_show_plots"))
      
      if (is.null(input$thesaurus_items_pivot)) prefix <- "datatable"
      else if (input$thesaurus_items_pivot == "thesaurus_items_table_view") prefix <- "datatable"
      else if (input$thesaurus_items_pivot == "thesaurus_items_tree_view") prefix <- "tree"
      
      thesaurus_item <- r$datamart_thesaurus_items %>% dplyr::filter(item_id == r$thesaurus_items_selected_item_id)
      
      # thesaurus_item <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
      
      all_values <- d$labs_vitals %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
        dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% dplyr::select(value, value_num)
      values_num <- all_values %>% dplyr::filter(!is.na(value_num))
      values <- all_values %>% dplyr::filter(!is.na(value))
      
      # print(values_num)
      
      shinyjs::show(paste0(prefix, "_value_num_plot"))
      
      output[[paste0(prefix, "_value_num_plot")]] <- renderPlot({
        values_num %>%
          ggplot2::ggplot(ggplot2::aes(x = value_num)) +
          ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
          ggplot2::labs(x = "", y = "") +
          ggplot2::theme_bw()
      })
      
      # output[[paste0(prefix, "_value_plot")]] <- renderPlot({
      #   values %>%
      #     ggplot2::ggplot(ggplot2::aes(x = value)) +
      #     ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), stat = "count", colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
      #     # ggplot2::labs(x = i18n$t(col), y = "") +
      #     ggplot2::theme_bw()
      # })
      
      # for (column_name in c("value", "value_num", "amount", "rate")){
      #   if (col %in% r$datamart_thesaurus_items_cols_not_empty){
      #     print(col)
      #     column_name <- col
      #     output[[paste0(col, "_plot")]] <- renderPlot({
      #       all_values %>%
      #         ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(column_name))) +
      #         ggplot2::geom_histogram(ggplot2::aes(y = (..count..)), colour = "#FFFFFF", alpha = 0.6, fill = "#1F68AE") +
      #         # ggplot2::scale_x_continuous(breaks = ) +
      #         ggplot2::labs(x = i18n$t(col), y = "") +
      #         ggplot2::theme_bw()
      #     })
      #   }
      # }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer input$datatable_show_plots"))
    })
    
    # --- --- --- --- --
    # Items mapping ----
    # --- --- --- --- --
    
      # --- --- --- --- --- --
      ## Create a mapping ----
      # --- --- --- --- --- --
      
      observeEvent(input$thesaurus_mapping1, {
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_mapping1"))
        r$thesaurus_mapping_reload <- paste0(Sys.time(), "_mapping1")
      })
      observeEvent(input$thesaurus_mapping2, {
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_mapping2"))
        r$thesaurus_mapping_reload <- paste0(Sys.time(), "_mapping2")
      })
      
      observeEvent(r$thesaurus_mapping_reload, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$thesaurus_mapping_reload"))
        
        if (grepl("mapping1", r$thesaurus_mapping_reload)) mapping <- "mapping1"
        else if (grepl("mapping2", r$thesaurus_mapping_reload)) mapping <- "mapping2"
        
        req(length(input[[paste0("thesaurus_", mapping)]]$key) > 0)
        
        r[[paste0("datamart_thesaurus_items_", mapping)]] <- DBI::dbGetQuery(r$db, paste0(
          "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.unit, t.datetime, t.deleted
            FROM thesaurus_items t
            WHERE t.thesaurus_id = ", input[[paste0("thesaurus_", mapping)]]$key, " AND t.deleted IS FALSE
            ORDER BY t.item_id")) %>% tibble::as_tibble() %>% dplyr::mutate(action = "")
        
        # Get user's modifications on items names & abbreviations
        
        r[[paste0("datamart_thesaurus_user_items_", mapping)]] <- DBI::dbGetQuery(r$db, paste0(
          "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.deleted
            FROM thesaurus_items_users t
            WHERE t.thesaurus_id = ", input[[paste0("thesaurus_", mapping)]]$key, " AND t.user_id = ", r$user_id ," AND t.deleted IS FALSE
            ORDER BY t.item_id")) %>% tibble::as_tibble()
        
        # Merge tibbles
        r[[paste0("datamart_thesaurus_items_", mapping)]] <-
          r[[paste0("datamart_thesaurus_items_", mapping)]] %>%
          dplyr::left_join(
            r[[paste0("datamart_thesaurus_user_items_", mapping)]] %>% dplyr::select(item_id, new_name = name, new_display_name = display_name),
            by = "item_id"
          ) %>%
          dplyr::mutate(
            name = dplyr::case_when(!is.na(new_name) ~ new_name, TRUE ~ name),
            display_name = dplyr::case_when(!is.na(new_display_name) ~ new_display_name, TRUE ~ display_name)
          ) %>%
          dplyr::select(-new_name, -new_display_name)
        
        count_items_rows <- tibble::tibble()
        count_patients_rows <- tibble::tibble()
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input[[paste0("thesaurus_", mapping)]]$key,
          datamart_id = r$chosen_datamart, category = "count_items_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus", 
            error_name = paste0("thesaurus - create_datatable_cache - count_items_rows - fail_load_thesaurus - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        # Add count_items_rows in the cache & get it if already in the cache
        tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, i18n = i18n, thesaurus_id = input[[paste0("thesaurus_", mapping)]]$key,
          datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_thesaurus", 
            error_name = paste0("thesaurus - create_datatable_cache - count_patients_rows - fail_load_thesaurus - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_thesaurus", "severeWarning", i18n = i18n, ns = ns)
        req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
  
        # Transform count_rows cols to integer, to be sortable
        r[[paste0("datamart_thesaurus_items_", mapping)]] <- r[[paste0("datamart_thesaurus_items_", mapping)]] %>%
          dplyr::left_join(count_items_rows, by = "item_id") %>%
          dplyr::left_join(count_patients_rows, by = "item_id") %>%
          dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
          dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action") %>%
          dplyr::arrange(name)
        
        r[[paste0("datamart_thesaurus_items_", mapping, "_temp")]] <- r[[paste0("datamart_thesaurus_items_", mapping)]] %>%
          dplyr::mutate(modified = FALSE) %>%
          dplyr::mutate_at("item_id", as.character)
        
        editable_cols <- c("name", "display_name")
        searchable_cols <- c("item_id", "name", "display_name", "unit")
        factorize_cols <- c("unit")
        column_widths <- c("id" = "80px", "action" = "80px", "unit" = "100px", "count_patients_rows" = "80px", "count_items_rows" = "80px")
        sortable_cols <- c("id", "name", "display_name", "count_patients_rows", "count_items_rows")
        centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
        col_names <- get_col_names(table_name = "mapping_thesaurus_items_with_counts", i18n = i18n)
        hidden_cols <- c("id", "thesaurus_id", "item_id", "display_name", "unit", "count_patients_rows", "datetime", "deleted", "modified", "action")
   
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0("datamart_thesaurus_items_", mapping, "_temp")]],
          output_name = paste0("thesaurus_", mapping, "_dt"), col_names = col_names, datatable_dom = "<'top't><'bottom'p>",
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$thesaurus_mapping_reload"))
      })
      
      # When a row is selected
      observeEvent(input$thesaurus_mapping1_dt_rows_selected, {
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_mapping1_dt_rows_selected"))
        r$thesaurus_mapping_item_info <- paste0(Sys.time(), "_mapping1")
      })
      observeEvent(input$thesaurus_mapping2_dt_rows_selected, {
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$thesaurus_mapping2_dt_rows_selected"))
        r$thesaurus_mapping_item_info <- paste0(Sys.time(), "_mapping2")
      })
      
      observeEvent(r$thesaurus_mapping_item_info, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$thesaurus_mapping_item_info"))
        
        if (grepl("mapping1", r$thesaurus_mapping_item_info)) mapping <- "mapping1"
        else if (grepl("mapping2", r$thesaurus_mapping_item_info)) mapping <- "mapping2"
        
        style <- "display:inline-block; width:200px; font-weight:bold;"
        
        thesaurus_item <- r[[paste0("datamart_thesaurus_items_", mapping, "_temp")]][input[[paste0("thesaurus_", mapping, "_dt_rows_selected")]], ] %>% dplyr::mutate_at("item_id", as.integer)
        
        thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
        
        # Which columns contain data
        r[[paste0("datamart_thesaurus_items_", mapping, "_d_var")]] <- ""
        r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- ""
        plots <- tagList()
        values_num <- ""
        values <- ""
        amounts <- ""
        rates <- ""
        
        for (var in c("labs_vitals", "orders", "text", "diagnoses")){
          
          values_text <- ""
          
          if (nrow(d[[var]]) > 0){
            all_values_temp <- d[[var]] %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
              dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id")
            
            if (nrow(all_values_temp) > 0){
              all_values <- all_values_temp
              r[[paste0("datamart_thesaurus_items_", mapping, "_d_var")]] <- var 
            }
          }
        }
  
        if (r[[paste0("datamart_thesaurus_items_", mapping, "_d_var")]] == "labs_vitals"){
          if (nrow(all_values %>% dplyr::filter(!is.na(value_num))) > 0){
            values_num <- suppressMessages(
              all_values %>% 
                dplyr::mutate(value_num_text = dplyr::case_when(!is.na(unit) ~ paste0(value_num, " ", unit), TRUE ~ as.character(value_num))) %>%
                dplyr::filter(!is.na(value_num)) %>%
                dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value_num_text)
            )
            # plots <- tagList(plots, plotOutput(ns("value_num_plot")))
            r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- c(r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]], "value_num")
          }
          
          if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
            
            values <- suppressMessages(
              all_values %>% 
                dplyr::filter(!is.na(value)) %>%
                dplyr::slice_sample(n = 5, replace = TRUE) %>% 
                dplyr::pull(value)
            )
            # plots <- tagList(plots, plotOutput(ns("value_plot")))
            r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- c(r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]], "value")
          }
          
          values_text <- tagList(
            span(i18n$t("numeric_values"), style = style), paste(values_num, collapse = " || "), br(),
            span(i18n$t("values"), style = style), paste(values, collapse = " || "), br())
        }
  
        if (r[[paste0("datamart_thesaurus_items_", mapping, "_d_var")]] == "orders"){
          
          if (nrow(all_values %>% dplyr::filter(!is.na(amount))) > 0){
            
            amounts <- suppressMessages(
              all_values %>% 
                dplyr::mutate(amount_text = dplyr::case_when(!is.na(amount_unit) ~ paste0(amount, " ", amount_unit), TRUE ~ as.character(amount))) %>%
                dplyr::filter(!is.na(amount)) %>%
                dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(amount_text)
            )
            # plots <- tagList(plots, plotOutput(ns("amount_plot")))
            r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- c(r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]], "amount")
          }
          
          if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0){
            
            rates <- suppressMessages(
              all_values %>% 
                dplyr::mutate(rate_text = dplyr::case_when(!is.na(rate_unit) ~ paste0(rate, " ", rate_unit), TRUE ~ as.character(rate))) %>%
                dplyr::filter(!is.na(rate)) %>%
                dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text)
            )
            # plots <- tagList(plots, plotOutput(ns("rate_plot")))
            r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- c(r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]], "rate")
          }
          
          values_text <- tagList(
            span(i18n$t("rate"), style = style), paste(rates, collapse = " || "), br(),
            span(i18n$t("amount"), style = style), paste(amounts, collapse = " || "), br())
        }
        
        if (r[[paste0("datamart_thesaurus_items_", mapping, "_d_var")]] == "text"){
          
          if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
            
            values <- suppressMessages(
              all_values %>% 
                dplyr::filter(!is.na(value)) %>%
                dplyr::slice_sample(n = 5, replace = TRUE) %>% 
                dplyr::pull(value)
            )
            # plots <- tagList(plots, plotOutput(ns("value_plot")))
            r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]] <- c(r[[paste0("datamart_thesaurus_items_", mapping, "_cols_not_empty")]], "value")
          }
          
          values_text <- tagList(
            span(i18n$t("rate"), style = style), paste(rate, collapse = " || "), br(),
            span(i18n$t("amount"), style = style), paste(amount, collapse = " || "), br())
        }
        
        output[[paste0("thesaurus_selected_item_", mapping)]] <- renderUI(tagList(div(
          span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
          span(i18n$t("item_id"), style = style), thesaurus_item$item_id, br(),
          span(i18n$t("name"), style = style), thesaurus_item$name, br(),
          span(i18n$t("abbreviation"), style = style), thesaurus_item$display_name, br(),
          span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
          values_text
        )))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$thesaurus_mapping_item_info"))
      })
      
      # When a mapping id added
      
      observeEvent(input$add_mapping, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$add_mapping"))
        
        req(length(input$thesaurus_mapping1_dt_rows_selected) > 0)
        req(length(input$thesaurus_mapping2_dt_rows_selected) > 0)
        req(nrow(r$datamart_thesaurus_items_mapping1_temp[input$thesaurus_mapping1_dt_rows_selected, ]) > 0)
        req(nrow(r$datamart_thesaurus_items_mapping2_temp[input$thesaurus_mapping2_dt_rows_selected, ]) > 0)
        
        item_1 <- r$datamart_thesaurus_items_mapping1_temp[input$thesaurus_mapping1_dt_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
        item_2 <- r$datamart_thesaurus_items_mapping2_temp[input$thesaurus_mapping2_dt_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
        
        # Check if mapping already added in database
        
        check_duplicates <- FALSE
        
        sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items_mapping WHERE ",
          "thesaurus_id_1 = {item_1$thesaurus_id} AND item_id_1 = {item_1$item_id} AND ",
          "thesaurus_id_2 = {item_2$thesaurus_id} AND item_id_2 = {item_2$item_id} AND ",
          "relation_id = {as.integer(input$mapping_type)} AND ",
          "category = 'user_added_mapping' AND deleted IS FALSE"), .con = r$db)
        existing_mapping <- DBI::dbGetQuery(r$db, sql)
        
        if (nrow(existing_mapping) > 0) show_message_bar(output, 3, "thesaurus_mapping_already_exists", "severeWarning", i18n, ns = ns)
        req(nrow(existing_mapping) == 0)
        
        if (item_1$thesaurus_id == item_2$thesaurus_id & item_1$item_id == item_2$item_id) show_message_bar(output, 3, "thesaurus_mapping_same_items", "severeWarning", i18n, ns = ns)
        req(item_1$thesaurus_id != item_2$thesaurus_id | item_1$item_id != item_2$item_id)
        
        last_row <- get_last_row(r$db, "thesaurus_items_mapping")

        # Add new mapping to r$thesaurus_added_mappings

        new_row <- tibble::tribble(~id, ~category, ~thesaurus_id_1, ~item_id_1, ~thesaurus_id_2, ~item_id_2, ~relation_id, ~creator_id, ~datetime, ~deleted,
          last_row + 1, "user_added_mapping", item_1$thesaurus_id, item_1$item_id, item_2$thesaurus_id, item_2$item_id,
          as.integer(input$mapping_type), r$user_id, as.character(Sys.time()), FALSE)

        r$thesaurus_added_mappings <- r$thesaurus_added_mappings %>% dplyr::bind_rows(new_row)

        # Add new mapping to database

        DBI::dbAppendTable(r$db, "thesaurus_items_mapping", new_row)

        # Notify user
        show_message_bar(output, 3, "thesaurus_mapping_added", "success", i18n, ns = ns)

        # Update datatables
        r$reload_thesaurus_added_mappings_datatable <- Sys.time()
        # r$reload_thesaurus_evaluate_mappings_datatable <- Sys.time()

        r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
          dplyr::bind_rows(
            tibble::tibble(
              id = last_row + 1,
              thesaurus_name_1 = r$thesaurus %>% dplyr::filter(id == item_1$thesaurus_id) %>% dplyr::pull(name),
              item_id_1 = as.character(item_1$item_id),
              relation = dplyr::case_when(as.integer(input$mapping_type) == 1 ~ i18n$t("equivalent_to"),
                as.integer(input$mapping_type) == 2 ~ i18n$t("included_in"), as.integer(input$mapping_type) == 3 ~ i18n$t("include")),
              thesaurus_name_2 = r$thesaurus %>% dplyr::filter(id == item_2$thesaurus_id) %>% dplyr::pull(name),
              item_id_2 = as.character(item_2$item_id),
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

        DT::replaceData(r$datamart_thesaurus_items_evaluate_mappings_datatable_proxy, r$datamart_thesaurus_items_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)

        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer input$add_mapping"))
      })
      
      # Table to summarize added mappings
      
      observeEvent(r$chosen_datamart, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$chosen_datamart 1"))
        
        r$thesaurus_added_mappings <- tibble::tibble(id = integer(), category = character(), thesaurus_id_1 = integer(), item_id_1 = integer(), 
          thesaurus_id_2 = integer(), item_id_2 = integer(), relation_id = integer(), creator_id = integer(), datetime = character(), deleted = logical())
        
        output$thesaurus_selected_item_mapping1 <- renderText("")
        output$thesaurus_selected_item_mapping2 <- renderText("")
        
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "thesaurus_mapping1_dt", datatable_dom = "")
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "thesaurus_mapping2_dt", datatable_dom = "")
        
        r$reload_thesaurus_added_mappings_datatable <- Sys.time()
        r$reload_thesaurus_evaluate_mappings_datatable <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$chosen_datamart 1"))
      })
      
      observeEvent(r$reload_thesaurus_added_mappings_datatable, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$reload_thesaurus_added_mappings_datatable"))
        
        r$thesaurus_added_mappings_temp <- r$thesaurus_added_mappings %>%
          dplyr::mutate_at(c("item_id_1", "item_id_2"), as.character) %>%
          dplyr::mutate(relation = dplyr::case_when(relation_id == 1 ~ i18n$t("equivalent_to"), relation_id == 2 ~ i18n$t("included_in"), relation_id == 3 ~ i18n$t("include"))) %>%
          dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_1 = id, thesaurus_name_1 = name), by = "thesaurus_id_1") %>%
          dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_2 = id, thesaurus_name_2 = name), by = "thesaurus_id_2") %>%
          dplyr::relocate(thesaurus_name_1, .after = "thesaurus_id_1") %>%
          dplyr::relocate(thesaurus_name_2, .after = "thesaurus_id_2") %>%
          dplyr::select(-thesaurus_id_1, -thesaurus_id_2, -relation_id) %>%
          dplyr::relocate(relation, .after = item_id_1) %>%
          dplyr::arrange(dplyr::desc(datetime))
        
        centered_cols <- c("id", "item_id_1", "thesaurus_name_1", "item_id_2", "thesaurus_name_2", "relation")
        col_names <- get_col_names(table_name = "datamart_thesaurus_items_mapping", i18n = i18n)
        hidden_cols <- c("id", "creator_id", "datetime", "deleted", "category")
  
        # Render datatable
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$thesaurus_added_mappings_temp, datatable_dom = "<'top't><'bottom'p>",
          output_name = "thesaurus_added_mappings", col_names = col_names, centered_cols = centered_cols, hidden_cols = hidden_cols)
  
        # Create a proxy for datatatable
        r$thesaurus_added_mappings_datatable_proxy <- DT::dataTableProxy("thesaurus_added_mappings", deferUntilFlush = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$reload_thesaurus_added_mappings_datatable"))
      })
      
    # --- --- --- --- - --
    ## Manage mapping ----
    # --- --- --- --- - --
      
    # Reload datatable  
    
    observeEvent(r$reload_thesaurus_evaluate_mappings_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$reload_thesaurus_evaluate_mappings_datatable"))
      
      # Get all items mappings
      
      data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      thesaurus_ids <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
          grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::pull(id)
      
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items_mapping WHERE (thesaurus_id_1 IN ({thesaurus_ids*}) OR thesaurus_id_2 IN ({thesaurus_ids*})) ",
        "AND category = 'user_added_mapping' AND deleted IS FALSE"), .con = r$db)
      r$datamart_thesaurus_items_evaluate_mappings <- DBI::dbGetQuery(r$db, sql)
      
      action_col <- tibble::tibble()
      
      # Join with evaluations
      
      sql <- glue::glue_sql(paste0("SELECT * FROM thesaurus_items_mapping_evals WHERE mapping_id IN ({r$datamart_thesaurus_items_evaluate_mappings %>% dplyr::pull(id)*}) ",
        "AND deleted IS FALSE"), .con = r$db)
      thesaurus_mapping_evals <- DBI::dbGetQuery(r$db, sql)
      
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
        dplyr::left_join(thesaurus_mapping_evals %>% dplyr::select(eval_id = id, id = mapping_id, evaluation_id), by = "id") %>%
        dplyr::group_by(id, thesaurus_id_1, item_id_1, thesaurus_id_2, item_id_2, relation_id, creator_id, datetime, deleted) %>%
        dplyr::summarize(
          positive_evals = sum(evaluation_id == 1, na.rm = TRUE),
          negative_evals = sum(evaluation_id == 2, na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          positive_evals = ifelse(positive_evals > 0, positive_evals, 0),
          negative_evals = ifelse(negative_evals > 0, negative_evals, 0)
        ) %>%
        dplyr::mutate_at(c("item_id_1", "item_id_2"), as.character) %>%
        dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, creator_name = paste0(firstname, " ", lastname)), by = "creator_id") %>%
        dplyr::relocate(creator_name, .after = "creator_id") %>%
        dplyr::select(-creator_id) %>%
        dplyr::left_join(thesaurus_mapping_evals %>% 
            dplyr::filter(creator_id == r$user_id) %>%
            dplyr::select(id = mapping_id, user_evaluation_id = evaluation_id), by = "id")
      
      # Create or get cache for action column
      tryCatch(action_col <- create_datatable_cache(output = output, r = r, i18n = i18n, module_id = id, thesaurus_id = thesaurus_ids, category = "thumbs_and_delete"))
      
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
        dplyr::left_join(action_col %>% dplyr::select(id, action), by = "id") %>%
        dplyr::relocate(action, .after = "negative_evals")
      
      # Update action buttons with user evaluations
      
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
        dplyr::mutate(
          positive_eval_button_background_color = dplyr::case_when(
            user_evaluation_id == 1 ~ "#5FBAFF",
            user_evaluation_id == 2 ~ "#E8E9EC"
          ),
          positive_eval_button_color = dplyr::case_when(
            user_evaluation_id == 1 ~ "white",
            user_evaluation_id == 2 ~ "black"
          ),
          negative_eval_button_background_color = dplyr::case_when(
            user_evaluation_id == 1 ~ "#E8E9EC",
            user_evaluation_id == 2 ~ "#FF434C"
          ),
          negative_eval_button_color = dplyr::case_when(
            user_evaluation_id == 1 ~ "black",
            user_evaluation_id == 2 ~ "white"
          )
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(action = dplyr::case_when(
          !is.na(user_evaluation_id) ~ as.character(tagList(
            shiny::actionButton(paste0("positive_eval_", id), "", icon = icon("thumbs-up"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_positive', this.id, {priority: 'event'})"),
              style = paste0("background-color:", positive_eval_button_background_color, "; color:", positive_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
            shiny::actionButton(paste0("negative_eval_", id), "", icon = icon("thumbs-down"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_evaluated_negative', this.id, {priority: 'event'})"),
              style = paste0("background-color:", negative_eval_button_background_color, "; color:", negative_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px;")),
            shiny::actionButton(paste0("remove_", id), "", icon = icon("trash-alt"),
              onclick = paste0("Shiny.setInputValue('", !!id, "-item_mapping_deleted_pressed', this.id, {priority: 'event'})"),
              style = "background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px;")
          )),
          TRUE ~ action
        )) %>%
        dplyr::ungroup() %>%
        dplyr::select(-positive_eval_button_background_color, -positive_eval_button_color, -negative_eval_button_background_color, -negative_eval_button_color)
      
      # Get thesaurus names instead of IDs
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
        dplyr::mutate(relation = dplyr::case_when(relation_id == 1 ~ i18n$t("equivalent_to"), relation_id == 2 ~ i18n$t("included_in"), relation_id == 3 ~ i18n$t("include"))) %>%
        dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_1 = id, thesaurus_name_1 = name), by = "thesaurus_id_1") %>%
        dplyr::left_join(r$thesaurus %>% dplyr::select(thesaurus_id_2 = id, thesaurus_name_2 = name), by = "thesaurus_id_2") %>%
        dplyr::relocate(thesaurus_name_1, .after = "thesaurus_id_1") %>%
        dplyr::relocate(thesaurus_name_2, .after = "thesaurus_id_2") %>%
        dplyr::select(-thesaurus_id_1, -thesaurus_id_2, -relation_id) %>%
        dplyr::relocate(relation, .after = item_id_1)
      
      # Select only mappings without evaluation
      
      # Render datatable
      
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
        dplyr::arrange(dplyr::desc(id)) %>% dplyr::mutate(modified = FALSE)
      
      searchable_cols <- c("thesaurus_name_1", "item_id_1", "thesaurus_name_2", "item_id_2", "relation", "creator_name", "positive_evals", "negative_evals")
      factorize_cols <- c("thesaurus_name_1", "thesaurus_name_2", "relation", "creator_name")
      sortable_cols <- c("thesaurus_name_1", "item_id_1", "thesaurus_name_2", "item_id_2", "relation", "creator_name", "datetime", "positive_evals", "negative_evals")
      centered_cols <- c("id", "datetime", "action", "thesaurus_name_1", "item_id_1", "thesaurus_name_2", "item_id_2", "creator_name", "relation")
      col_names <- get_col_names(table_name = "datamart_thesaurus_items_mapping_evals", i18n = i18n)
      hidden_cols <- c("id", "deleted", "modified", "user_evaluation_id")
      column_widths <- c("action" = "80px", "datetime" = "130px")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r$datamart_thesaurus_items_evaluate_mappings,
        output_name = "thesaurus_evaluate_mappings", hidden_cols = hidden_cols, centered_cols = centered_cols, searchable_cols = searchable_cols,
        col_names = col_names, filter = TRUE, factorize_cols = factorize_cols, sortable_cols = sortable_cols, column_widths = column_widths,
        selection = "multiple"
      )
      
      # Create a proxy for datatatable
      r$datamart_thesaurus_items_evaluate_mappings_datatable_proxy <- DT::dataTableProxy("thesaurus_evaluate_mappings", deferUntilFlush = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$reload_thesaurus_evaluate_mappings_datatable"))
    })
      
    # When an evaluation button is clicked
      
    observeEvent(input$item_mapping_evaluated_positive, {
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$item_mapping_evaluated_positive"))
      r$item_mapping_evaluation_type <- "positive"
      r$item_mapping_evaluation_update <- Sys.time()
    })
    
    observeEvent(input$item_mapping_evaluated_negative, {
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$item_mapping_evaluated_positive"))
      r$item_mapping_evaluation_type <- "negative"
      r$item_mapping_evaluation_update <- Sys.time()
    })
    
    observeEvent(r$item_mapping_evaluation_update, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$item_mapping_evaluation_update"))
      
      prefix <- r$item_mapping_evaluation_type
      new_evaluation_id <- switch(r$item_mapping_evaluation_type, "positive" = 1L, "negative" = 2L)
      
      link_id <- as.integer(substr(input[[paste0("item_mapping_evaluated_", prefix)]], nchar(paste0(prefix, "_eval_")) + 1, nchar(input[[paste0("item_mapping_evaluated_", prefix)]])))
      
      # If we cancel current evaluation
      current_evaluation_id <- r$datamart_thesaurus_items_evaluate_mappings %>%
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
      
      r$datamart_thesaurus_items_evaluate_mappings <- r$datamart_thesaurus_items_evaluate_mappings %>%
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
      DT::replaceData(r$datamart_thesaurus_items_evaluate_mappings_datatable_proxy, r$datamart_thesaurus_items_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer r$item_mapping_evaluation_update"))
    })
    
    # Delete a row or multiple rows in datatable
    
    mappings_delete_prefix <- "mappings"
    mappings_dialog_title <- "mapping_delete"
    mappings_dialog_subtext <- "mapping_delete_subtext"
    mappings_react_variable <- "mappings_delete_confirm"
    mappings_table <- "thesaurus_items_mapping"
    mappings_r_table <- "datamart_thesaurus_items_evaluate_mappings"
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
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$item_mapping_deleted_pressed"))

      r$delete_mappings <- as.integer(substr(input$item_mapping_deleted_pressed, nchar("delete_") + 1, 100))
      r[[mappings_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$datamart_thesaurus_items_evaluate_mappings_datatable_proxy, r$datamart_thesaurus_items_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$mapping_delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$mapping_delete_selection"))

      req(length(input$thesaurus_evaluate_mappings_rows_selected) > 0)

      r$delete_mappings <- r$datamart_thesaurus_items_evaluate_mappings[input$thesaurus_evaluate_mappings_rows_selected, ] %>% dplyr::pull(id)
      r[[mappings_delete_variable]] <- TRUE
    })
    
    # Reload data
    
    observeEvent(r[[mappings_reload_variable]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer r$reload_mappings_evals"))

      # Reload datatable
      DT::replaceData(r$datamart_thesaurus_items_evaluate_mappings_datatable_proxy, r$datamart_thesaurus_items_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    })

    # Save updates
    
    observeEvent(input$save_mappings_evaluation, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_thesaurus - observer input$save_mappings_evaluation"))
      
      # Update database
      
      if (nrow(r$datamart_thesaurus_items_evaluate_mappings %>% dplyr::filter(modified)) == 0) show_message_bar(output, 2, "modif_saved", "success", i18n = i18n, ns = ns)
      
      req(nrow(r$datamart_thesaurus_items_evaluate_mappings %>% dplyr::filter(modified)) > 0)
      
      sql <- glue::glue_sql(paste0("DELETE FROM thesaurus_items_mapping_evals WHERE creator_id = {r$user_id} ",
        "AND mapping_id IN ({r$datamart_thesaurus_items_evaluate_mappings %>% dplyr::filter(modified) %>% dplyr::pull(id)*})"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      if (nrow(r$datamart_thesaurus_items_evaluate_mappings %>% dplyr::filter(modified, !is.na(user_evaluation_id))) > 0){
        new_data <- r$datamart_thesaurus_items_evaluate_mappings %>%
          dplyr::filter(modified, !is.na(user_evaluation_id)) %>%
          dplyr::select(mapping_id = id, evaluation_id = user_evaluation_id) %>%
          dplyr::mutate(id = get_last_row(r$db, "thesaurus_items_mapping_evals") + 1:dplyr::n(), .before = "mapping_id") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE) %>%
          dplyr::relocate(evaluation_id, .after = "creator_id")
        
        DBI::dbAppendTable(r$db, "thesaurus_items_mapping_evals", new_data)
      }
      
      show_message_bar(output, 2, "modif_saved", "success", i18n = i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_thesaurus - observer input$save_mappings_evaluation"))
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_evaluate_mappings_rows_selected, {
      
    })
  })
}
