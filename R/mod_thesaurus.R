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
  
  cards <- c("thesaurus_items_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = "EN", words = words))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "thesaurus_main", text = i18n$t("Thesaurus"))
    ), maxDisplayedItems = 3),
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "items_card", itemKey = "items_card", headerText = i18n$t("All items")),
          shiny.fluent::PivotItem(id = "categories_card", itemKey = "categories_card", headerText = i18n$t("Categories")),
          shiny.fluent::PivotItem(id = "conversions_card", itemKey = "conversions_card", headerText = i18n$t("Conversions")),
          shiny.fluent::PivotItem(id = "create_items_card", itemKey = "create_items_card", headerText = i18n$t("Create items"))
        )
      )
    ),
    forbidden_cards,
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("Choose a damatart in the dropdown on the left-side of the page"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(
      div(
        id = ns("items_card"),
        make_card(i18n$t("Items"),
          div(
            div(
              make_combobox(language = "EN", ns = ns, label = "thesaurus", width = "300px", words = words, allowFreeform = FALSE, multiSelect = FALSE), br(),
              DT::DTOutput(ns("thesaurus_items")),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_thesaurus_items"), i18n$t("Save")), " ",
              shiny.fluent::DefaultButton.shinyInput(ns("reload_thesaurus_cache"), i18n$t("Reload cache")),
              br(),
              uiOutput(ns("thesaurus_selected_item"))
            ), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                tags$ul(
                  tags$li("Mettre une confirmation pour la recharge du cache"),
                  tags$li("Créer un cache pour la prévisualisation des données"),
                  tags$li("Faire en sorte que les modifications de nom d'affichage et d'unité ne concernent que ce datamart"),
                  tags$li("Les modifications globales de nom et d'unité se font dans les paramètres, par Thésaurus, hors Datamart")
                )
              ),
              messageBarType = 0)
            )
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("categories_card"),
        make_card(i18n$t("Categories"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Plusieurs choses seront possibles ici :",
                  tags$ul(
                    tags$li("Créer de nouvelles catégories"),
                    tags$li("Renommer les catégories existantes"),
                    tags$li("Changer les items de catégorie")
                  )  
                )
              ),
              messageBarType = 0)
            )
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("conversions_card"),
        make_card(i18n$t("Conversions"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Il sera possible de convertir les variables dans différentes unités"),
                p("Faut-il laisser possible le changement de l'unité, en changeant le texte, dans \"Tous les items\" ?")
              ),
              messageBarType = 0)
            )
          )
        ), br()
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("create_items_card"),
        make_card(i18n$t("Create items"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("In progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Le principe ici est de créer de nouveaux items à partir des items existants."),
                p("Par exemple, l'item diurèse est rarement disponible, on doit rassembler différents items tels que \"diurèse sur SUD\", \"néphrostomie\" etc"),
                p("Il nous faudra :",
                  tags$ul(
                    tags$li("Un éditeur pour créer les scripts, les enregistrer"),
                    tags$li("Un tableau pour gérer les scripts (supprimer, changer de nom)"),
                    tags$li("Un éxécuteur pour tester les scripts"),
                    tags$li("Si un script fonctionne, il sera lancé au chargement du datamart, et le nouvel item sera ajouté aux items classiques"),
                    tags$li("Tout ceci nécessite de créer une nouvelle table dans la BDD de l'appli")
                  )
                )
              ),
              messageBarType = 0)
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
mod_thesaurus_server <- function(id = character(), r = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    ##########################################
    # Show or hide cards                     #
    ##########################################
    
    cards <- c("items_card", "categories_card", "conversions_card", "create_items_card")
    # show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    ##########################################
    # Render thesaurus UI                      #
    ##########################################
    
    # Delete when "thesaurus_items_card" will be added in r$user_accesses
    
    observeEvent(input$current_tab, {
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # End of delete
    
    observeEvent(r$chosen_datamart, {
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        # if ("thesaurus_items_card" %in% r$user_accesses) shinyjs::show("thesaurus_items_card")
        # else shinyjs::show("thesaurus_items_card_forbidden")
        shinyjs::show("items_card")
        sapply(c("categories_card", "conversions_card", "create_items_card"), shinyjs::hide)
      }
      
      data_source <- r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id)
      
      # Multiple cases
      # Only one ID, so it's the beginning and the end
      # Last ID, so it's the end
      # ID between begin and last, so separated by commas
      thesaurus <- r$thesaurus %>% dplyr::filter(grepl(paste0("^", data_source, "$"), data_source_id) | 
        grepl(paste0(", ", data_source, "$"), data_source_id) | grepl(paste0("^", data_source, ","), data_source_id)) %>% dplyr::arrange(name)
      shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list(data = thesaurus, key_col = "id", text_col = "name", words = r$words), value = NULL)
      
      if (length(r$datamart_thesaurus_items_temp) > 0) r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items_temp %>% dplyr::slice(0)
      
      # Reset UI of selected item
      output$thesaurus_selected_item <- renderUI("")
    })
    
    observeEvent(input$thesaurus, {
      
      r$reload_thesaurus_datatable <- Sys.time()
    })
    
    observeEvent(r$reload_thesaurus_datatable, {
      
      req(length(input$thesaurus$key) > 0)
      
      r$datamart_thesaurus_items <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted
          FROM thesaurus_items t
          WHERE t.thesaurus_id = ", input$thesaurus$key, " AND t.deleted IS FALSE
          ORDER BY t.id")) %>% tibble::as_tibble() %>% dplyr::mutate(action = "")
      
      count_items_rows <- tibble::tibble()
      count_patients_rows <- tibble::tibble()
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_items_rows <- create_datatable_cache(output = output, r = r, language = "EN", thesaurus_id = input$thesaurus$key,
        datamart_id = r$chosen_datamart, category = "count_items_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("modules - create_datatable_cache - count_items_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = "EN"))
      
      # Add count_items_rows in the cache & get it if already in the cache
      tryCatch(count_patients_rows <- create_datatable_cache(output = output, r = r, language = "EN", thesaurus_id = input$thesaurus$key,
        datamart_id = as.integer(r$chosen_datamart), category = "count_patients_rows"),
          error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
            error_name = paste0("modules - create_datatable_cache - count_patients_rows - fail_load_datamart - id = ", r$chosen_datamart), category = "Error", error_report = toString(e), language = "EN"))
      
      if (nrow(count_items_rows) == 0 | nrow(count_patients_rows) == 0) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", "EN", words = r$words)
      req(nrow(count_items_rows) != 0, nrow(count_patients_rows) != 0)
      
      # Transform count_rows cols to integer, to be sortable
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>%
        dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
        dplyr::left_join(count_items_rows, by = "item_id") %>%
        dplyr::left_join(count_patients_rows, by = "item_id") %>%
        dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
      
      # Filter on count_items_rows > 0
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
      
      r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("item_id", as.character)
      
      editable_cols <- c("display_name", "unit")
      searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
      factorize_cols <- c("category", "unit")
      column_widths <- c("id" = "80px", "action" = "80px", "display_name" = "300px", "unit" = "100px", "category" = "400px")
      sortable_cols <- c("id", "item_id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
      centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
      col_names <- get_col_names(table_name = "datamart_thesaurus_items_with_counts", language = "EN", words = r$words)
      hidden_cols <- c("id", "name", "thesaurus_id", "item_id", "datetime", "deleted", "modified", "action")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, language = "EN", data = r$datamart_thesaurus_items_temp,
        output_name = "thesaurus_items", col_names =  col_names,
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatatable
      r$datamart_thesaurus_items_datatable_proxy <- DT::dataTableProxy("thesaurus_items", deferUntilFlush = FALSE)
    })
    
    # Reload thesarus cache
    
    observeEvent(input$reload_thesaurus_cache, {
      
      req(length(input$thesaurus$key) > 0)
      
      # Delete old cache
      
      sql <- glue::glue_sql("SELECT t.id FROM thesaurus_items t WHERE t.thesaurus_id = {input$thesaurus$key} AND t.deleted IS FALSE" , .con = r$db)
      ids_to_del <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
      
      sql <- glue::glue_sql("DELETE FROM cache WHERE category IN ('count_patients_rows', 'count_items_rows') 
        AND link_id IN ({ids_to_del*}) AND link_id_bis = {r$chosen_datamart}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      r$reload_thesaurus_datatable <- Sys.time()
    })
    
    # Reload datatable
    observeEvent(r$datamart_thesaurus_items_temp, {
      
      if (length(r$datamart_thesaurus_items_datatable_proxy) > 0) DT::replaceData(
        r$datamart_thesaurus_items_datatable_proxy, r$datamart_thesaurus_items_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$thesaurus_items_cell_edit, {
      
      edit_info <- input$thesaurus_items_cell_edit
      r$datamart_thesaurus_items_temp <- DT::editData(r$datamart_thesaurus_items_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$datamart_thesaurus_items_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_thesaurus_items, {
      
      req(input$thesaurus)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
                                      table = "thesaurus_items", r_table = "datamart_thesaurus_items", duplicates_allowed = TRUE, language = "EN")
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_items_rows_selected, {
      
      style <- "display:inline-block; width:200px; font-weight:bold;"
      
      thesaurus_item <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
      
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
      
      all_values <- r$labs_vitals %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
        dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% dplyr::select(value, value_num)
      values_num <- numeric(0)
      if (nrow(all_values %>% dplyr::filter(!is.na(value_num))) > 0) values_num <- suppressMessages(all_values %>% dplyr::filter(!is.na(value_num)) %>% 
        dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value_num))
      values <- character(0)
      if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0) values <- suppressMessages(all_values %>% dplyr::filter(!is.na(value)) %>% 
        dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(value))
      values_text <- tagList(
        span(i18n$t("Values"), style = style), paste(values, collapse = " || "), br(),
        span(i18n$t("Numeric values"), style = style), paste(values_num, collapse = " || "), br()
      )
      
      if (nrow(all_values) == 0){
        
        all_values <- r$orders %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
          dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% 
          dplyr::mutate(amount_text = paste0(amount, " ", amount_unit), rate_text = paste0(rate, " ", rate_unit)) %>%
          dplyr::select(amount, amount_text, rate, rate_text)
        amount <- numeric(0)
        if (nrow(all_values %>% dplyr::filter(!is.na(amount))) > 0) amount <- suppressMessages(all_values %>% dplyr::filter(!is.na(amount)) %>% 
          dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(amount_text))
        rate <- numeric(0)
        if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0) rate <- suppressMessages(all_values %>% dplyr::filter(!is.na(rate)) %>% 
          dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text))
        
        values_text <- tagList(
          span(i18n$t("Rate"), style = style), paste(rate, collapse = " || "), br(),
          span(i18n$t("Amount"), style = style), paste(amount, collapse = " || "), br()
        )
        
        if (nrow(all_values) == 0) values_text <- ""
      }
      
      output$thesaurus_selected_item <- renderUI(tagList(br(), div(
        span(i18n$t("Display name"), style = style), thesaurus_item$display_name, br(),
        span(i18n$t("Thesaurus ID"), style = style), thesaurus_item$thesaurus_id, br(),
        span(i18n$t("Item ID"), style = style), thesaurus_item$item_id, br(),
        values_text,
        style = "border:dashed 1px; padding:10px;"
      )))
    })
    
  })
}