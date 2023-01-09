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
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card_new(ns = ns, name = card, i18n = i18n))
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
          shiny.fluent::PivotItem(id = "thesaurus_categories_card", itemKey = "thesaurus_categories_card", headerText = i18n$t("categories")),
          shiny.fluent::PivotItem(id = "thesaurus_conversions_card", itemKey = "thesaurus_conversions_card", headerText = i18n$t("conversions")),
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
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
                make_combobox_new(i18n = i18n, ns = ns, label = "thesaurus", id = "thesaurus", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                conditionalPanel(
                  condition = "input.thesaurus != null", ns = ns,
                  div(strong(i18n$t("show_only_used_items"), style = "display:block; padding-bottom:12px;"),
                    shiny.fluent::Toggle.shinyInput(ns("show_only_used_items"), value = TRUE), style = "margin-top:15px;")
                ),
                style = "position:relative; z-index:1; width:800px;"
              ),
              div(DT::DTOutput(ns("thesaurus_items")), style = "margin-top:-30px; z-index:2"),
              conditionalPanel(
                condition = "input.thesaurus != null", ns = ns,
                shiny.fluent::PrimaryButton.shinyInput(ns("save_thesaurus_items"), i18n$t("save"))
              ),
              br(),
              uiOutput(ns("thesaurus_selected_item")), br()
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- -- -
    # Categories card ----
    # --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("thesaurus_categories_card"),
        make_card(i18n$t("categories"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
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
    
    # --- --- --- --- --- -
    # Conversions card ----
    # --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("thesaurus_conversions_card"),
        make_card(i18n$t("conversions"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
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
    
    # --- --- --- --- --- -- -
    # Items creation card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("thesaurus_mapping_card"),
        make_card(i18n$t("items_mapping"),
          div(
            
          )
        ), br()
      )
    )
  )
}
    
#' thesaurus Server Functions
#'
#' @noRd 
mod_thesaurus_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("thesaurus_items_card", "thesaurus_categories_card", "thesaurus_conversions_card", "thesaurus_mapping_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar_new(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n))
    observeEvent(r$show_message_bar2, show_message_bar_new(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n))
    
    # --- --- --- --- --- -- -
    # Thesaurus items ----
    # --- --- --- --- --- -- -
    
    # Delete when "thesaurus_items_card" will be added in r$user_accesses
    
    # observeEvent(input$current_tab, {
    #   sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
    #   shinyjs::show(input$current_tab)
    # })
    
    observeEvent(r$chosen_datamart, {
      
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
      shiny.fluent::updateComboBox.shinyInput(session, "thesaurus", options = convert_tibble_to_list_new(data = thesaurus, key_col = "id", text_col = "name", i18n = i18n))
      
      if (length(r$datamart_thesaurus_items_temp) > 0) r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items_temp %>% dplyr::slice(0)
      
      # Reset UI of selected item
      output$thesaurus_selected_item <- renderUI("")
    })
    
    observeEvent(input$show_only_used_items, r$reload_thesaurus_datatable <- Sys.time())
    observeEvent(input$thesaurus, r$reload_thesaurus_datatable <- Sys.time())
    
    observeEvent(r$reload_thesaurus_datatable, {
      
      req(length(input$thesaurus$key) > 0)
      
      r$datamart_thesaurus_items <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted
          FROM thesaurus_items t
          WHERE t.thesaurus_id = ", input$thesaurus$key, " AND t.deleted IS FALSE
          ORDER BY t.item_id")) %>% tibble::as_tibble() %>% dplyr::mutate(action = "")
      
      # Get user's modifications on items names & abbreviations
      
      r$datamart_thesaurus_user_items <- DBI::dbGetQuery(r$db, paste0(
        "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.deleted
          FROM thesaurus_items_users t
          WHERE t.thesaurus_id = ", input$thesaurus$key, " AND t.deleted IS FALSE
          ORDER BY t.item_id")) %>% tibble::as_tibble()
      
      # Merge tibbles
      r$datamart_thesaurus_items <-
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
        # dplyr::mutate(display_name = ifelse((display_name != "" & !is.na(display_name)), display_name, name)) %>%
        dplyr::left_join(count_items_rows, by = "item_id") %>%
        dplyr::left_join(count_patients_rows, by = "item_id") %>%
        dplyr::mutate_at(c("count_items_rows", "count_patients_rows"), as.integer) %>%
        dplyr::relocate(count_patients_rows, .before = "action") %>% dplyr::relocate(count_items_rows, .before = "action")
      
      # Order by name
      r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::arrange(name)
      
      # Filter on count_items_rows > 0
      if (input$show_only_used_items) r$datamart_thesaurus_items <- r$datamart_thesaurus_items %>% dplyr::filter(count_items_rows > 0)
      
      r$datamart_thesaurus_items_temp <- r$datamart_thesaurus_items %>%
        dplyr::mutate(modified = FALSE) %>%
        dplyr::mutate_at("item_id", as.character)
      
      editable_cols <- c("name", "display_name")
      searchable_cols <- c("item_id", "name", "display_name", "category", "unit")
      factorize_cols <- c("category", "unit")
      column_widths <- c("id" = "80px", "action" = "80px", "unit" = "100px", "count_patients_rows" = "80px", "count_items_rows" = "80px")
      sortable_cols <- c("id", "name", "display_name", "category", "count_patients_rows", "count_items_rows")
      centered_cols <- c("id", "item_id", "unit", "datetime", "count_patients_rows", "count_items_rows", "action")
      col_names <- get_col_names_new(table_name = "datamart_thesaurus_items_with_counts", i18n = i18n)
      hidden_cols <- c("id", "thesaurus_id", "item_id", "datetime", "deleted", "modified", "action")
      
      # Render datatable
      render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$datamart_thesaurus_items_temp,
        output_name = "thesaurus_items", col_names =  col_names,
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      
      # Create a proxy for datatatable
      r$datamart_thesaurus_items_datatable_proxy <- DT::dataTableProxy("thesaurus_items", deferUntilFlush = FALSE)
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
      save_settings_datatable_updates_new(output = output, r = r, ns = ns, table = "thesaurus_items_users", r_table = "datamart_thesaurus_items", duplicates_allowed = TRUE, i18n = i18n)
    })
    
    # When a row is selected
    observeEvent(input$thesaurus_items_rows_selected, {

      style <- "display:inline-block; width:200px; font-weight:bold;"

      thesaurus_item <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)

      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)

      # Which columns contain data
      r$datamart_thesaurus_items_d_var <- character(0)
      r$datamart_thesaurus_items_cols_not_empty <- character(0)
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
          plots <- tagList(plots, plotOutput(ns("value_num_plot")))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "value_num")
        }

        if (nrow(all_values %>% dplyr::filter(!is.na(value))) > 0){
          
          values <- suppressMessages(
            all_values %>% 
              dplyr::filter(!is.na(value)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% 
              dplyr::pull(value)
            )
          plots <- tagList(plots, plotOutput(ns("value_plot")))
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
          plots <- tagList(plots, plotOutput(ns("amount_plot")))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "amount")
        }
        
        if (nrow(all_values %>% dplyr::filter(!is.na(rate))) > 0){
          
          rates <- suppressMessages(
            all_values %>% 
              dplyr::mutate(rate_text = dplyr::case_when(!is.na(rate_unit) ~ paste0(rate, " ", rate_unit), TRUE ~ as.character(rate))) %>%
              dplyr::filter(!is.na(rate)) %>%
              dplyr::slice_sample(n = 5, replace = TRUE) %>% dplyr::pull(rate_text)
          )
          plots <- tagList(plots, plotOutput(ns("rate_plot")))
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
          plots <- tagList(plots, plotOutput(ns("value_plot")))
          r$datamart_thesaurus_items_cols_not_empty <- c(r$datamart_thesaurus_items_cols_not_empty, "value")
        }
        
        values_text <- tagList(
          span(i18n$t("rate"), style = style), paste(rate, collapse = " || "), br(),
          span(i18n$t("amount"), style = style), paste(amount, collapse = " || "), br())
      }
        
      output$thesaurus_selected_item <- renderUI(tagList(br(), div(
        span(i18n$t("var_containing_item"), style = style), paste0("d$", r$datamart_thesaurus_items_d_var), br(),
        span(i18n$t("thesaurus_id"), style = style), thesaurus_item$thesaurus_id, br(),
        span(i18n$t("thesaurus_name"), style = style), thesaurus_name, br(),
        span(i18n$t("item_id"), style = style), thesaurus_item$item_id, br(),
        span(i18n$t("name"), style = style), thesaurus_item$name, br(),
        span(i18n$t("abbreviation"), style = style), thesaurus_item$display_name, br(),
        span(i18n$t("category"), style = style), thesaurus_item$category, br(),
        span(i18n$t("unit"), style = style), ifelse(is.na(thesaurus_item$unit), "", thesaurus_item$unit), br(),
        values_text, br(),
        shiny.fluent::DefaultButton.shinyInput(ns("show_plots"), i18n$t("show_plots")),
        conditionalPanel(condition = "input.show_plots == true", ns = ns, br(), plots),
        style = "border:dashed 1px; padding:10px;"
      )))
    })
    
    observeEvent(input$show_plots, {
      
      thesaurus_item <- r$datamart_thesaurus_items_temp[input$thesaurus_items_rows_selected, ] %>% dplyr::mutate_at("item_id", as.integer)
      thesaurus_name <- r$thesaurus %>% dplyr::filter(id == thesaurus_item$thesaurus_id) %>% dplyr::pull(name)
      
      all_values <- d$labs_vitals %>% dplyr::filter(thesaurus_name == !!thesaurus_name) %>%
        dplyr::inner_join(thesaurus_item %>% dplyr::select(item_id), by = "item_id") %>% dplyr::select(value, value_num)
      # values_num <- all_values %>% dplyr::filter(!is.na(value_num))
      # values <- all_values %>% dplyr::filter(!is.na(value))
      
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
    })
    
    # --- --- --- --- --
    # Items mapping ----
    # --- --- --- --- --
    
  })
}
