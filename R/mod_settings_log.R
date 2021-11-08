#' settings_log UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_log_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  
  div(class = "main",
      
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "log_card", label = "log"))),
    
    div(id = ns("log_card"),
      make_card(translate(language, "log"),
        uiOutput(ns("main"))
      )
    )
  )
}
    
#' settings_log Server Functions
#'
#' @noRd 

mod_settings_log_server <- function(id = character(), r = shiny::reactiveValuess(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Log / Show or hide cards               #
    ##########################################
    
    # Depending on toggles activated
    
    # Reset toggles when we load the page (restart reactivity, sometimes frozen)
    observeEvent(shiny.router::get_query_param(), {
      shiny.fluent::updateToggle.shinyInput(session, "log_card_toggle", value = FALSE)
      # If this toggles was activated, reactivate it
      if (paste0(id, toggle) %in% r$activated_toggles) shiny.fluent::updateToggle.shinyInput(session, "log_card_toggle", value = TRUE)
    })
      
    # If user has no access, hide card
    observeEvent(r$user_accesses, if ("log" %not_in% r$user_accesses) shinyjs::hide("log_card")) 
    
    # If user has access, show or hide card when toggle is clicked
    observeEvent(input$log_card_toggle, {
      if ("log" %in% r$user_accesses){
        if(input$log_card_toggle){
          shinyjs::show("log_card") 
          r$activated_toggles <- c(r$activated_toggles, paste0(id, "log_card"))
        }
        else{
          shinyjs::hide("log_card")
          r$activated_toggles <- r$activated_toggles[r$activated_toggles != paste0(id, "log_card")]
        }
      }
    })
  
    ##########################################
    # Log / Render datatable                 #
    ##########################################
    
    observeEvent(r$user_accesses, {
      
      output$main <- renderUI({
        
        result <- ""
        
        if ("all_users" %in% r$user_accesses){
    
          options <-
            r$users %>%
            dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
            dplyr::transmute(
              key = id, 
              imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
              text = paste0(firstname, " ", lastname), 
              secondaryText = user_status)
          
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("see_log_of"), value = "only_me", options = list(
              list(key = "only_me", text = translate(language, "only_me", words)),
              list(key = "people_picker", text = translate(language, "people_picker", words))
            ), className = "inline_choicegroup"),
            conditionalPanel(condition = "input.see_log_of == 'people_picker'", ns = ns,
              make_people_picker(language = language, ns = ns, label = "users", options = options, words = words)
            ), br(),
            DT::DTOutput(ns("datatable"))
          ) -> result
        }
        
        if ("all_users" %not_in% r$user_accesses & "only_me" %in% r$user_accesses){
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("see_log_of"), value = "only_me", options = list(
              list(key = "only_me", text = translate(language, "only_me", words))
            ), className = "inline_choicegroup"),
            DT::DTOutput(ns("datatable"))
          )
        }
        
        result
      })
    })
    
    # When a user is chosen
    
    observeEvent(input$see_log_of, {
      
      r$log <- tibble::tibble()
      
      if (input$see_log_of == "only_me"){
        sql <- glue::glue_sql("SELECT id, category, name, value, creator_id, datetime FROM log WHERE creator_id = {r$user_id}", .con = r$db)
        r$log <- DBI::dbGetQuery(r$db, sql)
      }
      
    })
    
    observeEvent(input$users, {
      
      sql <- glue::glue_sql("SELECT id, category, name, value, creator_id, datetime FROM log WHERE creator_id IN ({input$users*})", .con = r$db)
      r$log <- DBI::dbGetQuery(r$db, sql)
      
    })
    
    observeEvent(r$log, {
      
      log <- r$log
      
      if (nrow(log) > 0){
        
        log <- log %>% dplyr::left_join(
          r$users %>% dplyr::mutate(display_name = paste0(firstname, " ", lastname)) %>% dplyr::select(creator_id = id, display_name),
          by = "creator_id") %>% 
          dplyr::relocate(display_name, .before = "datetime") %>%
          dplyr::select(-creator_id, creator_id = display_name) %>%
          dplyr::mutate(value = substr(value, 1, 100)) %>%
          dplyr::arrange(desc(datetime))
      }
      
      dt_translation <- list(
        paginate = list(previous = translate(language, "DT_previous_page", words), `next` = translate(language, "DT_next_page", words)),
        search = translate(language, "DT_search", words),
        lengthMenu = translate(language, "DT_length", words),
        emptyTable = translate(language, "DT_empty", words))
      
      col_names <- get_col_names("log")
      page_length <- 100
      centered_cols <- c("id", "name", "creator_id", "datetime")
      sortable_cols <- c("id", "category", "name", "creator_id", "datetime")
      column_widths <- c("category" = "100px", "name" = "100px", "datetime" = "180px")
      searchable_cols <- c("category", "name", "creator_id", "datetime")
      factorize_cols <- c("name", "category")
      
      render_datatable(output = output, r = r, ns = ns, data = log, output_name = "datatable", col_names = col_names,
        page_length = page_length, centered_cols = centered_cols, sortable_cols = sortable_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols)
    })
    
  })
}
