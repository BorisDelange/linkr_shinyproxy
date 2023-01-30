#' my_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_studies_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  # *** To be removed *** ----
  language <- "EN"
  
  cards <- c(#"datamarts_options_card", "datamarts_edit_code_card", 
    "study_messages_card", "studies_creation_card", "studies_datatable_card", "study_options_card",
    "import_study_card", "export_study_card"#, 
    #"modules_families_card", "thesaurus_datamart_card"
    )
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card_new(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "datamart_main", text = i18n$t("my_studies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("studies_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "study_messages_card", itemKey = "study_messages_card", headerText = i18n$t("messages")),
          shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = i18n$t("studies_management")),
          shiny.fluent::PivotItem(id = "study_options_card", itemKey = "study_options_card", headerText = i18n$t("study_options")),
          shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study_card", headerText = i18n$t("import_study")),
          shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study_card", headerText = i18n$t("export_study"))
        )
      )
    ),
    div(
      id = ns("choose_a_datamart_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- -- -- --
    # Study messages card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("study_messages_card"),
        div(id = ns("study_messages_content"),
          make_card(i18n$t("messages"),
            div(
              shiny.fluent::Pivot(
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-messages_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "all_messages", itemKey = "all_messages", headerText = i18n$t("all_messages")),
                shiny.fluent::PivotItem(id = "new_conversation", itemKey = "new_conversation", headerText = i18n$t("new_conversation"))
              ),
              conditionalPanel(condition = "input.messages_current_tab == null || input.messages_current_tab == 'all_messages'", ns = ns,
                DT::DTOutput(ns("study_conversations")),
                uiOutput(ns("selected_conversation"))
              ),
              conditionalPanel(condition = "input.messages_current_tab == 'new_conversation'", ns = ns,
                make_textfield_new(i18n = i18n, ns = ns, label = "object", id = "new_conversation_name"),
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
          id = ns("choose_a_study_card"),
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
              make_textfield_new(i18n = i18n, ns = ns, label = "name", id = "study_name", width = "300px"),
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
        id = ns("study_options_card"),
        make_card(i18n$t("study_options"),
          div(
            make_combobox_new(i18n = i18n, ns = ns, label = "study", id = "options_chosen", width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            div(
              div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                list(key = "everybody", text = i18n$t("everybody")),
                list(key = "people_picker", text = i18n$t("choose_users"))
              ), className = "inline_choicegroup"),
              conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                uiOutput(ns("users_allowed_read_div"))
              )
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save"))
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
                    tags$li("Importer les données relatives à l'étude (modules, données modifiées sur patients et sur modules)"),
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
    # div(shinyAce::aceEditor(
    #   ns("ace_edit_code"), "", mode = "r",
    #   code_hotkeys = list(
    #     "r", list(
    #       run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
    #       run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
    #       save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
    #     )
    #   ),
    #   autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
    # ), style = "width: 100%;"),
    # shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(),
    # div(verbatimTextOutput(ns("code_result")),
    #   style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
    br()
  )
}
    
#' my_studies Server Functions
#'
#' @noRd 
mod_my_studies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    language <- "EN"
    
    sapply(1:6, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    messages_timer <- reactiveTimer(10000, session)
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c(#"datamarts_options_card", "datamarts_edit_code_card", 
      "study_messages_card", "studies_datatable_card", "study_options_card",
      "import_study_card", "export_study_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)

    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar_new(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n, ns = ns))
    observeEvent(r$show_message_bar2, show_message_bar_new(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- --- --- --
    # When a datamart is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$chosen_datamart, {
      
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_datamart_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("study_messages_card" %in% r$user_accesses) shinyjs::show("study_messages_card")
        else shinyjs::show("study_messages_card_forbidden")
      }
      
      # Hide messages card & reset fields
      shinyjs::show("choose_a_study_card")
      shinyjs::hide("study_messages_content")
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      
      # The datamart is loaded here, and not in sidenav
      # Placed in sidenav, the datamart is loaded multiple times (each time a page loads its own sidenav)
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      # Reset d variables
      d$patients <- tibble::tibble()
      d$stays <- tibble::tibble()
      d$labs_vitals <- tibble::tibble()
      d$text <- tibble::tibble()
      d$orders <- tibble::tibble()
      d$diagnoses <- tibble::tibble()
      
      # Try to load datamart
      tryCatch({

        capture.output(run_datamart_code_new(output, r = r, d = d, datamart_id = r$chosen_datamart, i18n = i18n, quiet = TRUE))
  
        # A r variable to update study dropdown, when the load of datamart is finished
        r$loaded_datamart <- r$chosen_datamart
  
        r$show_message_bar1 <- tibble::tibble(message = "import_datamart_success", type = "success", trigger = Sys.time())
        
        # Try to run the scripts associated with this datamart
        
        tryCatch({
          
          scripts_code <- r$code %>% dplyr::filter(category == "script") %>% dplyr::select(id = link_id, code) %>%
            dplyr::inner_join(
              r$options %>% dplyr::filter(category == "datamart_scripts", link_id == r$chosen_datamart) %>% dplyr::select(id = value_num),
              by = "id"
            )
          
          if (nrow(scripts_code > 0)){
            for (i in 1:nrow(scripts_code)){
              eval(parse(text = scripts_code[i, ]$code %>% stringr::str_replace_all("\r", "\n")))
            }
            
            r$show_message_bar2 <- tibble::tibble(message = "run_scripts_success", type = "success", trigger = Sys.time())
          }
        },
          error = function(e){
            r$show_message_bar2 <<- tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
            report_bug_new(r = r, output = output, error_message = "fail_load_scripts",
              error_name = paste0(id, " - run server code"), category = "Error", error_report = e, i18n = i18n)
          })
      },
        error = function(e){
          r$show_message_bar1 <<- tibble::tibble(message = "fail_load_datamart", type = "severeWarning", trigger = Sys.time())
          report_bug_new(r = r, output = output, error_message = "fail_load_datamart",
            error_name = paste0(id, " - run server code"), category = "Error", error_report = e, i18n = i18n)
        })
      
    })
    
    # Once the datamart is loaded, load studies & scripts
    observeEvent(r$loaded_datamart, {
      
      update_r_new(r = r, m = m, table = "studies")
      update_r_new(r = r, m = m, table = "scripts")
      
      # Update dropdown for study options
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options)
    })
    
    # --- --- --- --- --- --- --- --
    # When a study is chosen ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$chosen_study, {
      
      req(!is.na(m$chosen_study))
      # Show first card & hide "choose a datamart" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::show("study_messages_content")
      
      # Reset new conversation fields
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      
      # Update study options combobox
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = m$chosen_study, text = r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)
    })
    
    # --- --- --- -
    # Messages ----
    # --- --- --- -
      
      # --- --- --- --- --
      ## All messages ----
      # --- --- --- --- --
      
      observeEvent(m$chosen_study, {
        
        req(!is.na(m$chosen_study))
        
        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id AND m.deleted IS FALSE ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$chosen_study} AND m.deleted IS FALSE"), .con = r$db)
        
        r$study_messages <- DBI::dbGetQuery(r$db, sql) %>% 
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime)) 
        
        if (nrow(r$study_messages) > 0) r$study_conversations <- r$study_messages %>%
          dplyr::group_by(conversation_id) %>%
          dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(read, dplyr::desc(datetime)) %>%
          dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ i18n$t("no"), TRUE ~ i18n$t("yes"))) %>%
          dplyr::select(-read) %>%
          dplyr::relocate(unread_messages, .after = "datetime") %>%
          dplyr::mutate_at("datetime", as.character) %>%
          dplyr::mutate(datetime = stringr::str_replace_all(datetime, "T|Z", ""))
        
        else r$study_conversations <- r$study_messages %>% dplyr::select(conversation_id, conversation_name, datetime, read)
        
        r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)

        searchable_cols <- c("conversation_name", "unread_messages")
        factorize_cols <- c("unread_messages")
        column_widths <- c("datetime" = "150px", "unread_messages" = "150px")
        sortable_cols <- c("id", "conversation_name", "datetime", "unread_messages")
        centered_cols <- c("id", "datetime", "unread_messages")
        col_names <- get_col_names_new(table_name = "study_conversations", i18n = i18n)
        hidden_cols <- c("conversation_id", "modified")

        # Render datatable
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$study_conversations_temp,
          output_name = "study_conversations", col_names = col_names,
         sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)

        # Create a proxy for datatatable
        r$study_conversations_datatable_proxy <- DT::dataTableProxy("study_conversations", deferUntilFlush = FALSE)
      })
    
      # When a conversation is selected
      observeEvent(input$study_conversations_rows_selected, {
        
        r$study_reload_conversation <- Sys.time()
        r$study_selected_conversation <- r$study_conversations_temp[input$study_conversations_rows_selected, ]
      })
      
      # Timer to update messages
      
      observe({
        messages_timer()
        req(!is.na(m$chosen_study))
        
        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id AND m.deleted IS FALSE ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$chosen_study} AND m.deleted IS FALSE"), .con = r$db)
        
        study_messages <- DBI::dbGetQuery(r$db, sql) %>% 
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime))
        
        if (study_messages %>% dplyr::select(id) %>% dplyr::anti_join(r$study_messages %>% dplyr::select(id), by = "id") %>% nrow() > 0 & nrow(study_messages) > 0){
          
          r$study_messages <- study_messages
          
          r$study_conversations <- r$study_messages %>%
            dplyr::group_by(conversation_id) %>%
            dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(read, dplyr::desc(datetime)) %>%
            dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ i18n$t("no"), TRUE ~ i18n$t("yes"))) %>%
            dplyr::select(-read) %>%
            dplyr::relocate(unread_messages, .after = "datetime") %>%
            dplyr::mutate_at("datetime", as.character) %>%
            dplyr::mutate(datetime = stringr::str_replace_all(datetime, "T|Z", ""))
          
          r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
          
          DT::replaceData(r$study_conversations_datatable_proxy, r$study_conversations_temp, resetPaging = FALSE, rownames = FALSE)
          
          r$study_reload_conversation <- Sys.time()
        }
      })
        
      observeEvent(r$study_reload_conversation, {
        
        # selected_conversation <- r$study_conversations_temp[input$study_conversations_rows_selected, ]
        # r$study_selected_conversation <- selected_conversation %>% dplyr::select(conversation_id, conversation_name)
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
          
          message_div <- div(HTML(study_message$message))
          
          if (study_message$filepath != ""){
            tryCatch({ 
              message_div <- div(class = "markdown_messages", withMathJax(includeMarkdown(study_message$filepath)))
            }, error = function(e) "")
          }
          
          date <- study_message$datetime %>% as.Date()
          
          if (study_message$creator_id == r$user_id){
            study_message_ui <- div(
              div(
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
            if (study_message$read) study_message_style <- "background-color:#F5F5F5; margin-top:10px; padding:15px; border-radius:10px;"
            else study_message_style <- "background-color:#ECF8E7; margin-top:10px; padding:15px; border-radius:10px;"
            
            creator_name <- r$users %>% dplyr::filter(id == study_message$creator_id) %>%
              dplyr::mutate(creator_name = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator_name)
            
            author_span <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
              div(creator_name), div(paste0(date, ", ", format(study_message$datetime, "%H:%M")))
            )
            
            study_message_ui <- 
            div(
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
        
        output$selected_conversation <- renderUI(tagList(
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
          ), br(),
          conversation_messages_ui
        ))
        
        # Clear message preview and aceEditor
        output$new_message_preview <- renderUI("")
        shinyAce::updateAceEditor(session, "new_message_text", value = "")
        sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
        shinyjs::show("conversation_new_message")
        
        # Update inbox_messages
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
      })
      
      # --- --- --- --- -
      ## New message ----
      # --- --- --- --- -
      
      observeEvent(input$conversation_new_message, {
        sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::show(name))
        shinyjs::hide("conversation_new_message")
      })
      
      observeEvent(input$conversation_hide_new_message, {
        sapply(c("new_message_text_div", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
        shinyjs::show("conversation_new_message")
      })
      
      # --- --- --- --- --- --- --- --- --- --- -- -
      ## Preview for new message & conversation ----
      # --- --- --- --- --- --- --- --- --- --- -- -
      
      observeEvent(input$preview_new_conversation, {
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "conversation"
      })
      
      observeEvent(input$preview_new_message, {
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "message"
      })
      
      observeEvent(r$study_preview_trigger, {
        
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
            
            # Create the markdown file
            knitr::knit(text = markdown_file, output = new_file, quiet = TRUE)
            
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
      })
      
      # --- --- --- --- --- --- --- --- --- --
      ## Save new message or conversation ----
      # --- --- --- --- --- --- --- --- --- --
      
      observeEvent(input$send_new_conversation, {
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "conversation"
      })
      
      observeEvent(input$send_new_message, {
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "message"
      })
      
      observeEvent(r$study_save_message_conversation_trigger, {
        
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
            
            # Create the markdown file
            knitr::knit(text = markdown_file, output = new_file, quiet = TRUE)
            
          }, error = function(e) "")
        }
        else {
          new_text <- input[[paste0("new_", type, "_text")]]
          new_file <- ""
        }
        
        # Get list of users authorized to see this study
        
        users_allowed_read_group <- r$options %>% dplyr::filter(category == "study", link_id == m$chosen_study, name == "users_allowed_read_group") %>% dplyr::pull(value)
        if (users_allowed_read_group == "everybody") receivers_ids <- r$users %>% dplyr::pull(id)
        if (users_allowed_read_group == "people_picker") receivers_ids <- r$options %>% dplyr::filter(category == "study", link_id == m$chosen_study, name == "user_allowed_read") %>% dplyr::pull(value_num)
        
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
          new_message_id, conversation_id, m$chosen_study, "study_message",
          new_text, new_file, r$user_id, as.character(Sys.time()), FALSE)
        DBI::dbAppendTable(r$db, "messages", new_data)

        # Inbox_messages table
        new_data <- tibble::tibble(
          id = seq(get_last_row(r$db, "inbox_messages") + 1, get_last_row(r$db, "inbox_messages") + length(receivers_ids), 1),
          message_id = new_message_id, receiver_id = receivers_ids, read = FALSE, datetime = as.character(Sys.time()), deleted = FALSE)
        DBI::dbAppendTable(r$db, "inbox_messages", new_data)

        # Reset fields
        if (type == "conversation") shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
        shinyAce::updateAceEditor(session, paste0("new_", type, "_text"), value = "")
        output[[paste0("new_", type, "_preview")]] <- renderUI("")
        
        if (type == "conversation"){
          
          # Reload conversations datatable
          r$study_conversations_temp <- r$study_conversations_temp %>%
            dplyr::bind_rows(
              tibble::tribble(
                ~conversation_id, ~conversation_name, ~datetime, ~unread_messages, ~modified,
                conversation_id, conversation_name, as.character(Sys.time()), i18n$t("yes"), FALSE
              )
            )
          DT::replaceData(r$study_conversations_datatable_proxy, r$study_conversations_temp, resetPaging = FALSE, rownames = FALSE)
        }
        
        # Reload conversation
        
        r$study_messages <- r$study_messages %>%
          dplyr::bind_rows(
            tibble::tribble(
              ~id, ~conversation_id, ~conversation_name, ~message, ~filepath, ~creator_id, ~datetime, ~read,
              new_message_id, conversation_id, conversation_name, new_text, new_file, r$user_id, Sys.time(), 0
            )
          ) %>%
          dplyr::arrange(dplyr::desc(datetime))
        
        if (type == "message") r$study_reload_conversation <- Sys.time()

        # Notify user
        show_message_bar_new(output = output, id = 4, message = paste0("new_", type, "_added"), type = "success", i18n = i18n, ns = ns)
      })
    
    # --- --- --- --- ---
    # Create a study ----
    # --- --- --- --- ---
    
    observeEvent(input$add_study, {
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$study_name)
      new_data$study_name <- new_data$name
      new_data$description <- ""
      new_data$patient_lvl_module_family <- get_last_row(r$db, "patient_lvl_modules_families") + 1
      new_data$aggregated_module_family <- get_last_row(r$db, "aggregated_modules_families") + 1
      new_data$datamart <- r$chosen_datamart
      
      add_settings_new_data_new(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "settings_studies", 
        data = new_data, table = "studies", required_textfields = "study_name", req_unique_values = "name")
      
      # Reload datatable
      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
    })
    
    # --- --- --- --- --- ---
    # Studies management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each module / page
    action_buttons <- c("options", "delete")
    
    editable_cols <- c("name")
    sortable_cols <- c("id", "name", "description", "datamart_id", "data_source_id", "study_id", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    centered_cols <- c("id", "creator", "datetime", "action")
    searchable_cols <- c("name", "description", "data_source_id", "datamart_id", "study_id", "creator_id")
    factorize_cols <- c("datamart_id", "creator_id")
    hidden_cols <- c("id", "description", "datamart_id", "patient_lvl_module_family_id", "aggregated_module_family_id", "deleted", "modified")
    col_names <- get_col_names_new("studies", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$studies, {
      
      if(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) == 0){
        render_datatable_new(output = output, r = r, ns = ns, i18n = i18n,
          data = tibble::tribble(~name, ~creator_id, ~datetime, ~action), output_name = "studies_datatable")
      }
      
      req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)

      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)

      # Prepare data for datatable

      r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons,
        data_input = r$studies_temp, words = r$words)

      # Render datatable

      render_datatable_new(output = output, r = r, ns = ns, i18n = i18n, data = r$studies_datatable_temp,
        output_name = "studies_datatable", col_names =  get_col_names_new("studies", i18n),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")

      # Create a proxy for datatable

      r$studies_datatable_proxy <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
    }, once = TRUE)
    
    # Reload datatable
    observeEvent(r$studies_temp, {

      # Reload datatable_temp variable
      r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, language = language, id = id,
        table = "studies", factorize_cols = factorize_cols, action_buttons = action_buttons, 
        data_input = r$studies_temp, words = r$words)

      # Reload data of datatable
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
        r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$studies_datatable_cell_edit, {
      
      edit_info <- input$studies_datatable_cell_edit
      r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_studies_management, {
      
      req(nrow(r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)) > 0)
      
      save_settings_datatable_updates_new(output = output, r = r, ns = ns, 
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
    
    delete_element_new(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
      react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
      delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
      information_variable = study_information_variable)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[study_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      req(length(input$studies_datatable_rows_selected) > 0)
      
      r$delete_study <- r$studies_temp[input$studies_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[study_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_studies, {
      
      # Reload sidenav dropdown with reloading studies
      update_r(r = r, table = "studies")
      r$studies_temp <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reset chosen study
      m$chosen_study <- NA_integer_
    })
    
    # --- --- --- --- --
    # Study options ----
    # --- --- --- --- --
    
    observeEvent(input$options, {
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$studies %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))

      shiny.fluent::updateComboBox.shinyInput(session, "options_chosen", options = options, value = value)

      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)

      # Set current pivot to options_card
      button_name <- gsub("'", "\\\\'", i18n$t('study_options'))
      shinyjs::runjs(glue::glue("$('#{id}-studies_pivot button[name=\"{button_name}\"]').click();"))
    })
    
    observeEvent(input$options_chosen, {
      
      if (length(input$options_chosen) > 1) link_id <- input$options_chosen$key
      else link_id <- input$options_chosen
      
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
        make_people_picker_new(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
    })
    
    observeEvent(input$options_save, {

      req(input$options_chosen)

      if (length(input$options_chosen) > 1) link_id <- input$options_chosen$key
      else link_id <- input$options_chosen

      data <- list()
      data$users_allowed_read <- input$users_allowed_read
      data$users_allowed_read_group <- input$users_allowed_read_group

      save_settings_options_new(output = output, r = r, id = id, category = "study", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = "users_allowed_read")
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
    
    observeEvent(input$execute_code, {
      
      code <- input$ace_edit_code %>% stringr::str_replace_all("\r", "\n")
      
      output$code_result <- renderText({
        
        options('cli.num_colors' = 1)
        
        # Capture console output of our code
        captured_output <- capture.output(
          tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
        
        # Restore normal value
        options('cli.num_colors' = NULL)
        
        # Display result
        paste(strwrap(captured_output), collapse = "\n")
      })
    })
    
  })
}
