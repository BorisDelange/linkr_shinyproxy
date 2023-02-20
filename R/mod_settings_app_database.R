#' settings_app_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_app_database_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card", "db_restore_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "app_db", text = i18n$t("app_db"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "db_connection_infos_card", itemKey = "db_connection_infos_card", headerText = i18n$t("db_connection_infos_card")),
      shiny.fluent::PivotItem(id = "db_datatable_card", itemKey = "db_datatable_card", headerText = i18n$t("db_datatable_card")),
      shiny.fluent::PivotItem(id = "db_request_card", itemKey = "db_request_card", headerText = i18n$t("db_request_card")),
      shiny.fluent::PivotItem(id = "db_save_card", itemKey = "db_save_card", headerText = i18n$t("db_save_card")),
      shiny.fluent::PivotItem(id = "db_restore_card", itemKey = "db_restore_card", headerText = i18n$t("db_restore_card"))
    ),
    
    forbidden_cards,
    
    # --- --- --- --- --- --- --- -
    # DB connection infos card ----
    # --- --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("db_connection_infos_card"),
        make_card(
          i18n$t("connection_infos"),
          div(
            div(
              div(class = "input_title", i18n$t("connection_type")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type"), options = list(
                  list(key = "local", text = i18n$t("local")),
                  list(key = "distant", text = i18n$t("distant"))
                ), className = "inline_choicegroup")
            ),
            shiny::conditionalPanel(
              condition = "input.connection_type == 'distant'", ns = ns,
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
                make_dropdown(i18n, ns, "sql_lib", options = list(
                  list(key = "postgres", text = "PostgreSQL"),
                  list(key = "sqlite", text = "SQLite")
                ), value = "postgres", width = "300px"),
                make_textfield(i18n, ns, "host", width = "300px")
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
                make_textfield(i18n, ns, "main_db_name", width = "300px"),
                shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, 
                  div(shiny::textOutput(ns("test_connection_main_db_success")), style = "margin-top:44px; font-weight:bold; color:#0078D4;")),
                shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, 
                  div(shiny::textOutput(ns("test_connection_main_db_failure")), style = "margin-top:44px; margin-left:-30px; color:red;"))
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
                make_textfield(i18n, ns, "public_db_name", width = "300px"),
                shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, 
                  div(shiny::textOutput(ns("test_connection_public_db_success")), style = "margin-top:44px; font-weight:bold; color:#0078D4;")),
                shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, 
                  div(shiny::textOutput(ns("test_connection_public_db_failure")), style = "margin-top:44px; margin-left:-30px; color:red;"))
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
                make_textfield(i18n, ns, "port", width = "300px"),
                make_textfield(i18n, ns, "user", width = "300px"),
                make_textfield(i18n, ns, "password", type = "password", canRevealPassword = TRUE, width = "300px")
              )), htmltools::br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              shiny.fluent::PrimaryButton.shinyInput(ns("db_connection_save"), i18n$t("save")), " ",
              shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, shiny.fluent::DefaultButton.shinyInput(ns("test_connection"), i18n$t("test_connection")))
            ),
          )
        )
      )
    ),
    
    # --- --- --- --- ---
    # DB tables card ----
    # --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("db_datatable_card"),
        make_card(
          i18n$t("app_db_tables"),
          div(
            br(), shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_tables"), options = list(
              list(key = "local", text = i18n$t("local")),
              list(key = "distant", text = i18n$t("distant"))
            ), className = "inline_choicegroup"),
            DT::DTOutput(ns("app_db_tables"))
          )
        )
      )
    ),
    
    # --- --- --- --- -- -
    # Request DB card ----
    # --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("db_request_card"),
        make_card(
          i18n$t("app_db_request"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_request"), options = list(
              list(key = "local", text = i18n$t("local")),
              list(key = "distant", text = i18n$t("distant"))
            ), className = "inline_choicegroup"),
            div(shinyAce::aceEditor(
              ns("app_db_request_code"), "", mode = "sql", 
              code_hotkeys = list(
                "r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                )
              ),
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
            ), style = "width: 100%;"),
            div(shiny::verbatimTextOutput(ns("request_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("request"), i18n$t("request"))
          )
        )
      )
    ),
    
    # --- --- --- --- -
    # Save DB card ----
    # --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("db_save_card"),
        make_card(
          i18n$t("db_save"),
          div(
            br(), uiOutput(ns("current_db_save")),
            br(), uiOutput(ns("last_db_save")), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, label = "db_export_log", value = FALSE, inline = TRUE)), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("db_save_button"), i18n$t("export_db"), iconProps = list(iconName = "Download")),
            div(style = "visibility:hidden;", downloadButton(ns("db_save"), label = ""))
          )
        )
      )
    ),
    
    # --- --- --- --- -- -
    # Restore DB card ----
    # --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("db_restore_card"),
        make_card(
          i18n$t("db_restore"),
          div(
            br(), uiOutput(ns("current_db_restore")),
            br(), uiOutput(ns("last_db_restore")), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, label = "db_import_log", value = FALSE, inline = TRUE)), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("db_restore_browse"), i18n$t("choose_zip_file")),
              uiOutput(ns("db_restore_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("db_restore_button"), i18n$t("restore_db"), iconProps = list(iconName = "Upload")),
            div(style = "display:none;", fileInput(ns("db_restore"), label = "", multiple = FALSE, accept = ".zip"))
          )
        )
      )
    )
  )
}
    
#' settings_app_database Server Functions
#'
#' @noRd 

mod_settings_app_database_server <- function(id = character(), r = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class(),
  app_folder = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    app_db_folder = paste0(app_folder, "/app_database")
    
    # Col types of database (to restore database)
    col_types <- tibble::tribble(
      ~table, ~col_types,
      "users", "icccciicl",
      "users_accesses", "icccl",
      "users_statuses", "icccl",
      "data_sources", "iccicl",
      "datamarts", "icciicl",
      "studies", "icciiiicl",
      "subsets", "icciicl",
      "subset_patients", "iiiicl",
      "thesaurus", "icccicl",
      "thesaurus_items", "iiicccccl",
      "plugins", "iccicl",
      "scripts", "iciicl",
      "patients_options", "iiiiiiciccnicl",
      "modules_elements_options", "iiiiicccnicl",
      "patient_lvl_modules_families", "iccicl",
      "aggregated_modules_families", "iccicl",
      "patient_lvl_modules", "icciiiicl",
      "aggregated_modules", "icciiiicl",
      "patient_lvl_modules_elements", "iciiiciccciicl",
      "aggregated_modules_elements", "iciiiiicl",
      "code", "icicicl",
      "options", "iciccnicl",
      "log", "icccic"
    )
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card", "db_restore_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("db_connection_infos_card" %in% r$user_accesses) shinyjs::show("db_connection_infos_card")
    else shinyjs::show("db_connection_infos_card_forbidden")
    
    # --- --- --- --- --- --- --- --- ---
    # Update connection type toggles ----
    # --- --- --- --- --- --- --- --- ---
    
    observeEvent(r$db_connection_type, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer r$db_connection_type"))
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = r$db_connection_type)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_request", value = r$db_connection_type)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_tables", value = r$db_connection_type)
    })
    
    observeEvent(input$connection_type_request, r$db_connection_type <- input$connection_type_request)
    observeEvent(input$connection_type_tables, r$db_connection_type <- input$connection_type_tables)
    
    # --- --- --- --- --- -- -
    # Database connection ----
    # --- --- --- --- --- -- -
    
    observeEvent(r$local_db, {
      
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer r$local_db"))

      # Get distant db informations
      db_info <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
      db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()

      # Fill textfields & choicegroup with recorded informations in local database
      sapply(names(db_info), function(name){
        if (name == "connection_type") r$db_connection_type <- db_info[[name]]
        if (name != "connection_type") shiny.fluent::updateTextField.shinyInput(session, name, value = db_info[[name]])
      })
    })
    
    # --- --- --- --- --- --- --- --- --- -
    # Save updates on connection infos ----
    # --- --- --- --- --- --- --- --- --- -
  
    # When save button is clicked
    
    observeEvent(input$db_connection_save, {
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$db_connection_save"))
      
      # If connection_type is local, save only connection_type but do not erase other informations (distant DB informations)
      if (input$connection_type == "local"){
        query <- "UPDATE options SET value = 'local' WHERE category = 'distant_db' AND name = 'connection_type'"
        DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
        add_log_entry(r = r, category = "SQL query", name = "Update SQL connection infos", value = query)
      }
      
      # If connection_type is distant, save connection_type and other distant DB informations
      if (input$connection_type == "distant"){
        
        # Checks inputs
        db_checks <- c("main_db_name" = FALSE, "public_db_name" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
        
        sapply(names(db_checks), function(name){
          shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = NULL)
          if (!is.null(input[[name]])){
            if (name != "port" & input[[name]] != "") db_checks[[name]] <<- TRUE
            if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <<- TRUE
          }
        })
        sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = i18n$t(paste0("provide_valid_", name))))
        
        req(db_checks[["main_db_name"]], db_checks[["public_db_name"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
        
        # If checks OK, insert data in database
        sapply(c("connection_type", "sql_lib", "main_db_name", "public_db_name", "host", "port", "user", "password"), function(name){
          sql <- glue::glue_sql(paste0("UPDATE options SET value = {as.character(input[[name]])}, creator_id = {r$user_id}, datetime = {as.character(Sys.time())} ",
            "WHERE category = 'distant_db' AND name = {name}"), .con = r$local_db)
          query <- DBI::dbSendStatement(r$local_db, sql)
          DBI::dbClearResult(query)
          add_log_entry(r = r, category = "SQL query", name = "Update SQL connection infos", value = toString(sql))
        })
      }
      
      # Reload r$db variable
      # get_db(r = r, m = m, app_db_folder = app_db_folder)
      
      show_message_bar(output, 1, "modif_saved", "success", i18n = i18n, ns = ns)
      show_message_bar(output, 2, "reload_app_to_take_into_account_changes", "warning", i18n = i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer input$db_connection_save"))
    })
    
    # --- --- --- --- -- -
    # Test connection ----
    # --- --- --- --- -- -
  
    # When test connection button is clicked
  
    observeEvent(input$test_connection, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$test_connection"))
      
      # Before testing connection, make sure fields are filled
      db_checks <- c("main_db_name" = FALSE, "public_db_name" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
      sapply(names(db_checks), function(name){
        shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = NULL)
        if (!is.null(input[[name]])){
          if (name != "port" & input[[name]] != "") db_checks[[name]] <<- TRUE
          if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <<- TRUE
        }
      })
      
      # Reset output textfields
      output$test_connection_main_db_success <- renderText("")
      output$test_connection_main_db_failure <- renderText("")
      output$test_connection_public_db_success <- renderText("")
      output$test_connection_public_db_failure <- renderText("")
      
      sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = i18n$t(paste0("provide_valid_", name))))
      
      req(db_checks[["main_db_name"]], db_checks[["public_db_name"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
      
      # If checks are OK, test connection
      sapply(c("main_db", "public_db"), function(db_type){
        
        code <- paste0("DBI::dbConnect(RPostgres::Postgres(),
          dbname = '", input[[paste0(db_type, "_name")]], "', host = '", input$host, "', port = ", input$port,
          ", user = '", input$user, "', password = '", input$password, "')")
        result_success <- ""
        result_failure <- ""
        result <- capture.output(
          tryCatch(eval(parse(text = isolate(code))), error = function(e) print(e), warning = function(w) print(w))
        )
        
        if (length(result) > 1){
          if (!grepl("exception|error|warning|fatal", tolower(result[1]))) result_success <- i18n$t("successfully_connected")
          if (grepl("exception|error|warning|fatal", tolower(result[1]))) result_failure <- result[1]
        }
        if (length(result) == 1){
          if (!grepl("exception|error|warning|fatal", tolower(result))) result_success <- i18n$t("successfully_connected")
          if (grepl("exception|error|warning|fatal", tolower(result))) result_failure <- result
        }
        
        output[[paste0("test_connection_", db_type, "_success")]] <- renderText(result_success)
        output[[paste0("test_connection_", db_type, "_failure")]] <- renderText(result_failure)
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer input$test_connection"))
    })
    
    # --- --- --- --
    # DB tables ----
    # --- --- --- --
    
    observeEvent(input$connection_type, {
      
      req(r$local_db)
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$connection_type"))
      
      # Update other ChoiceGroups
      
      r$db_connection_type <- input$connection_type
      # shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_tables", value = input$connection_type)
      # shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_request", value = input$connection_type)
      
      # Local database tables
      
      chosen_db <- ""
      data <- tibble::tibble(name = character(), row_number = integer())
      
      if (input$connection_type == "local"){
        chosen_db <- "local_db"
      }
      
      if (input$connection_type == "distant"){
        result <- get_distant_db(r = r, m = m, local_db = r$local_db)
        if (result == "success") chosen_db <- "remote_db"
      }
      
      if (chosen_db != ""){
        data <- tibble::tibble(
          name = DBI::dbListTables(r[[chosen_db]]),
          row_number = sapply(DBI::dbListTables(r[[chosen_db]]), function(table) DBI::dbGetQuery(r[[chosen_db]], paste0("SELECT COUNT(*) FROM ", table)) %>% dplyr::pull() %>% as.integer())
        ) %>%
          dplyr::bind_rows(
            tibble::tibble(
              name = DBI::dbListTables(m[[chosen_db]]),
              row_number = sapply(DBI::dbListTables(m[[chosen_db]]), function(table) DBI::dbGetQuery(m[[chosen_db]], paste0("SELECT COUNT(*) FROM ", table)) %>% dplyr::pull() %>% as.integer())
            )
          ) %>%
          dplyr::arrange(name)
      }
      
      colnames(data) <- c(i18n$t("table_name"), i18n$t("row_number"))
      
      dt_translation <- list(
        paginate = list(previous = i18n$t("dt_previous"), `next` = i18n$t("dt_next")),
        search = i18n$t("dt_search"),
        lengthMenu = i18n$t("dt_entries"),
        emptyTable = i18n$t("no_data_available"))
      
      output$app_db_tables <- DT::renderDT(
        data,
        options = list(
          dom = "t<'bottom'p>",
          pageLength = 40,
          language = dt_translation,
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          )),
        rownames = FALSE, selection = "none")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer input$connection_type"))
    })
    
    # --- --- --- ---
    # DB request ----
    # --- --- --- ---
    
    observeEvent(input$request, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$request"))
      
      r$app_db_request_code <- input$app_db_request_code
      r$app_db_request_trigger_code <- Sys.time()
    })
    
    observeEvent(input$app_db_request_code_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$app_db_request_code_run_selection"))
      
      if(!shinyAce::is.empty(input$app_db_request_code_run_selection$selection)) r$app_db_request_code <- input$app_db_request_code_run_selection$selection
      else r$app_db_request_code <- input$app_db_request_code_run_selection$line
      r$app_db_request_trigger_code <- Sys.time()
    })
    
    observeEvent(input$app_db_request_code_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$app_db_request_code_run_all"))
      
      r$app_db_request_code <- input$app_db_request_code
      r$app_db_request_trigger_code <- Sys.time()
    })
    
    observeEvent(r$app_db_request_trigger_code, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer r$app_db_request_trigger_code"))
        
      # Replace \r with \n to prevent bugs
      request <- isolate(r$app_db_request_code %>% stringr::str_replace_all("\r", "\n"))
      
      # Get local or distant db DBI object
      if (input$connection_type_request == "local") db <- r$local_db
      if (input$connection_type_request == "distant") db <- get_distant_db(r$local_db, i18n = i18n)
      
      # Change this option to display correctly tibble in textbox
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      
      captured_output <-
        capture.output(tryCatch({
          # dbSendStatement if it is not a select
          if (!grepl("^select", tolower(request))) capture.output({
            DBI::dbSendStatement(db, request) -> query
            print(query)
            DBI::dbClearResult(query)
          }) -> result
          
          # Else, a dbGetQuery
          else capture.output(DBI::dbGetQuery(db, request) %>% tibble::as_tibble() %>% print(n = 1000)) -> result
          
          # Render result
          result
          
        }, error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      # Display result
      output$request_result <- renderText(paste(captured_output, collapse = "\n"))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer r$app_db_request_trigger_code"))
    })
    
    # --- --- -- -
    # Save DB ----
    # --- --- -- -

    observeEvent(r$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer r$options"))
      
      # Last time the db was saved

      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")

      if (nrow(last_save) > 0) last_save <- last_save %>% dplyr::pull(value)
      else last_save <- i18n$t("never")

      output$last_db_save <- renderUI(tagList(strong(i18n$t("last_db_save")), " : ", last_save))

      current_db <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'distant_db'")
      connection_type <- current_db %>% dplyr::filter(name == "connection_type") %>% dplyr::pull(value)
      if (connection_type == "local") current_db_text <- paste0(i18n$t("local"), " (", app_db_folder, ")")
      else {
        sql_lib <- current_db %>% dplyr::filter(name == "sql_lib") %>% dplyr::pull(value)
        dbname <- current_db %>% dplyr::filter(name == "dbname") %>% dplyr::pull(value)
        current_db_text <- paste0(i18n$t("distant"), " (", sql_lib, " - ", dbname, ")")
      }

      output$current_db_save <- renderUI(tagList(strong(i18n$t("current_db")), " : ", current_db_text))
      output$current_db_restore <- renderUI(tagList(strong(i18n$t("current_db")), " : ", current_db_text))
      
      # Last time db was restored
      
      last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")
      
      if (nrow(last_restore) > 0) last_restore <- last_restore %>% dplyr::pull(value)
      else last_restore <- i18n$t("never")
      
      output$last_db_restore <- renderUI(tagList(strong(i18n$t("last_db_restore")), " : ", last_restore))
    })
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been saved
    
    observeEvent(input$db_save_button, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$db_save_button"))
      
      shinyjs::click("db_save")
      
      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")
      
      if (nrow(last_save) == 0) {
        
        # Insert last time row
        last_row <- get_last_row(r$db, "options")
        new_data <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
          as.integer(last_row + 1), "last_db_save", NA_integer_, "last_db_save", as.character(Sys.time()),
          NA_real_, as.integer(r$user_id), as.character(Sys.time()), FALSE)
        DBI::dbAppendTable(r$db, "options", new_data)
        r$options <- r$options %>% dplyr::bind_rows(new_data)
        
      }
      
      else {
        new_datetime <- as.character(Sys.time())
        sql <- glue::glue_sql("UPDATE options SET value = {new_datetime}, datetime = {new_datetime} WHERE category = 'last_db_save' AND name = 'last_db_save'", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        r$options <- r$options %>% dplyr::mutate(
          value = dplyr::case_when(category == "last_db_save" & name == "last_db_save" ~ new_datetime, TRUE ~ value),
          datetime = dplyr::case_when(category == "last_db_save" & name == "last_db_save" ~ new_datetime, TRUE ~ datetime)
        )
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer input$db_save_button"))
      
    })
    
    # Download all tables in one zip file
    
    output$db_save <- downloadHandler(
      
      filename = function() paste0("cdwtools_svg_", as.character(Sys.Date()), ".zip"),
      
      content = function(file){
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - output$db_save"))
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        files <- NULL
        
        tables <- DBI::dbListTables(r$db)

        for (table in tables){
          # Download all tables, except cache table
          if (table != "cache"){
            
            # Download log if user choice is TRUE
            if (table != "log" | (table == "log" & input$db_export_log)){
              file_name <- paste0(table, ".csv")
              readr::write_csv(DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table)), file_name)
              files <- c(file_name, files)
            }
          }
        }
        
        zip::zipr(file, files, include_directories = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - output$db_save"))
      }
    )
    
    # --- --- --- --- --- -
    # Restore database ----
    # --- --- --- --- --- -
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been restored
    
    observeEvent(input$db_restore_browse, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$db_restore_browse"))
      shinyjs::click("db_restore")
    })
    
    output$db_restore_status <- renderUI({
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - output$db_restore_status"))
      
      tagList(div(
      span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"), 
      span(input$db_restore$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
    })
    
    observeEvent(input$db_restore_button, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_app_database - observer input$db_restore_button"))
      
      req(input$db_restore)
      
      # Restore database
      
      tryCatch({
        
        exdir <- paste0(find.package("cdwtools"), "/data/temp/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"))
        dir.create(paste0(find.package("cdwtools"), "/data/"), showWarnings = FALSE)
        dir.create(paste0(find.package("cdwtools"), "/data/temp/"), showWarnings = FALSE)
        dir.create(exdir)
        
        zip::unzip(input$db_restore$datapath, exdir = exdir)
        csv_files <- zip::zip_list(input$db_restore$datapath)
        
        lapply(csv_files$filename, function(file_name){
          
          # Name of the table
          table <- substr(file_name, 1, nchar(file_name) - 4)
          
          # For older versions (when cache was downloaded when you clicked on save database)
          # (and when plugins_options table existed)
          if (table %not_in% c("cache", "plugins_options")){
            
            if (table != "log" | (table == "log" & input$db_import_log)){
            
              # Load CSV file
              col_types_temp <- col_types %>% dplyr::filter(table == !!table) %>% dplyr::pull(col_types)
              temp <- readr::read_csv(paste0(exdir, "/", file_name), col_types = col_types_temp, show_col_types = FALSE)
  
              # Delete data from old table
              sql <- glue::glue_sql("DELETE FROM {`table`}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
  
              # Insert new data in table
              DBI::dbAppendTable(r$db, table, temp)
            }
          }
          
          # Delete temp file
          # file.remove(paste0(exdir, "/", file_name))
        })
        
        # Remove temp dir
        unlink(paste0(find.package("cdwtools"), "/data/temp"), recursive = TRUE, force = TRUE)
        
        # Load database, restored
        load_database(r = r, i18n = i18n)
        
        # If restore is a success, save in database
        
        last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")
        
        if (nrow(last_restore) == 0) {
          
          # Insert last time row
          last_row <- get_last_row(r$db, "options")
          
          new_data <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
            as.integer(last_row + 1), "last_db_restore", NA_integer_, "last_db_restore", as.character(Sys.time()),
            NA_real_, as.integer(r$user_id), as.character(Sys.time()), FALSE)
          query <- DBI::dbAppendTable(r$db, "options", new_data)
          r$options <- r$options %>% dplyr::bind_rows(new_data)
        }
        
        else {
          new_datetime <- as.character(Sys.time())
          sql <- glue::glue_sql("UPDATE options SET value = {new_datetime}, datetime = {new_datetime} WHERE category = 'last_db_restore' AND name = 'last_db_restore'", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$options <- r$options %>% dplyr::mutate(
            value = dplyr::case_when(category == "last_db_restore" & name == "last_db_restore" ~ new_datetime, TRUE ~ value),
            datetime = dplyr::case_when(category == "last_db_restore" & name == "last_db_restore" ~ new_datetime, TRUE ~ datetime)
          )
        }
        
        # update_r(r = r, table = "options")
        
        show_message_bar(output, 3, "database_restored", "success", i18n = i18n, ns = ns, time = 15000)
      },
      error = function(e) report_bug(r = r, output = output, error_message = "error_restoring_database", 
        error_name = paste0(id, " - restore database"), category = "Error", error_report = e, i18n = i18n, ns = ns))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_app_database - observer input$db_restore_button"))
    })
    
  })
}
