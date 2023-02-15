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
              shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type"), value = "local", options = list(
                  list(key = "local", text = i18n$t("local")),
                  list(key = "distant", text = i18n$t("distant"))
                ), className = "inline_choicegroup")
            ),
            shiny::conditionalPanel(
              condition = "input.connection_type == 'distant'", ns = ns,
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_dropdown(i18n, ns, "sql_lib", options = list(
                  list(key = "postgres", text = "PostgreSQL"),
                  list(key = "sqlite", text = "SQLite")
                ), value = "postgres", width = "250px"),
                make_textfield(i18n, ns, "dbname", width = "250px"),
                make_textfield(i18n, ns, "host", width = "250px")
              ),
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(i18n, ns, "port", width = "250px"),
                make_textfield(i18n, ns, "user", width = "250px"),
                make_textfield(i18n, ns, "password", type = "password", canRevealPassword = TRUE, width = "250px")
              )), htmltools::br(),
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 20),
              shiny.fluent::PrimaryButton.shinyInput(ns("db_connection_save"), i18n$t("save")), " ",
              shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, shiny.fluent::PrimaryButton.shinyInput(ns("test_connection"), i18n$t("test_connection"))),
              shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, div(shiny::textOutput(ns("test_connection_success")), style = "padding-top:5px; font-weight:bold; color:#0078D4;")),
              shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, div(shiny::textOutput(ns("test_connection_failure")), style = "padding-top:5px; color:red;"))
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
            br(), shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_tables"), value = "local", options = list(
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
            shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_request"), value = "local", options = list(
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

mod_settings_app_database_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
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
    
    observeEvent(input$connection_type_tables, {
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = input$connection_type_tables)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_request", value = input$connection_type_tables)
    })
    
    observeEvent(input$connection_type_request, {
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = input$connection_type_request)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_tables", value = input$connection_type_request)
    })
    
    # --- --- --- --- --- -- -
    # Database connection ----
    # --- --- --- --- --- -- -
    
    observeEvent(r$local_db, {

      # Get distant db informations
      db_info <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
      db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()

      # Fill textfields & choicegroup with recorded informations in local database
      sapply(names(db_info), function(name){
        if (name == "connection_type"){
          shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = db_info[[name]])
          shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_tables", value = db_info[[name]])
          shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_request", value = db_info[[name]])
        }
        if (name != "connection_type") shiny.fluent::updateTextField.shinyInput(session, name, value = db_info[[name]])
      })
    })
    
    # --- --- --- --- --- --- --- --- --- -
    # Save updates on connection infos ----
    # --- --- --- --- --- --- --- --- --- -
  
    # When save button is clicked
    
    observeEvent(input$db_connection_save, {
      
      # If connection_type is local, save only connection_type but do not erase other informations (distant DB informations)
      if (input$connection_type == "local"){
        query <- "UPDATE options SET value = 'local' WHERE category = 'distant_db' AND name = 'connection_type'"
        DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
        add_log_entry(r = r, category = "SQL query", name = "Update SQL connection infos", value = query)
      }
      
      # If connection_type is distant, save connection_type and other distant DB informations
      if (input$connection_type == "distant"){
        
        # Checks inputs
        db_checks <- c("dbname" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
        
        sapply(names(db_checks), function(name){
          shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = NULL)
          if (!is.null(input[[name]])){
            if (name != "port" & input[[name]] != "") db_checks[[name]] <<- TRUE
            if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <<- TRUE
          }
        })
        sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = i18n$t(paste0("provide_valid_", name))))
        
        req(db_checks[["dbname"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
        
        # If checks OK, insert data in database
        sapply(c("connection_type", "sql_lib", "dbname", "host", "port", "user", "password"), function(name){
          query <- paste0("UPDATE options
            SET value = '", input[[name]], "', creator_id = ", r$user_id, ", datetime = '", as.character(Sys.time()), "'
            WHERE category = 'distant_db' AND name = '", name, "'")
          DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
          add_log_entry(r = r, category = "SQL query", name = "Update SQL connection infos", value = query)
        })
      }
      
      # Reload r$db variable
      r$db <- get_db(db_info = list(), app_db_folder = r$app_db_folder, i18n = i18n)
      
      show_message_bar(output, 1, "modif_saved", "success", i18n = i18n)
    })
    
    # --- --- --- --- -- -
    # Test connection ----
    # --- --- --- --- -- -
  
    # When test connection button is clicked
  
    observeEvent(input$test_connection, {
      
      # Before testing connection, make sure fields are filled
      db_checks <- c("dbname" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
      sapply(names(db_checks), function(name){
        shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = NULL)
        if (!is.null(input[[name]])){
          if (name != "port" & input[[name]] != "") db_checks[[name]] <<- TRUE
          if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <<- TRUE
        }
      })
      
      # Reset output textfields
      output$test_connection_success <- renderText("")
      output$test_connection_failure <- renderText("")
      
      sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = i18n$t(paste0("provide_valid_", name))))
      
      req(db_checks[["dbname"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
      
      # If checks are OK, test connection
      code <- paste0("DBI::dbConnect(RPostgres::Postgres(),
        dbname = '", input$dbname, "', host = '", input$host, "', port = ", input$port,
        ", user = '", input$user, "', password = '", input$password, "')")
      result_success <- ""
      result_failure <- ""
      result <- capture.output(
        tryCatch(eval(parse(text = isolate(code))), error = function(e) print(e), warning = function(w) print(w))
      )
      
      if (!grepl("exception|error|warning|fatal", tolower(result))) result_success <- paste0(i18n$t("success"), " !")
      if (grepl("exception|error|warning|fatal", tolower(result))) result_failure <- result
      
      output$test_connection_success <- renderText(result_success)
      output$test_connection_failure <- renderText(result_failure)
    })
    
    # --- --- --- --
    # DB tables ----
    # --- --- --- --
    
    observeEvent(input$connection_type, {
      
      # Update other ChoiceGroups
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_tables", value = input$connection_type)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type_request", value = input$connection_type)
      
      # Local database tables
      
      if (input$connection_type == "local"){
        tibble::tibble(name = DBI::dbListTables(r$local_db),
          row_number = sapply(DBI::dbListTables(r$local_db), 
            function(table) DBI::dbGetQuery(r$local_db, paste0("SELECT COUNT(*) FROM ", table)) %>% 
              dplyr::pull() %>% as.integer())) -> data
      } 
      
      # Distant database tables
      
      if (input$connection_type == "distant"){
        data <- tibble::tibble(name = character(), row_number = integer())
        if (test_distant_db(local_db = r$local_db, i18n = i18n) == "success"){
          distant_db <- get_distant_db(r$local_db)
          tibble::tibble(name = DBI::dbListTables(distant_db),
            row_number = sapply(DBI::dbListTables(distant_db),
              function(table) DBI::dbGetQuery(distant_db, paste0("SELECT COUNT(*) FROM ", table)) %>% 
                dplyr::pull() %>% as.integer())) -> data
        }
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
          pageLength = 30,
          language = dt_translation,
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          )),
        rownames = FALSE, selection = "none")
    })
    
    # --- --- --- ---
    # DB request ----
    # --- --- --- ---
    
    observeEvent(input$request, {
      r[[paste0(id, "_app_db_request_code")]] <- input$app_db_request_code
      r[[paste0(id, "_app_db_request_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$app_db_request_code_run_selection, {
      if(!shinyAce::is.empty(input$app_db_request_code_run_selection$selection)) r[[paste0(id, "_app_db_request_code")]] <- input$app_db_request_code_run_selection$selection
      else r[[paste0(id, "_app_db_request_code")]] <- input$app_db_request_code_run_selection$line
      r[[paste0(id, "_app_db_request_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$app_db_request_code_run_all, {
      r[[paste0(id, "_app_db_request_code")]] <- input$app_db_request_code
      r[[paste0(id, "_app_db_request_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(id, "_app_db_request_trigger_code")]], {
      
      output$request_result <- renderText({
        
        # Replace \r with \n to prevent bugs
        request <- isolate(r[[paste0(id, "_app_db_request_code")]] %>% stringr::str_replace_all("\r", "\n"))
        
        # Get local or distant db DBI object
        if (input$connection_type_request == "local") db <- r$local_db
        if (input$connection_type_request == "distant") db <- get_distant_db(r$local_db)
        
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
        paste(captured_output, collapse = "\n")
      })
    })
    
    # --- --- -- -
    # Save DB ----
    # --- --- -- -
    
    # Last time the db was saved
    # And current db

    observeEvent(r$options, {

      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")

      if (nrow(last_save) > 0) last_save <- last_save %>% dplyr::pull(value)
      else last_save <- i18n$t("never")

      output$last_db_save <- renderUI(tagList(strong(i18n$t("last_db_save")), " : ", last_save))

      current_db <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'distant_db'")
      connection_type <- current_db %>% dplyr::filter(name == "connection_type") %>% dplyr::pull(value)
      if (connection_type == "local") current_db_text <- paste0(i18n$t("local"), " (", r$app_db_folder, ")")
      else {
        sql_lib <- current_db %>% dplyr::filter(name == "sql_lib") %>% dplyr::pull(value)
        dbname <- current_db %>% dplyr::filter(name == "dbname") %>% dplyr::pull(value)
        current_db_text <- paste0(i18n$t("distant"), " (", sql_lib, " - ", dbname, ")")
      }

      output$current_db_save <- renderUI(tagList(strong(i18n$t("current_db")), " : ", current_db_text))
      output$current_db_restore <- renderUI(tagList(strong(i18n$t("current_db")), " : ", current_db_text))
    })
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been saved
    
    observeEvent(input$db_save_button, {
      
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
        
        # query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO options(id, category, name, value, creator_id, datetime, deleted) ",
        #   "SELECT ", last_row + 1, ", 'last_db_save', 'last_db_save', '", as.character(Sys.time()), "', ", r$user_id, ", ",
        #   "'", as.character(Sys.time()), "', FALSE"))
        # DBI::dbClearResult(query)
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
      
      # update_r(r = r, table = "options")
      
    })
    
    # Download all tables in one zip file
    
    output$db_save <- downloadHandler(
      
      filename = function() paste0("cdwtools_svg_", as.character(Sys.Date()), ".zip"),
      
      content = function(file){
        
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
      }
    )
    
    # --- --- --- --- --- -
    # Restore database ----
    # --- --- --- --- --- -
    
    # Last time the db was restored
    
    observeEvent(r$options, {
      
      last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")

      if (nrow(last_restore) > 0) last_restore <- last_restore %>% dplyr::pull(value)
      else last_restore <- i18n$t("never")

      output$last_db_restore <- renderUI(tagList(strong(i18n$t("last_db_restore")), " : ", last_restore))
    })
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been restored
    
    observeEvent(input$db_restore_browse, shinyjs::click("db_restore"))
    
    output$db_restore_status <- renderUI(tagList(div(
      span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"), 
      span(input$db_restore$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;")))
    
    observeEvent(input$db_restore_button, {
      
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
    })
    
  })
}
