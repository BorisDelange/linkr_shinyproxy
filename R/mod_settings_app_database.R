#' settings_app_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_app_database_ui <- function(id, language){
  ns <- NS(id)
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    render_settings_toggle_card(language = language, ns = ns, cards = list(
      list(key = "db_connection_infos_card", label = "db_connection_infos_card"),
      list(key = "db_datatable_card", label = "db_datatable_card"),
      list(key = "db_request_card", label = "db_request_card"),
      list(key = "db_save_card", label = "db_save_card"),
      list(key = "db_restore_card", label = "db_restore_card"))),
    div(
      id = ns("db_connection_infos_card"),
      make_card(
        translate(language, "connection_infos"),
        div(
          div(
            div(class = "input_title", translate(language, "connection_type")),
            shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type"), value = "local", options = list(
                list(key = "local", text = translate(language, "local")),
                list(key = "distant", text = translate(language, "distant"))
              ), className = "inline_choicegroup")
          ),
          shiny::conditionalPanel(
            condition = "input.connection_type == 'distant'", ns = ns,
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 50),
              make_dropdown(language, ns, "sql_lib", options = list(
                list(key = "postgres", text = "PostgreSQL"),
                list(key = "sqlite", text = "SQLite")
              ), value = "postgres", width = "250px"),
              make_textfield(language, ns, "dbname", width = "250px"),
              make_textfield(language, ns, "host", width = "250px")
            ),
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 50),
              make_textfield(language, ns, "port", width = "250px"),
              make_textfield(language, ns, "user", width = "250px"),
              make_textfield(language, ns, "password", type = "password", canRevealPassword = TRUE, width = "250px")
            )), htmltools::br(),
          shiny.fluent::Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 20),
            shiny.fluent::PrimaryButton.shinyInput(ns("db_connection_save"), translate(language, "save")), " ",
            shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, shiny.fluent::PrimaryButton.shinyInput(ns("test_connection"), translate(language, "test_connection"))),
            shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, div(shiny::textOutput(ns("test_connection_success")), style = "padding-top:5px; font-weight:bold; color:#0078D4;")),
            shiny::conditionalPanel(condition = "input.connection_type == 'distant'", ns = ns, div(shiny::textOutput(ns("test_connection_failure")), style = "padding-top:5px; color:red;"))
          ),
        )
      )
    ),
    div(
      id = ns("db_datatable_card"),
      make_card(
        translate(language, "app_db_tables"),
        div(
          br(), shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_tables"), value = "local", options = list(
            list(key = "local", text = translate(language, "local")),
            list(key = "distant", text = translate(language, "distant"))
          ), className = "inline_choicegroup"),
          DT::DTOutput(ns("app_db_tables"))
        )
      )
    ),
    div(
      id = ns("db_request_card"),
      make_card(
        translate(language, "app_db_request"),
        div(
          shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_request"), value = "local", options = list(
            list(key = "local", text = translate(language, "local")),
            list(key = "distant", text = translate(language, "distant"))
          ), className = "inline_choicegroup"),
          div(shinyAce::aceEditor(ns("app_db_request"), "", "sql",
            autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
          div(shiny::verbatimTextOutput(ns("request_result")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
          htmltools::br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("request"), translate(language, "request"))
        )
      )
    ),
    div(
      id = ns("db_save_card"),
      make_card(
        translate(language, "db_save"),
        div(
          br(), uiOutput(ns("last_db_save")), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("db_save_button"), translate(language, "export_db"), iconProps = list(iconName = "Download")),
          div(style = "display:none;", downloadButton(ns("db_save"), label = ""))
        )
      )
    ),
    div(
      id = ns("db_restore_card"),
      make_card(
        translate(language, "db_restore"),
        div(
          br(), uiOutput(ns("last_db_restore")), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("db_restore_button"), translate(language, "restore_db"), iconProps = list(iconName = "Upload")),
          div(style = "display:none;", fileInput(ns("db_restore"), label = "", multiple = FALSE, accept = ".tar"))
        )
      )
    )
  )
}
    
#' settings_app_database Server Functions
#'
#' @noRd 

mod_settings_app_database_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    toggles <- c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card", "db_restore_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Database connection                    #
    ##########################################
    
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
    
      ##########################################
      # Save modif on connection infos         #
      ##########################################
    
      # When save button is clicked
      
      observeEvent(input$db_connection_save, {
        
        # If connection_type is local, save only connection_type but do not erase other informations (distant DB informations)
        if (input$connection_type == "local"){
          query <- "UPDATE options SET value = 'local' WHERE category = 'distant_db' AND name = 'connection_type'"
          DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
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
          sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = translate(language, paste0("provide_valid_", name))))
          
          req(db_checks[["dbname"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
          
          # If checks OK, insert data in database
          sapply(c("connection_type", "sql_lib", "dbname", "host", "port", "user", "password"), function(name){
            query <- paste0("UPDATE options
              SET value = '", input[[name]], "', creator_id = ", r$user_id, ", datetime = '", as.character(Sys.time()), "'
              WHERE category = 'distant_db' AND name = '", name, "'")
            DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
          })
        }
        
        # Reload r$db variable
        r$db <- get_db(db_info = list(), language = language)
        
        show_message_bar(output, 1, "modif_saved", "success", language)
      })
      
      ##########################################
      # Test connection                        #
      ##########################################
      
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
        
        sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = translate(language, paste0("provide_valid_", name))))
        
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
        
        if (!grepl("exception|error|warning|fatal", tolower(result))) result_success <- paste0(translate(language, "success"), " !")
        if (grepl("exception|error|warning|fatal", tolower(result))) result_failure <- result
        
        output$test_connection_success <- renderText(result_success)
        output$test_connection_failure <- renderText(result_failure)
      })
    
    ##########################################
    # Database tables datatable              #
    ##########################################
    
    observeEvent(input$connection_type, {
      
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
        if (test_distant_db(local_db = r$local_db, language = language) == "success"){
          distant_db <- get_distant_db(r$local_db)
          tibble::tibble(name = DBI::dbListTables(distant_db),
            row_number = sapply(DBI::dbListTables(distant_db),
              function(table) DBI::dbGetQuery(distant_db, paste0("SELECT COUNT(*) FROM ", table)) %>% 
                dplyr::pull() %>% as.integer())) -> data
        }
      }
      
      colnames(data) <- c(translate(language, "table_name"), translate(language, "row_number"))
      
      output$app_db_tables <- DT::renderDT(
        data,
        options = list(dom = "t<'bottom'p>",
          columnDefs = list(
            list(className = "dt-left", targets = "_all")
          )),
        rownames = FALSE, selection = "none")
    })
    
    ##########################################
    # Database request                       #
    ##########################################
    
    observeEvent(input$request, {
      
      output$request_result <- renderText({
        
        # Change this option to display correctly tibble in textbox
        eval(parse(text = "options('cli.num_colors' = 1)"))
        
        # Capture console output of our code
        captured_output <-
          tryCatch({
            
            # Replace \r with \n to prevent bugs
            request <- isolate(input$app_db_request %>% stringr::str_replace_all("\r", "\n"))
            
            # Get local or distant db DBI object
            if (input$connection_type_request == "local") db <- r$local_db
            if (input$connection_type_request == "distant") db <- get_distant_db(r$local_db)
            
            # dbSendStatement if it is not a select
            if (!grepl("^select", tolower(request))) capture.output({
              DBI::dbSendStatement(db, request) -> query
              print(query)
              DBI::dbClearResult(query)
            }) -> result
            
            # Else, a dbGetQuery
            else capture.output(DBI::dbGetQuery(db, request) %>% tibble::as_tibble()) -> result
            
            # Render result
            result
            
          }, error = function(e) print(e), warning = function(w) print(w))
        
        # Restore normal value
        eval(parse(text = "options('cli.num_colors' = NULL)"))
        
        # Display result
        paste(captured_output, collapse = "\n")
      })
    })
    
    ##########################################
    # Save database                          #
    ##########################################
    
    # Last time the db was saved
    
    observeEvent(r$options, {
      
      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")
      
      if (nrow(last_save) > 0) last_save <- last_save %>% dplyr::pull(value)
      else last_save <- translate(language, "never")
      
      output$last_db_save <- renderUI(tagList(strong(translate(language, "last_db_save")), " : ", last_save))
    })
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been saved
    
    observeEvent(input$db_save_button, {
      
      shinyjs::click("db_save")
      
      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")
      
      if (nrow(last_save) == 0) {
        
        # Insert last time row
        last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
        query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO options(id, category, name, value, creator_id, datetime, deleted) ",
          "SELECT ", last_row + 1, ", 'last_db_save', 'last_db_save', '", as.character(Sys.time()), "', ", r$user_id, ", ",
          "'", as.character(Sys.time()), "', FALSE"))
        DBI::dbClearResult(query)
      }
      
      else {
        query <- DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", as.character(Sys.time()), "', datetime = '", as.character(Sys.time()), "'",
          " WHERE category = 'last_db_save' AND name = 'last_db_save'"))
        DBI::dbClearResult(query)
      }
      
      update_r(r = r, table = "options", language = language)
      
    })
    
    # Download all tables in one tar file
    
    output$db_save <- downloadHandler(
      
      filename = function() { paste0("cdwtools_svg_", as.character(Sys.Date()), ".tar") },
      
      content = function(file){
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        files <- NULL
        
        tables <- DBI::dbListTables(r$db)

        for (table in tables){
          file_name <- paste0(table, ".csv")
          readr::write_csv(DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table)), file_name)
          files <- c(file_name, files)
        }
        
        tar(file, files)
      }
    )
    
    ##########################################
    # Restore database                       #
    ##########################################
    
    # Last time the db was restored
    
    observeEvent(r$options, {
      
      last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")
      
      if (nrow(last_restore) > 0) last_restore <- last_restore %>% dplyr::pull(value)
      else last_restore <- translate(language, "never")
      
      output$last_db_restore <- renderUI(tagList(strong(translate(language, "last_db_restore")), " : ", last_restore))
    })
    
    # Overcome absence of downloadButton in shiny.fluent
    # And save that the database has been restored
    
    observeEvent(input$db_restore_button, {
      
      shinyjs::click("db_restore")
      
      last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")
      
      if (nrow(last_restore) == 0) {
        
        # Insert last time row
        last_row <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
        query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO options(id, category, name, value, creator_id, datetime, deleted) ",
          "SELECT ", last_row + 1, ", 'last_db_restore', 'last_db_restore', '", as.character(Sys.time()), "', ", r$user_id, ", ",
          "'", as.character(Sys.time()), "', FALSE"))
        DBI::dbClearResult(query)
      }
      
      else {
        query <- DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", as.character(Sys.time()), "', datetime = '", as.character(Sys.time()), "'",
          " WHERE category = 'last_db_restore' AND name = 'last_db_restore'"))
        DBI::dbClearResult(query)
      }
      
      update_r(r = r, table = "options", language = language)
      
    })
    
    # How to do is here : https://techinplanet.com/read-zip-file-containing-multiple-csv-tables-in-r-shiny-app/
    
  })
}