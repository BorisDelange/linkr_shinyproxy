#' settings_app_database UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_app_database_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  div(class = "main",
    shiny::uiOutput(ns("warnings1")),
    app_database_toggle_card(language, ns, activated = "db_connection_infos_card"),
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
          shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type_datatable"), value = "local", options = list(
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
          div(shinyAce::aceEditor(ns("app_db_request"), "", "sql", height = "200px"), style = "width: 100%;"),
          div(shiny::verbatimTextOutput(ns("request_result")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"),
          htmltools::br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("request"), translate(language, "request"))
        )
      )
    )
  ) -> result
  
  result
}
    
#' settings_app_database Server Functions
#'
#' @noRd 
mod_settings_app_database_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    toggles <- c("db_connection_infos_card", "db_datatable_card", "db_request_card")
    
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
      db_info <- DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
      db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
      sapply(names(db_info), function(name){
        if (name == "connection_type") shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = db_info[[name]])
        if (name != "connection_type") shiny.fluent::updateTextField.shinyInput(session, name, value = db_info[[name]])
      })
    })
    
      ##########################################
      # Save modif on connection infos         #
      ##########################################
      
      observeEvent(input$db_connection_save, {
        if (input$connection_type == "local"){
          query <- "UPDATE options SET value = 'local' WHERE category = 'distant_db' AND name = 'connection_type'"
          DBI::dbClearResult(DBI::dbSendStatement(r$local_db, query))
        }
        
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
        
        output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;"))
        shinyjs::show("warnings1")
        shinyjs::delay(3000, shinyjs::hide("warnings1"))
      })
      
      ##########################################
      # Test connection                        #
      ##########################################
      
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
    
    observeEvent(input$connection_type_datatable, {
      if (input$connection_type_datatable == "local"){
        tibble::tibble(name = DBI::dbListTables(r$local_db),
          row_number = sapply(DBI::dbListTables(r$local_db), 
            function(table) DBI::dbGetQuery(r$local_db, paste0("SELECT COUNT(*) FROM ", table)) %>% dplyr::pull())) -> data
      } 
      if (input$connection_type_datatable == "distant"){
        data <- tibble::tibble(name = character(), row_number = integer())
        if (test_distant_db(r$local_db) == "success"){
          distant_db <- get_distant_db(r$local_db)
          tibble::tibble(name = DBI::dbListTables(distant_db),
                         row_number = sapply(DBI::dbListTables(distant_db),
                                             function(table) DBI::dbGetQuery(distant_db, paste0("SELECT COUNT(*) FROM ", table)) %>% dplyr::pull())) -> data
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
            request <- isolate(input$app_db_request)
            if (input$connection_type_request == "local") db <- r$local_db
            if (input$connection_type_request == "distant") db <- get_distant_db(r$local_db)
            if (grepl("select", tolower(request))) capture.output(DBI::dbGetQuery(db, request) %>% tibble::as_tibble()) -> result
            if (grepl("update|delete|insert", tolower(request))) capture.output({
              DBI::dbSendStatement(db, request) -> query
              print(query)
              DBI::dbClearResult(query)
            }) -> result
            result
          }, error = function(e) print(e), warning = function(w) print(w))
        
        # Restore normal value
        eval(parse(text = "options('cli.num_colors' = NULL)"))
        # Display result
        paste(captured_output, collapse = "\n")
      })
    })
    
  })
}
    
## To be copied in the UI
# mod_settings_app_database_ui("settings_app_database_ui_1")
    
## To be copied in the server
# mod_settings_app_database_server("settings_app_database_ui_1")
