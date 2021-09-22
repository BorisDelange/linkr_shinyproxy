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
    make_card(
      translate(language, "app_db"),
      div(
        div(
          div(class = "input_title", translate(language, "db_connection_type")),
          shiny.fluent::ChoiceGroup.shinyInput(ns("db_connection_type"), value = "local", options = list(
            list(key = "local", text = translate(language, "local")),
            list(key = "distant", text = translate(language, "distant"))
          ))
        ),
        shiny::conditionalPanel(
          condition = "input.db_connection_type == 'distant'", ns = ns,
          shiny.fluent::Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 50),
            make_textfield(language, ns, "dbname"),
            make_textfield(language, ns, "host"),
            make_textfield(language, ns, "port"),
            make_textfield(language, ns, "user"),
            make_textfield(language, ns, "password", type = "password", canRevealPassword = TRUE)
          )
        ), br(),
        shiny.fluent::Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = 20),
          shiny.fluent::PrimaryButton.shinyInput(ns("db_connection_save"), translate(language, "save")),
          shiny::conditionalPanel(condition = "input.db_connection_type == 'distant'", ns = ns,
            shiny.fluent::PrimaryButton.shinyInput(ns("test_connection"), translate(language, "test_connection"))),
          shiny::conditionalPanel(condition = "input.db_connection_type == 'distant'", ns = ns,
            div(
              div(shiny::textOutput(ns("test_connection_success")), style = "padding-top:5px; font-weight:bold; color:#0078D4;"),
              div(shiny::textOutput(ns("test_connection_failure")), style = "color:red;"))),
        ), htmltools::br()
      )
    ),
    make_card(
      translate(language, "app_db_tables"),
      DT::DTOutput(ns("app_db_tables"))
    ),
    make_card(
      translate(language, "app_db_request"),
      div(
        div(shinyAce::aceEditor("app_db_request", "SELECT * FROM my_table", "sql", height = "200px"), style = "width: 50%;"),
        shiny.fluent::PrimaryButton.shinyInput("request", translate(language, "request"))
      )
    )
  ) -> result
  
  result
}
    
#' settings_app_database Server Functions
#'
#' @noRd 
mod_settings_app_database_server <- function(id, r, language, db_info){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Database connection                    #
    ##########################################
    
    if (class(db_info) == "list" & length(db_info) != 0){
      shiny.fluent::updateChoiceGroup.shinyInput(session, "db_connection_type", value = "distant")
      if (!is.null(db_info$dbname)) shiny.fluent::updateTextField.shinyInput(session, "dbname", value = db_info$dbname)
      if (!is.null(db_info$host)) shiny.fluent::updateTextField.shinyInput(session, "host", value = db_info$host)
      if (!is.null(db_info$port)) shiny.fluent::updateTextField.shinyInput(session, "port", value = db_info$port)
      if (!is.null(db_info$user)) shiny.fluent::updateTextField.shinyInput(session, "user", value = db_info$user)
      if (!is.null(db_info$password)) shiny.fluent::updateTextField.shinyInput(session, "password", value = db_info$password)
    }
    
    observeEvent(input$test_connection, {
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
    
    observeEvent(input$db_connection_save, {
      # Checks inputs
      db_checks <- c("dbname" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
      sapply(names(db_checks), function(name){
        if (!is.null(input[[name]])){
          if (name != "port" & input[[name]] != "") db_checks[[name]] <<- TRUE
          if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <<- TRUE
        }
      })
      sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = translate(language, paste0("provide_valid_", name))))
      
      req(db_checks[["dbname"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
      
      # If checks OK, insert data in database
      last_row <- DBI::dbGetQuery(db, "SELECT MAX(id) FROM options")
      query <- paste0("INSERT INTO options(id, category, name, value, creator_id, datetime, deleted)
                       SELECT ", last_row + 1, ", 'distant_db', 'name', '", input$dbname, "', ", r$user_id, '", as.character(Sys.time()), "', FALSE)
      DBI::dbSendStatement(db, query)
      r$options <- DBI::dbGetQuery(db, "SELECT * FROM options")
      
    })
    
    ##########################################
    # Database tables datatable              #
    ##########################################
    
    output$app_db_tables <- DT::renderDT(
      tibble::tibble(name = DBI::dbListTables(db),
                     row_number = sapply(DBI::dbListTables(db), 
                       function(table) DBI::dbGetQuery(db, paste0("SELECT COUNT(*) FROM ", table)) %>% dplyr::pull())),
      options = list(dom = "t<'bottom'p>",
                     columnDefs = list(
                       list(className = "dt-left", targets = "_all")
                     )),
      rownames = FALSE, selection = "none")
  })
}
    
## To be copied in the UI
# mod_settings_app_database_ui("settings_app_database_ui_1")
    
## To be copied in the server
# mod_settings_app_database_server("settings_app_database_ui_1")
