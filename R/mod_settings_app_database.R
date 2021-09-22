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
          shiny.fluent::PrimaryButton.shinyInput(ns("save"), translate(language, "save")),
          shiny::conditionalPanel(condition = "input.db_connection_type == 'distant'", ns = ns,
            shiny.fluent::PrimaryButton.shinyInput(ns("test_connection"), translate(language, "test_connection"))),
        ), htmltools::br(),
        div(shiny::verbatimTextOutput(ns("test_connection_result")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
    ),
    make_card(
      translate(language, "app_db_tables"),
      shiny.fluent::DetailsList(
        compact = TRUE,
        items = list(
          list(key = "1", table_name = "Users", nrows = 51),
          list(key = "2", table_name = "Datamarts", nrows = 13)
        ),
        columns = list(
          list(key = "table_name", fieldName = "table_name", name = "Table name", minWidth = 200, maxWidth = 200, isResizable = TRUE),
          list(key = "nrows", fieldName = "nrows", name = "Num of rows", minWidth = 200, maxWidth = 200, isResizable = TRUE)
        )
      )
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
      output$test_connection_result <- renderText({
        captured_output <- capture.output(eval(parse(text = code)))
        if (grepl("Error", captured_output)) result <- captured_output
        else result <- "Succès"
        result
      })
    })
  })
}
    
## To be copied in the UI
# mod_settings_app_database_ui("settings_app_database_ui_1")
    
## To be copied in the server
# mod_settings_app_database_server("settings_app_database_ui_1")
