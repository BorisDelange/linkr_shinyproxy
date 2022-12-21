#' Add a log entry
#'
#' @param r Shiny reactive value r used to communicate between modules
#' @param category Category of the log entry, eg : "SQL query" (character)
#' @param name Name of the log entry, eg : "Add a new datamart" (character)
#' @param value Value of the log entry, eg : "INSERT INTO ..."

add_log_entry <- function(r, category, name, value){
  
  con <- isolate(r$db)
  
  id_row <- get_last_row(con, "log") + 1
  datetime <- as.character(Sys.time())
  
  sql <- glue::glue_sql("INSERT INTO log(id, category, name, value, creator_id, datetime)
    SELECT {id_row}, {category}, {name}, {value}, {isolate(r$user_id)}, {datetime}", 
    .con = con)
  
  query <- DBI::dbSendStatement(con, sql)
  DBI::dbClearResult(query)
}

#' Add log entry - report bug
#' 
#' @param r Shiny r reactive value, used to communicate between modules
#' @param output Shiny output variable
#' @param error_message Error message that will be displayed to user, after translation (character())
#' @param error_name Name of the error, to add an entry in the log table (character)
#' @param category Category : error or warning ? (character)
#' @param error_report Report of the error (character)
#' @param language Language used for translations (character)

report_bug <- function(r = shiny::reactiveValues(), output, error_message = character(), 
  error_name = character(), category = character(), error_report = character(), language = "EN"){
  
  print(error_report)
  
  # Notification to user
  show_message_bar(output, 3, error_message, "severeWarning", language)
  
  # Add a log entry for bug report
  add_log_entry(r = r, category = category, name = error_name, value = error_report)
}

report_bug_new <- function(r = shiny::reactiveValues(), output, error_message = character(), 
  error_name = character(), category = character(), error_report = character(), i18n = R6::R6Class()){
  
  print(error_report)
  
  # Notification to user
  if (error_message %not_in% c("fail_load_datamart", "fail_load_scripts")) show_message_bar_new(output, 3, error_message, "severeWarning", i18n = i18n)
  
  # Add a log entry for bug report
  add_log_entry(r = r, category = category, name = error_name, value = error_report)
}