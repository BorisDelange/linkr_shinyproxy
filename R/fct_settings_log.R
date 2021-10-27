#' Add a log entry
#'
#' @param r Shiny reactive value r used to communicate between modules
#' @param category Category of the log entry, eg : "SQL query" (character)
#' @param name Name of the log entry, eg : "Add a new datamart" (character)
#' @param value Value of the log entry, eg : "INSERT INTO ..."

add_log_entry <- function(r, category, name, value){
  
  id_row <- get_last_row(r$db, "log") + 1
  datetime <- as.character(Sys.time())
  
  sql <- glue::glue_sql("INSERT INTO log(id, category, name, value, creator_id, datetime)
    SELECT {id_row}, {category}, {name}, {value}, {r$creator_id}, {datetime}", 
    .con = r$db)
  
  query <- DBI::dbSendStatement(r$db, sql)
  DBI::dbClearResult(query)
}