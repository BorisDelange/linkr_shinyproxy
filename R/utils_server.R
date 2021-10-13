#' Display an error message
#' 
#' @description Displays an error message on the top of the page
#' @details Choose different warning IDs on one page, to allows multiple messages to be displayed.
#' The different possible types are : c("info", "error", "blocked", "severeWarning", "success", "warning")
#' @param id ID of the warning output used (integer)
#' @param message message that will be displayed, after translation (character)
#' @param type type of message bar displayed (reference to Microsoft MessageBarType enum) (character)
#' @param language language used for the translation (character)
#' @param time time the message bar will by displayed, in ms (integer)
#' @examples 
#' \dontrun {
#' message_bar(id = 2, message = "name_already_used", type = "severeWarning", language = language, time = 5000)
#' }

message_bar <- function(output, id = integer(), message = character(), type = "severeWarning", language = "EN", time = 3000){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  output[[paste0("message_bar", id)]] <- renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = type), style = "margin-top:10px;"))
}

#' Update r variable
#' 
#' @description Update r value, requesting the corresponding table in the database
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param table Database table name (character)
#' @param language language used for the translation (character)
#' @examples
#' \dontrun {
#' update_r(r = r, table = "subsets")
#' }

update_r <- function(r = shiny::reactiveValues(), table = character(), language = "EN"){
  tables <- c("users", "users_accesses_statuses", "users_accesses_details",
    "data_sources", "datamarts", "studies", "subsets", "thesaurus", "thesaurus_items",
    "plugins", "patient_lvl_module_families", "patient_lvl_modules", "patient_lvl_module_elements",
    "aggregated_module_families", "aggregated_modules", "code", "options")
  
  if (table %not_in% tables) stop(paste0(translate(language, "invalid_table_name"), ". ", translate(language, "tables_allowed"), " : ", toString(tables)))
  
  new_table <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
  r[[table]] <- new_table
  r[[paste0(table, "_temp")]] <- new_table %>% dplyr::mutate(modified = FALSE)
}