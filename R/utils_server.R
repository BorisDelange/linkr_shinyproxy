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

message_bar <- function(id = integer(), message = character(), type = "severeWarning", language = "EN", time = 3000){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = type), style = "margin-top:10px;"))
}