#' Display plugin error message
#' 
#' @param message Error message to display (character)

display_plugin_error <- function(output, language, message){
  if (nchar(message[1]) > 0) show_message_bar(output, 2, paste0(translate(language, "error_run_plugin_server_code"), " : ", toString(message)), "severeWarning", language)   
}