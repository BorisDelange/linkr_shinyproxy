#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(page_style, router){
  function( input, output, session ) {
    if (page_style == "fluent") router$server(input, output, session)
    
    # mod_settings_data_management_server("settings_studies", page_style, page)
    mod_settings_data_management_server("settings_studies")
    # mod_settings_data_management_server("page_header_ui_settings")
    # mod_page_main_server("page_main_ui_home/datamarts_studies")
  }
}
