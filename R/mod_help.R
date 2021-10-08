#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

# Run locally only
# rmarkdown::render("inst/app/www/help_pages/EN_get_started.Rmd", output_format = "html_fragment", output_file = "EN_get_started.html")

mod_help_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  size <- 12
  
  # if (page == "help/get_started"){
    
  div(class = "main",
      tags$style("h1 {font-size: 18px;} h2 {font-size: 16px;} h3 {font-size: 14px;}"),
      div(
        class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
        style = "padding-top: 8px;",
        htmltools::includeHTML(app_sys(paste0("app/www/help_pages/", language, "_", substr(id, nchar("help_") + 1, nchar(id)), ".html")))
      )
  ) -> result
  # }
  
  
}
    
#' help Server Functions
#'
#' @noRd 
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_help_ui("help_ui_1")
    
## To be copied in the server
# mod_help_server("help_ui_1")
