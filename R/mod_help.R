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
    
  # div(class = "main",
  #     tags$style("h1 {font-size: 18px;} h2 {font-size: 16px;} h3 {font-size: 14px;}"),
  #     div(
  #       class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
  #       style = "padding-top: 8px;",
  #       htmltools::includeHTML(app_sys(paste0("app/www/help_pages/", language, "_", substr(id, nchar("help_") + 1, nchar(id)), ".html")))
  #     )
  # ) -> result
  # tags$iframe(src = paste0(golem::get_golem_wd(), srcdoc = "/inst/app/www/help_pages/EN_dev_data_management.html", seamless = NA))
  # div(class = "main",
  div(
    uiOutput(ns("test")),
    style = "height:100%"
  )
  # }
  
  
}
    
#' help Server Functions
#'
#' @noRd 
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$test <- renderUI({
      # div(paste0(golem::get_golem_wd(), "/inst/app/www/help_pages/EN_dev_data_management.html"))
      # my_url <- RCurl::getURL("http://www.google.fr")
      # includeHTML(paste0(RCurl::getURL("http://www.google.fr")))
      # tags$iframe(id = "app", srcdoc = RCurl::getURL("http://www.google.fr"), width = "100%")
      # e <- '<iframe id="app" src="URL" width="100%"></iframe>'
      # HTML(gsub("URL", url("http://www.google.fr"), e))
      
      # test <- "http://www.google.fr"
      tags$iframe(src = "https://pkgdown.r-lib.org/", height = "100%", width = "100%", style = "border:none", scrolling = "auto")
      # HTML(paste0("<iframe id = 'test2', src = '", golem::get_golem_wd(), "/inst/app/www/help_pages/EN_dev_data_management.html'></iframe>"))
    })
  })
}
    
## To be copied in the UI
# mod_help_ui("help_ui_1")
    
## To be copied in the server
# mod_help_server("help_ui_1")
