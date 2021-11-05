#' page_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_footer_ui <- function(words = tibble::tibble()){
  div(class = "footer", 
    shiny.fluent::Stack(
      horizontal = TRUE,
      horizontalAlign = 'space-between',
      tokens = list(childrenGap = 20),
      tags$a(icon("github"), "", href = "https://github.com/BorisDelange/cdwtools", target="_blank"),
      shiny.fluent::Text(variant = "medium", nowrap = FALSE, ""),
      shiny.fluent::Text(variant = "medium", nowrap = FALSE, "Version 0.0.2")
    )
  )
}