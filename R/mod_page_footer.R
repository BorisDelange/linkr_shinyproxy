#' page_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_footer_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  div(class = "footer", 
    shiny.fluent::Stack(
      horizontal = TRUE,
      horizontalAlign = 'space-between',
      tokens = list(childrenGap = 20),
      # shiny.fluent::CommandBar(
      #   items = list(
      #     shiny.fluent::CommandBarItem("Git Hub", "GitHubLogo", iconOnly = TRUE)
      #   )
      # )
      shiny.fluent::IconButton("github", href = "https://github.com/BorisDelange/cdwtools",
                               iconProps = list(iconName = "GitHubLogo"))
    )    
  )
}
    
#' page_footer Server Functions
#'
#' @noRd 
mod_page_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_page_footer_ui("page_footer_ui_1")
    
## To be copied in the server
# mod_page_footer_server("page_footer_ui_1")
