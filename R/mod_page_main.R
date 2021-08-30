#' page_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_main_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- div()
  
  if (page_style == "fluent"){
    if (page == "home"){
      div(class = "main",
        div(
          make_card(
            "My first card !",
            div(
              shiny.fluent::Text("Welcome")
            )),
          make_card(
            "Second one",
            div(
              shiny.fluent::Text("OK")
            )),
          shiny.fluent::Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            make_card(
              "This is a third card", 
              div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
              size = 4,
            ),
            make_card(
              "This is a 4th card", 
              div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
              size = 8,
            )
          )
        )
      )
    }
    if (grepl("^settings", page)){
      if(page == "settings/general"){
        div(class = "main",
          make_card(
            translate(language, "language"),
            div(
              div(class = "dropdown_title", translate(language, "language")),
              div(shiny.fluent::Dropdown.shinyInput("language", value = "EN", options = list(
                list(key = "EN", text = "English"),
                list(key = "FR", text = "Français")
                )),
                style = "width:200px"
              ), br(),
              shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
            )
          ),
          make_card(
            translate(language, "appearance"),
            div(
              div(class = "dropdown_title", translate(language, "page_type")),
              shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
                list(key = "fluent", text = "Fluent UI"),
                list(key = "fluid", text = "Fluid UI")
              )), br(),
              shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
            )
          )
        ) -> result
      }
    }
  }
  
  result
}
# mod_page_main_ui <- function(id){
#   ns <- NS(id)
#   div(class = "main",
#     div(
#       make_card(
#         "My first card !",
#         div(
#           shiny.fluent::Text("Welcome")
#         )),
#       make_card(
#         "Second one",
#         div(
#           shiny.fluent::Text("OK")
#         )),
#       shiny.fluent::Stack(
#         horizontal = TRUE,
#         tokens = list(childrenGap = 10),
#         make_card(
#           "This is a third card", 
#           div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
#           size = 4,
#         ),
#         make_card(
#           "This is a 4th card", 
#           div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
#           size = 8,
#         )
#       )
#     )
#     # )
#   )
# }
    
#' page_main Server Functions
#'
#' @noRd 
mod_page_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_main_ui("page_main_ui_1")
    
## To be copied in the server
# mod_page_main_server("page_main_ui_1")
