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
          div(
            class = glue::glue("card ms-depth-8 ms-sm{8} ms-xl{8}"),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Text(variant = "large", "Datamarts", block = TRUE),
              "Test"
            )
          )
          # make_card(
          #   "My first card !",
          #   div(
          #     shiny.fluent::Text("Welcome")
          #   )),
          # make_card(
          #   "Second one",
          #   div(
          #     shiny.fluent::Text("OK")
          #   )),
          # shiny.fluent::Stack(
          #   horizontal = TRUE,
          #   tokens = list(childrenGap = 10),
          #   make_card(
          #     "This is a third card", 
          #     div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
          #     size = 4,
          #   ),
          #   make_card(
          #     "This is a 4th card", 
          #     div(shiny.fluent::Text("Test of a text"), br(), shiny.fluent::Text("Another text")),
          #     size = 8,
          #   )
          # )
        )
      ) -> result
    }
    
    if (page == "patient_level_data"){
      div(class = "main",
        shiny.fluent::Pivot(
          shiny.fluent::PivotItem(headerText = "Stays",
            div(
              make_card(
                "Stays timeline",
                "My timeline"
              )
            )
          ),
          shiny.fluent::PivotItem(headerText = "Notes",
            div(
              make_card(
                "Clinical notes",
                "Notes"
              )
            )
          )
        )
      ) -> result
    }
    
    if (page == "aggregated_data"){
      div(class = "main",
        shiny.fluent::Breadcrumb(
          items = list(
            list(text = "Data cleaning", key = "data_cleaning", href = ""),
            list(text = "Outliers", key = "outliers", href = "")
          ),
          maxDisplayedItems = 3
        ),
        shiny.fluent::Pivot(
          shiny.fluent::PivotItem(headerText = "Outliers",
            div(
              make_card(
                "Outliers",
                div(htmltools::br(),
                "Choose of the outliers")
              )
            )
          ),
          shiny.fluent::PivotItem(headerText = "Missing data",
            div(
              make_card(
                "Missing data",
                div(htmltools::br(),
                "Showing missing data")
              )
            )
          )
        )
      ) -> result
    }
    
    if (grepl("^settings", page)){
      if(page == "settings/general"){
        div(class = "main",
          make_card(
            translate(language, "general"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 30),
                div(
                  div(class = "dropdown_title", translate(language, "language")),
                  div(shiny.fluent::Dropdown.shinyInput("language", value = "EN", options = list(
                    list(key = "EN", text = "English"),
                    list(key = "FR", text = "Français"))),
                    style = "width:200px"
                  )
                ),
                div(
                  div(class = "dropdown_title", translate(language, "dev_mode")),
                  div(shiny.fluent::Toggle.shinyInput("dev_mode", value = TRUE))
                )
              ),
              htmltools::br(),
              htmltools::div(
                shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save")),
              )
            )
          ),
          # div(shinyAce::aceEditor("css_code", mode = "css", height = 300, value = "CSS code"), style = "width: 300px;")
          make_card(
            translate(language, "appearance"),
            div(
              shiny.fluent::Pivot(
                shiny.fluent::PivotItem(headerText = translate(language, "choices"),
                  div(
                    shiny.fluent::Stack(
                      horizontal = TRUE,
                      tokens = list(childrenGap = 50),
                      div(br(),
                        div(class = "dropdown_title", translate(language, "page_type")),
                        shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
                          list(key = "fluent", text = "Fluent UI"),
                          list(key = "fluid", text = "Fluid UI")
                        ))
                      ),
                      div(br(),
                        div(class = "dropdown_title", translate(language, "page_theme")),
                        div(
                          shiny.fluent::Dropdown.shinyInput("page_theme",  translate(language, "page_theme"),
                            value = "darker", options = list(
                            list(key = "darker", text = "Darker"),
                            list(key = "light", text = "Light"),
                            list(key = "neutralQuaternary", text = "neutralQuaternary")
                          )),
                          style = "min-width: 200px;"
                        )
                      )
                    ), br(),
                    shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
                  )
                ),
                shiny.fluent::PivotItem(headerText = translate(language, "code"), br(),
                  shiny.fluent::TextField.shinyInput(
                    "code_css",
                    value = ".header {\n  grid-area: header;\n  background-color: #fff;\n  display: flex;\n}",
                    multiline = TRUE, autoAdjustHeight = TRUE, underlined = FALSE
                  ), br(),
                  shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
                  # div(shinyAce::aceEditor("css_code", mode = "css", height = 300, value = "CSS code"), style = "width: 500px;")
                )
              )
            )
          )
        ) -> result
      }
      
      if (page == "settings/app_db"){
        div(class = "main",
          make_card(translate(language, "app_db"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                div(div(class = "dropdown_title", translate(language, "db_connexion_type")),
                    shiny.fluent::ChoiceGroup.shinyInput("db_connexion_type", value = "local", options = list(
                      list(key = "local", text = translate(language, "local")),
                      list(key = "distant", text = translate(language, "distant"))
                    ))
                )
              ), br(),
              shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
            )
          )
        ) -> result
      }
    }
  }
  
  if (page_style == "fluid"){
    if (page == "home"){
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Datamarts", htmltools::br(), "Datamarts"),
          shiny::tabPanel("Messages", htmltools::br(), "Messages")
        )
      ) -> result
    }
    
    if (page == "patient_level_data"){
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Stays", htmltools::br(), "Patient stays"),
          shiny::tabPanel("Notes", htmltools::br(), "Patient clinical notes")
        )
      ) -> result 
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
