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
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    
    ##########################################
    # Fluent / Home                          #
    ##########################################
    
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
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Patient-level data            #
    ##########################################
    
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
    
    ##########################################
    # Fluent / Aggregated data               #
    ##########################################
    
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
    
    ##########################################
    # Fluent / Settings                      #
    ##########################################
    
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
                  div(class = "input_title", translate(language, "language")),
                  div(shiny.fluent::Dropdown.shinyInput("language", value = "EN", options = list(
                    list(key = "EN", text = "English"),
                    list(key = "FR", text = "Français"))),
                    style = "width:200px"
                  )
                ),
                div(
                  div(class = "input_title", translate(language, "dev_mode")),
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
                        div(class = "input_title", translate(language, "page_type")),
                        shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
                          list(key = "fluent", text = "Fluent UI"),
                          list(key = "fluid", text = "Fluid UI")
                        ))
                      ),
                      div(br(),
                        div(class = "input_title", translate(language, "page_theme")),
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
      
      ##########################################
      # Fluent / Settings / App database       #
      ##########################################
      
      if (page == "settings/app_db"){
        div(class = "main",
          make_card(translate(language, "app_db"),
            div(
              div(
                div(class = "input_title", translate(language, "db_connexion_type")),
                  shiny.fluent::ChoiceGroup.shinyInput(ns("db_connexion_type"), value = "local", options = list(
                    list(key = "local", text = translate(language, "local")),
                    list(key = "distant", text = translate(language, "distant"))
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "input.db_connexion_type == 'distant'", ns = ns,
                shiny.fluent::Stack(
                  horizontal = TRUE,
                  tokens = list(childrenGap = 50),
                  make_textfield(language, ns, "dbname"),
                  make_textfield(language, ns, "host"),
                  make_textfield(language, ns, "port"),
                  make_textfield(language, ns, "user"),
                  make_textfield(language, ns, "password", "password", TRUE)
                )
              ), br(),
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 20),
                shiny.fluent::PrimaryButton.shinyInput(ns("save"), translate(language, "save")),
                shiny.fluent::PrimaryButton.shinyInput(ns("test_connection"), translate(language, "test_connection"))
              )
            )
          )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Users              #
      ##########################################
      
      if (page == "settings/users"){
        div(class = "main",
          make_card(translate(language, "create_user"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(language, ns, "username"),
                make_textfield(language, ns, "first_name"),
                make_textfield(language, ns, "last_name"),
                make_textfield(language, ns, "password", type = "password", canRevealPassword = TRUE)
              ), br(),
              shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
            )          
          )  
        ) -> result
      }
    }
  }
  
  ##########################################
  # Fluid                                  #
  ##########################################
  
  if (page_style == "fluid"){
    
    ##########################################
    # Fluid / Home                           #
    ##########################################
    
    if (page == "home"){
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Datamarts", htmltools::br(), "Datamarts"),
          shiny::tabPanel("Messages", htmltools::br(), "Messages")
        )
      ) -> result
    }
    
    ##########################################
    # Fluid / Patient-level data             #
    ##########################################
    
    if (page == "patient_level_data"){
      shiny::mainPanel(
        # class = "fluid_main",
        shiny::tabsetPanel(
          shiny::tabPanel("Stays", htmltools::br(), "Patient stays"),
          shiny::tabPanel("Notes", htmltools::br(), "Patient clinical notes")
        )
      ) -> result 
    }
  }
  
  result
}

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
