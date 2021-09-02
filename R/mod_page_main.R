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
      
      ##########################################
      # Fluent / General settings              #
      ##########################################
      
      if(page == "settings/general"){
        div(class = "main",
          # Hidden aceEditor, allows the other to be displayed...
          div(shinyAce::aceEditor("test"), style = "display: none;"),
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
          make_card(
            translate(language, "appearance"),
            div(
              # shiny.fluent::Pivot(
                # shiny.fluent::PivotItem(headerText = translate(language, "choices"),
                  div(
                    shiny.fluent::Stack(
                      horizontal = TRUE,
                      tokens = list(childrenGap = 50),
                      # shiny.fluent::Stack(
                      #   horizontal = FALSE,
                        div(
                          div(class = "input_title", translate(language, "page_type")),
                          shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
                            list(key = "fluent", text = "Fluent UI"),
                            list(key = "fluid", text = "Fluid UI")
                          )),
                          style = "min-width: 180px;"
                        ), htmltools::br(),
                        div(div(class = "input_title", translate(language, "page_theme")),
                          div(
                            shiny.fluent::Dropdown.shinyInput("page_theme",  translate(language, "page_theme"),
                              value = "darker", options = list(
                              list(key = "darker", text = "Darker"),
                              list(key = "light", text = "Light"),
                              list(key = "neutralQuaternary", text = "neutralQuaternary")
                            )),
                            style = "min-width: 200px;"
                          )
                        ),
                      # ),
                      div(shinyAce::aceEditor("css_code", ".title {\n  padding: 5px 10px 0px 10px;\n  color: #737373;\n}", "css", 
                                              height = "200px"), style = "width: 100%;")
                    ), br(),
                    shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
                  )
                # ),
                # shiny.fluent::PivotItem(headerText = translate(language, "code"), br(),
                #   make_ace_editor("css_code", "", "css"),
                #   htmltools::br(),
                #   shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
                # )
              # )
            )
          ),
          make_card(
            translate(language, "my_account"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 30),
                make_textfield(language, ns, "old_password", "password", TRUE),
                make_textfield(language, ns, "new_password", "password", TRUE)
              ), htmltools::br(),
              shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
            ),
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
          make_card(
            translate(language, "create_user"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(language, ns, "username"),
                make_textfield(language, ns, "first_name"),
                make_textfield(language, ns, "last_name"),
                make_textfield(language, ns, "password", type = "password", canRevealPassword = TRUE),
                make_dropdown(language, ns, "user_access", list(
                  list(key = "admin", text = "Admin"),
                  list(key = "user", text = "User")
                  ), "user", "200px")
              ), br(),
              shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
            )          
          ),
          make_card(
            translate(language, "user_management"),
            div(
              shiny.fluent::DetailsList(
                compact = TRUE, checkboxVisibility = "hidden",
                items = list(
                  list(key = "1", username = "JDoe", first_name = "Johnnnnnnnnnnnnn", last_name = "Doe", user_access = "User"),
                  list(key = "2", username = "Jane", first_name = "Jane", last_name = "Doe", user_access = "Admin")
                ),
                columns = list(
                  list(key = "username", fieldName = "username", name = "Username", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                  list(key = "first_name", fieldName = "first_name", name = "First name", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                  list(key = "last_name", fieldName = "last_name", name = "Last name", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                  list(key = "user_access", fieldName = "user_access", name = "User", minWidth = 200, maxWidth = 200, isResizable = TRUE)
                )
              )
            )
          )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Data sources       #
      ##########################################
      
      if (page == "settings/data_sources"){
        div(class = "main",
          make_card(translate(language, "data_sources_create"),
            div(
              "..."
            )
          ),
          make_card(translate(language, "data_sources_management"),
            div(
              "..."
            )
          )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Datamarts          #
      ##########################################
      
      if (page == "settings/datamarts"){
        div(class = "main",
          make_card(translate(language, "datamarts_create"),
            div(
              "..."
            )
          ),
          make_card(translate(language, "datamarts_management"),
            div(
              "..."
            )
          ),
          make_card(translate(language, "datamarts_access"),
            div(
              "..."
            )
          )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Studies            #
      ##########################################
      
      if (page == "settings/studies"){
        div(class = "main",
          make_card(translate(language, "studies_create"),
            div(
              "..."
            )
          ),
          make_card(translate(language, "studies_management"),
            div(
              "..."
            )
          )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Subsets            #
      ##########################################
      
      if (page == "settings/subsets"){
        div(class = "main",
          make_card(translate(language, "subsets_create"),
            div(
              "..."
            )
          ),
          make_card(translate(language, "subsets_management"),
            div(
              "..."
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
