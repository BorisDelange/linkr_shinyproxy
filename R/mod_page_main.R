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
  result <- ""
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    
    ##########################################
    # Fluent / Home                          #
    ##########################################
    
    if (grepl("^home", page)){
      div(class = "main",
        div(
          div(
            class = glue::glue("card ms-depth-8 ms-sm{8} ms-xl{8}"),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Text(variant = "large", "Datamarts", block = TRUE),
              # shiny.fluent::Text(paste0("id = ", id)),
              shiny.fluent::Text(shiny::textOutput(ns("test2")))
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
                      shiny.fluent::Stack(
                        horizontal = FALSE,
                        div(
                          div(class = "input_title", translate(language, "page_type")),
                          shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
                            list(key = "fluent", text = "Fluent UI"),
                            list(key = "fluid", text = "Fluid UI")
                          )),
                          style = "min-width: 180px;"
                        ), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
                        div(shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save")), style = "width: 100px")
                      ),
                      div(
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
                      ),
                      div(shinyAce::aceEditor("css_code", ".title {\n  padding: 5px 10px 0px 10px;\n  color: #737373;\n}", "css", 
                                              height = "200px"), style = "width: 100%;")
                    )
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
        # Hidden aceEditor, allows the other to be displayed...
        div(shinyAce::aceEditor("test"), style = "display: none;")
        div(class = "main",
          make_card(
            translate(language, "app_db"),
            div(
              div(
                div(class = "input_title", translate(language, "db_connexion_type")),
                  shiny.fluent::ChoiceGroup.shinyInput("db_connexion_type", value = "local", options = list(
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
                  make_textfield(language, ns, "password", type = "password", canRevealPassword = TRUE)
                )
              ), br(),
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 20),
                shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save")),
                shiny.fluent::PrimaryButton.shinyInput("test_connection", translate(language, "test_connection"))
              )
            )
          ),
          make_card(
            translate(language, "app_db_tables"),
            shiny.fluent::DetailsList(
              compact = TRUE,
              items = list(
                list(key = "1", table_name = "Users", nrows = 51),
                list(key = "2", table_name = "Datamarts", nrows = 13)
              ),
              columns = list(
                list(key = "table_name", fieldName = "table_name", name = "Table name", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                list(key = "nrows", fieldName = "nrows", name = "Num of rows", minWidth = 200, maxWidth = 200, isResizable = TRUE)
              )
            )
          ),
          make_card(
            translate(language, "app_db_request"),
            div(
              div(shinyAce::aceEditor("app_db_request", "SELECT * FROM my_table", "sql", height = "200px"), style = "width: 50%;"),
              shiny.fluent::PrimaryButton.shinyInput("request", translate(language, "request"))
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
                  ), value = "user", width = "200px"),
                make_dropdown(language, ns, "user_status", list(
                  list(key = "intensivist", text = "Clinician"),
                  list(key = "data_scientist", text = "Data scientist")
                ), value = "user", width = "200px")
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
        mod_settings_data_management_ui("settings_data_sources", language, page_style, page) -> result
      }
      
      ##########################################
      # Fluent / Settings / Datamarts          #
      ##########################################
      
      if (page == "settings/datamarts"){
        mod_settings_data_management_ui("settings_datamarts", language, page_style, page) -> result
        # div(class = "main",
        #   make_card(translate(language, "datamarts_create"),
        #     div(
        #       "..."
        #     )
        #   ),
        #   make_card(translate(language, "datamarts_management"),
        #     div(
        #       "..."
        #     )
        #   ),
        #   make_card(translate(language, "datamarts_access"),
        #     div(
        #       shiny.fluent::Stack(
        #         horizontal = TRUE,
        #         tokens = list(childrenGap = 50),
        #         make_dropdown(language, ns, "datamart_access_choice", list(
        #           list(key = "ufh", text = "Cohorte héparine"),
        #           list(key = "wmv", text = "Sevrage ventilation")
        #         ), value = "ufh", width = "300px"),
        #         make_persona_picker(language, ns, "datamart_access_people", options = tibble::tribble(
        #           ~key, ~imageInitials, ~text, ~secondaryText,
        #           1, "JD", "John Doe", "Intensivist",
        #           2, "JD", "Jane Doe", "Data scientist"
        #         ), value = c(1), min_width = "300px", max_width = "500px")
        #       ), htmltools::br(),
        #       shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
        #     )
        #   )
        # ) -> result
      }
      
      ##########################################
      # Fluent / Settings / Studies            #
      ##########################################
      
      if (page == "settings/studies"){
        mod_settings_data_management_ui("settings_studies", language, page_style, page) -> result
      }
      
      ##########################################
      # Fluent / Settings / Subsets            #
      ##########################################
      
      if (page == "settings/subsets"){
        mod_settings_data_management_ui("settings_subsets", language, page_style, page) -> result
      }
      
      ###########################################
      # Fluent / Settings / Plugins             #
      ###########################################
      
      if (page == "settings/plugins"){
        div(class = "main",
          make_card(translate(language, "plugins_create"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(language, ns, "plugin_name", value = "", min_width = "300px", max_width = "500px"),
                make_dropdown(language, ns, "plugin_page", options = list(
                  list(key = "none", text = "None"),
                  list(key = "patient_lvl_data", text = "Patient-level data"),
                  list(key = "aggregated_data", text = "Aggregated data")
                ), value = "none", min_width = "300px", max_width = "500px")
              ), htmltools::br(),
              shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
            )
          ),
          make_card(translate(language, "plugins_management"),
            div(
              "..."
            )
          )
        ) -> result
      }
      
      ###########################################
      # Fluent / Settings / Modules patient-lvl #
      ###########################################
      
      if (page == "settings/modules_patient_lvl"){
        div(class = "main",
          make_card(translate(language, "modules_create"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(language, ns, "module_name", value = "", min_width = "300px", max_width = "500px"),
                make_dropdown(language, ns, "module_parent", options = list(
                  list(key = "none", text = "None"),
                  list(key = "stays", text = "Stays"),
                  list(key = "notes", text = "Long text here for test - clinical notes")
                ), value = "none", min_width = "300px", max_width = "500px"),
                make_dropdown(language, ns, "module_family", options = list(
                  list(key = "metavision_default", text = "Metavision default"),
                  list(key = "metavision_perso1", text = "Metavision perso 1")
                ), value = "metavision_default", min_width = "300px", max_width = "500px")
              ),
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_dropdown(language, ns, "module_access_type", options = list(
                  list(key = "everybody", text = "To everybody"),
                  list(key = "by_user_status", text = "By user status"),
                  list(key = "by_user_access", text = "By user access"),
                  list(key = "by_persona_picker", text = "Select users")
                ), value = "everybody", min_width = "300px", max_width = "500px"),
                shiny::conditionalPanel(
                  condition = "input.module_access_type == 'by_persona_picker'", ns = ns,
                  make_persona_picker(language, ns, "module_access_people", options = tibble::tribble(
                    ~key, ~imageInitials, ~text, ~secondaryText,
                    1, "JD", "John Doe", "Intensivist",
                    2, "JD", "Jane Doe", "Data scientist"
                  ), value = c(1), min_width = "300px", max_width = "500px")
                ),
                shiny::conditionalPanel(
                  condition = "input.module_access_type == 'by_user_status'", ns = ns,
                  make_dropdown(language, ns, "user_status", list(
                    list(key = "intensivist", text = "Clinician"),
                    list(key = "data_scientist", text = "Data scientist")
                  ), value = "user", min_width = "300px", max_width = "500px")
                )
              ), htmltools::br(),
              shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
            )
          ),
          make_card(translate(language, "modules_management"),
            div(br(),
              "DataTable with : name of module, parent module, module family, creator, who has access", br(),
              "Or a dropdown of modules", br(),
              "Or both with pivot", br(),
              "On selection : modify a module", br(),
              "For the access : dropdown with selection mode : everybody ? by user status ? by user access ? by persona picker ?"
            )
          )
        ) -> result
      }
      
      ###########################################
      # Fluent / Settings / Modules aggregated  #
      ###########################################
      
      if (page == "settings/modules_aggregated"){
        div(class = "main",
          make_card(translate(language, "modules_create"),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE,
                tokens = list(childrenGap = 50),
                make_textfield(language, ns, "module_name", width = "300px"),
                make_dropdown(language, ns, "module_parent", options = list(
                  list(key = "none", text = "None"),
                  list(key = "inclusions", text = "Inclusions"),
                  list(key = "analysis", text = "Analysis")
                ), value = "none", width = "300px")
              ), htmltools::br(),
              shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
            )
          ),
          make_card(translate(language, "modules_management"),
            div(
              "..."
            )
          )
        ) -> result
      }
      
      ###########################################
      # Fluent / Settings / Log                 #
      ###########################################
      
      if (page == "settings/log"){
        div(class = "main",
          make_card(translate(language, "log_filters"),
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 50),
              make_dropdown(language, ns, "log_filter_type", options = list(
                list(key = "everybody", text = "To everybody"),
                list(key = "myself", text = "My log only"),
                list(key = "by_persona_picker", text = "Select users")
              ), value = "everybody", min_width = "300px", max_width = "500px"),
              shiny::conditionalPanel(
                condition = "input.log_filter_type == 'by_persona_picker'", ns = ns,
                make_persona_picker(language, ns, "log_filter_people", options = tibble::tribble(
                  ~key, ~imageInitials, ~text, ~secondaryText,
                  1, "JD", "John Doe", "Intensivist",
                  2, "JD", "Jane Doe", "Data scientist"
                ), value = c(1), min_width = "300px", max_width = "500px")
              )
            )         
          ),
          make_card(translate(language, "log_details"),
            shiny.fluent::DetailsList(
              compact = TRUE, checkboxVisibility = "hidden",
              items = list(
                list(key = "1", username = "JDoe", datetime = "2020-09-01 15:32:20", activity = "Include patient", value = "Study 1 - Patient 4653"),
                list(key = "2", username = "Jane", datetime = "2020-08-23 10:00:37", activity = "Modify a report", value = "Study 2 - Introduction")
              ),
              columns = list(
                list(key = "username", fieldName = "username", name = "Username", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                list(key = "datetime", fieldName = "datetime", name = "Datetime", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                list(key = "activity", fieldName = "activity", name = "Activity", minWidth = 200, maxWidth = 200, isResizable = TRUE),
                list(key = "value", fieldName = "value", name = "Value", minWidth = 200, maxWidth = 200, isResizable = TRUE)
              )
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
    output$test <- shiny::renderText("Mon texte")
  })
}
    
## To be copied in the UI
# mod_page_main_ui("page_main_ui_1")
    
## To be copied in the server
# mod_page_main_server("page_main_ui_1")
