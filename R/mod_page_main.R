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
      mod_patient_and_aggregated_data_ui("patient_lvl_data", language, page_style, page) -> result
      # div(class = "main",
      #   shiny.fluent::Pivot(
      #     shiny.fluent::PivotItem(headerText = "Stays",
      #       div(
      #         make_card(
      #           "Stays timeline",
      #           "My timeline"
      #         )
      #       )
      #     ),
      #     shiny.fluent::PivotItem(headerText = "Notes",
      #       div(
      #         make_card(
      #           "Clinical notes",
      #           "Notes"
      #         )
      #       )
      #     )
      #   )
      # ) -> result
    }
    
    ##########################################
    # Fluent / Aggregated data               #
    ##########################################
    
    if (page == "aggregated_data"){
      mod_patient_and_aggregated_data_ui("aggregated_data", language, page_style, page) -> result
      # div(class = "main",
      #   shiny.fluent::Breadcrumb(
      #     items = list(
      #       list(text = "Data cleaning", key = "data_cleaning", href = ""),
      #       list(text = "Outliers", key = "outliers", href = "")
      #     ),
      #     maxDisplayedItems = 3
      #   ),
      #   shiny.fluent::Pivot(
      #     shiny.fluent::PivotItem(headerText = "Outliers",
      #       div(
      #         make_card(
      #           "Outliers",
      #           div(htmltools::br(),
      #           "Choose of the outliers")
      #         )
      #       )
      #     ),
      #     shiny.fluent::PivotItem(headerText = "Missing data",
      #       div(
      #         make_card(
      #           "Missing data",
      #           div(htmltools::br(),
      #           "Showing missing data")
      #         )
      #       )
      #     )
      #   )
      # ) -> result
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
          div(shinyAce::aceEditor("hidden"), style = "display: none;")
          # make_card(
          #   translate(language, "general"),
          #   div(
          #     shiny.fluent::Stack(
          #       horizontal = TRUE,
          #       tokens = list(childrenGap = 30),
          #       div(
          #         div(class = "input_title", translate(language, "language")),
          #         div(shiny.fluent::Dropdown.shinyInput("language", value = "EN", options = list(
          #           list(key = "EN", text = "English"),
          #           list(key = "FR", text = "Français"))),
          #           style = "width:200px"
          #         )
          #       ),
          #       div(
          #         div(class = "input_title", translate(language, "dev_mode")),
          #         div(shiny.fluent::Toggle.shinyInput("dev_mode", value = TRUE))
          #       )
          #     ),
          #     htmltools::br(),
          #     htmltools::div(
          #       shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save")),
          #     )
          #   )
          # ),
          # make_card(
          #   translate(language, "appearance"),
          #   div(
          #     # shiny.fluent::Pivot(
          #       # shiny.fluent::PivotItem(headerText = translate(language, "choices"),
          #         div(
          #           shiny.fluent::Stack(
          #             horizontal = TRUE,
          #             tokens = list(childrenGap = 50),
          #             shiny.fluent::Stack(
          #               horizontal = FALSE,
          #               div(
          #                 div(class = "input_title", translate(language, "page_type")),
          #                 shiny.fluent::ChoiceGroup.shinyInput("page_type", value = "fluent", options = list(
          #                   list(key = "fluent", text = "Fluent UI"),
          #                   list(key = "fluid", text = "Fluid UI")
          #                 )),
          #                 style = "min-width: 180px;"
          #               ), htmltools::br(), htmltools::br(), htmltools::br(), htmltools::br(),
          #               div(shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save")), style = "width: 100px")
          #             ),
          #             div(
          #               div(class = "input_title", translate(language, "page_theme")),
          #               div(
          #                 shiny.fluent::Dropdown.shinyInput("page_theme",  translate(language, "page_theme"),
          #                   value = "darker", options = list(
          #                   list(key = "darker", text = "Darker"),
          #                   list(key = "light", text = "Light"),
          #                   list(key = "neutralQuaternary", text = "neutralQuaternary")
          #                 )),
          #                 style = "min-width: 200px;"
          #               )
          #             ),
          #             div(shinyAce::aceEditor("css_code", ".title {\n  padding: 5px 10px 0px 10px;\n  color: #737373;\n}", "css",
          #                                     height = "200px"), style = "width: 100%;")
          #           )
          #         )
                # ),
                # shiny.fluent::PivotItem(headerText = translate(language, "code"), br(),
                #   make_ace_editor("css_code", "", "css"),
                #   htmltools::br(),
                #   shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
                # )
              # )
          #   )
          # ),
          # make_card(
          #   translate(language, "my_account"),
          #   div(
          #     shiny.fluent::Stack(
          #       horizontal = TRUE,
          #       tokens = list(childrenGap = 30),
          #       make_textfield(language, ns, "old_password", type = "password", canRevealPassword = TRUE),
          #       make_textfield(language, ns, "new_password", type = "password", canRevealPassword = TRUE)
          #     ), htmltools::br(),
          #     shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
          #   ),
          # )
        ) -> result
      }
      
      ##########################################
      # Fluent / Settings / App database       #
      ##########################################
      
      if (page == "settings/app_db"){
        mod_settings_app_database_ui("settings_app_database", language, page_style, page) -> result
      }
      
      ##########################################
      # Fluent / Settings / Users              #
      ##########################################
      
      if (page == "settings/users"){
        mod_settings_users_ui("settings_users", language, page_style, page) -> result
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
      
      ##########################################
      # Fluent / Settings / Thesaurus          #
      ##########################################
      
      if (page == "settings/thesaurus"){
        mod_settings_data_management_ui("settings_thesaurus", language, page_style, page) -> result
      }
      
      ###########################################
      # Fluent / Settings / Plugins             #
      ###########################################
      
      if (page == "settings/plugins"){
        mod_settings_plugins_ui("settings_plugins", language, page_style, page) -> result
      }
      
      ###########################################
      # Fluent / Settings / Modules patient-lvl #
      ###########################################
      
      if (page == "settings/modules_patient_lvl"){
        mod_settings_modules_ui("settings_patient_lvl_modules", language, page_style, page) -> result
      }
      
      ###########################################
      # Fluent / Settings / Modules aggregated  #
      ###########################################
      
      if (page == "settings/modules_aggregated"){
        mod_settings_modules_ui("settings_aggregated_modules", language, page_style, page) -> result
      }
      
      ###########################################
      # Fluent / Settings / Log                 #
      ###########################################
      
      if (page == "settings/log"){
        # div(class = "main",
        #   make_card(translate(language, "log_filters"),
        #     shiny.fluent::Stack(
        #       horizontal = TRUE,
        #       tokens = list(childrenGap = 50),
        #       make_dropdown(language, ns, "log_filter_type", options = list(
        #         list(key = "everybody", text = "To everybody"),
        #         list(key = "myself", text = "My log only"),
        #         list(key = "by_people_picker", text = "Select users")
        #       ), value = "everybody", min_width = "300px", max_width = "500px"),
        #       shiny::conditionalPanel(
        #         condition = "input.log_filter_type == 'by_people_picker'", ns = ns,
        #         make_people_picker(language, ns, "log_filter_people", options = tibble::tribble(
        #           ~key, ~imageInitials, ~text, ~secondaryText,
        #           1, "JD", "John Doe", "Intensivist",
        #           2, "JD", "Jane Doe", "Data scientist"
        #         ), value = tibble::tribble(
        #           ~key, ~imageInitials, ~text, ~secondaryText,
        #           1, "JD", "John Doe", "Intensivist") , min_width = "300px", max_width = "500px")
        #       )
        #     )         
        #   ),
        #   make_card(translate(language, "log_details"),
        #     shiny.fluent::DetailsList(
        #       compact = TRUE, checkboxVisibility = "hidden",
        #       items = list(
        #         list(key = "1", username = "JDoe", datetime = "2020-09-01 15:32:20", activity = "Include patient", value = "Study 1 - Patient 4653"),
        #         list(key = "2", username = "Jane", datetime = "2020-08-23 10:00:37", activity = "Modify a report", value = "Study 2 - Introduction")
        #       ),
        #       columns = list(
        #         list(key = "username", fieldName = "username", name = "Username", minWidth = 200, maxWidth = 200, isResizable = TRUE),
        #         list(key = "datetime", fieldName = "datetime", name = "Datetime", minWidth = 200, maxWidth = 200, isResizable = TRUE),
        #         list(key = "activity", fieldName = "activity", name = "Activity", minWidth = 200, maxWidth = 200, isResizable = TRUE),
        #         list(key = "value", fieldName = "value", name = "Value", minWidth = 200, maxWidth = 200, isResizable = TRUE)
        #       )
        #     )        
        #   )
        # ) -> result
      }
    }
    
    ##########################################
    # Fluent / Help                          #
    ##########################################
    
    if (grepl("^help", page)){
      # mod_help_ui("help", language, page_style, page) -> result
      mod_help_ui(stringr::str_replace(page, "/", "_"), language, page_style, page) -> result
      # html_code <- browseURL(paste0(golem::get_golem_wd(), "/inst/app/www/help_pages/EN_dev_data_management.html"))
      # tags$iframe(src = paste0(golem::get_golem_wd(), "/inst/app/www/help_pages/EN_dev_data_management.html"),
      #             height = "100%", width = "100%") -> result
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
