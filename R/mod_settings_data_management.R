#' settings_data_management UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_data_management_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    if (page == "settings/data_sources"){
      div(class = "main",
        data_management_creation_card(language, ns, "create_data_source", name = "data_source_name", description = "data_source_description")
      ) -> result
    }
    
    if (page == "settings/datamarts"){
      
    }
    
    if (page == "settings/studies"){
      div(class = "main",
        make_card(translate(language, "studies_create"),
          div(
            "..."
          )
        ),
        make_card(translate(language, "studies_management"),
          div(
            DT::DTOutput(ns("studies_datatable")),
            # make_dropdown(language, ns, "studies_management_choice", list(
            #   list(key = "study1", text = "Study 1 - first anti-Xa"),
            #   list(key = "study2", text = "Study 2 - all anti-Xa")
            # ), "study2", "300px"),
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 50),
              make_dropdown(language, ns, "study_patient_lvl_data_module_family", list(
                list(key = "mimic", text = "MIMIC"),
                list(key = "ehop", text = "eHOP")
              ), "study2", "300px"),
              make_dropdown(language, ns, "study_aggregated_data_module_family", list(
                list(key = "mimic", text = "MIMIC"),
                list(key = "ehop", text = "eHOP")
              ), "study2", "300px")
            )
          )
        ),
        make_card(translate(language, "studies_access"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 50),
              make_dropdown(language, ns, "studies_access_choice", list(
                list(key = "study1", text = "Study 1 - first anti-Xa"),
                list(key = "study2", text = "Study 2 - all anti-Xa")
              ), "study2", "300px"),
              div(
                make_persona_picker(language, ns, "datamart_access_people", options = tibble::tribble(
                  ~key, ~imageInitials, ~text, ~secondaryText,
                  1, "JD", "John Doe", "Intensivist",
                  2, "JD", "Jane Doe", "Data scientist"
                ), min_width = "300px", max_width = "500px")
              )
            ), htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
          )
        )
      ) -> result
    }
    
    if (page == "settings/subsets"){
      
    }
    
    if (page == "settings/thesaurus"){
      
    }
  }
  
  result
}
    
#' settings_studies Server Functions
#'
#' @noRd 
# mod_settings_data_management_server <- function(id, page_style, page){
mod_settings_data_management_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$studies_datatable <- DT::renderDT(
      # iris,
      tibble::tribble(~study_id, ~study_name,
                      1, "Study 1",
                      2, "Study 2"),
      options = list()
    )
    
    # output$test <- shiny::renderText("Mon texte")
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
