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
    if (page == "studies"){
      div(class = "main",
        make_card(translate(language, "studies_create"),
          div(
            "..."
          )
        ),
        make_card(translate(language, "studies_management"),
          div(
            make_dropdown(language, ns, "studies_management_choice", list(
              list(key = "study1", text = "Study 1 - first anti-Xa"),
              list(key = "study2", text = "Study 2 - all anti-Xa")
            ), "study2", "300px"),
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
  }
  result
}
    
#' settings_studies Server Functions
#'
#' @noRd 
mod_settings_data_management_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
