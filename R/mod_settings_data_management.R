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
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    
    ##########################################
    # Settings / Data sources                #
    ##########################################
    
    if (page == "settings/data_sources"){
      div(class = "main",
        data_management_creation_card(language, ns, "create_data_source",
                                      textfields = c(name = "name", description = "description"), textfields_width = "300px")
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      div(class = "main",
          data_management_creation_card(language, ns, "create_datamart",
                                        textfields = c(name = "name", description = "description"), textfields_width = "300px",
                                        dropdowns = c(data_source = "data_source"), dropdowns_width = "300px",
                                        data_sources = list(
                                          list(key = "ehop", text = "eHOP"),
                                          list(key = "mimic-iv", text = "MIMIC-IV")
                                        ))
      ) -> result
    }
    
    ##########################################
    # Settings / Studies                     #
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
            DT::DTOutput(ns("studies_datatable")),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput("management_save", translate(language, "save"))
            # make_dropdown(language, ns, "studies_management_choice", list(
            #   list(key = "study1", text = "Study 1 - first anti-Xa"),
            #   list(key = "study2", text = "Study 2 - all anti-Xa")
            # ), "study2", "300px"),
            # shiny.fluent::Stack(
            #   horizontal = TRUE,
            #   tokens = list(childrenGap = 50),
            #   make_dropdown(language, ns, "study_patient_lvl_data_module_family", list(
            #     list(key = "mimic", text = "MIMIC"),
            #     list(key = "ehop", text = "eHOP")
            #   ), value = "study2", width = "300px"),
            #   make_dropdown(language, ns, "study_aggregated_data_module_family", list(
            #     list(key = "mimic", text = "MIMIC"),
            #     list(key = "ehop", text = "eHOP")
            #   ), value = "study2", width = "300px")
            # )
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
              ), value = "study2", width = "300px"),
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
    
    ##########################################
    # Settings / Subsets                     #
    ##########################################
    
    if (page == "settings/subsets"){
      div(class = "main",
        data_management_creation_card(language, ns, "create_subset",
                                      textfields = c(name = "name"),
                                      dropdowns = c(datamart = "datamart", study = "study"),
                                      datamarts = list(
                                        list(key = "ufh", text = "Cohorte héparine"),
                                        list(key = "wmv", text = "Sevrage ventilation")
                                      ),
                                      studies = list(
                                        list(key = "study1", text = "Study 1"),
                                        list(key = "study2", text = "Study 2")
                                      ))
      ) -> result
    }
    
    ##########################################
    # Settings / Thesaurus                   #
    ##########################################
    
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
    
    data <- tibble::tribble(~Action, ~`Study ID`, ~`Study name`, ~Datamart, ~`Patient-level data module family`, ~`Aggregated data module family`,
                            "", 1, "Study 1", "Weaning from mechanical ventilation", "eHOP default", "Default 1",
                            "", 2, "Study 2", "Heparin datamart Metavision", "Metavision default", "Default 1",
                            "", 3, "Study 3", "My new study", "App default", "Default 2")
    for (i in 1:nrow(data)) {
      data[i, "Datamart"] <-
        as.character(
          div(shiny.fluent::Dropdown.shinyInput(paste0("datamart", i), options = list(
            list(key = "Weaning from mechanical ventilation", text = "Weaning from mechanical ventilation"),
            list(key = "My new study", text = "My new study"),
            list(key = "Heparin datamart Metavision", text = "Heparin datamart Metavision")
            ), value = as.character(data[i, "Datamart"]), style = "width:100%")
          )
        )
      data[i, "Patient-level data module family"] <-
        as.character(
          div(shiny.fluent::Dropdown.shinyInput(paste0("patient_lvl_module_family", i), options = list(
            list(key = "App default", text = "App default"),
            list(key = "Metavision default", text = "Metavision default"),
            list(key = "eHOP default", text = "eHOP default")
            ), value = as.character(data[i, "Patient-level data module family"]), style = "width:100%")
          )
        )
        # as.character(selectInput(paste0("patient_lvl_module_family", i), "", choices = c("App default", "Metavision default", "eHOP default"), 
        #                          selected = data[i, "Patient-level data module family"], width = "100%"))
      data[i, "Aggregated data module family"] <-
        as.character(
          div(shiny.fluent::Dropdown.shinyInput(paste0("aggregated_module_family", i), options = list(
            list(key = "Default 1", text = "Default 1"),
            list(key = "Default 2", text = "Default 2")
            ), value = as.character(data[i, "Aggregated data module family"]), style = "width:100%")
          )
        )
      data[i, "Action"] <-
        as.character(shiny::actionButton(paste0("delete", i), label = "a", icon = shiny::icon("glyphicon-trash", lib = "glyphicon"),
                                  onclick = "Shiny.setInputValue(\'deletePressed\', this.id, {priority: 'event'})"))
    }
    
    output$studies_datatable <- DT::renderDT(
      data,
      options = list(dom = "ft",
                     columnDefs = list(list(className = "dt-left", targets = "_all"))),
      rownames = FALSE, selection = "none", escape = FALSE, server = TRUE,
      editable = list(target = "cell", disable = list(columns = c(0, 2, 3, 4))),
      callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
    )
    
    # https://stackoverflow.com/questions/57215607/render-dropdown-for-single-column-in-dt-shiny
    # https://yihui.shinyapps.io/DT-edit/
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
