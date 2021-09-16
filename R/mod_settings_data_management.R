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
        shiny::uiOutput(ns("warnings")),
        data_management_creation_card(language, ns, "create_data_source",
                                      textfields = c(name = "name", description = "description"), textfields_width = "300px")
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      div(class = "main",
          shiny::uiOutput(ns("warnings")),
          data_management_creation_card(language, ns, "create_datamart",
                                        textfields = c(name = "name", description = "description"), textfields_width = "300px",
                                        dropdowns = c(data_source = "data_source"), dropdowns_width = "300px",
                                        data_sources = list(list(key = "", text = ""))
                                        ),
          data_management_management_card(language, ns, "datamarts_management"),
      ) -> result
    }
    
    ##########################################
    # Settings / Studies                     #
    ##########################################
    
    if (page == "settings/studies"){
      div(class = "main",
        shiny::uiOutput(ns("warnings")),
        # shiny.fluent::reactOutput("management_delete_confirm"),
        data_management_creation_card(language, ns, "create_study",
                                      textfields = c(name = "name", description = "description"), textfields_width = "300px",
                                      dropdowns = c(datamart = "datamart", patient_lvl_module_family = "patient_lvl_module_family",
                                                    aggregated_module_family = "aggregated_module_family"), 
                                      dropdowns_width = "300px",
                                      datamarts = list(list(key = "", text = "")),
                                      patient_lvl_module_families = list(
                                      list(key = "eHOP default", text = "eHOP default")
                                      ),
                                      aggregated_module_families = list(
                                        list(key = "Default 1", text = "Default 1")
                                      )),
        data_management_management_card(language, ns, "studies_management"),
        tableOutput(ns("test")),
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
        shiny::uiOutput(ns("warnings")),
        data_management_creation_card(language, ns, "create_subset",
                                      textfields = c(name = "name"), textfields_width = "300px",
                                      dropdowns = c(datamart = "datamart", study = "study"), dropdowns_width = "300px",
                                      datamarts = list(list(key = "", text = "")),
                                      studies = list(list(key = "", text = "")))
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
mod_settings_data_management_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    observeEvent(r$data_sources_data, shiny.fluent::updateDropdown.shinyInput(session, "data_source", 
                 options = tibble_to_list(r$data_sources_data, "Data source ID", "Data source name")))
    observeEvent(r$datamarts_data, shiny.fluent::updateDropdown.shinyInput(session, "datamart", 
                 options = tibble_to_list(r$datamarts_data, "Datamart ID", "Datamart name")))
    observeEvent(r$studies_data, shiny.fluent::updateDropdown.shinyInput(session, "study", 
                 options = tibble_to_list(r$studies_data, "Study ID", "Study name")))
    
    # onClick add button
    observeEvent(input$add, {
      message <- switch(id, "settings_data_sources" = "data_source_added",
                            "settings_datamarts" = "datamart_added",
                            "settings_studies" = "study_added",
                            "settings_subsets" = "subset_added")
      output$warnings <- renderUI({
        div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 4), style = "margin-top:10px;")
      })
      shinyjs::show("warnings")
      shinyjs::delay(5000, shinyjs::hide("warnings"))
    })
    
    # Add in database
    # ...
    
    ##########################################
    # Data management / Elements management  #
    ##########################################
    
    # data_management_elements_management(data = r[[paste0(substr(id, 10, 50), "_data")]])
    # 
    # r$data <- tibble::tribble(~Action, ~`Study ID`, ~`Study name`, ~Datamart, ~`Patient-level data module family`, ~`Aggregated data module family`,
    #                         "", 1, "Study 1", "Weaning from mechanical ventilation", "eHOP default", "Default 1",
    #                         "", 2, "Study 2", "Heparin datamart Metavision", "Metavision default", "Default 1",
    #                         "", 3, "Study 3", "My new study", "App default", "Default 2")
    
    # observeEvent(input$management_edit, {
      # output$test <- renderTable(r$studies_data)
    # })
    
    output$management_datatable <- DT::renderDT(
      #r[[paste0(substr(id, 10, 50), "_data")]]
      data_management_elements_management(tibble::tribble(~Action, ~`Study ID`, ~`Study name`, ~Datamart, ~`Patient-level data module family`, ~`Aggregated data module family`,
                                                          "", 1, "Study 1", "Weaning from mechanical ventilation", "eHOP default", "Default 1",
                                                          "", 2, "Study 2", "Heparin datamart Metavision", "Metavision default", "Default 1",
                                                          "", 3, "Study 3", "My new study", "App default", "Default 2")),
      options = list(dom = "t",
                     columnDefs = list(
                       list(className = "dt-center", targets = c(0:2)),
                       list(sortable = FALSE, targets = c(0, 3, 4, 5)))),
      rownames = FALSE, selection = "none", escape = FALSE, server = TRUE,
      editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
      callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-container');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
    )
    
    # observeEvent(input$management_edit, {
    #   for (i in 1:nrow(r$data)) {
    #     r$data[i, "Datamart"] <-
    #       as.character(
    #         div(shiny.fluent::Dropdown.shinyInput(paste0("datamart", i), options = list(
    #           list(key = "Weaning from mechanical ventilation", text = "Weaning from mechanical ventilation"),
    #           list(key = "My new study", text = "My new study"),
    #           list(key = "Heparin datamart Metavision", text = "Heparin datamart Metavision")
    #           ), value = as.character(r$data[i, "Datamart"]), style = "width:100%")
    #         )
    #       )
    #     r$data[i, "Patient-level data module family"] <-
    #       as.character(
    #         div(shiny.fluent::Dropdown.shinyInput(paste0("patient_lvl_module_family", i), options = list(
    #           list(key = "App default", text = "App default"),
    #           list(key = "Metavision default", text = "Metavision default"),
    #           list(key = "eHOP default", text = "eHOP default")
    #           ), value = as.character(r$data[i, "Patient-level data module family"]), style = "width:100%")
    #         )
    #       )
    #       # as.character(selectInput(paste0("patient_lvl_module_family", i), "", choices = c("App default", "Metavision default", "eHOP default"),
    #       #                          selected = data[i, "Patient-level data module family"], width = "100%"))
    #     r$data[i, "Aggregated data module family"] <-
    #       as.character(
    #         div(shiny.fluent::Dropdown.shinyInput(paste0("aggregated_module_family", i), options = list(
    #           list(key = "Default 1", text = "Default 1"),
    #           list(key = "Default 2", text = "Default 2")
    #           ), value = as.character(r$data[i, "Aggregated data module family"]), style = "width:100%")
    #         )
    #       )
    #     r$data[i, "Action"] <-
    #       as.character(shiny::actionButton(paste0("delete", i), "X", style = "color:red",
    #                           onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
    #       # as.character(shiny.fluent::IconButton.shinyInput(paste0("delete", i), "X", shiny::icon("glyphicon-trash", lib = "glyphicon"),
    #       #                           style = "color:red",
    #       #                           onClick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
    #   }
    # })

    # observeEvent(r$data, {
    #   output$management_datatable <- DT::renderDT(
    #     r$data,
    #     options = list(dom = "t",
    #                    columnDefs = list(
    #                      list(className = "dt-center", targets = c(0:2)),
    #                      # list(className = "dt-center", targets =),
    #                      list(sortable = FALSE, targets = c(0, 3, 4, 5)))),
    #     rownames = FALSE, selection = "none", escape = FALSE, server = TRUE,
    #     editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
    #     callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
    #       var $this = $(this.node());
    #       $this.attr('id', this.data()[0]);
    #       $this.addClass('shiny-input-container');
    #     });
    #     Shiny.unbindAll(table.table().node());
    #     Shiny.bindAll(table.table().node());")
    #   )
    # })
    
    # observeEvent(input$deleted_pressed, {
    #   message <- switch(id, "settings_data_sources" = "data_source_deleted",
    #                     "settings_datamarts" = "datamart_deleted",
    #                     "settings_studies" = "study_deleted",
    #                     "settings_subsets" = "subset_deleted")
    #   
    #   output$warnings <- renderUI({
    #     div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
    #   })
    #   shinyjs::show("warnings")
    #   shinyjs::delay(5000, shinyjs::hide("warnings"))
    #   
    #   r$data <- r$data[-as.integer(substr(input$deleted_pressed, 7, 15)), ]
    #   for (i in 1:nrow(r$data)) {
    #     r$data[i, "Action"] <-
    #       as.character(shiny::actionButton(paste0("delete", i), "X", style = "color:red",
    #                                        onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
    #   }
    # })
    
    # https://stackoverflow.com/questions/57215607/render-dropdown-for-single-column-in-dt-shiny
    # https://yihui.shinyapps.io/DT-edit/
    
    # observeEvent(input$management_save, {
    #   output$warnings <- renderUI({
    #     div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
    #   })
    #   shinyjs::show("warnings")
    #   shinyjs::delay(5000, shinyjs::hide("warnings"))
    # })
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
