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
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_creation_card(language, ns, "create_data_source",
                                      textfields = c(name = "name", description = "description"), textfields_width = "300px"),
        data_management_datatable_card(language, ns, "data_sources_management")
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      div(class = "main",
          shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
          shiny.fluent::reactOutput(ns("management_delete_confirm")),
          data_management_toggle_cards(language, creation_card = "create_datamart", datatable_card = "datamarts_management", edit_card = "edit_datamart_code"),
          data_management_creation_card(language, ns, "create_datamart",
                                        textfields = c(name = "name", description = "description"), textfields_width = "300px",
                                        dropdowns = c(data_source = "data_source"), dropdowns_width = "300px",
                                        data_sources = list(list(key = "", text = ""))
                                        ),
          data_management_datatable_card(language, ns, "datamarts_management"),
          shiny::uiOutput(ns("edit_card")),
          shiny::textOutput(ns("test"))
      ) -> result
    }
    
    ##########################################
    # Settings / Studies                     #
    ##########################################
    
    if (page == "settings/studies"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
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
        data_management_datatable_card(language, ns, "studies_management"),
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
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_creation_card(language, ns, "create_subset",
                                      textfields = c(name = "name"), textfields_width = "300px",
                                      dropdowns = c(datamart = "datamart", study = "study"), dropdowns_width = "300px",
                                      datamarts = list(list(key = "", text = "")),
                                      studies = list(list(key = "", text = ""))),
        data_management_datatable_card(language, ns, "subsets_management")
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
                 options = tibble_to_list(r$data_sources_data, "Data source ID", "Data source name", rm_deleted_rows = TRUE)))
    observeEvent(r$datamarts_data, shiny.fluent::updateDropdown.shinyInput(session, "datamart", 
                 options = tibble_to_list(r$datamarts_data, "Datamart ID", "Datamart name", rm_deleted_rows = TRUE)))
    observeEvent(r$studies_data, shiny.fluent::updateDropdown.shinyInput(session, "study", 
                 options = tibble_to_list(r$studies_data, "Study ID", "Study name", rm_deleted_rows = TRUE)))
    
    # onClick add button
    observeEvent(input$add, {
      # SQL request
      # ...
      
      message <- switch(id, "settings_data_sources" = "data_source_added",
                            "settings_datamarts" = "datamart_added",
                            "settings_studies" = "study_added",
                            "settings_subsets" = "subset_added")
      output$warnings1 <- renderUI({
        div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 4), style = "margin-top:10px;")
      })
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))
    })
    
    ##########################################
    # Data management / Elements management  #
    ##########################################
    
      ##########################################
      # Generate datatable                     #
      ##########################################
    
      output$management_datatable <- DT::renderDT(
        data_management_datatable(id = id,
                                  data = data_management_data(id, r),
                                  r,
                                  dropdowns = switch(id,
                                                     "settings_data_sources" = "",
                                                     "settings_datamarts" = c("Data source ID" = "data_sources"),
                                                     "settings_studies" = c("Datamart ID" = "datamarts",
                                                                            "Patient-level data module family ID" = "patient_lvl_modules_families",
                                                                            "Aggregated data module family ID" = "aggregated_modules_families"),
                                                     "settings_subsets" = c("Study ID" = "studies")
                                  )),
        options = list(dom = "t",
                       columnDefs = list(
                         list(className = "dt-center", targets = c(0)),
                         list(sortable = FALSE, targets = data_management_datatable_options(data_management_data(id, r), id, "sortable")))
                      ),
        rownames = FALSE, selection = "none", escape = FALSE, server = TRUE,
        editable = list(target = "cell", disable = list(columns = data_management_datatable_options(data_management_data(id, r), id, "disable"))),
        callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
            var $this = $(this.node());
            $this.attr('id', this.data()[0]);
            $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
      )
    
      ##########################################
      # Save changes in datatable              #
      ##########################################
    
      observeEvent(input$management_save, {
        
        # SQL request to save modifications...
        # ...
        
        # Notification to user
        output$warnings2 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
        })
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      })
    
      ##########################################
      # Delete a row in datatable              #
      ##########################################
      
      data_management_delete_dialog <- reactiveVal(FALSE)
        
      output$management_delete_confirm <- shiny.fluent::renderReact(data_management_delete_react(id, ns, language, data_management_delete_dialog()))
        
      observeEvent(input$hideDialog, data_management_delete_dialog(FALSE))
      observeEvent(input$management_delete_canceled, data_management_delete_dialog(FALSE))
      observeEvent(input$deleted_pressed, data_management_delete_dialog(TRUE))
      
      observeEvent(input$management_delete_confirmed, {
          
        data_management_delete_dialog(FALSE)
        
        # Modify reactive value r$...
        data_var <- paste0(substr(id, nchar("settings_") + 1, nchar(id)), "_data")
        row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete") + 1, nchar(input$deleted_pressed)))
        r[[data_var]][which(r[[data_var]][1] == row_deleted), "Deleted"] = TRUE
        
        # Notification to user
        message <- switch(id, "settings_data_sources" = "data_source_deleted",
                          "settings_datamarts" = "datamart_deleted",
                          "settings_studies" = "study_deleted",
                          "settings_subsets" = "subset_deleted")
  
        output$warnings3 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
        })
        shinyjs::show("warnings3")
        shinyjs::delay(3000, shinyjs::hide("warnings3"))
      })
    
      # https://stackoverflow.com/questions/57215607/render-dropdown-for-single-column-in-dt-shiny
      # https://yihui.shinyapps.io/DT-edit/
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      output$edit_card <- renderUI({
        req(input$edit_code)
        code_id <- as.integer(substr(input$edit_code, nchar("edit_code") + 1, nchar(input$edit_code)))
        code <- r$code %>% dplyr::filter(`Category` == "datamart" & `Link ID` == code_id) %>% dplyr::pull(`Code`)
        data_management_edit_card(language, ns, type = "code", title = switch(id, "settings_datamarts" = "edit_datamart_code"), code)
      })
      
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
