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
        data_management_toggle_cards(language, ns, creation_card = "create_data_source", datatable_card = "data_sources_management", activated = c("datatable_card")),
        div(id = ns("creation_card"), 
            data_management_creation_card(language, ns, "create_data_source",  textfields = c(name = "name", description = "description"), textfields_width = "300px")),
        div(id = ns("datatable_card"), data_management_datatable_card(language, ns, "data_sources_management"))
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      div(class = "main",
          shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")), shiny::uiOutput(ns("warnings4")),
          shiny.fluent::reactOutput(ns("management_delete_confirm")),
          data_management_toggle_cards(language, ns, creation_card = "create_datamart", datatable_card = "datamarts_management", 
                                       edit_card = "edit_datamart_code", options_card = "datamart_options", activated = c("datatable_card")),
            data_management_creation_card(language, ns, "create_datamart",
              textfields = c(name = "name", description = "description"), textfields_width = "300px",
              dropdowns = c(data_source = "data_source"), dropdowns_width = "300px",
              data_sources = list(list(key = "", text = ""))),
          data_management_datatable_card(language, ns, "datamarts_management"),
          shiny::uiOutput(ns("edit_card")),
          shiny::uiOutput(ns("options_card"))
      ) -> result
    }
    
    ##########################################
    # Settings / Studies                     #
    ##########################################
    
    if (page == "settings/studies"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_cards(language, ns, creation_card = "create_study", datatable_card = "studies_management", options_card = "study_options",
                                     activated = c("datatable_card")),
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
        shiny::uiOutput(ns("edit_card")),
        shiny::uiOutput(ns("options_card"))
        # div(id = ns("options_card"),
        #   make_card(translate(language, "study_options"),
        #     div(
        #       make_persona_picker(language, ns, "studies_access_people", options = tibble::tribble(
        #         ~key, ~imageInitials, ~text, ~secondaryText,
        #         1, "JD", "John Doe", "Intensivist",
        #         2, "JD", "Jane Doe", "Data scientist"
        #       ), width = "100%"),
        #     htmltools::br(),
        #     shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
        #     )
        #   )
        # )
      ) -> result
    }
    
    ##########################################
    # Settings / Subsets                     #
    ##########################################
    
    if (page == "settings/subsets"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_cards(language, ns, creation_card = "create_subset", datatable_card = "subsets_management", activated = c("datatable_card")),
        data_management_creation_card(language, ns, "create_subset",
                                      textfields = c(name = "name", description = "description"), textfields_width = "300px",
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
    
#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#' settings_studies Server Functions
#'
#' @noRd 
# mod_settings_data_management_server <- function(id, page_style, page){
mod_settings_data_management_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    observeEvent(input$creation_card_toggle, if (input$creation_card_toggle) shinyjs::show("creation_card") else shinyjs::hide("creation_card"))
    observeEvent(input$datatable_card_toggle, if (input$datatable_card_toggle) shinyjs::show("datatable_card") else shinyjs::hide("datatable_card"))
    observeEvent(input$edit_card_toggle, if (input$edit_card_toggle) shinyjs::show("edit_card") else shinyjs::hide("edit_card"))
    observeEvent(input$options_card_toggle, if (input$options_card_toggle) shinyjs::show("options_card") else shinyjs::hide("options_card"))
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    observeEvent(r$data_sources_data, {
      options <- tibble_to_list(r$data_sources_data, "Data source ID", "Data source name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "data_source", options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    observeEvent(r$datamarts_data, {
      options <- tibble_to_list(r$datamarts_data, "Datamart ID", "Datamart name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    observeEvent(r$studies_data, {
      options <- tibble_to_list(r$studies_data, "Study ID", "Study name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "study", options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    observeEvent(r$patient_lvl_modules_families, {
      options <- tibble_to_list(r$patient_lvl_modules_families, "Module family ID", "Module family name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "patient_lvl_module_family", options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    observeEvent(r$aggregated_modules_families, {
      options <- tibble_to_list(r$aggregated_modules_families, "Module family ID", "Module family name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "aggregated_module_family", options = options, value = options[[1]][["key"]])
    })
    
    # onClick add button
    observeEvent(input$add, {
      
      # Check if required fields are filled
      name_check <- ifelse(is.null(input$name), FALSE, TRUE)
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)
      # description_check <- ifelse(is.null(input$description), FALSE, TRUE)
      # if (!description_check) shiny.fluent::updateTextField.shinyInput(session, "description", errorMessage = translate(language, "provide_valid_description"))
      # if (description_check) shiny.fluent::updateTextField.shinyInput(session, "description", errorMessage = NULL)
      
      req(name_check)#, description_check)
      
      # SQL request
      # ...
      # Check if already exists
      # Add to database
      
      data_var <- paste0(substr(id, nchar("settings_") + 1, nchar(id)), "_data")
      prefix_cols <- switch(id, "settings_data_sources" = "Data source", "settings_datamarts" = "Datamart", "settings_studies" = "Study", "settings_subsets" = "Subset")
      distinct_names <- r[[data_var]] %>% dplyr::filter(!Deleted) %>% dplyr::pull(paste0(prefix_cols, " name"))
      
      if (input$name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(input$name %not_in% (r[[data_var]] %>% dplyr::filter(!Deleted) %>% dplyr::pull(paste0(prefix_cols, " name"))))
      
      last_row <- max(r[[data_var]][paste0(prefix_cols, " ID")])
    
      if (id == "settings_data_sources") tibble::tribble(~`Data source ID`, ~`Data source name`, ~`Data source description`, ~`Creator`, ~`Date & time`, ~`Deleted`,
            last_row + 1, input$name, ifelse(is.null(input$description), "", input$description), r$user, as.character(Sys.time()), FALSE) -> new_data
      if (id == "settings_datamarts") tibble::tribble(~`Datamart ID`, ~`Datamart name`, ~`Datamart description`, ~`Data source ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
            last_row + 1, input$name, ifelse(is.null(input$description), "", input$description), input$data_source, r$user, as.character(Sys.time()), FALSE) -> new_data
      if (id == "settings_studies") tibble::tribble(~`Study ID`, ~`Study name`, ~`Study description`, ~`Datamart ID`, ~`Patient-level data module family ID`, 
            ~`Aggregated data module family ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
            last_row + 1, input$name, ifelse(is.null(input$description), "", input$description), input$datamart, input$patient_lvl_module_family,
            input$aggregated_module_family, r$user, as.character(Sys.time()), FALSE) -> new_data
      if (id == "settings_subsets") tibble::tribble(~`Subset ID`, ~`Subset name`, ~`Subset description`, ~`Study ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
            last_row + 1, input$name, ifelse(is.null(input$description), "", input$description), input$study, r$user, as.character(Sys.time()), FALSE) -> new_data
      
      r[[data_var]] <- r[[data_var]] %>% dplyr::bind_rows(new_data)
      
      # If the row we add is a datamart, add a row in the code table also
      last_row_code <- max(r$code["Code ID"])
      if (id == "settings_datamarts") r$code <- r$code %>% dplyr::bind_rowd(
        tibble::tribble(~`Code ID`, ~`Category`, ~`Link ID`, ~`Code`, ~`Creator`, ~`Date & time`, ~`Deleted`,
                        last_row_code + 1, "datamart", last_row + 1, "", r$user, as.character(Sys.time()), FALSE))
      
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
        options = list(dom = "t<'bottom'p>",
                       columnDefs = list(
                         list(className = "dt-center", targets = c(0, -1, -2, -3)),
                         list(width = "80px", targets = -1), list(width = "130px", targets = -2),
                         list(sortable = FALSE, targets = data_management_datatable_options(data_management_data(id, r), id, "sortable")))
                      ),
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,
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
      # Edit options by selecting a row        #
      ##########################################
      
      observeEvent(input$options, {
        req(input$options)
        shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = TRUE)
        output$options_card <- renderUI({
          category <- switch(id, "settings_data_sources" = "data source", "settings_datamarts" = "datamart", "settings_studies" = "study", "settings_subsets" = "subset")
          link_id <- as.integer(substr(isolate(input$edit_code), nchar("edit_code") + 1, nchar(isolate(input$edit_code))))
          
          div(id = ns("options_card"),
            make_card(translate(language, "study_options"),
              div(
                make_persona_picker(language, ns, "studies_access_people", options = tibble::tribble(
                  ~key, ~imageInitials, ~text, ~secondaryText,
                  1, "JD", "John Doe", "Intensivist",
                  2, "JD", "Jane Doe", "Data scientist"
                ), width = "100%"),
                htmltools::br(),
                shiny.fluent::PrimaryButton.shinyInput("save", translate(language, "save"))
              )
            )
          )
          
          # options <- r$options %>% dplyr::filter(`Category` == category & `Link ID` == link_id) %>% dplyr::pull(`Code`)
          # data_management_edit_card(language, ns, type = "code", code = code, link_id = link_id, title = paste0("edit_", category, "_code"))
        })
      })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      observeEvent(input$edit_code, {
        req(input$edit_code)
        shiny.fluent::updateToggle.shinyInput(session, "edit_card_toggle", value = TRUE)
        output$edit_card <- renderUI({
          category <- switch(id, "settings_data_sources" = "data source", "settings_datamarts" = "datamart", "settings_studies" = "study", "settings_subsets" = "subset")
          link_id <- as.integer(substr(isolate(input$edit_code), nchar("edit_code") + 1, nchar(isolate(input$edit_code))))
          code <- r$code %>% dplyr::filter(`Category` == category & `Link ID` == link_id) %>% dplyr::pull(`Code`)
          data_management_edit_card(language, ns, type = "code", code = code, link_id = link_id, title = paste0("edit_", category, "_code"))
        })
      })
      
      observeEvent(input$edit_save, {
        category <- switch(id, "settings_data_sources" = "data source", "settings_datamarts" = "datamart", "settings_studies" = "study", "settings_subsets" = "subset")
        link_id <- as.integer(substr(isolate(input$edit_code), nchar("edit_code") + 1, nchar(isolate(input$edit_code))))
        code_id <- r$code %>% dplyr::filter(Category == category, `Link ID` == link_id) %>% dplyr::pull(`Code ID`)
        r$code <- r$code %>% dplyr::mutate(Code = ifelse(`Code ID` == code_id, input$ace_edit_code, Code))
        
        output$warnings4 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
        })
        shinyjs::show("warnings4")
        shinyjs::delay(3000, shinyjs::hide("warnings4"))
      })
  })
}
    
## To be copied in the UI
# mod_settings_data_management_ui("settings_studies_ui_1")
    
## To be copied in the server
# mod_settings_data_management_server("settings_studies_ui_1")
