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
    
    # Dropdowns shown in datatable for each page
    dropdowns <- tibble::tribble(~page_name, ~dropdowns,
                                 "settings/data_sources", "",
                                 "settings/datamarts", "data_source",
                                 "settings/studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
                                 "settings/subsets", c("datamart", "study"))
    
    ##########################################
    # Settings / Data sources                #
    ##########################################
    
    if (page == "settings/data_sources"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_card(language, ns, creation_card = "create_data_source", datatable_card = "data_sources_management", activated = c("datatable_card")),
        data_management_creation_card(language, ns, "create_data_source",  textfields = c("name", "description"), textfields_width = "300px"),
        data_management_datatable_card(language, ns, "data_sources_management")
      ) -> result
    }
    
    ##########################################
    # Settings / Datamarts                   #
    ##########################################
    
    if (page == "settings/datamarts"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")), shiny::uiOutput(ns("warnings4")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_card(
          language, ns, creation_card = "create_datamart", datatable_card = "datamarts_management", 
          edit_card = "edit_datamart_code", options_card = "datamart_options", activated = c("datatable_card")),
        data_management_creation_card(language, ns, "create_datamart",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
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
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")), shiny::uiOutput(ns("warnings4")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_card(
          language, ns, creation_card = "create_study", datatable_card = "studies_management", options_card = "study_options",
          activated = c("datatable_card")),
        data_management_creation_card(
          language, ns, "create_study",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(),
          dropdowns_width = "300px"),
        data_management_datatable_card(language, ns, "studies_management"),
        shiny::uiOutput(ns("edit_card")),
        shiny::uiOutput(ns("options_card"))
      ) -> result
    }
    
    ##########################################
    # Settings / Subsets                     #
    ##########################################
    
    if (page == "settings/subsets"){
      div(class = "main",
        shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
        shiny.fluent::reactOutput(ns("management_delete_confirm")),
        data_management_toggle_card(language, ns, creation_card = "create_subset", datatable_card = "subsets_management", activated = c("datatable_card")),
        data_management_creation_card(
          language, ns, "create_subset",
          textfields = c("name", "description"), textfields_width = "300px",
          dropdowns = dropdowns %>% dplyr::filter(page_name == page) %>% dplyr::pull(dropdowns) %>% unlist(), dropdowns_width = "300px"),
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
    
    toggles <- c("creation_card", "datatable_card", "edit_card", "options_card")
    data_management_elements <- c("data_sources", "datamarts", "studies", "subsets", "patient_lvl_module_families", "aggregated_module_families")
    dropdowns <- tibble::tribble(~page_name, ~dropdowns,
                                 "data_sources", "",
                                 "datamarts", "data_source",
                                 "studies", c("datamart", "patient_lvl_module_family", "aggregated_module_family"),
                                 "subsets", c("datamart", "study"))
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Data management / Add a new element    #
    ##########################################
    
    # Update dropdowns with reactive data
    sapply(c(data_management_elements), function(data_management_element){
                    observeEvent(r[[data_management_element]], {
                      options <- tibble_to_list(r[[data_management_element]], "id", "name", rm_deleted_rows = TRUE)
                    shiny.fluent::updateDropdown.shinyInput(session, id_get_other_name(data_management_element, "singular_form"),
                                                            options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
                    })
                  })
    
    # When add button is used
    observeEvent(input$add, {
      
      # Check if required fields are filled (name is required, description is not)
      # We can add other requirements (eg characters only)
      name_check <- ifelse(is.null(input$name), FALSE, TRUE)
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)
      
      req(name_check)
      
      data_var <- substr(id, nchar("settings_") + 1, nchar(id)) 
      distinct_names <- r[[data_var]] %>% dplyr::filter(!deleted) %>% dplyr::pull(name)
      
      if (input$name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(input$name %not_in% (r[[data_var]] %>% dplyr::filter(!deleted) %>% dplyr::pull(name)))
      
      last_row <- max(r[[data_var]]["id"])
      
      new_data <- data_management_new_data(
        id,
        new_id = last_row + 1,
        name = as.character(input$name),
        description = ifelse(is.null(input$description), "", as.character(input$description)),
        creator = as.numeric(r$user_id),
        datetime = as.character(Sys.time()),
        deleted = FALSE,
        data_source_id = as.integer(input$data_source),
        datamart_id = as.integer(input$datamart),
        study_id = as.integer(input$study),
        patient_lvl_module_family_id = as.integer(input$patient_lvl_module_family),
        aggregated_module_family_id = as.integer(input$aggregated_module_family))
    
      r[[data_var]] <- r[[data_var]] %>% dplyr::bind_rows(new_data)
      # Update also temp dataframe
      r[[paste0(data_var, "_temp")]] <- r[[data_var]]
      
      # If the row we add is a datamart :
      # - add a row in the code table
      # - add rows in options table
      last_row_code <- max(r$code["id"])
      last_row_options <- max(r$options["id"])
      if (id == "settings_datamarts"){ 
        r$code <- r$code %>% dplyr::bind_rows(
          tibble::tribble(~id, ~category, ~link_id, ~code, ~creator, ~datetime, ~deteleted,
            last_row_code + 1, "datamart", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
        r$options <- r$options %>% dplyr::bind_rows(
          tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator, ~datetime, ~deleted,
            last_row_options + 1, "datamart", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
            last_row_options + 2, "datamart", last_row + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE))
      }
      if (id == "settings_studies"){
        r$options <- r$options %>% dplyr::bind_rows(
          tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator, ~datetime, ~deleted,
            last_row_options + 1, "study", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))
      }
      
      # If the row we add is a datamart or a study, hide options card
      shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
      # Hide create card
      shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
      
      message <- paste0(id_get_other_name(id, "singular_form"), "_added")
      
      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 4), style = "margin-top:10px;"))
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
        data_management_datatable(
          id = id, data = data_management_data(id, r), ns = ns, r = r, language = language, data_management_elements = data_management_elements,
          dropdowns = switch(id,
                             "settings_data_sources" = "",
                             "settings_datamarts" = c("data_source_id" = "data_sources"),
                             "settings_studies" = c("datamart_id" = "datamarts",
                                                    "patient_lvl_module_family_id" = "patient_lvl_module_families",
                                                    "aggregated_module_family_id" = "aggregated_module_families"),
                             "settings_subsets" = c("study_id" = "studies")
        )),
        options = list(dom = "t<'bottom'p>",
                       columnDefs = list(
                         list(className = "dt-center", targets = c(0, -1, -2, -3)),
                         # -1 : action column / -2 : datetime column
                         list(width = "80px", targets = -1), list(width = "130px", targets = -2),
                         list(sortable = FALSE, targets = data_management_datatable_options(data_management_data(id, r), id, "non_sortable")))
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
    
      # Each time a row is updated, modify temp variable
      observeEvent(input$management_datatable_cell_edit, {
        edit_info <- input$management_datatable_cell_edit
        edit_info$col <- edit_info$col + 1 # Datatable cols starts at 0, we have to add 1
        data_var <- substr(id, nchar("settings_") + 1, nchar(id)) 
        r[[paste0(data_var, "_temp")]] <- DT::editData(r[[paste0(data_var, "_temp")]], edit_info)
      })
    
      # Each time a dropdown is updated, modify temp variable
      sapply(data_management_elements[!data_management_elements %in% c("patient_lvl_module_families", "aggregated_module_families")], function(data_var){
        observeEvent(r[[data_var]], {
          sapply(r[[data_var]] %>% dplyr::filter(!deleted) %>% dplyr::pull(id), function(id){
            sapply(dropdowns %>% dplyr::filter(page_name == data_var) %>% dplyr::pull(dropdowns) %>% unlist(), function(dropdown){
              observeEvent(input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]], {
                r[[paste0(data_var, "_temp")]][[which(r[[paste0(data_var, "_temp")]]["id"] == id), paste0(dropdown, "_id")]] <-
                  input[[paste0(id_get_other_name(dropdown, "plural_form"), id)]]
              })
            })
          })
        })
      })
    
      observeEvent(input$management_save, {
        
        data_var <- substr(id, nchar("settings_") + 1, nchar(id))
        
        # Make sure there's no duplicate in names
        duplicates <- r[[paste0(data_var, "_temp")]] %>% dplyr::filter(!deleted) %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        if (duplicates > 0){
          output$warnings1 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, "modif_names_duplicates"), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings1")
          shinyjs::delay(3000, shinyjs::hide("warnings1"))
        }
        req(duplicates == 0)
        
        # Restitute temp values to permanent variable
        r[[data_var]] <- r[[paste0(data_var, "_temp")]]
        
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
        data_var <- substr(id, nchar("settings_") + 1, nchar(id))
        row_deleted <- as.integer(substr(input$deleted_pressed, nchar("delete") + 1, nchar(input$deleted_pressed)))
        r[[data_var]][which(r[[data_var]][1] == row_deleted), "deleted"] = TRUE
        # Update also temp dataframe
        r[[paste0(data_var, "_temp")]] <- r[[data_var]]
        
        # Notification to user
        message <- paste0(id_get_other_name(id, "singular_form"), "_deleted")
  
        output$warnings3 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
        })
        shinyjs::show("warnings3")
        shinyjs::delay(3000, shinyjs::hide("warnings3"))
      })
      
      ##########################################
      # Edit options by selecting a row        #
      ##########################################
      
      observeEvent(input$options, {
        req(input$options)
        shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = TRUE)
        output$options_card <- renderUI({
          category <- id_get_other_name(id, "singular_form")
          link_id <- as.integer(substr(isolate(input$options), nchar("options") + 1, nchar(isolate(input$options))))
          data_management_options_card(language, ns, id, r, category_filter = category, link_id_filter = link_id,
            title = paste0(id_get_other_name(id, "singular_form"), "_options"))
        })
      })
      
      observeEvent(input$options_save, {
        category_filter <- id_get_other_name(id, "singular_form")
        link_id_filter <- as.integer(substr(isolate(input$options), nchar("options") + 1, nchar(isolate(input$options))))
        options <- r$options %>% dplyr::filter(category == category_filter, link_id == link_id_filter)
        options_by_cat <- id_get_other_name(id, "options_by_cat")
        
        if("show_only_aggregated_data" %in% options_by_cat){
          option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
          r$options[which(r$options[["id"]] == option_id), "value_num"] <- input[[paste0(category_filter, "_show_only_aggregated_data")]]
          r$options <- r$options %>% dplyr::mutate(value_num = ifelse(id == option_id, input[[paste0(category_filter, "_show_only_aggregated_data")]], value_num))
        }
        
        if ("user_allowed_read" %in% options_by_cat){
          users_already_allowed <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(value_num)
          users_allowed <- input[[paste0(category_filter, "_users_allowed_read")]]
          last_row <- max(r[[substr(id, nchar("settings_") + 1, nchar(id))]]["id"])
          
          # Delete users not in the selected list
          rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
          rows_to_del <-
            options %>%
            dplyr::filter(name == "user_allowed_read" & value_num %not_in% users_allowed) %>%
            dplyr::pull(id)
          r$options <- r$options %>% dplyr::filter(id %not_in% rows_to_del)
          
          # Add users in the selected list
          users_allowed <- users_allowed[!users_allowed %in% users_already_allowed]
          if (length(users_allowed) != 0){
            r$options <-
              r$options %>%
              dplyr::bind_rows(
                tibble::tibble(id = ((last_row + 1) + (1:length(users_allowed))), category = category_filter, link_id = link_id_filter,
                               name = "user_allowed_read", value = "", value_num = users_allowed, creator = as.integer(r$user_id),
                               datetime = as.character(Sys.time()), deleted = FALSE))
          }
        }
        
        output$warnings4 <- renderUI({
          div(shiny.fluent::MessageBar(translate(language, "modif_saved"), messageBarType = 4), style = "margin-top:10px;")
        })
        shinyjs::show("warnings4")
        shinyjs::delay(3000, shinyjs::hide("warnings4"))
      })
      
      ##########################################
      # Edit code by selecting a row           #
      ##########################################
      
      observeEvent(input$edit_code, {
        req(input$edit_code)
        shiny.fluent::updateToggle.shinyInput(session, "edit_card_toggle", value = TRUE)
        output$edit_card <- renderUI({
          category_filter <- id_get_other_name(id, "singular_form")
          link_id_filter <- as.integer(substr(isolate(input$edit_code), nchar("edit_code") + 1, nchar(isolate(input$edit_code))))
          code <- r$code %>% dplyr::filter(category == category_filter & link_id == link_id_filter) %>% dplyr::pull(code)
          data_management_edit_card(language, ns, type = "code", code = code, link_id = link_id_filter, title = paste0("edit_", category_filter, "_code"))
        })
      })
      
      observeEvent(input$edit_save, {
        category_filter <- id_get_other_name(id, "singular_form")
        link_id_filter <- as.integer(substr(isolate(input$edit_code), nchar("edit_code") + 1, nchar(isolate(input$edit_code))))
        code_id <- r$code %>% dplyr::filter(category == category_filter, link_id == link_id_filter) %>% dplyr::pull(id)
        r$code <- r$code %>% dplyr::mutate(code = ifelse(id == code_id, input$ace_edit_code, code))
        
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
