#' settings_modules UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_modules_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")),
      shiny.fluent::reactOutput(ns("modules_delete_confirm")), 
      settings_toggle_card(language, ns, creation_card = "modules_creation_card", datatable_card = "modules_management_card",
                           options_card = "modules_options_card", activated = c("")),
      div(
        id = ns("creation_card"),
        make_card(
          translate(language, "modules_creation"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("module_type"), value = "module", options = list(
              list(key = "module", text = translate(language, "module")),
              list(key = "family", text = translate(language, "module_family"))
            ), className = "inline_choicegroup"),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(language, ns, "name", width = "300px"),
              make_textfield(language, ns, "description", width = "300px")
            ),
            shiny::conditionalPanel(
              condition = "input.module_type == 'module'", ns = ns,
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 20),
                make_dropdown(language, ns, "module_family", width = "300px"),
                make_dropdown(language, ns, "module_parent", width = "300px")
              )
            ),
            htmltools::br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
          )
        )
      ),
      div(
        id = ns("datatable_card"),
        make_card(
          translate(language, "modules_management"),
          div(
            DT::DTOutput(ns("management_datatable")),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save"), style = "top:-20px;")
          )
        )
      ),
      div(
        shiny::uiOutput(ns("options_card")),
      )
    ) -> result
  }
  
  result
}
    
#' settings_modules Server Functions
#'
#' @noRd 
mod_settings_modules_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    toggles <- c("creation_card", "datatable_card", "options_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Add a new module                       #
    ##########################################
    
    # Update dropdowns with reactive data
    data_var_families <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", 
                           "settings_aggregated_modules" = "aggregated_module_familes")
    data_var_modules <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", 
                               "settings_aggregated_modules" = "aggregated_modules")
    
    observeEvent(r[[data_var_families]], {
      options <- tibble_to_list(r[[data_var_families]], "id", "name", rm_deleted_rows = TRUE)
      shiny.fluent::updateDropdown.shinyInput(session, "module_family",
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    
    observeEvent(input$module_family, {
      module_parents <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var_modules, " WHERE module_family_id = ", input$module_family))
      options <- tibble_to_list(module_parents, "id", "name", rm_deleted_rows = TRUE, null_value = TRUE, language = language)
      shiny.fluent::updateDropdown.shinyInput(session, "module_parent",
        options = options, value = ifelse(length(options) > 0, options[[1]][["key"]], ""))
    })
    
    observeEvent(input$add, {
      new_name <- input$name
      name_check <- FALSE

      if (!is.null(new_name)){
        if (new_name != "") name_check <- TRUE
      }
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)

      req(name_check)

      # Check if chosen name is already used
      if (input$module_type == "module") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_modules", "settings_aggregated_modules" = "aggregated_modules")
      if (input$module_type == "family") table <- switch(id, "settings_patient_lvl_modules" = "patient_lvl_module_families", "settings_aggregated_modules" = "aggregated_module_families")
      
      distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", table, " WHERE deleted IS FALSE")) %>% dplyr::pull()

      if (new_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(new_name %not_in% distinct_names)

      last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", table)) %>% dplyr::pull()

      new_data <- switch(input$module_type,
        "module" = tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~creator_id, ~datetime, ~deleted,
          last_row + 1, as.character(new_name), ifelse(is.null(input$description), "", as.character(input$description)),
          as.integer(input$module_family), ifelse(is.null(input$module_parent), NA_integer_, as.integer(input$module_parent)),
          r$creator_id, as.character(Sys.time()), FALSE),
        "family" = tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
          last_row + 1, as.character(new_name), ifelse(is.null(input$description), "", as.character(input$description)),
          r$creator_id, as.character(Sys.time()), FALSE))

      DBI::dbAppendTable(r$db, table, new_data)

      r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table))
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)

      # Add a row in options table
      # last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
      # DBI::dbAppendTable(r$db, "options",
      #   tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      #     last_row_options + 1, table, last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))

      output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "new_plugin_added"), messageBarType = 4), style = "margin-top:10px;"))
      shinyjs::show("warnings1")
      shinyjs::delay(3000, shinyjs::hide("warnings1"))

      # Reset textfields
      shiny.fluent::updateTextField.shinyInput(session, "name", value = "")
      shiny.fluent::updateTextField.shinyInput(session, "description", value = "")
    })
    
  })
}
    
## To be copied in the UI
# mod_settings_modules_ui("settings_modules_ui_1")
    
## To be copied in the server
# mod_settings_modules_server("settings_modules_ui_1")
