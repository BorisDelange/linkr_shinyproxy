#' settings_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_users_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    div(class = "main",
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")),
      users_toggle_card(language, ns, activated = c("add_access_card", "accesses_management_card")),
      users_creation_card(language, ns, title = "add_user", card = "add_user", 
        textfields = c("username", "first_name", "last_name", "password"), textfields_width = "200px", 
        dropdowns = c("user_access", "user_status"), dropdowns_width = "200px"),
      users_datatable_card(language, ns, "users_management", "users_management"),
      users_creation_card(language, ns, title = "add_access", card = "add_access", 
        textfields = c("name", "description"), textfields_width = "300px"),
      users_edit_card(language, ns, "accesses_management", "accesses_management"),
      users_creation_card(language, ns, title = "add_status", card = "add_status", 
        textfields = c("name", "description"), textfields_width = "300px"),
      users_datatable_card(language, ns, "statuses_management", "statuses_management")
    ) -> result
  }
}
    
#' settings_users Server Functions
#'
#' @noRd 
mod_settings_users_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    toggles <- c("add_user_card", "users_management_card", "add_access_card", "accesses_management_card",
                 "add_status_card", "statuses_management_card")
    
    ##########################################
    # Show or hide cards   #
    ##########################################
    
    sapply(toggles, function(toggle){
      observeEvent(input[[paste0(toggle, "_toggle")]], if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle))
    })
    
    ##########################################
    # Update dropdowns with database values  #
    ##########################################
    
    observeEvent(r$users_accesses_statuses, {
      dropdowns_access <- c("add_user_user_access", "accesses_management_access")
      sapply(dropdowns_access, function(id){
        shiny.fluent::updateDropdown.shinyInput(session, id,
          options = tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "access"), "id", "name", rm_deleted_rows = FALSE))
      })
      dropdowns_status <- c("add_user_user_status")
      sapply(dropdowns_status, function(id){
        shiny.fluent::updateDropdown.shinyInput(session, id,
          options = tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "status"), "id", "name", rm_deleted_rows = FALSE))
      })
    })
    
    ##########################################
    # Add a new user                         #
    ##########################################
    
    ##########################################
    # Add an access                          #
    ##########################################
    
    observeEvent(input$add_access_add, {
      
      # Check if required fields are filled (name is required, description is not)
      # We can add other requirements (eg characters only)
      name_check <- FALSE
      if (!is.null(input$add_access_name)){
        if (input$add_access_name != "") name_check <- TRUE
      }
      if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "add_access_name", errorMessage = translate(language, "provide_valid_name"))
      if (name_check) shiny.fluent::updateTextField.shinyInput(session, "add_access_name", errorMessage = NULL)
      
      req(name_check)
      
      # Check if chosen name is already used
      distinct_names <- r$users_accesses_statuses %>% dplyr::filter(!deleted, type == "access") %>% dplyr::pull(name)

      if (input$add_access_name %in% distinct_names){
        output$warnings2 <- renderUI(div(shiny.fluent::MessageBar(translate(language, "name_already_used"), messageBarType = 3), style = "margin-top:10px;"))
        shinyjs::show("warnings2")
        shinyjs::delay(3000, shinyjs::hide("warnings2"))
      }
      req(input$add_access_name %not_in% (r$users_accesses_statuses %>% dplyr::filter(!deleted, type == "access") %>% dplyr::pull(name)))
      # 
      # last_row <- max(r[[data_var]]["id"])
      # 
      # new_data <- data_management_new_data(
      #   id,
      #   new_id = last_row + 1,
      #   name = as.character(input$name),
      #   description = ifelse(is.null(input$description), "", as.character(input$description)),
      #   creator = as.numeric(r$user_id),
      #   datetime = as.character(Sys.time()),
      #   deleted = FALSE,
      #   data_source_id = as.integer(input$data_source),
      #   datamart_id = as.integer(input$datamart),
      #   study_id = as.integer(input$study),
      #   patient_lvl_module_family_id = as.integer(input$patient_lvl_module_family),
      #   aggregated_module_family_id = as.integer(input$aggregated_module_family))
      # 
      # r[[data_var]] <- r[[data_var]] %>% dplyr::bind_rows(new_data)
      # # Update also temp dataframe
      # r[[paste0(data_var, "_temp")]] <- r[[data_var]]
      # 
      # # If the row we add is a datamart :
      # # - add a row in the code table
      # # - add rows in options table
      # last_row_code <- max(r$code["id"])
      # last_row_options <- max(r$options["id"])
      # if (id == "settings_datamarts"){ 
      #   r$code <- r$code %>% dplyr::bind_rows(
      #     tibble::tribble(~id, ~category, ~link_id, ~code, ~creator, ~datetime, ~deteleted,
      #                     last_row_code + 1, "datamart", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
      #   r$options <- r$options %>% dplyr::bind_rows(
      #     tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator, ~datetime, ~deleted,
      #                     last_row_options + 1, "datamart", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
      #                     last_row_options + 2, "datamart", last_row + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE))
      # }
      # if (id == "settings_studies"){
      #   r$options <- r$options %>% dplyr::bind_rows(
      #     tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator, ~datetime, ~deleted,
      #                     last_row_options + 1, "study", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))
      # }
      # 
      # # If the row we add is a datamart or a study, hide options card
      # shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
      # # Hide create card
      # shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
      # 
      # message <- paste0(id_get_other_name(id, "singular_form"), "_added")
      
      # output$warnings1 <- renderUI(div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 4), style = "margin-top:10px;"))
      # shinyjs::show("warnings1")
      # shinyjs::delay(3000, shinyjs::hide("warnings1"))
    })
    
    ##########################################
    # Add a status                           #
    ##########################################
    
  })
}
    
## To be copied in the UI
# mod_settings_users_ui("settings_users_ui_1")
    
## To be copied in the server
# mod_settings_users_server("settings_users_ui_1")
