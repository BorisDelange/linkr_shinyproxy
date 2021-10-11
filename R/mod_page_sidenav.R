#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_sidenav_ui <- function(id, language, page_style, page){
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
      div(class = "sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = list(
              list(name = translate(language, "datamarts_studies"), key = "datamarts_studies",
                   url = shiny.router::route_link("home/datamarts_studies")),
              list(name = translate(language, "messages"), key = "messages",
                   url = shiny.router::route_link("home/messages"))
              )
            )
          ),
          initialSelectedKey = "datamarts_studies",
          selectedKey = substr(page, nchar("home") + 2, 100)
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Patient-level data            #
    ##########################################
    
    if (page == "patient_level_data"){
      div(class = "sidenav",
        make_dropdown(language, ns, "datamart"),
        make_dropdown(language, ns, "study"),
        make_dropdown(language, ns, "subset"),
        htmltools::br(), htmltools::hr(),
        make_dropdown(language, ns, "patient"),
        make_dropdown(language, ns, "stay")
      ) -> result
    }
    
    ##########################################
    # Fluent / Aggregated data               #
    ##########################################
    
    if (page == "aggregated_data"){
      div(class = "sidenav",
        make_dropdown(language, ns, "datamart"),
        make_dropdown(language, ns, "study"),
        make_dropdown(language, ns, "subset"),
      ) -> result
    }
    
    ##########################################
    # Fluent / Settings                      #
    ##########################################
    
    if (grepl("^settings", page)){
      div(class = "sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = list(
              list(name = translate(language, "general_settings"), key = "general", url = shiny.router::route_link("settings/general")),
              list(name = translate(language, "app_db"), key = "app_db", url = shiny.router::route_link("settings/app_db")),
              list(name = translate(language, "users"), key = "users", url = shiny.router::route_link("settings/users")),
              list(name = translate(language, "data_management"), key = "data", links = list(
                list(name = translate(language, "data_sources"), key = "data_sources", url = shiny.router::route_link("settings/data_sources")),
                list(name = translate(language, "datamarts"), key = "datamarts", url = shiny.router::route_link("settings/datamarts")),
                list(name = translate(language, "studies"), key = "studies", url = shiny.router::route_link("settings/studies")),
                list(name = translate(language, "subsets"), key = "subsets", url = shiny.router::route_link("settings/subsets")),
                list(name = translate(language, "thesaurus"), key = "thesaurus", url = shiny.router::route_link("settings/thesaurus"))
                ),
                initialSelectedKey = "data_source",
                selectedKey = substr(page, nchar("settings") + 2, 100),
                isExpanded = TRUE),
              list(name = translate(language, "modules_plugins"), key = "modules", links = list(
                list(name = translate(language, "plugins"), key = "plugins", url = shiny.router::route_link("settings/plugins")),
                list(name = translate(language, "modules_patient_lvl"), key = "modules_patient_lvl", url = shiny.router::route_link("settings/modules_patient_lvl")),
                list(name = translate(language, "modules_aggregated"), key = "modules_aggregated", url = shiny.router::route_link("settings/modules_aggregated"))
                ),
                initialSelectedKey = "data_source",
                selectedKey = substr(page, nchar("settings") + 2, 100),
                isExpanded = TRUE),
              list(name = translate(language, "log"), key = "log", url = shiny.router::route_link("settings/log"))
              # list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
            ))
          ),
          initialSelectedKey = "general",
          selectedKey = substr(page, nchar("settings") + 2, 100),
          styles = list(
            root = list(
              height = "100%",
              boxSizing = "border-box",
              overflowY = "auto"
            )
          )
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Help                          #
    ##########################################
    
    if (grepl("^help", page)){
      div(class = "sidenav",
          shiny.fluent::Nav(
            groups = list(
              list(links = list(
                list(name = translate(language, "get_started"), key = "get_started", url = shiny.router::route_link("help/get_started")),
                list(name = translate(language, "user_side"), key = "user_side", links = list(
                  list(name = translate(language, "data_management"), key = "user_data_management", url = shiny.router::route_link("help/user_data_management")),
                  list(name = translate(language, "modules_plugins"), key = "user_modules_plugins", url = shiny.router::route_link("help/user_modules_plugins")),
                  list(name = translate(language, "patient_level_data"), key = "user_patient_lvl_data", url = shiny.router::route_link("help/user_patient_lvl_data")),
                  list(name = translate(language, "aggregated_data"), key = "user_aggregated_data", url = shiny.router::route_link("help/user_aggregated_data"))
                ),
                initialSelectedKey = "data_management", selectedKey = substr(page, nchar("help") + 2, 100), isExpanded = TRUE),
                list(name = translate(language, "dev_side"), key = "dev_side", links = list(
                  list(name = translate(language, "app_db"), key = "dev_app_db", url = shiny.router::route_link("help/dev_app_db")),
                  list(name = translate(language, "users"), key = "dev_users", url = shiny.router::route_link("help/dev_users")),
                  list(name = translate(language, "data_management"), key = "dev_data_management", url = shiny.router::route_link("help/dev_data_management")),
                  list(name = translate(language, "modules_plugins"), key = "dev_modules_plugins", url = shiny.router::route_link("help/dev_modules_plugins"))
                ),
                initialSelectedKey = "data_management", selectedKey = substr(page, nchar("help") + 2, 100), isExpanded = TRUE)
              ))
            ),
            initialSelectedKey = "get_started",
            selectedKey = substr(page, nchar("help") + 2, 100),
            styles = list(
              root = list(
                height = "100%",
                boxSizing = "border-box",
                overflowY = "auto"
              )
            )
          )
      ) -> result
    }
  }
  
  ##########################################
  # Fluid                                  #
  ##########################################
  
  if (page_style == "fluid"){
    
    ##########################################
    # Fluid / Patient-level data             #
    ##########################################
    
    if (page == "patient_level_data"){
      shiny::sidebarPanel(
        # class = "sidenav",
        width = 2,
        shiny::selectInput(ns("datamart"), htmltools::strong(translate(language, "datamart")),
                           choices = c("Données héparine" = "ufh", "Ventilation" = "wmv")),
        shiny::selectInput(ns("study"), htmltools::strong(translate(language, "study")),
                           choices = c("Etude 1 - premier anti-Xa")),
        shiny::selectInput(ns("subset"), htmltools::strong(translate(language, "subset")),
                           choices = c("Patients inclus"))
      ) -> result
    }
    
    ##########################################
    # Fluid / Aggregated data                #
    ##########################################
    
    if (page == "aggregated_data"){
      shiny::sidebarPanel(
        width = 2,
        shiny::selectInput(ns("datamart"), htmltools::strong(translate(language, "datamart")),
                           choices = c("Données héparine" = "ufh", "Ventilation" = "wmv")),
        shiny::selectInput(ns("study"), htmltools::strong(translate(language, "study")),
                           choices = c("Etude 1 - premier anti-Xa")),
        shiny::selectInput(ns("subset"), htmltools::strong(translate(language, "subset")),
                           choices = c("Patients inclus"))
      ) -> result
    }
  }
  
  result
}
    
#' page_sidenav Server Functions
#'
#' @noRd 
mod_page_sidenav_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Fluent / Patient-level data            #
    ##########################################
    
    observeEvent(r$datamarts, {
      # Datamarts to which the user has access
      datamarts_allowed <- 
        r$options %>% 
        dplyr::filter(category == "datamart" & name == "user_allowed_read" & value_num == r$user_id) %>%
        dplyr::pull(link_id)
      datamarts <- r$datamarts %>% dplyr::filter(id %in% datamarts_allowed)
      # Update dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = tibble_to_list(datamarts, "id", "name", rm_deleted_rows = TRUE))
    })
    
    observeEvent(input$datamart, {
      # Studies depending on the chosen datamart
      studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
      # Studies to which the user has access
      studies_allowed <-
        r$options %>%
        dplyr::filter(category == "study" & link_id %in% studies$id & name == "user_allowed_read" & value_num == r$user_id) %>%
        dplyr::pull(link_id)
      studies <- studies %>% dplyr::filter(id %in% studies_allowed)
      
      # Reset r$chosen_study (to reset main display)
      r$chosen_study <- NA_integer_
      
      # Save value in r$chosen_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
      r$chosen_datamart <- input$datamart
      
      # Update dropdowns
      shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      
      # If studies is empty...
      if (nrow(studies) == 0) shiny.fluent::updateDropdown.shinyInput(session, "study", options = list(), value = NULL, errorMessage = translate(language, "no_study_available"))
      
      if (nrow(studies) > 0){
        
        # Update dropdowns
        shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies, "id", "name", rm_deleted_rows = TRUE))
        
        # Load data of the datamart
        code <- DBI::dbGetQuery(r$db, paste0("SELECT code FROM code WHERE category = 'datamart' AND link_id = ", input$datamart)) %>% dplyr::pull()
        try(eval(parse(text = code)))
      }
    })
    
    observeEvent(input$study, {
      r$chosen_study <- input$study
      
      # Subsets depending on the chosen study
      subsets <- r$subsets %>% dplyr::filter(study_id == input$study)
      
      # Update dropdowns
      shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      
      # If subsets si empty...
      if (nrow(subsets) == 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = translate(language, "no_subset_available"))
      if (nrow(subsets) > 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = tibble_to_list(subsets, "id", "name", rm_deleted_rows = TRUE))
    })
    
    observeEvent(input$subset, {
      r$chosen_subset <- input$subset
      
      # Update dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      
      if (nrow(r$data_patients) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available"))
      if (nrow(r$data_patients) > 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = tibble_to_list(r$data_patients, "subject_id", "subject_id"))
    })
    
    observeEvent(input$patient, {
      r$chosen_patient <- input$patient
      
      # Load stays of the patient & update dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = tibble_to_list(r$data_patients, "subject_id", "subject_id"))
    })
    
    

    # Update the two pages dropdowns (patient-level data page & aggregated data page)
    observeEvent(r$chosen_datamart, {
      datamarts_allowed <- 
        r$options %>% 
        dplyr::filter(category == "datamart" & name == "user_allowed_read" & value_num == r$user_id) %>%
        dplyr::pull(link_id)
      datamarts <- r$datamarts %>% dplyr::filter(id %in% datamarts_allowed)
      shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = tibble_to_list(datamarts, "id", "name", rm_deleted_rows = TRUE),
                                              value = r$chosen_datamart)
    })
    
    observeEvent(r$chosen_study, {
      req(input$datamart & !is.na(r$chosen_study))
      studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
      studies_allowed <-
        r$options %>%
        dplyr::filter(category == "study" & link_id %in% studies$id & name == "user_allowed_read" & value_num == r$user_id) %>%
        dplyr::pull(link_id)
      studies <- studies %>% dplyr::filter(id %in% studies_allowed)
      shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies, "id", "name", rm_deleted_rows = TRUE),
                                              value = r$chosen_study)
    })
    
    observeEvent(r$chosen_subset, {
      req(input$study)
      subsets <- r$subsets %>% dplyr::filter(study_id == input$study)
      shiny.fluent::updateDropdown.shinyInput(session, "subset", options = tibble_to_list(subsets, "id", "name", rm_deleted_rows = TRUE),
        value = r$chosen_subset)
    })
 
  })
}
    
## To be copied in the UI
# mod_page_sidenav_ui("page_sidenav_ui_1")
    
## To be copied in the server
# mod_page_sidenav_server("page_sidenav_ui_1")
