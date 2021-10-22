#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_sidenav_ui <- function(id, language){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Home                                   #
  ##########################################
  
  if (grepl("^home", id)){
    div(class = "sidenav",
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = translate(language, "datamarts_studies"), key = "datamarts_studies",
                 url = shiny.router::route_link("home/datamarts_studies"))
            )
          )
        ),
        initialSelectedKey = "datamarts_studies",
        selectedKey = substr(id, nchar("home") + 2, 100),
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
  # Patient-level data                     #
  ##########################################
  
  if (id == "patient_level_data"){
    div(class = "sidenav",
      make_dropdown(language = language, ns = ns, label = "datamart"),
      make_dropdown(language = language, ns = ns, label = "study"),
      make_dropdown(language = language, ns = ns, label = "subset"),
      br(), hr(),
      make_dropdown(language = language, ns = ns, label = "patient"),
      make_dropdown(language = language, ns = ns, label = "stay"), br(), hr(),
      make_dropdown(language = language, ns = ns, label = "patient_status"), br(),
      uiOutput(ns("patient_info"))
    ) -> result
  }
  
  ##########################################
  # Aggregated data                        #
  ##########################################
  
  if (id == "aggregated_data"){
    div(class = "sidenav",
      make_dropdown(language = language, ns = ns, label = "datamart"),
      make_dropdown(language = language, ns = ns, label = "study"),
      make_dropdown(language = language, ns = ns, label = "subset"),
    ) -> result
  }
  
  ##########################################
  # Settings                               #
  ##########################################
  
  if (grepl("^settings", id)){
    div(class = "sidenav", 
      # uiOutput(ns("sidenav"))
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = translate(language, "general_settings"), key = "general", url = shiny.router::route_link("settings/general")),
            list(name = translate(language, "app_db"), key = "app_db", url = shiny.router::route_link("settings/app_db")),
            list(name = translate(language, "users"), key = "users", url = shiny.router::route_link("settings/users")),
            list(name = translate(language, "r_console"), key = "r_console", url = shiny.router::route_link("settings/r_console")),
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
          ))
        ),
        initialSelectedKey = "general_settings",
        selectedKey = substr(id, nchar("settings") + 2, 100),
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
  
  result
}
    
#' page_sidenav Server Functions
#'
#' @noRd 
mod_page_sidenav_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Patient-level data                     #
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
      
      # Reset dropdowns & uiOutput
      shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      output$patient_info <- renderUI("")
      
      # If studies is empty
      if (nrow(studies) == 0) shiny.fluent::updateDropdown.shinyInput(session, "study", options = list(), value = NULL, errorMessage = translate(language, "no_study_available"))
      
      if (nrow(studies) > 0){
        
        # Update dropdowns
        shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies, "id", "name", rm_deleted_rows = TRUE))
        
        # Code of datamart will be run from mod_patient_and_aggregated_data.R
      }
    })
    
    observeEvent(input$study, {
      r$chosen_study <- input$study
      
      # Subsets depending on the chosen study
      subsets <- r$subsets %>% dplyr::filter(study_id == input$study)
      
      # Reset dropdowns & uiOutput
      shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL)
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      output$patient_info <- renderUI("")
      
      # If subsets si empty
      if (nrow(subsets) == 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = translate(language, "no_subset_available"))
      if (nrow(subsets) > 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = tibble_to_list(subsets, "id", "name", rm_deleted_rows = TRUE))
    })
    
    observeEvent(input$subset, {
      r$chosen_subset <- input$subset
      
      # Reset dropdown & uiOutput
      shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
      output$patient_info <- renderUI("")
      
      # Select patients who belong to this subset
      subset_patients <- r$subset_patients %>% dplyr::filter(subset_id == input$subset)
      patients <- tibble::tribble()
      if (nrow(subset_patients) > 0){
        patients <- r$patients %>% dplyr::inner_join(subset_patients %>% dplyr::select(patient_id), by = "patient_id")
      }
      
      if (nrow(patients) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available"))
      if (nrow(patients) > 0){
        # Order patients by patient_id
        patients <- patients %>% dplyr::arrange(patient_id)
        
        # Update patients dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "patient", 
        options = convert_tibble_to_list(data = patients %>% dplyr::mutate(name_display = paste0(patient_id, " - ", gender, " - ", age, " ", translate(language, "years"))), 
          key_col = "patient_id", text_col = "name_display"))
      }
    })
    
    observeEvent(input$patient, {
      
      r$chosen_patient <- input$patient
      
      if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available"))
      if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) > 0){
        
        # Order stays by admission datetime
        stays <- r$stays %>% dplyr::filter(patient_id == input$patient) %>% dplyr::arrange(admission_datetime)
      
        # Load stays of the patient & update dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "stay",
          options = convert_tibble_to_list(data = stays %>% dplyr::mutate(name_display = paste0(unit_name, " - ", 
            format(as.POSIXct(admission_datetime), format = "%Y-%m-%d"), " ", translate(language, "to"), " ",  format(as.POSIXct(discharge_datetime), format = "%Y-%m-%d"))),
            key_col = "stay_id", text_col = "name_display"))
      }
      
      # Update patient informations on sidenav
      
      style <- "display:inline-block; width:60px; font-weight:bold;"
      output$patient_info <- renderUI({
        tagList(span(translate(language, "age"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years"), br(),
        span(translate(language, "gender"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender))
      })
      
      # shiny.fluent::updateDropdown.shinyInput()
    })
    
    observeEvent(input$stay, {
      r$chosen_stay <- input$stay
      
      # Update patient informations on sidenav
      
      style <- "display:inline-block; width:60px; font-weight:bold;"
      output$patient_info <- renderUI({
        tagList(span(translate(language, "age"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years"), br(),
          span(translate(language, "gender"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender) , br(), br(),
          span(translate(language, "unit"), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(unit_name), br(),
          span(translate(language, "from"), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(admission_datetime), br(),
          span(translate(language, "to"), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(discharge_datetime))
      })
    })
    
    
    observeEvent(input$patient_status, {
      
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
    
    observeEvent(r$users, output$test <- renderText(print(paste0("ID is ", id))))
    
    
    ##########################################
    # Settings                               #
    ##########################################
    
    # if (grepl("^settings", id)){
    #   
    #   observeEvent(r$user_accesses, {
    #     
    #     output$sidenav <- renderUI({
    #       
    #       settings <- tagList()
    #       
    #       # For first links, before data management
    #       sapply(c("app_db", "users", "r_console"), function(page){
    #         disabled <- FALSE
    #         if (page %not_in% r$user_accesses) disabled <- TRUE
    #         settings <<- tagList(settings,
    #           list(name = translate(language, page), key = page, url = shiny.router::route_link(paste0("settings/", page)), disabled = disabled))
    #       })
    #       
    #       # For data management link & sub-links
    # 
    #       sub_settings <- tagList()
    #       sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page){
    #         if (page %in% r$user_accesses){
    #           disabled <- FALSE
    #           if (page %not_in% r$user_accesses) disabled <- TRUE
    #           sub_settings <<- tagList(sub_settings,
    #             list(name = translate(language, page), key = page, url = shiny.router::route_link(paste0("settings/", page)), disabled = disabled))
    #         }
    #       })
    # 
    #       settings <- tagList(settings,
    #         list(name = translate(language, "data_management"), key = "data_management", links = list(sub_settings),
    #           selectedKey = substr(page, nchar("settings") + 2, 100), isExpanded = TRUE))
    #       
    #       # For modules & plugins link & sublinks
    #       
    #       sub_settings <- tagList()
    #       sapply(c("plugins", "modules_patient_lvl", "modules_aggregated"), function(page){
    #         if (page %in% r$user_accesses){
    #           disabled <- FALSE
    #           if (page %not_in% r$user_accesses) disabled <- TRUE
    #           sub_settings <<- tagList(sub_settings,
    #             list(name = translate(language, page), key = page, url = shiny.router::route_link(paste0("settings/", page)), disabled = disabled))
    #         }
    #       })
    # 
    #       settings <- tagList(settings,
    #         list(name = translate(language, "modules_plugins"), key = "modules_plugins", links = list(sub_settings),
    #           selectedKey = substr(page, nchar("settings") + 2, 100), isExpanded = TRUE))
    #       # 
    #       # if ("log" %in% r$user_accesses) settings <- tagList(settings,
    #       #                                                     list(name = translate(language, "log"), key = "log", url = shiny.router::route_link("settings/log")))
    #       
    #       
    #       
    #       shiny.fluent::Nav(
    #         groups = list(
    #           list(links = list(
    #             list(name = translate(language, "general_settings"), key = "general", url = shiny.router::route_link("settings/general")),
    #             settings
    #       #       list(name = translate(language, "app_db"), key = "app_db", url = shiny.router::route_link("settings/app_db"), disabled = TRUE),
    #       #       list(name = translate(language, "users"), key = "users", url = shiny.router::route_link("settings/users")),
    #       #       list(name = translate(language, "r_console"), key = "r_console", url = shiny.router::route_link("settings/r_console")),
    #       #       list(name = translate(language, "data_management"), key = "data", links = list(
    #       #         list(name = translate(language, "data_sources"), key = "data_sources", url = shiny.router::route_link("settings/data_sources")),
    #       #         list(name = translate(language, "datamarts"), key = "datamarts", url = shiny.router::route_link("settings/datamarts")),
    #       #         list(name = translate(language, "studies"), key = "studies", url = shiny.router::route_link("settings/studies")),
    #       #         list(name = translate(language, "subsets"), key = "subsets", url = shiny.router::route_link("settings/subsets")),
    #       #         list(name = translate(language, "thesaurus"), key = "thesaurus", url = shiny.router::route_link("settings/thesaurus"))
    #       #       ),
    #       #       initialSelectedKey = "data_source",
    #       #       selectedKey = substr(page, nchar("settings") + 2, 100),
    #       #       isExpanded = TRUE),
    #       #       list(name = translate(language, "modules_plugins"), key = "modules", links = list(
    #       #         list(name = translate(language, "plugins"), key = "plugins", url = shiny.router::route_link("settings/plugins")),
    #       #         list(name = translate(language, "modules_patient_lvl"), key = "modules_patient_lvl", url = shiny.router::route_link("settings/modules_patient_lvl")),
    #       #         list(name = translate(language, "modules_aggregated"), key = "modules_aggregated", url = shiny.router::route_link("settings/modules_aggregated"))
    #       #       ),
    #       #       initialSelectedKey = "data_source",
    #       #       selectedKey = substr(page, nchar("settings") + 2, 100),
    #       #       isExpanded = TRUE),
    #       #       list(name = translate(language, "log"), key = "log", url = shiny.router::route_link("settings/log"))
    #           ))
    #         ),
    #         initialSelectedKey = "general_settings",
    #         selectedKey = substr(id, nchar("settings") + 2, 100),
    #       #   # selectedKey = "general",
    #         styles = list(
    #           root = list(
    #             height = "100%",
    #             boxSizing = "border-box",
    #             overflowY = "auto"
    #           )
    #         )
    #       )
    #     })
    #     
    #   })
    #   #   
    # 
    #     
    #     # output$test <- renderText(print(paste0("ID is ", id)))
    #     
    #     # output$sidenav <- renderUI("This is a")
    #     
    #     # output$sidenav <- renderUI({
    #     #   
    #     # })
    #   # })
    # }
 
  })
}