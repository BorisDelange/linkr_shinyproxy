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
      uiOutput(ns("patient_info")),
      textOutput(ns("test"))
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
    
    links_data_management <- list()
    lapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page){
      links_data_management <<- rlist::list.append(links_data_management, list(name = translate(language, page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links_plugins_modules <- list()
    sapply(c("plugins", "modules_patient_lvl", "modules_aggregated"), function(page){
      links_plugins_modules <<- rlist::list.append(links_plugins_modules, list(name = translate(language, page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links <- list()
    sapply(c("general_settings", "app_db", "users", "r_console", "data_management", "plugins_modules", "log"), function(page){
      # Sub links for data management
      if (page == "data_management") links <<- rlist::list.append(links, list(name = translate(language, page),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
      # # Sub links for plugins & modules
      else if (page == "plugins_modules") links <<- rlist::list.append(links, list(name = translate(language, page),
        id = ns(page), key = page, links = links_plugins_modules, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      # 
      # No sub links
      else links <<- rlist::list.append(links, list(name = translate(language, page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    div(class = "sidenav", 
      shiny.fluent::Nav(
        groups = list(
          list(links = links)
        ),
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
    
    if (id %in% c("patient_level_data", "aggregated_data")){
      
      ##########################################
      # Patient-level & aggregated data        #
      ##########################################
      
      observeEvent(r$datamarts, {
        
        # Datamarts to which the user has access
        datamarts_allowed <- 
          r$options %>% 
          dplyr::filter(category == "datamart" & name == "user_allowed_read" & value_num == r$user_id) %>%
          dplyr::pull(link_id)
        datamarts <- r$datamarts %>% dplyr::filter(id %in% datamarts_allowed)
        
        # Update dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = tibble_to_list(datamarts, "id", "name", rm_deleted_rows = TRUE), value = NULL)
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
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(), value = NULL)
        output$patient_info <- renderUI("")
        
        # If studies is empty
        if (nrow(studies) == 0) shiny.fluent::updateDropdown.shinyInput(session, "study", options = list(), value = NULL, errorMessage = translate(language, "no_study_available"))
        
        if (nrow(studies) > 0){
          
          # Update dropdowns
          shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies, "id", "name", rm_deleted_rows = TRUE), value = NULL)
          
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
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(), value = NULL)
        output$patient_info <- renderUI("")
        
        # If subsets si empty
        if (nrow(subsets) == 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = translate(language, "no_subset_available"))
        if (nrow(subsets) > 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = tibble_to_list(subsets, "id", "name", rm_deleted_rows = TRUE), value = NULL)
      })
      
      observeEvent(input$subset, {
        r$chosen_subset <- input$subset
        
        # Reset dropdown & uiOutput
        shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(), value = NULL)
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
              key_col = "stay_id", text_col = "name_display"), value = NULL)
        }
        
        # Update patient informations on sidenav
        
        style <- "display:inline-block; width:60px; font-weight:bold;"
        output$patient_info <- renderUI({
          tagList(span(translate(language, "age"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years"), br(),
          span(translate(language, "gender"), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender))
        })
        
        # Update patient status dropdown
        
        r$subset_patients %>% 
          # Filter on patient_id
          dplyr::filter(patient_id == input$patient) %>%
          # Subsets from chosen study
          dplyr::inner_join(r$subsets %>% dplyr::filter(study_id == r$chosen_study) %>% dplyr::select(subset_id = id, name), by = "subset_id") %>%
          dplyr::pull(name) -> subsets_names
        
        # subset_names = names of subsets from which patient belongs to
        
        value <- "undefined"
        if (translate("FR", "subset_included_patients") %in% subsets_names | translate("EN", "subset_included_patients") %in% subsets_names) value <- "included"
        if (translate("FR", "subset_excluded_patients") %in% subsets_names | translate("EN", "subset_excluded_patients") %in% subsets_names) value <- "excluded"
        
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(
          list(key = "undefined", text = translate(language, "undefined_status")),
          list(key = "included", text = translate(language, "included_status")),
          list(key = "excluded", text = translate(language, "excluded_status"))),
          value = value)
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
  
        # Get included_patients & excluded_patients subset IDs
        
        subsets <- r$subsets %>% dplyr::filter(study_id == r$chosen_study)
        
        included_patients_subset <- subsets %>% 
          dplyr::filter(name %in% c(translate("FR", "subset_included_patients"), translate("EN", "subset_included_patients"))) %>% dplyr::pull(id)
        excluded_patients_subset <- subsets %>% 
          dplyr::filter(name %in% c(translate("FR", "subset_excluded_patients"), translate("EN", "subset_excluded_patients"))) %>% dplyr::pull(id)
        
        add_patients_subset_id <- NA_integer_
        remove_patients_subset_id <- NA_integer_
        
        # Put patient to included subset
        if (input$patient_status == "included"){
          add_patients_subset_id <- included_patients_subset
          remove_patients_subset_id <- excluded_patients_subset
        }
        
        # Put patient to excluded subset
        if (input$patient_status == "excluded"){
          add_patients_subset_id <- excluded_patients_subset
          remove_patients_subset_id <- included_patients_subset
        }
        
        # Remove patient from both subsets (status undefined)
        if (input$patient_status == "undefined"){
          remove_patients_subset_id <- c(included_patients_subset, excluded_patients_subset)
        }
        
        # Add patients to chosen subset
        if (!is.na(add_patients_subset_id)){
          add_patients_to_subset(output = output, r = r, patients = tibble::tribble(~patient_id, as.integer(r$chosen_patient)),
            subset_id = add_patients_subset_id, language = language)
        }
  
        # Remove patients from chosen subset
        sapply(remove_patients_subset_id, function(subset_id){
          remove_patients_from_subset(output = output, r = r, patients = tibble::tribble(~patient_id, as.integer(r$chosen_patient)),
            subset_id = subset_id, language = language)
        })
        
        # Reload r$subset_patients
        update_r(r = r, table = "subset_patients", language = language)
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
    }
    
    
    ##########################################
    # Settings                               #
    ##########################################
    
    if (grepl("^settings", id)){

      observeEvent(r$user_accesses, {
        
        # Hide links to pages that user doesn't have access
        pages <- c("general_settings", "app_db", "users", "r_console", "data_sources", "datamarts", "studies", "subsets", "thesaurus",
          "plugins", "patient_lvl_modules", "aggregated_modules", "log")
        
        sapply(pages, function(page) if (page %not_in% r$user_accesses) shinyjs::hide(page))
        
        if ("data_sources" %not_in% r$user_accesses & "datamarts" %not_in% r$user_accesses & "studies" %not_in% r$user_accesses &
          "subsets" %not_in% r$user_accesses & "thesaurus" %not_in% r$user_accesses) shinyjs::hide("data_management")
        
        if ("plugins" %not_in% r$user_accesses & "patient_lvl_modules" %not_in% r$user_accesses & "aggregated_modules" %not_in% r$user_accesses) shinyjs::hide("plugins_modules")
      })
    }
  })
}