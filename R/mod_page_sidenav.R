#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_sidenav_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
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
            list(name = translate(language, "home", words), key = "home",
                 url = shiny.router::route_link("home"))
            )
          )
        ),
        initialSelectedKey = "home",
        selectedKey = "home",
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
      div(id = ns("exclusion_reason_div"),
        div(class = "input_title", translate(language, "exclusion_reason", words)),
        div(shiny.fluent::Dropdown.shinyInput(ns("exclusion_reason"), value = NULL, options = list()))), br(),
      # make_dropdown(language = language, ns = ns, label = "exclusion_reason"), br(),
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
      links_data_management <<- rlist::list.append(links_data_management, list(name = translate(language, page, words),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links_plugins_modules <- list()
    sapply(c("plugins", "patient_lvl_modules", "aggregated_modules"), function(page){
      links_plugins_modules <<- rlist::list.append(links_plugins_modules, list(name = translate(language, page, words),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links <- list()
    sapply(c("general_settings", "app_db", "users", "r_console", "data_management", "plugins_modules", "log"), function(page){
      # Sub links for data management
      if (page == "data_management") links <<- rlist::list.append(links, list(name = translate(language, page, words),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
      # # Sub links for plugins & modules
      else if (page == "plugins_modules") links <<- rlist::list.append(links, list(name = translate(language, page, words),
        id = ns(page), key = page, links = links_plugins_modules, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      # 
      # No sub links
      else links <<- rlist::list.append(links, list(name = translate(language, page, words),
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
mod_page_sidenav_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (id %in% c("patient_level_data", "aggregated_data")){
      
      ##########################################
      # Patient-level & aggregated data        #
      ##########################################
      
      observeEvent(r$datamarts, {
        
        # Update dropdown
        shiny.fluent::updateDropdown.shinyInput(session, "datamart", 
          options = tibble_to_list(r$datamarts %>% dplyr::arrange(name), "id", "name", rm_deleted_rows = TRUE), value = NULL)
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$datamart, {
        
        # Studies depending on the chosen datamart
        
        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
        
        # Reset r$chosen_study (to reset main display)
        r$chosen_study <- NA_integer_
        
        # Save value in r$chosen_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
        r$chosen_datamart <- input$datamart
        
        # Reset dropdowns & uiOutput
        # Hide exclusion_reason dropdown
        
        shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        # If studies is empty
        if (nrow(studies) == 0) shiny.fluent::updateDropdown.shinyInput(session, "study", options = list(), value = NULL, errorMessage = translate(language, "no_study_available", words))
        
        if (nrow(studies) > 0){
          
          # Update dropdowns
          shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies %>% dplyr::arrange(name), "id", "name", rm_deleted_rows = TRUE), value = NULL)
          
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
        shiny.fluent::updateDropdown.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        # If subsets si empty
        if (nrow(subsets) == 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = translate(language, "no_subset_available", words))
        if (nrow(subsets) > 0) shiny.fluent::updateDropdown.shinyInput(session, "subset", options = tibble_to_list(subsets, "id", "name", rm_deleted_rows = TRUE), value = NULL)
      })
      
      observeEvent(input$subset, {
        r$chosen_subset <- input$subset
        
        # Reset dropdown & uiOutput
        shiny.fluent::updateDropdown.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        # Select patients who belong to this subset
        subset_patients <- r$subset_patients %>% dplyr::filter(subset_id == input$subset)
        patients <- tibble::tribble()
        if (nrow(subset_patients) > 0 & nrow(r$patients) > 0){
          patients <- r$patients %>% dplyr::inner_join(subset_patients %>% dplyr::select(patient_id), by = "patient_id")
        }
        
        if (nrow(patients) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available", words))
        if (nrow(patients) > 0){
          # Order patients by patient_id
          patients <- patients %>% dplyr::arrange(patient_id)
          
          # Update patients dropdown
          shiny.fluent::updateDropdown.shinyInput(session, "patient", 
          options = convert_tibble_to_list(data = patients %>% dplyr::mutate(name_display = paste0(patient_id, " - ", gender, " - ", age, " ", translate(language, "years", words))), 
            key_col = "patient_id", text_col = "name_display"))
        }
      })
      
      observeEvent(input$patient, {
        
        r$chosen_patient <- input$patient
        
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) == 0) shiny.fluent::updateDropdown.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available", words))
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient)) > 0){
          
          # Order stays by admission datetime
          stays <- r$stays %>% dplyr::filter(patient_id == input$patient) %>% dplyr::arrange(admission_datetime)
        
          # Load stays of the patient & update dropdown
          shiny.fluent::updateDropdown.shinyInput(session, "stay",
            options = convert_tibble_to_list(data = stays %>% dplyr::mutate(name_display = paste0(unit_name, " - ", 
              format(as.POSIXct(admission_datetime), format = "%Y-%m-%d"), " ", translate(language, "to", words), " ",  format(as.POSIXct(discharge_datetime), format = "%Y-%m-%d"))),
              key_col = "stay_id", text_col = "name_display"), value = NULL)
        }
        
        # Update patient informations on sidenav
        
        style <- "display:inline-block; width:60px; font-weight:bold;"
        output$patient_info <- renderUI({
          tagList(span(translate(language, "age", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years", words), br(),
          span(translate(language, "gender", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender))
        })
        
        # Update patient status dropdown
        
        subsets_names <- ""
        if (nrow(r$subset_patients) > 0){
          r$subset_patients %>% 
            # Filter on patient_id
            dplyr::filter(patient_id == input$patient) %>%
            # Subsets from chosen study
            dplyr::inner_join(r$subsets %>% dplyr::filter(study_id == r$chosen_study) %>% dplyr::select(subset_id = id, name), by = "subset_id") -> subsets
          
          if (nrow(subsets) > 0) subsets %>% dplyr::pull(name) -> subsets_names
        }
        
        # subset_names = names of subsets from which patient belongs to
        
        value <- "undefined"
        if (translate("FR", "subset_included_patients", words) %in% subsets_names | translate("EN", "subset_included_patients", words) %in% subsets_names) value <- "included"
        if (translate("FR", "subset_excluded_patients", words) %in% subsets_names | translate("EN", "subset_excluded_patients", words) %in% subsets_names) value <- "excluded"
        
        # Set to null to reload input$patient_status if the value is the same between to patients, to load exclusion_reason input
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", value = NULL)
        shiny.fluent::updateDropdown.shinyInput(session, "patient_status", options = list(
          list(key = "undefined", text = translate(language, "undefined_status", words)),
          list(key = "included", text = translate(language, "included_status", words)),
          list(key = "excluded", text = translate(language, "excluded_status", words))),
          value = value)
        
        # Reset exclusion_reason dropdown & hide it
        shiny.fluent::updateDropdown.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$stay, {
        r$chosen_stay <- input$stay
        
        # Update patient informations on sidenav
        
        style <- "display:inline-block; width:60px; font-weight:bold;"
        output$patient_info <- renderUI({
          tagList(span(translate(language, "age", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years"), br(),
            span(translate(language, "gender", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender) , br(), br(),
            span(translate(language, "unit", words), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(unit_name), br(),
            span(translate(language, "from", words), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(admission_datetime), br(),
            span(translate(language, "to", words), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(discharge_datetime))
        })
      })
      
      
      observeEvent(input$patient_status, {
  
        # Get included_patients & excluded_patients subset IDs
        
        subsets <- r$subsets %>% dplyr::filter(study_id == r$chosen_study)
        
        included_patients_subset <- subsets %>% 
          dplyr::filter(name %in% c(translate("FR", "subset_included_patients", words), translate("EN", "subset_included_patients", words))) %>% dplyr::pull(id)
        excluded_patients_subset <- subsets %>% 
          dplyr::filter(name %in% c(translate("FR", "subset_excluded_patients", words), translate("EN", "subset_excluded_patients", words))) %>% dplyr::pull(id)
        
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
            subset_id = add_patients_subset_id, success_notification = FALSE, language = language)
        }
  
        # Remove patients from chosen subset
        sapply(remove_patients_subset_id, function(subset_id){
          remove_patients_from_subset(output = output, r = r, patients = tibble::tribble(~patient_id, as.integer(r$chosen_patient)),
            subset_id = subset_id, language = language)
        })
        
        # Reload r$subset_patients
        update_r(r = r, table = "subset_patients", language = language)
        
        # If choice is excluded, update exclusion reason dropdown & show dropdown
        if (input$patient_status == "excluded"){
          exclusion_reasons <- r$patients_options %>% dplyr::filter(category == "exclusion_reasons" & study_id == r$chosen_study) %>% dplyr::arrange(value)
          options <- list()
          if (nrow(exclusion_reasons) > 0) options <- convert_tibble_to_list(data = exclusion_reasons, key_col = "value_num", text_col = "value")
          
          value <- r$patients_options %>% dplyr::filter(category == "exclusion_reason" & study_id == r$chosen_study & patient_id == r$chosen_patient)
          if (nrow(value) == 0) value <- NULL
          else value <- value %>% dplyr::pull(value_num)
          
          shiny.fluent::updateDropdown.shinyInput(session, "exclusion_reason", options = options, value = value)
          shinyjs::show("exclusion_reason_div")
        }
        else shinyjs::hide("exclusion_reason_div")
        
      })
      
      observeEvent(input$exclusion_reason, {
        
        if (length(input$exclusion_reason) != 0){
          
          # If already a row, get its ID
          id <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM patients_options
          WHERE category = 'exclusion_reason' AND study_id = ", r$chosen_study, " AND patient_id = ", r$chosen_patient))
          
          last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM patients_options") %>% dplyr::pull())
          
          if (nrow(id) > 0) query <- DBI::dbSendStatement(r$db, paste0("UPDATE patients_options SET value_num = ", as.integer(input$exclusion_reason),
            ", creator_id = ", r$user_id, ", datetime = '", as.character(Sys.time()), "' WHERE id = ", id))
          
          else query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO patients_options(id, study_id, patient_id, category, value_num, creator_id, datetime, deleted)
            SELECT ", last_row + 1, ", ", r$chosen_study, ", ", r$chosen_patient, ", 'exclusion_reason', ", as.integer(input$exclusion_reason),
            ", ", r$user_id, ", '", as.character(Sys.time()), "', FALSE"))
          
          DBI::dbClearResult(query)
        }
        
        update_r(r = r, table = "patients_options", language = language)
      })
      
  
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      observeEvent(r$chosen_datamart, {
      
        # If users_allowed_read_group is set to everybody, everybody has access. Else, filter on people who has access.
        
        datamarts <- r$datamarts
        if (nrow(datamarts) > 0) {
          
          # Merge with options
          datamarts_options <- datamarts %>% dplyr::inner_join(r$options %>% 
            dplyr::filter(category == "datamart") %>% dplyr::select(option_id = id, link_id, option_name = name, value, value_num), by = c("id" = "link_id"))
          
          # Vector of authorized datamarts
          datamarts_allowed <- integer()
          
          # For each datamart, select those the user has access
          sapply(unique(datamarts_options$id), function(datamart_id){
            
            # Loop over each datamart ID
            
            users_allowed_read_group <- datamarts_options %>% dplyr::filter(id == datamart_id, option_name == "users_allowed_read_group")
            users_allowed_read <- datamarts_options %>% dplyr::filter(id == datamart_id, option_name == "user_allowed_read")
            
            if (users_allowed_read_group %>% dplyr::pull(value) == "everybody") datamarts_allowed <<- c(datamarts_allowed, datamart_id)
            else {
              if (nrow(users_allowed_read %>% dplyr::filter(value_num == r$user_id)) > 0) datamarts_allowed <<- c(datamarts_allowed, datamart_id)
            }
          })
          
          # Select authorized datamarts
          datamarts <- datamarts %>% dplyr::filter(id %in% datamarts_allowed) %>% dplyr::arrange(name)
        }
        
      shiny.fluent::updateDropdown.shinyInput(session, "datamart", options = tibble_to_list(datamarts, "id", "name", rm_deleted_rows = TRUE), value = r$chosen_datamart)
      })
      
      observeEvent(r$chosen_study, {
        req(input$datamart & !is.na(r$chosen_study))

        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart)
        if (nrow(studies) > 0) {

          # Merge with options
          studies_options <- studies %>% dplyr::inner_join(r$options %>%
            dplyr::filter(category == "study") %>% dplyr::select(option_id = id, link_id, option_name = name, value, value_num), by = c("id" = "link_id"))

          # Vector of authorized studies
          studies_allowed <- integer()

          # For each study, select those the user has access
          sapply(unique(studies_options$id), function(study_id){

            # Loop over each study ID

            users_allowed_read_group <- studies_options %>% dplyr::filter(id == study_id, option_name == "users_allowed_read_group")
            users_allowed_read <- studies_options %>% dplyr::filter(id == study_id, option_name == "user_allowed_read")

            if (users_allowed_read_group %>% dplyr::pull(value) == "everybody") studies_allowed <<- c(studies_allowed, study_id)
            else {
              if (nrow(users_allowed_read %>% dplyr::filter(value_num == r$user_id)) > 0) studies_allowed <<- c(studies_allowed, study_id)
            }
          })

          # Select authorized studies
          studies <- studies %>% dplyr::filter(id %in% studies_allowed) %>% dplyr::arrange(name)
        }

        shiny.fluent::updateDropdown.shinyInput(session, "study", options = tibble_to_list(studies, "id", "name", rm_deleted_rows = TRUE), value = r$chosen_study)
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
        # Never hide General settings page (default when you click on settings on header page)
        
        pages <- c("app_db", "users", "r_console", "data_sources", "datamarts", "studies", "subsets", "thesaurus",
          "plugins", "patient_lvl_modules", "aggregated_modules", "log")
        
        sapply(pages, function(page) if (page %not_in% r$user_accesses) shinyjs::hide(page))
        
        if ("data_sources" %not_in% r$user_accesses & "datamarts" %not_in% r$user_accesses & "studies" %not_in% r$user_accesses &
          "subsets" %not_in% r$user_accesses & "thesaurus" %not_in% r$user_accesses) shinyjs::hide("data_management")
        
        if ("plugins" %not_in% r$user_accesses & "patient_lvl_modules" %not_in% r$user_accesses & "aggregated_modules" %not_in% r$user_accesses) shinyjs::hide("plugins_modules")
      })
    }
  })
}