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
            list(name = translate(language, "get_started", words), key = "get_started",
              url = shiny.router::route_link("home/get_started"))
            )
          )
        ),
        initialSelectedKey = "get_started",
        selectedKey = "get_started",
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
  # Patient-level & aggregated data        #
  ##########################################
  
  if (id %in% c("patient_level_data", "aggregated_data")){
    
    dropdowns <- function(names, arrows = TRUE){
      
      result <- tagList()
      
      sapply(names, function(name){
        
        action_button <- ""
        width <- "250px"
        
        if (arrows){
          action_button <- actionButton(ns(paste0(name, "_page")), "", icon = icon("arrow-right"),
            style = "background-color:white; border-width:1px; height:32px;")
          width <- "220px"
        }
        
        result <<- tagList(result,
          div(id = ns(paste0(name, "_title")), class = "input_title", translate(language, name, words)),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
            div(shiny.fluent::ComboBox.shinyInput(ns(name), allowFreeForm = FALSE, autoComplete = "on"), style = paste0("min-width:", width, "; max-width:", width, ";")),
            action_button
          )
        )
      })
      
      result
    }
  }
  
  ##########################################
  # Patient-level data                     #
  ##########################################
  
  if (id == "patient_level_data"){
    div(class = "sidenav",
      dropdowns(c("datamart", "study", "subset")),
      br(), div(id = ns("hr1"), hr()),
      dropdowns(c("patient", "stay"), arrows = FALSE),
      br(), div(id = ns("hr2"), hr()),
      dropdowns("patient_status", arrows = FALSE), br(),
      div(id = ns("exclusion_reason_div"),
        div(class = "input_title", translate(language, "exclusion_reason", words)),
        div(shiny.fluent::Dropdown.shinyInput(ns("exclusion_reason"), value = NULL, options = list()))), br(),
      uiOutput(ns("patient_info"))
    ) -> result
  }
  
  ##########################################
  # Aggregated data                        #
  ##########################################
  
  if (id == "aggregated_data"){
    
    div(class = "sidenav", dropdowns(c("datamart", "study", "subset"))) -> result
  }
  
  ##########################################
  # Plugins                                #
  ##########################################
  
  if (grepl("^plugins", id)){
    div(class = "sidenav",
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = translate(language, "plugins_patient_lvl", words = words),
              key = "patient_lvl", url = shiny.router::route_link("plugins/patient_lvl")),
            list(name = translate(language, "plugins_aggregated", words = words),
              key = "aggregated", url = shiny.router::route_link("plugins/aggregated"))
          ))
        ),
        selectedKey = substr(id, nchar("plugins") + 2, 100),
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
  # Settings                               #
  ##########################################
  
  if (grepl("^settings", id)){
    
    links_data_management <- list()
    lapply(c("data_sources", "datamarts", "thesaurus"), function(page){
      links_data_management <<- rlist::list.append(links_data_management, list(name = translate(language, page, words),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    # links_plugins_modules <- list()
    # sapply(c("plugins", "patient_lvl_modules", "aggregated_modules"), function(page){
    # sapply(c("plugins"), function(page){
    #   links_plugins_modules <<- rlist::list.append(links_plugins_modules, list(name = translate(language, page, words),
    #     id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    # })
    
    links <- list()
    sapply(c("general_settings", "app_db", "users", "r_console", "data_management", "log"), function(page){
      # Sub links for data management
      if (page == "data_management") links <<- rlist::list.append(links, list(name = translate(language, page, words),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
      # Sub links for plugins & modules
      # else if (page == "plugins_modules") links <<- rlist::list.append(links, list(name = translate(language, page, words),
      #   id = ns(page), key = page, links = links_plugins_modules, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
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
      
      # Show or hide main UI
      
      sapply(c("datamart", "study", "subset"), function(name) observeEvent(input[[paste0(name, "_page")]], r[[paste0(name, "_page")]] <- Sys.time()))
      
      observeEvent(r$datamarts, {
        
        # Update dropdown
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", 
          options = convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words), value = NULL)
        
        sapply(c("study", "subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$datamart, {
        
        # Save value in r$chosen_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
        r$chosen_datamart <- input$datamart$key
        
        # Reset chosen_study variable
        r$chosen_study <- NA_integer_
        r$chosen_patient <- NA_integer_ # TO prevent bug when execute plugin code from plugin page
        # shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL)
        
        sapply(c("subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("study"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        # Update Dropdowns AFTER having executing datamart code (prevents a bug, where UI displays and disappears)
      })
      
      observeEvent(input$study, {

        # Prevent multiple changes of r$chosen_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(r$chosen_study)) r$chosen_study <- input$study$key
        if (!is.na(r$chosen_study) & r$chosen_study != input$study$key) r$chosen_study <- input$study$key

        # Subsets depending on the chosen study
        update_r(r = r, table = "subsets")

        # Reset dropdowns & uiOutput
        shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)

        sapply(c("patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("subset"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")

        # If subsets si empty
        if (nrow(r$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = translate(language, "no_subset_available", r$words))
        if (nrow(r$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(r$subsets, key_col = "id", text_col = "name", words = r$words), value = NULL)
        
        # Load patients options
        sql <- glue::glue_sql("SELECT * FROM patients_options WHERE study_id = {r$chosen_study}", .con = r$db)
        r$patients_options <- DBI::dbGetQuery(r$db, sql)
        
      })
      
      observeEvent(input$subset, {
        
        r$chosen_subset <- input$subset$key
        
        # Reset dropdown & uiOutput
        shiny.fluent::updateComboBox.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        sapply(c("stay", "patient_status", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("patient", "hr1"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        # Select patients belonging to subsets of this study
        update_r(r = r, table = "subsets_patients")
        
        # Select patients who belong to this subset
        update_r(r = r, table = "subset_patients")
        
        patients <- tibble::tribble()
        if (nrow(r$subset_patients) > 0 & nrow(r$patients) > 0){
          patients <- r$patients %>% dplyr::inner_join(r$subset_patients %>% dplyr::select(patient_id), by = "patient_id")
        }
        
        if (nrow(patients) == 0){
          # Set chosen_patient to NA, not to display a chart when no patient is chosen
          r$chosen_patient <- NA_integer_
          shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available", words)) 
        }
        if (nrow(patients) > 0){
          # Order patients by patient_id
          patients <- patients %>% dplyr::arrange(patient_id)
          
          # Update patients dropdown
          shiny.fluent::updateComboBox.shinyInput(session, "patient", 
          options = convert_tibble_to_list(data = patients %>% dplyr::mutate(name_display = paste0(patient_id, " - ", gender, " - ", age, " ", translate(language, "years", words))), 
            key_col = "patient_id", text_col = "name_display"), words = r$words)
        }
      })
      
      observeEvent(input$patient, {
        
        r$chosen_patient <- input$patient$key
        
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient$key)) == 0) shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = translate(language, "no_patient_available", words))
        if (nrow(r$stays %>% dplyr::filter(patient_id == input$patient$key)) > 0){
          
          # Load stays of the patient & update dropdown
          shiny.fluent::updateComboBox.shinyInput(session, "stay",
            options = convert_tibble_to_list(data = r$stays %>% dplyr::filter(patient_id == input$patient$key) %>% dplyr::mutate(name_display = paste0(unit_name, " - ", 
              format(as.POSIXct(admission_datetime), format = "%Y-%m-%d"), " ", translate(language, "to", words), " ",  format(as.POSIXct(discharge_datetime), format = "%Y-%m-%d"))),
              key_col = "stay_id", text_col = "name_display", words = r$words), value = NULL)
        }
        
        # Update patient informations on sidenav
        
        style <- "display:inline-block; width:60px; font-weight:bold;"
        output$patient_info <- renderUI({
          tagList(span(translate(language, "age", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years", words), br(),
          span(translate(language, "gender", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender))
        })
        
        # Update patient status dropdown
        
        subsets_names <- ""
        if (nrow(r$subsets_patients) > 0){
          r$subsets_patients %>% 
            # Filter on patient_id
            dplyr::filter(patient_id == input$patient$key) %>%
            # Subsets from chosen study
            dplyr::inner_join(r$subsets %>% dplyr::select(subset_id = id, name), by = "subset_id") -> subsets
          
          if (nrow(subsets) > 0) subsets %>% dplyr::pull(name) -> subsets_names
        }
        
        # subset_names = names of subsets from which patient belongs to
        
        value <- "undefined"
        if (translate("FR", "subset_included_patients", words) %in% subsets_names | translate("EN", "subset_included_patients", words) %in% subsets_names) value <- "included"
        if (translate("FR", "subset_excluded_patients", words) %in% subsets_names | translate("EN", "subset_excluded_patients", words) %in% subsets_names) value <- "excluded"
        
        # Set to null to reload input$patient_status$key if the value is the same between to patients, to load exclusion_reason input
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(
          list(key = "undefined", text = translate(language, "undefined_status", words)),
          list(key = "included", text = translate(language, "included_status", words)),
          list(key = "excluded", text = translate(language, "excluded_status", words))),
          value = list(key = value))
        
        # Reset exclusion_reason dropdown & hide it
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        sapply(c("stay", "patient_status", "hr1", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$stay, {
        
        r$chosen_stay <- input$stay$key

        # Update patient informations on sidenav

        style <- "display:inline-block; width:60px; font-weight:bold;"
        output$patient_info <- renderUI({
          tagList(span(translate(language, "age", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(age), " ", translate(language, "years"), br(),
            span(translate(language, "gender", words), style = style), r$patients %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(gender) , br(), br(),
            span(translate(language, "unit", words), style = style), r$stays %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::pull(unit_name), br(),
            span(translate(language, "from", words), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(admission_datetime), br(),
            span(translate(language, "to", words), style = style), r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::pull(discharge_datetime))
        })
      })

      
      observeEvent(input$patient_status, {

        # Get included_patients & excluded_patients subset IDs

        included_patients_subset <- r$subsets %>%
          dplyr::filter(name %in% c(translate("FR", "subset_included_patients", words), translate("EN", "subset_included_patients", words))) %>% dplyr::pull(id)
        excluded_patients_subset <- r$subsets %>%
          dplyr::filter(name %in% c(translate("FR", "subset_excluded_patients", words), translate("EN", "subset_excluded_patients", words))) %>% dplyr::pull(id)

        add_patients_subset_id <- NA_integer_
        remove_patients_subset_id <- NA_integer_

        # Put patient to included subset
        if (input$patient_status$key == "included"){
          add_patients_subset_id <- included_patients_subset
          remove_patients_subset_id <- excluded_patients_subset
        }

        # Put patient to excluded subset
        if (input$patient_status$key == "excluded"){
          add_patients_subset_id <- excluded_patients_subset
          remove_patients_subset_id <- included_patients_subset
        }

        # Remove patient from both subsets (status undefined)
        if (input$patient_status$key == "undefined"){
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

        # Reload r$subset_patients & r$subsets_patients
        update_r(r = r, table = "subset_patients", language = language)
        update_r(r = r, table = "subsets_patients", language = language)

        # If choice is excluded, update exclusion reason dropdown & show dropdown
        if (input$patient_status$key == "excluded"){

          sql <- glue::glue_sql(paste0("SELECT id, value FROM modules_elements_options WHERE deleted IS FALSE AND category = 'aggregated' AND name = 'exclusion_reason_name'
            AND study_id = {r$chosen_study}"), .con = r$db)
          exclusion_reasons <- DBI::dbGetQuery(r$db, sql) %>% dplyr::arrange(value)

          options <- list()
          if (nrow(exclusion_reasons) > 0) options <- convert_tibble_to_list(data = exclusion_reasons, key_col = "id", text_col = "value", words = r$words)

          value <- r$patients_options %>% dplyr::filter(category == "exclusion_reason" & study_id == r$chosen_study & patient_id == r$chosen_patient)
          if (nrow(value) == 0) value <- NULL
          if (length(value) > 0) value <- value %>% dplyr::pull(value_num)

          shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = options, value = list(key = value))
          shinyjs::show("exclusion_reason_div")
        }
        else shinyjs::hide("exclusion_reason_div")

      })
      
      observeEvent(input$exclusion_reason, {

        if (length(input$exclusion_reason$key) != 0){

          # If already a row, get its ID
          id <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM patients_options
          WHERE category = 'exclusion_reason' AND study_id = ", r$chosen_study, " AND patient_id = ", r$chosen_patient))

          last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM patients_options") %>% dplyr::pull())

          if (nrow(id) > 0) query <- DBI::dbSendStatement(r$db, paste0("UPDATE patients_options SET value_num = ", as.integer(input$exclusion_reason$key),
            ", creator_id = ", r$user_id, ", datetime = '", as.character(Sys.time()), "' WHERE id = ", id))

          else query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO patients_options(id, study_id, patient_id, category, value_num, creator_id, datetime, deleted)
            SELECT ", last_row + 1, ", ", r$chosen_study, ", ", r$chosen_patient, ", 'exclusion_reason', ", as.integer(input$exclusion_reason$key),
            ", ", r$user_id, ", '", as.character(Sys.time()), "', FALSE"))

          DBI::dbClearResult(query)
        }

        update_r(r = r, table = "patients_options", language = language)
      })

  
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      observeEvent(r$chosen_datamart, {
        
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", options = 
          convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words), 
          value = list(key = r$chosen_datamart))
        
        # Studies depending on the chosen datamart
        
        # studies <- r$studies %>% dplyr::filter(datamart_id == r$chosen_datamart)
        
        # Reset r$chosen_study (to reset main display)
        if (length(r$chosen_study) == 0) r$chosen_study <- NA_integer_
        if (!is.na(r$chosen_study)) r$chosen_study <- NA_integer_
        
        # Reset dropdowns & uiOutput
        # Hide exclusion_reason dropdown
        
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        r$datamart_page <- Sys.time()
      })
      
      # Once the datamart is loaded, load studies
      observeEvent(r$loaded_datamart, update_r(r = r, table = "studies"))
      
      observeEvent(r$studies, {
        if (nrow(r$studies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL, 
          errorMessage = translate(language, "no_study_available", r$words))
        
        if (nrow(r$studies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "study",
          options = convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words), value = NULL)
      })
      
      observeEvent(r$chosen_study, {

        req(input$datamart$key & !is.na(r$chosen_study))

        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart$key)

        shiny.fluent::updateComboBox.shinyInput(session, "study", options =
          convert_tibble_to_list(studies %>% dplyr::arrange(name), key_col = "id", text_col = "name", words = r$words),
          value = list(key = r$chosen_study))
      })

      observeEvent(r$chosen_subset, {
        req(input$study$key)
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(r$subsets, key_col = "id", text_col = "name", words = r$words),
          value = list(key = r$chosen_subset))
      })
    }
    
    
    ##########################################
    # Settings                               #
    ##########################################
    
    # if (grepl("^settings", id)){
    # 
    #   observeEvent(r$user_accesses, {
    #     
    #     # Hide links to pages that user doesn't have access
    #     # Never hide General settings page (default when you click on settings on header page)
    #     
    #     pages <- c("app_db", "users", "r_console", "data_sources", "datamarts", "thesaurus", "log")
    #     
    #     sapply(pages, function(page) if (page %not_in% r$user_accesses) shinyjs::hide(page))
    #     
    #     if ("data_sources" %not_in% r$user_accesses & "datamarts" %not_in% r$user_accesses & "studies" %not_in% r$user_accesses &
    #       "subsets" %not_in% r$user_accesses & "thesaurus" %not_in% r$user_accesses) shinyjs::hide("data_management")
    #     
    #     # if ("plugins" %not_in% r$user_accesses & "patient_lvl_modules" %not_in% r$user_accesses & "aggregated_modules" %not_in% r$user_accesses) shinyjs::hide("plugins_modules")
    #   })
    # }
  })
}