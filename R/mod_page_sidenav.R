#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_sidenav_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  
  # --- --- -
  # Home ----
  # --- --- -
  
  if (grepl("^home", id)){
    div(class = "sidenav",
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = i18n$t("home"), key = "home", url = shiny.router::route_link("home")),
            list(name = i18n$t("get_started"), key = "home_get_started", url = shiny.router::route_link("home/get_started")),
            list(name = i18n$t("tutorials"), key = "home_tutorials", url = shiny.router::route_link("home/tutorials")),
            list(name = i18n$t("resources"), key = "home_resources", url = shiny.router::route_link("home/resources"))
            )
          )
        ),
        initialSelectedKey = id,
        selectedKey = id,
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

  
  # --- --- --- --
  # Dropdowns ----
  # --- --- --- --
  
  if (id %in% c("my_studies", "my_subsets", "vocabularies", "scripts", "patient_level_data", "aggregated_data")){
    
    dropdowns <- function(names){
      
      result <- tagList()
      
      sapply(names, function(name){
        
        width <- "250px"
        
        result <<- tagList(result,
          div(id = ns(paste0(name, "_title")), class = "input_title", i18n$t(name)),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
            div(shiny.fluent::ComboBox.shinyInput(ns(name), allowFreeForm = FALSE, autoComplete = "on"), style = paste0("min-width:", width, "; max-width:", width, ";"))
          )
        )
      })
      
      result
    }
  }
  
  # --- --- --- ---
  # My studies ----
  # --- --- --- ---
  
  if (id == "my_studies") div(class = "sidenav", dropdowns(c("datamart", "study"))) -> result
  
  # --- --- --- ---
  # My subsets ----
  # --- --- --- ---
  
  if (id == "my_subsets") div(class = "sidenav", dropdowns(c("datamart", "study"))) -> result
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  if (id == "vocabularies") div(class = "sidenav", dropdowns(c("datamart"))) -> result
  
  # --- --- -- -
  # Scripts ----
  # --- --- -- -
  
  if (id == "scripts") div(class = "sidenav", dropdowns(c("datamart"))) -> result
  
  # --- --- --- --- --- ---
  # Patient-level data ----
  # --- --- --- --- --- ---
  
  if (id == "patient_level_data"){
    div(class = "sidenav",
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::PrimaryButton.shinyInput(ns("data_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::DefaultButton.shinyInput(ns("data_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      ),
      dropdowns(c("datamart", "study", "subset")),
      br(), div(id = ns("hr1"), hr()),
      dropdowns(c("patient", "stay")),
      br(), div(id = ns("hr2"), hr()),
      uiOutput(ns("patient_info"))
    ) -> result
  }
  
  # --- --- --- --- -- -
  # Aggregated data ----
  # --- --- --- --- -- -
  
  if (id == "aggregated_data") div(
    class = "sidenav", 
    div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
    div(
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
        shiny.fluent::DefaultButton.shinyInput(ns("data_page_ind"), i18n$t("individual"), style = "width:125px;"), 
        shiny.fluent::PrimaryButton.shinyInput(ns("data_page_agg"), i18n$t("aggregated"), style = "width:125px;")
      ), style = "width:250px;"
    ),                               
    dropdowns(c("datamart", "study", "subset"))
  ) -> result
  
  # --- --- --- --- --- --- --
  # Plugins patient-level ----
  # --- --- --- --- --- --- --
  
  if (id == "plugins_patient_lvl"){
    
    div(
      class = "sidenav", 
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::PrimaryButton.shinyInput(ns("plugins_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::DefaultButton.shinyInput(ns("plugins_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      )
    ) -> result
  }
  
  # --- --- --- --- --- ---
  # Plugins aggregated ----
  # --- --- --- --- --- ---
  
  if (id == "plugins_aggregated"){
    
    div(
      class = "sidenav", 
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::DefaultButton.shinyInput(ns("plugins_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::PrimaryButton.shinyInput(ns("plugins_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      )
    ) -> result
  }
  
  # --- --- --- -
  # Settings ----
  # --- --- --- -
  
  if (grepl("^settings", id)){
    
    links_data_management <- list()
    lapply(c("data_sources", "datamarts", "vocabularies"), function(page){
      links_data_management <<- rlist::list.append(links_data_management, list(name = i18n$t(page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links <- list()
    sapply(c("general_settings", "app_db", "users", "dev", "data_management", "log"), function(page){
      # Sub links for data management
      if (page == "data_management") links <<- rlist::list.append(links, list(name = i18n$t(page),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))

      # No sub links
      else links <<- rlist::list.append(links, list(name = i18n$t(page),
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
mod_page_sidenav_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), 
  m = shiny::reactiveValues(), i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) print(paste0(Sys.time(), " - mod_plugins - start"))
    
    # --- --- -- -
    # Plugins ----
    # --- --- -- -
    
    if (id %in% c("plugins_patient_lvl", "plugins_aggregated")){
      
      # Changing page between patient-lvl & aggregated plugins
      
      r$plugins_page <- "plugins_patient_lvl"
      
      if (id == "plugins_patient_lvl") observeEvent(input$plugins_page_agg, {
        shiny.router::change_page("plugins_aggregated")
        r$plugins_page <- "plugins_aggregated"
      })
      if (id == "plugins_aggregated") observeEvent(input$plugins_page_ind, {
        shiny.router::change_page("plugins_patient_lvl")
        r$plugins_page <- "plugins_patient_lvl"
      })
    }
    
    if (id %in% c("my_studies", "my_subsets", "vocabularies", "scripts", "patient_level_data", "aggregated_data")){
      
      # --- --- --- --- --- --- --- --- -
      # Patient-lvl & agregated data ----
      # --- --- --- --- --- --- --- --- -
      
      # Changing page between patient-lvl & aggregated data
      
      r$data_page <- "patient_level_data"
      
      if (id == "patient_level_data") observeEvent(input$data_page_agg, {
        shiny.router::change_page("aggregated_data")
        r$data_page <- "aggregated_data"
      })
      if (id == "aggregated_data") observeEvent(input$data_page_ind, {
        shiny.router::change_page("patient_level_data")
        r$data_page <- "patient_level_data"
      })
      
      # --- --- --- --- -- -
      # Chosen datamart ----
      # --- --- --- --- -- -
      
      observeEvent(r$datamarts, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$datamarts"))
        
        # Update dropdown
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", 
          options = convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
        
        sapply(c("study", "subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$datamart, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$datamart"))
        
        # Save value in r$chosen_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
        r$chosen_datamart <- input$datamart$key
        
        sapply(c("subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("study"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        # Reset dropdowns & uiOutput
        # Hide exclusion_reason dropdown
        
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
      })
      
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      observeEvent(r$chosen_datamart, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$chosen_datamart"))
        
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", options = 
          convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name"), 
            value = list(key = r$chosen_datamart))
        
        # Reset m$chosen_study (to reset main display)
        if (length(m$chosen_study) == 0) m$chosen_study <- NA_integer_
        if (!is.na(m$chosen_study)) m$chosen_study <- NA_integer_
        
        # Reset of data variables, load of vocabulary code happens in mod_my_studies.R
        # With this solution, code is run only one time
        # Here, code is run for each page
      })
      
      # --- --- --- --- -
      # Chosen study ----
      # --- --- --- --- -
      
      observeEvent(r$studies, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$studies"))
        
        if (nrow(r$studies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL, 
          errorMessage = i18n$t("no_study_available"))
        
        if (nrow(r$studies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "study",
          options = convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
      })
      
      observeEvent(input$study, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$study"))

        req(input$study$key)
        
        # Prevent multiple changes of m$chosen_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$chosen_study)) m$chosen_study <- input$study$key
        if (!is.na(m$chosen_study) & m$chosen_study != input$study$key) m$chosen_study <- input$study$key

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
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$study"))
      })
      
      observeEvent(m$chosen_study, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$chosen_study"))
        
        req(input$datamart$key & !is.na(m$chosen_study))
        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart$key)
        
        shiny.fluent::updateComboBox.shinyInput(session, "study", options =
            convert_tibble_to_list(studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
          value = list(key = m$chosen_study))
        
        # Load of subsets is done in mod_my_studies.R
        # With this solution, code is run only one time
      })
      
      observeEvent(m$subsets, {
        req(!is.na(m$chosen_study))
        
        # Update subset dropdown
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
      })
      
      # --- --- --- --- --
      # Chosen subset ----
      # --- --- --- --- --
      
      observeEvent(m$subsets, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$subsets"))
        
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, 
          errorMessage = i18n$t("no_study_available"))
        
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset",
          options = convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
      })
      
      observeEvent(input$subset, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$subset"))
        
        req(input$subset$key)
        
        # Prevent multiple changes of m$chosen_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$chosen_subset)) m$chosen_subset <- input$subset$key
        if (!is.na(m$chosen_subset) & m$chosen_subset != input$subset$key) m$chosen_subset <- input$subset$key

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
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$subset"))
      })
      
      observeEvent(m$chosen_subset, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$chosen_datamart"))
        
        req(input$study$key)
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"),
          value = list(key = m$chosen_subset))
      })
      
      observeEvent(m$subset_patients, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$subset_patients"))
        
        patients <- tibble::tibble()
        
        if (nrow(m$subset_patients) > 0 & nrow(d$patients) > 0){
          patients <- d$patients %>% dplyr::inner_join(m$subset_patients %>% dplyr::select(patient_id), by = "patient_id")
        }
        
        if (nrow(patients) == 0){
          # Set chosen_patient to NA, not to display a chart when no patient is chosen
          m$chosen_patient <- NA_integer_
          shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = i18n$t("no_patient_in_subset")) 
        }
        
        if (nrow(patients) > 0){
          # Order patients by patient_id
          patients <- patients %>% dplyr::arrange(patient_id)
          
          # Update patients dropdown
          shiny.fluent::updateComboBox.shinyInput(session, "patient", 
            options = convert_tibble_to_list(data = patients %>% dplyr::mutate(name_display = paste0(patient_id, " - ", gender)),
              key_col = "patient_id", text_col = "name_display"))
        }
        
      })
      
      # --- --- --- --- ---
      # Chosen patient ----
      # --- --- --- --- ---
      
      observeEvent(input$patient, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$patient"))
        
        m$chosen_patient <- input$patient$key
        
        if (nrow(d$stays %>% dplyr::filter(patient_id == input$patient$key)) == 0) shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = i18n$t("no_patient_in_subset"))
        if (nrow(d$stays %>% dplyr::filter(patient_id == input$patient$key)) > 0){
          
          if (tolower(language) == "fr") stays <- convert_tibble_to_list(data = d$stays %>% dplyr::filter(patient_id == input$patient$key) %>% dplyr::mutate(name_display = paste0("Unit...", " - ", #paste0(unit_name, " - ", 
              format(as.POSIXct(admission_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(discharge_datetime), format = "%d-%m-%Y"))),
              key_col = "stay_id", text_col = "name_display")
          else stays <- convert_tibble_to_list(data = d$stays %>% dplyr::filter(patient_id == input$patient$key) %>% dplyr::mutate(name_display = paste0("Unit...", " - ", #paste0(unit_name, " - ", 
            format(as.POSIXct(admission_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(discharge_datetime), format = "%Y-%m-%d"))),
            key_col = "stay_id", text_col = "name_display")
          
          # Load stays of the patient & update dropdown
          shiny.fluent::updateComboBox.shinyInput(session, "stay", options = stays, value = NULL)
        }
        
        # Update patient informations on sidenav
        
        style <- "display:inline-block; width:100px; font-weight:bold;"
        output$patient_info <- renderUI({
          
          tagList(
            span(i18n$t("patient_id"), style = style), m$chosen_patient, br(),
            span(i18n$t("gender"), style = style), d$patients %>% dplyr::filter(patient_id == m$chosen_patient) %>% dplyr::pull(gender)
          )
        })
        
        sapply(c("stay", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$patient"))
      })
      
      # --- --- --- -- -
      # Chosen stay ----
      # --- --- --- -- -
      
      observeEvent(input$stay, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$stay"))
        
        m$chosen_stay <- input$stay$key

        # Update patient informations on sidenav

        style <- "display:inline-block; width:100px; font-weight:bold;"
        
        age <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(age)
        if (age > 2) age_div <- tagList(age, " ", i18n$t("years"))
        else age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
        
        admission_datetime <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(admission_datetime) %>% format_datetime(language)
        discharge_datetime <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(discharge_datetime) %>% format_datetime(language)
        
        thesaurus_name <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(thesaurus_name)
        thesaurus_id <- r$thesaurus %>% dplyr::filter(name == thesaurus_name) %>% dplyr::pull(id)
        
        sql <- glue::glue_sql("SELECT name, display_name FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND 
          item_id = {d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(item_id)}", .con = r$db)
        unit_name <- DBI::dbGetQuery(r$db, sql)
        
        unit_name <- unit_name %>% dplyr::mutate(name = dplyr::case_when(!is.na(display_name) ~ display_name, TRUE ~ name)) %>% dplyr::pull(name)
        
        output$patient_info <- renderUI({
          tagList(
            span(i18n$t("patient_id"), style = style), m$chosen_patient, br(),
            span(i18n$t("gender"), style = style), d$patients %>% dplyr::filter(patient_id == m$chosen_patient) %>% dplyr::pull(gender), br(), br(),
            span(i18n$t("stay_id"), style = style), m$chosen_stay, br(),
            span(i18n$t("age"), style = style), age_div, br(),
            span(i18n$t("hosp_unit"), style = style), unit_name, br(),
            span(i18n$t("from"), style = style), admission_datetime, br(),
            span(i18n$t("to"), style = style), discharge_datetime)
        })
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$stay"))
      })
    }
  })
}
