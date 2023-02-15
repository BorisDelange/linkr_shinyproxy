#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_sidenav_ui <- function(id = character(), i18n = R6::R6Class()){
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
            list(name = i18n$t("resources"), key = "home_resources", url = shiny.router::route_link("home/resources"))#,
            # list(name = i18n$t("dev"), key = "home_dev", url = shiny.router::route_link("home/dev"))
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
  
  if (id %in% c("my_studies", "my_subsets", "thesaurus", "scripts", "patient_level_data", "aggregated_data")){
    
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
  
  if (id == "my_subsets") div(class = "sidenav", dropdowns(c("datamart", "study", "subset"))) -> result
  
  # --- --- --- --
  # Thesaurus ----
  # --- --- --- --
  
  if (id == "thesaurus") div(class = "sidenav", dropdowns(c("datamart"))) -> result
  
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
      # dropdowns("patient_status", arrows = FALSE), br(),
      # div(id = ns("exclusion_reason_div"),
      #   div(class = "input_title", i18n$t("exclusion_reason")),
      #   div(shiny.fluent::ComboBox.shinyInput(ns("exclusion_reason"), value = NULL, options = list()))), br(),
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
  
  # --- --- -- -
  # Plugins ----
  # --- --- -- -
  
  if (grepl("^plugins", id)){
    div(class = "sidenav",
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = i18n$t("patient_level_data"),
              key = "patient_lvl", url = shiny.router::route_link("plugins/patient_lvl")),
            list(name = i18n$t("aggregated_data"),
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
  
  # --- --- --- -
  # Settings ----
  # --- --- --- -
  
  if (grepl("^settings", id)){
    
    links_data_management <- list()
    lapply(c("data_sources", "datamarts", "thesaurus"), function(page){
      links_data_management <<- rlist::list.append(links_data_management, list(name = i18n$t(page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links <- list()
    sapply(c("general_settings", "app_db", "users", "dev", "data_management", "log"), function(page){
      # Sub links for data management
      if (page == "data_management") links <<- rlist::list.append(links, list(name = i18n$t(page),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
      # Sub links for plugins & modules
      # else if (page == "plugins_modules") links <<- rlist::list.append(links, list(name = translate(language, page, words),
      #   id = ns(page), key = page, links = links_plugins_modules, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))
      
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
  m = shiny::reactiveValues(), i18n = R6::R6Class(), language = "EN"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (id %in% c("my_studies", "my_subsets", "thesaurus", "scripts", "patient_level_data", "aggregated_data")){
      
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
      
      # Show or hide main UI
      
      # sapply(c("datamart", "study", "subset"), function(name) observeEvent(input[[paste0(name, "_page")]], r[[paste0(name, "_page")]] <- Sys.time()))
      
      # --- --- --- --- -- -
      # Chosen datamart ----
      # --- --- --- --- -- -
      
      observeEvent(r$datamarts, {
        
        # Update dropdown
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", 
          options = convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
        
        sapply(c("study", "subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$datamart, {
        
        # Save value in r$chosen_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
        r$chosen_datamart <- input$datamart$key
        
        # Reset chosen_study variable
        m$chosen_study <- NA_integer_
        m$chosen_patient <- NA_integer_ # TO prevent bug when execute plugin code from plugin page
        # shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL)
        
        sapply(c("subset", "patient", "stay", "patient_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("study"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        # Update Dropdowns AFTER having executing datamart code (prevents a bug, where UI displays and disappears)
        
        # Reset data variables
        d$data_patient$stays <- tibble::tibble()
        d$data_patient$labs_vitals <- tibble::tibble()
        d$data_patient$text <- tibble::tibble()
        d$data_patient$orders <- tibble::tibble()
        d$data_patient$diagnoses <- tibble::tibble()
        
        d$data_stay$labs_vitals <- tibble::tibble()
        d$data_stay$text <- tibble::tibble()
        d$data_stay$orders <- tibble::tibble()
        d$data_stay$diagnoses <- tibble::tibble()
        
        d$data_subset$patients <- tibble::tibble()
        d$data_subset$stays <- tibble::tibble()
        d$data_subset$labs_vitals <- tibble::tibble()
        d$data_subset$test <- tibble::tibble()
        d$data_subset$orders <- tibble::tibble()
        d$data_subset$diagnoses <- tibble::tibble()
      })
      
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      observeEvent(r$chosen_datamart, {
        
        shiny.fluent::updateComboBox.shinyInput(session, "datamart", options = 
            convert_tibble_to_list(r$datamarts %>% dplyr::arrange(name), key_col = "id", text_col = "name"), 
          value = list(key = r$chosen_datamart))
        
        # Reset m$chosen_study (to reset main display)
        if (length(m$chosen_study) == 0) m$chosen_study <- NA_integer_
        if (!is.na(m$chosen_study)) m$chosen_study <- NA_integer_
        
        # Reset dropdowns & uiOutput
        # Hide exclusion_reason dropdown
        
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "stay", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$patient_info <- renderUI("")
        
        # r$datamart_page <- Sys.time()
        
      })
      
      # --- --- --- --- -
      # Chosen study ----
      # --- --- --- --- -
      
      observeEvent(r$studies, {
        if (nrow(r$studies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL, 
          errorMessage = i18n$t("no_study_available"))
        
        if (nrow(r$studies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "study",
          options = convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
      })
      
      observeEvent(input$study, {

        # Prevent multiple changes of m$chosen_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$chosen_study)) m$chosen_study <- input$study$key
        if (!is.na(m$chosen_study) & m$chosen_study != input$study$key) m$chosen_study <- input$study$key

        # Subsets depending on the chosen study
        update_r(r = r, m = m, table = "subsets")

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

        # Update subset dropdown
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
        
        # Load patients options
        sql <- glue::glue_sql("SELECT * FROM patients_options WHERE study_id = {m$chosen_study}", .con = m$db)
        m$patients_options <- DBI::dbGetQuery(m$db, sql)
      })
      
      observeEvent(m$chosen_study, {
        
        req(input$datamart$key & !is.na(m$chosen_study))
        studies <- r$studies %>% dplyr::filter(datamart_id == input$datamart$key)
        
        shiny.fluent::updateComboBox.shinyInput(session, "study", options =
            convert_tibble_to_list(studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
          value = list(key = m$chosen_study))
      })
      
      # --- --- --- --- --
      # Chosen subset ----
      # --- --- --- --- --
      
      observeEvent(input$subset, {
        
        m$chosen_subset <- input$subset$key
        
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
        update_r(r = r, m = m, table = "subsets_patients")
        
        # Select patients who belong to this subset
        update_r(r = r, m = m, table = "subset_patients")
        
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
          options = convert_tibble_to_list(data = patients %>% 
            dplyr::mutate(name_display = paste0(patient_id, " - ", gender)),
            # dplyr::mutate(name_display = dplyr::case_when(
            #   age > 2 ~ paste0(patient_id, " - ", gender, " - ", age, " ", i18n$t("years")),
            #   age <= 2 ~ paste0(patient_id, " - ", gender, " - ", round(age * 12, 0), " ", i18n$t("months"))
            #   )), 
            key_col = "patient_id", text_col = "name_display"))
        }
      })
      
      observeEvent(m$chosen_subset, {
        req(input$study$key)
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"),
          value = list(key = m$chosen_subset))
      })
      
      # --- --- --- --- ---
      # Chosen patient ----
      # --- --- --- --- ---
      
      observeEvent(input$patient, {
        
        m$chosen_patient <- input$patient$key
        
        if (nrow(d$stays %>% dplyr::filter(patient_id == input$patient$key)) == 0) shiny.fluent::updateComboBox.shinyInput(session, "patient", options = list(), value = NULL, errorMessage = i18n$t("no_patient_in_subset"))
        if (nrow(d$stays %>% dplyr::filter(patient_id == input$patient$key)) > 0){
          
          # thesaurus_name <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(thesaurus_name)
          # thesaurus_id <- r$thesaurus %>% dplyr::filter(name == thesaurus_name) %>% dplyr::pull(id)
          # 
          # sql <- glue::glue_sql("SELECT name, display_name FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND 
          # item_id = {d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(item_id)}", .con = r$db)
          # unit_name <- DBI::dbGetQuery(r$db, sql)
          # 
          # # sql <- glue::glue_sql("SELECT name, display_name FROM thesaurus_items_users WHERE thesaurus_id = {thesaurus_id} AND 
          # #   item_id = {d$stays %>% dplyr::filter(stay_id == m$chosen_stay %>% dplyr::pull(item_id)}", .con = r$db)
          # # unit_name_user <- 
          # 
          # unit_name <- unit_name %>% dplyr::mutate(name = dplyr::case_when(!is.na(display_name) ~ display_name, TRUE ~ name)) %>% dplyr::pull(name)
          
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
          
          # age <- d$patients %>% dplyr::filter(patient_id == m$chosen_patient) %>% dplyr::pull(age)
          # if (age > 2) age_div <- tagList(age, " ", i18n$t("years"))
          # else age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
          
          tagList(
            span(i18n$t("patient_id"), style = style), m$chosen_patient, br(),
            span(i18n$t("gender"), style = style), d$patients %>% dplyr::filter(patient_id == m$chosen_patient) %>% dplyr::pull(gender)
          )
        })
        
        sapply(c("stay", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        # Update patient status dropdown
        
        # subsets_names <- ""
        # if (nrow(r$subsets_patients) > 0){
        #   r$subsets_patients %>% 
        #     # Filter on patient_id
        #     dplyr::filter(patient_id == input$patient$key) %>%
        #     # Subsets from chosen study
        #     dplyr::inner_join(r$subsets %>% dplyr::select(subset_id = id, name), by = "subset_id") -> subsets
        #   
        #   if (nrow(subsets) > 0) subsets %>% dplyr::pull(name) -> subsets_names
        # }
        
        # subset_names = names of subsets from which patient belongs to
        # 
        # value <- "undefined"
        # if ("Included patients" %in% subsets_names | "Patients inclus" %in% subsets_names) value <- "included"
        # if ("Excluded patients" %in% subsets_names | "Patients exclus" %in% subsets_names) value <- "excluded"
        
        # Set to null to reload input$patient_status$key if the value is the same between to patients, to load exclusion_reason input
        # shiny.fluent::updateComboBox.shinyInput(session, "patient_status", value = NULL)
        # shiny.fluent::updateComboBox.shinyInput(session, "patient_status", options = list(
        #   list(key = "undefined", text = i18n$t("undefined")),
        #   list(key = "included", text = i18n$t("included")),
        #   list(key = "excluded", text = i18n$t("excluded"))),
        #   value = list(key = value))
        
        # Reset exclusion_reason dropdown & hide it
        # shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        # sapply(c("stay", "patient_status", "hr1", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        # shinyjs::hide("exclusion_reason_div")
      })
      
      # --- --- --- -- -
      # Chosen stay ----
      # --- --- --- -- -
      
      observeEvent(input$stay, {
        
        m$chosen_stay <- input$stay$key

        # Update patient informations on sidenav

        style <- "display:inline-block; width:100px; font-weight:bold;"
        
        age <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(age)
        if (age > 2) age_div <- tagList(age, " ", i18n$t("years"))
        else age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
        
        admission_datetime <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(admission_datetime)
        discharge_datetime <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(discharge_datetime)
        
        if (tolower(language) == "fr"){
          admission_datetime <- admission_datetime %>% format(format = "%d-%m-%Y %H:%M:%S")
          discharge_datetime <- discharge_datetime %>% format(format = "%d-%m-%Y %H:%M:%S")
        }
        
        thesaurus_name <- d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(thesaurus_name)
        thesaurus_id <- r$thesaurus %>% dplyr::filter(name == thesaurus_name) %>% dplyr::pull(id)
        
        sql <- glue::glue_sql("SELECT name, display_name FROM thesaurus_items WHERE thesaurus_id = {thesaurus_id} AND 
          item_id = {d$stays %>% dplyr::filter(stay_id == m$chosen_stay) %>% dplyr::pull(item_id)}", .con = r$db)
        unit_name <- DBI::dbGetQuery(r$db, sql)
        
        # sql <- glue::glue_sql("SELECT name, display_name FROM thesaurus_items_users WHERE thesaurus_id = {thesaurus_id} AND 
        #   item_id = {d$stays %>% dplyr::filter(stay_id == m$chosen_stay %>% dplyr::pull(item_id)}", .con = r$db)
        # unit_name_user <- 
        
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
      })
      
      # --- --- --- --- --- --- -
      # Study patient status ----
      # --- --- --- --- --- --- -
      
      # observeEvent(input$patient_status, {
      # 
      #   # Get included_patients & excluded_patients subset IDs
      # 
      #   included_patients_subset <- r$subsets %>%
      #     dplyr::filter(name %in% c("Included patients", "Patients inclus")) %>% dplyr::pull(id)
      #   excluded_patients_subset <- r$subsets %>%
      #     dplyr::filter(name %in% c("Excluded patients", "Patients exclus")) %>% dplyr::pull(id)
      # 
      #   add_patients_subset_id <- NA_integer_
      #   remove_patients_subset_id <- NA_integer_
      # 
      #   # Put patient to included subset
      #   if (input$patient_status$key == "included"){
      #     add_patients_subset_id <- included_patients_subset
      #     remove_patients_subset_id <- excluded_patients_subset
      #   }
      # 
      #   # Put patient to excluded subset
      #   if (input$patient_status$key == "excluded"){
      #     add_patients_subset_id <- excluded_patients_subset
      #     remove_patients_subset_id <- included_patients_subset
      #   }
      # 
      #   # Remove patient from both subsets (status undefined)
      #   if (input$patient_status$key == "undefined"){
      #     remove_patients_subset_id <- c(included_patients_subset, excluded_patients_subset)
      #   }
      # 
      #   # Add patients to chosen subset
      #   if (!is.na(add_patients_subset_id)){
      #     add_patients_to_subset(output = output, r = r, patients = tibble::tribble(~patient_id, as.integer(m$chosen_patient)),
      #       subset_id = add_patients_subset_id, success_notification = FALSE, language = language)
      #   }
      # 
      #   # Remove patients from chosen subset
      #   sapply(remove_patients_subset_id, function(subset_id){
      #     remove_patients_from_subset(output = output, r = r, patients = tibble::tribble(~patient_id, as.integer(m$chosen_patient)),
      #       subset_id = subset_id, language = language)
      #   })
      # 
      #   # Reload r$subset_patients & r$subsets_patients
      #   update_r(r = r, m = m, table = "subset_patients")
      #   update_r(r = r, m = m, table = "subsets_patients")
      # 
      #   # If choice is excluded, update exclusion reason dropdown & show dropdown
      #   if (input$patient_status$key == "excluded"){
      # 
      #     sql <- glue::glue_sql(paste0("SELECT id, value FROM modules_elements_options WHERE deleted IS FALSE AND category = 'aggregated' AND name = 'exclusion_reason_name'
      #       AND study_id = {m$chosen_study}"), .con = r$db)
      #     exclusion_reasons <- DBI::dbGetQuery(r$db, sql) %>% dplyr::arrange(value)
      # 
      #     options <- list()
      #     if (nrow(exclusion_reasons) > 0) options <- convert_tibble_to_list(data = exclusion_reasons, key_col = "id", text_col = "value", words = r$words)
      # 
      #     value <- m$patients_options %>% dplyr::filter(category == "exclusion_reason" & study_id == m$chosen_study & patient_id == m$chosen_patient)
      #     if (nrow(value) == 0) value <- NULL
      #     if (length(value) > 0) value <- value %>% dplyr::pull(value_num)
      # 
      #     shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = options, value = list(key = value))
      #     shinyjs::show("exclusion_reason_div")
      #   }
      #   else shinyjs::hide("exclusion_reason_div")
      # 
      # })
      # 
      # observeEvent(input$exclusion_reason, {
      #   
      #   if (length(input$exclusion_reason$key) > 0){
      # 
      #     # If already a row, get its ID
      #     id <- DBI::dbGetQuery(r$db, paste0("SELECT id FROM patients_options
      #     WHERE category = 'exclusion_reason' AND study_id = ", m$chosen_study, " AND patient_id = ", m$chosen_patient))
      # 
      #     last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM patients_options") %>% dplyr::pull())
      # 
      #     if (nrow(id) > 0) query <- DBI::dbSendStatement(r$db, paste0("UPDATE patients_options SET value_num = ", as.integer(input$exclusion_reason$key),
      #       ", creator_id = ", r$user_id, ", datetime = '", as.character(Sys.time()), "' WHERE id = ", id))
      # 
      #     else query <- DBI::dbSendStatement(r$db, paste0("INSERT INTO patients_options(id, study_id, patient_id, category, value_num, creator_id, datetime, deleted)
      #       SELECT ", last_row + 1, ", ", m$chosen_study, ", ", m$chosen_patient, ", 'exclusion_reason', ", as.integer(input$exclusion_reason$key),
      #       ", ", r$user_id, ", '", as.character(Sys.time()), "', FALSE"))
      # 
      #     DBI::dbClearResult(query)
      #   }
      # 
      #   update_r(r = r, m = m, table = "patients_options")
      # })

    }
  })
}
