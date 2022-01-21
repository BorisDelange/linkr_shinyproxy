#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_patient_and_aggregated_data_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
  ns <- NS(id)
  result <- ""
  
  # Prefix depending on page id
  if (id == "patient_level_data") prefix <- "patient_lvl"
  if (id == "aggregated_data") prefix <- "aggregated"
  
  div(class = "main",
      render_settings_default_elements(ns = ns),
      shinyjs::hidden(div(id = ns("datamart_main"), mod_patient_and_aggregated_data_datamart_ui(id = paste0(id, "_datamart"), language = language, words = words))),
      shinyjs::hidden(div(id = ns("study_main"), mod_patient_and_aggregated_data_study_ui(id = paste0(id, "_study"), language = language, words = words))),
      shinyjs::hidden(div(id = ns("subset_main"), mod_patient_and_aggregated_data_subsets_ui(id = paste0(id, "_subsets"), language = language, words = words)))
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 

mod_patient_and_aggregated_data_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data") prefix <- "patient_lvl"
    if (id == "aggregated_data") prefix <- "aggregated"
    
    ##########################################
    # Display distinct pages                 #
    ##########################################
    
    # When arrow button is clicked
    pages <- c("datamart", "study", "subset")
    
    sapply(pages, function(page){
      
      observeEvent(r[[paste0(page, "_page")]], {
        
        req(!is.na(r[[paste0("chosen_", page)]]))
        shinyjs::show(paste0(page, "_main"))
        sapply(paste0(pages %>% setdiff(., page), "_main"), shinyjs::hide)
      })
    })
    
    ##########################################
    # LOAD DATA                              #
    ##########################################
    
    if (prefix == "patient_lvl"){
      
      observeEvent(r$chosen_patient, {
        
        r$data_patient <- list()
        
        # Reset variables
        r$data_patient$stays <- tibble::tibble()
        r$data_patient$labs_vitals <- tibble::tibble()
        r$data_patient$text <- tibble::tibble()
        r$data_patient$orders <- tibble::tibble()
        r$data_stay$labs_vitals_stay <- tibble::tibble()
        r$data_stay$text_stay <- tibble::tibble()
        r$data_stay$orders_stay <- tibble::tibble()
        
        if (length(r$chosen_patient) > 0){
          if (!is.na(r$chosen_patient) & r$chosen_patient != ""){
            if (nrow(r$stays) > 0) r$data_patient$stays <- r$stays %>% dplyr::filter(patient_id == r$chosen_patient) %>% dplyr::arrange(admission_datetime)
            if (nrow(r$labs_vitals) > 0) r$data_patient$labs_vitals <- r$labs_vitals %>% dplyr::filter(patient_id == r$chosen_patient)
            if (nrow(r$text) > 0) r$data_patient$text <- r$text %>% dplyr::filter(patient_id == r$chosen_patient)
            if (nrow(r$orders) > 0) r$data_patient$orders <- r$orders %>% dplyr::filter(patient_id == r$chosen_patient)
          }
        }
      })
      
      observeEvent(r$chosen_stay, {
        
        req(r$data_patient)
        
        r$data_stay <- list()
        
        if (length(r$chosen_stay) > 0){
          if (!is.na(r$chosen_stay) & r$chosen_stay != ""){
            
            r$data_stay$stay <- r$data_patient$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::select(admission_datetime, discharge_datetime)
            
            if (nrow(r$data_patient$labs_vitals) > 0) r$data_stay$labs_vitals_stay <- r$data_patient$labs_vitals %>% dplyr::filter(datetime_start >= r$data_stay$stay$admission_datetime & datetime_start <= r$data_stay$stay$discharge_datetime)
            if (nrow(r$data_patient$text) > 0) r$data_stay$text_stay <- r$data_patient$text %>% dplyr::filter(datetime_start >= r$data_stay$stay$admission_datetime & datetime_start <= r$data_stay$stay$discharge_datetime)
            if (nrow(r$data_patient$orders) > 0) r$data_stay$orders_stay <- r$data_patient$orders %>% dplyr::filter(datetime_start >= r$data_stay$stay$admission_datetime & datetime_start <= r$data_stay$stay$discharge_datetime)
          }
        }
      })
    }
    
    if (prefix == "aggregated"){
      
      observeEvent(r$chosen_subset, {
        
        r$data_subset <- list()
        patients <- tibble::tibble()
        
        if (length(r$chosen_subset) > 0){
          if (!is.na(r$chosen_subset) & r$chosen_subset != "") patients <- r$subset_patients %>% dplyr::filter(subset_id == r$chosen_subset)
        }
        
        if (nrow(patients) > 0){
          patients <- patients %>% dplyr::select(patient_id)
          if (nrow(r$patients) > 0) r$data_subset$patients <- r$patients %>% dplyr::inner_join(patients, by = "patient_id")
          if (nrow(r$stays) > 0) r$data_subset$stays <- r$stays %>% dplyr::inner_join(patients, by = "patient_id")
          if (nrow(r$labs_vitals) > 0) r$data_subset$labs_vitals <- r$labs_vitals %>% dplyr::inner_join(patients, by = "patient_id")
          if (nrow(r$text) > 0) r$data_subset$text <- r$text %>% dplyr::inner_join(patients, by = "patient_id")
          if (nrow(r$orders) > 0) r$data_subset$orders <- r$orders %>% dplyr::inner_join(patients, by = "patient_id")
        }
      })
    }
    
  })
}