#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(router, language){
  function( input, output, session ) {
    
    r <- reactiveValues()
    
    r$user_id <- 1
    r$local_db <- get_local_db()
    r$db <- get_db()
    
    # Close DB connection on exit
    session$onSessionEnded(function() {
      observe(on.exit(DBI::dbDisconnect(r$db)))
    })
    
    # Load all data from database
    # Don't load thesaurus_items, load it only when a thesaurus is selected
    observeEvent(r$db, {
      tables <- c("users", "users_accesses_statuses", "users_accesses_details",
        "data_sources", "datamarts", "studies", "subsets", "subset_patients", "thesaurus",
        "plugins", "patient_lvl_module_families", "patient_lvl_modules", "patient_lvl_module_elements",
        "aggregated_module_families", "aggregated_modules", #"aggregated_module_elements",
        "code", "options")
      
      sapply(tables, function(table){
        r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      })
      
      # Add a module_types variable, for settings/plugins dropdown
      r$module_types <- tibble::tribble(~id, ~name, 1, translate(language, "patient_level_data"), 2, translate(language, "aggregated_data"))
      
      r$result <- list()
    })
    
    router$server(input, output, session)
    
    mod_page_sidenav_server(paste0("patient_level_data"), r, language)
    mod_page_sidenav_server(paste0("aggregated_data"), r, language)
    
    mod_patient_and_aggregated_data_server("patient_lvl_data", r, language)
    mod_patient_and_aggregated_data_server("aggregated_data", r, language)
    mod_settings_app_database_server("settings_app_database", r, language)
    mod_settings_users_server("settings_users", r, language)
    mod_settings_r_console_server("settings_r_console", r, language)
    mod_settings_data_management_server("settings_data_sources", r, language)
    mod_settings_data_management_server("settings_datamarts", r, language)
    mod_settings_data_management_server("settings_studies", r, language)
    mod_settings_data_management_server("settings_subsets", r, language)
    mod_settings_data_management_server("settings_thesaurus", r, language)
    mod_settings_plugins_server("settings_plugins", r, language)
    mod_settings_modules_server("settings_patient_lvl_modules", r, language)
    mod_settings_modules_server("settings_aggregated_modules", r, language)
  }
}