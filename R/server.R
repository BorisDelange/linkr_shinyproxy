#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(router, language){
  function(input, output, session ) {
    
    # Create r reactive value
    r <- reactiveValues()
    
    # Get user ID
    r$user_id <- 1
    
    # Connection to database
    r$local_db <- get_local_db()
    r$db <- get_db()
    
    # Close DB connection on exit
    session$onSessionEnded(function() {
      observe(on.exit(DBI::dbDisconnect(r$db)))
    })

    # Add default values in database if database is empty
    # Load all data from database
    # Don't load thesaurus_items, load it only when a thesaurus is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      # Add default values in database, if it is empty
      insert_default_values(r = r)
      
      tables <- c(
        "users_accesses", "users_statuses",
        "data_sources", "datamarts", "studies", "subsets", "subset_patients", "thesaurus",
        "plugins", 
        "patient_lvl_modules", "patient_lvl_modules_families", "patient_lvl_modules_elements",
        "aggregated_modules", "aggregated_modules_families",
        "code", 
        "options", "plugins_options", "patients_options")
      
      sapply(tables, function(table){
        r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      })
      
      # For users table, don't load passwords
      r$users <- DBI::dbGetQuery(r$db, "SELECT id, username, firstname, lastname, user_access_id, user_status_id, datetime, deleted
        FROM users WHERE deleted IS FALSE ORDER BY id")
      r$users_temp <- r$users %>% dplyr::mutate(modified = FALSE)
      
      # Add a module_types variable, for settings/plugins dropdown
      r$module_types <- tibble::tribble(~id, ~name, 1, translate(language, "patient_level_data"), 2, translate(language, "aggregated_data"))
      
      r$result <- list()
    })
    
    # When r$user_id loaded, load user_accesses
    
    observeEvent(r$user_id, {
      user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id)
      
      # Get user accesses
      r$user_accesses <- r$options %>% dplyr::filter(category == "users_accesses" & link_id == user_access_id & value_num == 1) %>% dplyr::pull(name)

    })
    
    # Route pages
    router$server(input, output, session)
    
    # Load modules
    sapply(c("patient_level_data", "aggregated_data"), function(page){
      mod_patient_and_aggregated_data_server(page, r, language)
      mod_page_sidenav_server(page, r, language)
    })
    
    mod_settings_general_server("settings_general_settings", r, language)
    mod_page_sidenav_server("settings_general_settings", r, language)

    mod_settings_app_database_server("settings_app_db", r, language)
    mod_page_sidenav_server("settings_app_db", r, language)

    mod_settings_users_server("settings_users", r, language)
    mod_page_sidenav_server("settings_users", r, language)
    sapply(c("users", "users_statuses", "users_accesses"), function(page){
      mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, language)
      mod_settings_users_server(paste0("settings_users_", page, "_management"), r, language)
      if (page == "users_accesses") mod_settings_users_server(paste0("settings_users_", page, "_options"), r, language)
    })

    mod_settings_r_console_server("settings_r_console", r, language)
    mod_page_sidenav_server("settings_r_console", r, language)

    sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page){
      mod_settings_data_management_server(paste0("settings_", page), r, language)
      mod_page_sidenav_server(paste0("settings_", page), r, language)
    })

    mod_settings_plugins_server("settings_plugins", r, language)
    mod_page_sidenav_server("settings_plugins", r, language)

    sapply(c("modules_patient_lvl", "modules_aggregated"), function(page){
      mod_settings_modules_server(paste0("settings_", page), r, language)
      mod_page_sidenav_server(paste0("settings_", page), r, language)
    })
    
    # Patient-lvl & aggregated modules page sub modules
    sapply(c("patient_lvl", "aggregated"), function(prefix){
      sapply(c("modules", "modules_families", "modules_elements"), function(page){
        mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_creation"), r, language)
        mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_management"), r, language)
        if (page == "modules_families") mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_options"), r, language)
      })
    })
    
  }
}