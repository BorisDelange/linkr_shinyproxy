#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny
#' @noRd

app_server <- function(router, language, db_info){
  function(input, output, session ) {
    
    # Create r reactive value
    r <- reactiveValues()
    
    # Connection to database
    # If connection informations have been given in cdwtools() function, use these informations
    
    r$local_db <- get_local_db()
    
    # Add distant db informations in local database
    
    observeEvent(r$local_db, {
      
      if (length(db_info) > 0){
        tryCatch({
          
          query <- DBI::dbSendStatement(r$local_db, "UPDATE options SET value = 'distant' WHERE category = 'distant_db' AND name = 'connection_type'")
          DBI::dbClearResult(query)
          
          # Insert each parameter of db_info
          sapply(names(db_info), function(name){
            query <- DBI::dbSendStatement(r$local_db, paste0("UPDATE options SET value = '", db_info[[name]], "' WHERE category = 'distant_db' AND name = '", name, "'"))
            DBI::dbClearResult(query)
          })
        },
        error = function(e) print(translate(language, "error_insert_distant_db_info")),
        warning = function(w) print(translate(language, "error_insert_distant_db_info")))
      }
    })
    
    r$db <- get_db(db_info = db_info, language = language)
    
    
    # Close DB connection on exit
    session$onSessionEnded(function() {
      observe(on.exit({
        add_log_entry(r = r, category = "Conncetion end", name = "Connection end", value = "")
        DBI::dbDisconnect(r$db)
      }))
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
        "aggregated_modules", "aggregated_modules_families", "aggregated_modules_elements",
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
      
    })

    
    # Secure the app with ShinyManager
    
    # r$res_auth <- shinymanager::secure_server(check_credentials = check_authentification(r$db))
    
    # Get user ID
    r$user_id <- 2
    # observeEvent(r$res_auth, {
    #   r$user_id <- reactiveValuesToList(r$res_auth)$id
    #   add_log_entry(r = r, category = "Connection start", name = "Connection start", value = "")
    # })
    
    
    # When r$user_id loaded, load user_accesses
    
    observeEvent(r$user_id, {
      user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id)
      
      # Get user accesses
      r$user_accesses <- r$options %>% dplyr::filter(category == "users_accesses" & link_id == user_access_id & value_num == 1) %>% dplyr::pull(name)

    })
    
    # Route pages
    router$server(input, output, session)
    
    # Load modules
    # Don't load modules user has no access to
    
    observeEvent(r$user_accesses, {
      
      ##########################################
      # Keep data user has access to          #
      ##########################################
      
      # Thesaurus & data_sources tables are visible for everybody
      
      # Access by options => user access list
      sapply(c("studies", "datamarts", "plugins"), function(table){
        if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
          if (nrow(r[[table]] > 0)){
            r[[table]] <- get_authorized_data(r = r, table = table)
            r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
          }
        }
      })
      sapply(c("patient_lvl_modules_families", "aggregated_modules_families"), function(table){
        
        if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
        if (grepl("aggregated", table)) prefix <- "aggregated_"
        
        if (paste0(prefix, "_modules_see_all_data") %not_in% r$user_accesses){
          if (nrow(r[[table]] > 0)){
            r[[table]] <- get_authorized_data(r = r, table = table)
            r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
          }
        }
      })
      
      # Access by parent
      
      if ("subsets_see_all_data" %not_in% r$user_accesses){
        if (nrow(r$subsets > 0)){
          studies_ids <- r$studies %>% dplyr::pull(id)
          r$subsets <- r$subsets %>% dplyr::filter(study_id %in% studies_ids)
          r$studies_temp <- r$studies %>% dplyr::mutate(modified = FALSE)
        }
      }
      
      sapply(c("patient_lvl_modules", "aggregated_modules", "patient_lvl_modules_elements", "aggregated_modules_elements"), function(table){
        
        if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
        if (grepl("aggregated", table)) prefix <- "aggregated_"
        
        if (paste0(prefix, "_modules_see_all_data") %not_in% r$user_accesses){
          modules_families_ids <- get_authorized_data(r = r, table = paste0(prefix, "modules_families")) %>% dplyr::pull(id)
          if (nrow(r[[paste0(prefix, "modules")]]) > 0) modules_ids <- r[[paste0(prefix, "modules")]] %>%
            dplyr::filter(module_family_id %in% modules_families_ids) %>% dplyr::pull(id)
          
          if (nrow(r[[table]] > 0)){
            if (grepl("modules$", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_family_id %in% modules_families_ids)
            if (grepl("modules_elements", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_id %in% modules_ids)
          }
          
          r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        }
      })
      
      ##########################################
      # Load server modules                    #
      ##########################################
      
      mod_page_header_server(input, output, session)
      
      sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_patient_and_aggregated_data_server(page, r, language)
        mod_page_sidenav_server(page, r, language)
      })
      
      mod_settings_general_server("settings_general_settings", r, language)
      mod_page_sidenav_server("settings_general_settings", r, language)
  
      if ("app_db" %in% r$user_accesses) mod_settings_app_database_server("settings_app_db", r, language)
      mod_page_sidenav_server("settings_app_db", r, language)
  
      if ("users" %in% r$user_accesses) mod_settings_users_server("settings_users", r, language)
      mod_page_sidenav_server("settings_users", r, language)
      
      sapply(c("users", "users_statuses", "users_accesses"), function(page){
        if ("users" %in% r$user_accesses) mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, language)
        if ("users" %in% r$user_accesses) mod_settings_users_server(paste0("settings_users_", page, "_management"), r, language)
        if ("users" %in% r$user_accesses & page == "users_accesses") mod_settings_users_server(paste0("settings_users_", page, "_options"), r, language)
      })
  
      if ("r_console" %in% r$user_accesses) mod_settings_r_console_server("settings_r_console", r, language)
      mod_page_sidenav_server("settings_r_console", r, language)
  
      sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page){
        if (page %in% r$user_accesses) mod_settings_data_management_server(paste0("settings_", page), r, language)
        mod_page_sidenav_server(paste0("settings_", page), r, language)
      })
  
      if ("plugins" %in% r$user_accesses) mod_settings_plugins_server("settings_plugins", r, language)
      mod_page_sidenav_server("settings_plugins", r, language)
  
      sapply(c("patient_lvl_modules", "aggregated_modules"), function(page){
        if (page %in% r$user_accesses) mod_settings_modules_server(paste0("settings_", page), r, language)
        mod_page_sidenav_server(paste0("settings_", page), r, language)
      })
      
      # Patient-lvl & aggregated modules page sub modules
      if ("patient_lvl_modules" %in% r$user_accesses | "aggregated_modules" %in% r$user_accesses){
        sapply(c("patient_lvl", "aggregated"), function(prefix){
          sapply(c("modules", "modules_families", "modules_elements"), function(page){
            mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_creation"), r, language)
            mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_management"), r, language)
            if (page == "modules_families") mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_options"), r, language)
          })
        })
      }
      
      if ("log" %in% r$user_accesses) mod_settings_log_server("settings_log", r, language)
      mod_page_sidenav_server("settings_log", r, language)
    })
    
  }
}