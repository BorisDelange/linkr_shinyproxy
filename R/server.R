#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny
#' @noRd

app_server <- function(router, language = "EN", db_info = list(), datamarts_folder = character(), app_db_folder = character(),
  initial_wd = character()){
  function(input, output, session ) {
    
    # Create r reactive value
    r <- reactiveValues()
    
    # Create an agg reactive value, to communicate between aggregated plugins
    agg <- reactiveValues()
    
    # Create r$server_plugins_loaded
    r$server_plugins_loaded <- ""
    
    # Save datamarts_folder in r variable
    r$datamarts_folder <- datamarts_folder
    
    # Get translations
    r$words <- get_translations()
    
    # Save currently opened toggles (used to reload cards when we load a page, restart reactivity)
    r$activated_toggles <- ""
    
    # Connection to database
    # If connection informations have been given in cdwtools() function, use these informations
    
    r$local_db <- get_local_db(app_db_folder = app_db_folder)
    
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
    
    r$db <- get_db(db_info = db_info, app_db_folder = app_db_folder, language = language)
    
    
    # Close DB connection on exit
    # And restore initial working directory
    onStop(function() {
      add_log_entry(r = isolate(r), category = "Connection ends", name = "Connection ends", value = "")
      DBI::dbDisconnect(isolate(r$db))
      setwd(initial_wd)
    })
    
    # Add default values in database if database is empty
    # Load all data from database
    # Don't load thesaurus_items, load it only when a thesaurus is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      # Add default values in database, if it is empty
      insert_default_values(output = output, r = r)
      
      # Load database
      load_database(r = r, language = language)
      
    })

    
    # Secure the app with ShinyManager
    
    r$res_auth <- shinymanager::secure_server(check_credentials = check_authentification(r$db))
    
    # Get user ID
    
    observeEvent(r$res_auth, {
      r$user_id <- as.integer(reactiveValuesToList(r$res_auth)$id)
      add_log_entry(r = r, category = "Connection starts", name = "Connection starts", value = "")
    })
    
    
    # When r$user_id loaded, load user_accesses
    
    observeEvent(r$user_id, {
      req(r$user_id)
      
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
          r$subsets_temp <- r$subsets %>% dplyr::mutate(modified = FALSE)
        }
      }
      
      sapply(c("patient_lvl_modules", "aggregated_modules", "patient_lvl_modules_elements", "aggregated_modules_elements"), function(table){
        
        if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
        if (grepl("aggregated", table)) prefix <- "aggregated_"
        
        if (paste0(prefix, "_modules_see_all_data") %not_in% r$user_accesses){
          modules_families_ids <- get_authorized_data(r = r, table = paste0(prefix, "modules_families")) %>% dplyr::pull(id)
          if (nrow(r[[paste0(prefix, "modules")]]) > 0) modules_ids <- r[[paste0(prefix, "modules")]] %>%
            dplyr::filter(module_family_id %in% modules_families_ids) %>% dplyr::pull(id)
          
          if (nrow(r[[table]]) > 0){
            if (grepl("modules$", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_family_id %in% modules_families_ids)
            if (grepl("modules_elements", table)) r[[table]] <- r[[table]] %>% dplyr::filter(module_id %in% modules_ids)
          }
          
          r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        }
      })
      
      ##########################################
      # Load server modules                    #
      ##########################################
      
      sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_patient_and_aggregated_data_server(page, r, language, r$words)
        mod_page_sidenav_server(page, r, language, r$words)
      })
      
      mod_settings_general_server("settings_general_settings", r, language, r$words)
      mod_page_sidenav_server("settings_general_settings", r, language, r$words)
  
      if ("app_db" %in% r$user_accesses) mod_settings_app_database_server("settings_app_db", r, language, r$words)
      mod_page_sidenav_server("settings_app_db", r, language, r$words)
  
      if ("users" %in% r$user_accesses) mod_settings_users_server("settings_users", r, language, r$words)
      mod_page_sidenav_server("settings_users", r, language, r$words)
      
      sapply(c("users", "users_statuses", "users_accesses"), function(page){
        if ("users" %in% r$user_accesses) mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, language, r$words)
        if ("users" %in% r$user_accesses) mod_settings_users_server(paste0("settings_users_", page, "_management"), r, language, r$words)
        if ("users" %in% r$user_accesses & page == "users_accesses") mod_settings_users_server(paste0("settings_users_", page, "_options"), r, language, r$words)
      })
  
      if ("r_console" %in% r$user_accesses) mod_settings_r_console_server("settings_r_console", r, language, r$words)
      mod_page_sidenav_server("settings_r_console", r, language, r$words)
  
      sapply(c("data_sources", "datamarts", "studies", "subsets", "thesaurus"), function(page){
        if (page %in% r$user_accesses) mod_settings_data_management_server(paste0("settings_", page), r, language, r$words)
        mod_page_sidenav_server(paste0("settings_", page), r, language, r$words)
      })
  
      if ("plugins" %in% r$user_accesses) mod_settings_plugins_server("settings_plugins", r, language, r$words)
      mod_page_sidenav_server("settings_plugins", r, language, r$words)
  
      sapply(c("patient_lvl_modules", "aggregated_modules"), function(page){
        if (page %in% r$user_accesses) mod_settings_modules_server(paste0("settings_", page), r, language, r$words)
        mod_page_sidenav_server(paste0("settings_", page), r, language, r$words)
      })
      
      # Patient-lvl & aggregated modules page sub modules
      if ("patient_lvl_modules" %in% r$user_accesses | "aggregated_modules" %in% r$user_accesses){
        sapply(c("patient_lvl", "aggregated"), function(prefix){
          sapply(c("modules", "modules_families", "modules_elements"), function(page){
            mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_creation"), r, language, r$words)
            mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_management"), r, language, r$words)
            if (page == "modules_families") mod_settings_modules_server(paste0("settings_modules_", prefix, "_", page, "_options"), r, language, r$words)
          })
        })
      }
      
      if ("log" %in% r$user_accesses) mod_settings_log_server("settings_log", r, language, r$words)
      mod_page_sidenav_server("settings_log", r, language, r$words)
      
    })
    
  }
}