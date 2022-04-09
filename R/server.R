#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny
#' @noRd

app_server <- function(router, language = "EN", db_info = list(), datamarts_folder = character(), app_db_folder = character(),
  #initial_wd = character(), 
  perf_monitoring = FALSE){
  function(input, output, session ) {
    
    # Create r reactive value
    r <- reactiveValues()
    
    # If perf_monotoring activated
    r$perf_monitoring <- perf_monitoring
    
    # Create r$server_modules_groups_loaded & r$ui_modules_groups_loaded
    r$server_modules_groups_loaded <- ""
    r$ui_modules_groups_loaded <- ""
    
    # Save datamarts_folder in r variable
    r$datamarts_folder <- datamarts_folder
    
    # App db folder
    if (length(app_db_folder) > 0) r$app_db_folder <- app_db_folder
    if (length(app_db_folder) == 0) r$app_db_folder <- path.expand('~')
    
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
      # setwd(initial_wd)
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
      
      # Show username on top of the page
      r$username <- r$users %>% dplyr::filter(id == r$user_id)
      r$username <- paste0(r$username$firstname, " ", r$username$lastname)
    })

    # Route pages
    router$server(input, output, session)
    
    # Keep trace of loaded observers (not to have multiple identical observers)
    r$loaded_observers <- ""

    # Load modules
    # Don't load modules user has no access to
    
    observeEvent(r$user_accesses, {
      
      ##########################################
      # Keep data user has access to          #
      ##########################################
      
      sapply(c("datamarts", "plugins"), function(table){
        if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
          if (nrow(r[[table]] > 0)){
            r[[table]] <- get_authorized_data(r = r, table = table)
            r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
          }
        }
      })

      ##########################################
      # Load server modules                    #
      ##########################################
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ START LOAD SERVER MODULES"))
      
      sapply(c("home", "home_get_started", "home_tutorials", "home_resources", "home_dev"), function(page){
        mod_home_server(page, r, language, r$words)
        mod_page_header_server(page, r, language, r$words)
      })
     
      if (perf_monitoring) print(paste0(Sys.time(), " _ data_pages"))
      sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_patient_and_aggregated_data_server(page, r, language, r$words)
        mod_page_sidenav_server(page, r, language, r$words)
        mod_page_header_server(page, r, language, r$words)
      })
      
      mod_my_studies_server("my_studies", r, language, r$words)
      mod_my_subsets_server("my_subsets", r, language, r$words)
      mod_thesaurus_server("thesaurus", r, language, r$words)
      
      sapply(c("my_studies", "my_subsets", "thesaurus"), function(page){
        mod_page_sidenav_server(page, r, language, r$words)
        mod_page_header_server(page, r, language, r$words)
      })
      
      if (perf_monitoring) print(paste0(Sys.time(), " _ plugins"))
      sapply(c("plugins_patient_lvl", "plugins_aggregated"), function(page){
        mod_plugins_server(page, r, language, r$words)
        mod_page_header_server(page, r, language, r$words)
      })
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ general"))
      mod_settings_general_server("settings_general_settings", r, language, r$words)
      mod_page_sidenav_server("settings_general_settings", r, language, r$words)
      mod_page_header_server("settings_general_settings", r, language, r$words)
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ app_db"))
      mod_settings_app_database_server("settings_app_db", r, language, r$words)
      mod_page_sidenav_server("settings_app_db", r, language, r$words)
      mod_page_header_server("settings_app_db", r, language, r$words)
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ users"))
      mod_settings_users_server("settings_users", r, language, r$words)
      mod_page_sidenav_server("settings_users", r, language, r$words)
      mod_page_header_server("settings_users", r, language, r$words)
    
      sapply(c("users", "users_statuses", "users_accesses"), function(page){
        mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, language, r$words)
        mod_settings_users_server(paste0("settings_users_", page, "_management"), r, language, r$words)
        mod_settings_users_server(paste0("settings_users_", page, "_options"), r, language, r$words)
      })
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ data_management"))
      mod_settings_r_console_server("settings_r_console", r, language, r$words)
      mod_page_sidenav_server("settings_r_console", r, language, r$words)
      mod_page_header_server("settings_r_console", r, language, r$words)
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ data_management"))
      sapply(c("data_sources", "datamarts", "thesaurus"), function(page){
        mod_settings_data_management_server(paste0("settings_", page), r, language, r$words)
        mod_page_sidenav_server(paste0("settings_", page), r, language, r$words)
        mod_page_header_server(paste0("settings_", page), r, language, r$words)
      })
    
      if (perf_monitoring) print(paste0(Sys.time(), " _ log"))
      mod_settings_log_server("settings_log", r, language, r$words)
      mod_page_sidenav_server("settings_log", r, language, r$words)
      mod_page_header_server("settings_log", r, language, r$words)
      
      if (perf_monitoring) print(paste0(Sys.time(), " _ END LOAD SERVER MODULES"))
      
      r$end_load_modules <- TRUE
    })
  }
}