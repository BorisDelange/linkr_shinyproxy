#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'
#' @import shiny
#' @noRd

app_server <- function(router, language = "en", db_info = list(), app_folder = character(), datamarts_folder = character(), app_db_folder = character(),
  perf_monitoring = FALSE, debug = FALSE){
  function(input, output, session ) {
    
    if (debug) print(paste0(Sys.time(), " - server - init"))
    
    language <- tolower(language)
    
    if (debug) print(paste0(Sys.time(), " - server - reactive values"))
    # Create r reactive value, for the application processings
    r <- reactiveValues()
    
    # Create d reactive value, for datamart data
    d <- reactiveValues()
    vars <- c("patients", "stays", "labs_vitals", "orders", "text", "diagnoses")
    for (var in vars) d[[var]] <- tibble::tibble()
    
    # Create m reactive value, for plugins & modules data
    m <- reactiveValues()
    
    # Create o reactive values, for observers inactivation
    o <- reactiveValues()
    
    # App version
    r$app_version <- "0.2.0"
    
    # Test internet connection
    if (debug) print(paste0(Sys.time(), " - server - has_internet"))
    r$has_internet <- curl::has_internet()
    
    # If perf_monotoring activated
    if (debug) print(paste0(Sys.time(), " - server - perf_monitoring"))
    # r$perf_monitoring <- perf_monitoring
    
    r$perf_monitoring_table <- tibble::tibble(task = character(), datetime_start = lubridate::ymd_hms(), datetime_stop = lubridate::ymd_hms())
    datetime_start <- Sys.time()
    datetime_stop <- Sys.time()
    
    # Create r$server_modules_groups_loaded & r$ui_modules_groups_loaded
    r$server_modules_groups_loaded <- ""
    r$ui_modules_groups_loaded <- ""
    
    # Save datamarts_folder in r variable
    if (debug) print(paste0(Sys.time(), " - server - datamarts_folder"))
    r$datamarts_folder <- datamarts_folder
    
    # App folder
    if (debug) print(paste0(Sys.time(), " - server - app_folder"))
    r$app_folder <- app_folder
    
    # App db folder
    app_db_folder <- paste0(app_folder, "/app_database")
    r$app_db_folder <- app_db_folder
    
    # Get translations
    # Update : use shiny.i18n instead. When it is done, delete get_translations
    # r$words <- get_translations()
    if (debug) print(paste0(Sys.time(), " - server - translations"))
    i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
    i18n$set_translation_language(language)
    r$i18n <- i18n
    
    # Save currently opened toggles (used to reload cards when we load a page, restart reactivity)
    r$activated_toggles <- ""
    
    # Connection to database
    # If connection informations have been given in cdwtools() function, use these informations
    if (debug) print(paste0(Sys.time(), " - server - app_db"))
    r$local_db <- get_local_db(app_db_folder = app_db_folder, type = "main")
    
    # Add distant db informations in local database
    
    observeEvent(r$local_db, {
      
      if (debug) print(paste0(Sys.time(), " - server - observer r$local_db"))
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
        error = function(e) print(i18n$t("error_insert_distant_db_info")),
        warning = function(w) print(i18n$t("error_insert_distant_db_info")))
      }
    })
    
    r$db <- get_db(db_info = db_info, app_db_folder = app_db_folder, type = "main")
    m$db <- get_db(db_info = db_info, app_db_folder = app_db_folder, type = "plugins")
    
    # Close DB connection on exit
    # And restore initial working directory
    onStop(function() {
      if (debug) print(paste0(Sys.time(), " - server - observer onStop"))
      add_log_entry(r = isolate(r), category = "Connection ends", name = "Connection ends", value = "")
      DBI::dbDisconnect(isolate(r$db))
    })
    
    # Add default values in database if database is empty
    # Load all data from database
    # Don't load thesaurus_items, load it only when a thesaurus is selected
    # Don't load cache table neither
    
    observeEvent(r$db, {
      
      if (debug) print(paste0(Sys.time(), " - server - observer r$db"))
      
      # Add default values in database, if it is empty
      # insert_default_values(output = output, r = r)
      
      # Load database
      load_database(r = r, i18n = i18n)
      
    })


    # Secure the app with shinymanager

    if (debug) print(paste0(Sys.time(), " - server - shinyManager"))
    r$res_auth <- shinymanager::secure_server(check_credentials = check_authentification(r$db))

    # Get user ID

    observeEvent(r$res_auth, {
      if (debug) print(paste0(Sys.time(), " - server - observer r$res_auth"))
      r$user_id <- as.integer(reactiveValuesToList(r$res_auth)$id)
      add_log_entry(r = r, category = "Connection starts", name = "Connection starts", value = "")
    })


    # When r$user_id loaded, load user_accesses

    observeEvent(r$user_id, {
      if (debug) print(paste0(Sys.time(), " - server - observer r$user_id"))
      
      req(r$user_id)

      user_access_id <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(user_access_id)

      # Get user accesses
      r$user_accesses <- r$options %>% dplyr::filter(category == "users_accesses" & link_id == user_access_id & value_num == 1) %>% dplyr::pull(name)
      
      # Show username on top of the page
      r$username <- r$users %>% dplyr::filter(id == r$user_id)
      r$username <- paste0(r$username$firstname, " ", r$username$lastname)
    })

    # Route pages
    if (debug) print(paste0(Sys.time(), " - server - shiny.router"))
    router$server(input, output, session)
    
    # Keep trace of loaded observers (not to have multiple identical observers)
    r$loaded_observers <- ""

    # Load modules
    # Don't load modules user has no access to
    
    observeEvent(r$user_accesses, {
      
      if (debug) print(paste0(Sys.time(), " - server - observer r$user_accesses"))
      
      # --- --- --- --- --- -- -
      # Get authorized data ----
      # --- --- --- --- --- -- -
      
      sapply(c("datamarts", "plugins"), function(table){
        if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
          if (nrow(r[[table]] > 0)){
            r[[table]] <- get_authorized_data(r = r, table = table)
            r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
          }
        }
      })

      # --- --- --- --- --- -- -
      # Load server modules ----
      # --- --- --- --- --- -- -
    
      if (debug) print(paste0(Sys.time(), " - server - load server modules"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      if (debug) print(paste0(Sys.time(), " - server - load server modules - home"))
      sapply(c("home", "home_get_started", "home_tutorials", "home_resources", "home_dev"), function(page){
        mod_home_server(page, r, language, i18n, perf_monitoring, debug)
        mod_page_header_server(page, r, language, i18n)
      })
     
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - home")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - mod_patient_and_aggregated_data"))
      
      sapply(c("patient_level_data", "aggregated_data"), function(page){
        mod_patient_and_aggregated_data_server(page, r, d, m, o, i18n, perf_monitoring, debug)
        mod_page_sidenav_server(page, r, d, m, i18n, language)
        mod_page_header_server(page, r, language, i18n)
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - mod_patient_and_aggregated_data")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - my_studies / my_subsets / thesaurus / scripts"))
      
      mod_my_studies_server("my_studies", r, d, m, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - my_studies")
      mod_my_subsets_server("my_subsets", r, d, m, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - my_subsets")
      mod_thesaurus_server("thesaurus", r, d, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - thesaurus")
      mod_scripts_server("scripts", r, d, m, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - scripts")
      
      sapply(c("my_studies", "my_subsets", "thesaurus", "scripts"), function(page){
        mod_page_sidenav_server(page, r, d, m, i18n, language)
        mod_page_header_server(page, r, language, i18n)
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - my_studies / my_subsets / thesaurus / scripts - sidenav")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - plugins"))
      
      sapply(c("plugins_patient_lvl", "plugins_aggregated"), function(page){
        mod_plugins_server(page, r, d, m, o, language, i18n, app_folder)
        mod_page_header_server(page, r, language, i18n)
      })
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - plugins")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - general_settings"))
    
      mod_settings_general_server("settings_general_settings", r, language, i18n)
      mod_page_sidenav_server("settings_general_settings", r, d, m, i18n, language)
      mod_page_header_server("settings_general_settings", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - general_settings")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - settings_app_db"))
    
      mod_settings_app_database_server("settings_app_db", r, language, i18n)
      mod_page_sidenav_server("settings_app_db", r, d, m, i18n, language)
      mod_page_header_server("settings_app_db", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - settings_app_db")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - settings_users"))
    
      mod_settings_users_server("settings_users", r, i18n)
      mod_page_sidenav_server("settings_users", r, d, m, i18n, language)
      mod_page_header_server("settings_users", r, language, i18n)
    
      sapply(c("users", "users_statuses", "users_accesses"), function(page){
        mod_settings_users_server(paste0("settings_users_", page, "_creation"), r, i18n)
        mod_settings_users_server(paste0("settings_users_", page, "_management"), r, i18n)
        mod_settings_users_server(paste0("settings_users_", page, "_options"), r, i18n)
      })
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - settings_users")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - settings_r_console"))
    
      mod_settings_r_console_server("settings_r_console", r, d, m, i18n)
      mod_page_sidenav_server("settings_r_console", r, d, m, i18n, language)
      mod_page_header_server("settings_r_console", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - settings_r_console")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - data_sources / datamarts / thesaurus"))
    
      sapply(c("data_sources", "datamarts", "thesaurus"), function(page){
        mod_settings_data_management_server(paste0("settings_", page), r = r, d = d, m = m, i18n = i18n)
        mod_page_sidenav_server(paste0("settings_", page), r, d, m, i18n, language)
        mod_page_header_server(paste0("settings_", page), r, language, i18n)
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("server - load server modules - ", page))
      })
    
      if (debug) print(paste0(Sys.time(), " - server - load server modules - settings_log"))
      mod_settings_log_server("settings_log", r, i18n)
      mod_page_sidenav_server("settings_log", r, d, m, i18n, language)
      mod_page_header_server("settings_log", r, language, i18n)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = "server - load server modules - settings_log")
      if (debug) print(paste0(Sys.time(), " - server - load server modules - end"))
      
      r$end_load_modules <- TRUE
      
      r$perf_monitoring_table <-
        r$perf_monitoring_table %>%
        dplyr::mutate(elapsed_time = round(datetime_stop - datetime_start, 2), .before = "task") #%>%
        # dplyr::arrange(dplyr::desc(elapsed_time))
    })
  }
}
