
#' Insert default values in database
#'
#' @param r shiny reactive value, used to communicate between modules

insert_default_data <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), has_internet = FALSE, options_toggles = tibble::tibble()){
  
  # --- --- --- --- --- --- --- --- --- --- ---
  # Add default users, accesses & statuses ----
  # --- --- --- --- --- --- --- --- --- --- ---
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    DBI::dbAppendTable(r$db, "users", tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      1, "admin", "Gandalf", "The grey", rlang::hash("admin"), 1, 1, as.character(Sys.time()), FALSE,
      2, "test1", "Bilbo", "Baggins", rlang::hash("test1"), 2, 2, as.character(Sys.time()), FALSE,
      3, "test2", "Samwise", "Gamgee", rlang::hash("test2"), 2, 2, as.character(Sys.time()), FALSE,
      4, "test3", "Peregrin", "Took", rlang::hash("test3"), 2, 2, as.character(Sys.time()), FALSE,
      5, "test4", "Meriadoc", "Brandybuck", rlang::hash("test4"), 2, 3, as.character(Sys.time()), FALSE,
      6, "test5", "Elrond", "Half-elven", rlang::hash("test5"), 2, 3, as.character(Sys.time()), FALSE,
      6, "test6", "Aragorn", "Son of Arathorn", rlang::hash("test6"), 2, 3, as.character(Sys.time()), FALSE,
      6, "test7", "Frodo", "Baggins", rlang::hash("test7"), 2, 3, as.character(Sys.time()), FALSE))
  }
  
  # Add default user access
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses")) == 0){
    DBI::dbAppendTable(r$db, "users_accesses", tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      1, i18n$t("admin"), i18n$t("admin_access"), as.character(Sys.time()), FALSE,
      2, i18n$t("user"), i18n$t("user_access"), as.character(Sys.time()), FALSE))
  }
  
  # Add default user status
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_statuses")) == 0){
    DBI::dbAppendTable(r$db, "users_statuses", tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      1, i18n$t("data_scientist"), "", as.character(Sys.time()), FALSE,
      2, i18n$t("clinician"), "", as.character(Sys.time()), FALSE,
      3, i18n$t("statistician"), "", as.character(Sys.time()), FALSE))
  }
  
  # --- --- --- --- --- --- --- --- --- --- -
  # Add default data source, datamart... ----
  # --- --- --- --- --- --- --- --- --- --- -
  
  # Add default data source
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM data_sources")) == 0){
    DBI::dbAppendTable(r$db, "data_sources", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      1, "MIMIC-IV", "", 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default datamart
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM datamarts")) == 0){
    DBI::dbAppendTable(r$db, "datamarts", tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      1, "MIMIC-IV", i18n$t("mimic_iv_test_datamart"), 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default study
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM studies")) == 0){
    DBI::dbAppendTable(r$db, "studies", tibble::tribble(~id, ~name, ~description,
      ~datamart_id, ~patient_lvl_module_family_id, ~aggregated_module_family_id, ~creator_id, ~datetime, ~deleted,
      1, i18n$t("mortality_prediction"), "", 1, 1, 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default subsets
  if (nrow(DBI::dbGetQuery(m$db, "SELECT * FROM subsets")) == 0){
    DBI::dbAppendTable(m$db, "subsets", tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id, ~datetime, ~deleted,
      1, i18n$t("subset_all_patients"), "", 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default thesaurus
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM thesaurus")) == 0){
    DBI::dbAppendTable(r$db, "thesaurus", tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      1, "MIMIC-IV", "", 1L, 1, as.character(Sys.time()), FALSE))
  }
  
  # --- --- --- --- --- -- -
  # Add default modules ----
  # --- --- --- --- --- -- -
  
  # Add default patient_lvl_modules_families
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules_families")) == 0){
  #   DBI::dbAppendTable(r$db, "patient_lvl_modules_families", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
  #     1, "Default patient-lvl module family", "", 1, as.character(Sys.time()), FALSE))
  # }
  
  # Add default patient_lvl_modules
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules")) == 0){
  #   DBI::dbAppendTable(r$db, "patient_lvl_modules", tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~display_order, 
  #     ~creator_id, ~datetime, ~deleted,
  #     1, "Haemodynamics", "A module containing haemodynamics data", 1, NA_integer_, 1, 1, as.character(Sys.time()), FALSE,
  #     2, "Respiratory", "A module containing respiratory data", 1, NA_integer_, 2, 1, as.character(Sys.time()), FALSE,
  #     3, "Clinical notes", "A module containing clinical notes", 1, NA_integer_, 3, 1, as.character(Sys.time()), FALSE,
  #     4, "Admission notes", "A sub-module containing admission clinical notes", 1, 3, 1, 1, as.character(Sys.time()), FALSE,
  #     5, "Daily notes", "A sub-module containing daily clinical notes", 1, 3, 2, 1, as.character(Sys.time()), FALSE))
  # }
  
  # Add default patient_lvl_modules_elements
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules_elements")) == 0){
  #   DBI::dbAppendTable(r$db, "patient_lvl_modules_elements", tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, 
  #     ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~display_order, ~creator_id, ~datetime, ~deleted,
  #     1, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 11, "HR", "bpm", "#5AAE61", 1, 1, as.character(Sys.time()), FALSE,
  #     2, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 12, "SBP", "bpm", "#CB181D", 1, 1, as.character(Sys.time()), FALSE,
  #     3, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 13, "DBP", "bpm", "#CB181D", 1, 1, as.character(Sys.time()), FALSE,
  #     4, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 14, "MBP", "bpm", "#EF3B2C", 1, 1, as.character(Sys.time()), FALSE,
  #     5, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 11, "HR", "bpm", "#5AAE61", 2, 1, as.character(Sys.time()), FALSE,
  #     6, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 12, "SBP", "bpm", "#CB181D", 2, 1, as.character(Sys.time()), FALSE,
  #     7, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 13, "DBP", "bpm", "#CB181D", 2, 1, as.character(Sys.time()), FALSE,
  #     8, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 14, "MBP", "bpm", "#EF3B2C", 2, 1, as.character(Sys.time()), FALSE,
  #     9, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 16, "Tidal volume", "mL", "#000000", 1, 1, as.character(Sys.time()), FALSE,
  #     10, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 17, "PEEP", "cmH2O", "#000000", 1, 1, as.character(Sys.time()), FALSE,
  #     11, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 15, "Pulse oxymetry", "%", "#2B8CBE", 1, 1, as.character(Sys.time()), FALSE,
  #     12, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 16, "Tidal volume", "mL", "#000000", 2, 1, as.character(Sys.time()), FALSE,
  #     13, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 17, "PEEP", "cmH2O", "#000000", 2, 1, as.character(Sys.time()), FALSE,
  #     14, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 15, "Pulse oxymetry", "%", "#2B8CBE", 2, 1, as.character(Sys.time()), FALSE,
  #     15, "Admission notes", 5, 4, 3, "My datawarehouse thesaurus", 18, "Daily clinical note", "", "#000000", 1, 1, as.character(Sys.time()), FALSE,
  #     16, "Admission notes", 5, 4, 3, "My datawarehouse thesaurus", 19, "Reason for hospital admission", "", "#000000", 1, 1, as.character(Sys.time()), FALSE,
  #     17, "Daily notes", 6, 5, 3, "My datawarehouse thesaurus", 20, "Daily clinical note", "", "#000000", 1, 1, as.character(Sys.time()), FALSE))
  # }
  
  # Add default aggregated_modules_families
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM aggregated_modules_families")) == 0){
  #   DBI::dbAppendTable(r$db, "aggregated_modules_families", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
  #     1, "Default aggregated module family", "", 1, as.character(Sys.time()), FALSE))
  # }
  
  # Add default aggregated_modules
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM aggregated_modules")) == 0){
  #   DBI::dbAppendTable(r$db, "aggregated_modules", tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~display_order,
  #     ~creator_id, ~datetime, ~deleted,
  #     1, "Study patients management", "A module made to manage patients status for the study", 1, NA_integer_, 1, 1, as.character(Sys.time()), FALSE,
  #     2, "Inclusion / exclusion", "A module made to manage inclusion & exclusion criteria", 1, 1, 1, 1, as.character(Sys.time()), FALSE,
  #     3, "Flowchart", "A module made to create the flowchart", 1, 1, 2, 1, as.character(Sys.time()), FALSE,
  #     4, "Guidelines", "A module made to show EQUATOR guidelines", 1, NA_integer_, 2, 1, as.character(Sys.time()), FALSE))
  # }
  
  # --- --- --- --- --- -- -
  # Add default options ----
  # --- --- --- --- --- -- -
  
  insert_options_rows <- FALSE

  if (r$db_connection == "local"){
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category != 'remote_db'")) == 0) insert_options_rows <- TRUE
  }
  
  if (r$db_connection == "remote"){
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM options")) == 0) insert_options_rows <- TRUE
  }
  
  if (insert_options_rows){
    
    last_row <- get_last_row(r$db, "options")
    
    # Options for datamart & study
    
    DBI::dbAppendTable(r$db, "options", tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row + 1, "datamart", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 2, "datamart", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 3, "datamart", 1, "show_only_aggregated_data", "", 0, 1, as.character(Sys.time()), FALSE,
      last_row + 4, "datamart", 1, "activate_scripts_cache", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 5, "datamart", 1, "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_, 1, as.character(Sys.time()), FALSE,
      last_row + 6, "study", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 7, "study", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 8, "study", 1, "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_, 1, as.character(Sys.time()), FALSE))
    
    # Users accesses
    data <- tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted)
    
    # Loop over all toggles, set 1 to value_num for admin, 0 for test
    sapply(1:2, function(link_id) {
      sapply(1:nrow(options_toggles), function(i){
        data <<- data %>% dplyr::bind_rows(
          tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
            "users_accesses", link_id, options_toggles[[i, "name"]], "", ifelse(link_id == 1, 1, 0), 1, as.character(Sys.time()), FALSE)
        )
        if (options_toggles[[i, "toggles"]] != ""){
          sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
            
            data <<- data %>% dplyr::bind_rows(
              tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                "users_accesses", link_id, toggle, "", ifelse(link_id == 1, 1, 0), 1, as.character(Sys.time()), FALSE)
            )
          })
        }
      })
    })
    
    # Attribute id values
    last_row <- get_last_row(r$db, "options")
    data$id <- seq.int(nrow(data)) + last_row
    
    # Set rights to test user
    test_user_rights <- c("general_settings", "change_password_card",
      "studies", "studies_edit_data", "studies_creation_card", "studies_datatable_card", "studies_options_card",
      "thesaurus", "thesaurus_see_all_data", "thesaurus_datatable_card", "thesaurus_sub_datatable_card",
      "patient_lvl_modules", "patient_lvl_modules_edit_data", "patient_lvl_modules_delete_data", "patient_lvl_modules_creation_card", "patient_lvl_modules_management_card", "patient_lvl_modules_options_card",
      "plugins", "plugins_description_card",
      "aggregated_modules",  "aggregated_modules_edit_data", "aggregated_modules_delete_data", "aggregated_modules_creation_card", "aggregated_modules_management_card", "aggregated_modules_options_card"
    )
    
    data <- data %>% dplyr::mutate(value_num = dplyr::case_when((link_id == 2 & name %in% test_user_rights) ~ 1, TRUE ~ value_num))
    
    # Add new values to database
    DBI::dbAppendTable(r$db, "options", data)
  }
  
  # --- --- --- --- --- -
  # Add default code ----
  # --- --- --- --- --- -
  
    datamart_code <- ""
    thesaurus_code <- ""
  
    if (has_internet){
      
      # Datamart code
      datamart_code <- readLines("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/datamarts/mimic_iv_demo_1.0.R", warn = FALSE) %>%
        paste(collapse = "\n")
      
      # Thesaurus code
      thesaurus_code <- readLines("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/thesaurus/mimic_iv_1.0.R", warn = FALSE) %>%
        paste(collapse = "\n")
    }
    
    # Subset code
    subset_code <- paste0("patients <- d$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at(\"patient_id\", as.integer)\n\n",
      "add_patients_to_subset(output = output, m = m, patients = patients, subset_id = %subset_id%, i18n = i18n, ns = ns)")
    
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM code")) == 0){
      DBI::dbAppendTable(r$db, "code", tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        1, "datamart", 1, datamart_code, 1, as.character(Sys.time()), FALSE,
        2, "thesaurus", 1, thesaurus_code, 1, as.character(Sys.time()), FALSE,
        3, "subset", 1, subset_code, 1, as.character(Sys.time()), FALSE))
    }
  # --- --- --- -- -
  # Add plugins ----
  # --- --- --- -- -
  
  # if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM plugins")) == 0){
  #   DBI::dbAppendTable(r$db, "plugins", tibble::tribble(~id, ~name, ~description, ~module_type_id, ~datetime, ~deleted,
  #     1, "Dygraph", "", 1, as.character(Sys.time()), FALSE,
  #     2, "Datatable", "", 1, as.character(Sys.time()), FALSE,
  #     3, "Text", "", 1, as.character(Sys.time()), FALSE))
  # }
    
  # --- --- --- -- -
  # Add scripts ----
  # --- --- --- -- -
}
