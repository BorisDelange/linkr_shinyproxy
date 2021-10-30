
#' Insert default values in database
#'
#' @param r shiny reactive value, used to communicate between modules

insert_default_values <- function(r){
  
  # Add default users
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    DBI::dbAppendTable(r$db, "users", tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      1, "admin", "John", "Doe", rlang::hash("admin"), 1, 1, as.character(Sys.time()), FALSE,
      2, "test", "Jane", "Doe", rlang::hash("test"), 2, 2, as.character(Sys.time()), FALSE))
  }
  
  # Add default user access
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses")) == 0){
    DBI::dbAppendTable(r$db, "users_accesses", tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      1, "Administrator", "Administrator access", as.character(Sys.time()), FALSE,
      2, "User", "User access", as.character(Sys.time()), FALSE))
  }
  
  # Add default user status
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_statuses")) == 0){
    DBI::dbAppendTable(r$db, "users_statuses", tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      1, "Data scientist", "", as.character(Sys.time()), FALSE))
  }
  
  # Add default data source
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM data_sources")) == 0){
    DBI::dbAppendTable(r$db, "data_sources", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      1, "My datawarehouse", "", 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default datamart
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM datamarts")) == 0){
    DBI::dbAppendTable(r$db, "datamarts", tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      1, "Invasive mechanical ventilation", "A datamart containing patients who were treated with invasive MV during ICU stay", 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default study
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM studies")) == 0){
    DBI::dbAppendTable(r$db, "studies", tibble::tribble(~id, ~name, ~description, 
      ~datamart_id, ~patient_lvl_module_family_id, ~aggregated_module_family_id, ~creator_id, ~datetime, ~deleted,
      1, "Predicting extubation success", "This study aims to build a predictive model to predict extubation success", 1, 1, 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default subsets
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM subsets")) == 0){
    DBI::dbAppendTable(r$db, "subsets", tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id, ~datetime, ~deleted,
      1, "All patients", "", 1, 1, as.character(Sys.time()), FALSE,
      2, "Included patients", "", 1, 1, as.character(Sys.time()), FALSE,
      3, "Excluded patients", "", 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default thesaurus
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM thesaurus")) == 0){
    DBI::dbAppendTable(r$db, "thesaurus", tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      1, "My datawarehouse thesaurus", "", 1L, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default patient_lvl_modules_families
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules_families")) == 0){
    DBI::dbAppendTable(r$db, "patient_lvl_modules_families", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      1, "Default patient-lvl module family", "", 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default patient_lvl_modules
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules")) == 0){
    DBI::dbAppendTable(r$db, "patient_lvl_modules", tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~display_order, 
      ~creator_id, ~datetime, ~deleted,
      1, "Haemodynamics", "A module containing haemodynamics data", 1, NA_integer_, 1, 1, as.character(Sys.time()), FALSE,
      2, "Respiratory", "A module containing respiratory data", 1, NA_integer_, 2, 1, as.character(Sys.time()), FALSE,
      3, "Clinical notes", "A module containing clinical notes", 1, NA_integer_, 3, 1, as.character(Sys.time()), FALSE,
      4, "Admission notes", "A sub-module containing admission clinical notes", 1, 3, 1, 1, as.character(Sys.time()), FALSE,
      5, "Daily notes", "A sub-module containing daily clinical notes", 1, 3, 2, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default aggregated_modules_families
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM aggregated_modules_families")) == 0){
    DBI::dbAppendTable(r$db, "aggregated_modules_families", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      1, "Default aggregated module family", "", 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default aggregated_modules
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM aggregated_modules")) == 0){
    DBI::dbAppendTable(r$db, "aggregated_modules", tibble::tribble(~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~display_order,
      ~creator_id, ~datetime, ~deleted,
      1, "Study patients management", "A module made to manage patients status for the study", 1, NA_integer_, 1, 1, as.character(Sys.time()), FALSE,
      2, "Inclusion / exclusion", "A module made to manage inclusion & exclusion criteria", 1, 1, 1, 1, as.character(Sys.time()), FALSE,
      3, "Flowchart", "A module made to create the flowchart", 1, 1, 2, 1, as.character(Sys.time()), FALSE,
      4, "Guidelines", "A module made to show EQUATOR guidelines", 1, NA_integer_, 2, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default options
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category != 'distant_db'")) == 0){
    DBI::dbAppendTable(r$db, "options", tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      1, "datamart", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      2, "datamart", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      3, "datamart", 1, "show_only_aggregated_data", "", 0, 1, as.character(Sys.time()), FALSE,
      4, "study", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      5, "study", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      6, "patient_lvl_module_family", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      7, "patient_lvl_module_family", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      8, "aggregated_module_family", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      9, "aggregated_module_family", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE))
  }
  
  options_toggles <- tibble::tribble(
    ~name, ~toggles,
    "general_settings", "change_password_card",
    "app_db", c("db_connection_infos_card", "db_datatable_card", "db_request_card", "db_save_card"),#, "db_restore_card"),
    "users", c("users_delete_data", "users_creation_card", "users_management_card",
      "users_accesses_creation_card", "users_accesses_management_card", "users_accesses_options_card",
      "users_statuses_creation_card", "users_statuses_management_card"),
    "r_console", "r_console_edit_code_card",
    "data_sources", c("data_sources_see_all_data", "data_sources_edit_data", "data_sources_delete_data", "data_sources_creation_card", "data_sources_datatable_card"),
    "datamarts", c("datamarts_see_all_data", "datamarts_edit_data", "datamarts_delete_data", "datamarts_creation_card", "datamarts_datatable_card", "datamarts_options_card", "datamarts_edit_code_card"),
    "studies", c("studies_see_all_data", "studies_edit_data", "studies_delete_data", "studies_creation_card", "studies_datatable_card", "studies_options_card"),
    "subsets", c("subsets_see_all_data", "subsets_edit_data", "subsets_delete_data", "subsets_creation_card", "subsets_datatable_card", "subsets_edit_code_card"),
    "thesaurus", c("thesaurus_see_all_data", "thesaurus_edit_data", "thesaurus_delete_data", "thesaurus_creation_card", "thesaurus_datatable_card", "thesaurus_sub_datatable_card", "thesaurus_edit_code_card"),
    "plugins", c("plugins_see_all_data", "plugins_edit_data", "plugins_delete_data", "plugins_description_card", "plugins_creation_card", "plugins_datatable_card", "plugins_options_card", "plugins_edit_code_card"),
    "patient_lvl_modules", c("patient_lvl_modules_see_all_data", "patient_lvl_modules_edit_data", "patient_lvl_modules_delete_data", "patient_lvl_modules_creation_card", "patient_lvl_modules_management_card", "patient_lvl_modules_options_card"),
    "aggregated_modules", c("aggregated_modules_see_all_data", "aggregated_modules_edit_data", "aggregated_modules_delete_data", "aggregated_modules_creation_card", "aggregated_modules_management_card", "aggregated_modules_options_card"),
    "log", c("all_users", "only_me")
  )
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category != 'distant_db'")) == 9){
    
    data <- tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted)
    
    # Loop over all toggles, set 0 to value_num is toggle is FALSE, 1 else
    sapply(1:nrow(options_toggles), function(i){
      data <<- data %>% dplyr::bind_rows(
        tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
          "users_accesses", 1, options_toggles[[i, "name"]], "", 1, 1, as.character(Sys.time()), FALSE)
      )
      if (options_toggles[[i, "toggles"]] != ""){
        sapply(options_toggles[[i, "toggles"]][[1]], function(toggle){
          
          data <<- data %>% dplyr::bind_rows(
            tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              "users_accesses", 1, toggle, "", 1, 1, as.character(Sys.time()), FALSE)
          )
        })
      }
    })
    
    # Delete old data from options
    
    query <- DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE category = 'users_accesses' AND link_id = 1"))
    DBI::dbClearResult(query)
    
    # Attribute id values
    
    last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull())
    data$id <- seq.int(nrow(data)) + last_row
    
    # Add new values to database
    DBI::dbAppendTable(r$db, "options", data)
  }
  
  code_subsets <- paste0("run_datamart_code(output = output, r = r, datamart_id = %datamart_id%)\n",
    "patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)\n",
    "add_patients_to_subset(output = output, r = r, patients = patients, subset_id = %subset_id%)")
  
  # # Add default code
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM code")) == 0){
    DBI::dbAppendTable(r$db, "code", tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      1, "datamart", 1, "", 1, as.character(Sys.time()), FALSE,
      2, "thesaurus", 1, "", 1, as.character(Sys.time()), FALSE,
      3, "subset", 1, code_subsets, 1, as.character(Sys.time()), FALSE,
      4, "subset", 2, "", 1, as.character(Sys.time()), FALSE,
      5, "subset", 3, "", 1, as.character(Sys.time()), FALSE))
  }
}