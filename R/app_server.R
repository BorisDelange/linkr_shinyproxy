#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(page_style, router, language){
  function( input, output, session ) {
    
    r <- reactiveValues()
    
    r$user_id <- 1
    r$local_db <- get_local_db()
    r$db <- get_db()
    
    # users <- tibble::tribble(
    #   ~id, ~username, ~firstname, ~lastname, ~password, ~user_access, ~user_status, ~datetime, ~deleted,
    #   1, "admin", "John", "Doe", "", "Admin", "Clinician", "2021-04-15 09:45:43", FALSE,
    #   3, "janed", "Jane", "Doe", "", "User level 1", "Data scientist", "2021-05-13 11:51:17", FALSE
    # )
    
    # Load all data from database
    observeEvent(r$db, {
      tables <- c("users", "users_accesses_statuses", "users_accesses_details",
                  "data_sources", "datamarts", "studies", "subsets",
                  "patient_lvl_module_families", "patient_lvl_modules", "aggregated_module_families", "aggregated_modules",
                  "code", "options"
                  )
      sapply(tables, function(table){
        r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table))
        r[[paste0(table, "_temp")]] <- r[[table]]
        r[[paste0(table, "_temp")]] <- r[[paste0(table, "_temp")]] %>% dplyr::filter(deleted == FALSE) %>% dplyr::mutate(modified = FALSE)
      })
    })
    
   
    # 
    # users_accesses_statuses <- tibble::tribble(
    #   ~id, ~type, ~name, ~description, ~datetime, ~deleted,
    #   1, "access", "User level 1", "Allowed to manage his own studies & subsets", "2021-06-01 14:31:32", FALSE,
    #   2, "access", "Admin", "Administrator", "2021-06-01 14:31:32", FALSE,
    #   3, "status", "Data scientist", "", "2021-06-01 14:31:32", FALSE,
    #   4, "status", "Clinician", "", "2021-06-01 14:31:32", FALSE
    # )
    # r$users_accesses_statuses <- users_accesses_statuses
    # r$users_accesses_statuses_temp <- users_accesses_statuses
    # 
    # r$users_accesses_details <- tibble::tribble(
    #   ~id, ~link_id, ~action, ~value, ~value_num, ~datetime, ~deleted,
    #   1, 2, "developper_mode", "", 1, "2021-06-01 14:31:32", FALSE,
    #   2, 1, "developper_mode", "", 0, "2021-06-01 14:31:32", FALSE
    # )
    # 
    # data_sources <- tibble::tribble(
    #   ~id, ~name, ~description, ~creator, ~datetime, ~deleted,
    #   # ~`Data source ID`, ~`Data source name`, ~`Data source description`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #    13, "MIMIC-IV", "MIMIC database version 4", 1, "2021-09-16 17:58:21", FALSE,
    #    15, "eHOP", "eHOP university hospital of Rennes", 1, "2021-09-16 17:59:00", FALSE)
    # r$data_sources <- data_sources
    # r$data_sources_temp <- data_sources
    # 
    # datamarts <- tibble::tribble(
    #   ~id, ~name, ~description, ~data_source_id, ~creator, ~datetime, ~deleted,
    #   # ~`Datamart ID`, ~`Datamart name`, ~`Datamart description`, ~`Data source ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   2, "Weaning from mechanical ventilation", "A study with MV", 13, 3, "2021-09-15 16:20:21", FALSE,
    #   4, "Heparin data Metavision", "Prediction of response of UFH therapy in ICU", 15, 1, "2021-09-14 15:20:23", FALSE)
    # r$datamarts <- datamarts
    # r$datamarts_temp <- datamarts
    # 
    # studies <- tibble::tribble(
    #   ~id, ~name, ~description, ~datamart_id, ~patient_lvl_module_family_id, ~aggregated_module_family_id, ~creator, ~datetime, ~deleted,
    #   # ~`Study ID`, ~`Study name`, ~`Study description`, ~`Datamart ID`,
    #   # ~`Patient-level data module family ID`, ~`Aggregated data module family ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   1, "Study 1", "Weaning from mechanical ventilation", 2, 1, 3, 3, "2021-08-23 08:53:45", FALSE,
    #   2, "Study 2", "Heparin datamart Metavision", 4, 3, 3, 3, "2021-07-25 09:45:43", FALSE,
    #   3, "Study 3", "My new study", 2, 5, 4, 1, "2021-06-23 21:13:47", FALSE)
    # r$studies <- studies
    # r$studies_temp <- studies
    # 
    # subsets <- tibble::tribble(
    #   ~id, ~name, ~description, ~study_id, ~creator, ~datetime, ~deleted,
    #   # ~`Subset ID`, ~`Subset name`, ~`Subset description`, ~`Study ID`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   1, "Included patients", "A subset with only included patients", 1, 1, "2021-07-21 19:45:13", FALSE)
    # r$subsets <- subsets
    # r$subsets_temp <- subsets
    # 
    # r$patient_lvl_module_families <- tibble::tribble(
    #   ~id, ~name, ~description, ~creator, ~datetime, ~deleted,
    #   # ~`Module family ID`, ~`Module family name`, ~`Module family description`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   1, "eHOP default", "Default eHOP module family", 3, "2021-05-21 14:13:12", FALSE,
    #   3, "Metavision default", "Default MV module family", 3, "2021-04-21 13:12:46", FALSE,
    #   5, "App default", "Default app module family", 1, "2021-04-20 16:45:31", TRUE)
    # 
    # r$patient_lvl_modules <- tibble::tribble(
    #   ~id, ~name, ~description, ~module_family_id, ~parent_module_id, ~creator, ~datetime, ~deleted,
    #   # ~`Module ID`, ~`Module name`, ~`Module description`, ~`Module family ID`, ~`Parent module ID`, ~`Creator`, ~`Date & time`, ~`Deleted`
    #   )
    # 
    # r$aggregated_module_families <- tibble::tribble(
    #   ~id, ~name, ~description, ~creator, ~datetime, ~deleted,
    #   #  ~`Module family ID`, ~`Module family name`, ~`Module family description`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   3, "Default 1", "Default 1 aggregated data module family", 1, "2021-04-13 13:12:51", FALSE,
    #   4, "Default 2", "Default 2 aggregated data module family", 3, "2021-06-21 19:12:31", FALSE)
    # 
    # r$aggregated_modules <- tibble::tribble(
    #   ~id, ~name, ~description, ~parent_module_id, ~creator, ~datetime, ~deleted,
    #   # ~`Module ID`, ~`Module name`, ~`Module description`, ~`Parent module ID`, ~`Creator`, ~`Date & time`, ~`Deleted`
    #   )
    # 
    # r$code <- tibble::tribble(
    #   ~id, ~category, ~link_id, ~code, ~creator, ~datetime, ~deleted,
    #   # ~`Code ID`, ~`Category`, ~`Link ID`, ~`Code`, ~`Creator`, ~`Date & time`, ~`Deleted`,
    #   5, "datamart", 2, "datamart <- read_csv('my_csv.csv')", 1, "2021-07-04 17:23:54", FALSE,
    #   7, "datamart", 4, "", 1, "2021-08-05 13:45:23", FALSE)
    # 
    # r$options <- tibble::tribble(
    #   ~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator, ~datetime, ~deleted,
    #   1, "datamart", 2, "user_allowed_read", "", 3, 3, "2021-07-04 17:23:54", FALSE,
    #   2, "datamart", 2, "show_only_aggregated_data", "", 0, 1, "2021-07-04 17:23:54", FALSE,
    #   3, "datamart", 4, "user_allowed_read", "", 3, 3, "2021-07-04 17:23:54", FALSE,
    #   4, "datamart", 4, "show_only_aggregated_data", "", 0, 1, "2021-07-04 17:23:54", FALSE,
    # )
    # 
    if (page_style == "fluent") router$server(input, output, session)
    
    mod_settings_app_database_server("settings_app_database", r, language)
    mod_settings_users_server("settings_users", r, language)
    mod_settings_data_management_server("settings_data_sources", r, language)
    mod_settings_data_management_server("settings_datamarts", r, language)
    mod_settings_data_management_server("settings_studies", r, language)
    mod_settings_data_management_server("settings_subsets", r, language)
  }
}
