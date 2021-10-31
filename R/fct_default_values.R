
#' Insert default values in database
#'
#' @param r shiny reactive value, used to communicate between modules

insert_default_values <- function(output, r){
  
  ##########################################
  # Add default users, accesses & statuses #
  ##########################################
  
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
      1, "Data scientist", "", as.character(Sys.time()), FALSE,
      2, "Clinician", "", as.character(Sys.time()), FALSE))
  }
  
  ##########################################
  # Add default data management            #
  ##########################################
  
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
  
  # Add default thesaurus items
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM thesaurus_items")) == 0){
    DBI::dbAppendTable(r$db, "thesaurus_items", tibble::tribble(
      ~id, ~thesaurus_id, ~item_id, ~name, ~display_name, ~category, ~unit, ~datetime, ~deleted,
      1, 1, 11L, "Heart rate", "HR", "Vitals", "bpm",
      2, 1, 12L, "Systolic blood pressure", "SBP", "Vitals", "mmHg", as.character(Sys.time()), FALSE,
      3, 1, 13L, "Diastolic blood pressure", "DBP", "Vitals", "mmHg", as.character(Sys.time()), FALSE,
      4, 1, 14L, "Mean blood pressure", "MBP", "Vitals", "mmHg", as.character(Sys.time()), FALSE,
      5, 1, 15L, "Pulse oxymetry", "", "Vitals", "%", as.character(Sys.time()), FALSE,
      6, 1, 16L, "Tidal volumne", "", "Vitals", "mL", as.character(Sys.time()), FALSE,
      7, 1, 17L, "Positive End-Expiratory Pressure", "PEEP", "Vitals", "cmH2O", as.character(Sys.time()), FALSE,
      8, 1, 18L, "Past medical history", "", "Admission notes", "", as.character(Sys.time()), FALSE,
      9, 1, 19L, "Reason for hospital admission", "", "Admission notes", "", as.character(Sys.time()), FALSE,
      10, 1, 20L, "Daily clinical note", "", "Daily notes", "", as.character(Sys.time()), FALSE,))
  }
  
  ##########################################
  # Add default modules                    #
  ##########################################
  
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
  
  # Add default patient_lvl_modules_elements
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM patient_lvl_modules_elements")) == 0){
    DBI::dbAppendTable(r$db, "patient_lvl_modules_elements", tibble::tribble(~id, ~name, ~group_id, ~module_id, ~plugin_id, 
      ~thesaurus_name, ~thesaurus_item_id, ~thesaurus_item_display_name, ~thesaurus_item_unit, ~thesaurus_item_colour, ~display_order, ~creator_id, ~datetime, ~deleted,
      1, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 11, "HR", "bpm", "#5AAE61", 1, 1, as.character(Sys.time()), FALSE,
      2, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 12, "SBP", "bpm", "#CB181D", 1, 1, as.character(Sys.time()), FALSE,
      3, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 13, "DBP", "bpm", "#CB181D", 1, 1, as.character(Sys.time()), FALSE,
      4, "Vitals dygraph", 1, 1, 1, "My datawarehouse thesaurus", 14, "MBP", "bpm", "#EF3B2C", 1, 1, as.character(Sys.time()), FALSE,
      5, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 11, "HR", "bpm", "#5AAE61", 2, 1, as.character(Sys.time()), FALSE,
      6, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 12, "SBP", "bpm", "#CB181D", 2, 1, as.character(Sys.time()), FALSE,
      7, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 13, "DBP", "bpm", "#CB181D", 2, 1, as.character(Sys.time()), FALSE,
      8, "Vitals datatable", 2, 1, 2, "My datawarehouse thesaurus", 14, "MBP", "bpm", "#EF3B2C", 2, 1, as.character(Sys.time()), FALSE,
      9, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 16, "Tidal volume", "mL", "#000000", 1, 1, as.character(Sys.time()), FALSE,
      10, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 17, "PEEP", "cmH2O", "#000000", 1, 1, as.character(Sys.time()), FALSE,
      11, "Vitals dygraph", 3, 2, 1, "My datawarehouse thesaurus", 15, "Pulse oxymetry", "%", "#2B8CBE", 1, 1, as.character(Sys.time()), FALSE,
      12, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 16, "Tidal volume", "mL", "#000000", 2, 1, as.character(Sys.time()), FALSE,
      13, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 17, "PEEP", "cmH2O", "#000000", 2, 1, as.character(Sys.time()), FALSE,
      14, "Vitals datatable", 4, 2, 2, "My datawarehouse thesaurus", 15, "Pulse oxymetry", "%", "#2B8CBE", 2, 1, as.character(Sys.time()), FALSE,
      15, "Admission notes", 5, 4, 3, "My datawarehouse thesaurus", 18, "Daily clinical note", "", "#000000", 1, 1, as.character(Sys.time()), FALSE,
      16, "Admission notes", 5, 4, 3, "My datawarehouse thesaurus", 19, "Reason for hospital admission", "", "#000000", 1, 1, as.character(Sys.time()), FALSE,
      17, "Daily notes", 6, 5, 3, "My datawarehouse thesaurus", 20, "Daily clinical note", "", "#000000", 1, 1, as.character(Sys.time()), FALSE))
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
  
  ##########################################
  # Add default options                    #
  ##########################################
  
  insert_options_rows <- FALSE
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category != 'distant_db'")) == 0) insert_options_rows <- TRUE
    
  if (insert_options_rows){
    
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
  
  if (insert_options_rows){
    
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
    test_user_rights <- c("general_settings", "change_paswword_card",
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
  
  # Add plugins
  
  # Add plugins descriptions
  
  plugin_description1 <- '- **Version** : 0.0.1
- **Libraries** : dygraphs, xts
- **Data allowed** : data$labs_vitals, data$labs_vitals_stay

This plugin creates a **dygraph chart** using [R dygraphs library](https://rstudio.github.io/dygraphs/), a R library which uses [dygraphs Javascript library](https://dygraphs.com/).

Here is an example :

![Screenshot](https://github.com/BorisDelange/cdwtools/blob/master/inst/app/www/plugins/dygraph.jpg?raw=true)

You choose **which items to display** in the time-chart. The chosen colours & display names will be used.

You can change the value in the box at the bottom left corner : this **smoothes the curve** more or less.

You can **zoom in on a time interval by two ways** : click and select a time interval directly on the curves or change the time interval in the range selector. Double click to **reset** the interval.'
  
  plugin_description2 <- '- **Version** : 0.0.1
- **Libraries** : DT
- **Data allowed** : data$labs_vitals, data$labs_vitals_stay

This plugin creates a **datatable**, using [R Datatable library](https://rstudio.github.io/DT/shiny.html), a R library which is derived from [Datatables Javascript library](https://datatables.net/).

Here is an example :

![Screenshot](https://github.com/BorisDelange/cdwtools/blob/master/inst/app/www/plugins/datatable.jpg?raw=true)

Values are sortable. You can **search** with the text area on the top right corner on the **whole data** and also **column by column** with text areas on top of the columns.'
  plugin_description3 <- '- **Version** : 0.0.1
- **Libraries** : none
- **Data allowed** : data$text, data$text_stay

This plugin renders **text data**, using Shiny functions.

Here is an example :

![Screenshot](https://github.com/BorisDelange/cdwtools/blob/master/inst/app/www/plugins/text.jpg?raw=true)'
  
  if (insert_options_rows){
    DBI::dbAppendTable(r$db, "options", tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      170, "plugin", 1, "markdown_description", plugin_description1, NA_integer_, 1,  as.character(Sys.time()), FALSE,
      171, "plugin", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      172, "plugin", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      173, "plugin", 2, "markdown_description", plugin_description2, NA_integer_, 1,  as.character(Sys.time()), FALSE,
      174, "plugin", 2, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      175, "plugin", 2, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      176, "plugin", 3, "markdown_description", plugin_description3, NA_integer_, 1,  as.character(Sys.time()), FALSE,
      177, "plugin", 3, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      178, "plugin", 3, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,))
  }
  
  ##########################################
  # Add default code                       #
  ##########################################
  
  datamart_code <- 
'####################
#     PATIENTS     #
####################

patients <- function() {
    tibble::tribble(
    ~patient_id, ~gender, ~age, ~dod, 
    1L, "F", 45, "",
    2L, "M", 71, "",
    3L, "F", 65, "2020-09-17 20:11:00") %>%
    dplyr::mutate_at("dod", lubridate::ymd_hms)}
    
import_datamart(output = output, r = r, datamart_id = %datamart_id%, data = patients(), type = "patients", save_as_csv = FALSE, language = language)

####################
#       STAYS      #
####################

stays <- function(){
    tibble::tribble(
    ~patient_id, ~stay_id, ~unit_name, ~admission_datetime, ~discharge_datetime,
    1L, 1L, "Emergency room", "2021-04-21 15:10:00", "2021-04-22 03:17:00",
    1L, 2L, "Medical ICU", "2021-04-22 03:17:00", "2021-05-01 04:27:00",
    2L, 3L, "Emergency room", "2021-01-18 09:10:00", "2021-01-19 03:05:00",
    2L, 4L, "Medical ICU", "2021-01-19 03:05:00", "2021-02-02 17:52:00",
    3L, 5L, "Emergency room", "2020-09-15 16:10:00", "2020-09-15 19:12:00",
    3L, 6L, "Medical ICU", "2020-09-15 19:12:00", "2020-09-17 20:11:00") %>%
    dplyr::mutate_at(c("admission_datetime", "discharge_datetime"), lubridate::ymd_hms)}

import_datamart(output = output, r = r, datamart_id = %datamart_id%, data = stays(), type = "stays", save_as_csv = FALSE, language = language)

####################
#  LABS VITALS     #
####################

labs_vitals <- function(){

    data_temp <- tibble::tibble(patient_id = integer(), thesaurus_name = character(), item_id = integer(), 
    datetime_start = lubridate::ymd_hms(), datetime_stop = lubridate::ymd_hms(), value = character(), value_num = numeric(),
    unit = character(), comments = character())
    
    # Boundaries for our values
    boundaries <- tibble::tribble(
        ~item_id, ~lower_limit, ~upper_limit,
        11L, 70, 80, # Heart rate
        12L, 90, 110, # SBP // DBP & MBP obtained with SBP
        15L, 90, 100, # Pulse oxymetry
        16L, 400, 450, # Tidal volume,
        17L, 5, 8 # PEEP
    )
  
    # Loop over patients stays
    sapply(1:nrow(stays()), function(i){
    
        row <- stays()[i, ]
        
        # Loop over thesaurus items
        sapply(1:nrow(boundaries), function(i){
            
            item <- boundaries[i, ]
        
            by <- ifelse(grepl("ICU", row$unit_name), "5 min", "60 min") # Higher frequency of vitals for ICU stays
            
            seq_datetimes <- seq(row$admission_datetime, row$discharge_datetime, by = by)
            
            seq_value_num <- round(runif(length(seq_datetimes), item$lower_limit, item$upper_limit), 0)
            if (item$item_id == 12L){
                seq_dbp <- seq_value_num - 30L
                seq_mbp <- as.integer(round((2*seq_value_num + seq_dbp) / 3, 0))
                seq_value_num <- c(seq_value_num, seq_dbp, seq_mbp)
                item_id <- as.integer(c(seq(12, 12, length.out = length(seq_datetimes)), 
                    seq(13, 13, length.out = length(seq_datetimes)), seq(14, 14, length.out = length(seq_datetimes))))
                seq_datetimes <- rep(seq_datetimes, 3)
            }
            else item_id <- as.integer(rep(item$item_id, length(seq_datetimes)))
            
            data_temp <<- data_temp %>% dplyr::bind_rows(
                tibble::tibble(
                    patient_id = row$patient_id, thesaurus_name = "My datawarehouse thesaurus", item_id = item_id,
                    datetime_start = seq_datetimes, datetime_stop = lubridate::ymd_hms(""),
                    value = as.character(seq_value_num), value_num = seq_value_num, unit = "", comments = ""
                )
            )
        })
    })
    
    data_temp
}

import_datamart(output = output, r = r, datamart_id = %datamart_id%, data = labs_vitals(), type = "labs_vitals", save_as_csv = FALSE, language = language)

####################
#       TEXT       #
####################

text <- function(){
    tibble::tribble(
        ~patient_id, ~thesaurus_name, ~item_id, ~datetime_start, ~datetime_stop, ~value, ~comments,
        1L, "My datawarehouse thesaurus", 18L, "2021-04-21 15:10:00", "", "- Atrial fibrillation\n- Chronic heart failure\n- Type 2 diabetes", "",
        1L, "My datawarehouse thesaurus", 19L, "2021-04-21 15:10:00", "", "Septic shock", "",
        1L, "My datawarehouse thesaurus", 20L, "2021-04-22 08:00:00", "", "Vitals : HR 70 bpm, BP 90/60 mmHg\n\nDay 2 of Piperacillin-tazobactam", "",
        1L, "My datawarehouse thesaurus", 20L, "2021-04-23 08:00:00", "", "Vitals : HR 60 bpm, BP 100/60 mmHg\n\nEnd of Norepinephrine today", "",
        1L, "My datawarehouse thesaurus", 20L, "2021-04-24 08:00:00", "", "Vitals : HR 65 bpm, BP 110/70 mmHg\n\nBacterial blood sample positive : E. Coli", "",
        1L, "My datawarehouse thesaurus", 20L, "2021-04-25 08:00:00", "", "Vitals : HR 70 bpm, BP 115/65 mmHg\n\nExtubation with success today", "",
        2L, "My datawarehouse thesaurus", 18L, "2021-01-18 09:10:00", "", "- Cirrhosis\n- Chronic kidney disease", "",
        2L, "My datawarehouse thesaurus", 19L, "2021-01-18 09:10:00", "", "Acute respiratory failure", "",
        2L, "My datawarehouse thesaurus", 20L, "2021-01-19 08:00:00", "", "Vitals : HR 60 bpm, BP 100/60 mmHg\n\nDiagnosis of COVID-19. Initiation of Dexamethasone", "",
        2L, "My datawarehouse thesaurus", 20L, "2021-01-20 08:00:00", "", "Vitals : HR 70 bpm, BP 140/90 mmHg\n\nIncrease of oxygenotherapy, facial oxygen mask with 10 L / min", "",
        2L, "My datawarehouse thesaurus", 20L, "2021-01-21 08:00:00", "", "Vitals : HR 90 bpm, BP 110/60 mmHg\n\nPatient intubated this morning. Cisatracurium initiated.", "",
        3L, "My datawarehouse thesaurus", 18L, "2020-09-15 16:10:00", "", "- Cirrhosis", "",
        3L, "My datawarehouse thesaurus", 19L, "2020-09-15 16:10:00", "", "Acute on chronic liver failure", "",
        3L, "My datawarehouse thesaurus", 20L, "2020-09-16 08:10:00", "", "Day 1 of admission.\n\nRapid deterioration of clinical condition.", ""
    ) %>%
    dplyr::mutate_at(c("datetime_start", "datetime_stop"), lubridate::ymd_hms)}

import_datamart(output = output, r = r, datamart_id = %datamart_id%, data = text(), type = "text", save_as_csv = FALSE, language = language)'
  
thesaurus_code <-
'####################
#    THESAURUS     #
####################

thesaurus <- function(){
    tibble::tribble(
    ~item_id, ~name, ~display_name, ~category, ~unit,
    11L, "Heart rate", "HR", "Vitals", "bpm",
    12L, "Systolic blood pressure", "SBP", "Vitals", "mmHg",
    13L, "Diastolic blood pressure", "DBP", "Vitals", "mmHg",
    14L, "Mean blood pressure", "MBP", "Vitals", "mmHg",
    15L, "Pulse oxymetry", "", "Vitals", "%",
    16L, "Tidal volumne", "", "Vitals", "mL",
    17L, "Positive End-Expiratory Pressure", "PEEP", "Vitals", "cmH2O",
    18L, "Past medical history", "", "Admission notes", "",
    19L, "Reason for hospital admission", "", "Admission notes", "",
    20L, "Daily clinical note", "", "Daily notes", "")}

import_thesaurus(output = output, r = r, thesaurus_id = %thesaurus_id%, thesaurus = thesaurus(), language = language)'

subset_code <- 'run_datamart_code(output = output, r = r, datamart_id = %datamart_id%)
patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at("patient_id", as.integer)
add_patients_to_subset(output = output, r = r, patients = patients, subset_id = %subset_id%)'

plugin_code_ui_1 <- 'div(
    br(),
    dygraphs::dygraphOutput(ns("dygraph_%group_id%_%patient_id%"), height = "300px")
)'
plugin_code_server_1 <- '# Remove duplicates if exist

data_temp <- 
    data$labs_vitals %>%
    dplyr::group_by(datetime_start, item_id, value_num) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(datetime_start, display_name, value_num) %>%
    dplyr::arrange(datetime_start)

data_temp <- data_temp %>% tidyr::pivot_wider(names_from = display_name, values_from = value_num)

data_temp <- xts::xts(x = data_temp %>% dplyr::select(-datetime_start), order.by = data_temp %>% dplyr::pull(datetime_start))

data_temp <-
    dygraphs::dygraph(data_temp) %>%
    dygraphs::dyAxis("y", valueRange = c(0, max(data_temp, na.rm = T) + max(data_temp, na.rm = T) / 5)) %>%
    dygraphs::dyAxis("x", drawGrid = FALSE)

output$dygraph_%group_id% <- dygraphs::renderDygraph(data_temp)# Remove duplicates if exist

data_temp <- 
    data$labs_vitals %>%
    dplyr::group_by(datetime_start, item_id, value_num) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(datetime_start, display_name, value_num) %>%
    dplyr::arrange(datetime_start)

data_temp <- data_temp %>% tidyr::pivot_wider(names_from = display_name, values_from = value_num)

data_temp <- xts::xts(x = data_temp %>% dplyr::select(-datetime_start), order.by = data_temp %>% dplyr::pull(datetime_start))

data_temp <-
    dygraphs::dygraph(data_temp) %>%
    dygraphs::dyAxis("y", valueRange = c(0, max(data_temp, na.rm = T) + max(data_temp, na.rm = T) / 4)) %>%
    dygraphs::dyAxis("x", drawGrid = FALSE) %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyRoller(rollPeriod = 50) %>%
    dygraphs::dyLegend(show = "always", hideOnMouseOut = TRUE, labelsSeparateLines = T) %>%
    dygraphs::dyHighlight(highlightCircleSize = 5)
    
data_temp_items <-
    data$labs_vitals %>%
    dplyr::group_by(item_id, display_name, colour, unit) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
    
sapply(1:nrow(data_temp_items), function(i){
    row <- data_temp_items[i, ]
    data_temp <<- 
        data_temp %>%
        dygraphs::dySeries(
            name = row$display_name, 
            label = paste0(row$display_name, " (", row$unit, ")"),
            color = row$colour
    )
})

output$dygraph_%group_id%_%patient_id% <- dygraphs::renderDygraph(data_temp)'

plugin_code_ui_2 <- 'DT::DTOutput(ns("datatable_%group_id%_%patient_id%"))'
plugin_code_server_2 <- 'data_temp <-
    data$labs_vitals %>%
    dplyr::select(display_name, datetime_start, value_num, unit) %>%
    dplyr::mutate_at("datetime_start", as.character)

col_names <- c("Item name", "Datetime", "Value", "Unit")
sortable_cols <- c("display_name", "datetime_start", "value_num")
centered_cols <- c("datetime_start")
column_widths = c("datetime" = "180px")
searchable_cols <- c("display_name", "datetime_start", "value_num")
factorize_cols <- c("display_name")

render_datatable(
    data = data_temp,
    output = output,
    r = r,
    ns = ns,
    language = language,
    output_name = "datatable_%group_id%_%patient_id%",
    col_names = col_names,
    page_length = 10,
    sortable_cols = sortable_cols,
    centered_cols = centered_cols,
    column_widths = column_widths,
    filter = TRUE,
    searchable_cols = searchable_cols,
    factorize_cols = factorize_cols
)'
plugin_code_ui_3 <- 'uiOutput(ns("text_%group_id%_%patient_id%"))'
plugin_code_server_3 <- 'output$text_%group_id%_%patient_id% <- renderUI({

    result <- tagList()
    
    data_temp <- data$text %>% dplyr::arrange(desc(datetime_start))

    sapply(1:nrow(data_temp), function(i){
        row <- data_temp[i, ]
        result <<- tagList(result, br(),
            div(
                strong(paste0(row$datetime_start, " - ", row$display_name)), br(), br(),
                HTML(row$value %>% stringr::str_replace_all("\n", "<br />")),
                style = "border: dashed 1px; padding: 10px"
            ),
        )
    })
    
    result
})'

  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM code")) == 0){
    DBI::dbAppendTable(r$db, "code", tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      1, "datamart", 1, datamart_code, 1, as.character(Sys.time()), FALSE,
      2, "thesaurus", 1, thesaurus_code, 1, as.character(Sys.time()), FALSE,
      3, "subset", 1, subset_code, 1, as.character(Sys.time()), FALSE,
      4, "subset", 2, "", 1, as.character(Sys.time()), FALSE,
      5, "subset", 3, "", 1, as.character(Sys.time()), FALSE,
      6, "plugin_ui", 1, plugin_code_ui_1, 1, as.character(Sys.time()), FALSE,
      7, "plugin_server", 1, plugin_code_server_1, 1, as.character(Sys.time()), FALSE,
      8, "plugin_ui", 2, plugin_code_ui_2, 1, as.character(Sys.time()), FALSE,
      9, "plugin_server", 2, plugin_code_server_2, 1, as.character(Sys.time()), FALSE,
      10, "plugin_ui", 3, plugin_code_ui_3, 1, as.character(Sys.time()), FALSE,
      11, "plugin_server", 3, plugin_code_server_3, 1, as.character(Sys.time()), FALSE))
  }

  ##########################################
  # Add subset patients                    #
  ##########################################
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM subset_patients")) == 0){
    DBI::dbAppendTable(r$db, "subset_patients", tibble::tribble(~id, ~subset_id, ~patient_id, ~creator_id, ~datetime, ~deleted,
      1, 1, 1, 1, as.character(Sys.time()), FALSE,
      2, 1, 2, 1, as.character(Sys.time()), FALSE,
      3, 1, 3, 1, as.character(Sys.time()), FALSE))
  }

  ##########################################
  # Add plugins                            #
  ##########################################

  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM plugins")) == 0){
    DBI::dbAppendTable(r$db, "plugins", tibble::tribble(~id, ~name, ~description, ~module_type_id, ~datetime, ~deleted,
      1, "Dygraph", "", 1, as.character(Sys.time()), FALSE,
      2, "Datatable", "", 1, as.character(Sys.time()), FALSE,
      3, "Text", "", 1, as.character(Sys.time()), FALSE))
  }
}