
#' Insert default values in database
#'
#' @param r shiny reactive value, used to communicate between tabs

insert_default_data <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), has_internet = FALSE, options_toggles = tibble::tibble()){
  
  # --- --- --- --- --- --- --- --- --- --- ---
  # Add default users, accesses & statuses ----
  # --- --- --- --- --- --- --- --- --- --- ---
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    DBI::dbAppendTable(r$db, "users", tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      1, "admin", "Alan", "Turing", rlang::hash("admin"), 1, 1, as.character(Sys.time()), FALSE,
      2, "test1", "Ada", "Lovelace", rlang::hash("test1"), 2, 1, as.character(Sys.time()), FALSE,
      3, "test2", "Yann", "LeCun", rlang::hash("test2"), 2, 1, as.character(Sys.time()), FALSE,
      4, "test3", "Andrew", "Ng", rlang::hash("test3"), 2, 1, as.character(Sys.time()), FALSE,
      5, "test4", "Hadley", "Wickham", rlang::hash("test4"), 2, 1, as.character(Sys.time()), FALSE))
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
  # Add default data source, dataset... ----
  # --- --- --- --- --- --- --- --- --- --- -
  
  # Add default data source
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM data_sources")) == 0){
    DBI::dbAppendTable(r$db, "data_sources", tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      1, "MIMIC-IV", "", 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default dataset
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM datasets")) == 0){
    DBI::dbAppendTable(r$db, "datasets", tibble::tribble(~id, ~name, ~description, ~data_source_id, ~creator_id, ~datetime, ~deleted,
      1, "MIMIC-IV", i18n$t("mimic_iv_test_dataset"), 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default study
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM studies")) == 0){
    DBI::dbAppendTable(r$db, "studies", tibble::tribble(~id, ~name, ~description,
      ~dataset_id, ~patient_lvl_tab_group_id, ~aggregated_tab_group_id, ~creator_id, ~datetime, ~deleted,
      1, i18n$t("mortality_prediction"), "", 1, 1, 1, 1, as.character(Sys.time()), FALSE))
  }
  
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
    
    # Options for dataset & study
    
    DBI::dbAppendTable(r$db, "options", tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
      last_row + 1, "dataset", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 2, "dataset", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 3, "dataset", 1, "show_only_aggregated_data", "", 0, 1, as.character(Sys.time()), FALSE,
      last_row + 4, "dataset", 1, "activate_scripts_cache", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 5, "dataset", 1, "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_, 1, as.character(Sys.time()), FALSE,
      last_row + 6, "dataset", 1, "omop_version", "5.3", NA_integer_, 1, as.character(Sys.time()), FALSE,
      last_row + 7, "study", 1, "users_allowed_read_group", "everybody", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 8, "study", 1, "user_allowed_read", "", 1, 1, as.character(Sys.time()), FALSE,
      last_row + 9, "study", 1, "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_, 1, as.character(Sys.time()), FALSE,
      last_row + 10, "study", 1, "markdown_description", "", NA_integer_, 1, as.character(Sys.time()), FALSE))
    
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
      "patient_lvl_tabs", "patient_lvl_tabs_edit_data", "patient_lvl_tabs_delete_data", "patient_lvl_tabs_creation_card", "patient_lvl_tabs_management_card", "patient_lvl_tabs_options_card",
      "plugins", "plugins_description_card",
      "aggregated_tabs",  "aggregated_tabs_edit_data", "aggregated_tabs_delete_data", "aggregated_tabs_creation_card", "aggregated_tabs_management_card", "aggregated_tabs_options_card"
    )
    
    data <- data %>% dplyr::mutate(value_num = dplyr::case_when((link_id == 2 & name %in% test_user_rights) ~ 1, TRUE ~ value_num))
    
    # Add new values to database
    DBI::dbAppendTable(r$db, "options", data)
  }
  
  # --- --- --- --- --- -
  # Add default code ----
  # --- --- --- --- --- -
  
    dataset_code <- ""
    thesaurus_code <- ""
  
    if (has_internet){
      
      # Dataset code
      tryCatch(dataset_code <- readLines("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/datasets/mimic_iv_demo_1.0.R", warn = FALSE) %>%
        paste(collapse = "\n"), error = function(e) "", warning = function(w) "")
      
      # Thesaurus code
      tryCatch(thesaurus_code <- readLines("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/thesaurus/mimic_iv_1.0.R", warn = FALSE) %>%
        paste(collapse = "\n"), error = function(e) "", warning = function(w) "")
    }
    
    # Subset code
    subset_code <- paste0("add_persons_to_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)")
    
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM code")) == 0){
      DBI::dbAppendTable(r$db, "code", tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        1, "dataset", 1, dataset_code, 1, as.character(Sys.time()), FALSE,
        2, "thesaurus", 1, thesaurus_code, 1, as.character(Sys.time()), FALSE,
        3, "subset", 1, subset_code, 1, as.character(Sys.time()), FALSE))
    }
}
