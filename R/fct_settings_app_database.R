#' Create a new table
#' 
#' @description Create a new table in the database
#' 
#' @param db Database connection object (DBI)
#' @param table_name Name of the new table (character)
#' @param dataframe Dataframe with table informations (columns names & type of value) (dataframe / tibble)
#' @examples 
#' \dontrun{
#'   db_create_table(db, "my_new_table", tibble::tibble(id = integer(), name = character()))
#' }

db_create_table <- function(db, table_name, dataframe){
  if (table_name %not_in% DBI::dbListTables(db)) DBI::dbWriteTable(db, table_name, dataframe)
}

#' Create tables
#' 
#' @description Create all application database required tables. It creates the tables if they do not already exist.
#' @param db Database connection object (DBI)

db_create_tables <- function(db){
  
  # Create tables if not exist
  
  # In table users, create an admin user
  db_create_table(db, "users",
    tibble::tibble(id = integer(), username = character(), firstname = character(), lastname = character(), password = character(),
      user_access_id = integer(), user_status_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "users_accesses",
    tibble::tibble(id = integer(), name = character(), description = character(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "users_statuses",
    tibble::tibble(id = integer(), name = character(), description = character(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "data_sources",
    tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "datamarts",
    tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = integer(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "studies",
    tibble::tibble(id = integer(), name = character(), description = character(), datamart_id = integer(),
      patient_lvl_module_family_id = integer(), aggregated_module_family_id = integer(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "subsets",
    tibble::tibble(id = integer(), name = character(), description = character(), study_id = integer(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "subset_patients",
    tibble::tibble(id = integer(), subset_id = integer(), patient_id = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "thesaurus",
    tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = character(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "thesaurus_items",
    tibble::tibble(id = integer(), thesaurus_id = integer(), item_id = integer(),
      name = character(), display_name = character(), category = character(), unit = character(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "plugins",
    tibble::tibble(id = integer(), name = character(), description = character(), module_type_id = integer(), 
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "plugins_options",
    tibble::tibble(id = integer(), plugin_id = integer(), module_id = integer(), group_id = integer(), category = character(), link_id = integer(),
    value = character(), valuenum = numeric(), creator_id = integer(), datetime = character(), deleted = logical()))

  db_create_table(db, "patients_options",
    tibble::tibble(id = integer(), datamart_id = integer(), study_id = integer(), subset_id = integer(), patient_id = integer(), stay_id = integer(),
      category = character(), link_id = integer(), name = character(), value = character(), value_num = numeric(), 
      creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "patient_lvl_modules_families",
    tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = character(),
      deleted = logical()))
  
  db_create_table(db, "patient_lvl_modules",
    tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
      display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "patient_lvl_modules_elements",
    tibble::tibble(id = integer(), name = character(), group_id = integer(), module_id = integer(), plugin_id = integer(), 
      thesaurus_name = character(), thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
      thesaurus_item_colour = character(), display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))

  db_create_table(db, "aggregated_modules_families",
    tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = character(),
      deleted = logical()))
  
  db_create_table(db, "aggregated_modules",
    tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
      display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "aggregated_modules_elements",
    tibble::tibble(id = integer(), name = character(), group_id = integer(), module_id = integer(), plugin_id = integer(), 
      display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "code",
    tibble::tibble(id = integer(), category = character(), link_id = integer(), code = character(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "options",
    tibble::tibble(id = integer(), category = character(), link_id = integer(), name = character(), value = character(),
      value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "cache",
    tibble::tibble(id = integer(), category = character(), link_id = integer(), link_id_bis = integer(), value = character(), datetime = character()))
  
  db_create_table(db, "log",
    tibble::tibble(id = integer(), category = character(), name = character(), value = character(), creator_id = integer(), datetime = character()))
}

#' Connection to local database
#' 
#' @description Get a connection to local application database. If tables do not already exist, there are created
#' (with db_create_tables function). It uses RSQLite library.
#' It also adds distant database connection informations in the options table, if they do not already exist.

get_local_db <- function(){
  
  # Connect to local database
  db <- DBI::dbConnect(RSQLite::SQLite(), "cdwtools")
  
  db_create_tables(db)
  
  # Add distant db rows if they do not already exist
  if (DBI::dbGetQuery(db, "SELECT COUNT(id) FROM options WHERE category = 'distant_db'") != 7){
    
    DBI::dbSendStatement(db, "DELETE FROM options WHERE category = 'distant_db'")
    
    last_row <- DBI::dbGetQuery(db, "SELECT COALESCE(MAX(id), 0) FROM options")
    
    query <- paste0("INSERT INTO options(id, category, name, value, deleted)
                     SELECT ", last_row + 1, ", 'distant_db', 'connection_type', 'local', FALSE 
               UNION SELECT ", last_row + 2, ", 'distant_db', 'sql_lib', 'postgres', FALSE
               UNION SELECT ", last_row + 3, ", 'distant_db', 'dbname', '', FALSE
               UNION SELECT ", last_row + 4, ", 'distant_db', 'host', '', FALSE
               UNION SELECT ", last_row + 5, ", 'distant_db', 'port', '', FALSE
               UNION SELECT ", last_row + 6, ", 'distant_db', 'user', '', FALSE
               UNION SELECT ", last_row + 7, ", 'distant_db', 'password', '', FALSE")
    DBI::dbSendStatement(db, query)
    DBI::dbClearResult(query)
  }
  
  # Return DBI db object
  db
}

#' Test connection to distant database
#'
#' @description Test the connection to a distant database. If gets the distant db informations in local database
#' and then tries to connect to this distant database.
#' @param local_db DBI db object of local database
#' @param language Language used to display messages (character)

test_distant_db <- function(local_db, language = "EN"){
  
  result <- "fail"
  
  # Get distant db informations in local database
  db_info <- DBI::dbGetQuery(local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
  db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  
  # Try the connection
  tryCatch({
    
    # Postgres
    if (db_info$sql_lib == "postgres") DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$dbname, host = db_info$host,
      port = db_info$port, user = db_info$user, password = db_info$password)
    
    # SQLite
    if (db_info$sql_lib == "postgres") DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$dbname, host = db_info$host,
      port = db_info$port, user = db_info$user, password = db_info$password)
    result <- "success"
  },
  error = function(e) print(translate(language, "failed_connect_distant_db")),
  warning = function(w) print(translate(language, "failed_connect_distant_db")))
  
  # Result is fail or success
  result
}

#' Connection to distant database
#' 
#' @description Get a connection to a distant database. If the distant connection fails, returns local DBI connection object.
#' @param local_db DBI db object of local database
#' @param language Language used to display messages (character)

get_distant_db <- function(local_db, db_info = list(), language = "EN"){
  
  # If we fail to connect to distant db, the result is the local db
  db <- local_db
  
  # If db_info is not empty, use these informations
  # Else, use distant db informations saved in local db
  
  if (length(db_info) == 0){
    # Get db connection infos
    db_info <- DBI::dbGetQuery(db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
    db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  }
  
  # Try distant connection
  tryCatch({
    
    # Postgres
    if (db_info$sql_lib == "postgres") db <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$dbname, host = db_info$host,
      port = db_info$port, user = db_info$user, password = db_info$password)
    
    # SQLite
    if (db_info$sql_lib == "sqlite") db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$dbname, host = db_info$host,
      port = db_info$port, user = db_info$user, password = db_info$password)
  }, 
  error = function(e) print(translate(language, "failed_connect_distant_db")),
  warning = function(w) print(translate(language, "failed_connect_distant_db")))
  
  # Create tables if they do not already exist
  db_create_tables(db)
  
  # Returns distant db DBI object is connection is a success, local db DBI object else
  db
}

#' Get database DBI object
#'
#' @description Get final database connection DBI object.
#' First, get local database connection. 
#' Second, if db_info argument is not empty, try a connection with these informations.
#' Third, if db_info argument is empty or connection fails and if the choice recorded in local database is distant db, try distant db connection.
#' @param db_info DB informations given in cdwtools function
#' @param language Language used to display messages (character)

get_db <- function(db_info = list(), language = "EN"){
  
  # First, get local database connection
  
  db <- get_local_db()
  
  # Second, if db_info is not empty, try this connection
  
  if (length(db_info) > 0){
    get_distant_db(local_db = db, db_info = db_info, language = language)
  }
  
  # Third, if db_info is empty, get distant db if parameters in local db are set to distant connection
  # If connection fails, returns local db
  
  if (length(db_info) == 0){
    DBI::dbGetQuery(db, "SELECT value FROM options WHERE category = 'distant_db' AND name = 'connection_type'") %>%
      dplyr::pull() -> choice_distant_db
    if (choice_distant_db == "distant") db <- get_distant_db(local_db = db, language = language)
  }
  
  # Returns distant db connection if succesfully loaded, returns local db connection else
  db
}

#' Insert default values in database
#'
#' @param r shiny reactive value, used to communicate between modules

insert_default_values <- function(r){
  
  # Add default user
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    DBI::dbAppendTable(r$db, "users", tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      1, "admin", "John", "Doe", rlang::hash("admin"), 1, 1, as.character(Sys.time()), FALSE))
  }
  
  # Add default user access
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses")) == 0){
    DBI::dbAppendTable(r$db, "users_accesses", tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      1, "Administrator", "Administrator access", as.character(Sys.time()), FALSE))
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
      1, "My datawarehouse thesaurus", "", 1, 1, as.character(Sys.time()), FALSE))
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
      1, "Study patients managmeent", "A module made to manage patients status for the study", 1, NA_integer_, 1, 1, as.character(Sys.time()), FALSE,
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

#' Check user authentification
#' 
#' @param r shiny r reactive value

check_authentification <- function(db){
  
  function(user, password) {
    
    password <- rlang::hash(password)
    
    res <- DBI::dbGetQuery(db, paste0("SELECT * FROM users WHERE username = '", user, "' AND password = '", password, "'"))
    
    if (nrow(res) > 0) list(result = TRUE, user_info = list(user = user))
    else list(result = FALSE)

  }
}

#' Get last row ID of a table
#'
#' @param con DBI connection object to the database
#' @param table Name of the table

get_last_row <- function(con, table){
  glue::glue_sql("SELECT COALESCE(MAX(id), 0) FROM {`table`}", .con = con) -> sql
  DBI::dbGetQuery(con, sql) %>% dplyr::pull() %>% as.integer()
}