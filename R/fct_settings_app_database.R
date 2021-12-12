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
  
  db_create_table(db, "patients_options",
    tibble::tibble(id = integer(), datamart_id = integer(), study_id = integer(), subset_id = integer(), patient_id = integer(), stay_id = integer(),
      category = character(), link_id = integer(), name = character(), value = character(), value_num = numeric(), 
      creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "modules_elements_options",
    tibble::tibble(id = integer(), group_id = integer(), study_id = integer(), patient_id = integer(), link_id = integer(),
      category = character(), name = character(), value = character(), value_num = numeric(),
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
  
  # db_create_table(db, "cache_for_settings",
  #   tibble::tibble(id = integer(), table_name = character(), link_id = integer(), name = character(), description = character(),
  #     username = character(), firstname = character(), lastname = character(), password = character(),
  #     user_access_id = character(), user_status_id = character(),
  #     data_source_id = character(), data_source_id_thesaurus = character(), datamart_id = character(), 
  #     patient_lvl_module_family_id = character(), aggregated_module_family_id = character(),
  #     study_id = character(), module_type_id = character(), module_family_id = character(), parent_module_id = character(),
  #     group_id = integer(), module_id = character(), plugin_id = character(),
  #     thesaurus_name = character(), thesaurus_item_id = character(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
  #     thesaurus_item_colour = character(), display_order = character(),
  #     action = character(), creator_id = character(), datetime = character(), deleted = logical())
  #   )
  
  db_create_table(db, "log",
    tibble::tibble(id = integer(), category = character(), name = character(), value = character(), creator_id = integer(), datetime = character()))
}

#' Connection to local database
#' 
#' @description Get a connection to local application database. If tables do not already exist, there are created
#' (with db_create_tables function). It uses RSQLite library.
#' It also adds distant database connection informations in the options table, if they do not already exist.

get_local_db <- function(app_db_folder = character()){
  
  # Connect to local database
  # If r$default_folder is not null, take this folder
  # Else, take home folder
  
  if (length(app_db_folder) > 0) db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/cdwtools"))
  else db <- DBI::dbConnect(RSQLite::SQLite(), paste0(path.expand('~'), "/cdwtools"))
  
  db_create_tables(db)
  
  # Add distant db rows if they do not already exist
  if (DBI::dbGetQuery(db, "SELECT COUNT(id) FROM options WHERE category = 'distant_db'") != 7){
    
    DBI::dbSendStatement(db, "DELETE FROM options WHERE category = 'distant_db'")
    
    last_row <- DBI::dbGetQuery(db, "SELECT COALESCE(MAX(id), 0) FROM options")
    
    sql <- paste0("INSERT INTO options(id, category, name, value, deleted)
                     SELECT ", last_row + 1, ", 'distant_db', 'connection_type', 'local', FALSE 
               UNION SELECT ", last_row + 2, ", 'distant_db', 'sql_lib', 'postgres', FALSE
               UNION SELECT ", last_row + 3, ", 'distant_db', 'dbname', '', FALSE
               UNION SELECT ", last_row + 4, ", 'distant_db', 'host', '', FALSE
               UNION SELECT ", last_row + 5, ", 'distant_db', 'port', '', FALSE
               UNION SELECT ", last_row + 6, ", 'distant_db', 'user', '', FALSE
               UNION SELECT ", last_row + 7, ", 'distant_db', 'password', '', FALSE")
    query <- DBI::dbSendStatement(db, sql)
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

test_distant_db <- function(local_db, language = "EN", words = tibble::tibble()){
  
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
  error = function(e) print(translate(language, "failed_connect_distant_db", words)),
  warning = function(w) print(translate(language, "failed_connect_distant_db", words)))
  
  # Result is fail or success
  result
}

#' Connection to distant database
#' 
#' @description Get a connection to a distant database. If the distant connection fails, returns local DBI connection object.
#' @param local_db DBI db object of local database
#' @param language Language used to display messages (character)

get_distant_db <- function(local_db, db_info = list(), language = "EN", words = tibble::tibble()){
  
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
  error = function(e) print(translate(language, "failed_connect_distant_db", words)),
  warning = function(w) print(translate(language, "failed_connect_distant_db", words)))
  
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

get_db <- function(db_info = list(), app_db_folder = character(), language = "EN"){
  
  # First, get local database connection
  
  db <- get_local_db(app_db_folder = app_db_folder)
  
  # Second, if db_info is not empty, try this connection
  
  if (length(db_info) > 0){
    get_distant_db(local_db = db, db_info = db_info, language = language, words = words)
  }
  
  # Third, if db_info is empty, get distant db if parameters in local db are set to distant connection
  # If connection fails, returns local db
  
  if (length(db_info) == 0){
    DBI::dbGetQuery(db, "SELECT value FROM options WHERE category = 'distant_db' AND name = 'connection_type'") %>%
      dplyr::pull() -> choice_distant_db
    if (choice_distant_db == "distant") db <- get_distant_db(local_db = db, language = language, words = words)
  }
  
  # Returns distant db connection if succesfully loaded, returns local db connection else
  db
}


#' Load database
#' 
#' @param r Shiny r reactive value, used to communicate between modules

load_database <- function(r = shiny::reactiveValues(), language = "EN"){
  
  # Database tables to load
  tables <- c(
    "users", "users_accesses", "users_statuses",
    "data_sources", "datamarts", "studies", "subsets", "subset_patients", "thesaurus",
    "plugins", 
    "patient_lvl_modules", "patient_lvl_modules_families", "patient_lvl_modules_elements",
    "aggregated_modules", "aggregated_modules_families", "aggregated_modules_elements",
    "code", 
    "options", "patients_options", "modules_elements_options")
  
  sapply(tables, function(table){
    r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
  })
  
  # Add a module_types variable, for settings/plugins dropdown
  r$module_types <- tibble::tribble(~id, ~name, 1, translate(language, "patient_level_data"), 2, translate(language, "aggregated_data"))
}


#' Check user authentification
#' 
#' @param r shiny r reactive value

check_authentification <- function(db){
  
  function(user, password) {
    
    password <- rlang::hash(password)
    
    res <- DBI::dbGetQuery(db, paste0("SELECT * FROM users WHERE username = '", user, "' AND password = '", password, "'"))
    
    
    if (nrow(res) > 0) list(result = TRUE, user_info = list(user = user, id = res$id))
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