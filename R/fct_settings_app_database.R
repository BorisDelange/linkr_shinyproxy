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

db_create_tables <- function(db, type = character()){
  
  # Create tables if doest not exist
  
  # Type = main for main database
  # Type = plugins for plugins / modules database
  
  if (type == "main"){
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
    
    db_create_table(db, "thesaurus",
      tibble::tibble(id = integer(), name = character(), description = character(), data_source_id = character(), creator_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "thesaurus_items",
      tibble::tibble(id = integer(), thesaurus_id = integer(), item_id = integer(),
        name = character(), display_name = character(), unit = character(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "thesaurus_items_users",
      tibble::tibble(id = integer(), user_id = integer(), thesaurus_id = integer(), item_id = integer(),
        name = character(), display_name = character(), unit = character(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "thesaurus_items_mapping",
      tibble::tibble(id = integer(), category = character(), thesaurus_id_1 = integer(), item_id_1 = integer(), thesaurus_id_2 = integer(), item_id_2 = integer(),
        relation_id = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "thesaurus_items_mapping_evals",
      tibble::tibble(id = integer(), mapping_id = integer(), creator_id = integer(), evaluation_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "plugins",
      tibble::tibble(id = integer(), name = character(), description = character(), module_type_id = integer(), 
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "scripts",
      tibble::tibble(id = integer(), name = character(), data_source_id = integer(), creator_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "patient_lvl_modules_families",
      tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = character(),
        deleted = logical()))
    
    db_create_table(db, "patient_lvl_modules",
      tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
        display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    # db_create_table(db, "patient_lvl_modules_elements",
    #   tibble::tibble(id = integer(), name = character(), group_id = integer(), module_id = integer(), plugin_id = integer(), 
    #     thesaurus_name = character(), thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
    #     thesaurus_item_colour = character(), display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "patient_lvl_modules_elements",
      tibble::tibble(id = integer(), name = character(), module_id = integer(), plugin_id = integer(), 
        display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "patient_lvl_modules_elements_items",
      tibble::tibble(id = integer(), db_item_id = integer(), group_id = integer(),
        thesaurus_name = character(), thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
        thesaurus_item_colour = character(), mapped_to_item_id = integer(), merge_items = logical(),
        creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "aggregated_modules_families",
      tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = character(),
        deleted = logical()))
    
    db_create_table(db, "aggregated_modules",
      tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
        display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    # db_create_table(db, "aggregated_modules_elements",
    #   tibble::tibble(id = integer(), name = character(), group_id = integer(), module_id = integer(), plugin_id = integer(), 
    #     display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "aggregated_modules_elements",
      tibble::tibble(id = integer(), name = character(), module_id = integer(), plugin_id = integer(), 
        display_order = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "aggregated_modules_elements_items",
      tibble::tibble(id = integer(), group_id = integer(),
        thesaurus_name = character(), thesaurus_item_id = integer(), thesaurus_item_display_name = character(), thesaurus_item_unit = character(), 
        thesaurus_item_colour = character(), mapped_to_widget_item_id = integer(), merge_items = logical(),
        creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "code",
      tibble::tibble(id = integer(), category = character(), link_id = integer(), code = character(), creator_id = integer(),
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "options",
      tibble::tibble(id = integer(), category = character(), link_id = integer(), name = character(), value = character(),
        value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "messages",
      tibble::tibble(id = integer(), conversation_id = integer(), study_id = integer(), category = character(), 
        message = character(), filepath = character(), creator_id = integer(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "conversations",
        tibble::tibble(id = integer(), name = character(), datetime = character(), deleted = logical()))
    
    db_create_table(db, "inbox_messages",
      tibble::tibble(id = integer(), message_id = integer(), receiver_id = integer(), read = logical(), 
        datetime = character(), deleted = logical()))
    
    db_create_table(db, "cache",
      tibble::tibble(id = integer(), category = character(), link_id = integer(), link_id_bis = integer(), value = character(), datetime = character()))
    
    db_create_table(db, "log",
      tibble::tibble(id = integer(), category = character(), name = character(), value = character(), creator_id = integer(), datetime = character()))
  }
  
  if (type == "plugins"){
  
  db_create_table(db, "patients_options",
    tibble::tibble(id = integer(), datamart_id = integer(), study_id = integer(), subset_id = integer(), patient_id = integer(), stay_id = integer(),
      category = character(), link_id = integer(), name = character(), value = character(), value_num = numeric(), 
      creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "modules_elements_options",
    tibble::tibble(id = integer(), group_id = integer(), study_id = integer(), patient_id = integer(), link_id = integer(),
      category = character(), name = character(), value = character(), value_num = numeric(),
      creator_id = integer(), datetime = character(), deleted = logical()))
  
  db_create_table(db, "subsets",
    tibble::tibble(id = integer(), name = character(), description = character(), study_id = integer(), creator_id = integer(),
      datetime = character(), deleted = logical()))
  
  db_create_table(db, "subset_patients",
    tibble::tibble(id = integer(), subset_id = integer(), patient_id = integer(), creator_id = integer(), datetime = character(), deleted = logical()))
  }
}

#' Get authorized data for a user
#'
#' @param r Shiny r reactive value, used to communicate between modules
#' @param table Name of the table the data comes from (character)
#' @param data If data is not r[[table]] (tibble / dataframe)

get_authorized_data <- function(r, table, data = tibble::tibble()){
  
  if (nrow(data) == 0) data <- r[[table]]
  
  # Merge with options
  options <- data %>% dplyr::inner_join(r$options %>% dplyr::filter(category == get_singular(table)) %>% 
      dplyr::select(option_id = id, link_id, option_name = name, value, value_num), by = c("id" = "link_id"))
  
  # Vector of authorized data
  data_allowed <- integer()
  
  # For each data row, select those the user has access
  sapply(unique(options$id), function(data_id){
    
    # Loop over each data ID
    
    users_allowed_read_group <- options %>% dplyr::filter(id == data_id, option_name == "users_allowed_read_group")
    users_allowed_read <- options %>% dplyr::filter(id == data_id, option_name == "user_allowed_read")
    
    if (users_allowed_read_group %>% dplyr::pull(value) == "everybody") data_allowed <<- c(data_allowed, data_id)
    else if (nrow(users_allowed_read %>% dplyr::filter(value_num == r$user_id)) > 0) data_allowed <<- c(data_allowed, data_id)
    
  })
  
  # Select authorized data
  data %>% dplyr::filter(id %in% data_allowed)
}


#' Connection to local database
#' 
#' @description Get a connection to local application database. If tables do not already exist, there are created
#' (with db_create_tables function). It uses RSQLite library.
#' It also adds distant database connection informations in the options table, if they do not already exist.

# get_local_db <- function(app_db_folder = character()){
#   
#   # Connect to local database
#   
#   db <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
#   db_create_tables(db, type = type)
#   # Add distant db rows if they do not already exist
#   if (DBI::dbGetQuery(db, "SELECT COUNT(id) FROM options WHERE category = 'distant_db'") != 7){
#     
#     DBI::dbSendStatement(db, "DELETE FROM options WHERE category = 'distant_db'")
#     
#     last_row <- DBI::dbGetQuery(db, "SELECT COALESCE(MAX(id), 0) FROM options")
#     
#     sql <- paste0("INSERT INTO options(id, category, name, value, deleted) ",
#                     "SELECT ", last_row + 1, ", 'distant_db', 'connection_type', 'local', FALSE ",
#               "UNION SELECT ", last_row + 2, ", 'distant_db', 'sql_lib', 'postgres', FALSE ",
#               "UNION SELECT ", last_row + 3, ", 'distant_db', 'dbname', '', FALSE ",
#               "UNION SELECT ", last_row + 4, ", 'distant_db', 'host', '', FALSE ",
#               "UNION SELECT ", last_row + 5, ", 'distant_db', 'port', '', FALSE ",
#               "UNION SELECT ", last_row + 6, ", 'distant_db', 'user', '', FALSE ",
#               "UNION SELECT ", last_row + 7, ", 'distant_db', 'password', '', FALSE")
#     query <- DBI::dbSendStatement(db, sql)
#     DBI::dbClearResult(query)
#   }
#   
#   # Return DBI db object
#   db
# }

#' Test connection to distant database
#'
#' @description Test the connection to a distant database. If gets the distant db informations in local database
#' and then tries to connect to this distant database.
#' @param local_db DBI db object of local database
#' @param language Language used to display messages (character)

# test_distant_db <- function(local_db){
#   
#   result <- "fail"
#   
#   # Get distant db informations in local database
#   db_info <- DBI::dbGetQuery(local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
#   db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
#   
#   # Try the connection
#   tryCatch({
#     
#     # Postgres
#     if (db_info$sql_lib == "postgres"){
#       # Main DB
#       DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#       
#       # Public DB
#       DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#     } 
#     
#     # SQLite
#     if (db_info$sql_lib == "postgres"){
#       # Main DB
#       DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#       
#       # Public DB
#       DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
#     }
#     
#     result <- "success"
#   }, error = function(e) "", warning = function(w) "")
#   
#   # Result is fail or success
#   result
# }

#' Connection to distant database
#' 
#' @description Get a connection to a distant database. If the distant connection fails, returns local DBI connection object.
#' @param local_db DBI db object of local database
#' @param language Language used to display messages (character)

get_distant_db <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), local_db){
  
  result <- "fail"
  
  db_info <- DBI::dbGetQuery(local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
  db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  
  # Try the connection
  tryCatch({
    
    # Postgres
    if (db_info$sql_lib == "postgres"){
      db$main <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      db$plugins <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
    } 
    
    # SQLite
    if (db_info$sql_lib == "sqlite"){
      db$main <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      db$plugins <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
    }
    
    r$remote_db <- db$main
    m$remote_db <- db$plugins
    
    result <- "success"
    
  }, error = function(e) "", warning = function(w) "")
  
  result
}

#' Get database DBI object
#'
#' @description Get final database connection DBI object.
#' First, get local database connection. 
#' Second, if db_info argument is not empty, try a connection with these informations.
#' Third, if db_info argument is empty or connection fails and if the choice recorded in local database is distant db, try distant db connection.
#' @param db_info DB informations given in cdwtools function
#' @param language Language used to display messages (character)

get_db <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), app_db_folder = character()){
  
  # Get local database connection
  
  db <- list()
  db$local_main <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_main"))
  r$db <- db$local_main
  db$local_public <- DBI::dbConnect(RSQLite::SQLite(), paste0(app_db_folder, "/linkr_public"))
  m$db <- db$local_public
  
  # Create tables for local databases
  
  db_create_tables(db = db$local_main, type = "main")
  db_create_tables(db = db$local_public, type = "plugins")
  
  # Add distant db rows if they do not already exist
  
  if (DBI::dbGetQuery(db$local_main, "SELECT COUNT(id) FROM options WHERE category = 'distant_db'") != 8){
    
    DBI::dbSendStatement(db$local_main, "DELETE FROM options WHERE category = 'distant_db'")
    
    last_row <- DBI::dbGetQuery(db$local_main, "SELECT COALESCE(MAX(id), 0) FROM options")
    
    sql <- paste0("INSERT INTO options(id, category, name, value, deleted) ",
      "SELECT ", last_row + 1, ", 'distant_db', 'connection_type', 'local', FALSE ",
      "UNION SELECT ", last_row + 2, ", 'distant_db', 'sql_lib', 'postgres', FALSE ",
      "UNION SELECT ", last_row + 3, ", 'distant_db', 'main_db_name', '', FALSE ",
      "UNION SELECT ", last_row + 4, ", 'distant_db', 'public_db_name', '', FALSE ",
      "UNION SELECT ", last_row + 5, ", 'distant_db', 'host', '', FALSE ",
      "UNION SELECT ", last_row + 6, ", 'distant_db', 'port', '', FALSE ",
      "UNION SELECT ", last_row + 7, ", 'distant_db', 'user', '', FALSE ",
      "UNION SELECT ", last_row + 8, ", 'distant_db', 'password', '', FALSE")
    query <- DBI::dbSendStatement(db$local_main, sql)
    DBI::dbClearResult(query)
  }
  
  DBI::dbGetQuery(db$local_main, "SELECT value FROM options WHERE category = 'distant_db' AND name = 'connection_type'") %>% dplyr::pull() -> choice_distant_db
  
  if (choice_distant_db == "distant"){
    
    # Get distant DB parameters
    
    db_info <- DBI::dbGetQuery(db$local_main, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
    db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
    
    # Try distant connection
    
    result <- "failure"
    
    tryCatch({
      
      # Postgres
      if (db_info$sql_lib == "postgres"){
        db$remote_main <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        db$remote_public <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      } 
      
      # SQLite
      if (db_info$sql_lib == "sqlite"){
        db$remote_main <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$main_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
        db$remote_public <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_info$public_db_name, host = db_info$host, port = db_info$port, user = db_info$user, password = db_info$password)
      }
      
      r$db <- db$remote_main
      m$db <- db$remote_public
      
      print("create tables")
      db_create_tables(db = db$remote_main, type = "main")
      db_create_tables(db = db$remote_public, type = "plugins")
      
      result <- "success"
      
    }, error = function(e) "", warning = function(w) "")
    
    # If didn't succeed to connect to distant DB, update database and set connection to local
    if (result != "success"){
      sql <- glue::glue_sql("UPDATE options SET value = 'local' WHERE category = 'distant_db' AND name = 'connection_type'", .con = db$local_main)
      query <- DBI::dbSendStatement(db$local_main, sql)
      DBI::dbClearResult(query)
    }
  }
}


#' Load database
#' 
#' @param r Shiny r reactive value, used to communicate between modules
load_database <- function(r = shiny::reactiveValues(), i18n = R6::R6Class()){
  
  # Database tables to load
  tables <- c(
    "users", "users_accesses", "users_statuses",
    "data_sources", "datamarts", "thesaurus",
    "plugins",
    "code", 
    "options"
  )
  
  sapply(tables, function(table){
    r[[table]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
  })
  
  # Add a module_types variable, for settings/plugins dropdown
  r$module_types <- tibble::tribble(~id, ~name, 1, i18n$t("patient_level_data"), 2, i18n$t("aggregated_data"))
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
