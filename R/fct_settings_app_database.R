#' settings_app_database 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

app_database_toggle_card <- function(language, ns, activated = ""){
  toggles <- tagList()
  sapply(c("db_connection_infos_card", "db_datatable_card", "db_request_card"), function(label){
    toggles <<- tagList(toggles, make_toggle(language, ns, label = label,
                                   id = paste0(label, "_toggle"), value = ifelse(label %in% activated, TRUE, FALSE), inline = TRUE))
  })
  make_card("",
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10), toggles
            )
  )
}

db_create_table <- function(db, table_name, dataframe){
  if (table_name %not_in% DBI::dbListTables(db)) DBI::dbWriteTable(db, table_name, dataframe)
}

db_create_tables <- function(db){
  # Create tables if not exist
  db_create_table(db, "users",
    tibble::tibble(id = integer(), username = character(), first_name = character(), last_name = character(), password = character(),
      access_status = character(), user_status = character(), datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "access_statuses",
    tibble::tibble(id = integer(), name = character(), action = character(), value = character(), value_num = integer(),
      datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "data_sources",
    tibble::tibble( id = integer(), name = character(), description = character(), creator_id = integer(), datetime = lubridate::ymd_hms(),
      deleted = logical()))
  
  db_create_table(db, "datamarts",
    tibble::tibble(id = integer(), name = character(), description = character(), date_source_id = integer(), creator_id = integer(),
      datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "studies",
    tibble::tibble(id = integer(), name = character(), description = character(), datamart_id = integer(),
      patient_lvl_module_family_id = integer(), aggregated_module_family_id = integer(), creator_id = integer(),
      datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "subsets",
    tibble::tibble(id = integer(), name = character(), description = character(), study_id = integer(), creator_id = integer(),
      datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "patient_lvl_module_families",
    tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = lubridate::ymd_hms(),
      deleted = logical()))
  
  db_create_table(db, "patient_lvl_modules",
    tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
      creator_id = integer(), datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "aggregated_module_families",
    tibble::tibble(id = integer(), name = character(), description = character(), creator_id = integer(), datetime = lubridate::ymd_hms(),
      deleted = logical()))
  
  db_create_table(db, "aggregated_modules",
    tibble::tibble(id = integer(), name = character(), description = character(), module_family_id = integer(), parent_module_id = integer(),
      creator_id = integer(), datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "code",
    tibble::tibble(id = integer(), category = character(), link_id = integer(), code = character(), creator_id = integer(),
      datetime = lubridate::ymd_hms(), deleted = logical()))
  
  db_create_table(db, "options",
    tibble::tibble(id = integer(), category = character(), link_id = integer(), name = character(), value = character(),
      value_num = numeric(), creator_id = integer(), datetime = lubridate::ymd_hms(), deleted = logical()))
}

get_local_db <- function(){
  
  # Connect to local database
  db <- DBI::dbConnect(RSQLite::SQLite(), "cdwtools")
  
  db_create_tables(db)
  
  # Add distant db rows if not exist
  if (DBI::dbGetQuery(db, "SELECT COUNT(id) FROM options WHERE category = 'distant_db'") != 7){
    DBI::dbSendStatement(db, "DELETE FROM options WHERE category = 'distant_db'")
    last_row <- DBI::dbGetQuery(db, "SELECT COALESCE(MAX(id), 0) FROM options")
    query <- paste0("INSERT INTO options(id, category, name, value, deleted)
                     SELECT ", last_row + 1, ", 'distant_db', 'connection_type', 'local', FALSE 
               UNION SELECT ", last_row + 2, ", 'distant_db', 'sql_lib', 'postgres', FALSE
               UNION SELECT ", last_row + 3, ", 'distant_db', 'dbname', NULL, FALSE
               UNION SELECT ", last_row + 4, ", 'distant_db', 'host', NULL, FALSE
               UNION SELECT ", last_row + 5, ", 'distant_db', 'port', NULL, FALSE
               UNION SELECT ", last_row + 6, ", 'distant_db', 'user', NULL, FALSE
               UNION SELECT ", last_row + 7, ", 'distant_db', 'password', NULL, FALSE")
    DBI::dbSendStatement(db, query)
  }
  
  db
}

test_distant_db <- function(local_db){
  result <- "fail"
  db_info <- DBI::dbGetQuery(local_db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
  db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  try({
    if (db_info$sql_lib == "postgres") DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$dbname, host = db_info$host,
                                                            port = db_info$port, user = db_info$user, password = db_info$password)
    result <- "success"
  })
  result
}

get_distant_db <- function(local_db){
  # If we fail to connect to distant db, the return is the local db
  db <- local_db
  
  # Get db connection infos
  db_info <- DBI::dbGetQuery(db, "SELECT * FROM options WHERE category = 'distant_db'") %>% tibble::as_tibble()
  db_info <- db_info %>% dplyr::pull(value, name) %>% as.list()
  
  try(
    if (db_info$sql_lib == "postgres") db <- DBI::dbConnect(RPostgres::Postgres(), dbname = db_info$dbname, host = db_info$host,
                                                            port = db_info$port, user = db_info$user, password = db_info$password)
  )
  
  db_create_tables(db)
  
  db
}

get_db <- function(){
  local_db <- get_local_db()
  db <- get_distant_db(local_db)
  db
}