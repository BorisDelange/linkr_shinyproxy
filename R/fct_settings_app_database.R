#' settings_app_database 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

db_create_table <- function(db, table_name, dataframe){
  if (table_name %not_in% DBI::dbListTables(db)) DBI::dbWriteTable(db, table_name, dataframe)
}

local_db <- function(){
  db <- DBI::dbConnect(RSQLite::SQLite(), "cdwtools")
  
  db_create_table(db, "users",
    tibble::tibble(
      id = integer(),
      username = character(),
      first_name = character(),
      last_name = character(),
      password = character(),
      access_status = character(),
      user_status = character(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "access_statuses",
    tibble::tibble(
      id = integer(),
      name = character(),
      action = character(),
      value = character(),
      value_num = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "data_sources",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "datamarts",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      date_source_id = integer(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "studies",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      datamart_id = integer(),
      patient_lvl_module_family_id = integer(),
      aggregated_module_family_id = integer(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "subsets",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      study_id = integer(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "patient_lvl_module_families",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
 
  db_create_table(db, "patient_lvl_modules",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      module_family_id = integer(),
      parent_module_id = integer(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "aggregated_module_families",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "aggregated_modules",
    tibble::tibble(
      id = integer(),
      name = character(),
      description = character(),
      module_family_id = integer(),
      parent_module_id = integer(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "code",
    tibble::tibble(
      id = integer(),
      category = character(),
      link_id = integer(),
      code = character(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db_create_table(db, "options",
    tibble::tibble(
      id = integer(),
      category = character(),
      link_id = integer(),
      name = character(),
      value = character(),
      value_num = numeric(),
      creator_id = integer(),
      datetime = lubridate::ymd_hms(),
      deleted = logical()
    )
  )
  
  db
}

distant_db <- function(){
  
}