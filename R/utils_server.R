#' Update r variable
#' 
#' @description Update r value, requesting the corresponding table in the database
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param table Database table name (character)
#' @param language language used for the translation (character)
#' @examples
#' \dontrun{
#' update_r(r = r, table = "subsets")
#' }
update_r <- function(r = shiny::reactiveValues(), table = character(), language = "EN"){
  tables <- c("users", "users_accesses", "users_statuses",
    "data_sources", "datamarts", "studies", "subsets", "subset_patients", "subsets_patients", "thesaurus", "thesaurus_items",
    "plugins", "scripts",
    "patient_lvl_modules_families", "patient_lvl_modules", "patient_lvl_modules_elements",
    "aggregated_modules_families", "aggregated_modules", "aggregated_modules_elements",
    "code", 
    "options",
    "modules_elements_options", "patients_options")
  
  if (table %not_in% tables) stop(paste0(translate(language, "invalid_table_name"), ". ", translate(language, "tables_allowed"), " : ", toString(tables)))
  
  # if (table %in% c("patients_options", "modules_elements_options", "subsets", "subset_patients")) db <- m$db
  db <- r$db
  
  if (table %in% c("datamarts", "plugins", "data_sources", "thesaurus")){
    
    r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    
    if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
      if (nrow(r[[table]] > 0)){
        r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
        r[[table]] <- get_authorized_data(r = r, table = table)
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
      }
    }
  }
  
  else if (grepl("modules", table)){
    
    if (table == "modules_elements_options"){
      sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND study_id = {r$chosen_study}", .con = db)
      r$modules_elements_options <- DBI::dbGetQuery(db, sql)
    }
    
    else {
      if (grepl("patient_lvl", table)) prefix <- "patient_lvl" else prefix <- "aggregated"
      
      if (grepl("families", table)){
        family_id <- r$studies %>% dplyr::filter(id == r$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
        sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND id = {family_id}", .con = db)
        r[[paste0(prefix, "_modules_families")]] <- DBI::dbGetQuery(db, sql)
      }
      
      else if (grepl("elements", table)){
        modules_ids <- r[[paste0(prefix, "_modules")]] %>% dplyr::pull(id)
        sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND module_id IN ({modules_ids*})", .con = db)
        r[[paste0(prefix, "_modules_elements")]] <- DBI::dbGetQuery(db, sql)
      }
      
      else {
        family_id <- r$studies %>% dplyr::filter(id == r$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
        sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND module_family_id = {family_id}", .con = db)
        r[[paste0(prefix, "_modules")]] <- DBI::dbGetQuery(db, sql)
      }
      
    }
  }
  
  else if (table %in% c("studies", "subsets", "scripts", "subset_patients", "subsets_patients", "patients_options")){
    
    if (table == "subsets_patients"){
      
      sql <- glue::glue_sql("SELECT * FROM subset_patients WHERE deleted IS FALSE AND subset_id IN ({r$subsets %>% dplyr::pull(id)*})", .con = db)
      r$subsets_patients <- DBI::dbGetQuery(db, sql)
    }
    
    else {
      
      tables <- tibble::tribble(~name, ~col_name, ~col_value,
        "studies", "datamart_id", r$chosen_datamart,
        "scripts", "data_source_id", r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id),
        "subsets", "study_id", r$chosen_study,
        "subset_patients", "subset_id", r$chosen_subset,
        "patients_options", "study_id", r$chosen_study)
      
      row <- tables %>% dplyr::filter(name == table)
      
      sql <- glue::glue_sql("SELECT * FROM {`row$name`} WHERE deleted IS FALSE AND {`row$col_name`} = {row$col_value}", .con = db)
      r[[row$name]] <- DBI::dbGetQuery(db, sql)
    }
  }
  
  else {
    
    r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
    r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
  }
}

update_r_new <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), table = character(), i18n = R6::R6Class()){
  tables <- c("users", "users_accesses", "users_statuses",
    "data_sources", "datamarts", "studies", "subsets", "subset_patients", "subsets_patients", "thesaurus", "thesaurus_items",
    "plugins", "scripts",
    "patient_lvl_modules_families", "patient_lvl_modules", "patient_lvl_modules_elements",
    "aggregated_modules_families", "aggregated_modules", "aggregated_modules_elements",
    "code", 
    "options",
    "modules_elements_options", "patients_options")
  
  if (table %not_in% tables) stop(paste0(i18n$t("invalid_table_name"), ". ", i18n$t("tables_allowed"), " : ", toString(tables)))
  
  if (table %in% c("patients_options", "modules_elements_options", "subsets", "subset_patients", "subsets_patients")){
    db <- m$db
    
    if (table %in% c("subsets", "subset_patients", "subsets_patients", "patients_options")){
      
      if (table == "subsets_patients"){
        
        sql <- glue::glue_sql("SELECT * FROM subset_patients WHERE deleted IS FALSE AND subset_id IN ({m$subsets %>% dplyr::pull(id)*})", .con = db)
        m$subsets_patients <- DBI::dbGetQuery(db, sql)
      }
      
      else {
        
        tables <- tibble::tribble(~name, ~col_name, ~col_value,
          "subsets", "study_id", m$chosen_study,
          "subset_patients", "subset_id", m$chosen_subset,
          "patients_options", "study_id", m$chosen_study)
        
        row <- tables %>% dplyr::filter(name == table)
        
        sql <- glue::glue_sql("SELECT * FROM {`row$name`} WHERE deleted IS FALSE AND {`row$col_name`} = {row$col_value}", .con = db)
        m[[row$name]] <- DBI::dbGetQuery(db, sql)
      }
    }
    
    else {
      
      m[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      m[[paste0(table, "_temp")]] <- m[[table]] %>% dplyr::mutate(modified = FALSE)
    }
  }
  
  else {
    db <- r$db
    
    if (table %in% c("datamarts", "plugins", "data_sources", "thesaurus")){
      
      r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      
      if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
        if (nrow(r[[table]] > 0)){
          r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
          r[[table]] <- get_authorized_data(r = r, table = table)
          r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        }
      }
    }
    
    else if (grepl("modules", table)){
      
      if (table == "modules_elements_options"){
        sql <- glue::glue_sql("SELECT * FROM modules_elements_options WHERE deleted IS FALSE AND study_id = {m$chosen_study}", .con = db)
        r$modules_elements_options <- DBI::dbGetQuery(db, sql)
      }
      
      else {
        if (grepl("patient_lvl", table)) prefix <- "patient_lvl" else prefix <- "aggregated"
        
        if (grepl("families", table)){
          family_id <- r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
          sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND id = {family_id}", .con = db)
          r[[paste0(prefix, "_modules_families")]] <- DBI::dbGetQuery(db, sql)
        }
        
        else if (grepl("elements", table)){
          modules_ids <- r[[paste0(prefix, "_modules")]] %>% dplyr::pull(id)
          sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND module_id IN ({modules_ids*})", .con = db)
          r[[paste0(prefix, "_modules_elements")]] <- DBI::dbGetQuery(db, sql)
        }
        
        else {
          family_id <- r$studies %>% dplyr::filter(id == m$chosen_study) %>% dplyr::pull(paste0(prefix, "_module_family_id"))
          sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE deleted IS FALSE AND module_family_id = {family_id}", .con = db)
          r[[paste0(prefix, "_modules")]] <- DBI::dbGetQuery(db, sql)
        }
        
      }
    }
    
    else if (table %in% c("studies", "scripts")){
      
      tables <- tibble::tribble(~name, ~col_name, ~col_value,
        "studies", "datamart_id", r$chosen_datamart,
        "scripts", "data_source_id", r$datamarts %>% dplyr::filter(id == r$chosen_datamart) %>% dplyr::pull(data_source_id))
      
      row <- tables %>% dplyr::filter(name == table)
      
      sql <- glue::glue_sql("SELECT * FROM {`row$name`} WHERE deleted IS FALSE AND {`row$col_name`} = {row$col_value}", .con = db)
      r[[row$name]] <- DBI::dbGetQuery(db, sql)
    }
    
    else {
      
      r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    }
  }
}

#' Get options of a page
#' 
#' @description Get the options of a setting page (as datamarts, studies...)
#' @param id ID of the module / page 
#' @return A character vector with options
#' @examples 
#' get_page_options(id == "settings_datamarts")
get_page_options <- function(id = character()){
  result <- ""
  switch(id,
    "settings_datamarts" = c("show_only_aggregated_data", "users_allowed_read"),
    "settings_studies" = "users_allowed_read",
    "settings_plugins" = c("markdown_description", "users_allowed_read"),
    "settings_users_accesses_options" = "users_accesses_options",
    "settings_modules_patient_lvl_modules_families_options" = "users_allowed_read",
    "settings_modules_aggregated_modules_families_options" = "users_allowed_read") -> result
  result
}

#' Get column names
#' 
#' @param table_name Name of the table (character)
#' @param language Language used (charater)
#' @examples 
#' get_col_names(table_name = "datamarts", language = "EN")
get_col_names <- function(table_name = character(), language = "EN", words = tibble::tibble()){
  result <- ""
  
  if (table_name %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words))
    c(result, switch(table_name,
      "datamarts" = translate(language, "data_source", words),
      "studies" = c(translate(language, "datamart", words), translate(language, "patient_lvl_module_family", words),
        translate(language, "aggregated_module_family", words)),
      "subsets" = translate(language, "study", words),
      "thesaurus" = translate(language, "data_sources", words))) -> result
    result <- c(result, translate(language, "creator", words), translate(language, "datetime", words),
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "studies"){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words),
      translate(language, "datamart", words), translate(language, "patient_lvl_module_family", words),
      translate(language, "aggregated_module_family", words), translate(language, "creator", words), translate(language, "datetime", words),
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "studies_no_data"){
    result <- c(translate(language, "name", words), translate(language, "creator", words), translate(language, "datetime", words),
      translate(language, "action", words))
  }
  
  if (table_name == "thesaurus_items"){
    result <- c(translate(language, "id", words), translate(language, "thesaurus", words), translate(language, "item", words), translate(language, "name", words), 
      translate(language, "display_name", words), translate(language, "category", words), translate(language, "unit", words),
      translate(language, "datetime", words), translate(language, "deleted", words), translate(language, "action", words), translate(language, "modified", words))
  }
  
  if (table_name == "modules_thesaurus_items"){
    result <- c(translate(language, "id", words), translate(language, "thesaurus", words), translate(language, "item", words), translate(language, "name", words), 
      translate(language, "display_name", words), translate(language, "category", words), translate(language, "unit", words),
      translate(language, "colour", words), translate(language, "datetime", words), translate(language, "deleted", words),
      translate(language, "action", words), translate(language, "modified", words))
  }
  
  if (table_name == "thesaurus_items_with_counts"){
    result <- c(translate(language, "id", words), translate(language, "thesaurus", words), translate(language, "item", words), translate(language, "name", words), 
      translate(language, "display_name", words), translate(language, "category", words), translate(language, "unit", words),
      translate(language, "datetime", words), translate(language, "deleted", words),
      translate(language, "num_patients", words), translate(language, "num_rows", words),
      translate(language, "action", words), translate(language, "modified", words))
  }
  
  if (table_name == "modules_thesaurus_items_with_counts"){
    result <- c(translate(language, "id", words), translate(language, "thesaurus", words), translate(language, "item", words), translate(language, "name", words), 
      translate(language, "display_name", words), translate(language, "category", words), translate(language, "unit", words),
      translate(language, "item_colour", words), translate(language, "datetime", words), translate(language, "deleted", words),
      translate(language, "num_patients", words), translate(language, "num_rows", words),
      translate(language, "action", words), translate(language, "modified", words))
  }
  
  if (table_name == "datamart_thesaurus_items_with_counts"){
    result <- c(translate(language, "id", words), translate(language, "thesaurus", words), translate(language, "item", words), translate(language, "name", words), 
    translate(language, "display_name", words), translate(language, "category", words), translate(language, "unit", words),
    translate(language, "datetime", words), translate(language, "deleted", words),
    translate(language, "num_patients", words), translate(language, "num_rows", words),
    translate(language, "action", words), translate(language, "modified", words))
  }
  
  if (table_name == "plugins"){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words), translate(language, "module_type", words), 
      translate(language, "last_update", words),  translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "users"){
    result <- c(translate(language, "id", words), translate(language, "username", words), translate(language, "firstname", words), translate(language, "lastname", words),
      translate(language, "password", words), translate(language, "user_access", words), translate(language, "user_status", words), translate(language, "datetime", words), 
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name %in% c("users_accesses", "users_statuses")){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words), 
      translate(language, "datetime", words), translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name %in% c("patient_lvl_modules", "aggregated_modules")){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words), translate(language, "module_family", words),
      translate(language, "parent_module", words), translate(language, "display_order", words), translate(language, "creator", words), translate(language, "datetime", words), 
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "description", words),
      translate(language, "creator", words), translate(language, "datetime", words), 
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "patient_lvl_modules_elements"){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "module_family", words), 
      translate(language, "group", words), translate(language, "module", words), translate(language, "plugin", words), 
      translate(language, "thesaurus", words), translate(language, "thesaurus_item", words), translate(language, "display_name", words),
      translate(language, "unit", words), translate(language, "colour", words), translate(language, "display_order", words),
      translate(language, "creator", words), translate(language, "datetime", words),
      translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "aggregated_modules_elements"){
    result <- c(translate(language, "id", words), translate(language, "name", words), translate(language, "module_family", words),
      translate(language, "group", words), translate(language, "module", words), 
      translate(language, "plugin", words), translate(language, "display_order", words), translate(language, "creator", words),
      translate(language, "datetime", words), translate(language, "deleted", words), translate(language, "modified", words), translate(language, "action", words))
  }
  
  if (table_name == "log"){
    result <- c(translate(language, "id", words), translate(language, "category", words), translate(language, "name", words),
      translate(language, "value", words), translate(language, "user", words), translate(language, "datetime", words))
  }
  
  result
}

get_col_names_new <- function(table_name = character(), i18n = R6::R6Class()){
  result <- ""
  
  if (table_name %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"))
    c(result, switch(table_name,
      "datamarts" = i18n$t("data_source"),
      "studies" = c(i18n$t("datamart"), i18n$t("patient_lvl_module_family"),
        i18n$t("aggregated_module_family")),
      "subsets" = i18n$t("study"),
      "thesaurus" = i18n$t("data_sources"))) -> result
    result <- c(result, i18n$t("creator"), i18n$t("datetime"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "studies"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"),
      i18n$t("datamart"), i18n$t("patient_lvl_module_family"),
      i18n$t("aggregated_module_family"), i18n$t("creator"), i18n$t("datetime"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "studies_no_data"){
    result <- c(i18n$t("name"), i18n$t("creator"), i18n$t("datetime"),
      i18n$t("action"))
  }
  
  if (table_name == "thesaurus_items"){
    result <- c(i18n$t("id"), i18n$t("thesaurus"), i18n$t("item_id"), i18n$t("name"), 
      i18n$t("abbreviation"), i18n$t("category"), i18n$t("unit"),
      i18n$t("datetime"), i18n$t("deleted"), i18n$t("action"), i18n$t("modified"))
  }
  
  if (table_name == "modules_thesaurus_items"){
    result <- c(i18n$t("id"), i18n$t("thesaurus"), i18n$t("item_id"), i18n$t("name"), 
      i18n$t("abbreviation"), i18n$t("category"), i18n$t("unit"),
      i18n$t("colour"), i18n$t("datetime"), i18n$t("deleted"),
      i18n$t("action"), i18n$t("modified"))
  }
  
  if (table_name == "thesaurus_items_with_counts"){
    result <- c(i18n$t("id"), i18n$t("thesaurus"), i18n$t("item_id"), i18n$t("name"), 
      i18n$t("abbreviation"), i18n$t("category"), i18n$t("unit"),
      i18n$t("datetime"), i18n$t("deleted"),
      i18n$t("num_patients"), i18n$t("num_rows"),
      i18n$t("action"), i18n$t("modified"))
  }
  
  if (table_name == "modules_thesaurus_items_with_counts"){
    result <- c(i18n$t("id"), i18n$t("thesaurus"), i18n$t("item_id"), i18n$t("name"), 
      i18n$t("name_abbreviation"), i18n$t("category"), i18n$t("unit"),
      i18n$t("item_colour"), i18n$t("datetime"), i18n$t("deleted"),
      i18n$t("num_patients"), i18n$t("num_rows"),
      i18n$t("action"), i18n$t("modified"))
  }
  
  if (table_name == "datamart_thesaurus_items_with_counts"){
    result <- c(i18n$t("id"), i18n$t("thesaurus"), i18n$t("item_id"), i18n$t("name"), 
      i18n$t("abbreviation"), i18n$t("category"), i18n$t("unit"),
      i18n$t("datetime"), i18n$t("deleted"),
      i18n$t("num_patients"), i18n$t("num_rows"),
      i18n$t("action"), i18n$t("modified"))
  }
  
  if (table_name == "datamart_thesaurus_items_mapping"){
    result <- c(i18n$t("id"), i18n$t("thesaurus1"), i18n$t("item_id"), i18n$t("relation"), i18n$t("thesaurus2"), i18n$t("item_id"),
      i18n$t("creator"), i18n$t("datetime"), i18n$t("deleted"))
  }
  
  if (table_name == "datamart_thesaurus_items_mapping_evals"){
    result <- c(i18n$t("id"), i18n$t("thesaurus1"), i18n$t("item_id"), i18n$t("relation"), i18n$t("thesaurus2"), i18n$t("item_id"),
      i18n$t("creator"), i18n$t("datetime"), i18n$t("deleted"), i18n$t("positive_evals"), i18n$t("negative_evals"), 
      i18n$t("action"), i18n$t("user_evaluation_id"), i18n$t("modified"))
  }
  
  if (table_name == "plugins"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), i18n$t("module_type"), 
      i18n$t("updated_on"),  i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "users"){
    result <- c(i18n$t("id"), i18n$t("username"), i18n$t("firstname"), i18n$t("lastname"),
      i18n$t("password"), i18n$t("user_access"), i18n$t("user_status"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name %in% c("users_accesses", "users_statuses")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), 
      i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name %in% c("patient_lvl_modules", "aggregated_modules")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), i18n$t("module_family"),
      i18n$t("parent_module"), i18n$t("display_order"), i18n$t("creator"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name %in% c("patient_lvl_modules_families", "aggregated_modules_families")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"),
      i18n$t("creator"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "patient_lvl_modules_elements"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("module_family"), 
      i18n$t("group"), i18n$t("module"), i18n$t("plugin"), 
      i18n$t("thesaurus"), i18n$t("thesaurus_item"), i18n$t("abbreviation"),
      i18n$t("unit"), i18n$t("colour"), i18n$t("display_order"),
      i18n$t("creator"), i18n$t("datetime"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "aggregated_modules_elements"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("module_family"),
      i18n$t("group"), i18n$t("module"), 
      i18n$t("plugin"), i18n$t("display_order"), i18n$t("creator"),
      i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  if (table_name == "log"){
    result <- c(i18n$t("id"), i18n$t("category"), i18n$t("name"),
      i18n$t("value"), i18n$t("user"), i18n$t("datetime"))
  }
  
  if (table_name == "scripts"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), i18n$t("data_source_id"), i18n$t("creator"), 
      i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  result
}
