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
  tables <- c("users", "users_accesses_statuses", "users_accesses_details",
    "data_sources", "datamarts", "studies", "subsets", "thesaurus", "thesaurus_items",
    "plugins", "patient_lvl_module_families", "patient_lvl_modules", "patient_lvl_module_elements",
    "aggregated_module_families", "aggregated_modules", "code", "options")
  
  if (table %not_in% tables) stop(paste0(translate(language, "invalid_table_name"), ". ", translate(language, "tables_allowed"), " : ", toString(tables)))
  
  new_table <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
  r[[table]] <- new_table
  r[[paste0(table, "_temp")]] <- new_table %>% dplyr::mutate(modified = FALSE)
}