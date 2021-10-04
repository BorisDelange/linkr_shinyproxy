#' settings_modules 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

settings_modules_get_table <- function(prefix, module_type){
  switch(prefix,
    "patient_lvl" = switch(module_type, "module" = "patient_lvl_modules", "family" = "patient_lvl_module_families"),
    "aggregated" = switch(module_type, "module" = "aggregated_modules", "family" = "aggregated_module_families"))
}

settings_modules_get_dropdowns <- function(prefix, module_type){
  switch(prefix,
    "patient_lvl" =
      switch(module_type,
        "module" = c("module_family_id" = "patient_lvl_module_families", "parent_module_id" = "patient_lvl_modules"),
        "family" = ""),
    "aggregated" =
      switch(module_type,
        "module" = c("module_family_id" = "aggregated_module_families", "parent_module_id" = "aggregated_modules"),
        "family" = ""))
}