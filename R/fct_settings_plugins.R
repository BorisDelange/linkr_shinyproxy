#' settings_plugins 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# Datatable                              #
##########################################

plugins_management_datatable <- function(data, ns, r, language, prefix, dropdowns = NULL){
  if (nrow(data) == 0) return(data)
  
  # Add a column action in the DataTable
  data["action"] <- NA_character_
  
  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
  
  modules_types <- list(
    list(key = "patient_lvl_data", text = translate(language, "patient_level_data")),
    list(key = "aggregated_data", text = translate(language, "aggregated_data")))
  
  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button
  if (nrow(data) != 0){
    for (i in 1:nrow(data)){
      lapply(names(dropdowns), function(name){
        data[i, name] <<- as.character(
          div(
            shiny.fluent::Dropdown.shinyInput(ns(paste0(prefix, "_", name, data[i, "id"])),
            options = eval(parse(text = dropdowns[name])),
            value = data[i, name]),
            style = "width:100%")
        )
      })
      
      # Add delete button
      actions <- tagList(shiny::actionButton(paste0(prefix, "_delete_", data[i, "id"]), "", icon = icon("trash-alt"),
        onclick = paste0("Shiny.setInputValue('settings_plugins-", prefix, "_deleted_pressed', this.id, {priority: 'event'})")),
      shiny::actionButton(paste0(prefix, "_edit_code_", data[i, "id"]), "", icon = icon("file-code"),
        onclick = paste0("Shiny.setInputValue('settings_plugins-", prefix, "_edit_code', this.id, {priority: 'event'})")))
      
      data[i, "action"] <- as.character(div(actions))
    }
  }
  
  # Change name of cols 
  colnames(data) <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
    translate(language, "module_type"), translate(language, "datetime"), translate(language, "action"))
  
  data
}