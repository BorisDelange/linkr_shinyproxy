#' settings_plugins 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plugins_toggle_card <- function(language, ns, activated = ""){
  toggles <- tagList()
  sapply(c("plugins_creation_card", "plugins_management_card", "plugins_code_card"), function(label){
    toggles <<- tagList(toggles, make_toggle(language, ns, label = label,
      id = paste0(label, "_toggle"), value = ifelse(label %in% activated, TRUE, FALSE), inline = TRUE))
  })
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

##########################################
# Datatable                              #
##########################################

plugins_management_datatable <- function(data, ns, r, language, dropdowns = NULL){
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
            shiny.fluent::Dropdown.shinyInput(ns(paste0(name, data[i, "id"])),
                                              options = eval(parse(text = dropdowns[name])),
                                              value = data[i, name]),
            style = "width:100%")
        )
      })
      
      # Add delete button
      actions <- tagList(shiny::actionButton(paste0("delete", data[i, "id"]), "", icon = icon("trash-alt"),
                                             onclick = paste0("Shiny.setInputValue('settings_plugins-plugins_deleted_pressed', this.id, {priority: 'event'})")),
                         shiny::actionButton(paste0("edit_code", data[i, "id"]), "", icon = icon("file-code"),
                                             onclick = paste0("Shiny.setInputValue('settings_plugins-plugnis_edit_code', this.id, {priority: 'event'})")))
      
      data[i, "action"] <- as.character(div(actions))
    }
  }
  
  # Change name of cols 
  colnames(data) <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
                      translate(language, "module_type"), translate(language, "datetime"), translate(language, "action"))
  
  data
}

##########################################
# Edit code card                         #
##########################################

plugins_edit_card <- function(language, ns, type = "code", code, link_id){
  div(id = ns("plugins_code_card"),
    make_card(tagList(translate(language, "plugins_code"), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        div(shinyAce::aceEditor(ns("ace_edit_code"), code, mode = "r", height = "400px"), style = "width: 100%;"),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_save"), translate(language, "edit_save")), " ",
        shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), 
        htmltools::br(), htmltools::br(),
        div(shiny::verbatimTextOutput(ns("code_result")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
    )
  )
}