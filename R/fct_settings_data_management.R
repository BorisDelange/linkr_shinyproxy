#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

data_management_creation_card <- function(language, ns, title,
                                          textfields = NULL, textfields_width = "200px",
                                          dropdowns = NULL, dropdowns_width = "200px",
                                          data_sources = NULL, datamarts = NULL, studies = NULL, subsets = NULL,
                                          patient_lvl_module_families = NULL, aggregated_module_families = NULL){
  make_card(
    translate(language, title),
    div(
      shiny.fluent::Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 50),
        lapply(names(textfields), function(name){
          make_textfield(language, ns, textfields[name], id = name, width = textfields_width)
          # if (name == "description") make_textfield(language, ns, textfields[name], id = name, width = "400px")
          # else make_textfield(language, ns, textfields[name], id = name)
        }),
      ), 
      shiny.fluent::Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 50),
        lapply(names(dropdowns), function(name){
          dropdown_options <- switch(name, "data_source" = data_sources, "datamart" = datamarts, "study" = studies, "subset" = subsets,
                                     "patient_lvl_module_family" = patient_lvl_module_families, "aggregated_module_family" = aggregated_module_families)
          make_dropdown(language, ns, dropdowns[name], dropdown_options, id = name, width = dropdowns_width)
        })
      ),
      htmltools::br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
    )          
  )
}

data_management_management_card <- function(language, ns, title){
  make_card(translate(language, title),
    div(
      DT::DTOutput(ns("management_datatable")),
      htmltools::br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save"))
      # shiny.fluent::PrimaryButton.shinyInput(ns("management_edit"), "Edit")
    )
  )
}

data_management_elements_management <- function(data, editable_cols = NULL,
                                                data_sources = NULL, datamarts = NULL, studies = NULL, subsets = NULL,
                                                patient_lvl_module_families = NULL, aggregated_module_families = NULL){
  for (i in 1:nrow(data)){
    data[i, "Datamart"] <-
      as.character(
        div(shiny.fluent::Dropdown.shinyInput(paste0("datamart", i), options = list(
          list(key = "Weaning from mechanical ventilation", text = "Weaning from mechanical ventilation"),
          list(key = "My new study", text = "My new study"),
          list(key = "Heparin datamart Metavision", text = "Heparin datamart Metavision")
        ), value = as.character(data[i, "Datamart"]), style = "width:100%")
        )
      )
    data[i, "Patient-level data module family"] <-
      as.character(
        div(shiny.fluent::Dropdown.shinyInput(paste0("patient_lvl_module_family", i), options = list(
          list(key = "App default", text = "App default"),
          list(key = "Metavision default", text = "Metavision default"),
          list(key = "eHOP default", text = "eHOP default")
        ), value = as.character(data[i, "Patient-level data module family"]), style = "width:100%")
        )
      )
    data[i, "Aggregated data module family"] <-
      as.character(
        div(shiny.fluent::Dropdown.shinyInput(paste0("aggregated_module_family", i), options = list(
          list(key = "Default 1", text = "Default 1"),
          list(key = "Default 2", text = "Default 2")
        ), value = as.character(data[i, "Aggregated data module family"]), style = "width:100%")
        )
      )
    data[i, "Action"] <-
      as.character(shiny::actionButton(paste0("delete", i), "X", style = "color:red",
                                       onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
  }
  return(data)
}