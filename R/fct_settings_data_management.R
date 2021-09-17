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
      shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "save"))#,
      # shiny.fluent::PrimaryButton.shinyInput(ns("management_edit"), "Edit")
    )
  )
}

data_management_elements_management <- function(id, data, editable_cols = NULL, dropdowns = NULL,
                                                data_sources = NULL, datamarts = NULL, studies = NULL, subsets = NULL,
                                                patient_lvl_module_families = NULL, aggregated_module_families = NULL){
  
  data_sources <- tibble_to_list(data_sources, "Data source ID", "Data source name")
  
  # Add a column Action in the DataTable (eg : delete a row)
  data <- data %>% dplyr::bind_rows(tibble::tibble(`Action` = character()))
  
  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
  
  # For each row of the dataframe, transform dropdowns columns to show dropdowns in Shiny app & add an Action column with delete action button
  for (i in 1:nrow(data)){
    
    lapply(names(dropdowns), function(name){
      data[i, name] <<- as.character(
        div(shiny.fluent::Dropdown.shinyInput(paste0(dropdowns[name], i),
                                              options = eval(parse(text = dropdowns[name])), 
                                              value = as.integer(data[i, name])
                                              ),
            style = "width:100%"))
    })
    
    data[i, "Action"] <-
      as.character(shiny::actionButton(paste0("delete", i), "X", style = "color:red",
                                       onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
  }
  return(data)
}