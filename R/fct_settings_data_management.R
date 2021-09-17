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

data_management_data <- function(id, r){
  switch(id, 
         "settings_data_sources" = r$data_sources_data %>% dplyr::select(-`Deleted`),
         "settings_datamarts" = r$datamarts_data %>% dplyr::select(-`Deleted`),
         "settings_studies" = r$studies_data %>% dplyr::select(-`Deleted`),
         "settings_subsets" = r$subsets_data %>% dplyr::select(-`Deleted`))
}

data_management_datatable <- function(id, data, r, dropdowns = NULL){
  
  data_sources <- tibble_to_list(r$data_sources_data, "Data source ID", "Data source name")
  datamarts <- tibble_to_list(r$datamarts_data, "Datamart ID", "Datamart name")
  studies <- tibble_to_list(r$studies_data, "Study ID", "Study name")
  
  # Add a column Action in the DataTable (eg : delete a row)
  data <- data %>% dplyr::bind_rows(tibble::tibble(`Action` = character()))
  
  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
  
  # For each row of the dataframe, transform dropdowns columns to show dropdowns in Shiny app & add an Action column with delete action button
  for (i in 1:nrow(data)){
    
    lapply(names(dropdowns), function(name){
      data[i, name] <<- as.character(
        div(
          shiny.fluent::Dropdown.shinyInput(paste0(dropdowns[name], i),
                                            options = eval(parse(text = dropdowns[name])), 
                                            value = as.integer(data[i, name])),
          style = "width:100%")
        )
    })
    
    data[i, "Action"] <- as.character(
      shiny::actionButton(paste0("delete", i), "X", style = "color:red",
                          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
  }
  
  # Change name of ID cols (except the first one)
  data <- data %>% dplyr::rename_at(dplyr::vars(-1), ~stringr::str_replace_all(., "ID", "name"))
  
  return(data)
}

data_management_datatable_options <- function(data, option){
  data <- data %>% 
    dplyr::bind_rows(tibble::tibble(`Action` = character())) %>%
    dplyr::rename_at(dplyr::vars(-1), ~stringr::str_replace_all(., "ID", "name"))
  
  # Columns sortable : all except Action & ID 
  if (option == "sortable"){ 
    # result <- c(1:length(names(data)))
    # result <- result[-c(1, which(grepl("name|description|Creator|Date", names(data))))] - 1
    result <- which(grepl("ID|Action", names(data))) - 1
  }
  result
}