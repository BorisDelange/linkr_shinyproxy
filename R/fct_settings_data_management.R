#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

data_management_toggle_cards <- function(language, ns, creation_card = "", datatable_card = "", edit_card = "", options_card = "", activated = ""){
  toggles <- tagList()
  sapply(c("creation_card", "datatable_card", "edit_card", "options_card"), function(card){
    if (eval(parse(text = card)) != "") toggles <<- tagList(toggles, shiny.fluent::Toggle.shinyInput(ns(paste0(card, "_toggle")), value = ifelse(card %in% activated, TRUE, FALSE)), 
                                                           div(class = "toggle_title", translate(language, eval(parse(text = card)))))
  })
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

data_management_creation_card <- function(language, ns, title,
                                          textfields = NULL, textfields_width = "200px",
                                          dropdowns = NULL, dropdowns_width = "200px",
                                          data_sources = NULL, datamarts = NULL, studies = NULL, subsets = NULL,
                                          patient_lvl_module_families = NULL, aggregated_module_families = NULL){
  div(id = ns("creation_card"),
    make_card(
      translate(language, title),
      div(
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),# wrap = TRUE,
          lapply(names(textfields), function(name){
            make_textfield(language, ns, textfields[name], id = name, width = textfields_width)#, margin_right = "50px")
          }),
        ),
        shiny.fluent::Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = 50),
          lapply(names(dropdowns), function(name){
            dropdown_options <- switch(name, "data_source" = data_sources, "datamart" = datamarts, "study" = studies, "subset" = subsets,
                                       "patient_lvl_module_family" = patient_lvl_module_families, "aggregated_module_family" = aggregated_module_families)
            make_dropdown(language, ns, dropdowns[name], dropdown_options, id = name, width = dropdowns_width)#, margin_right = "50px")
          })
        ),
        htmltools::br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
      )
    )
  )
}

data_management_datatable_card <- function(language, ns, title){
  div(id = ns("datatable_card"),
    make_card(translate(language, title),
      div(
        DT::DTOutput(ns("management_datatable")),
        shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), translate(language, "datatable_save"), style = "top:-20px;")
      )
    )
  )
}

data_management_edit_card <- function(language, ns, type = "code", code, link_id, title){
  div(id = ns("edit_card"),
    make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        div(shinyAce::aceEditor(ns("ace_edit_code"), code, "R", height = "400px"), style = "width: 100%;"),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_save"), translate(language, "edit_save"))
      )
    )
  )
}

data_management_data <- function(id, r){
  data <- switch(id, 
         "settings_data_sources" = r$data_sources_data,
         "settings_datamarts" = r$datamarts_data,
         "settings_studies" = r$studies_data,
         "settings_subsets" = r$subsets_data)
  
  if (nrow(data) != 0) data <- data %>% dplyr::filter(!Deleted) %>% dplyr::select(-Deleted)
  data
}

data_management_datatable <- function(id, data, r, dropdowns = NULL){
  if (nrow(data) == 0) return(data)
  
  data_sources <- tibble_to_list(r$data_sources_data, "Data source ID", "Data source name", rm_deleted_rows = TRUE)
  datamarts <- tibble_to_list(r$datamarts_data, "Datamart ID", "Datamart name", rm_deleted_rows = TRUE)
  studies <- tibble_to_list(r$studies_data, "Study ID", "Study name", rm_deleted_rows = TRUE)
  subsets <- tibble_to_list(r$subsets_data, "Subset ID", "Subset name", rm_deleted_rows = TRUE)
  patient_lvl_modules_families <- tibble_to_list(r$patient_lvl_modules_families, "Module family ID", "Module family name", rm_deleted_rows = TRUE)
  aggregated_modules_families <- tibble_to_list(r$aggregated_modules_families, "Module family ID", "Module family name", rm_deleted_rows = TRUE)
  
  # Add a column Action in the DataTable
  data["Action"] <- NA_character_
  
  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
  
  # For each row of the dataframe, transform dropdowns columns to show dropdowns in Shiny app & add an Action column with delete action button
  if (nrow(data) != 0){
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
      
      actions <- tagList(shiny::actionButton(paste0("delete", data[i, 1]), "", icon = icon("trash-alt"),
                                             onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
      if (id %in% c("settings_datamarts", "settings_studies")) actions <- tagList(actions, shiny::actionButton(paste0("options", data[i, 1]), "", icon = icon("cog"),
                                                                                                               onclick = paste0("Shiny.setInputValue('", id, "-options', this.id, {priority: 'event'})")), "")
      
      if (id == "settings_datamarts") actions <- tagList(actions, shiny::actionButton(paste0("edit_code", data[i, 1]), "", icon = icon("file-code"),
                                                                                       onclick = paste0("Shiny.setInputValue('", id, "-edit_code', this.id, {priority: 'event'})")), "")
      data[i, "Action"] <- as.character(div(actions))
    }
  }
  
  # Change name of ID cols (except the first one)
  data <- data %>% dplyr::rename_at(dplyr::vars(-1), ~stringr::str_replace_all(., "ID", "name"))
  
  data
}

data_management_datatable_options <- function(data, id, option){
  if (nrow(data) == 0) return("")
  
  data <- data %>% 
    dplyr::bind_rows(tibble::tibble(`Action` = character())) %>%
    dplyr::rename_at(dplyr::vars(-1), ~stringr::str_replace_all(., "ID", "name"))
  
  # Non-sortabled columns : Action & Name columns (except second one)
  if (option == "sortable"){
    result <- c(which(grepl("name|Action", names(data))) - 1)
    result <- result[!result %in% c(1)]
  }
  
  # Disabled columns
  else if (option == "disable"){
    result <- c(1:length(names(data)))
    regex <- switch(id, 
                    "settings_data_sources" = "Data source",
                    "settings_datamarts" = "Datamart",
                    "settings_studies" = "Study",
                    "settings_subsets" = "Subset")
    result <- c(0, result[-which(grepl(regex, names(data)))] - 1)
  }
  
  result
}

data_management_delete_react <- function(id, ns, language, data_management_delete_dialog){
  page <- switch(id, 
                 "settings_data_sources" = "data_source",
                 "settings_datamarts" = "datamart",
                 "settings_studies" = "study",
                 "settings_subsets" = "subset")
  
  dialogContentProps <- list(
    type = 0,
    title = translate(language, paste0(page, "_delete")),
    closeButtonAriaLabel = "Close",
    subText = translate(language, paste0(page, "_delete_subtext"))
  )
  shiny.fluent::Dialog(
    hidden = !data_management_delete_dialog,
    onDismiss = htmlwidgets::JS("function() { Shiny.setInputValue('hideDialog', Math.random()); }"),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns("management_delete_confirmed"), text = "Delete"),
      shiny.fluent::DefaultButton.shinyInput(ns("management_delete_canceled"), text = "Don't delete")
    )
  )
}