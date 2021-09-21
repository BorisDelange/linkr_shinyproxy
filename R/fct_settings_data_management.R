#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# Toggle card                            #
##########################################

data_management_toggle_card <- function(language, ns, creation_card = "", datatable_card = "", edit_card = "", options_card = "", activated = ""){
  toggles <- tagList()
  sapply(c("creation_card", "datatable_card", "edit_card", "options_card"), function(card){
    label <- eval(parse(text = card))
    if (label != "") toggles <<- 
        tagList(toggles, make_toggle(language, ns, label = label, 
          id = paste0(card, "_toggle"), value = ifelse(card %in% activated, TRUE, FALSE), inline = TRUE))
  })
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

##########################################
# Creation card                          #
##########################################

data_management_creation_card <- function(language, ns, title, textfields = NULL, textfields_width = "200px", dropdowns = NULL, dropdowns_width = "200px"){
  div(id = ns("creation_card"),
    make_card(
      translate(language, title),
      div(
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),# wrap = TRUE,
          lapply(textfields, function(name){
            make_textfield(language, ns, name, id = name, width = textfields_width)#, margin_right = "50px")
          }),
        ),
        shiny.fluent::Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = 50),
          lapply(dropdowns, function(name){
            make_dropdown(language, ns, name, options = "", id = name, width = dropdowns_width)#, margin_right = "50px")
          })
        ),
        htmltools::br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
      )
    )
  )
}

##########################################
# Datatable card                         #
##########################################

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

##########################################
# Edit code card                         #
##########################################

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

##########################################
# Options card                           #
##########################################

data_management_options_card <- function(language, ns, r, category_filter, link_id_filter, title){
  options <- r$options %>% dplyr::filter(category == category_filter, link_id == link_id_filter)
  
  people_picker <- ""
  toggles <- ""
  dropdowns <- ""
  options_by_cat <- id_get_other_name(id, "options_by_cat")
  
  if("user_allowed_read" %in% options_by_cat){
    # List of users in the database
    form_options <- 
      r$users %>% 
      dplyr::filter(!deleted) %>%
      dplyr::transmute(key = id, imageInitials = paste0(substr(first_name, 0, 1), substr(last_name, 0, 1)),
                       text = paste0(first_name, " ", last_name), secondaryText = user_status)
    
    # Users already allowed
    value <- 
      form_options %>% 
      dplyr::mutate(n = 1:dplyr::n()) %>%
      dplyr::inner_join(
        options %>% 
          dplyr::filter(!deleted, name == "user_allowed_read") %>% 
          dplyr::select(key = value_num),
        by = "key"
      ) %>%
      dplyr::pull(key)
    people_picker <- make_people_picker(language, ns, paste0(id_get_other_name(id, "singular_form"), "_users_allowed_read"), 
        options = form_options, value = value, width = "100%")
  }
  
  if ("show_only_aggregated_data" %in% options_by_cat){
    value_show_only_aggregated_data <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num)
    toggles <- tagList(
      toggles,
      make_toggle(language, ns, 
                  label = "show_only_aggregated_data", 
                  id = paste0(id_get_other_name(id, "singular_form"), "_show_only_aggregated_data"), value = value_show_only_aggregated_data, inline = TRUE))
  }
  
  div(id = ns("options_card"),
      make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id_filter, ")"), style = "font-size: 15px;")),
        div(
          htmltools::br(), shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), toggles),
          people_picker, htmltools::br(),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), dropdowns),
          shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), translate(language, "save"))
        )
      )
  )
}

##########################################
# New data                               #
##########################################

data_management_new_data <- function(id, new_id, name, description, creator, datetime, deleted, 
                                     data_source_id = NULL, datamart_id = NULL, study_id = NULL,
                                     patient_lvl_module_family_id = NULL, aggregated_module_family_id = NULL){
  data <- tibble::tribble(~id, ~name, ~description, new_id, name, description)
  if (id == "settings_datamarts") data <- data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data_source_id))
  if (id == "settings_studies") data <- data %>% dplyr::bind_cols(
    tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
                    datamart_id, patient_lvl_module_family_id, aggregated_module_family_id))
  if (id == "settings_subsets") data <- data %>% dplyr::bind_cols(tibble::tribble(~study_id, study_id))
  data <- data %>% dplyr::bind_cols(tibble::tribble(~creator, ~datetime, ~deleted, creator, datetime, FALSE))
  data
}

##########################################
# Data                                   #
##########################################

data_management_data <- function(id, r){
  data <- r[[substr(id, nchar("settings_") + 1, nchar(id))]]
  if (nrow(data) != 0) data <- data %>% dplyr::filter(!deleted) %>% dplyr::select(-deleted)
  data
}

##########################################
# Datatable                             #
##########################################

data_management_datatable <- function(id, data, r, language, data_management_elements, dropdowns = NULL){
  if (nrow(data) == 0) return(data)
  
  # Create vars with existing options (ie : for data_sources, a list of existing data_sources in the database)
  sapply(data_management_elements, function(var_name){
    assign(var_name, tibble_to_list(r[[var_name]], "id", "name", rm_deleted_rows = TRUE), envir = .GlobalEnv)
  })
  
  # Add a column action in the DataTable
  data["action"] <- NA_character_
  
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

      # Add delete button
      actions <- tagList(shiny::actionButton(paste0("delete", data[i, 1]), "", icon = icon("trash-alt"),
                                             onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))
      
      # Add options button
      if (id %in% c("settings_datamarts", "settings_studies")) actions <- tagList(actions, shiny::actionButton(paste0("options", data[i, 1]), "", icon = icon("cog"),
                                                                                                               onclick = paste0("Shiny.setInputValue('", id, "-options', this.id, {priority: 'event'})")), "")

      # Add edit code button
      if (id == "settings_datamarts") actions <- tagList(actions, shiny::actionButton(paste0("edit_code", data[i, 1]), "", icon = icon("file-code"),
                                                                                       onclick = paste0("Shiny.setInputValue('", id, "-edit_code', this.id, {priority: 'event'})")), "")
      data[i, "action"] <- as.character(div(actions))
    }
  }
  
  # Change name of cols 
  colnames(data) <- id_get_other_name(id, "colnames_text_version", language = language)
  
  data
}

##########################################
# Datatable options                      #
##########################################

data_management_datatable_options <- function(data, id, option){
  if (nrow(data) == 0) return("")
  
  data <- data %>% dplyr::bind_rows(tibble::tibble(action = character()))
  
  # Non-sortabled columns : action & id columns (except first one)
  if (option == "non_sortable"){
    result <- c(which(grepl("id|action", names(data))) - 1)
    result <- result[!result %in% c(0)]
  }
  
  # Disabled columns
  else if (option == "disable"){
    result <- c(1:length(names(data)))
    result <- c(0, result[-which(grepl("name|description", names(data)))] - 1)
  }
  
  result
}

##########################################
# Delete react                           #
##########################################

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