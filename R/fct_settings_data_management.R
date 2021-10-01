#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# Creation card                          #
##########################################

data_management_creation_card <- function(language, ns, title, prefix, textfields = NULL, textfields_width = "200px", dropdowns = NULL, dropdowns_width = "200px"){
  div(id = ns(paste0(prefix, "_creation_card")),
    make_card(
      translate(language, title),
      div(
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(textfields, function(name){
            make_textfield(language, ns, name, id = paste0(prefix, "_", name), width = textfields_width)
          })
        ),
        shiny.fluent::Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = 50),
          lapply(dropdowns, function(name){
            make_dropdown(language, ns, name, options = "", id = paste0(prefix, "_", name), width = dropdowns_width)
          })
        ),
        htmltools::br(),
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_add")), translate(language, "add"))
      )
    )
  )
}

##########################################
# Datatable card                         #
##########################################

data_management_datatable_card <- function(language, ns, title, prefix){
  div(id = ns(paste0(prefix, "_datatable_card")),
    make_card(translate(language, title),
      div(
        DT::DTOutput(ns(paste0(prefix, "_management_datatable"))),
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "datatable_save"), style = "top:-20px;")
      )
    )
  )
}

##########################################
# Options card                           #
##########################################

data_management_options_card <- function(language, ns, id, r, category_filter, link_id_filter, title, prefix){
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
      dplyr::left_join(r$users_accesses_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
      dplyr::transmute(key = id, imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
                       text = paste0(firstname, " ", lastname), secondaryText = user_status)
    # If this is study options, we have to show only users who have access to the parent datamart
    if(category_filter == "study"){
      datamart_id <- r$studies %>% dplyr::filter(id == link_id_filter) %>% dplyr::pull(datamart_id)
      users_allowed_datamart <- 
        r$options %>% 
        dplyr::filter(category == "datamart", link_id == datamart_id, name == "user_allowed_read") %>%
        dplyr::pull(value_num)
      form_options <- form_options %>% dplyr::filter(key %in% users_allowed_datamart)
    }

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
    people_picker <- make_people_picker(language, ns, paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_users_allowed_read"),
        options = form_options, value = value, width = "100%")
  }

  if ("show_only_aggregated_data" %in% options_by_cat){
    value_show_only_aggregated_data <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num)
    toggles <- tagList(
      htmltools::br(), 
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10),
        make_toggle(language, ns,
          label = "show_only_aggregated_data",
          id = paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_show_only_aggregated_data"), value = value_show_only_aggregated_data, inline = TRUE)
      )
    )
  }

  div(id = ns(paste0(prefix, "_options_card")),
      make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id_filter, ")"), style = "font-size: 15px;")),
        div(
          toggles, people_picker, htmltools::br(),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), dropdowns),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_options_save")), translate(language, "save"))
        )
      )
  )
}

##########################################
# New data                               #
##########################################

data_management_new_data <- function(prefix, new_id, name, description, creator_id, datetime = as.character(Sys.time()), deleted = FALSE, 
                                     data_source_id = NA_integer_, datamart_id = NA_integer_, study_id = NA_integer_,
                                     patient_lvl_module_family_id = NA_integer_, aggregated_module_family_id = NA_integer_){
  data <- tibble::tribble(~id, ~name, ~description, new_id, name, description)
  if (prefix == "datamarts") data <- data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data_source_id))
  if (prefix == "studies") data <- data %>% dplyr::bind_cols(
    tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
                    datamart_id, patient_lvl_module_family_id, aggregated_module_family_id))
  if (prefix == "subsets") data <- data %>% dplyr::bind_cols(tibble::tribble(~study_id, study_id))
  # if (prefix == "thesaurus")
  # if (prefix == "thesaurus_items")
  data <- data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, creator_id, datetime, FALSE))
  data
}

##########################################
# Data                                   #
##########################################

data_management_data <- function(id, r){
  data_var <- substr(id, nchar("settings_") + 1, nchar(id))
  data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var, " WHERE deleted IS FALSE"))
  if (nrow(data) != 0) data <- data %>% dplyr::select(-deleted)
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