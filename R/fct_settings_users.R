#' settings_users 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

users_toggle_card <- function(language, ns, activated = ""){
  toggles <- tagList()
  sapply(c("add_user_card", "users_management_card", "add_access_card", "accesses_management_card",
           "add_status_card", "statuses_management_card"), function(label){
    toggles <<- tagList(toggles, make_toggle(language, ns, label = label,
    id = paste0(label, "_toggle"), value = ifelse(label %in% activated, TRUE, FALSE), inline = TRUE))
  })
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

users_creation_card <- function(language, ns, title, card, textfields = NULL, textfields_width = "200px", dropdowns = NULL, dropdowns_width = "200px"){
  div(id = ns(paste0(card, "_card")),
    make_card(
      translate(language, title),
      div(
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(textfields, function(name){
            if (name == "password") textfield <- make_textfield(language, ns, name, id = paste0(card, "_", name), width = textfields_width, type = "password", canRevealPassword = TRUE)
            if (name != "password") textfield <- make_textfield(language, ns, name, id = paste0(card, "_", name), width = textfields_width)
            textfield
          }),
        ),
        shiny.fluent::Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = 50),
          lapply(dropdowns, function(name){
            make_dropdown(language, ns, name, options = "", id = paste0(card, "_", name), width = dropdowns_width)
          })
        ),
        htmltools::br(),
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(card, "_add")), translate(language, "add"))
      )
    )
  )
}

users_datatable_card <- function(language, ns, title, card){
  div(id = ns(paste0(card, "_card")),
    make_card(translate(language, title),
      div(
        DT::DTOutput(ns(paste0(card, "_datatable"))),
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(card,"_save")), translate(language, "save"), style = "top:-20px;")
      )
    )
  )
}

users_edit_card <- function(language, ns, title, card){
  div(id = ns(paste0(card, "_card")),
    make_card(translate(language, title),
      div(
        make_dropdown(language, ns, "accesses_management_access", width = "300px")
      )          
    )
  )
}

##########################################
# Datatable                              #
##########################################

users_management_datatable <- function(id, data, ns, r, language, dropdowns = NULL){
  if (nrow(data) == 0) return(data)
  
  # Create vars with existing options
  users_accesses <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "access"), "id", "name", rm_deleted_rows = TRUE)
  users_statuses <- tibble_to_list(r$users_accesses_statuses %>% dplyr::filter(type == "status"), "id", "name", rm_deleted_rows = TRUE)
  
  # Add a column action in the DataTable
  data["action"] <- NA_character_
  
  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
  
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
                                              value = as.integer(data[i, name])),
            style = "width:100%")
        )
      })
      
      # Add delete button
      actions <- tagList(shiny::actionButton(paste0(id, "_delete", data[i, "id"]), "", icon = icon("trash-alt"),
                                             onclick = paste0("Shiny.setInputValue('settings_users-users_", id, "_deleted_pressed', this.id, {priority: 'event'})")))
      
      data[i, "action"] <- as.character(div(actions))
    }
  }
  
  # Change name of cols 
  colnames(data) <- switch(id,
   "users" = c(translate(language, "id"), translate(language, "username"), translate(language, "firstname"),
               translate(language, "lastname"), translate(language, "password"), translate(language, "user_access"),
               translate(language, "user_status"), translate(language, "datetime"), translate(language, "action")),
   "statuses" = c(translate(language, "id"), translate(language, "name"), translate(language, "description"),
                  translate(language, "datetime"), translate(language, "action")))
  
  data
}