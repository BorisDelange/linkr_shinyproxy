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