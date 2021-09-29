#' settings 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# Toggle card                            #
##########################################

settings_toggle_card <- function(language, ns, creation_card = "", datatable_card = "", edit_code_card = "", options_card = "", activated = ""){
  toggles <- tagList()
  sapply(c("creation_card", "datatable_card", "edit_code_card", "options_card"), function(card){
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
# Delete react                           #
##########################################

# Code for the dialog box when the action button "delete" is pressed
settings_delete_react <- function(name, ns, language, delete_dialog){
  dialogContentProps <- list(
    type = 0,
    title = translate(language, paste0(name, "_delete")),
    closeButtonAriaLabel = "Close",
    subText = translate(language, paste0(name, "_delete_subtext"))
  )
  shiny.fluent::Dialog(
    hidden = !delete_dialog,
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", name, "_hide_dialog', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns(paste0(name, "_delete_confirmed")), text = translate(language, "delete")),
      shiny.fluent::DefaultButton.shinyInput(ns(paste0(name, "_delete_canceled")), text = translate(language, "dont_delete"))
    )
  )
}

##########################################
# Edit code card                         #
##########################################

settings_edit_code_card <- function(language, ns, type = "code", code, link_id, title){
  div(id = ns("edit_code_card"),
    make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        div(shinyAce::aceEditor(ns("ace_edit_code"), code, mode = "r", height = "400px"), style = "width: 100%;"),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), translate(language, "save")), " ",
        shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), 
        htmltools::br(), htmltools::br(),
        div(shiny::verbatimTextOutput(ns("code_result")), 
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
    )
  )
}