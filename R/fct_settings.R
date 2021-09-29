#' settings 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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