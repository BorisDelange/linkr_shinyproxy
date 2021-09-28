#' settings_plugins 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plugins_toggle_card <- function(language, ns, activated = ""){
  toggles <- tagList()
  sapply(c("plugins_creation_card", "plugins_management_card", "plugins_code_card"), function(label){
    toggles <<- tagList(toggles, make_toggle(language, ns, label = label,
      id = paste0(label, "_toggle"), value = ifelse(label %in% activated, TRUE, FALSE), inline = TRUE))
  })
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}