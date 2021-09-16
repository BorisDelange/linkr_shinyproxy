#' settings_data_management 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

data_management_creation_card <- function(language, ns, title, name = NULL, description = NULL){
  textfields <- c(name = name, description = description)
  
  make_card(
    translate(language, title),
    div(
      shiny.fluent::Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 50),
        lapply(names(textfields), function(name){
          if (name == "description") make_textfield(language, ns, textfields[name], id = name, width = "400px")
          else make_textfield(language, ns, textfields[name], id = name)
        }),
      ), br(),
      shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
    )          
  )
}