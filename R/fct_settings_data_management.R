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
                                          data_sources = NULL, datamarts = NULL, studies = NULL, subsets = NULL){
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
          dropdown_options <- switch(name, "data_source" = data_sources, "datamart" = datamarts, "study" = studies, "subset" = subsets)
          make_dropdown(language, ns, dropdowns[name], dropdown_options, id = name, width = dropdowns_width)
        })
      ),
      htmltools::br(),
      shiny.fluent::PrimaryButton.shinyInput("add", translate(language, "add"))
    )          
  )
}

data_management_management_card <- function(language, ns, title){
  
}