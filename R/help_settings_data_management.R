help_settings_data_management <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_panel")]],
      br(),
      strong(""), br(), br(),
      shiny.fluent::Link("", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_settings_data_management_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_settings_data_management_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_settings_data_management_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  # What's a plugin ?
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("whats_a_plugin")
    r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
      
    )
  })

}