help_plugins <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_plugins_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("all_plugins")), br(), br(),
      shiny.fluent::Link(i18n$t("local_plugins"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("git_remote_plugins"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong(i18n$t("create_and_test_a_plugin")), br(), br(),
      shiny.fluent::Link(i18n$t("create_a_plugin"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_plugin_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("plugin_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      strong(i18n$t("import_and_export_plugins")), br(), br(),
      shiny.fluent::Link(i18n$t("import_plugin"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("export_plugin"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r[[paste0("help_plugins_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_plugins_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_plugins_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_plugins_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- "Help page 1"
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p("Paragraph 1")
    )
  })
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- "Help page 2"
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p("Paragraph 2")
    )
  })
}