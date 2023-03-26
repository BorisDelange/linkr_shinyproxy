help_settings_data_management <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("data_sources")), br(), br(),
      shiny.fluent::Link(i18n$t("data_sources_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      strong(i18n$t("datamarts")), br(), br(),
      shiny.fluent::Link(i18n$t("datamarts_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_datamart_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("datamart_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      strong(i18n$t("thesaurus")), br(), br(),
      shiny.fluent::Link(i18n$t("thesaurus_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_thesaurus_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("thesaurus_items"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
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
  
  # Data sources management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("data_sources_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Ajouter une source de données")),
        p("Pour créer une source de données, entrez un nom et cliquez sur ", tags$em("Ajouter"), "."),
        p("Une source de données comprend ", strong("plusieurs sets de données"), "."),
        p("Par exemple, vous créez une source de données nommée ", tags$em("MIMIC-IV"), ", qui comprendra les sets de données créés à partir de la base de données MIMIC-IV."),
        p("Différents sets de données seront créés à partir de cette source, par exemple "),
        tags$ul(
          tags$li("un set ", tags$em("Infections nosocomiales"), " qui comprendra les patients ayant eu une infection nosocomiale au cours de leur séjour hospitalier"),
          tags$li("un set ", tags$em("Hépatites"), " qui comprendra les patients ayant été diagnostiqué d'une hépatite")
        ),
        p(strong("2) Gérer les sources de données")),
        p("Vous pouvez modifier le nom des sources de données en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer une ou plusieurs sources de données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une source de données en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Datamarts management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datamarts_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Edit datamart code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_datamart_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Datamart options
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datamart_options")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Thesaurus management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("thesaurus_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Edit thesaurus code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_thesaurus_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Thesaurus items
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_7")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("thesaurus_items")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
}