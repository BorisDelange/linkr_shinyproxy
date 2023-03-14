help_my_subsets <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_my_subsets_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("subsets_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_subset_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("subset_patients"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_my_subsets_open_panel_light_dismiss,
      isBlocking = r$help_my_subsets_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_my_subsets_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_my_subsets_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_my_subsets_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_my_subsets_open_modal <- TRUE
    r$help_my_subsets_open_panel_light_dismiss <- FALSE
  }
  
  # Subsets management
  
  observeEvent(r$help_my_subsets_page_1, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("subsets_management")
    r$help_my_subsets_modal_text <- div(
      p(strong("1) Créer un subset")),
      p("Pour créer un subset, allez dans l'onglet ", tags$em("Gestion des subsets"), "."), 
      p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
      p(strong("2) Changer le nom d'un subset")),
      p("Pour changer le nom d'un subset, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
      p(strong("3) Supprimer un ou des subsets")),
      p("Pour supprimer un ou plusieurs subsets, sélectionnez-les en cliquant dessus dans le datatable puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
      p("Vous pouvez également supprimer un subset en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
      p(strong("4) Editer le code d'un subset")),
      p("Cliquez sur"),
      p(shiny::actionButton("edit_code_button_help", "", icon = icon("file-code")), " pour ", strong("éditer le code"), " du subset,"),
      p(shiny::actionButton("subset_patients_button_help", "", icon = icon("table")), " pour ", strong("gérer les patients"), " du subset."),
      br()
    )
  })
  
  # Edit subset code
  
  observeEvent(r$help_my_subsets_page_2, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("edit_subset_code")
    r$help_my_subsets_modal_text <- div(
      p("Vous pouvez ici ajouter ou supprimer des patients d'un subset en utilisant les fonctions :"),
      tags$ul(
        tags$li(strong("add_patients_to_subset"), " pour ajouter des patients à un subset"),
        tags$li(strong("remove_patients_from_subset"), " pour retirer des patients d'un subset")
      ),
      p("Ceci permet de sélectionner les patients de ", strong("façon plus ciblée"), " qu'avec le tableau listant tous les patients dans l'onglet ", tags$em("Patients du subset"), "."),
      p("Voici un exemple de code :"),
      div(
        span("# Sélection des patients depuis la variable d$person"), br(),
        span("patients <- d$person %>% dplyr::select(person_id) %>% dplyr::mutate_at(\"person_id\", as.integer)"), br(), br(),
        span("# Ajout des patients au subset sélectionné dans le menu déroulant"), br(),
        span("add_patients_to_subset(output = output, m = m, patients = patients, subset_id = %subset_id%, i18n = i18n, ns = ns)"), br(), br(),
        span("# Supprimer des patients du subset"), br(),
        span("remove_patients_from_subset(output = output, m = m, patients = patients, subset_id = %subset_id%, i18n = i18n, ns = ns)"),
        style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
      ),
      p("Remarquez l'utilisation de la balise ", strong("%subset_id%"), ", qui sera remplacée par l'ID du subset sélectionné dans le menu déroulant."),
      br()
    )
  })
  
  # Subset patients
  
  observeEvent(r$help_my_subsets_page_3, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("subset_patients")
    r$help_my_subsets_modal_text <- div(
      p(strong("1) Ajouter des patients au subset")),
      p("Pour ajouter des patients au subset sélectionné dans le menu déroulant, ", strong("sélectionnez les patients dans le datatable du haut"),
        " puis cliquez sur ", tags$em("Ajouter"), "."),
      p(strong("2) Supprimer des patients du subset")),
      p("De la même façon, ", strong("sélectionnez les patients dans le datatable du bas"), 
        " puis cliquez sur ", tags$em("Supprimer la sélection"), " pour retirer les patients du subset."),
      br()
    )
  })
}