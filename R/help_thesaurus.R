help_thesaurus <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_thesaurus_open_panel,
      br(),
      strong(i18n$t("thesaurus_and_concepts")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_thesaurus"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("thesaurus_concepts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong(i18n$t("thesaurus_concepts_mapping")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_concept_mapping"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_and_evaluate_concept_mapping"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_thesaurus_open_panel_light_dismiss,
      isBlocking = r$help_thesaurus_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_thesaurus_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_thesaurus_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_thesaurus_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_thesaurus_open_modal <- TRUE
    r$help_thesaurus_open_panel_light_dismiss <- FALSE
  }
  
  # What's a thesaurus ?
  
  observeEvent(r$help_thesaurus_page_1, {
    
    load_help_page(r)
    
    r$help_thesaurus_modal_title <- i18n$t("whats_a_thesaurus")
    r$help_thesaurus_modal_text <- div(
      p(strong("1) Thésaurus")),
      p("Un ", strong("thésaurus"), " est un ", strong("dictionnaire de concepts"), ", associant des ", strong("codes"), " à des ", strong("noms"), "."),
      p("Quelques exemples de concepts issus d'un thésaurus :"),
      tags$ul(
        tags$li("42503 - Fréquence cardiaque : ici le concept ayant pour code 42503 est associé au nom Fréquence cardiaque"),
        tags$li("800902 - Noradrénaline : ici le concept ayant pour code 800902 est associé au nom Noradrénaline")
      ),
      p("Un ", strong("entrepôt de données de santé"), " stocke des données en associant des ", strong("valeurs"), " à des ", strong("concepts"),"."),
      p("Quelques exemples :"),
      tags$ul(
        tags$li("patient 409 - datetime 13-01-2022 14:44:32 - concept 42503 - valeur 54"),
        tags$li("ici le concept ayant pour code 42503 (Fréquence cardiaque) est associé à la valeur 54 pour le patient 409 à la date indiquée.", style = "margin-left:20px;"),
        tags$li("patient 311 - datetime 12-02-2022 09:41:33 - concept 800902 - valeur 1.25"),
        tags$li("ici le concept ayant pour code 800902 (Noradrénaline) est associé à la valeur 1.25 pour le patient 311 à la date indiquée.", style = "margin-left:20px;")
      ),
      p(strong("2) Thésaurus standards")),
      p("Les logiciels médicaux utilisent classiquement chacun un thésaurus qui leur est propre."),
      p("Des ", strong("thésaurus standards"), " ont été crées pour permettre ", strong("d'homogénéiser"), " les entrepôts de données",
        " et permettre leur ", strong("interopérabilité"), "."),
      p("Quelques exemples de thésaurus standardisés :"),
      tags$ul(
        tags$li("LOINC"),
        tags$li("SNOMED"),
        tags$li("RxNorm")
      ),
      p(strong("Athena"), " est un moteur de recherche de concepts, rassemblant plusieurs thésaurus. Vous pouvez y accéder ",
        shiny.fluent::Link(href = "https://athena.ohdsi.org/", "via ce lien", target = "_blank"), "."),
      br()
    )
  })
  
  # Thesaurus concepts
  
  observeEvent(r$help_thesaurus_page_2, {
    
    load_help_page(r)
    
    r$help_thesaurus_modal_title <- i18n$t("thesaurus_concepts")
    r$help_thesaurus_modal_text <- div(
      p(strong("1) Choisir un thésaurus")),
      p("Depuis l'onglet ", tags$em("Items"), ", choisissez un thésaurus dans le menu déroulant."),
      p("L'ensemble des concepts de ce thésaurus est affiché dans le tableau."),
      p(strong("2) Filtrer les concepts utilisés")),
      p("Vous pouvez filtrer les concepts utilisés dans le set de données actuel en cochant la case ", tags$em("N'afficher que les items utilisés dans ce set de données"), "."),
      p("La colonne ", tags$em("Patients"), " renseigne le nombre de patients ayant au moins une fois ce concept."),
      p("La colonne ", tags$em("Lignes"), " renseigne le nombre de lignes dans le set de données concernant ce concept."),
      p(strong("3) Chercher un concept")),
      p("Vous pouvez rechercher un concept via la barre de recherche, en haut de la colonne ", tags$em("Nom"), "."),
      p(strong("4) Modiifer un concept")),
      p("Vous pouvez modifier le nom d'un concept en double cliquant sur le nom, en le modifiant puis en cliquant sur ", tags$em("Sauvegarder"), ".")
    )
  })
  
  # What's a concept mapping ?
  
  observeEvent(r$help_thesaurus_page_3, {
    
    load_help_page(r)
    
    r$help_thesaurus_modal_title <- i18n$t("whats_a_concept_mapping")
    r$help_thesaurus_modal_text <- div(
      p("Il est fréquent que les différents thésaurus comportent des ", strong("concepts similaires"), "."),
      p("Pour faire en sorte de ", strong("faciliter les requêtes"), " sur les bases de données, nous procédons à un ", strong("alignement de concepts"), "."),
      p("Il s'agit d'indiquer que tel concept de tel thésaurus est lié à tel autre concept de tel autre thésaurus."),
      p("Par exemple :"),
      tags$ul(
        tags$li("Thésaurus MIMIC-IV, Concept 220045 - Fréquence cardiaque"),
        tags$li("Thésaurus LOINC, Concept 8867-4 - Fréquence cardiaque")
      ),
      p("Nous pouvons ", strong("lier"), " ces deux items par un code indiquant qu'ils sont ", strong("similaires"), "."),
      p("Il est également possible d'indiquer des ", strong("relations de type hiérarchique"), "."),
      p("Par exemple :"),
      tags$ul(
        tags$li("Thésaurus SNOMED, Concept 102594003 - Anomalie à l'ECG"),
        tags$li("Thésaurus SNOMED, Concept 164893009 - Arythmie ventriculaire")
      ),
      p("Nous pouvons ", strong("lier"), " ces deux items par un code indiquant que le deuxième item est ", strong("inclus"), " dans le premier item."),
      br()
    )
  })
  
  # Add and evaluate a thesaurus concept mapping
  
  observeEvent(r$help_thesaurus_page_4, {
    
    load_help_page(r)
    
    r$help_thesaurus_modal_title <- i18n$t("add_and_evaluate_concept_mapping")
    r$help_thesaurus_modal_text <- div(
      p(strong("1) Ajouter un alignement de concepts")),
      p("Pour ajouter un alignement de concepts, ", strong("sélectionnez"), " dans les menus déroulants les ", strong("deux thésaurus"), "."),
      p("Vous pouvez filtrer les concepts en faisant une recherche sur le nom."),
      p("Cliquez sur les concepts dans le tableau pour les sélectionner."),
      p("Une fois un ", strong("concept de chaque thésaurus sélectionné"), ", choisissez la ", strong("relation"), " unissant les deux concepts puis cliquez sur ", tags$em("Ajouter"), "."),
      p(strong("2) Evaluer un alignement de concepts")),
      p("Pour évaluer un alignement de concepts, allez dans l'onglet ", tags$em("Evaluer & éditer"), "."),
      p("Cliquez sur :"),
      p(shiny::actionButton("thumbs_up_button_help", "", icon = icon("thumbs-up")), " pour indiquer un alignement conforme,"),
      p(shiny::actionButton("thumbs_down_button_help", "", icon = icon("thumbs-down")), " pour indiquer un alignement non conforme."),
      p("Pensez à sauvegarder l'évaluation."),
      p(strong("3) Supprimer un alignement de concepts")),
      p("Pour supprimer un ou plusieurs alignements de concepts, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
      p("Vous pouvez également supprimer un alignement de concepts en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
      br()
    )
  })
  
}