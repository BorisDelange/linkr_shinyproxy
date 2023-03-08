help_plugins <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_plugins_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("plugins")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_plugin"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("local_and_remote_plugins"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong(i18n$t("create_and_test_a_plugin")), br(), br(),
      shiny.fluent::Link(i18n$t("plugins_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
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
  
  # What's a plugin ?
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("whats_a_plugin")
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p("Les plugins sont des scripts écrits en R - Shiny permettant ", strong("d'ajouter des fonctionnalités à l'application"), "."),
      p(strong("1) Plugins de données individuelles")),
      p("Les plugins de données individuelles permettent d'afficher les données, patient par patient, sous un format particulier."),
      p("Quelques exemples :"),
      tags$ul(
        tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, par exemple pour représenter les données de prescription."),
        tags$li(strong("Plugin Datatable"), " : permet d'afficher les données sous forme de tableau."),
        tags$li(strong("Plugin Texte"), " : permet d'afficher les données textuelles, de filter le texte et de faire des recherches avec des mots clés.")
      ),
      p(strong("2) Plugins de données agrégées")),
      p("Les plugins de données agrégées permettent de visualiser et d'analyser des données sur un groupe de patient."),
      p("Quelques exemples :"),
      tags$ul(
        tags$li(strong("Plugin Inclusion / exclusion"), " : permet de créer des critères d'inclusion et d'exclusion et de les appliquer aux patients de mon étude."),
        tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir de critères d'inclusion et d'exclusion."),
        tags$li(strong("Plugin Données aberrantes"), " : permet d'analyser les données item par item, de déterminer quelles sont les données aberrantes et les exclure de l'étude."),
        tags$li(strong("Plugin Rapport"), " : permet d'écrire un rapport en intégrant les figures de l'étude, qui se mettront à jour dynamiquement (ex : figure du plugin Flowchart).")
      ), br()
    )
  })
  
  # Local & remote plugins
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("local_and_remote_plugins")
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p("Dans l'onglet ", tags$em("Tous les plugins"), ", vous avez accès aux plugins locaux et à des plugins sur git distant."),
      p(strong("1) Plugins locaux")),
      p("Ici sont répertoriés l'ensemble des plugins ", strong("disponibles sur votre instance"), " de l'application."),
      p("Ces plugins sont soit des plugins que vous avez ", strong("crées localement"), ", soit des plugins ", strong("téléchargés"), " depuis un git distant."),
      p("Pour obtenir une ", strong("description du plugin"), ", cliquez sur le nom ou l'image du plugin."),
      p("Cliquez sur ", tags$em("Rafraîchir"), " pour mettre à jour la liste, si des plugins ont été ajoutés ou modifiés entre-temps."),
      p(strong("2) Plugins sur git distant")),
      p("Vous pouvez accéder à une liste de plugins disponibles sur un git distant (fichiers stockés sur un site type github.com, framagit.org...)."),
      p("Pour obtenir une ", strong("description du plugin"), ", cliquez sur le nom ou l'image du plugin."),
      p("Une fois la description d'un plugin ouvert, vous pouvez le ", strong("télécharger"), " ou le ", strong("mettre à jour"), "."),
      p("Ce plugin sera alors disponible dans la rubrique ", tags$em("Plugins locaux"), " et vous pourrez l'utiliser dans vos études."), br()
    )
  })
  
  # Create a plugin
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("plugins_management")
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p(strong("1) Créer un plugin")),
      p("Pour créer un plugin, allez dans l'onglet ", tags$em("Gestion des plugins"), "."), 
      p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
      p(strong("2) Supprimer un ou des plugins")),
      p("Pour supprimer un ou plusieurs plugins, sélectionnez-les en cliquant dessus dans le datatable puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
      p("Vous pouvez également supprimer un plugin en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
      p(strong("3) Editer le code ou les options d'un plugin")),
      p("Cliquez sur"),
      p(shiny::actionButton("delete_button_help", "", icon = icon("cog")), " pour ", strong("éditer les options"), " du plugin"),
      p(shiny::actionButton("delete_button_help", "", icon = icon("table")), " pour ", strong("éditer le code"), " du plugin"),
      br()
    )
  })
  
  # Edit plugin code
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("edit_plugin_code")
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p("Dans cette rubrique, vous pouvez ", strong("écrire le code"), " d'un plugin et le ", strong("tester"), " en chargeant au préalable des données."),
      p("Pour les plugins de données individuelles, il faut charger les données d'un patient. Pour les plugins de données agrégées, charger les données d'une étude suffit."),
      p(strong("1) UI (user interface) code")),
      p("Vous codez ici ", strong("l'interface utilisateur"), " du plugin."),
      p("Voici un exemple de code :"),
      div(
        span("tagList("), br(),
          span("shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),", style = "padding-left:20px;"), br(),
            span("shiny.fluent::TextField.shinyInput(ns(\"text_input_%widget_id%\")),", style = "padding-left:40px;"), br(),
            span("shiny.fluent::PrimaryButton.shinyInput(ns(\"submit_%widget_id%\"), i18np$t(\"show\"))", style = "padding-left:40px;"), br(),
          span("),", style = "padding-left:20px;"), br(),
          span("div(verbatimTextOutput(ns(\"text_output_%widget_id%\")), style = \"border:dashed 1px; margin-top:10px;\")", style = "padding-left:20px;"), br(),
        span(")"),
        style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
      ),
      p("Le code doit être intégré au sein d'une fonction ", strong("tagList"), " comme c'est le cas ici."),
      p("N'oubliez pas de déclarer les ID des éléments avec le ", strong("namespace"), ", via la fonction ", strong("ns()"), "."),
      p("Remarquez l'utilisation de la balise ", strong("%widget_id%"), ", qui sera remplacée par l'ID du widget une fois le plugin lancé."),
      p("Voici les autres balises que vous pouvez utiliser :"),
      tags$ul(
        tags$li(strong("%module_id%"), " : sera remplacé par l'ID du module dans lequel sont contenus le widget et le plugin."),
        tags$li(strong("%study_id%"), " : sera remplacé par l'ID de l'étude en cours.")
      ),
      p(strong("2) Server code")),
      p("Vous codez ici le partie ", strong("serveur"), " de votre plugin, avec les ", strong("observers"), "."),
      p("Cela fonctionne exactement comme une application Shiny."),
      p("Voici un exemple de code :"),
      div(
        span(), br(),
        style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
      ),
      p(strong("3) Traductions")),
      p(),
      br()
    )
  })
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("local_and_remote_plugins")
    r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
      p(), br()
    )
  })
}