help_my_studies <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_my_studies_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("messages"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("studies_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("study_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_my_studies_open_panel_light_dismiss,
      isBlocking = r$help_my_studies_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_my_studies_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_my_studies_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_my_studies_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_my_studies_open_modal <- TRUE
    r$help_my_studies_open_panel_light_dismiss <- FALSE
  }
  
  # Study messages
  
  observeEvent(r$help_my_studies_page_1, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("messages")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        p(strong("1) Nouvelle conversation")),
        p("Créez une nouvelle conversation en renseignant ", strong("l'objet"), " et le ", strong("premier message"), " de la conversation."),
        p("Tous les utilisateurs ayant accès à l'étude verront cette conversation."),
        p("Vous pouvez écrire en RMarkdown en cochant la case ", tags$em("RMarkdown"), "."),
        p("Le ", strong("RMarkdown"), " est du Markdown où l'on peut intégrer du code R."),
        p("Vous pouvez ", strong("utiliser les données de l'étude actuellement chargée"), "."),
        p("Voici un exemple de code en RMarkdown."),
        div(
          span("Bonjour à tous."), br(), br(),
          span("Voici les données que j'obtiens lorsque je charge la variable *d$person*."), br(), br(),
          span("```{r}"), br(),
          span("d$person"), br(),
          span("```"), br(),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        p("Cliquez sur ", tags$em("Aperçu"), " pour afficher un aperçu du message, avec le code exécuté."),
        p(strong("2) Tous les messages")),
        p("Toutes les conversations s'affichent ici."),
        p("Une conversation non lue s'affichera en gras."),
        p("Répondez à une conversation en cliquant sur ", tags$em("Nouveau message"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        p(strong("1) New conversation")),
        p("Create a new conversation by filling in ", strong("the subject"), " and the ", strong("first message"), " of the conversation."),
        p("All users with access to the study will see this conversation."),
        p("You can write in RMarkdown by checking the ", tags$em("RMarkdown"), " box."),
        p("RMarkdown is Markdown where you can embed R code."),
        p("You can ", strong("use the data from the currently loaded study"), "."),
        p("Here is an example of code in RMarkdown."),
        div(
          span("Hello everyone."), br(), br(),
          span("Here is the data I get when I load the variable *d$person*."), br(), br(),
          span("```{r}"), br(),
          span("d$person"), br(),
          span("```"), br(),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        p("Click on ", tags$em("Preview"), " to display a preview of the message, with the code executed."),
        p(strong("2) All messages")),
        p("All conversations are displayed here."),
        p("An unread conversation will be displayed in bold."),
        p("Reply to a conversation by clicking on ", tags$em("New message"), "."),
        br()
      )
    }
  })
  
  # Studies management
  
  observeEvent(r$help_my_studies_page_2, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("studies_management")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        p(strong("1) Créer une étude")),
        p("Pour créer une étude, allez dans l'onglet ", tags$em("Gestion des études"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        p(strong("2) Changer le nom d'une étude")),
        p("Pour changer le nom d'une étude, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        p(strong("3) Supprimer une ou des études")),
        p("Pour supprimer une ou plusieurs études, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une étude en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        p(strong("4) Editer les options d'une étude")),
        p("Cliquez sur ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), " pour ", strong("éditer les options"), " de l'étude."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        p(strong("1) Create a study")),
        p("To create a study, go to the ", tags$em("Study Management"), " tab."),
        p("Choose a name, make sure it's not already in use, then click on ", tags$em("Add"), "."),
        p(strong("2) Rename a study")),
        p("To rename a study, double-click on the name, change it, make sure it's not already in use, then click on ", tags$em("Save"), "."),
        p(strong("3) Delete one or more studies")),
        p("To delete one or more studies, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a study by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        p(strong("4) Edit study options")),
        p("Click on ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), " to ", strong("edit the options"), " of the study."),
        br()
      )
    }
  })
  
  # Study options
  
  observeEvent(r$help_my_studies_page_3, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("study_options")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        p(strong("Accès")),
        p("Choisissez ici qui peut avoir accès à cette étude."),
        br()
      )
    }
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        p(strong("Access")),
        p("Choose here who can access this study."),
        br()
      )
    }
  })
}