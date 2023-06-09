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
  
  # Code divs
  
  code_1 <- list()
  div_code_1 <- list()
  
  code_1$fr <- paste0("Bonjour à tous.\n\n",
    "Voici les données que j'obtiens lorsque je charge la variable *d$person*.\n\n",
    "```{r}\n",
    "d$person\n",
    "```")
  div_code_1$fr <- div(
    span("Bonjour à tous."), br(), br(),
    span("Voici les données que j'obtiens lorsque je charge la variable *d$person*."), br(), br(),
    span("```{r}"), br(),
    span("d$person"), br(),
    span("```"), br(),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_1$en <- paste0("Hello everyonde.\n\n",
    "Here are the data I get when I load the *d$person* variable *d$person*.\n\n",
    "```{r}\n",
    "d$person\n",
    "```")
  div_code_1$en <- div(
    span("Hello everyone."), br(), br(),
    span("Here are the data I get when I load the *d$person* variable."), br(), br(),
    span("```{r}"), br(),
    span("d$person"), br(),
    span("```"), br(),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Study messages
  
  observeEvent(r$help_my_studies_page_1, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("messages")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ",
          strong("Nouvelle conversation")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Créez une nouvelle conversation en renseignant ", strong("l'objet"), " et le ", strong("premier message"), " de la conversation."),
        p("Tous les utilisateurs ayant accès à l'étude verront cette conversation."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez écrire en RMarkdown en cochant la case ", tags$em("RMarkdown"), "."),
        p("Le ", strong("RMarkdown"), " est du Markdown où l'on peut intégrer du code R."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez ", strong("utiliser les données de l'étude actuellement chargée"), "."),
        p("Voici un exemple de code en RMarkdown."),
        div_code_1$fr,
        p("Cliquez sur ", tags$em("Aperçu"), " pour afficher un aperçu du message, avec le code exécuté."),
        tags$h3(tags$i(class = "fa fa-message", style = "color: steelblue;"), " ",
          strong("Tous les messages")),
        p("Toutes les conversations s'affichent ici."),
        p("Une conversation non lue s'affichera en gras."),
        p("Répondez à une conversation en cliquant sur ", tags$em("Nouveau message"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ",
          strong("New conversation")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Create a new conversation by filling in ", strong("the subject"), " and the ", strong("first message"), " of the conversation."),
        p("All users with access to the study will see this conversation."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can write in RMarkdown by checking the ", tags$em("RMarkdown"), " box."),
        p("RMarkdown is Markdown where you can embed R code."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can ", strong("use the data from the currently loaded study"), "."),
        p("Here is an example of code in RMarkdown."),
        div_code_1$en,
        p("Click on ", tags$em("Preview"), " to display a preview of the message, with the code executed."),
        tags$h3(tags$i(class = "fa fa-message", style = "color: steelblue;"), " ",
          strong("All messages")),
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
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ",
          strong("Créer une étude")),
        p("Pour créer une étude, allez dans l'onglet ", tags$em("Gestion des études"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ",
          strong("Changer le nom d'une étude")),
        p("Pour changer le nom d'une étude, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ",
          strong("Supprimer une ou des études")),
        p("Pour supprimer une ou plusieurs études, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une étude en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ",
          strong("4) Editer les options d'une étude")),
        p("Cliquez sur  ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), "  pour ", strong("éditer les options"), " de l'étude."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ",
          strong("Create a study")),
        p("To create a study, go to the ", tags$em("Study Management"), " tab."),
        p("Choose a name, make sure it's not already in use, then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ",
          strong("Rename a study")),
        p("To rename a study, double-click on the name, change it, make sure it's not already in use, then click on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ",
          strong("Delete one or more studies")),
        p("To delete one or more studies, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a study by clicking on the  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "  icon."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ",
          strong("Edit study options")),
        p("Click on  ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), "  to ", strong("edit the options"), " of the study."),
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
        
      )
    }
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_my_studies_copy_code_1, clipr::write_clip(code_1[[language]]))
}