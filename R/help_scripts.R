help_scripts <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_scripts_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("choose_datamart_scripts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("scripts_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_script_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("script_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_scripts_open_panel_light_dismiss,
      isBlocking = r$help_scripts_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_scripts_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_scripts_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_scripts_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_scripts_open_modal <- TRUE
    r$help_scripts_open_panel_light_dismiss <- FALSE
  }
  
  # Choose datamart scripts
  
  observeEvent(r$help_scripts_page_1, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("choose_datamart_scripts")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p(strong("1) Configurer le set de données")),
        p("La colonne de droite référencie les scripts ", strong("disponibles"), " pour cet set de données, non utilisés."),
        p("La colonne de gauche référencie les scripts ", strong("utilisés"), " pour ce set de données."),
        p("Lorsque vous chargez un set de données, tous les scripts de la colonne ", tags$em("Scripts choisis"), " seront ", strong("éxécutés au lancement du set de données"), "."),
        p("Cliquez sur un script et glissez-le dans la colonne correspondante."),
        p(strong("2) Mémoire cache")),
        p("L'éxécution de certains scripts peut prendre du temps au chargement d'un set de données."),
        p("Activer la mémoire cache permet de ", strong("sauvegarder"), " au format CSV les ", strong("sets de données après éxécution des scripts"), 
          ", permettant un gain de temps au chargement du set de données.")
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p(strong("1) Configure the datamart")),
        p("The right column lists the ", strong("available"), " scripts for this datamart that are not used."),
        p("The left column lists the ", strong("used"), " scripts for this datamart."),
        p("When you load a datamart, all the scripts in the ", tags$em("Selected scripts"), " column will be ", strong("executed at the launch of the datamart"), "."),
        p("Click on a script and drag it to the corresponding column."),
        p(strong("2) Cache memory")),
        p("The execution of some scripts can take time when loading a datamart."),
        p("Enabling cache memory allows you to ", strong("save"), " the datamarts in CSV format after the ", strong("scripts have been executed"),
          ", which saves time when loading the datamart.")
      )
    }
  })
  
  # Scripts management
  
  observeEvent(r$help_scripts_page_2, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("scripts_management")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p(strong("1) Créer un script")),
        p("Pour créer un script, allez dans l'onglet ", tags$em("Gestion des scripts"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        p(strong("2) Changer le nom d'un script")),
        p("Pour changer le nom d'une étude, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        p(strong("3) Supprimer un ou des scripts")),
        p("Pour supprimer une ou plusieurs scripts, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un script en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        p(strong("4) Editer le code ou les options d'un script")),
        p("Cliquez sur"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), " pour ", strong("éditer le code"), " du script,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), " pour ", strong("éditer les options"), " du script"),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p(strong("1) Creating a script")),
        p("To create a script, go to the ", tags$em("Scripts Management"), " tab."),
        p("Choose a name, make sure it is not already used, then click on ", tags$em("Add"), "."),
        p(strong("2) Changing the name of a script")),
        p("To change the name of a script, double-click on the name, change it, make sure it is not already used, then click on ", tags$em("Save"), "."),
        p(strong("3) Deleting one or more scripts")),
        p("To delete one or more scripts, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a script by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        p(strong("4) Editing the code or options of a script")),
        p("Click on"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), " to ", strong("edit the code"), " of the script,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), " to ", strong("edit the options"), " of the script"),
        br()
      )
    }
  })
  
  # Edit script code
  
  observeEvent(r$help_scripts_page_3, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("edit_script_code")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p("Dans cette rubrique, vous pouvez ", strong("écrire le code"), " d'un script et le ", strong("tester"), "."),
        p("L'intérêt d'un script est de faire du ", strong("data cleaning"), " sur un set, ou de ", strong("créer de nouveaux concepts"), " de thésaurus."),
        p("Par exemple :"),
        tags$ul(
          tags$li("Script permettant ", strong("d'exclure les données aberrantes de poids et de taille"), ", par exemple lorsque les deux valeurs sont inversées."),
          tags$li("Script permettant de ", strong("créer un concept \"Diurèse\""), " faisant la somme des diurèses de sonde urinaire, de néphrostomie, de miction etc")
        ),
        p("Voici un exemple de code pour ", strong("l'exclusion de valeurs aberrantes"), " de taille, en cm :"),
        div(
          span("d$measurement <-"), br(),
          span("d$measurement %>%", style = "margin-left:20px;"), br(),
          span("dplyr::mutate(value_as_number = dplyr::case_when(", style = "margin-left:20px;"), br(),
          span("measurement_concept_id == 3453 & value_as_number > 280 ~ NA_real_,", style = "margin-left:40px;"), br(),
          span("TRUE ~ value_as_number", style = "margin-left:40px;"), br(),
          span(")", style = "margin-left:20px;"), br(),
          span(")"),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ), br(),
        p("Exemple de code pour la ", strong("création d'un concept"), " de diurèse :"),
        div(
          span("# Création d'un item \"Diurèse\" dans le thésaurus MIMIC-IV"), br(),
          span("add_thesaurus_item(output = output, r = r, thesaurus_name = \"MIMIC-IV\", item_name = \"Diurèse\", item_unit = \"mL\")"), br(), br(),
          span("# Récupération du concept_id du nouveau concept ajouté."), br(),
          span("new_thesaurus_item_id <- get_thesaurus_item(output = output, r = r, thesaurus_name = \"MIMIC-IV\", item_name = \"Diurèse\", method = \"item_name\", i18n = i18n, ns = ns)"), br(), br(),
          span("# Fusion des différents items concernant la diurèse au sein de notre nouvel item \"Diurèse\""), br(),
          span("new_item_values <-"), br(),
          span("d$measurement %>%", style = "margin-left:20px;"), br(),
          span("dplyr::filter(measurement_concept_id %in% c(25230, 9093, 190290)) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::select(measurement_concept_id, value_as_number) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::mutate(measurement_concept_id = new_thesaurus_item_id) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::group_by(measurement_concept_id) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::summarize(value_as_number = sum(value_as_number)) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::ungroup()", style = "margin-left:20px;"), br(), br(),
          span("# Merge old and new data"), br(),
          span("d$measurement <-"), br(),
          span("d$measuremenent %>%", style = "margin-left:20px;"), br(),
          span("dplyr::bind_rows(", style = "margin-left:20px;"), br(),
          span("__", style = "margin-left:40px;"), br(),
          span(")", style = "margin-left:20px;"), br(),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p("In this section, you can ", strong("write code"), " for a script and ", strong("test it"), "."),
        p("The purpose of a script is to perform ", strong("data cleaning"), " on a datamart or to ", strong("create new concepts"), " in a thesaurus."),
        p("For example:"),
        tags$ul(
          tags$li("Script allowing to ", strong("exclude aberrant weight and height data"), ", for example when the two values are reversed."),
          tags$li("Script allowing to ", strong("create a \"Diuresis\" concept"), " by adding up the diuresis from urinary catheter, nephrostomy, micturition, etc.")
        ),
        p("Here is an example of code for ", strong("excluding aberrant height values"), " in cm:"),
        div(
          span("d$measurement <-"), br(),
          span("d$measurement %>%", style = "margin-left:20px;"), br(),
          span("dplyr::mutate(value_as_number = dplyr::case_when(", style = "margin-left:20px;"), br(),
          span("measurement_concept_id == 3453 & value_as_number > 280 ~ NA_real_,", style = "margin-left:40px;"), br(),
          span("TRUE ~ value_as_number", style = "margin-left:40px;"), br(),
          span(")", style = "margin-left:20px;"), br(),
          span(")"),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ), br(),
        p("Example of code for ", strong("creating a new concept"), " of diuresis:"),
        div(
          span("# Creation of a \"Diuresis\" item in the MIMIC-IV thesaurus"), br(),
          span("add_thesaurus_item(output = output, r = r, thesaurus_name = \"MIMIC-IV\", item_name = \"Diuresis\", item_unit = \"mL\")"), br(), br(),
          span("# Retrieval of the concept_id of the new added concept."), br(),
          span("new_thesaurus_item_id <- get_thesaurus_item(output = output, r = r, thesaurus_name = \"MIMIC-IV\", item_name = \"Diuresis\", method = \"item_name\", i18n = i18n, ns = ns)"), br(), br(),
          span("# Merge different diuresis items into our new \"Diuresis\" item"), br(),
          span("new_item_values <-"), br(),
          span("d$measurement %>%", style = "margin-left:20px;"), br(),
          span("dplyr::filter(measurement_concept_id %in% c(25230, 9093, 190290)) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::select(measurement_concept_id, value_as_number) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::mutate(measurement_concept_id = new_thesaurus_item_id) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::group_by(measurement_concept_id) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::summarize(value_as_number = sum(value_as_number)) %>%", style = "margin-left:20px;"), br(),
          span("dplyr::ungroup()", style = "margin-left:20px;"), br(), br(),
          span("# Merge old and new data"), br(),
          span("d$measurement <-"), br(),
          span("d$measuremenent %>%", style = "margin-left:20px;"), br(),
          span("dplyr::bind_rows(", style = "margin-left:20px;"), br(),
          span("__", style = "margin-left:40px;"), br(),
          span(")", style = "margin-left:20px;"), br(),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        br()
      )
    }
  })
  
  # Script options
  
  observeEvent(r$help_scripts_page_4, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("script_options")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p("Vous pouvez ici éditer la ", strong("description du script"), ", en Markdown, qui sera visible depuis l'onglet ", tags$em("Description des scripts"), "."),
        p("Voici un exemple de code :"),
        div(
          span("## Script de data cleaning pour le poids et la taille"), br(), br(),
          span("Ce script permet d'exclure les données aberrantes pour le poids et la taille, avec les règles suivantes."), br(), br(),
          span("**Pour le poids** :"), br(),
          span("- Exclusion des valeurs inférieures à __ kg"), br(),
          span("- Exclusion des valeurs supérieures à __ kg"), br(), br(),
          span("**Pour la taille** :"), br(),
          span("- Exclusion des valeurs inférieures à __ cm"), br(),
          span("- Exclusion des valeurs supérieures à __ cm"), br(), br(),
          span("Les poids sont transformés en taille si leur valeur est inférieure à 2.5 et l'âge du patient supérieur à 1 an (inversion des champs poids et taille)."),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p("Here you can edit the ", strong("description of the script"), ", in Markdown, which will be visible in the ", tags$em("Scripts description"), " tab."),
        p("Here is an example of code:"),
        div(
          span("## Data cleaning script for weight and height"), br(), br(),
          span("This script allows excluding outliers for weight and height, using the following rules."), br(), br(),
          span("For weight: "), br(),
          span("- Exclude values below __ kg"), br(),
          span("- Exclude values above __ kg"), br(), br(),
          span("For height: "), br(),
          span("- Exclude values below __ cm"), br(),
          span("- Exclude values above __ cm"), br(), br(),
          span("Weights are transformed into height if their value is below 2.5 and the patient's age is over 1 year (swapping weight and height fields)."),
          style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
        ),
        br()
      )
    }
  })
}