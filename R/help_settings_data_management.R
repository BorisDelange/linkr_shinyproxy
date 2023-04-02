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
      strong(i18n$t("data_model")), br(), br(),
      shiny.fluent::Link(i18n$t("data_model"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_8', Math.random()); }"))), br(), br(),
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
        p(strong("1) Add a data source")),
        p("To create a data source, enter a name and click on ", tags$em("Add"), "."),
        p("A data source includes ", strong("several datamarts"), "."),
        p("For example, you create a data source named ", tags$em("MIMIC-IV"), ", which will include datamarts created from the MIMIC-IV database."),
        p("Different datamarts will be created from this source, for example "),
        tags$ul(
          tags$li("a datamart called ", tags$em("Nosocomial Infections"), " which will include patients who had a nosocomial infection during their hospital stay"),
          tags$li("a datamart called ", tags$em("Hepatitis"), " which will include patients diagnosed with hepatitis")
        ),
        p(strong("2) Manage data sources")),
        p("You can modify the names of data sources by double-clicking on the corresponding row and column in the table."),
        p("Once the information is modified, click on ", tags$em("Save"), "."),
        p("To delete one or more data sources, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a data source by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        br()
      )
    }
  })
  
  # Datamarts management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datamarts_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Ajouter un set de données")),
        p("Pour créer un set de données, entrez un nom, choisissez de ", strong("quelle source de données"), " va dépendre le set puis cliquez sur ", tags$em("Ajouter"), "."),
        p("Un set de données comprend ", strong("plusieurs études"), "."),
        p("Par exemple, vous créez un set de données nommé ", tags$em("Infections nosocomiales"), " à partir de la source de données ", tags$em("MIMIC-IV"), 
          ", qui comprendra tous les patients ayant développé une infection nosocomiale au cours d'une période définie."),
        p("Plusieurs études seront créées à partir de ce set, par exemple "),
        tags$ul(
          tags$li("une étude ", tags$em("Mortalité et infections nosocomiales")),
          tags$li("une étude ", tags$em("Epidémiologie des infections nosocomiales"))
        ),
        p(strong("2) Gérer les sets de données")),
        p("Vous pouvez modifier le nom des sets de données en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer un ou plusieurs sets de données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un set de données en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Add a datamart")),
        p("To create a datamart, enter a name, choose ", strong("which data source"), " the dataset will depend on, and click ", tags$em("Add"), "."),
        p("A datamart comprises ", strong("multiple studies"), "."),
        p("For example, you create a datamart named ", tags$em("Nosocomial infections"), " from the data source ", tags$em("MIMIC-IV"),
          ", which will include all patients who developed a nosocomial infection during a defined period."),
        p("Multiple studies will be created from this dataset, such as "),
        tags$ul(
          tags$li("a study on ", tags$em("Mortality and nosocomial infections")),
          tags$li("a study on ", tags$em("Epidemiology of nosocomial infections"))
        ),
        p(strong("2) Manage datamarts")),
        p("You can modify the name of the datamarts by double-clicking on the corresponding row and column in the table."),
        p("Once the information is modified, click ", tags$em("Save"), "."),
        p("To delete one or more datamarts, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a datamart by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        br()
      )
    }
  })
  
  # Edit datamart code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_datamart_code")
    
    div_code_1 <- div(
      "## Code ##",
      style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
    )
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p("Dans cette rubrique, vous pouvez ", strong("écrire le code"), " qui permettra d'obtenir les données du set de données."),
        p("Pour vous aider, référez-vous à la section ", tags$em("Modèle de données"), ", qui détaille le ", strong("modèle de données"), " utilisé par l'application."),
        p("Pour charger un set de données, deux étapes sont nécessaires :"),
        tags$ul(
          tags$li(strong("Créez une fonction"), " qui chargera les données une fois éxécutée, variable par variable (", tags$em("person, measurement"), "...)"),
          tags$li(strong("Importez les données"), " avec la fonction ", tags$em("import_datamart"))
        ),
        p("La fonction ", tags$em("import_datamart"), " comprend les arguments suivants :"),
        tags$ul(
          tags$li(tags$em("output, ns, r, d, i18n"), " : qui sont les variables permettant le fonctionnement de l'application"),
          tags$li(tags$em("datamart_id"), " : où vous indiquez ", strong("l'ID du datamart"), " actuel, via la balise ", tags$em("%datamart_id%")),
          tags$li(tags$em("data"), " : où vous indiquez la ", strong("fonction qui chargera les données"), " pour une variable (exemple : ", tags$em("person"), ")"),
          tags$li(tags$em("type"), " : où vous indiquez la ", strong("variable que vous souhaitez importer"), " (", tags$em("person, measurement"), "...)"),
          tags$li(tags$em("save_as_csv"), " : indiquant si vous souhaitez ", strong("sauvegarder l'import"), " dans un fichier CSV (logical)"),
          tags$li(tags$em("rewrite"), " : indiquant si vous souhaitez écraser l'ancien fichier CSV pour le remplacer par le nouveau (logical)")
        ),
        p("L'argument ", tags$em("rewrite"), " n'est utile que lorsque vous travaillez sur le code permettant d'importer le datamart. ",
          "En effet, inutile d'enregistrer une copie au format CSV de votre import s'il est écrasé et recréé à chaque fois que vous chargez le set de données."),
        p("Le code que vous créez ici ", strong("s'éxécutera à chaque fois"), " que quelqu'un chargera le set de données,",
          " d'où le fait d'utiliser l'argument ", tags$em("save_as_csv"), " qui permettra d'économiser des ressources."),
        p("Voici un exemple de code :"),
        div_code_1,
        p("Cliquez sur ", tags$em("Sauvegarder"), " pour sauvegarder le code, sur ", tags$em("Exécuter"), " pour tester le code."),
        p("Un ", strong("tableau"), " en bas de la page vous indiquera le nombre de lignes chargées par variable."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p("In this section, you can ", strong("write the code"), " that will retrieve the data from the datamart."),
        p("To help you, refer to the section ", tags$em("Data Model"), ", which details the ", strong("data model"), " used by the application."),
        p("To load a datamart, two steps are required:"),
        tags$ul(
          tags$li(strong("Create a function"), " that will load the data, variable by variable (", tags$em("person, measurement"), "...), when executed."),
          tags$li(strong("Import the data"), " with the function ", tags$em("import_datamart"))
        ),
        p("The function ", tags$em("import_datamart"), " includes the following arguments:"),
        tags$ul(
          tags$li(tags$em("output, ns, r, d, i18n"), " : which are the variables allowing the application to function"),
          tags$li(tags$em("datamart_id"), " : where you indicate the ", strong("current datamart ID"), ", using the tag ", tags$em("%datamart_id%")),
          tags$li(tags$em("data"), " : where you specify the ", strong("function that will load the data"), " for a variable (e.g. ", tags$em("person"), ")"),
          tags$li(tags$em("type"), " : where you indicate the ", strong("variable you wish to import"), " (", tags$em("person, measurement"), "...)"),
          tags$li(tags$em("save_as_csv"), " : indicating whether you want to ", strong("save the import"), " to a CSV file (logical)"),
          tags$li(tags$em("rewrite"), " : indicating whether you want to overwrite the old CSV file to replace it with the new one (logical)")
        ),
        p("The ", tags$em("rewrite"), " argument is only useful when working on the code that imports the datamart. ",
          "Indeed, there's no need to save a copy in CSV format of your import if it's overwritten and recreated every time you load the datamart."),
        p("The code you create here will be ", strong("executed every time"), " someone loads the datamart,",
          " which is why you should use the ", tags$em("save_as_csv"), " argument to save resources."),
        p("Here's an example of code:"),
        div_code_1,
        p("Click on ", tags$em("Save"), " to save the code, and on ", tags$em("Run"), " to test the code."),
        p("A ", strong("table"), " at the bottom of the page will indicate the number of rows loaded per variable."),
        br()
      )
    }
  })
  
  # Datamart options
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datamart_options")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Accès aux données agrégées uniquement")),
        p("Vous pouvez choisir de ne donner accès qu'aux ", strong("Données agrégées"), " d'un set de données."),
        p("Ainsi, les utilisateurs n'auront ", strong("pas accès aux données individuelles"), " de cet set, depuis la page ", tags$em("Données"), "."),
        p(strong("2) Accès aux données")),
        p("Choisissez qui a accès à ce set de données."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Access to aggregated data only")),
        p("You can choose to give access only to the ", strong("Aggregated data"), " of a dataset."),
        p("This way, users will ", strong("not have access to individual data"), " of this datamart, from the ", tags$em("Data"), " page."),
        p(strong("2) Access to data")),
        p("Choose who has access to this datamart."),
        br()
      )
    }
  })
  
  # Thesaurus management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("thesaurus_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Ajouter un thésaurus")),
        p("Pour créer un thésaurus, entrez un nom, choisissez de ", strong("quelle source de données"), " va dépendre le thésaurus puis cliquez sur ", tags$em("Ajouter"), "."),
        p("Lors que vous accéderez aux sets de données, vous n'aurez accès qu'aux ", strong("thésaurus appartenant à la même source de données"), "que le set de données."),
        p(strong("2) Gérer les thésaurus")),
        p("Vous pouvez modifier le nom des thésaurus en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Vous pouvez changer les ", strong("sources de données associées au thésaurus"), " en cliquant sur les menus déroulants dans la colonne ", tags$em("Sources de données"), "."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer un ou plusieurs thésaurus, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un thésaurus en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(strong("1) Add a thesaurus")),
        p("To create a thesaurus, enter a name, choose which ", strong("data source"), " the thesaurus will depend on, then click ", tags$em("Add"), "."),
        p("When you access the datamarts, you will only have access to the ", strong("thesauri belonging to the same data source"), " as the datamart."),
        p(strong("2) Manage thesauri")),
        p("You can edit the name of thesauri by double-clicking on the corresponding row and column in the table."),
        p("You can change the ", strong("data sources associated with thesauri"), " by clicking on the dropdown menus in the ", tags$em("Data sources"), " column."),
        p("Once you have made changes, click ", tags$em("Save"), "."),
        p("To delete one or more thesauri, select them by clicking on them in the table, then click ", tags$em("Delete selected"), "."),
        p("You can also delete a thesaurus by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        br()
      )
    }
  })
  
  # Edit thesaurus code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_thesaurus_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p("Dans cette rubrique, vous pouvez ", strong("écrire le code"), " qui permettra de charger le thésaurus."),
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
  
  # Data model
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_8")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("data_model")
    
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