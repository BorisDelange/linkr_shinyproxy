help_settings_app_database <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_app_database_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("database_structure"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_connection_infos_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_request_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_export_and_restore"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_app_database_open_panel_light_dismiss,
      isBlocking = r$help_settings_app_database_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_app_database_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_app_database_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_app_database_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_app_database_open_modal <- TRUE
    r$help_settings_app_database_open_panel_light_dismiss <- FALSE
  }
  
  # Database structure
  
  observeEvent(r$help_settings_app_database_page_1, {
    
    load_help_page(r)
    
    div_code_1 <- div(
      "r$datasets",
      "DBI::dbGetQuery(r$db, \"SELECT * FROM datasets\")",
      style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
    )
    
    div_code_2 <- div(
      "m$subsets", br(),
      "DBI::dbGetQuery(m$db, \"SELECT * FROM modules_elements_options\")",
      style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
    )
    
    r$help_settings_app_database_modal_title <- i18n$t("database_structure")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p("La base de données, qu'elle soit locale ou distante, est composée de ", strong("deux bases de données"), 
          " : la base de données principale et la base de données publique."),
        p(strong("1) Base de données principale")),
        p("La base de données principale comprend la ", strong("majorité des tables servant au fonctionnement de l'application"), 
          ", y compris les tables concernant les utilisateurs, les mots de passe cryptés etc."),
        p("Cette BDD a été séparée afin d'y ", strong("restreindre l'accès"), "."),
        p("On accède à ces tables via la variable ", tags$em("r"), ", ou par requête avec la librairie ", tags$em("DBI"), ". Par exemple :"),
        div_code_1,
        p(strong("2) Base de données publiques")),
        p("La base de données publique comprend les tables qui concernent les ", strong("études et les données"),
          ". Ces données seront accessibles par l'ensemble des utilisateurs."),
        p("On accède à ces tables via la variable ", tags$em("m"), ", ou par requête avec la librairie ", tags$em("DBI"), ". Par exemple :"),
        div_code_2,
        p("Les tables sont détaillées dans la rubrique ", tags$em("Tables de la BDD"), "."), 
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p("The database, whether local or remote, is composed of ", strong("two databases"),
          " : the main database and the public database."),
        p(strong("1) Main database")),
        p("The main database includes the ", strong("majority of tables used to make the application work"),
          ", including tables related to users, encrypted passwords, etc."),
        p("This database has been separated in order to ", strong("restrict access"), "."),
        p("These tables can be accessed via the ", tags$em("r"), " variable, or by query using the ", tags$em("DBI"), " library. For example:"),
        div_code_1,
        p(strong("2) Public database")),
        p("The public database includes tables related to ", strong("studies and data"),
          ". This data will be accessible to all users."),
        p("These tables can be accessed via the ", tags$em("m"), " variable, or by query using the ", tags$em("DBI"), " library. For example:"),
        div_code_2,
        p("The tables are detailed in the section ", tags$em("Database Tables"), "."),
        br()
      )
    }
  })
  
  # Database connection informations
  
  observeEvent(r$help_settings_app_database_page_2, {
    
    load_help_page(r)
    
    div_code_3 <- div(
      "linkr(language = \"fr\", app_folder = \"/Users/borisdelange\")",
      style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
    )
    
    r$help_settings_app_database_modal_title <- i18n$t("db_connection_infos_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("1) Type de connexion")),
        p("Au lancement de l'application, une ", strong("connexion"), " est établie vers la ", strong("base de données de l'application"), "."),
        p("Par défaut, la connexion est établie avec une ", strong("base de données locale"), "."),
        p("Il est également possible de se connecter à une ", strong("base de données distante"), "."),
        p(strong("2) Connexion locale")),
        p("Lorsque vous choisissez la connexion locale, des ", strong("fichiers SQLite"), " sont créés dans le ", strong("dossier de l'application"),
          ", qui est ", tags$em("app_folder/app_database"), ", où app_folder est le dossier renseigné par l'argument du même nom",
          " lors du lancement de l'application."),
        p("Par exemple, si vous lancez l'application avec :"),
        div_code_3,
        p("Les fichiers de la base de données seront enregistrés dans ", tags$em("/Users/borisdelange/linkr/app_database"), "."),
        p(strong("3) Connexion distante")),
        p("Pour ", strong("configurer une connexion distante"), ", vous devez remplir les champs suivants :"),
        tags$ul(
          tags$li(strong("Librairie SQL"), " : la librairie SQL utilisée par la BDD distante. ",
          " Pour l'instant, uniquement ", tags$em("PostgreSQL"), " et ", tags$em("SQLite"), " sont disponibles."),
          tags$li(strong("Hote"), " : l'adresse de la base de données distante, par exemple ", tags$em("localhost"), ", ou une adresse URL"),
          tags$li(strong("Nom de la BDD principale"), " : le nom de la BDD principale, qui ", strong("doit être créée au prélable")),
          tags$li(strong("Nom de la BDD publique"), " : le nom de la BDD publique, qui ", strong("doit être créée au prélable")),
          tags$li(strong("Port"), " : le numéro du port, par exemple 5432"),
          tags$li(strong("Utilisateur"), " : le login pour se connecter à la BDD distante"),
          tags$li(strong("Mot de passe"), " : le mot de passe qui est associé au compte")
        ),
        p("Penser à sauvegarder ces paramètres en cliquant sur ", tags$em("Sauvegader"), "."),
        p("Vous pouvez tester la connexion avec les paramètres rentrés en cliquant sur ", tags$em("Tester la connexion"), "."),
        p("Deux cas ce figure :"),
        tags$ul(
          tags$li("Soit la connexion fonctionne, vous avez sauvegardé ces paramètres, et alors au prochain chargement de l'application,",
          " la ", strong("connexion distante sera utilisée par défaut"), "."),
          tags$li("Soit la connexion ne fonctionne pas (ou ne fonctionne plus à un moment donné), et alors on retourne sur la",
            " connexion locale.")
        ), 
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("1) Connection type")),
        p("At the launch of the application, a ", strong("connection"), " is established to the ", strong("application database"), "."),
        p("By default, the connection is established with a ", strong("local database"), "."),
        p("It is also possible to connect to a ", strong("remote database"), "."),
        p(strong("2) Local connection")),
        p("When you choose the local connection, ", strong("SQLite files"), " are created in the ", strong("application folder"),
          ", which is ", tags$em("app_folder/app_database"), ", where app_folder is the folder specified by the argument of the same name",
          " at the launch of the application."),
        p("For example, if you launch the application with:"),
        div_code_3,
        p("The database files will be saved in ", tags$em("/Users/borisdelange/linkr/app_database"), "."),
        p(strong("3) Remote connection")),
        p("To ", strong("configure a remote connection"), ", you have to fill in the following fields:"),
        tags$ul(
          tags$li(strong("SQL library"), " : the SQL library used by the remote database. ",
            " For now, only ", tags$em("PostgreSQL"), " and ", tags$em("SQLite"), " are available."),
          tags$li(strong("Host"), " : the address of the remote database, for example ", tags$em("localhost"), ", or a URL"),
          tags$li(strong("Name of the main database"), " : the name of the main database, which ", strong("must be created beforehand")),
          tags$li(strong("Name of the public database"), " : the name of the public database, which ", strong("must be created beforehand")),
          tags$li(strong("Port"), " : the port number, for example 5432"),
          tags$li(strong("User"), " : the login to connect to the remote database"),
          tags$li(strong("Password"), " : the password associated with the account")
        ),
        p("Remember to save these settings by clicking on ", tags$em("Save"), "."),
        p("You can test the connection with the entered parameters by clicking on ", tags$em("Test connection"), "."),
        p("Two cases are possible:"),
        tags$ul(
          tags$li("Either the connection works, you have saved these settings, and then the next time you load the application,",
            " the ", strong("remote connection will be used by default"), "."),
          tags$li("Or the connection does not work (or stops working at some point), and then we fall back on the",
            " local connection.")
        ),
        br()
      )
    }
  })
  
  # Database tables
  
  observeEvent(r$help_settings_app_database_page_3, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_datatable_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p("Voici les différentes tables des deux bases de données de l'application."),
        p("Choisissez le ", tags$em("Type de connexion"), " (locale ou distante), et la ", tags$em("Base de données"), " (principale ou publique)."),
        p(strong("1) BDD principale")),
        p("Voici les tables composant la BDD principale, classées par groupes."),
        p(strong("Utilisateurs et droits d'accès :")),
        tags$ul(
          tags$li(tags$em("users"), " : liste les utilisateurs"),
          tags$li(tags$em("users_accesses"), " : liste les différents accès utilisateurs"),
          tags$li(tags$em("users_statuses"), " : liste les différents statuts des utilisateurs (data scientist, clinicien etc)")
        ),
        p(strong("Gestion des données")),
        tags$ul(
          tags$li(tags$em("data_sources"), " : comprend les différentes sources de données"),
          tags$li(tags$em("datasets"), " : les set de données associés aux sources de données"),
          tags$li(tags$em("studies"), " : les différentes études associées aux sets de données"),
          tags$li(tags$em("thesaurus"), " : comprend les différents thésaurus, associés aux sources de données"),
          tags$li(tags$em("thesaurus_items"), " : les concepts / items des thésaurus sont stockés ici"),
          tags$li(tags$em("thesaurus_items_mapping"), " : les alignements de concepts sont ici"),
          tags$li(tags$em("thesaurus_items_mapping_evals"), " : comprend les évaluations des alignements de concepts"),
          tags$li(tags$em("thesaurus_items_users"), " : lorsqu'un utilisateur modifie le nom d'un concept, le nouveau nom est enregistré ici")
        ),
        p(strong("Modules, plugins et scripts")),
        tags$ul(
          tags$li(tags$em("patient_lvl_modules_families"), " : les modules sont regroupés au sein de familles de modules, ce qui permettra dans une ",
            "version future de l'application de copier des modules d'une étude à l'autre"),
          tags$li(tags$em("patient_lvl_modules"), " : comprend les modules de type ", tags$em("Données individuelles")),
          tags$li(tags$em("patient_lvl_modules_elements"), " : comprend les widgets"),
          tags$li(tags$em("patient_lvl_modules_elements_items"), " : comprend les concepts / items du thésaurus sélectionnés pour tel ou tel widget"),
          tags$li(tags$em("aggregated_modules_families"), " : ces tables sont l'équivalent de celles de ", tags$em("patient_lvl"), ", pour les données agrégées"),
          tags$li(tags$em("aggregated_modules")),
          tags$li(tags$em("aggregated_modules_elements")),
          tags$li(tags$em("aggregated_modules_elements_items")),
          tags$li(tags$em("plugins"), " : liste les différents plugins locaux"),
          tags$li(tags$em("scripts"), " : comprend les différents scripts"),
        ),
        p(strong("Autres")),
        tags$ul(
          tags$li(tags$em("code"), " : les codes de différentes tables (datasets, thésaurus etc) sont stockés ici"),
          tags$li(tags$em("options"), " : les options de différentes tables (data_sources, datasets etc) sont stockées ici"),
          tags$li(tags$em("cache"), " : cette table permet de créer un cache pour les opérations prenant du temps ",
          " (création des datatables de thésaurus avec les boutons plus, moins, supprimer...)"),
          tags$li(tags$em("conversations"), " : les conversations que l'on créé au sein d'études sont stockées ici"),
          tags$li(tags$em("messages"), " : les messages associés aux conversations"),
          tags$li(tags$em("inbox_messages"), " : les messages non lus sont indiqués ici"),
          tags$li(tags$em("log"), " : le log des utilisateurs est stocké ici"),
          tags$li(tags$em("git_sources"), " : cette table permettra dans une future version de l'application de stocker les ",
          " différents sources de git distants (à sélectionner dans la liste des plugins sur git distant notamment)")
        ),
        p(strong("2) BDD publique")),
        tags$ul(
          tags$li(tags$em("subsets"), " : les différents subsets associés aux études"),
          tags$li(tags$em("subset_patients"), " : les patients de tel ou tel subset"),
          tags$li(tags$em("modules_elements_options"), " : cette table comporte les options associés aux widgets, ce qui permet",
            " aux plugins utilisés au sein de widgets d'être configurés par les utilisateurs"),
          tags$li(tags$em("patients_options"), " : cette table peut être utilisée pour stocker des informations sur les patients, dans une étude")
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p("Here are listed the different tables of the two databases of the application."),
        p("Choose the ", tags$em("Connection Type"), " (local or remote), and the ", tags$em("Database"), " (main or public)."),
        p(strong("1) Main database")),
        p("Here are the tables composing the main database, classified by groups."),
        p(strong("Users and Access Rights:")),
        tags$ul(
          tags$li(tags$em("users"), " : lists the users"),
          tags$li(tags$em("users_accesses"), " : lists the different users accesses"),
          tags$li(tags$em("users_statuses"), " : lists the different statuses of the users (data scientist, clinician, etc.)")
        ),
        p(strong("Data Management:")),
        tags$ul(
          tags$li(tags$em("data_sources"), " : includes the different data sources"),
          tags$li(tags$em("datasets"), " : the datasets associated with the data sources"),
          tags$li(tags$em("studies"), " : the different studies associated with the datasets"),
          tags$li(tags$em("thesaurus"), " : includes the different thesauri associated with the data sources"),
          tags$li(tags$em("thesaurus_items"), " : the concepts/items of the thesauri are stored here"),
          tags$li(tags$em("thesaurus_items_mapping"), " : the concept mappings are stored here"),
          tags$li(tags$em("thesaurus_items_mapping_evals"), " : includes the evaluations of the concept mappings"),
          tags$li(tags$em("thesaurus_items_users"), " : when a user modifies the name of a concept, the new name is saved here")
        ),
        p(strong("Modules, Plugins, and Scripts:")),
        tags$ul(
          tags$li(tags$em("patient_lvl_modules_families"), " : modules are grouped within families of modules, which will allow in a ",
            "future version of the application to copy the modules from one study to another"),
          tags$li(tags$em("patient_lvl_modules"), " : includes the modules of type ", tags$em("Patient-level Data")),
          tags$li(tags$em("patient_lvl_modules_elements"), " : includes the widgets"),
          tags$li(tags$em("patient_lvl_modules_elements_items"), " : includes the thesaurus concepts/items selected for a given widget"),
          tags$li(tags$em("aggregated_modules_families"), " : these tables are the equivalent of those of ", tags$em("patient_lvl"), ", for aggregated data"),
          tags$li(tags$em("aggregated_modules")),
          tags$li(tags$em("aggregated_modules_elements")),
          tags$li(tags$em("aggregated_modules_elements_items")),
          tags$li(tags$em("plugins"), " : lists the different local plugins"),
          tags$li(tags$em("scripts"), " : includes the different scripts"),
        ),
        p(strong("Others")),
        tags$ul(
          tags$li(tags$em("code"), " : the codes of different tables (datasets, thesaurus, etc.) are stored here"),
          tags$li(tags$em("options"), " : the options of different tables (data_sources, datasets, etc.) are stored here"),
          tags$li(tags$em("cache"), " : this table allows creating a cache for operations that take time ",
            " (creating thesaurus datatables with the plus, minus, delete buttons, etc.)"),
          tags$li(tags$em("conversations"), " : the conversations created within studies are stored here"),
          tags$li(tags$em("messages"), " : the messages associated with the conversations"),
          tags$li(tags$em("inbox_messages"), " : unread messages are indicated here"),
          tags$li(tags$em("log"), " : user logs are stored here"),
          tags$li(tags$em("git_sources"), " : this table will allow storing the ",
            "different remote git sources (to be selected from the list of plugins on remote git) in a future version of the application")
        ),
        p(strong("2) Public database")),
        tags$ul(
          tags$li(tags$em("subsets"), " : the different subsets associated with the studies"),
          tags$li(tags$em("subset_patients"), " : the patients in a given subset"),
          tags$li(tags$em("modules_elements_options"), " : this table contains the options associated with widgets, allowing",
            " plugins used within widgets to be configured by users"),
          tags$li(tags$em("patients_options"), " : this table can be used to store information about patients, in a study")
        ),
        br()
      )
    }
  })
  
  # Request database
  
  observeEvent(r$help_settings_app_database_page_4, {
    
    load_help_page(r)
    
    div_code_4 <- div(
      "SELECT u.username, u.firstname, u.lastname, ua.name AS user_access_name", br(),
      "FROM users u", br(),
      "LEFT JOIN users_accesses ua ON u.user_access_id = ua.id",
      style = "padding:5px; font-size:90%; font-family:monospace; color: #c7254e; background-color: #f9f2f4; border-radius:5px;"
    )
    
    r$help_settings_app_database_modal_title <- i18n$t("db_request_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("Requêtez ici la base de données"), ", en choisissant le ", tags$em("Type de connexion"), " (locale ou distante), et la ", 
          tags$em("Base de données"), " (principale ou publique)."),
        p("Exemple de requête :"),
        div_code_4,
        p("Référez-vous au ", strong("nom des tables"), " dans la catégorie ", tags$em("Tables de la BDD"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("Query the database here"), ", by choosing the ", tags$em("Type of connection"), " (local or remote), and the ",
          tags$em("Database"), " (main or public)."),
        p("Example of a query:"),
        div_code_4,
        p("Refer to the ", strong("table names"), " in the category ", tags$em("Database Tables"), "."),
        br()
      )
    }
  })
  
  # Export and restore database
  
  observeEvent(r$help_settings_app_database_page_5, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_export_and_restore")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("1) Exporter la base de données")),
        p("Vous pouvez faire des sauvegardes de la base de données depuis l'onglet ", tags$em("Exporter la BDD")),
        p("Vous pouvez choisir ou non d'exporter la table de Log en cochant ", tags$em("Exporter le log"), "."),
        p("Cliquez ensuite sur ", tags$em("Exporter la base de données"), "."),
        p("La base de données sera alors ", strong("téléchargée au format ZIP"), "."),
        p("Le fichier comprend ", strong("toutes les tables au format CSV"), ", ainsi qu'un fichier ", tags$em("db_info.xml"), 
          " qui renseigne notamment la version de l'application."),
        p(strong("2) Restaurer la base de données")),
        p("Pour restaurer la base de données, il vous suffit se sélectionner un fichier de sauvegarde en cliquant sur ", tags$em("Choisir un fichier ZIP"), "."),
        p("De la même façon que pour la sauvegarde, vous pouvez choisir d'importer ou non le log en cochant ", tags$em("Importer le log"), "."),
        p("Cliquez ensuite sur ", tags$em("Restaurer la base de données"), "."),
        p("Il vous faudra ", strong("redémarrer l'application"), " pour prendre en compte la restauration.")
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p(strong("1) Export the database")),
        p("You can make backups of the database from the ", tags$em("Export database"), " tab."),
        p("You can choose whether or not to export the Log table by checking the ", tags$em("Export log"), " box."),
        p("Then click on ", tags$em("Export database"), "."),
        p("The database will then be ", strong("downloaded in ZIP format"), "."),
        p("The file includes ", strong("all tables in CSV format"), ", as well as a ", tags$em("db_info.xml"),
          " file which specifies the application version."),
        p(strong("2) Restore the database")),
        p("To restore the database, simply select a backup file by clicking on ", tags$em("Choose a ZIP file"), "."),
        p("As with backup, you can choose whether or not to import the log by checking the ", tags$em("Import log"), " box."),
        p("Then click on ", tags$em("Restore database"), "."),
        p("You will need to ", strong("restart the application"), " to take into account the restoration.")
      )
    }
  })
  
}
