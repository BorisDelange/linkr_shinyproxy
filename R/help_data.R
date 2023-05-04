help_data <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
  
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_data_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("data")), br(), br(),
      shiny.fluent::Link(i18n$t("load_data"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("data_model"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("patient_lvl_or_aggregated_data"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      strong(i18n$t("tabs")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_tab"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_a_tab"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      strong(i18n$t("widgets")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_widget"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_a_widget"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({

    shiny.fluent::Modal(
      isOpen = r[[paste0("help_data_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_data_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_data_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_data_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  # Load data
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_1")]], {

    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("load_data")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(strong("1) Choisir un set de données")),
        p("Un set de données contient les données d'un ", strong("groupe de patients"), "."),
        p("Un même set de données peut contenir ", strong("plusieurs études"), "."),
        p("Choisissez le set de données dans le menu déroulant sur la gauche de l'écran."),
        p(strong("2) Chosir une étude")),
        p("Choisissez ensuite une étude, dans le menu déroulant."),
        p("Vous pouvez créer des études depuis l'onglet ", tags$em("Mes études"), " en haut de l'écran."),
        p("Une même étude peut contenir ", strong("plusieurs subsets"), "."),
        p(strong("3) Chosir un subset")),
        p("Un subset est un sous-ensemble du set de données, sur des patients sélectionnés."),
        p("Il est possible de ", strong("créer d'autres subsets"), " depuis l'onglet ", tags$em("Mes subsets"), "."),
        p(strong("4) Choisir un patient & un séjour")),
        p("En chargeant un subset, la liste des patients appartenant à ce subset est chargée dans le menu déroulant ", tags$em("Patient"),
          ", seulement si l'on se trouve dans les ", tags$em("Données individuelles"), "."),
        p("Les ", strong("widgets se mettent à jour"), " à chaque changement de patient & de séjour."), 
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(strong("1) Choose a dataset")),
        p("A dataset contains the data of a ", strong("group of patients"), "."),
        p("The same dataset can contain ", strong("multiple studies"), "."),
        p("Choose the dataset from the dropdown menu on the left of the screen."),
        p(strong("2) Choose a study")),
        p("Next, choose a study from the dropdown menu."),
        p("You can create studies from the ", tags$em("My studies"), " tab at the top of the screen."),
        p("The same study can contain ", strong("multiple subsets"), "."),
        p(strong("3) Choose a subset")),
        p("A subset is a subset of the dataset, containing selected patients."),
        p("It is possible to ", strong("create other subsets"), " from the ", tags$em("My subsets"), " tab."),
        p(strong("4) Choose a patient & a stay")),
        p("When loading a subset, the list of patients belonging to that subset is loaded into the ", tags$em("Patient"),
          " dropdown menu, only if you are in the ", tags$em("Individual data"), " section."),
        p("The ", strong("widgets update"), " every time the patient & stay are changed."), 
        br()
      )
    }
  })
  
  # Data model
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("data_model")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Au chargement d'un set de données, les variables suivantes se chargent."),
          p("Les colonnes de chaque variable sont détaillées, avec les noms, les descriptions des colonnes et le type de colonne (integer, character etc)."),
          p(strong("1) d$patients")),
          p("Variable contenant les informations sur les patients du set de données."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("gender"), " : Sexe (M / F) (character)"),
            tags$li(strong("dod"), " : Date de décès (dod pour date of death) (datetime)")
          ),
          p(strong("2) d$stays")),
          p("Contient les informations sur les séjours hospitaliers des patients."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("stay_id"), " : Identifiant unique du séjour hospitalier (integer)"),
            tags$li(strong("age"), " : Age du patient au moment de l'admission (numeric)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : Identifiant unique du concept de l'unité / du service hospitalier (integer)"),
            tags$li(strong("admission_datetime"), " : Date & heure d'admission dans l'unité / le service (datetime)"),
            tags$li(strong("discharge_datetime"), " : Date & heure de sortie de l'unité / du service (datetime)")
          ),
          p(strong("3) d$labs_vitals")),
          p("Contient les valeurs de la majorité des données structurées, hormis les données de prescription."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
            tags$li(strong("value"), " : Valeur textuelle (character)"),
            tags$li(strong("value_num"), " : Valeur numérique (numeric)"),
            tags$li(strong("unit"), " : Unité de la valeur (character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ),
          p(strong("4) d$orders")),
          p("Contient les données de prescriptions médicamenteuses."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur (datetime)"),
            tags$li(strong("route"), " : Voie d'administration du médicament (character)"),
            tags$li(strong("continuous"), " : Administration continue ou non (logical)"),
            tags$li(strong("amount"), " : Quantité du médicament / de la prescription non médicamenteuse (numeric)"),
            tags$li(strong("amount_unit"), " : Unité de la quantité (character)"),
            tags$li(strong("rate"), " : Débit du médicament (numeric)"),
            tags$li(strong("rate_unit"), " : Unité du débit (character)"),
            tags$li(strong("concentration"), " : Concentration du médicament (numeric)"),
            tags$li(strong("concentration_unit"), " : Unité de la concentration(character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ),
          p(strong("5) d$text")),
          p("Contient les données non structurées de type texte."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
            tags$li(strong("value"), " : Valeur textuelle (character)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ),
          p(strong("6) d$diagnoses")),
          p("Contient les données de diagnostic."),
          tags$ul(
            tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
            tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
            tags$li(strong("item_id"), " : identifiant unique du concept (integer)"),
            tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
            tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
            tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
          ), br()
      ) 
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # Patient-level or aggregated data
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("patient_lvl_or_aggregated_data")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Vous pouvez choisir dans le menu à gauche de charger les données individuelles ou agrégées."),
        p(strong("1) Modules & plugins différents")),
        p("Selon que vous choisissez les données individuelles ou agrégées, les tabs & widgets chargés diffèrent."),
        p("Lorsque vous chargez une étude, vous chargez :"),
        tags$ul(
          tags$li("D'un côté les onglets & widgets de données individuelles, permettant de ", strong("visualiser les données patient par patient"), "."),
          tags$li("De l'autre côté les onglets & widgets de données agrégées, permettant de ", strong("visualiser les données sur l'ensemble des patients ou sur le subset sélectionné."))
        ),
        p("En pratique, cela crée de ", strong("nouvelles variables"), " filtrant les variables générales sur ",
          strong("le subset, le patient ou sur le séjour sélectionné"), "."),
        p(strong("2) Données agrégées - Variables du subset sélectionné")),
        p("Lorsque vous sélectionnez un subset, les variables suivantes sont créées, avec la même structure que détaillée dans ", tags$em("Modèle de données"), " :"),
        tags$ul(
          tags$li(strong("d$data_subset$patients")),
          tags$li(strong("d$data_subset$stays")),
          tags$li(strong("d$data_subset$labs_vitals")),
          tags$li(strong("d$data_subset$orders")),
          tags$li(strong("d$data_subset$text")),
          tags$li(strong("d$data_subset$diagnoses"))
        ),
        p(strong("3) Données individuelles - Variables du patient sélectionné")),
        p("Lorsque vous sélectionnez un patient, les variables suivantes sont créées :"),
        tags$ul(
          tags$li(strong("d$data_patient$stays")),
          tags$li(strong("d$data_patient$labs_vitals")),
          tags$li(strong("d$data_patient$orders")),
          tags$li(strong("d$data_patient$text")),
          tags$li(strong("d$data_patient$diagnoses"))
        ),
        p(strong("4) Données individuelles - Variables du séjour sélectionné")),
        p("De la même façon, lorsque vous sélectionnez un séjour, les variables suivantes sont créées :"),
        tags$ul(
          tags$li(strong("d$data_stay$labs_vitals")),
          tags$li(strong("d$data_stay$orders")),
          tags$li(strong("d$data_stay$text")),
          tags$li(strong("d$data_stay$diagnoses"))
        ), br()
      ) 
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        
      )
    }
  })
  
  # What is a tab
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("whats_a_tab")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Une étude est ", strong("structurée autour d'onglets"), ", qui sont des ", strong("pages personnalisées"),
          " sur lesquelles je choisis ", strong("quelles données afficher et sous quelle forme"), "."),
        p(strong("1) Onglets de données individuelles")),
        p("Les onglets de données individuelles ", strong("reproduisent un dossier clinique"), "."),
        p("Par exemple, si je fais une étude sur le choc septique, je crée un onglet ", tags$em("Hémodynamique"),
            " où j'affiche la FC, la PAs, la PAd, la PAm & les doses reçues de Noradrénaline."),
        p(strong("2) Onglets de données agrégées")),
        p("Les onglets de données agrégées ", strong("permettent de conduire une étude"), " sur mes données."),
        p("Par exemple, je peux créer un onglet ", tags$em("Critères d'exclusion"), " où je vais créer mes critères ",
          "d'exclusion et les appliquer à mes patients."),
        p("Je peux également créer un onglet ", tags$em("Flowchart"), " pour afficher le flowchart de mon étude."), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("An study is ", strong("structured around tabs"), ", which are ", strong("custom pages"),
          " where I choose ", strong("which data to display and in what form"), "."),
        p(strong("1) Individual data tabs")),
        p("Individual data tabs ", strong("reproduce a clinical folder"), "."),
        p("For example, if I am conducting a study on septic shock, I can create a ", tags$em("Hemodynamics"),
          " tab where I display HR, SBP, DBP, MAP & the received doses of Norepinephrine."),
        p(strong("2) Aggregate data tabs")),
        p("Aggregate data tabs ", strong("allow to conduct a study"), " on my data."),
        p("For example, I can create an ", tags$em("Exclusion criteria"), " tab where I will create my exclusion criteria ",
          "and apply them to my patients."),
        p("I can also create a ", tags$em("Flowchart"), " tab to display the flowchart of my study."), br()
      )
    }
  })
  
  # Add a tab
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("add_a_tab")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Pour ajouter un onglet, il faut ", strong("avoir chargé une étude"), " dans le menu déroulant à gauche de l'écran."),
        p("Il faut ensuite cliquer sur l'icône :"),
        div(shiny.fluent::Icon(iconName = "Add"), span(i18n$t("add_a_tab"), style = "padding:0px 0px 10px 10px;")),
        p("Elle se trouve sous le titre (", tags$em("Données individuelles"), " ou ", tags$em("Données agrégées"), ")."),
        p("Ensuite, :"),
        tags$ul(
          tags$li(strong("Choisissez un nom"), " pour cet onglet."),
          tags$li(strong("Choisissez le niveau "), "de l'onglet. Faut-il qu'il soit au même niveau que l'onglet actuel,",
            " ou est-ce un sous-onglet de l'onglet actuellement sélectionné ?")
        ),
        p("Lorsque le menu ", tags$em("Ajouter un onglet"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux widgets."), br()
      ) 
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("To add a tab, you must ", strong("have loaded a study"), " in the dropdown menu on the left side of the screen."),
        p("Then click on the icon:"),
        div(shiny.fluent::Icon(iconName = "Add"), span(i18n$t("add_a_tab"), style = "padding:0px 0px 10px 10px;")),
        p("It is located under the title (", tags$em("Individual Data"), " or ", tags$em("Aggregate Data"), ")."),
        p("Then:"),
        tags$ul(
          tags$li(strong("Choose a name"), " for this tab."),
          tags$li(strong("Choose the level"), " of the tab. Should it be at the same level as the currently selected tab,",
            " or is it a sub-tab of the currently selected tab ?")
        ),
        p("When the ", tags$em("Add a Tab"), " menu is open, click on the cross on the right side of the menu to return to the widgets."), br()
      )
    }
  })
  
  # What is a widget
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("whats_a_widget")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Un onglet est ", strong("composé de widgets"), ", qui sont des plugins appliqués à des données."),
        p(strong("1) Plugins")),
        p("Les plugins sont des scripts écrits en R - Shiny permettant ", strong("d'ajouter des fonctionnalités à l'application"), "."),
        p("Quelques exemples :"),
        tags$ul(
          tags$li(strong("Plugin Datatable"), " : permet d'afficher des données sous forme de tableau."),
          tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, utile pour les prescriptions par exemple."),
          tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir des données d'une étude.")
        ),
        p("L'application a vocation à s'enrichir au fur et à mesure par la ", strong("création de nouveaux plugins"), "."),
        p("Les plugins des données individuelles ou agrégées ne sont pas les mêmes."),
        p(strong("2) Widgets")),
        p("Un widget est donc un plugin appliqué à des données."),
        p("Je choisis un plugin, quelles données vont être utilisées par ce plugin, puis le ",
        strong("plugin affiche ces données sous la forme désirée"), " (timeline pour le plugin timeline etc)."), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("A tab is ", strong("composed of widgets"), ", which are plugins applied to data."),
        p(strong("1) Plugins")),
        p("Plugins are R scripts - Shiny allowing ", strong("to add features to the application"), "."),
        p("Some examples:"),
        tags$ul(
          tags$li(strong("Datatable Plugin"), " : allows displaying data in the form of a table."),
          tags$li(strong("Timeline Plugin"), " : allows displaying data in the form of a timeline, useful for prescriptions for example."),
          tags$li(strong("Flowchart Plugin"), " : allows creating a Flowchart from study data.")
        ),
        p("The application is intended to become richer over time by ", strong("creating new plugins"), "."),
        p("The plugins for individual or aggregated data are not the same."),
        p(strong("2) Widgets")),
        p("A widget is therefore a plugin applied to data."),
        p("I choose a plugin, which data will be used by this plugin, and then the ",
          strong("plugin displays this data in the desired form"), " (timeline for the timeline plugin, etc.)."), br()
      )
    }
  })
  
  # Add a widget
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_7")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("add_a_widget")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Pour ajouter un widget, il faut ", strong("avoir chargé une étude "), " dans le menu déroulant à gauche de l'écran puis ",
        strong("avoir sélectionné un onglet"), "."),
        p("Il faut ensuite cliquer sur :"),
          div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_help")),
            i18n$t("add_a_widget"), iconProps = list(iconName = "Add"))),
        p("Ensuite, :"),
        tags$ul(
          tags$li(strong("Choisissez un nom"), " pour ce widget."),
          tags$li(strong("Choisissez le plugin "), " que vous souhaitez utiliser pour ce widget")
        ),
        p("S'il s'agit d'un widget de données agrégées, cliquez sur Ajouter et c'est terminé."),
        p("S'il s'agit d'un widget de données individuelles, vous devez :"),
        tags$ul(
          tags$li(strong("Sélectionner un thésaurus"), " : un thésaurus est un dictionnaire de concepts utilisés par un dataset."),
          tags$li(strong("Sélectionner les items "), " que vous souhaitez utiliser pour ce widget, avec le plugin sélectionné."),
          tags$li("Vous pouvez choisir des items liés à l'item sélectionné via le menu déroulant ", tags$em("Alignement de concepts"), ".")
        ),
        p("Lorsque le tableau des ", strong("items du thésaurus"), " est chargé, vous pouvez filtrer les données pour trouver les items qui vous intéresent :"),
        tags$ul(
          tags$li(strong("Nom / abréviation"), " : cherchez dans la barre de texte les items. En double-cliquant sur un nom, vous pouvez le changer : il sera affiché avec ce nouveau nom."),
          tags$li(strong("Unité"), " : utile essentiellement pour changer l'affichage de l'unité."),
          tags$li(strong("Couleur de l'item"), " : utile pour différencier les items sur un graphique par ex."),
          tags$li(strong("Patients"), " : affiche le nombre total de patients ayant au moins une fois l'item."),
          tags$li(strong("Lignes"), " : nombre d'occurences de l'item dans le set de données, tous patients confondus.")
        ),
        p("Ajouter ensuite les items en cliquant sur l'icône ", actionButton(ns(paste0(prefix, "_add_thesaurus_item_help")), "", icon = icon("plus")),
        " dans la dernière colonne du tableau."),
        p("Lorsque j'ajoute un item et que le menu ", tags$em("Alignement de concepts"), " contient une valeur, les items / concepts liés à l'item ajouté seront également ajoutés."),
        p("Si le bouton ", tags$em("Fusionner les concepts alignés"), " est activé, les différents items liés seront fusionnés dans le widget."),
        p("Par exemple, si j'ajoute le concept ", tags$em("Fréquence cardiaque"), " et que les items ", tags$em("FC"), " et ", tags$em("Fréq card"), " sont également ajoutés, ",
        "si je sélectionne la fusion, les différents items apparaîtront sous le nom ", tags$em("Fréquence cardiaque"), "."),
        p("Lorsque le menu ", tags$em("Nouveau widget"), " est ouvert, cliquez sur la croix à droite du menu pour retourner à l'onglet actuel."), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("To add a widget, you need to ", strong("load a study "), "from the dropdown menu on the left side of the screen, and then ",
          strong("select a tab"), "."),
        p("Then, click on:"),
        div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_help")),
          i18n$t("add_a_widget"), iconProps = list(iconName = "Add"))),
        p("Next, you need to:"),
        tags$ul(
          tags$li("Choose a name for this widget."),
          tags$li("Choose the plugin you want to use for this widget.")
        ),
        p("If it is an aggregate data widget, just click on Add and it's done."),
        p("If it is an individual data widget, you need to:"),
        tags$ul(
          tags$li("Select a thesaurus : a thesaurus is a dictionary of concepts used by a dataset."),
          tags$li("Select the items / concepts you want to use for this widget, with the selected plugin."),
          tags$li("You can choose items related to the selected item via the ", tags$em("Concept Alignment"), " dropdown menu.")
        ),
        p("When the table of ", strong("thesaurus items"), " is loaded, you can filter the data to find the items you are interested in:"),
        tags$ul(
          tags$li(strong("Name / abbreviation"), " : search for items in the text box. By double-clicking on a name, you can change it: it will be displayed with this new name."),
          tags$li(strong("Unit"), " : mainly useful for changing the display of the unit."),
          tags$li(strong("Item color"), " : useful for differentiating items on a graph, for example."),
          tags$li(strong("Patients"), " : displays the total number of patients who have had the item at least once."),
          tags$li(strong("Rows"), " : number of occurrences of the item in the dataset, for all patients.")
        ),
        p("Then, add the items by clicking on the ", tags$em("Add item"), " icon ", actionButton(ns(paste0(prefix, "_add_thesaurus_item_help")), "", icon = icon("plus")),
        " in the last column of the table."),
        p("When I add an item and the ", tags$em("Concept Alignment"), " menu contains a value, the items / concepts linked to the added item will also be added."),
        p("If the ", tags$em("Merge aligned concepts"), " button is enabled, the different linked items will be merged in the widget."),
        p("For example, if I add the concept ", tags$em("Heart rate"), " and the items ", tags$em("HR"), " and ", tags$em("H. rate"), " are also added, ",
          "if I select the merge, the different items will appear under the name ", tags$em("Heart rate"), "."),
        p("When the ", tags$em("New widget"), " menu is open, click on the cross on the right side of the menu to return to the current tab."), br()
      )
      
    }
  })
}
