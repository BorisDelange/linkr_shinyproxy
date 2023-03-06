help_data <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
  
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_data_", prefix, "_open_panel")]],
      br(),
      strong("Category 1"), br(), br(),
      shiny.fluent::Link("Link 1", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link("Link 2", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong("Category 2"), br(), br(),
      shiny.fluent::Link("Link 3", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
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
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_1")]], {

    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- "Help page 1"
    r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
      p("Paragraph 1")
    )
  })
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- "Help page 2"
    r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
      p("Paragraph 2")
    )
  })
}


# help_panel_words <- tibble::tribble(~language, ~reference_word, ~translated_word,
#   "EN", "load_data", "Load data",
#   "FR", "load_data", "Charger des données",
#   "EN", "data_model", "Understanding the data model",
#   "FR", "data_model", "Comprendre le modèle de données",
#   "EN", "ind_or_agg_data", "Individual or aggregated data ?",
#   "FR", "ind_or_agg_data", "Données individuelles ou agrégées ?",
#   "EN", "whats_a_module", "What is a tab ?",
#   "FR", "whats_a_module", "Qu'est ce qu'un onglet ?",
#   "EN", "add_module", "Add a tab",
#   "FR", "add_module", "Ajouter un onglet",
#   "EN", "delete_module", "Delete a tab",
#   "FR", "delete_module", "Supprimer un onglet",
#   "EN", "modules_elements", "Modules",
#   "FR", "modules_elements", "Modules",
#   "EN", "whats_a_module_element", "What is a module ?",
#   "FR", "whats_a_module_element", "Qu'est ce qu'un module ?",
#   "EN", "add_module_element", "Add a module",
#   "FR", "add_module_element", "Ajouter un module",
#   "EN", "delete_modul_elemente", "Delete a module",
#   "FR", "delete_module_element", "Supprimer un module"
# )
# 
# output$help_panel <- shiny.fluent::renderReact({
#   
#   shiny.fluent::Panel(
#     headerText = translate(language, "help", r$words),
#     isOpen = r[[paste0(prefix, "_open_help_panel")]], br(),
#     strong(translate(language, "data", r$words)), br(), br(),
#     shiny.fluent::Link(translate(language, "load_data", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_1', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "data_model", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_2', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "ind_or_agg_data", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_3', Math.random()); }"))), br(), br(),
#     strong(translate(language, "modules", r$words)), br(), br(),
#     shiny.fluent::Link(translate(language, "whats_a_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_4', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "add_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_5', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "delete_module", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_6', Math.random()); }"))), br(), br(),
#     strong(translate(language, "modules_elements", help_panel_words)), br(), br(),
#     shiny.fluent::Link(translate(language, "whats_a_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_7', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "add_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_8', Math.random()); }"))), br(), br(),
#     shiny.fluent::Link(translate(language, "delete_module_element", help_panel_words), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_9', Math.random()); }"))), br(), br(),
#     isLightDismiss = r[[paste0(prefix, "_open_help_panel_light_dismiss")]],
#     isBlocking = r[[paste0(prefix, "_open_help_panel_light_dismiss")]],
#     onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
#     onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
#   )
# })
# 
# # Help modal
# 
# observeEvent(input$show_modal, r[[paste0(prefix, "_open_help_modal")]] <- TRUE)
# observeEvent(input$hide_modal, {
#   r[[paste0(prefix, "_open_help_modal")]] <- FALSE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- TRUE
# })
# 
# observeEvent(input$help_1, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Charger des données"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p(strong("1) Choisir un datamart")),
#       p("Un datamart est un ", strong("set de données"), ", auquel vous avez accès. Il contient les données d'un ", strong("groupe de patients"), "."),
#       p("Un même datamart peut contenir ", strong("plusieurs études différentes"), "."),
#       p("Choisissez le datamart sur la gauche de l'écran, dans le menu déroulant."),
#       p(strong("2) Chosir une étude")),
#       p("Choisissez ensuite une étude, dans le menu déroulant."),
#       p("Une même étude peut contenir ", strong("plusieurs subsets différents"), "."),
#       p(strong("3) Chosir un subset")),
#       p("Il existe trois subsets par défaut :"),
#       tags$ul(
#         tags$li("Tous les patients"),
#         tags$li("Patients inclus"),
#         tags$li("Patients exclus")
#       ),
#       p("Il est possible de ", strong("créer d'autres subsets"), " dans l'onglet ", tags$em("Mes subsets"), " en haut de l'écran."),
#       p(strong("4) Choisir un patient & un séjour")),
#       p("En chargeant un subset, la liste des patients appartenant à ce subset est chargée dans le menu déroulant ", tags$em("Patient"), 
#         ", seulement si l'on se trouve dans les ", tags$em("Données individuelles"), "."),
#       p("Les modules se ", strong("mettent en jour"), " à chaque changement de patient & de séjour.")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_2, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Comprendre le modèle de données"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Au chargement d'un datamart, cinq variables se chargent."),
#       p("Les colonnes de chaque variable sont détaillées, avec les noms complets et le type de colonne (integer, character etc)."),
#       p(strong("1) d$patients")),
#       tags$ul(
#         tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
#         tags$li(strong("gender"), " : Sexe (M / F) (character)"),
#         tags$li(strong("age"), " : Age (character)"),
#         tags$li(strong("dod"), " : Date de décès (dod pour date of death) (datetime)")
#       ),
#       p(strong("2) d$stays")),
#       tags$ul(
#         tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
#         tags$li(strong("stay_id"), " : Identifiant unique du séjour hospitalier (integer)"),
#         tags$li(strong("unit_name"), " : Nom de l'unité / du service du séjour hospitalier (character)"),
#         tags$li(strong("admission_datetime"), " : Date & heure d'admission dans l'unité / dans le service (datetime)"),
#         tags$li(strong("discharge_datetime"), " : Date & heure de sortie de l'unité / de service (datetime)")
#       ),
#       p(strong("3) d$labs_vitals")),
#       tags$ul(
#         tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
#         tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
#         tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
#         tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
#         tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
#         tags$li(strong("value"), " : Valeur textuelle (character)"),
#         tags$li(strong("value_num"), " : Valeur numérique (numeric)"),
#         tags$li(strong("unit"), " : Unité de la valeur (character)"),
#         tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
#       ),
#       p(strong("4) d$orders")),
#       tags$ul(
#         tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
#         tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
#         tags$li(strong("item_id"), " : Identifiant unique du concept (integer)"),
#         tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
#         tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur (datetime)"),
#         tags$li(strong("route"), " : Voie d'administration du médicament (character)"),
#         tags$li(strong("continuous"), " : Administration continue ou non (logical)"),
#         tags$li(strong("amount"), " : Quantité du médicament / de la prescription non médicamenteuse (numeric)"),
#         tags$li(strong("amount_unit"), " : Unité de la quantité (character)"),
#         tags$li(strong("rate"), " : Débit du médicament (numeric)"),
#         tags$li(strong("rate_unit"), " : Unité du débit (character)"),
#         tags$li(strong("concentration"), " : Concentration du médicament (numeric)"),
#         tags$li(strong("concentration_unit"), " : Unité de la concentration(character)"),
#         tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
#       ),
#       p(strong("5) d$text")),
#       tags$ul(
#         tags$li(strong("patient_id"), " : Identifiant unique du patient (integer)"),
#         tags$li(strong("thesaurus_name"), " : Nom du thésaurus comprenant le concept (character)"),
#         tags$li(strong("item_id"), " : identifiant unique du concept (integer)"),
#         tags$li(strong("datetime_start"), " : Date & heure de début de valeur (datetime)"),
#         tags$li(strong("datetime_stop"), " : Date & heure de fin de la valeur, optionnel (datetime)"),
#         tags$li(strong("value"), " : Valeur textuelle (character)"),
#         tags$li(strong("comments"), " : Commentaires sur la valeur (character)")
#       )
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_3, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Données individuelles ou agrégées ?"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Vous pouvez choisir dans le menu à gauche de charger les données individuelles ou agrégées."),
#       p(strong("1) Modules & plugins différents")),
#       p("Selon que vous choisissiez les données individuelles ou agrégées, les modules & plugins chargés diffèrent."),
#       p("Lorsque vous chargez une étude, vous chargez :"),
#       tags$ul(
#         tags$li("D'un côté les onglets & modules de données individuelles, permettant de ", strong("visualiser les données patient par patient")),
#         tags$li("De l'autre côté les onglets & modules de données agrégées, permettant de ", strong("visualiser les données sur l'ensemble des patients ou sur le subset sélectionné"))
#       ),
#       p("En pratique, cela crée de ", strong("nouvelles variables"), " filtrant les variables générales sur ", 
#         strong("le subset, le patient ou sur le séjour sélectionné"), "."),
#       p(strong("2) Données agrégées - Variables du subset sélectionné")),
#       p("Lorsque vous sélectionnez un subset, les variables suivantes sont créées, avec la même structure que détaillée dans ", tags$em("Comprendre le modèle de données"), " :"),
#       tags$ul(
#         tags$li(strong("d$data_subset$patients")),
#         tags$li(strong("d$data_subset$stays")),
#         tags$li(strong("d$data_subset$labs_vitals")),
#         tags$li(strong("d$data_subset$orders")),
#         tags$li(strong("d$data_subset$text"))
#       ),
#       p(strong("3) Données individuelles - Variables du patient sélectionné")),
#       p("Lorsque vous sélectionnez un patient, les variables suivantes sont créées :"),
#       tags$ul(
#         tags$li(strong("r$data_patient$stays")),
#         tags$li(strong("r$data_patient$labs_vitals")),
#         tags$li(strong("r$data_patient$orders")),
#         tags$li(strong("r$data_patient$text"))
#       ),
#       p(strong("4) Données individuelles - Variables du séjour sélectionné")),
#       p("De la même façon, lorsque vous sélectionnez un séjour, les variables suivantes sont créées :"),
#       tags$ul(
#         tags$li(strong("d$data_stay$labs_vitals")),
#         tags$li(strong("d$data_stay$orders")),
#         tags$li(strong("d$data_stay$text"))
#       )
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_4, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Qu'est ce qu'un onglet ?"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Une étude est ", strong("structurée autour d'onglets"), ", qui sont en quelque sorte des ", strong("pages personnalisées"),
#         " sur lesquelles je choisis ", strong("quelles données afficher et sous quelle forme"), "."),
#       p(strong("1) Onglets de données individuelles")),
#       p("Les onglets de données individuelles ", strong("reproduisent un dossier clinique"), "."),
#       p("Par exemple, si je fais une étude sur le choc septique, je crée un onglet de données individuelles ", tags$em("Hémodynamique"),
#           " où j'affiche la FC, la PAs, la PAd, la PAm & les doses reçues de Noradrénaline."),
#       p(strong("2) Onglets de données agrégées")),
#       p("Les onglets de données agrégées ", strong("permettent de conduire une étude"), " sur mes données."),
#       p("Sur cette même étude, je choisis de créer un onglet ", tags$em("Critères d'exclusion"), " où je vais créer mes critères ",
#         "d'exclusion et les appliquer à mes patients."),
#       p("Je peux également créer un onglet ", tags$em("Flowchart"), " pour afficher le flowchart de mon étude.")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_5, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Ajouter un onglet"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Pour ajouter un onglet, il faut ", strong("avoir chargé une étude"), " dans le menu déroulant à gauche de l'écran."),
#       p("Il faut ensuite cliquer sur l'icône :"),
#       div(shiny.fluent::Icon(iconName = "Add"), span(translate(language, "add_module", r$words), style = "padding:0px 0px 10px 10px;")), 
#       p("Elle se trouve sous le titre (", tags$em("Données individuelles"), " ou ",
#         tags$em("Données agrégées"), ")."),
#       p("Ensuite, :"),
#       tags$ul(
#         tags$li(strong("Choisissez un nom"), " pour cet onglet"),
#         tags$li(strong("Choisissez le niveau "), "de l'onglet. Faut-il qu'il soit au même niveau que l'onglet actuel,",
#           " ou est-ce un sous-onglet de l'onglet actuellement sélectionné ?")
#       ),
#       p("Lorsque le menu ", tags$em("Ajouter un onglet"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux modules.")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_6, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Supprimer un onglet"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Pour supprimer un onglet, ", strong("cliquez sur l'onglet en question"), " puis cliquez sur :"),
#       div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_remove_module_help")), 
#         translate(language, "remove_module", isolate(r$words)), iconProps = list(iconName = "Delete"))),
#       p("Supprimer un onglet supprime ", strong("tous les modules situés dans cet onglet"),
#         ", ainsi que ", strong("tous les onglets situés sous cet onglet"), ".")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_7, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Qu'est ce qu'un module ?"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Un onglet est ", strong("composé de modules"), ", qui sont des plugins appliqués à des données."),
#       p(strong("1) Plugins")),
#       p("Les plugins sont des scripts écrits en R - Shiny, permettant ", strong("d'ajouter une fonctionnalité à l'application"), "."),
#       p("Quelques exemples :"),
#       tags$ul(
#         tags$li(strong("Plugin Datatable"), " : permet d'afficher des données sous forme de tableau."),
#         tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, utile pour les prescriptions par exemple."),
#         tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir des données d'une étude.")
#       ),
#       p("L'application a vocation à s'enrichir au fur et à mesure par la ", strong("création de nouveaux plugins"), "."),
#       p("Les plugins des données individuelles ou agrégées ne sont pas les mêmes."),
#       p(strong("2) Modules")),
#       p("Un module est donc un plugin appliqué à des données."),
#       p("Je choisis un plugin, quelles données vont être utilisées par ce plugin, puis le ",
#         strong("plugin affiche ces données sous la forme désirée"), " (timeline pour le plugin timeline etc).")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_8, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Ajouter un module"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Pour ajouter un module, il faut ", strong("avoir chargé une étude "), " dans le menu déroulant à gauche de l'écran puis ",
#         strong("avoir sélectionné un onglet"), "."),
#       p("Il faut ensuite cliquer sur :"),
#         div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_module_element_help")), 
#           translate(language, "new_module_element", isolate(r$words)), iconProps = list(iconName = "Add"))),
#       p("Ensuite, :"),
#       tags$ul(
#         tags$li(strong("Choisissez un nom"), " pour ce module"),
#         tags$li(strong("Choisissez le plugin "), " que vous souhaitez utiliser pour ce module")
#       ),
#       p("S'il s'agit d'un module de données agrégées, cliquez sur Ajouter et c'est terminé."),
#       p("S'il s'agit d'un module de données individuelles, vous devez :"),
#       tags$ul(
#         tags$li(strong("Sélectionner un thésaurus"), " : un thésaurus est un dictionnaire de concepts utilisés par un datamart."),
#         tags$li(strong("Sélectionner les items "), " que vous souhaitez utiliser pour cet élément de module, avec le plugin sélectionné.")
#       ),
#       p("Lorsque le tableau des ", strong("items du thésaurus"), " est chargé, vous pouvez filtrer les données pour trouver les items qui vous intéresent :"),
#       tags$ul(
#         tags$li(strong("Nom d'affichage"), " : cherchez dans la barre de texte les items. En double-cliquant sur un nom, vous pouvez le changer : il sera affiché avec ce nouveau nom."),
#         tags$li(strong("Catégorie"), " : filtrez les données sur les catégories, plusieurs catégories peuvent être sélectionnées à la fois."),
#         tags$li(strong("Unité"), " : utile essentiellement pour changer l'affichage de l'unité."),
#         tags$li(strong("Couleur de l'item"), " : utile pour différencier les items sur un graphique par ex."),
#         tags$li(strong("Patients"), " : affiche le nombre total de patients ayant au moins une fois l'item."),
#         tags$li(strong("Lignes"), " : nombre d'occurences de l'item dans le datamart, tous patients confondus.")
#       ),
#       p("Ajouter ensuite les items en cliquant sur l'icône "),
#       div(actionButton(ns(paste0(prefix, "_add_thesaurus_item_help")), "", icon = icon("plus"))),
#       p(" dans la dernière colonne du tableau."),
#       p("Lorsque le menu ", tags$em("Nouveau module"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux modules.")
#       
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# observeEvent(input$help_9, {
#   
#   if (language == "FR"){
#     r[[paste0(prefix, "_help_modal_title")]] <- "Supprimer un module"
#     
#     r[[paste0(prefix, "_help_modal_text")]] <- div(
#       p("Pour supprimer un module, ", strong("cliquez sur "), " :"),
#       div(actionButton(ns(paste0(prefix, "_remove_module_element_help")), "", icon = icon("trash-alt"))),
#       p("Cette icône se trouve en haut à droite du module.")
#     )
#   }
#   
#   else if (language == "EN"){
#     r[[paste0(prefix, "_help_modal_title")]] <- ""
#     r[[paste0(prefix, "_help_modal_text")]] <- div()
#   }
#   
#   r[[paste0(prefix, "_open_help_modal")]] <- TRUE
#   r[[paste0(prefix, "_open_help_panel_light_dismiss")]] <- FALSE
#   
# })
# 
# output$help_modal <- shiny.fluent::renderReact({
#   
#   shiny.fluent::Modal(
#     isOpen = r[[paste0(prefix, "_open_help_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
#     onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
#     div(
#       style = "width: 1000px; padding: 15px 10px 0px 15px;",
#       shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
#         div(style = list(display = "flex"),
#           shiny.fluent::Text(r[[paste0(prefix, "_help_modal_title")]], variant = "large"),
#           div(style = list(flexGrow = 1)),
#           shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
#         ),
#         r[[paste0(prefix, "_help_modal_text")]]
#       )
#     )
#   )
# })
# 
