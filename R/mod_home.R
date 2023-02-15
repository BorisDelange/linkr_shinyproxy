#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  if (id == "home"){
    
    main <- div(
      shiny.fluent::Breadcrumb(items = list(
        list(key = "home", text = i18n$t("home"))
      ), maxDisplayedItems = 3),
      shiny.fluent::Pivot(
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "overview_card", itemKey = "overview", headerText = i18n$t("overview")),
        shiny.fluent::PivotItem(id = "news_card", itemKey = "news", headerText = i18n$t("news")),
        shiny.fluent::PivotItem(id = "versions_card", itemKey = "versions", headerText = i18n$t("versions"))
      ),
      div(id = ns("overview_card"), make_card(i18n$t("overview"), uiOutput(ns("overview_div")))),
      shinyjs::hidden(div( id = ns("news_card"), uiOutput(ns("news_div")))),
      shinyjs::hidden(div( id = ns("versions_card"), uiOutput(ns("versions_div")))),
    )
  }
  
  if (id == "home_get_started") main <- div(
    shiny.fluent::Breadcrumb(items = list(
      list(key = "get_started", text = i18n$t("get_started"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "import_excel_card", itemKey = "import_excel", headerText = i18n$t("import_excel_file")),
      shiny.fluent::PivotItem(id = "import_csv_card", itemKey = "import_csv", headerText = i18n$t("import_csv_file")),
      shiny.fluent::PivotItem(id = "connect_db_card", itemKey = "connect_db", headerText = i18n$t("connect_to_database"))
    ),
    div(
      id = ns("import_excel_card"),
      make_card(i18n$t("import_excel_file"),
        div(
          br(),
          div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
          div(shiny.fluent::MessageBar(
            div(
              strong("A faire"),
              p("L'idée ici serait de pouvoir charger un fichier Excel, que cela crée un datamart & un thésaurus."),
              p("Pour cela, plusieurs problématiques :",
                tags$ul(
                  tags$li("Un format à respecter : un patient par ligne ?"),
                  tags$li("Formater les cellules : integer ? character ? factor ?"),
                  tags$li("Pour les facteurs, proposer d'uniformiser les textes disponibles (transformer des textes similaires pour n'avoir qu'un seul niveau de facteur)"),
                  tags$li("Identifier la colonne identifiant le patient, en faire un patient_id, anonymisant les identités"),
                  tags$li("Identifier les colonnes des données, qui seront des items de thésaurus"),
                  tags$li("Faire un pivot_longer, transformer les donneés au format de l'application")
                )  
              ),
              p("Faire un processus étape par étape.")
            ),
            messageBarType = 0)
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("import_csv_card"),
        make_card(i18n$t("import_csv_file"),
          div(
            br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("On se trouve dans une problématique similaire à l'import de fichier Excel."),
                p("Formater, identifier les colonnes patient, données, pivoter.")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("connect_db_card"),
        make_card(i18n$t("connect_to_database"),
          div(
            br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Proposer de se connecter à une base de données."),
                p("Une fois la connexion établie, on se retrouve avec les mêmes problématiques que pour les fichiers CSV."),
                p("Formater, identifier les colonnes patient, données, pivoter.")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    )
  )
  
  if (id == "home_tutorials") main <- div(
    shiny.fluent::Breadcrumb(items = list(
      list(key = "tutorials", text = i18n$t("tutorials"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "tutorials", itemKey = "tutorials", headerText = i18n$t("tutorials"))
    ),
    div(
      id = ns("tutorials_card"),
      make_card(i18n$t("tutorials"),
        div(
          br(),
          div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
          div(shiny.fluent::MessageBar(
            div(
              strong("A faire"),
              p("Proposer des tutoriels, chargés depuis Github."),
              p("Quelques idées :",
                tags$ul(
                  tags$li("Tutoriels autour du tidyverse, de ggplot2, depuis l'application"),
                  tags$li("Tutoriels autour de la MIMIC-III, en l'intégrant à l'application, avec une étude simple, de A à Z (du style prédiction de la mortalité à J30)"),
                  tags$li("Tutoriel pour créer des plugins depuis l'application")
                )  
              )
            ),
            messageBarType = 0)
          )
        )
      )
    )
  )
  
  if (id == "home_resources") main <- div(
    shiny.fluent::Breadcrumb(items = list(
      list(key = "resources", text = i18n$t("resources"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "resources", itemKey = "resources", headerText = i18n$t("resources"))
    ),
    div(
      id = ns("resources_card"),
      make_card(i18n$t("resources"),
        div(
          br(),
          div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
          div(shiny.fluent::MessageBar(
            div(
              strong("A faire"),
              p("Proposer des ressources, chargées depuis Github."),
              p("Quelques idées :",
                tags$ul(
                  tags$li("Ressources R & RStudio, tidyverse etc"),
                  tags$li("Ressources SQL"),
                  tags$li("Ressources bases de données, entrepôts de données de santé, les différentes architectures"),
                  tags$li("Bases de données disponibles (MIMIC, eICU, AmsterdamUMCdb)")
                )  
              )
            ),
            messageBarType = 0)
          )
        )
      )
    )
  )
  
  if (id == "home_dev") main <- div(
    shiny.fluent::Breadcrumb(items = list(
      list(key = "dev", text = i18n$t("dev"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "dev", itemKey = "dev", headerText = i18n$t("dev"))
    ),
    div(
      id = ns("dev_card"),
      make_card("",
        div(
          shiny.fluent::MessageBar(
            div(
              strong("A faire pour la version 0.1.3"),
              p(
                tags$ul(
                  # tags$li("Plugins : vignettes, infos en XML sur Github, code sur Github, identifiant unique par plugin"),
                  # tags$li("Pages d'accueil"),
                  # tags$li("Pages de mise en place, Excel / CSV / connexion BDD"),
                  # tags$li("Pages de tutoriels"),
                  # tags$li("Pages de ressources"),
                  # tags$li("Corrections Thésaurus - items (confirmation recharger cache, ajouter cache pour valeurs exemples)"),
                  # tags$li("Création page Catégories pour Thésaurus"),
                  # tags$li("Création page Conversions pour Thésaurus"),
                  # tags$li("Création page Créer des items pour Thésaurus"),
                  # tags$li("Pages d'aide sur chaque page"),
                  # tags$li("Modifier forme Options accès utilisateurs"),
                  # tags$li("Pages de subset"),
                  # tags$li("Revoir les droits par utilisateur, pour l'affichage des pages (ajouter celles qui n'existaient pas encore"),
                  # tags$li("Ajouter des items dans Settings / data sources"),
                  # tags$li("Conversions / Catégories / Mapping dans Settings / thesaurus"),
                  tags$li("Résoudre les bugs suivants ",
                    tags$ul(
                      tags$li("Lors connexion à BDD appli distante, switch pour la nouvelle BDD sans changer de compte"),
                      # tags$li("En choisissant sur paramètres dans Gestion accès, afficher la page Options accès")
                    )  
                  )
                )  
              )
            ),
            messageBarType = 5), br(),
          shiny.fluent::MessageBar(
            div(
              strong("Fait pour la version 0.1.3"),
              p(
                tags$ul(
                  
                )  
              )
            ),
            messageBarType = 4)
        )
      )
    )
  )
  
  div(class = "main",
      
    render_settings_default_elements(ns = ns),
    main
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id = character(), r, language = "en", i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # Show or hide datamart cards
    
    if (id == "home"){
      
      cards <- c("overview_card", "news_card", "versions_card")
      
      if (!r$has_internet){
        default_div <- make_card("", shiny.fluent::MessageBar(i18n$t("error_connection_github"), messageBarType = 3))
        overview_div <- default_div
        news_div <- default_div
        versions_div <- default_div
      }
      if (r$has_internet){
        
        # Overview div
        
        con <- textConnection("https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/overview.Md")
        overview_div <- readLines(con) %>% includeMarkdown() %>% withMathJax()
        close(con)
        
        # News div
        
        tryCatch(news_files <- readr::read_csv("https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/news/index.csv", col_types = "cc") %>%
          dplyr::mutate(n = 1:dplyr::n()) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::select(-n), error = function(e) "", warning = function(w) "")
        
        news_div <- tagList()
        
        if (nrow(news_files) == 0) news_div <- make_card("", shiny.fluent::MessageBar(i18n$t("no_data_available"), messageBarType = 5))
        if (nrow(news_files) > 0){
        
          sapply(1:nrow(news_files), function(i){
            
            news_file <- news_files[i, ]
            
            datetime <- substr(news_file$file, 1, 16) %>% as.POSIXct(format = "%Y-%m-%d_%H-%M")
            if (tolower(language) == "fr") datetime <- format(as.POSIXct(datetime), format = "%d-%m-%Y %H:%M")
            if (tolower(language) == "en") datetime <- format(as.POSIXct(datetime), format = "%Y-%m-%d %H:%M")
            
            filepath_github <- paste0("https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/news/", tolower(language), "/", news_file$file)
            con <- textConnection(filepath_github)
            news_md <- readLines(textConnection(filepath_github)) %>% includeMarkdown() %>% withMathJax()
            close(con)
            
            output_name <- paste0("news_", substr(news_file$file, 1, 16))
            
            news_div <<- tagList(news_div, 
              make_card(
                tagList(news_file$title, span(" - ", datetime, style = "font-size:12px;")), 
                uiOutput(ns(output_name))
              )
            )
            output[[output_name]] <- renderUI(news_md)
          })
        }
        
        # Versions div
        
        versions_files <- tibble::tibble()
        
        tryCatch(versions_files <- readr::read_csv("https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/versions/index.csv", col_types = "cc") %>%
          dplyr::mutate(n = 1:dplyr::n()) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::select(-n), error = function(e) "", warning = function(w) "")
        
        versions_div <- tagList()
        
        if (nrow(versions_files) == 0) versions_div <- make_card("", shiny.fluent::MessageBar(i18n$t("no_data_available"), messageBarType = 5))
        if (nrow(versions_files) > 0){
          
          sapply(1:nrow(versions_files), function(i){
            
            versions_file <- versions_files[i, ]
            
            datetime <- substr(versions_file$file, 1, 16) %>% as.POSIXct(format = "%Y-%m-%d_%H-%M")
            if (tolower(language) == "fr") datetime <- format(as.POSIXct(datetime), format = "%d-%m-%Y %H:%M")
            if (tolower(language) == "en") datetime <- format(as.POSIXct(datetime), format = "%Y-%m-%d %H:%M")
            
            filepath_github <- paste0("https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/versions/", tolower(language), "/", versions_file$file)
            con <- textConnection(filepath_github)
            news_md <- readLines(con) %>% includeMarkdown() %>% withMathJax()
            close(con)
            
            output_name <- paste0("news_", substr(versions_file$file, 1, 16))
            
            news_div <<- tagList(news_div, 
              make_card(
                tagList(versions_file$title, span(" - ", datetime, style = "font-size:12px;")), 
                uiOutput(ns(output_name))
              )
            )
            output[[output_name]] <- renderUI(news_md)
          })
        }
      }
      
      output$overview_div <- renderUI(overview_div)
      output$news_div <- renderUI(news_div)
      output$versions_div <- renderUI(versions_div)
      
    } 
    if (id == "home_get_started") cards <- c("import_excel_card", "import_csv_card", "connect_db_card")
    if (id == "home_tutorials") cards <- c("tutorials_card")
    if (id == "home_resources") cards <- c("resources_card")
    if (id == "home_dev") cards <- c("dev_card")
    
    observeEvent(input$current_tab, {
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
  })
}