#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  # --- --- -
  # Home ----
  # --- --- -
  
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
      div(id = ns("overview_card"), uiOutput(ns("overview_div"))),
      shinyjs::hidden(div( id = ns("news_card"), uiOutput(ns("news_div")))),
      shinyjs::hidden(div( id = ns("versions_card"), uiOutput(ns("versions_div")))),
    )
  }
  
  # --- --- -- -- --
  # Get started ----
  # --- --- -- -- --
  
  # if (id == "home_get_started") main <- div(
  #   shiny.fluent::Breadcrumb(items = list(
  #     list(key = "get_started", text = i18n$t("get_started"))
  #   ), maxDisplayedItems = 3),
  #   shiny.fluent::Pivot(
  #     onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
  #     shiny.fluent::PivotItem(id = "import_excel_card", itemKey = "import_excel", headerText = i18n$t("import_excel_file")),
  #     shiny.fluent::PivotItem(id = "import_csv_card", itemKey = "import_csv", headerText = i18n$t("import_csv_file")),
  #     shiny.fluent::PivotItem(id = "connect_db_card", itemKey = "connect_db", headerText = i18n$t("connect_to_database"))
  #   ),
  #   div(
  #     id = ns("import_excel_card"),
  #     make_card(i18n$t("import_excel_file"),
  #       div(
  #         br(),
  #         div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
  #         div(shiny.fluent::MessageBar(
  #           div(
  #             strong("A faire"),
  #             p("L'idée ici serait de pouvoir charger un fichier Excel, que cela crée un dataset & un thésaurus."),
  #             p("Pour cela, plusieurs problématiques :",
  #               tags$ul(
  #                 tags$li("Un format à respecter : un patient par ligne ?"),
  #                 tags$li("Formater les cellules : integer ? character ? factor ?"),
  #                 tags$li("Pour les facteurs, proposer d'uniformiser les textes disponibles (transformer des textes similaires pour n'avoir qu'un seul niveau de facteur)"),
  #                 tags$li("Identifier la colonne identifiant le patient, en faire un patient_id, anonymisant les identités"),
  #                 tags$li("Identifier les colonnes des données, qui seront des items de thésaurus"),
  #                 tags$li("Faire un pivot_longer, transformer les donneés au format de l'application")
  #               )  
  #             ),
  #             p("Faire un processus étape par étape.")
  #           ),
  #           messageBarType = 0)
  #         )
  #       )
  #     )
  #   ),
  #   shinyjs::hidden(
  #     div(
  #       id = ns("import_csv_card"),
  #       make_card(i18n$t("import_csv_file"),
  #         div(
  #           br(),
  #           div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
  #           div(shiny.fluent::MessageBar(
  #             div(
  #               strong("A faire"),
  #               p("On se trouve dans une problématique similaire à l'import de fichier Excel."),
  #               p("Formater, identifier les colonnes patient, données, pivoter.")
  #             ),
  #             messageBarType = 0)
  #           )
  #         )
  #       )
  #     )
  #   ),
  #   shinyjs::hidden(
  #     div(
  #       id = ns("connect_db_card"),
  #       make_card(i18n$t("connect_to_database"),
  #         div(
  #           br(),
  #           div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
  #           div(shiny.fluent::MessageBar(
  #             div(
  #               strong("A faire"),
  #               p("Proposer de se connecter à une base de données."),
  #               p("Une fois la connexion établie, on se retrouve avec les mêmes problématiques que pour les fichiers CSV."),
  #               p("Formater, identifier les colonnes patient, données, pivoter.")
  #             ),
  #             messageBarType = 0)
  #           )
  #         )
  #       )
  #     )
  #   )
  # )
  
  # --- --- -- - -
  # Tutorials ----
  # --- --- -- - -
  
  # if (id == "home_tutorials") main <- div(
  #   shiny.fluent::Breadcrumb(items = list(
  #     list(key = "tutorials", text = i18n$t("tutorials"))
  #   ), maxDisplayedItems = 3),
  #   shiny.fluent::Pivot(
  #     onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
  #     shiny.fluent::PivotItem(id = "tutorials", itemKey = "tutorials", headerText = i18n$t("tutorials"))
  #   ),
  #   div(
  #     id = ns("tutorials_card"),
  #     make_card(i18n$t("tutorials"),
  #       div(
  #         br(),
  #         div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
  #         div(shiny.fluent::MessageBar(
  #           div(
  #             strong("A faire"),
  #             p("Proposer des tutoriels, chargés depuis Github."),
  #             p("Quelques idées :",
  #               tags$ul(
  #                 tags$li("Tutoriels autour du tidyverse, de ggplot2, depuis l'application"),
  #                 tags$li("Tutoriels autour de la MIMIC-III, en l'intégrant à l'application, avec une étude simple, de A à Z (du style prédiction de la mortalité à J30)"),
  #                 tags$li("Tutoriel pour créer des plugins depuis l'application")
  #               )  
  #             )
  #           ),
  #           messageBarType = 0)
  #         )
  #       )
  #     )
  #   )
  # )
  
  # --- --- -- - -
  # Resources ----
  # --- --- -- - -
  
  if (id %in% c("home_get_started", "home_tutorials", "home_resources")){
    page <- substr(id, 6, nchar(id))
    
    main <- div(
      shiny.fluent::Breadcrumb(items = list(
        list(key = page, text = i18n$t(page))
      ), maxDisplayedItems = 3),
      uiOutput(ns(paste0(page, "_pivot"))),
      uiOutput(ns(paste0(page, "_cards")))
    )
  }
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    main
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id = character(), r, language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    if(perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - start"))
    
    observeEvent(input$current_tab, {
      
      if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - observer input$current_tab"))
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # --- --- --- --- --- --- --- --- --- -- -
    # Get CSV file and download markdowns ----
    # --- --- --- --- --- --- --- --- --- -- -
    
    csv_file <- tibble::tribble(
      ~language, ~page, ~type, ~category, ~key, ~title, ~content, ~markdown_file, ~datetime,
      "fr", "resources", "pivot_item", "health_data_warehouses", "health_data_warehouses", "Entrepôts de données de santé", "", "", "",
      "fr", "resources", "pivot_item", "learn_r", "learn_r", "Programmer en R", "", "", "",
      "fr", "resources", "pivot_item", "learn_sql", "learn_sql", "Programmer en SQL", "", "", "",
      "fr", "resources", "pivot_item", "learn_python", "learn_python", "Programmer en Python", "", "", "",
      "fr", "resources", "card", "health_data_warehouses", "div1", "Div 1", "Test, br(), test", "", "",
      "fr", "resources", "card", "health_data_warehouses", "div2", "Div 2", "Test2, br(), test", "", ""
    )
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_home_", id, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_home_", id, "_open_panel")]] <- FALSE)
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r[[paste0("help_home_", id, "_open_panel")]] <- FALSE
      r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
    })
    
    r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_home_", id, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
      r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_home_", id, "_page_", i)]] <- Sys.time())
    })
    
    help_home(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- -
    # Home ----
    # --- --- -
    
    if (id == "home"){
      
      cards <- c("overview_card", "news_card", "versions_card")
      
      default_div <- make_card("", shiny.fluent::MessageBar(i18n$t("error_connection_github"), messageBarType = 3))
      overview_div <- default_div
      news_div <- default_div
      versions_div <- default_div
      
      if (r$has_internet){
        
        # Overview div
        
        if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - download overview.Md"))
        
        tryCatch({
          filename <- paste0("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/home/overview_", language, ".Md")
          con <- textConnection(filename)
          overview_md <- readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax()
          overview_div <- make_card("", uiOutput(ns("overview_ui")))
          output$overview_ui <- renderUI(overview_md)
          close(con)
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_home_files", 
          error_name = "Home overview_card load markdown", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        if(perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home_server - ", id, " - download overview.Md"))
        
        # News div
        
        if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - download news/index.csv"))
        
        news_files <- tibble::tibble()
        tryCatch({
          news_files <- readr::read_csv("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/home/news/index.csv",
            col_types = "cc", show_col_types = FALSE) %>%
            dplyr::mutate(n = 1:dplyr::n()) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::select(-n)
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_home_files", 
          error_name = "Home news load csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        if(perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home_server - ", id, " - download news/index.csv"))
        
        news_div <- tagList()
        
        if (nrow(news_files) == 0) news_div <- make_card("", shiny.fluent::MessageBar(i18n$t("no_data_available"), messageBarType = 5))
        if (nrow(news_files) > 0){
          
          if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - loop over news files"))
        
          sapply(1:nrow(news_files), function(i){
            
            news_file <- news_files[i, ]
            
            datetime <- substr(news_file$file, 1, 16) %>% as.POSIXct(format = "%Y-%m-%d_%H-%M") %>% format_datetime(language, sec = FALSE)
            
            filepath_github <- paste0("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/home/news/", tolower(language), "/", news_file$file)
            con <- textConnection(filepath_github)
            news_md <- readLines(textConnection(filepath_github), warn = FALSE) %>% includeMarkdown() %>% withMathJax()
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
          
          if(perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home_server - ", id, " - loop over news files"))
        }
        
        # Versions div
        
        versions_files <- tibble::tibble()
        
        if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - download versions/index.csv"))
        
        tryCatch({
          versions_files <- readr::read_csv("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/home/versions/index.csv", 
            col_types = "cc", show_col_types = FALSE) %>%
            dplyr::mutate(n = 1:dplyr::n()) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::select(-n)
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_home_files", 
          error_name = "Home versions load csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        if(perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home_server - ", id, " - download versions/index.csv"))
        
        versions_div <- tagList()
        
        if (nrow(versions_files) == 0) versions_div <- make_card("", shiny.fluent::MessageBar(i18n$t("no_data_available"), messageBarType = 5))
        if (nrow(versions_files) > 0){
          
          if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - loop over versions files"))
          
          sapply(1:nrow(versions_files), function(i){
            
            versions_file <- versions_files[i, ]
            
            datetime <- substr(versions_file$file, 1, 16) %>% as.POSIXct(format = "%Y-%m-%d_%H-%M") %>% format_datetime(language, sec = FALSE)
            
            filepath_github <- paste0("https://raw.githubusercontent.com/BorisDelange/linkr-content/main/home/versions/", tolower(language), "/", versions_file$file)
            con <- textConnection(filepath_github)
            news_md <- readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax()
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
          if(perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home_server - ", id, " - loop over versions files"))
        }
      }
      
      output$overview_div <- renderUI(overview_div)
      output$news_div <- renderUI(news_div)
      output$versions_div <- renderUI(versions_div)
      
    } 
    
    # --- --- --- -- -
    # Get started ----
    # --- --- --- -- -
    
    # if (id == "home_get_started") cards <- c("import_excel_card", "import_csv_card", "connect_db_card")
    
    # --- --- --- --
    # Tutorials ----
    # --- --- --- --
    
    # if (id == "home_tutorials") cards <- c("tutorials_card")
    
    # --- --- --- --
    # Resources ----
    # --- --- --- --
    
    if (id %in% c("home_get_started", "home_tutorials", "home_resources")){
      
      page <- substr(id, 6, nchar(id))
      
      # --- --- --- -- --
      ## Pivot items ----
      # --- --- --- -- --
      
      pivot_items <- tagList()
      
      if (nrow(csv_file) > 0){
        
        pivot_items_rows <- csv_file %>% dplyr::filter(language == "fr", page == !!page, type == "pivot_item")
      
        for (i in 1:nrow(pivot_items_rows)){
          row <- pivot_items_rows[i, ]
          pivot_items <- tagList(pivot_items, shiny.fluent::PivotItem(id = row$key, itemKey = row$key, headerText = row$title))
        }
        
        output[[paste0(page, "_pivot")]] <- renderUI(
          shiny.fluent::Pivot(
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", page, "_pivot_current_tab', item.props.id)")),
            pivot_items
          )
        )
      }
      
      page_cards <- csv_file %>% dplyr::filter(page == !!page, type == "card")
      page_divs <- page_cards %>% dplyr::pull(key)
      categories <- csv_file %>% dplyr::filter(page == !!page, type == "pivot_item") %>% dplyr::pull(category)
      
      observeEvent(input[[paste0(page, "_pivot_current_tab")]], {
        sapply(page_divs, shinyjs::hide)
        sapply(categories, function(category) shinyjs::hide(paste0(category, "_summary")))
        shinyjs::show(paste0(input[[paste0(page, "_pivot_current_tab")]], "_summary"))
      })
      
      # --- --- ---
      # Cards ----
      # --- --- ---
      
      ui_result <- tagList()
      
      # Create summary cards
      
      if (length(categories) > 0){
        for (i in 1:length(categories)){
  
          category <- categories[i]
  
          cards_rows <- csv_file %>% dplyr::filter(page == !!page, type == "card", category == !!category)
  
          summary_links <- tagList()
  
          if (nrow(cards_rows) > 0){
            for (j in 1:nrow(cards_rows)){
              row <- cards_rows[j, ]
              summary_links <- tagList(summary_links,
                shiny.fluent::Link(row$title, onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", page, "_current_card', '", row$key , "')"))), br(),
              )
            }
          }
  
          summary_card <- div(
            id = ns(paste0(category, "_summary")),
            make_card(
              i18n$t("summary"),
              div(paste0(category, "_summary"), br(), br(), summary_links)
            )
          )
  
          if (i == 1) ui_result <- tagList(ui_result, summary_card)
          else ui_result <- tagList(ui_result, shinyjs::hidden(summary_card))
        }
      }
      
      # Create other cards
      
      if (nrow(page_cards) > 0){
        for (i in 1:nrow(page_cards)){
          row <- page_cards[i, ]
          ui_result <- tagList(ui_result,
            shinyjs::hidden(
              div(
                id = ns(row$key),
                make_card(
                  row$title,
                  row$content
                )
              )
            )
          )
        }
      }
      
      observeEvent(input[[paste0(page, "_current_card")]], {
        sapply(page_divs, shinyjs::hide)
        shinyjs::show(input[[paste0(page, "_current_card")]])
      })
      
      output[[paste0(page, "_cards")]] <- renderUI(div(ui_result))
    }
    
    if (debug) print(paste0(Sys.time(), " - mod_home_server - ", id, " - end"))
  })
}
