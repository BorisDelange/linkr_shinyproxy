#' my_subsets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_subsets_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("datatable_card", "creation_card", "management_card", "edit_code_card")
  language <- "en"
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, language = language, words = words))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "subset_main", text = i18n$t("my_subsets"))
    ), maxDisplayedItems = 3),
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("subsets_management")),
          shiny.fluent::PivotItem(id = "creation_card", itemKey = "creation_card", headerText = i18n$t("create_subset")),
          shiny.fluent::PivotItem(id = "management_card", itemKey = "management_card", headerText = i18n$t("subset_management")),
          shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_subset_code"))
        )
      )
    ),
    forbidden_cards,
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(
      div(
        id = ns("datatable_card"),
        make_card(i18n$t("subsets_management"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Créer un datatable pour changer le nom, supprimer les subsets du datamart")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("creation_card"),
        make_card(i18n$t("create_subset"), 
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Création d'un subset, en choisissant le nom, vérifier qu'il n'est pas utilisé.")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("management_card"),
        make_card(i18n$t("subset_management"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("On choisit un subset."),
                p("On peut :",
                  tags$ul(
                    tags$li("Filter des patients sur des paramètres"),
                    tags$li("Ajouter des patients"),
                    tags$li("Supprimer des patients")
                  )
                )
              ),
              messageBarType = 0)
            )
          )
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("edit_code_card"),
        make_card(i18n$t("edit_subset_code"),
          div(
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5), style = "margin-top:10px;"), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Création de subsets avec du code, directement.")
              ),
              messageBarType = 0)
            )
          )
        )
      )
    )
  )
}
    
#' my_subsets Server Functions
#'
#' @noRd 
mod_my_subsets_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    # if (id == "patient_level_data_subsets") prefix <- "patient_lvl"
    # if (id == "aggregated_data_subsets") prefix <- "aggregated"
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("management_card", "edit_code_card", "creation_card", "datatable_card")
    # show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    observeEvent(input$current_tab, {
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a datamart in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar1, show_message_bar_new(output, 1, r$show_message_bar1$message, r$show_message_bar1$type, i18n = i18n, ns = ns))
    observeEvent(r$show_message_bar2, show_message_bar_new(output, 2, r$show_message_bar2$message, r$show_message_bar2$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- --
    # Render subsets UI ----
    # --- --- --- --- --- --
    
    observeEvent(m$chosen_study, {
      req(!is.na(m$chosen_study))
      
      # Show first card & hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        shinyjs::show("datatable_card")
        # if ("subset_datatable_card" %in% r$user_accesses) shinyjs::show("subset_datatable_card")
        # else shinyjs::show("subset_datatable_card_forbidden")
      }
      
    })
    
    observeEvent(m$chosen_subset, {
      
      # Render subset UI
      # ...
      
      # Subset data are loaded when the study is loaded
    })
    
  })
}
