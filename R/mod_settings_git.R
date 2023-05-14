#' mod_settings_git UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_git_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("git_add_repo_card", "git_repo_management_card")
  
  forbidden_cards <- tagList()
  for (card in cards) forbidden_cards <- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "remote_git_repos", text = i18n$t("remote_git_repos"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "git_add_repo_card", itemKey = "git_add_repo_card", headerText = i18n$t("add_git_repo")),
      shiny.fluent::PivotItem(id = "git_repo_management_card", itemKey = "git_repo_management_card", headerText = i18n$t("git_repo_management")),
    ),
    forbidden_cards,
    
    shinyjs::hidden(
      div(id = ns("git_add_repo_card"),
        make_card(i18n$t("add_git_repo"), div(
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_textfield(i18n = i18n, ns = ns, label = "name", id = "name", width = "300px"),
            make_textfield(i18n = i18n, ns = ns, label = "description", id = "description", width = "600px")
          ),
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_dropdown(i18n = i18n, ns = ns, label = "category", id = "category", multiSelect = FALSE, width = "300px",
              options = list(
                list(key = "plugin", text = i18n$t("plugins")),
                list(key = "script", text = i18n$t("scripts"))
              ), value = "plugin"),
            make_textfield(i18n = i18n, ns = ns, label = "url_address", id = "url_address", width = "600px")
          ), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo"), i18n$t("add"))
        ))
      )
    ),
    shinyjs::hidden(
      div(id = ns("git_repo_management_card"),
        
      )
    ), br()
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_git_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("git_add_repo_card", "git_repo_management_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    if ("git_add_repo_card" %in% r$user_accesses) shinyjs::show("git_add_repo_card")
    else shinyjs::show("git_add_repo_card_forbidden")
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    # observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_dev_open_panel <- TRUE)
    # observeEvent(input$hide_panel, r$help_settings_dev_open_panel <- FALSE)

    # r$help_settings_dev_open_panel_light_dismiss <- TRUE
    # observeEvent(input$show_modal, r$help_settings_dev_open_modal <- TRUE)
    # observeEvent(input$hide_modal, {
    #   r$help_settings_dev_open_modal <- FALSE
    #   r$help_settings_dev_open_panel_light_dismiss <- TRUE
    # })

    # observeEvent(shiny.router::get_page(), {
    #   if (debug) print(paste0(Sys.time(), " - mod_settings_git - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_settings_dev_open_panel <- FALSE
    #   r$help_settings_dev_open_modal <- FALSE
    # })

    # sapply(1:10, function(i){
    #   observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_dev_page_", i)]] <- Sys.time())
    # })

    # help_settings_dev(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- -- -
    # Create a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_git_repo, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$add_git_repo"))
        
      new_data <- list()
      for (name in c("name", "description", "category", "url_address")) new_data[[name]] <- coalesce2(type = "char", x = input[[name]])
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = "git_sources", required_textfields = c("name", "url_address"), req_unique_values = "name")
      
      # Reload datatable
      r$git_sources_temp <- r$git_sources %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer input$add_git_repo"))
    })
  
  })
}
