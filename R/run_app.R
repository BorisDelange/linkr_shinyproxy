#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
#' @importFrom magrittr %>%
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  language = "EN",
  css = "fluent_style.css",
  page_style = "fluent",
  db_info = list(),
  # page_theme = "",
  # router_on = TRUE,
  ...
) {
  
  # devtools::load_all(".")
  # run_app(language = "FR", db_info = list(dbname = "cdwtools", host = "localhost", port = 5432, user = "postgres", password = "postgres"))
  
  translations <- get_translations()
  # language <- "EN"
  # css <- "style.css"
  # page_style <- "fluid"
  # page_theme <- "lumen"
  # router_on <- FALSE
  pages <- c("home", "patient_level_data", "aggregated_data", "settings", "help")
  page <- ""
  
  # router <- shiny.router::make_router(
  #   purrr::map(pages, ~ shiny.router::route(.x, make_layout(page_style = "fluent", page = .x))))
  
  routes <- vector("character", )
  
  if (page_style == "fluent"){
    css <- "fluent_style.css"
    # shiny.router::make_router(sapply(pages, function(page){
    #   shiny.router::route(page, make_layout(language = language, page_style = page_style, page = page))})) -> page
      
    shiny.router::make_router(
      shiny.router::route("home/datamarts_studies", make_layout(language = language, page_style = page_style, page = "home/datamarts_studies")),
      shiny.router::route("home/messages", make_layout(language = language, page_style = page_style, page = "home/messages")),
      shiny.router::route("patient_level_data", make_layout(language = language, page_style = page_style, page = "patient_level_data")),
      shiny.router::route("aggregated_data", make_layout(language = language, page_style = page_style, page = "aggregated_data")),
      shiny.router::route("settings/general", make_layout(language = language, page_style = page_style, page = "settings/general")),
      shiny.router::route("settings/app_db", make_layout(language = language, page_style = page_style, page = "settings/app_db")),
      shiny.router::route("settings/users", make_layout(language = language, page_style = page_style, page = "settings/users")),
      shiny.router::route("settings/data_sources", make_layout(language = language, page_style = page_style, page = "settings/data_sources")),
      shiny.router::route("settings/datamarts", make_layout(language = language, page_style = page_style, page = "settings/datamarts")),
      shiny.router::route("settings/studies", make_layout(language = language, page_style = page_style, page = "settings/studies")),
      shiny.router::route("settings/subsets", make_layout(language = language, page_style = page_style, page = "settings/subsets")),
      shiny.router::route("settings/thesaurus", make_layout(language = language, page_style = page_style, page = "settings/thesaurus")),
      shiny.router::route("settings/plugins", make_layout(language = language, page_style = page_style, page = "settings/plugins")),
      shiny.router::route("settings/modules_patient_lvl", make_layout(language = language, page_style = page_style, page = "settings/modules_patient_lvl")),
      shiny.router::route("settings/modules_aggregated", make_layout(language = language, page_style = page_style, page = "settings/modules_aggregated")),
      shiny.router::route("settings/log", make_layout(language = language, page_style = page_style, page = "settings/log")),
      shiny.router::route("help/get_started", make_layout(language = language, page_style = page_style, page = "help/get_started")),
      shiny.router::route("help/data_management", make_layout(language = language, page_style = page_style, page = "help/data_management"))
    ) -> page
  }
  
  if (page_style == "fluid"){
    css <- "fluent_style.css"
    page_theme <- "lumen"
    shiny::fluidPage(
      theme = shinythemes::shinytheme(page_theme),
      shiny::navbarPage(
        title = "CDW Tools",
        make_layout(language = language, page_style = page_style, page = "home"),
        make_layout(language = language, page_style = page_style, page = "aggregated_data"),
        make_layout(language = language, page_style = page_style, page = "patient_level_data"),
        make_layout(language = language, page_style = page_style, page = "settings"),
        make_layout(language = language, page_style = page_style, page = "messages")
      )
    ) -> page
  }
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui(css = css, page_style = page_style, page = page),
      server = app_server(page_style = page_style, router = page, language = language, db_info = db_info),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}