#' Run the Shiny Application
#'
#' @description 
#' Runs the cdwtools Shiny Application.\cr
#' Use language argument to choose default language to use ("EN" or "FR").\cr
#' Use options to set options to shiny::shinyApp() function, see documentation of shinyApp for more informations.
#' @param options A list of options that could be passed to shiny::shinyApp() function (list)
#' @param language Default language to use in the App (character)
#' @param ... arguments to pass to golem_opts. 
#' @examples 
#' \dontrun{
#' cdwtools(language = "EN")
#' }
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
#' @importFrom magrittr %>%

cdwtools <- function(
  options = list(),
  language = "EN",
  ...
) {
  
  translations <- get_translations()
  
    css <- "fluent_style.css"
     
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
      shiny.router::route("help/user_data_management", make_layout(language = language, page_style = page_style, page = "help/user_data_management")),
      shiny.router::route("help/user_modules_plugins", make_layout(language = language, page_style = page_style, page = "help/user_modules_plugins")),
      shiny.router::route("help/user_patient_lvl_data", make_layout(language = language, page_style = page_style, page = "help/user_patient_lvl_data")),
      shiny.router::route("help/user_aggregated_data", make_layout(language = language, page_style = page_style, page = "help/user_aggregated_data")),
      shiny.router::route("help/dev_app_db", make_layout(language = language, page_style = page_style, page = "help/dev_app_db")),
      shiny.router::route("help/dev_users", make_layout(language = language, page_style = page_style, page = "help/dev_users")),
      shiny.router::route("help/dev_data_management", make_layout(language = language, page_style = page_style, page = "help/dev_data_management")),
      shiny.router::route("help/dev_modules_plugins", make_layout(language = language, page_style = page_style, page = "help/dev_modules_plugins"))
    ) -> page
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui(css = css, page_style = page_style, page = page),
      server = app_server(router = page, language = language),
      options = options
    ), 
    golem_opts = list(...)
  )
}