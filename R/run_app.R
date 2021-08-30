#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  translations <- get_translations()
  language <- "FR"
  css <- "style.css"
  page_style <- "fluent"
  pages <- c("home", "patient_level_data", "aggregated_data", "settings", "help")
  
  # router <- shiny.router::make_router(
  #   purrr::map(pages, ~ shiny.router::route(.x, make_layout(page_style = "fluent", page = .x))))
  
  router <- shiny.router::make_router(
    shiny.router::route("/", make_layout(language = language, page_style = "fluent", page = "home")),
    shiny.router::route("patient_level_data", make_layout(language = language, page_style = "fluent", page = "patient_level_data")),
    shiny.router::route("aggregated_data", make_layout(language = language, page_style = "fluent", page = "aggregated_data")),
    shiny.router::route("settings/general", make_layout(language = language, page_style = "fluent", page = "settings/general")),
    shiny.router::route("settings/data", make_layout(language = language, page_style = "fluent", page = "settings/data")),
    shiny.router::route("help", make_layout(language = language, page_style = "fluent", page = "help"))
  )
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui(router = router, css = css, page_style = page_style),
      server = app_server(router = router),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}