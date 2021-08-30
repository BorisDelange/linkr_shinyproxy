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
  root_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  other_page <- make_layout(page_style = "fluent", mod_page_main_ui("page_main_ui_1"))
  
  router <- shiny.router::make_router(
    shiny.router::route("/", root_page),
    shiny.router::route("other", other_page)
  )
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui(router = router),
      server = app_server(router = router),
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}