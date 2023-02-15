#' Run the Shiny Application
#'
#' @description 
#' Runs the cdwtools Shiny Application.\cr
#' Use language argument to choose language to use ("EN" or "FR" available).\cr
#' Use options to set options to shiny::shinyApp() function, see documentation of shiny::shinyApp for more informations.\cr\cr
#' To connect to distant database, use db_info argument. db_info is a list, containing these parameters :\cr
#' \itemize{
#' \item{\strong{sql_lib} : SQL library used, "postgres" or "sqlite" available for now}
#' \item{\strong{dbname} : database name (character)}
#' \item{\strong{host} : host connection name (character)}
#' \item{\strong{port} : connection port (integer)}
#' \item{\strong{user} : user name (character)}
#' \item{\strong{password} : password in clear (character)}
#' }
#' @param options A list of options that could be passed to shiny::shinyApp() function (list)
#' @param language Default language to use in the App (character)
#' @param db_info Database connection informations, if it is needed to connect a distant db (list).
#' @param datamarts_folder Folder where to save datamarts CSV files (character)
#' @param app_db_folder Folder where to save local database file (character)
#' @param perf_monitoring Monitore app performances print timestamps step by step (boolean)
#' @param ... arguments to pass to golem_opts. 
#' @examples 
#' \dontrun{
#' db_info <- list(
#'   sql_lib = "postgres",
#'   dbname = "cdwtools",
#'   host = "localhost",
#'   port = 5432,
#'   user = "admin",
#'   passord = "admin"
#' )
#' datamarts_folder <- "C:/Users/John/My CDW project/data"
#' app_db_folder <- "C:/Users/John/My CDW project"
#' cdwtools(language = "EN", db_info = db_info, datamarts_folder = datamarts_folder, app_db_folder = app_db_folder)
#' }
#' See `?golem::get_golem_options` for more details.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
#' @importFrom magrittr %>%

linkr <- function(
  language = "EN",
  db_info = list(),
  app_folder = character(),
  app_db_folder = character(),
  datamarts_folder = character(),
  perf_monitoring = FALSE,
  debug = FALSE,
  options = list(),
  ...
) {
  
  # Maximum size for uploaded data (500 MB)
  # Used to restore database
  # shiny.launch.browser to automatically open browser
  
  if (debug) print(paste0(Sys.time(), " - linkr - init"))
  options(shiny.maxRequestSize = 500*1024^2, shiny.launch.browser = TRUE)
  
  suppressMessages(require(shinyTree))
  
  # Create app folder if it doesn't exist
  if (debug) print(paste0(Sys.time(), " - linkr - app_folder"))
  if (length(app_folder) == 0) app_folder <- paste0(path.expand("~"), "/linkr")
  
  if (!dir.exists(app_folder)){
    tryCatch(
      dir.create(app_folder),
      error = function(e) print(e)
    )
  }
  
  # Clear temp dir
  if (debug) print(paste0(Sys.time(), " - linkr - temp dir"))
  unlink(paste0(app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
  
  # Create app sub-dirs
  if (debug) print(paste0(Sys.time(), " - linkr - app sub-dirs"))
  sub_dirs <- c("app_database", "datamarts", "messages", "plugins", "studies", "temp_files", "thesaurus")
  for (sub_dir in sub_dirs) if (!dir.exists(paste0(app_folder, "/", sub_dir))) dir.create(paste0(app_folder, "/", sub_dir))
  
  # Initial wd
  # initial_wd <- getwd()
  
  # Set wd to app wd
  # setwd(find.package("cdwtools"))
  
  # Load translations
  # Update : use shiny.i18n instead. When it is done, delete get_translations
  # Change also tolower...
  # words <- get_translations()
  if (debug) print(paste0(Sys.time(), " - linkr - translation"))
  i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = "translations"))
  i18n$set_translation_language(tolower(language))
  
  options(digits.secs = 0)

  css <- "fluent_style.css"
  
  if (debug) print(paste0(Sys.time(), " - linkr - make_router"))
  pages <- c(
    "home", 
      "home/get_started", 
      "home/tutorials", 
      "home/resources", 
      # "home/dev",
    "my_studies", 
    "my_subsets", 
    "thesaurus", 
    "data", 
    "scripts", 
    "patient_level_data", 
    "aggregated_data",
    "plugins/patient_lvl",
    "plugins/aggregated",
    "settings/general_settings",
    "settings/app_db",
    "settings/users", 
    "settings/dev", 
    "settings/data_sources",
    "settings/datamarts", 
    "settings/thesaurus", 
    "settings/log")
  
  do.call(shiny.router::make_router, lapply(pages, function(page) shiny.router::route(page, 
    make_layout(language = language, page = page, i18n = i18n)))) -> page
  
  # Load UI & server
  
  if (debug) print(paste0(Sys.time(), " - linkr - load UI & server"))
  with_golem_options(
    app = shinyApp(
      ui = app_ui(css = css, page = page, language = language, debug = debug),
      server = app_server(router = page, language = language, db_info = db_info, 
        app_folder = app_folder, datamarts_folder = datamarts_folder, app_db_folder = app_db_folder,# initial_wd = initial_wd, 
        perf_monitoring = perf_monitoring, debug = debug),
      options = options
    ), 
    golem_opts = list()
  )
}