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
  app_folder = character(),
  perf_monitoring = FALSE,
  debug = FALSE,
  local = FALSE,
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
  sub_dirs <- c("app_database", "datamarts", "messages", "plugins", "studies", "temp_files", "vocabularies", "translations")
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
    "my_studies", 
    "my_subsets", 
    "vocabularies", 
    "data", 
    "scripts", 
    "patient_level_data", 
    "aggregated_data",
    "plugins",
    "plugins_patient_lvl",
    "plugins_aggregated",
    "settings/general_settings",
    "settings/app_db",
    "settings/users", 
    "settings/dev", 
    "settings/data_sources",
    "settings/datamarts", 
    "settings/vocabularies",
    "settings/log")
  
  # Toggles for users accesses
  
  options_toggles <- tibble::tribble(
    ~name, ~toggles,
    "general_settings", "change_password_card",
    "app_db", c(
      "db_connection_infos_card",
      "db_datatable_card",
      "db_request_card",
      "db_save_card",
      "db_restore_card"),
    "users", c(
      "users_creation_card",
      "users_management_card",
      "users_accesses_management_card",
      "users_accesses_options_card",
      "users_statuses_management_card"),
    "dev", c(
      "dev_edit_code_card",
      "dev_perf_monitoring_card"),
    "data_sources", c(
      "data_sources_datatable_card"),
    "datamarts", c(
      "datamarts_see_all_data",
      "datamarts_datatable_card",
      "datamarts_options_card",
      "datamarts_edit_code_card"),
    "studies", c(
      "studies_see_all_data",
      "studies_messages_card",
      "studies_datatable_card",
      "studies_options_card"),
    "subsets", c(
      "subsets_datatable_card",
      "subsets_edit_code_card",
      "subsets_patients_card"),
    "vocabularies", c(
      "vocabularies_items_card",
      "vocabularies_mapping_card",
      "vocabularies_datatable_card",
      "vocabularies_sub_datatable_card",
      "vocabularies_edit_code_card"),
    "plugins", c(
      "plugins_see_all_data",
      "all_plugins_card",
      "plugins_datatable_card",
      "plugins_options_card",
      "plugins_edit_code_card",
      "import_plugin_card",
      "export_plugin_card"),
    "scripts", c(
      "datamart_scripts_card",
      "scripts_descriptions_card",
      "scripts_datatable_card",
      "scripts_edit_code_card",
      "scripts_options_card"
    ),
    "log", c(
      "all_users",
      "only_me")
  )
  
  do.call(shiny.router::make_router, 
    lapply(pages, function(page){
      if (debug) print(paste0(Sys.time(), " - linkr - make_router - ", page))
      shiny.router::route(page, make_layout(language = language, page = page, i18n = i18n, options_toggles = options_toggles))
    })
  ) -> page
  
  # Load UI & server
  
  if (debug) print(paste0(Sys.time(), " - linkr - load UI & server"))
  with_golem_options(
    app = shinyApp(
      ui = app_ui(css = css, page = page, language = language, debug = debug),
      server = app_server(router = page, language = language, app_folder = app_folder, 
        perf_monitoring = perf_monitoring, debug = debug, local = local, options_toggles = options_toggles),
      options = options
    ), 
    golem_opts = list()
  )
}