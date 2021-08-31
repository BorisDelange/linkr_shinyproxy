#' Translates a word
#'
#' @param language A two-letters or three-letters character indicating the requested language
#' @param words A tibble containing the translations
#' @description Translates a reference word to the requested language
#'
#' @return Returns the translated word (character)
#' @examples
#' words <- dplyr::tribble(~language, ~reference_word, ~translated_word,
#' "FR", "Settings", "Parametres",
#' "IT", "Settings", "Impostazioni")
#' 
#' translate("ESP", words, "Settings")
#' translate("FR", words, "Settings")

translate <- function(language, reference_word){
  words <- get_translations()
  if (nchar(language) %not_in% c(2, 3)) stop("Input 'language' has less than 2 or more than 3 characters")
  if (!is.data.frame(words)) stop("Input 'words' is not a dataframe")
  if (!is.character(reference_word) | nchar(reference_word) == 0) stop("Input 'reference word' is not a character or is of length 0")
  words %>%
    dplyr::filter(language == !!language, reference_word == !!reference_word) %>%
    dplyr::pull(translated_word)
}

#' Create a tibble with translations
#'@noRd
#'

get_translations <- function(){
  needed_file <- "./data/translations.csv"
  # if (!file.exists(needed_file)){
    dplyr::tribble(~language, ~reference_word, ~translated_word,
            "EN", "home", "Home",
            "FR", "home", "Accueil",
            "EN", "patient_level_data", "Patient-level data",
            "FR", "patient_level_data", "Données individuelles",
            "EN", "aggregated_data", "Aggregated data",
            "FR", "aggregated_data", "Données agrégées",
            "EN", "settings", "Settings",
            "FR", "settings", "Paramètres",
            "EN", "help", "Help",
            "FR", "help", "Aide",
            "EN", "datamart", "Datamart",
            "FR", "datamart", "Datamart",
            "EN", "datamarts", "Datamarts",
            "FR", "datamarts", "Datamarts",
            "EN", "datamarts_studies", "Datamarts & studies",
            "FR", "datamarts_studies", "Datamarts & études",
            "EN", "study", "Study",
            "FR", "study", "Etude",
            "EN", "subset", "Subset",
            "FR", "subset", "Subset",
            "EN", "patient", "Patient",
            "FR", "patient", "Patient",
            "EN", "general", "General",
            "FR", "general", "Général",
            "EN", "general_settings", "General settings",
            "FR", "general_settings", "Paramètres généraux",
            "EN", "data_management", "Data management",
            "FR", "data_management", "Gestion des données",
            "EN", "data_sources", "Data sources",
            "FR", "data_sources", "Sources de données",
            "EN", "language", "Language",
            "FR", "language", "Langue",
            "EN", "save", "Save",
            "FR", "save", "Sauvegarder",
            "EN", "appearance", "Appearance",
            "FR", "appearance", "Apparence",
            "EN", "page_type", "Page type",
            "FR", "page_type", "Type de page",
            "EN", "modules", "Modules",
            "FR", "modules", "Modules",
            "EN", "modules_patient_lvl", "Patient-level data",
            "FR", "modules_patient_lvl", "Données individuelles",
            "EN", "modules_aggregated", "Aggregated data",
            "FR", "modules_aggregated", "Données agrégées",
            "EN", "dev_mode", "Developper mode",
            "FR", "dev_mode", "Mode développeur",
            "EN", "search", "Search",
            "FR", "search", "Rechercher",
            "EN", "choices", "Choices",
            "FR", "choices", "Choix",
            "EN", "code", "Code",
            "FR", "code", "Code",
            "EN", "page_theme", "Theme",
            "FR", "page_theme", "Thème",
            "EN", "messages", "Messages",
            "FR", "messages", "Messages",
            "EN", "disconnect", "Disconnect",
            "FR", "disconnect", "Déconnexion",
            "EN", "users", "Users",
            "FR", "users", "Utilisateurs",
            "EN", "log", "Log",
            "FR", "log", "Historique",
            "EN", "app_db", "App database",
            "FR", "app_db", "BDD de l'application",
            "EN", "db_connexion_type", "Connexion type",
            "FR", "db_connexion_type", "Type de connexion",
            "EN", "local", "Local",
            "FR", "local", "Locale",
            "EN", "distant", "Distant",
            "FR", "distant", "Distante"
            ) -> data
    # readr::write_csv(data, needed_file)
  # }
  # if (file.exists(needed_file)) vroom::vroom(file = needed_file, col_types = c("")) -> data
  data
}