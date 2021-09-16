#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_sidenav_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  ##########################################
  # Fluent                                 #
  ##########################################
  
  if (page_style == "fluent"){
    
    ##########################################
    # Fluent / Home                          #
    ##########################################
    
    if (grepl("^home", page)){
      div(class = "sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = list(
              list(name = translate(language, "datamarts_studies"), key = "datamarts_studies",
                   url = shiny.router::route_link("home/datamarts_studies")),
              list(name = translate(language, "messages"), key = "messages",
                   url = shiny.router::route_link("home/messages"))
              )
            )
          ),
          initialSelectedKey = "datamarts_studies",
          selectedKey = substr(page, nchar("home") + 2, 100)
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Patient-level data            #
    ##########################################
    
    if (page == "patient_level_data"){
      div(class = "sidenav",
        div(class = "input_title_first", translate(language, "datamart")),
        shiny.fluent::Dropdown.shinyInput("datamart", value = "ufh",
                                          options = list(list(key = "ufh", text = "Cohorte héparine"),
                                                         list(key = "wmv", text = "Sevrage ventilation"))),
        div(class = "input_title", translate(language, "study")),
        shiny.fluent::Dropdown.shinyInput("study", value = "study1",
                                          options = list(list(key = "study1", text = "Etude 1 - premier anti-Xa"),
                                                         list(key = "study2", text = "Etude 2 - tous les anti-Xa"))),
        div(class = "input_title", translate(language, "subset")),
        shiny.fluent::Dropdown.shinyInput("subset", value = "all_patients",
                                          options = list(list(key = "all_patients", text = "Tous les patients"),
                                                         list(key = "included_patients", text = "Patients inclus"))),
        htmltools::br(), htmltools::hr(),
        div(class = "input_title", translate(language, "patient")),
        shiny.fluent::Dropdown.shinyInput("patient", value = "1442",
                                          options = list(list(key = "1442", text = "1442 - M - 82 ans"),
                                                         list(key = "4653", text = "4653 - F - 45 ans"))),
        make_dropdown(language, ns, "stay", list(
          list(key = "cardio", text = "Cardio 02/09 - 04/09/20"),
          list(key = "pneumo", text = "Pneumo 04/09 - 09/09/20")),
          value = "cardio"
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Aggregated data               #
    ##########################################
    
    if (page == "aggregated_data"){
      div(class = "sidenav",
          div(class = "input_title_first", translate(language, "datamart")),
          shiny.fluent::Dropdown.shinyInput("datamart", "Datamart", value = "ufh",
                                            options = list(list(key = "ufh", text = "Cohorte héparine"),
                                                           list(key = "wmv", text = "Sevrage ventilation"))),
          div(class = "input_title", translate(language, "study")),
          shiny.fluent::Dropdown.shinyInput("study", "Study", value = "study1",
                                            options = list(list(key = "study1", text = "Etude 1 - premier anti-Xa"),
                                                           list(key = "study2", text = "Etude 2 - tous les anti-Xa"))),
          make_dropdown(language, ns, "subset", list(
            list(key = "all_patients", text = "Tous les patients"),
            list(key = "included_patients", text = "Patients inclus")),
            value = "all_patients"
          )
      ) -> result
    }
    
    ##########################################
    # Fluent / Settings                      #
    ##########################################
    
    if (grepl("^settings", page)){
      div(class = "sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = list(
              list(name = translate(language, "general_settings"), key = "general", url = shiny.router::route_link("settings/general")),
              list(name = translate(language, "app_db"), key = "app_db", url = shiny.router::route_link("settings/app_db")),
              list(name = translate(language, "users"), key = "users", url = shiny.router::route_link("settings/users")),
              list(name = translate(language, "data_management"), key = "data", links = list(
                list(name = translate(language, "data_sources"), key = "data_sources", url = shiny.router::route_link("settings/data_sources")),
                list(name = translate(language, "datamarts"), key = "datamarts", url = shiny.router::route_link("settings/datamarts")),
                list(name = translate(language, "studies"), key = "studies", url = shiny.router::route_link("settings/studies")),
                list(name = translate(language, "subsets"), key = "subsets", url = shiny.router::route_link("settings/subsets")),
                list(name = translate(language, "thesaurus"), key = "thesaurus", url = shiny.router::route_link("settings/thesaurus"))
                ),
                initialSelectedKey = "data_source",
                selectedKey = substr(page, nchar("settings") + 2, 100),
                isExpanded = TRUE),
              list(name = translate(language, "modules_plugins"), key = "modules", links = list(
                list(name = translate(language, "plugins"), key = "plugins", url = shiny.router::route_link("settings/plugins")),
                list(name = translate(language, "modules_patient_lvl"), key = "modules_patient_lvl", url = shiny.router::route_link("settings/modules_patient_lvl")),
                list(name = translate(language, "modules_aggregated"), key = "modules_aggregated", url = shiny.router::route_link("settings/modules_aggregated"))
              ),
              initialSelectedKey = "data_source",
              selectedKey = substr(page, nchar("settings") + 2, 100),
              isExpanded = TRUE),
              list(name = translate(language, "log"), key = "log", url = shiny.router::route_link("settings/log"))
              # list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport'),
            ))
          ),
          initialSelectedKey = "general",
          selectedKey = substr(page, nchar("settings") + 2, 100),
          styles = list(
            root = list(
              height = "100%",
              boxSizing = "border-box",
              overflowY = "auto"
            )
          )
        )
      ) -> result
    }
    
    ##########################################
    # Fluent / Help                          #
    ##########################################
    
    if (grepl("^help", page)){
      div(class = "sidenav",
          shiny.fluent::Nav(
            groups = list(
              list(links = list(
                list(name = translate(language, "get_started"), key = "get_started", url = shiny.router::route_link("help/get_started")),
                list(name = translate(language, "data_management"), key = "data_management", url = shiny.router::route_link("help/data_management"))
              ))
            ),
            initialSelectedKey = "get_started",
            selectedKey = substr(page, nchar("help") + 2, 100),
            styles = list(
              root = list(
                height = "100%",
                boxSizing = "border-box",
                overflowY = "auto"
              )
            )
          )
      ) -> result
    }
  }
  
  ##########################################
  # Fluid                                  #
  ##########################################
  
  if (page_style == "fluid"){
    
    ##########################################
    # Fluid / Patient-level data             #
    ##########################################
    
    if (page == "patient_level_data"){
      shiny::sidebarPanel(
        # class = "sidenav",
        width = 2,
        shiny::selectInput(ns("datamart"), htmltools::strong(translate(language, "datamart")),
                           choices = c("Données héparine" = "ufh", "Ventilation" = "wmv")),
        shiny::selectInput(ns("study"), htmltools::strong(translate(language, "study")),
                           choices = c("Etude 1 - premier anti-Xa")),
        shiny::selectInput(ns("subset"), htmltools::strong(translate(language, "subset")),
                           choices = c("Patients inclus"))
      ) -> result
    }
    
    ##########################################
    # Fluid / Aggregated data                #
    ##########################################
    
    if (page == "aggregated_data"){
      shiny::sidebarPanel(
        width = 2,
        shiny::selectInput(ns("datamart"), htmltools::strong(translate(language, "datamart")),
                           choices = c("Données héparine" = "ufh", "Ventilation" = "wmv")),
        shiny::selectInput(ns("study"), htmltools::strong(translate(language, "study")),
                           choices = c("Etude 1 - premier anti-Xa")),
        shiny::selectInput(ns("subset"), htmltools::strong(translate(language, "subset")),
                           choices = c("Patients inclus"))
      ) -> result
    }
  }
  
  result
}
    
#' page_sidenav Server Functions
#'
#' @noRd 
mod_page_sidenav_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_page_sidenav_ui("page_sidenav_ui_1")
    
## To be copied in the server
# mod_page_sidenav_server("page_sidenav_ui_1")
