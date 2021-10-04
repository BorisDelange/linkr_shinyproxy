#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_ui <- function(id, language, page_style, page){
  ns <- NS(id)
  result <- ""
  
  if (page_style == "fluent"){
    div(class = "main",
      # uiOutput(ns("breadcrumb")),
      uiOutput(ns("main")),
      # shiny.fluent::Pivot(
      #   shiny.fluent::PivotItem(headerText = "test1", "description"),
      #   shiny.fluent::PivotItem(headerText = "test2",
      #     shiny.fluent::Pivot(
      #       shiny.fluent::PivotItem(headerText = "test2", "descr")
      #     )
      #   )
        # shiny.fluent::PivotItem()
      # ),
      textOutput(ns("test"))
    ) -> result
  }
  result
}
    
#' patient_and_aggregated_data Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    r$current_pivot <- "none"
    
    # Once a study is chosen, load its tabs
    observeEvent(r$chosen_study, {
      study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))
      
      patient_lvl_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_modules WHERE module_family_id = ", 
                                                          study_infos$patient_lvl_module_family_id, " AND deleted IS FALSE"))
      
      # Modules without parent are set to level 1
      patient_lvl_modules <-
        patient_lvl_modules %>% dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))
      
      while(nrow(patient_lvl_modules %>% dplyr::filter(is.na(level))) > 0){
        patient_lvl_modules <-
          patient_lvl_modules %>%
            dplyr::left_join(patient_lvl_modules %>%
              dplyr::filter(!is.na(level)) %>%
              dplyr::transmute(parent_module_id = id, parent_level = level), by = "parent_module_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
      }
      
      # r$num_levels <- max(patient_lvl_modules$level)
      
      # output$breadcrumb <- renderUI({
      #   items <- list(
      #     list(key = "main", text = translate(language, "patient_level_data"), href = "#!/patient_level_data", isCurrentItem = TRUE)#,
      #     # list(key = "lvl1", text = "Néphro & uro", isCurrentItem = TRUE)
      #   )
      #   shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3)
      # })
      
      output$main <- renderUI({
        max_level <- max(patient_lvl_modules$level)
        result <- list()
        for (i in 1:nrow(patient_lvl_modules)){
          result <- rlist::list.append(result, 
            list(id = patient_lvl_modules[[i, "id"]], level = patient_lvl_modules[[i, "level"]], parent_module_id = patient_lvl_modules[[i, "parent_module_id"]],
                 name = patient_lvl_modules[[i, "name"]], description = patient_lvl_modules[[i, "description"]], code = tagList()))
        }
        for(current_level in max_level:1){
          for (i in 1:length(result)){
            if (result[[i]]$level == current_level){
              children <- rlist::list.filter(result, result[[i]]$id %in% parent_module_id)
              children_code <- tagList()
              for (j in children){
                children_code <- tagList(children_code, j$code)
              }
              if (length(children_code) == 0) result[[i]]$code <- shiny.fluent::PivotItem(headerText = result[[i]]$name, result[[i]]$description)
              else result[[i]]$code <- shiny.fluent::PivotItem(headerText = result[[i]]$name, shiny.fluent::Pivot(children_code))
            }
          }
        }
        
        result_concat <- tagList()
        for (i in 1:length(result)){
          if (result[[i]]$level == 1) result_concat <- tagList(result_concat, result[[i]]$code)
        }

        make_card("", shiny.fluent::Pivot(result_concat))
      })
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_patient_and_aggregated_data_ui("patient_and_aggregated_data_ui_1")
    
## To be copied in the server
# mod_patient_and_aggregated_data_server("patient_and_aggregated_data_ui_1")
