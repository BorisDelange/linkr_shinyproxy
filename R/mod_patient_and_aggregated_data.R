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
      shiny::uiOutput(ns("warnings1")), shiny::uiOutput(ns("warnings2")), shiny::uiOutput(ns("warnings3")), shiny::uiOutput(ns("warnings4")),
      uiOutput(ns("main"))
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
      
      max_level <- max(patient_lvl_modules$level)
      r$result <- list()
      for (i in 1:nrow(patient_lvl_modules)){
        r$result <- rlist::list.append(r$result, 
          list(id = patient_lvl_modules[[i, "id"]], level = patient_lvl_modules[[i, "level"]], parent_module_id = patient_lvl_modules[[i, "parent_module_id"]],
            name = patient_lvl_modules[[i, "name"]], description = patient_lvl_modules[[i, "description"]], code_ui = tagList(), code_server = ""))
      }
      for(current_level in max_level:1){
        for (i in 1:length(r$result)){
          if (r$result[[i]]$level == current_level){
            children <- rlist::list.filter(r$result, r$result[[i]]$id %in% parent_module_id)
            children_code_ui <- tagList()
            for (j in children){
              children_code_ui <- tagList(children_code_ui, j$code_ui)
            }
            if (length(children_code_ui) == 0){
              # Load module elements
              module_elements <- r$patient_lvl_module_elements %>% dplyr::filter(module_id == r$result[[i]]$id)
              code_ui <- tagList()
              
              if (nrow(module_elements) != 0){
                
                # Get module element group_id
                distinct_groups <- unique(module_elements$group_id)
                
                # Loop over distinct cards
                sapply(distinct_groups, function(loop_group_id){
                  plugin_id <- module_elements %>% dplyr::filter(group_id == loop_group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
                  if (length(plugin_id) != 0) code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
                  
                  # Try to run plugin UI code
                  # ID of UI element is in the following format : "group_[ID]"
                  tryCatch({
                    code_ui_card <- 
                      code_ui_card %>% 
                      stringr::str_replace_all("%group_id%", as.character(loop_group_id))
                    code_ui <<- tagList(code_ui, make_card("", eval(parse(text = code_ui_card))))
                    },
                    error = function(e){
                      output$warnings1 <<- renderUI(div(shiny.fluent::MessageBar(
                        paste0(translate(language, "error_run_plugin_ui_code"), " (group_id = ", loop_group_id, ")"), messageBarType = 3), style = "margin-top:10px;"))
                      shinyjs::show("warnings1")
                      shinyjs::delay(10000, shinyjs::hide("warnings1"))
                  })
                })
              }
              
              # Merge UIs
              r$result[[i]]$code_ui <- shiny.fluent::PivotItem(headerText = r$result[[i]]$name, code_ui)
              
              # Server code
              r$result[[i]]$code_server <- "not_empty"
            }
            else r$result[[i]]$code_ui <- shiny.fluent::PivotItem(headerText = r$result[[i]]$name, shiny.fluent::Pivot(children_code_ui))
          }
        }
      }
      
      result_concat <- tagList()
      for (i in 1:length(r$result)){
        if (r$result[[i]]$level == 1) result_concat <- tagList(result_concat, r$result[[i]]$code_ui)
      }
      
      # output$breadcrumb <- renderUI({
      #   items <- list(
      #     list(key = "main", text = translate(language, "patient_level_data"), href = "#!/patient_level_data", isCurrentItem = TRUE)#,
      #     # list(key = "lvl1", text = "Néphro & uro", isCurrentItem = TRUE)
      #   )
      #   shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3)
      # })
      
      # observeEvent(r$chosen_study, {
      output$main <- renderUI({
       shiny.fluent::Pivot(result_concat)
      })
      # })
    })
    
    # Once a patient is chosen, render its tabs
    observeEvent(r$chosen_patient, {
      req(!is.na(r$chosen_patient))
      
      sapply(1:length(r$result), function(i){
        if (r$result[[i]]$code_server != ""){
          module_elements <- r$patient_lvl_module_elements %>% dplyr::filter(module_id == r$result[[i]]$id)

          if (nrow(module_elements) != 0){
            
            # Get module element group_id
              distinct_groups <- unique(module_elements$group_id)

            # Loop over distinct cards
            sapply(distinct_groups, function(loop_group_id){
              # Get thesaurus items with thesaurus own item_id
              thesaurus_items <- module_elements %>% dplyr::filter(group_id == loop_group_id) %>% 
                dplyr::select(thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit) %>%
                dplyr::inner_join(r$thesaurus_items %>% dplyr::select(thesaurus_item_id = id, item_id), by = "thesaurus_item_id") %>%
                dplyr::mutate_at("item_id", as.integer)
              
              plugin_id <- module_elements %>% dplyr::filter(group_id == loop_group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              if (length(plugin_id) != 0) code_server_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_server") %>% dplyr::pull(code)
  
              # Try to run plugin server code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                code_server_card <-
                  code_server_card %>%
                  stringr::str_replace_all("%group_id%", as.character(loop_group_id)) %>%
                  stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))
                eval(parse(text = code_server_card))
              },
              error = function(e){
                output$warnings2 <<- renderUI(div(shiny.fluent::MessageBar(
                  paste0(translate(language, "error_run_plugin_server_code"), " (group_id = ", loop_group_id, ")"), messageBarType = 3), style = "margin-top:10px;"))
                shinyjs::show("warnings2")
                shinyjs::delay(10000, shinyjs::hide("warnings2"))
              })
            })
          }
        }
      })
    })
    
  })
}
    
## To be copied in the UI
# mod_patient_and_aggregated_data_ui("patient_and_aggregated_data_ui_1")
    
## To be copied in the server
# mod_patient_and_aggregated_data_server("patient_and_aggregated_data_ui_1")
