#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_and_aggregated_data_ui <- function(id, language, page){
  ns <- NS(id)
  result <- ""
  
  div(class = "main",
    uiOutput(ns("message_bar1")), uiOutput(ns("message_bar2")), uiOutput(ns("message_bar3")), 
    uiOutput(ns("message_bar4")), uiOutput(ns("message_bar5")),
    uiOutput(ns("main"))
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 
mod_patient_and_aggregated_data_server <- function(id, r, language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Once a datamart is chosen, load its data
    
    observeEvent(r$chosen_datamart, {
      run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language) 
    })

    # Once a study is chosen, load its tabs
    observeEvent(r$chosen_study, {

      req(!is.na(r$chosen_study))

      study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))

      r$patient_lvl_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_modules WHERE module_family_id = ",
                                                            study_infos$patient_lvl_module_family_id, " AND deleted IS FALSE"))

      # Modules without parent are set to level 1
      r$patient_lvl_modules <-
        r$patient_lvl_modules %>% dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))

      while(nrow(r$patient_lvl_modules %>% dplyr::filter(is.na(level))) > 0){
        r$patient_lvl_modules <-
          r$patient_lvl_modules %>%
          dplyr::left_join(r$patient_lvl_modules %>%
                             dplyr::filter(!is.na(level)) %>%
                             dplyr::transmute(parent_module_id = id, parent_level = level), by = "parent_module_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
      }
    })

    output$main <- renderUI({

      req(nrow(r$patient_lvl_modules > 0) & "level" %in% names(r$patient_lvl_modules) & !is.na(r$chosen_study))

      # At initialization, input$current_tab is null
      # if (is.null(input$current_tab)){

      # First module shown
      first_module_shown <- r$patient_lvl_modules %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
      if (max(r$patient_lvl_modules$level) >= 2){
        sapply(2:max(r$patient_lvl_modules$level), function(current_level){
          children <- r$patient_lvl_modules %>% dplyr::filter(level == current_level, parent_module_id == first_module_shown$id) %>% dplyr::slice(1)
          if (nrow(children) > 0) first_module_shown <<- children
        })
      }

      # If we are at level one, show all levels one
      if (first_module_shown$level == 1){
        shown_modules <- r$patient_lvl_modules %>% dplyr::filter(level == 1)
      }

      # Else, show only current & those who has same level & same parent
      if (first_module_shown$level > 1){
        shown_modules_temp <- r$patient_lvl_modules %>% dplyr::filter(level == first_module_shown$level & parent_module_id == first_module_shown$parent_module_id)
        if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
        if (nrow(shown_modules_temp) == 0) shown_modules <- first_module_shown
      }
      # }

      if (!is.null(input$current_tab)){
        # If value = 0, go back to first level
        # if (input$current_tab == 0)

        shown_modules_temp <- r$patient_lvl_modules %>% dplyr::filter(parent_module_id == input$current_tab)
        if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
        if (nrow(shown_modules_temp) == 0){
          current_module <- r$patient_lvl_modules %>% dplyr::filter(id == input$current_tab)
          shown_modules <- r$patient_lvl_modules %>% dplyr::filter(parent_module_id == current_module$parent_module_id & level == current_module$level)
          # If not any "brother", we are at level one
          if (nrow(shown_modules) == 0){
            shown_modules <- r$patient_lvl_modules %>% dplyr::filter(level == 1)
          }
        }
      }

      # Currently selected tab
      r$selected_key <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
      if (!is.null(input$current_tab)){
        if (input$current_tab %in% shown_modules$id) r$selected_key <- input$current_tab
      }

      nb_levels <- max(shown_modules$level)

      # First level
      is_current_item <- FALSE
      if (nb_levels == 1) is_current_item <- TRUE
      items <- list(
        list(key = "main", text = translate(language, "patient_level_data"), href = "#!/patient_level_data", isCurrentItem = is_current_item,
             onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', ", first_module_shown$id, ")"))))

      # Other levels
      if (nb_levels >= 2){
        # Remove last level
        modules_tree <- r$patient_lvl_modules %>% dplyr::filter(level < nb_levels)
        modules_tree2 <- r$patient_lvl_modules %>% dplyr::filter(level < nb_levels)
        current_parent <- NA_integer_
        sapply(nb_levels:1, function(current_level){
          if (!is.na(current_parent)){
            modules_tree <<- modules_tree %>% dplyr::filter(level != current_level | id == current_parent)
            current_parent <<- r$patient_lvl_modules %>% dplyr::filter(id == current_parent) %>% dplyr::pull(parent_module_id)
          }
          if (is.na(current_parent)) current_parent <<- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(parent_module_id)
        })
        modules_tree <- modules_tree %>% dplyr::arrange(level)
        sapply(1:nrow(modules_tree), function(i){
          is_current_item <- FALSE
          if (modules_tree[[i, "level"]] == nb_levels) is_current_item <- TRUE
          items <<- rlist::list.append(items, list(
            key = modules_tree[[i, "name"]], text = modules_tree[[i, "name"]], href = "#!/patient_level_data", isCurrentItem = is_current_item,
            onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', ", modules_tree[[i, "id"]], ")"))
          ))
        })
      }

      shown_tabs <- tagList()
      sapply(1:nrow(shown_modules), function(i){
        # Get module UI code

        module_elements <- r$patient_lvl_module_elements %>% dplyr::filter(module_id == shown_modules[[i, "id"]])
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
              code_ui_card <- code_ui_card %>%
                stringr::str_replace_all("%group_id%", as.character(loop_group_id))
              code_ui <<- tagList(code_ui, make_card("", eval(parse(text = code_ui_card))))
            },
            error = function(e){
              # Libraries needed
              libraries_needed <- paste0(translate(language, "libraries_needed_plugin"), " : ", 
                strsplit(code_ui_card, " ") %>% unlist() %>% grep("::", ., value = TRUE) %>% sub("::.*", "", .) %>% sub("\n", "", .) %>% toString(), ".")
              plugin_name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)
              output$message_bar1 <- show_message_bar(1, paste0(translate(language, "error_run_plugin_ui_code"), " (group_id = ", loop_group_id, ", plugin_id = ", plugin_id, ", plugin_name = ", plugin_name, "). ", libraries_needed), "severeWarning", language)
            })
          })
        }

        shown_tabs <<- tagList(shown_tabs, shiny.fluent::PivotItem(id = shown_modules[[i, "id"]], itemKey = shown_modules[[i, "id"]], headerText = shown_modules[[i, "name"]], code_ui))
      })

      tagList(
        shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3),
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          selectedKey = r$selected_key, shown_tabs
        )
      )
    })



    # Once a patient is chosen, render its tabs
    observeEvent(r$chosen_patient, {
      req(!is.na(r$chosen_patient))

      module_elements <- r$patient_lvl_module_elements %>% dplyr::filter(module_id == r$selected_key)

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
            code_server_card <- code_server_card %>%
              stringr::str_replace_all("%group_id%", as.character(loop_group_id)) %>%
              stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))
            eval(parse(text = code_server_card))
          },
          error = function(e){
            plugin_name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)
            output$message_bar2 <- show_message_bar(1, paste0(translate(language, "error_run_plugin_server_code"), " (group_id = ", loop_group_id, ", plugin_id = ", plugin_id, ", plugin_name = ", plugin_name, ")"), "severeWarning", language) 
          }
          )
        })
      }
    })

  })
}

## To be copied in the UI
# mod_patient_and_aggregated_data_ui("patient_and_aggregated_data_ui_1")

## To be copied in the server
# mod_patient_and_aggregated_data_server("patient_and_aggregated_data_ui_1")