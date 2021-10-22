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
    render_settings_default_elements(ns = ns),
    uiOutput(ns("main"))
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 

mod_patient_and_aggregated_data_server <- function(id, r, language){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Once a datamart is chosen, load its data
    
    observeEvent(r$chosen_datamart, {
      # Initiate r$selected_key
      r$selected_key <- NA_integer_
      
      # Try to load datamart 
      tryCatch(run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language),
        error = function(e) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language), 
        warning = function(w) show_message_bar(output, 1, "fail_load_datamart", "severeWarning", language))
    })
    
    ##########################################
    # Patient-lvl data                       #
    ##########################################

    if (id == "patient_lvl_data"){
      
      # Once a study is chosen, load its tabs
      observeEvent(r$chosen_study, {
  
        req(!is.na(r$chosen_study))
  
        # Load study informations
        # For one study, you choose ONE patient_lvl data module family
        study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))
  
        # Load modules belonging to this module family
        r$patient_lvl_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM patient_lvl_modules WHERE module_family_id = ",
          study_infos$patient_lvl_module_family_id, " AND deleted IS FALSE"))
  
        # Modules without parent are set to level 1
        r$patient_lvl_modules <-
          r$patient_lvl_modules %>% dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))
  
        # Creating levels for distinct modules
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
  
      # Render modules in UI
      output$main <- renderUI({
  
        req(r$chosen_study)
        
        # If no module to show, notificate user
        if (nrow(r$patient_lvl_modules) == 0 | "level" %not_in% names(r$patient_lvl_modules)){
          return(div(shiny.fluent::MessageBar(translate(language, "no_modules_to_show"), messageBarType = 3), style = "margin-top:10px;"))
        }
        
        req(nrow(r$patient_lvl_modules > 0) & "level" %in% names(r$patient_lvl_modules) & !is.na(r$chosen_study))
  
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
  
          module_elements <- r$patient_lvl_modules_elements %>% dplyr::filter(module_id == shown_modules[[i, "id"]])
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

        # Update module when selected key changed
        # One module is composed by multiple groups
        # One group corresponds to one plugin and one or multiple thesaurus items
        
        observeEvent(r$selected_key, {
          req(!is.na(r$selected_key))
    
          module_elements <- r$patient_lvl_module_elements %>% dplyr::filter(module_id == r$selected_key)
          
          # If no thesaurus elements to show in this module, notificate user
          if (nrow(module_elements) == 0) show_message_bar(output = output, id = 2, message = "no_thesaurus_item_to_show", type = "severeWarning")
          
          if (nrow(module_elements) > 0){
  
            # Get module element group_id
            distinct_groups <- unique(module_elements$group_id)

            # Loop over distinct cards
            sapply(distinct_groups, function(group_id){
              
              # Get thesaurus items with thesaurus own item_id
              thesaurus_items <- module_elements %>% dplyr::filter(group_id == !!group_id) %>%
                dplyr::select(thesaurus_item_id, thesaurus_item_display_name, thesaurus_item_unit)
              
              # Need to have thesaurus_name & thesaurus_item_id (of the thesaurus, not of r$thesaurus table) to merge with r data variables
              
              # Get data from patient, considering thesaurus items selected
              # Unit can be on r data vars or in thesaurus var : prioritize thesaurus unit name
              labs_vitals <- r$labs_vitals %>%
                dplyr::inner_join(
                  thesaurus_items %>% dplyr::select(thesaurus_name, item_id = thesaurus_item_id, name = thesaurus_item_display_name, thesaurus_item_unit),
                  by = c("thesaurus_name", "item_id"))
              if (nrow(labs_vitals) > 0) labs_vitals <- labs_vitals %>% dplyr::mutate(unit = dplyr::case_when(unit != "" ~ unit, T ~ thesaurus_item_unit))
              
              text <- r$text %>%
                dplyr::inner_join(
                  thesaurus_items %>% dplyr::select(thesaurus_name, item_id = thesaurus_item_id, name = thesaurus_item_display_name, thesaurus_item_unit),
                  by = c("thesaurus_name", "item_id"))
              if (nrow(text) > 0) text <- text %>% dplyr::mutate(unit = dplyr::case_when(unit != "" ~ unit, T ~ thesaurus_item_unit))
              
              orders <- r$orders %>%
                dplyr::inner_join(
                  thesaurus_items %>% dplyr::select(thesaurus_name, item_id = thesaurus_item_id, name = thesaurus_item_display_name, thesaurus_item_unit),
                  by = c("thesaurus_name", "item_id"))
              if (nrow(orders) > 0) orders <- orders %>% dplyr::mutate(unit = dplyr::case_when(unit != "" ~ unit, T ~ thesaurus_item_unit))
              
              # Get data for current stay only
              stay <- r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::select(admission_datetime, discharge_datetime)
              
              if (nrow(labs_vitals) > 0) labs_vitals_stay <- labs_vitals %>% dplyr::filter(datetime_start >= stay$admission_datetime & datetime_start <= stay$discharge_datetime)
              if (nrow(text) > 0) text_stay <- text %>% dplyr::filter(datetime_start >= stay$admission_datetime & datetime_start <= stay$discharge_datetime)
              if (nrow(orders) > 0) orders_stay <- orders %>% dplyr::filter(datetime_start >= stay$admission_datetime & datetime_start <= stay$discharge_datetime)
              
              output$test1 <- renderTable(labs_vitals)
              output$test2 <- renderTable(text)
              output$test3 <- renderTable(orders)
              
              # Get plugin code
              plugin_id <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              code_server_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_server") %>% dplyr::pull(code)

              # Try to run plugin server code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                # code_server_card <- code_server_card %>%
                #   stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                #   stringr::str_replace_all("%patient_id%", as.character(isolate(r$chosen_patient)))
                eval(parse(text = code_server_card))
                },
                error = function(e){
                  # plugin_name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)
                  # output$message_bar2 <- renderUI(shiny.fluent::MessageBar(
                  #   paste0(translate(language, "error_run_plugin_server_code"), " (group_id = ", group_id, ", plugin_id = ", plugin_id, ", plugin_name = ", plugin_name, ")"), messageBarType = 3))
                  # shinyjs::show("message_bar2")
                  # shinyjs::delay(time, shinyjs::hide("message_bar2"))
              })
              # }
            })
          }
        })
      })
    }
    
    ##########################################
    # Aggregated data                        #
    ##########################################
    
    if (id == "aggregated_data"){
      
    }

  })
}