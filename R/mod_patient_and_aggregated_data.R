#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_patient_and_aggregated_data_ui <- function(id = character(), language = "EN", words = tibble::tibble()){
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

mod_patient_and_aggregated_data_server <- function(id = character(), r, language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prefix depending on page id
    if (id == "patient_level_data") prefix <- "patient_lvl"
    if (id == "aggregated_data") prefix <- "aggregated"
    
    # Once a datamart is chosen, load its data
    
    observeEvent(r$chosen_datamart, {
      # Initiate selected_key
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      # Reset r variables (prevent bug later if datamart code doesn't work)
      r$patients <- tibble::tibble()
      r$stays <- tibble::tibble()
      r$labs_vitals <- tibble::tibble()
      r$text <- tibble::tibble()
      r$orders <- tibble::tibble()
      
      # Try to load datamart 
      tryCatch(run_datamart_code(output, r, datamart_id = r$chosen_datamart, language = language),
        error = function(e) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language), 
        warning = function(w) report_bug(r = r, output = output, error_message = "fail_load_datamart", 
          error_name = paste0(id, " - run server code"), category = "Warning", error_report = w, language = language))
      
      # Reload main output
      output$main <- renderUI("")
    })
    
    ##########################################
    # Initiate observers                     #
    ##########################################
    
    observeEvent(r$chosen_study, {
      
      req(!is.na(r$chosen_study))
      
      # Load tabs
      r[[paste0("load_", prefix, "_tabs")]] <- r$chosen_study
      
      # Load code for toggles
      r[[paste0("reload_", prefix, "_code")]] <- paste0(r$chosen_study, "load_toggles")
    })
  
    ##########################################
    # Load & display tabs                    #
    ##########################################
    
    # Once a study is chosen, load its tabs
    observeEvent(r[[paste0("load_", prefix, "_tabs")]], {

      # Load study informations
      # For one study, you choose ONE patient_lvl or aggregated data module family
      study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", r$chosen_study))

      # Load modules belonging to this module family
      display_modules <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, "_modules WHERE module_family_id = ",
        study_infos[[paste0(prefix, "_module_family_id")]], " AND deleted IS FALSE"))

      # Modules without parent are set to level 1
      display_modules <- display_modules %>% 
        dplyr::mutate(level = dplyr::case_when(is.na(parent_module_id) ~ 1L, TRUE ~ NA_integer_))

      # Creating levels for distinct modules
      while(nrow(display_modules %>% dplyr::filter(is.na(level))) > 0){
        display_modules <-
          display_modules %>%
          dplyr::left_join(display_modules %>%
            dplyr::filter(!is.na(level)) %>%
            dplyr::transmute(parent_module_id = id, parent_level = level), by = "parent_module_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
      }
      
      # Now we have a level for each module, order them by display order
      display_modules <- display_modules %>% dplyr::arrange(level, display_order)

      # Render modules in UI
      output$main <- renderUI({
        
        # Check if users has access only to aggregated data
        r$options %>% dplyr::filter(category == "datamart" & link_id == r$chosen_datamart & name == "show_only_aggregated_data") %>%
          dplyr::pull(value_num) -> show_only_aggregated_data
        
        if (prefix == "patient_lvl" & show_only_aggregated_data == 1) show_message_bar(output, 1, "only_aggregated_data_authorized", "severeWarning", language)
        req((prefix == "patient_lvl" & show_only_aggregated_data != 1) | prefix == "aggregated")
        
        # If no module to show, notificate user
        if (nrow(display_modules) == 0 | "level" %not_in% names(display_modules)){
          return(div(shiny.fluent::MessageBar(translate(language, "no_modules_to_show", words), messageBarType = 3), style = "margin-top:10px;"))
        }

        req(nrow(display_modules > 0) & "level" %in% names(display_modules) & !is.na(r$chosen_study))

        # First module shown
        first_module_shown <- display_modules %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
        if (max(display_modules$level) >= 2){
          sapply(2:max(display_modules$level), function(current_level){
            children <- display_modules %>% dplyr::filter(level == current_level, parent_module_id == first_module_shown$id) %>% dplyr::slice(1)
            if (nrow(children) > 0) first_module_shown <<- children
          })
        }

        # If we are at level one, show all levels one
        if (first_module_shown$level == 1){
          shown_modules <- display_modules %>% dplyr::filter(level == 1)
        }

        # Else, show only current & those who has same level & same parent
        if (first_module_shown$level > 1){
          shown_modules_temp <- display_modules %>% dplyr::filter(level == first_module_shown$level & parent_module_id == first_module_shown$parent_module_id)
          if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp
          if (nrow(shown_modules_temp) == 0) shown_modules <- first_module_shown
        }

        if (length(input$current_tab) > 0){

          # If value = 0, go back to first level
          if (input$current_tab == 0) shown_modules <- display_modules %>% dplyr::filter(level == 1)
          else {

            shown_modules_temp <- display_modules %>% dplyr::filter(parent_module_id == input$current_tab)

            # If current tab has children
            if (nrow(shown_modules_temp) > 0) shown_modules <- shown_modules_temp

            # If current tab has no children
            if (nrow(shown_modules_temp) == 0){
              current_module <- display_modules %>% dplyr::filter(id == input$current_tab)
              if (nrow(current_module) > 0) shown_modules <- display_modules %>% dplyr::filter(parent_module_id == current_module$parent_module_id & level == current_module$level)
              else show_modules <- tibble::tibble()
              
              # If not any "brother", we are at level one
              if (nrow(shown_modules) == 0){
                shown_modules <- display_modules %>% dplyr::filter(level == 1)
              }
            }
          }
        }

        # Currently selected tab
        r[[paste0(prefix, "_selected_key")]] <- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(id)
        if (length(input$current_tab) > 0){
          if (input$current_tab %in% shown_modules$id) r[[paste0(prefix, "_selected_key")]] <- input$current_tab
        }

        nb_levels <- max(shown_modules$level)

        # First level
        is_current_item <- FALSE
        if (nb_levels == 1) is_current_item <- TRUE
        items <- list(
          list(key = "main", text = translate(language, id, words), href = paste0("#!/", id), isCurrentItem = is_current_item,
               onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', 0)"))))

        # Other levels
        if (nb_levels >= 2){

          # Remove last level
          modules_tree <- display_modules %>% dplyr::filter(level < nb_levels)

          current_parent <- NA_integer_
          sapply(nb_levels:1, function(current_level){
            if (!is.na(current_parent)){
              modules_tree <<- modules_tree %>% dplyr::filter(level != current_level | id == current_parent)
              current_parent <<- display_modules %>% dplyr::filter(id == current_parent) %>% dplyr::pull(parent_module_id)
            }
            if (is.na(current_parent)) current_parent <<- shown_modules %>% dplyr::slice(1) %>% dplyr::pull(parent_module_id)
          })
          modules_tree <- modules_tree %>% dplyr::arrange(level)
          sapply(1:nrow(modules_tree), function(i){
            is_current_item <- FALSE
            if (modules_tree[[i, "level"]] == nb_levels) is_current_item <- TRUE
            items <<- rlist::list.append(items, list(
              key = modules_tree[[i, "name"]], text = modules_tree[[i, "name"]], href = paste0("#!/", id), isCurrentItem = is_current_item,
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', ", modules_tree[[i, "id"]], ")"))
            ))
          })
        }

        shown_tabs <- tagList()
        sapply(1:nrow(shown_modules), function(i){
          
          # Get module UI code
          
          has_children <- r[[paste0(prefix, "_modules")]] %>% dplyr::filter(parent_module_id == shown_modules[[i, "id"]]) %>% nrow()

          module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == shown_modules[[i, "id"]]) %>% dplyr::arrange(display_order)
          code_ui <- tagList("")
          cards <- list()
          activated_cards <- ""

          if (nrow(module_elements) != 0){

            # Get module element group_id
            distinct_groups <- unique(module_elements$group_id)

            # Loop over distinct cards
            sapply(distinct_groups, function(group_id){
              plugin_id <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              if (length(plugin_id) != 0){
                code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
              }

              # Get name of module element
              # Remove special characters (-, / ...)
              module_element_name <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
              module_element_name_escaping <- module_element_name %>% stringr::str_replace_all(c("-" = "_", "/" = "_", "\\(" = "_", "\\)" = "_"))

              # Append a toggle to our cards list
              cards <<- rlist::list.append(cards, list(key = paste0(module_element_name_escaping, group_id), label = module_element_name))
              activated_cards <<- c(activated_cards, paste0(module_element_name_escaping, group_id))

              # Try to run plugin UI code
              # ID of UI element is in the following format : "group_[ID]"
              tryCatch({
                code_ui_card <- code_ui_card %>%
                  stringr::str_replace_all("%module_id%", as.character(r[[paste0(prefix, "_selected_key")]])) %>%
                  stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
                  stringr::str_replace_all("\r", "\n")

                if (length(r$chosen_study) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%study_id%", as.character(r$chosen_study))
                if (length(r$chosen_patient) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))

                code_ui <<- tagList(code_ui, div(id = ns(paste0(module_element_name_escaping, group_id)), make_card("", eval(parse(text = code_ui_card)))))
              },
              error = function(e){
                # Libraries needed
                libraries_needed <- paste0(translate(language, "libraries_needed_plugin", words), " : ",
                  strsplit(code_ui_card, " ") %>% unlist() %>% grep("::", ., value = TRUE) %>% sub("::.*", "", .) %>% sub("\n", "", .) %>% toString(), ".")
                plugin_name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)

                error_message <- paste0(translate(language, "error_run_plugin_ui_code", words), " (group_id = ", group_id, ", plugin_id = ", plugin_id, ", plugin_name = ", plugin_name, "). ", libraries_needed)

                report_bug(r = r, output = output, error_message = error_message,
                  error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language)
              })
            })
          }
        
          # If no toggles to show, display a message
          if (length(cards) == 0 & has_children == 0) code_ui <- tagList(code_ui, make_card("", 
            div(shiny.fluent::MessageBar(translate(language, "empty_page", r$words), messageBarType = 3), style = "margin-top:10px;")))
          
          if (has_children > 0) code_ui <- tagList(code_ui, make_card("", 
            div(shiny.fluent::MessageBar(translate(language, "module_contains_submodules", r$words), messageBarType = 0), style = "margin-top:10px;")))
          
          if (length(cards) > 0 & has_children == 0){
            code_ui <- tagList(
              render_settings_toggle_card(language = language, ns = ns, cards = cards, activated = activated_cards, translate = FALSE),
              code_ui)
          }

          shown_tabs <<- tagList(shown_tabs, shiny.fluent::PivotItem(id = shown_modules[[i, "id"]], itemKey = shown_modules[[i, "id"]], headerText = shown_modules[[i, "name"]], code_ui))
        })

        tagList(
          shiny.fluent::Breadcrumb(items = items, maxDisplayedItems = 3),
          shiny.fluent::Pivot(
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
            selectedKey = r[[paste0(prefix, "_selected_key")]],
            shown_tabs
          ),
          br()
        )
      })
    })
    
    ##########################################
    # Run server code                        #
    ##########################################

    # Once a patient is chosen, render its tabs (without data for current stay)
    # Once a stay is chosen, render patient's tabs

    if (prefix == "patient_lvl"){
      observeEvent(r$chosen_patient, r$reload_patient_lvl_code <- as.character(paste0("patient_", r$chosen_patient)))
      observeEvent(r$chosen_stay, r$reload_patient_lvl_code <- as.character(paste0("stay_", r$chosen_stay)))
    }
    if (prefix == "aggregated"){
      observeEvent(r$chosen_subset, r$reload_aggregated_code <- r$chosen_subset)
    }

    observeEvent(r[[paste0(prefix, "_selected_key")]], r[[paste0("reload_", prefix, "_code")]] <- paste0("selected_key", r[[paste0(prefix, "_selected_key")]]))

    observeEvent(r[[paste0("reload_", prefix, "_code")]], {

      req(!is.na(r$chosen_study))
      
      # Update module when selected key changed
      # One module is composed by multiple groups
      # One group corresponds to one plugin and one or multiple thesaurus items

      # Get modules elements, arrange them by display_order

      module_elements <- r[[paste0(prefix, "_modules_elements")]] %>% dplyr::filter(module_id == r[[paste0(prefix, "_selected_key")]]) %>% dplyr::arrange(display_order)

      # If no thesaurus elements to show in this module, notificate user
      if (nrow(module_elements) == 0 & !grepl("selected_key|load_toggles", r[[paste0("reload_", prefix, "_code")]])) show_message_bar(output = output, id = 2,
            message = "no_module_element_to_show", type = "severeWarning", language = language)

      if (nrow(module_elements) > 0){

        # Get module element group_id
        distinct_groups <- unique(module_elements$group_id)

        toggles <- c()

        # Loop over distinct cards
        sapply(distinct_groups, function(group_id){

            # Get name of module element
            module_element_name_escaping <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% 
              dplyr::pull(name) %>% stringr::str_replace_all(c("-" = "_", "/" = "_", "\\(" = "_", "\\)" = "_"))

            toggles <<- c(toggles, paste0(module_element_name_escaping, group_id))

          if (!grepl("load_toggles", r[[paste0("reload_", prefix, "_code")]])){

            if (prefix == "patient_lvl"){

              # Get thesaurus items with thesaurus own item_id
              thesaurus_selected_items <- module_elements %>% dplyr::filter(group_id == !!group_id) %>%
                dplyr::select(thesaurus_name, item_id = thesaurus_item_id, display_name = thesaurus_item_display_name,
                  thesaurus_item_unit, colour = thesaurus_item_colour)

              # Need to have thesaurus_name & thesaurus_item_id (of the thesaurus, not of r$thesaurus table) to merge with r data variables

              # Get data from patient, considering thesaurus items selected

              # Initialize variables

              data <- list()
              data$stays <- tibble::tibble()
              data$labs_vitals <- tibble::tibble()
              data$text <- tibble::tibble()
              data$orders <- tibble::tibble()
              data$labs_vitals_stay <- tibble::tibble()
              data$text_stay <- tibble::tibble()
              data$orders_stay <- tibble::tibble()

              # Filter r variables with selected thesaurus items and with patient_id
              # Choose unit, priority to thesaurus (user choice), then data

              if (length(r$chosen_patient) > 0){
                if (!is.na(r$chosen_patient) & r$chosen_patient != ""){

                  if (nrow(r$stays) > 0) data$stays <- r$stays %>% dplyr::filter(patient_id == r$chosen_patient)
                  if (nrow(r$labs_vitals) > 0){
                    data$labs_vitals <-
                      r$labs_vitals %>%
                      dplyr::filter(patient_id == r$chosen_patient) %>%
                      dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id")) %>%
                      dplyr::mutate(unit = dplyr::case_when((thesaurus_item_unit != "" & !is.na(thesaurus_item_unit)) ~ thesaurus_item_unit, TRUE ~ unit)) %>%
                      dplyr::select(-thesaurus_item_unit)
                  }
                  if (nrow(r$text) > 0){
                    data$text <-
                      r$text %>%
                      dplyr::filter(patient_id == r$chosen_patient) %>%
                      dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
                  }
                  if (nrow(r$orders) > 0){
                    data$orders <-
                      r$orders %>%
                      dplyr::filter(patient_id == r$chosen_patient) %>%
                      dplyr::inner_join(thesaurus_selected_items, by = c("thesaurus_name", "item_id"))
                  }
                }
              }

              # If a stay is selected
              if (length(r$chosen_stay) > 0){
                if (!is.na(r$chosen_stay) & r$chosen_stay != ""){

                  data$stay <- r$stays %>% dplyr::filter(stay_id == r$chosen_stay) %>% dplyr::select(admission_datetime, discharge_datetime)

                  if (nrow(data$labs_vitals) > 0) data$labs_vitals_stay <- data$labs_vitals %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)
                  if (nrow(data$text) > 0) data$text_stay <- data$text %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)
                  if (nrow(data$orders) > 0) data$orders_stay <- data$orders %>% dplyr::filter(datetime_start >= data$stay$admission_datetime & datetime_start <= data$stay$discharge_datetime)
                }
              }
            }

            if (prefix == "aggregated"){

              # Initialize variables

              data <- list()
              data$patients <- tibble::tibble()
              data$stays <- tibble::tibble()
              data$labs_vitals <- tibble::tibble()
              data$text <- tibble::tibble()
              data$orders <- tibble::tibble()
              data$patients_subset <- tibble::tibble()
              data$stays_subset <- tibble::tibble()
              data$labs_vitals_subset <- tibble::tibble()
              data$text_subset <- tibble::tibble()
              data$orders_subset <- tibble::tibble()

              if (nrow(r$patients) > 0) data$patients <- r$patients
              if (nrow(r$stays) > 0) data$stays <- r$stays
              if (nrow(r$labs_vitals) > 0) data$labs_vitals <- r$labs_vitals
              if (nrow(r$text) > 0) data$text <- r$text
              if (nrow(r$orders) > 0) data$orders <- r$orders

              patients <- tibble::tibble()
              if (length(r$chosen_subset) > 0){
                if (!is.na(r$chosen_subset) & r$chosen_subset != "") patients <- r$subset_patients %>% dplyr::filter(subset_id == r$chosen_subset)
              }

              if (nrow(patients) > 0){
                patients <- patients %>% dplyr::select(patient_id)
                if (nrow(r$patients) > 0) data$patients_subset <- r$patients %>% dplyr::inner_join(patients, by = "patient_id")
                if (nrow(r$stays) > 0) data$stays_subset <- r$stays %>% dplyr::inner_join(patients, by = "patient_id")
                if (nrow(r$labs_vitals) > 0) data$labs_vitals_subset <- r$labs_vitals %>% dplyr::inner_join(patients, by = "patient_id")
                if (nrow(r$text) > 0) data$text_subset <- r$text %>% dplyr::inner_join(patients, by = "patient_id")
                if (nrow(r$orders) > 0) data$orders_subset <- r$orders %>% dplyr::inner_join(patients, by = "patient_id")
              }
            }

            # Get plugin code

            plugin_id <- module_elements %>% dplyr::filter(group_id == !!group_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)

            code_server_card <- r$code %>%
              dplyr::filter(link_id == plugin_id, category == "plugin_server") %>%
              dplyr::pull(code) %>%
              stringr::str_replace_all("%module_id%", as.character(r[[paste0(prefix, "_selected_key")]])) %>%
              stringr::str_replace_all("%group_id%", as.character(group_id)) %>%
              stringr::str_replace_all("\r", "\n")

            # If it is an aggregated plugin, change %study_id% with current chosen study
            if (length(r$chosen_study) > 0 & prefix == "aggregated") code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(r$chosen_study))
            # If it is a patient-lvl plugin, change %patient_id% with current chosen patient
            if (length(r$chosen_patient) > 0 & prefix == "patient_lvl") code_server_card <- code_server_card %>% stringr::str_replace_all("%patient_id%", as.character(r$chosen_patient))

            # Try to run plugin server code
            # Only if this code has not been already loaded
            if (prefix == "aggregated") trace_code <- paste0(prefix, "_", group_id, "_", r$chosen_study)
            if (prefix == "patient_lvl") trace_code <- paste0(prefix, "_", group_id, "_", r$chosen_patient, "_", r$chosen_stay)

            if ((length(r$chosen_study) > 0 & prefix == "aggregated") | (length(r$chosen_patient) > 0 & prefix == "patient_lvl")){
              if (trace_code %not_in% r$server_plugins_loaded){
                # Add the trace_code to loaded plugins list
                r$server_plugins_loaded <- c(r$server_plugins_loaded, trace_code)

                tryCatch(eval(parse(text = code_server_card)),
                  error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
                    error_name = paste0(id, " - run server code"), category = "Error", error_report = e, language = language),
                  warning = function(w) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
                    error_name = paste0(id, " - run server code"), category = "Warning", error_report = w, language = language)
                )
              }
            }
          }
        })

        if (length(toggles) > 0){
          # Load toggles server code
          sapply(toggles, function(toggle){
            observeEvent(input[[paste0(toggle, "_toggle")]], {
              if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
              else shinyjs::hide(toggle)
            })
          })
        }
      }
    })

  })
}