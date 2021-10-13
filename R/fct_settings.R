#' settings 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

##########################################
# UI FUNCTIONS                           #
##########################################
  
  ##########################################
  # Default elements                       #
  ##########################################
  
  settings_default_elements <- function(ns, prefix){
    tagList(shiny::uiOutput(ns("message_bar1")), shiny::uiOutput(ns("message_bar2")), shiny::uiOutput(ns("message_bar3")), 
      shiny::uiOutput(ns("message_bar4")), shiny::uiOutput(ns("message_bar5")), shiny.fluent::reactOutput(ns(paste0(prefix, "_delete_confirm"))))
  }

  ##########################################
  # Toggle card                            #
  ##########################################
  
  settings_toggle_card <- function(language, ns, cards = list(), activated = ""){
    toggles <- tagList()
    sapply(cards, function(card){
      if (card$label != "") toggles <<- tagList(toggles, make_toggle(language, ns, label = card$label, 
        id = paste0(card$key, "_toggle"), value = ifelse(card$key %in% activated, TRUE, FALSE), inline = TRUE))
    })
    make_card("",
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10), toggles
      )
    )
  }
  
  ##########################################
  # Creation card                          #
  ##########################################
  
  settings_creation_card <- function(language, ns, title, prefix, textfields = NULL, textfields_width = "200px", dropdowns = NULL, dropdowns_width = "200px"){
    div(id = ns(paste0(prefix, "_creation_card")),
      make_card(
        translate(language, title),
        div(
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 50),
            lapply(textfields, function(name){
              make_textfield(language, ns, name, id = paste0(prefix, "_", name), width = textfields_width)
            })
          ),
          shiny.fluent::Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 50),
            lapply(dropdowns, function(name){
              # Allow multiSelect for thesaurus, column data source
              multiSelect <- FALSE
              if (prefix == "thesaurus") multiSelect <- TRUE
              make_dropdown(language, ns, name, options = "", id = paste0(prefix, "_", name), multiSelect = multiSelect, width = dropdowns_width)
            })
          ),
          htmltools::br(),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_add")), translate(language, "add"))
        )
      )
    )
  }
  
  ##########################################
  # Datatable card                         #
  ##########################################
  
  settings_datatable_card <- function(language, ns, title, prefix){
    div(id = ns(paste0(prefix, "_datatable_card")),
      make_card(translate(language, title),
        div(
          DT::DTOutput(ns(paste0(prefix, "_management_datatable"))),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "save"), style = "top:-20px;")
        )
      )
    ) -> result
    result
  }

  
  ##########################################
  # Options card                           #
  ##########################################
  
  settings_options_card <- function(language, ns, id, r, category_filter, link_id_filter, title, prefix){
  options <- r$options %>% dplyr::filter(category == category_filter, link_id == link_id_filter)
  
  people_picker <- ""
  toggles <- ""
  dropdowns <- ""
  options_by_cat <- id_get_other_name(id, "options_by_cat")
    
    ##########################################
    # Users allowed to read option           #
    ##########################################
    
    if("user_allowed_read" %in% options_by_cat){
      # List of users in the database
      form_options <-
        r$users %>%
        dplyr::filter(!deleted) %>%
        dplyr::left_join(r$users_accesses_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
        dplyr::transmute(key = id, imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
          text = paste0(firstname, " ", lastname), secondaryText = user_status)
      
      # If this is study options, we have to show only users who have access to the parent datamart
      if(category_filter == "study"){
        datamart_id <- r$studies %>% dplyr::filter(id == link_id_filter) %>% dplyr::pull(datamart_id)
        users_allowed_datamart <- 
          r$options %>% 
          dplyr::filter(category == "datamart", link_id == datamart_id, name == "user_allowed_read") %>%
          dplyr::pull(value_num)
        form_options <- form_options %>% dplyr::filter(key %in% users_allowed_datamart)
      }
      
      # Users already allowed
      value <-
        form_options %>%
        dplyr::mutate(n = 1:dplyr::n()) %>%
        dplyr::inner_join(
          options %>%
            dplyr::filter(!deleted, name == "user_allowed_read") %>%
            dplyr::select(key = value_num),
          by = "key"
        ) %>%
        dplyr::pull(key)
      people_picker <- make_people_picker(language, ns, paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_users_allowed_read"),
                                          options = form_options, value = value, width = "100%")
    }
    
    ##########################################
    # Show only aggregated data option       #
    ##########################################
    
    if ("show_only_aggregated_data" %in% options_by_cat){
      value_show_only_aggregated_data <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num)
      toggles <- tagList(
        htmltools::br(), 
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 10),
          make_toggle(language, ns,
            label = "show_only_aggregated_data",
            id = paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_show_only_aggregated_data"), value = value_show_only_aggregated_data, inline = TRUE)
        )
      )
    }
    
    ##########################################
    # Result                                 #
    ##########################################
    
    div(id = ns(paste0(prefix, "_options_card")),
      make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id_filter, ")"), style = "font-size: 15px;")),
        div(
          toggles, people_picker, htmltools::br(),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), dropdowns),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_options_save")), translate(language, "save"))
        )
      )
    )
  }
  
  ##########################################
  # Edit code card                         #
  ##########################################
  
  settings_edit_code_card <- function(language, ns, type = "code", code, link_id, title, prefix){
    choice_ui_server <- tagList()
    if (prefix == "plugins"){
      shiny.fluent::ChoiceGroup.shinyInput(ns(paste0(prefix, "_edit_code_choice_ui_server")), value = "ui", options = list(
        list(key = "ui", text = translate(language, "ui")),
        list(key = "server", text = translate(language, "server"))
      ), className = "inline_choicegroup") -> choice_ui_server
    }
    
    # if (prefix == "plugins"){
    #   div(
    #     shiny::conditionalPanel(
    #       condition = paste0("input.", prefix, "_edit_code_choice_ui_server == 'ui'"), ns = ns,
    #       div(shinyAce::aceEditor(ns(paste0(prefix, "_ace_edit_code_ui")), code, mode = "r", height = "400px"), style = "width: 100%;")),
    #     shiny::conditionalPanel(
    #       condition = paste0("input.", prefix, "_edit_code_choice_ui_server == 'server'"), ns = ns,
    #       div(shinyAce::aceEditor(ns(paste0(prefix, "_ace_edit_code_server")), code, mode = "r", height = "400px"), style = "width: 100%;"))
    #   ) -> ace_editor
    # }
    # if (prefix != "plugins") div(shinyAce::aceEditor(ns(paste0(prefix, "_ace_edit_code")), code, mode = "r", height = "400px"), style = "width: 100%;") -> ace_editor
    
    div(id = ns(paste0(prefix, "_edit_code_card")),
      make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
        div(
          choice_ui_server,
          div(shinyAce::aceEditor(ns(paste0(prefix, "_ace_edit_code")), code, mode = "r", height = "400px"), style = "width: 100%;"),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_edit_code_save")), translate(language, "save")), " ",
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_execute_code")), translate(language, "execute_code")), 
          htmltools::br(), htmltools::br(),
          div(shiny::verbatimTextOutput(ns(paste0(prefix, "_code_result"))), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
        )
      )
    )
  }

##########################################
# SERVER FUNCTIONS                       #
##########################################

  ##########################################
  # New data                               #
  ##########################################
  
  settings_new_data <- function(prefix, data){
    if (prefix %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){
      result <- tibble::tribble(~id, ~name, ~description, data$id, data$name, data$description)
      
      if (prefix == "datamarts") result <- result %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data$data_source_id))
      if (prefix == "studies") result <- result %>% dplyr::bind_cols(
        tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
          data$datamart_id, data$patient_lvl_module_family_id, data$aggregated_module_family_id))
      if (prefix == "subsets") result <- result %>% dplyr::bind_cols(tibble::tribble(~study_id, data$study_id))
      if (prefix == "thesaurus") result <- result %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data$data_source_id))
      # if (prefix == "thesaurus")
      # if (prefix == "thesaurus_items")
      
      result <- result %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, data$creator_id, data$datetime, FALSE))
    }
    result
  }
  
  ##########################################
  # Delete react                           #
  ##########################################
  
  # Code for the dialog box when the action button "delete" is pressed
  settings_delete_react <- function(name, ns, language, delete_dialog){
    dialogContentProps <- list(
      type = 0,
      title = translate(language, paste0(name, "_delete")),
      closeButtonAriaLabel = "Close",
      subText = translate(language, paste0(name, "_delete_subtext"))
    )
    shiny.fluent::Dialog(
      hidden = !delete_dialog,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", name, "_hide_dialog', Math.random()); }")),
      dialogContentProps = dialogContentProps,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(name, "_delete_confirmed")), text = translate(language, "delete")),
        shiny.fluent::DefaultButton.shinyInput(ns(paste0(name, "_delete_canceled")), text = translate(language, "dont_delete"))
      )
    )
  }
    
  ##########################################
  # Datatable                             #
  ##########################################
    
    ##########################################
    # Data                                   #
    ##########################################
    
    settings_datatable_data <- function(prefix, r){
      data <- r[[paste0(prefix, "_temp")]]
      if (nrow(data) != 0) data <- data %>% dplyr::select(-deleted, -modified)
      data
    }  
  
    ##########################################
    # Generate datatable                     #
    ##########################################
  
      # ===== settings_datatable function =====
      # id = id of the module (eg : settings_datamarts)
      # prefix = name of the page or element of the page (eg : datamarts for datamarts page, users_accesses for subpage accesses of user page)
      # data = data used in datatable
      # data_variables = variables r$... containing data (eg : r$datamarts)
      # dropdowns = dropdowns shown in the datatable
      # action_buttons = actions buttons created (eg : "delete", "edit_code", "options")
      
      settings_datatable <- function(ns, r, id, prefix, data, data_variables, dropdowns = NULL, action_buttons = "", new_colnames = ""){
        if (nrow(data) == 0) return(data)
        
        # Order data by ID
        data <- data %>% dplyr::arrange(id)
        
        # Create vars with existing options (ie : for data_sources, a list of existing data_sources in the database)
        sapply(data_variables, function(data_var){
          if (data_var %in% c("patient_lvl_modules", "aggregated_modules")) null_value <- TRUE else null_value <- FALSE
            assign(data_var, tibble_to_list(r[[data_var]], "id", "name", null_value = null_value, rm_deleted_rows = TRUE), envir = parent.env(environment()))
        })
        
        # Add a column action in the DataTable
        data["action"] <- NA_character_
        
        # Transform dropdowns columns in the dataframe to character
        lapply(names(dropdowns), function(name) data %>% dplyr::mutate_at(name, as.character) ->> data)
        
        # For each row of the dataframe :
        # - transform dropdowns columns to show dropdowns in Shiny app
        # - add an Action column with delete action button (+/- options / edit code buttons)
        # - show creator name
        if (nrow(data) != 0){
          for (i in 1:nrow(data)){
            lapply(names(dropdowns), function(name){
              if (prefix == "thesaurus"){
                data[i, name] <<- as.character(
                  div(
                    shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[name], data[i, "id"])),
                      options = eval(parse(text = dropdowns[name])), multiSelect = TRUE, selectedKeys = stringr::str_split(data[i, name], ", ") %>% unlist() %>% as.integer()), 
                    onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_dropdown_updated', '", paste0(dropdowns[name], data[i, "id"]), "', {priority: 'event'})"),
                    style = "width:100%")
                )
              }
              else {
                data[i, name] <<- as.character(
                  div(
                    shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[name], data[i, "id"])),
                      options = eval(parse(text = dropdowns[name])), value = as.integer(data[i, name])), 
                    onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_dropdown_updated', '", paste0(dropdowns[name], data[i, "id"]), "', {priority: 'event'})"),
                    style = "width:100%")
                ) 
              }
            })
    
            # Action buttons : if in action_buttons vector, add action button
            actions <- tagList()
    
            # Add options button
            if ("options" %in% action_buttons){
              actions <- tagList(actions, shiny::actionButton(paste0(prefix, "_options_", data[i, 1]), "", icon = icon("cog"),
              onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_options", "', this.id, {priority: 'event'})")), "")}
    
            # Add edit code button
            if ("edit_code" %in% action_buttons){
              actions <- tagList(actions, shiny::actionButton(paste0(prefix, "_edit_code_", data[i, 1]), "", icon = icon("file-code"),
              onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_edit_code", "', this.id, {priority: 'event'})")), "")}
    
            # Add sub datatable button
            if ("sub_datatable" %in% action_buttons){
              actions <- tagList(actions, shiny::actionButton(paste0(prefix, "_sub_datatable_", data[i, 1]), "", icon = icon("table"),
                onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_sub_datatable", "', this.id, {priority: 'event'})")), "")}
            
            # Add delete button
            if ("delete" %in% action_buttons){
              # If row is deletable (not a column for deletable or not, only default subsets are not deletable)
              if (prefix != "subsets" | data[i, "name"] %not_in% c("All patients", "Included patients", "Excluded patients")){
                actions <- tagList(actions, shiny::actionButton(paste0(prefix, "_delete_", data[i, 1]), "", icon = icon("trash-alt"),
                                                                onclick = paste0("Shiny.setInputValue('", id, "-", prefix, "_deleted_pressed', this.id, {priority: 'event'})")))} 
            }
    
            # Update action column in dataframe
            data[i, "action"] <- as.character(div(actions))
            # if (prefix == "thesaurus_items") data <- data %>% dplyr::select(-action)
    
            # Get creator name
            if ("creator_id" %in% names(data)){
              data[i, "creator_id"] <- r$users %>% dplyr::filter(id == data[[i, "creator_id"]]) %>%
                dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator)
            }
            
            # Get names for other columns if there are not dropdowns
            # Failed to loop that...
            if ("data_source_id" %in% names(data) & "data_source_id" %not_in% names(dropdowns)){
              result <- r$data_sources %>% dplyr::filter(id == data[[i, "data_source_id"]]) %>% dplyr::pull(name)
              if (length(result) == 0) result <- ""
              data[[i, "data_source_id"]] <- result
            }
            if ("datamart_id" %in% names(data) & "datamart_id" %not_in% names(dropdowns)){
              result <- r$datamarts %>% dplyr::filter(id == data[[i, "datamart_id"]]) %>% dplyr::pull(name)
              if (length(result) == 0) result <- ""
              data[[i, "datamart_id"]] <- result
            }
            if ("study_id" %in% names(data) & "study_id" %not_in% names(dropdowns)){
              result <- r$studies %>% dplyr::filter(id == data[[i, "study_id"]]) %>% dplyr::pull(name)
              if (length(result) == 0) result <- ""
              data[[i, "study_id"]] <- result
            }
          }
        }
        
        # Change name of cols 
        colnames(data) <- new_colnames
        
        data
      }
      
      datatable_callback <- function(){
        htmlwidgets::JS("table.rows().every(function(i, tab, row) {
                var $this = $(this.node());
                $this.attr('id', this.data()[0]);
                $this.addClass('shiny-input-container');
              });
              Shiny.unbindAll(table.table().node());
              Shiny.bindAll(table.table().node());")
      }
    
    ##########################################
    # Delete a row in datatable              #
    ##########################################
    
      settings_delete_row <- function(input, output, r, ns, language, prefix, data_var, message){
        
        # Create & show dialog box 
        output[[paste0(prefix, "_delete_confirm")]] <- shiny.fluent::renderReact(settings_delete_react(prefix, ns, language, r[[paste0(prefix, "_delete_dialog")]]))
        
        # Whether to close or not delete dialog box
        observeEvent(input[[paste0(prefix, "_hide_dialog")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
        observeEvent(input[[paste0(prefix, "_delete_canceled")]], r[[paste0(prefix, "_delete_dialog")]] <<- FALSE)
        observeEvent(input[[paste0(prefix, "_deleted_pressed")]], r[[paste0(prefix, "_delete_dialog")]] <<- TRUE)
        
        # When the delete is confirmed...
        observeEvent(input[[paste0(prefix, "_delete_confirmed")]], {
          
          # Close dialog box
          r[[paste0(prefix, "_delete_dialog")]] <- FALSE
          
          # Get the ID of row deleted
          deleted_pressed_value <- isolate(input[[paste0(prefix, "_deleted_pressed")]])
          row_deleted <- as.integer(substr(deleted_pressed_value, nchar(paste0(prefix, "_delete_")) + 1, nchar(deleted_pressed_value)))
          # Delete row in database
          DBI::dbSendStatement(r$db, paste0("UPDATE ", data_var, " SET deleted = TRUE WHERE id = ", row_deleted))
          # Update r vars (including temp variable, used in management datatables)
          r[[data_var]] <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", data_var))
          r[[paste0(data_var, "_temp")]] <- r[[data_var]] %>% dplyr::filter(!deleted) %>% dplyr::mutate(modified = FALSE)
          
          # Notification to user
          output$warnings3 <- renderUI({
            div(shiny.fluent::MessageBar(translate(language, message), messageBarType = 3), style = "margin-top:10px;")
          })
          shinyjs::show("warnings3")
          shinyjs::delay(3000, shinyjs::hide("warnings3"))
        }) 
      }