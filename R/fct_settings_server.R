##########################################
# Add new data                           #
##########################################
  
#' Add new settings data
#' 
#' @param session Shiny session variable
#' @param output Shiny output variable
#' @param r Shiny r reactive value, to communicate between modules (reactiveValue)
#' @param language Language used (character)
#' @param id ID of current module / page (character)
#' @param data A list with data to add (list)
#' @param dropdowns Tibble with the values of distinct dropdowns names (tibble)
#' @examples 
#' \dontrun{
#' data <- list()
#' data$name <- "New datamart"
#' data$description <- "Description of the datamart"
#' data$data_source <- 5
#' add_settings_new_data(output = output, r = r, language = language, id = "settings_datamarts", data = data, dropdowns = "data_source")
#' }

add_settings_new_data <- function(session, output, r = shiny::reactiveValues(), language = "EN", id = character(),
  data = tibble::tibble(), dropdowns = character()){
  
  # Get table name
  table <- substr(id, nchar("settings_") + 1, nchar(id))
  
  # For textfields, we have the choice if empty data is possible or not
  # For dropdowns, we want all of them filled
  
  # Check if name field is not empty
  name_check <- FALSE
  if (!is.na(data$name)) name_check <- TRUE
  if (!name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = translate(language, "provide_valid_name"))
  if (name_check) shiny.fluent::updateTextField.shinyInput(session, "name", errorMessage = NULL)
  req(name_check)
    
  # Check, if name is not empty, if it is not already used
  if (!is.na(data$name)){
    distinct_names <- DBI::dbGetQuery(r$db, paste0("SELECT DISTINCT(name) FROM ", table, " WHERE deleted IS FALSE")) %>% dplyr::pull()
    if (data$name %in% distinct_names) show_message_bar(output, 2, "name_already_used", "severeWarning", language)
    req(data$name %not_in% distinct_names)
  }
  
  # Check if dropdowns are not empty
  dropdowns_check <- TRUE
  sapply(dropdowns, function(dropdown){
    if (dropdown != "") if(is.na(data[[dropdown]])) dropdowns_check <<- FALSE
  })
  if (!dropdowns_check) show_message_bar(output, 2, "dropdown_empty", "severeWarning", language)
  req(dropdowns_check)

  # Get last_row nb
  last_row <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(id), 0) FROM ", table)) %>% dplyr::pull()

  # Creation of new_data variable for data_management pages
  if (table %in% c("data_sources", "datamarts", "studies", "subsets", "thesaurus")){

    # These columns are found in all of these tables
    new_data <- tibble::tribble(~id, ~name, ~description, last_row + 1, data$name, data$description)

    if (id == "settings_datamarts") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data$data_source))
    if (id == "settings_studies") new_data <- new_data %>% dplyr::bind_cols(
      tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
                      data$datamart, data$patient_lvl_module_family, data$aggregated_module_family))
    if (id == "settings_subsets") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~study_id, data$study))
    if (id == "settings_thesaurus") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, data$data_source))

    # These columns are also found in all of these tables
    # Add them at last to respect the order of cols
    new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, r$user_id, as.character(Sys.time()), FALSE))
  }

  # Append data to the table
  DBI::dbAppendTable(r$db, table, new_data)
  # Refresh r variables
  update_r(r = r, table = table, language = language)

  # Add new rows in code table & options table
  # Add default subsets when creating a new study
  last_row_code <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM code") %>% dplyr::pull()
  last_row_options <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM options") %>% dplyr::pull()
  last_row_subsets <- DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM subsets") %>% dplyr::pull()

  # Add a row in code if table is datamarts or thesaurus
  if (table %in% c("datamarts", "thesaurus")){

    DBI::dbAppendTable(r$db, "code",
      tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row_code + 1, get_singular(word = table), last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "code", language = language)
  }

  # Options / datamarts, need ot add two rows
  if (id == "settings_datamarts"){

    DBI::dbAppendTable(r$db, "options",
      tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row_options + 1, "datamart", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_options + 2, "datamart", last_row + 1, "show_only_aggregated_data", "", 0, as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "options", language = language)
  }

  # For studies, need to add one row in options and add rows of code for subsets, with default value
  if (id == "settings_studies"){

    DBI::dbAppendTable(r$db, "options",
      tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row_options + 1, "study", last_row + 1, "user_allowed_read", "", as.integer(r$user_id), as.integer(r$user_id), as.character(Sys.time()), FALSE))

    # Add rows in subsets table, for inclusion / exclusion subsets
    # Add also code corresponding to each subset
    DBI::dbAppendTable(r$db, "subsets",
      tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id,  ~datetime, ~deleted,
        last_row_subsets + 1, translate(language, "subset_all_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_subsets + 2, translate(language, "subset_included_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_subsets + 3, translate(language, "subset_excluded_patients"), "", last_row + 1, as.integer(r$user_id), as.character(Sys.time()), FALSE))

    # Add code for creating subset with all patients
    code <- paste0("run_datamart_code(output, r, %datamart_id%)\n",
      "patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)\n",
      "add_patients_to_subset(output, r, patients, %subset_id%, erase = FALSE)")
    DBI::dbAppendTable(r$db, "code",
      tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row_code + 1, "subset", last_row_subsets + 1, code, as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_code + 2, "subset", last_row_subsets + 2, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_code + 3, "subset", last_row_subsets + 3, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))

    # Update r$options, r$code & r$subsets
    update_r(r, "options", language)
    update_r(r, "subsets", language)
    update_r(r, "code", language)

    # Run code to add patients in the subset. Get datamart_id first.
    datamart_id <- r$studies %>% dplyr::filter(id == last_row) %>% dplyr::pull(datamart_id)
    run_datamart_code(output, r,datamart_id)
    if (nrow(r$patients) == 0) show_message_bar(output = output, id = 2, message = "error_loading_datamart", type = "severeWarning", language = language)
    if (nrow(r$patients) != 0){
      patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)
      add_patients_to_subset(output, r, patients, last_row_subsets + 1, erase = FALSE)
    }
  }

  # Hide creation card & options card, show management card
  shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
  shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
  shiny.fluent::updateToggle.shinyInput(session, "datatable_card_toggle", value = TRUE)

  show_message_bar(output = output, id = 1, message = paste0(get_singular(table), "_added"), type = "success", language = language) 

  # Reset textfields
  sapply(c("name", "description"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
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
                      options = eval(parse(text = dropdowns[name])), multiSelect = TRUE, value = stringr::str_split(data[i, name], ", ") %>% unlist() %>% as.integer()), 
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
              actions <- tagList(actions, shiny::actionButton(paste0("edit_code_", data[i, 1]), "", icon = icon("file-code"),
              onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}
    
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
      
##########################################
# Save edition of the code               #
##########################################  
    
#' Save code edition
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "datamart", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param edited_code New code, after editing it (character)
#' @param language Language used
#' 
save_settings_code <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), edited_code = character(), language = "EN"){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("edit_code_") + 1, nchar(code_id_input)))
  
  # Reload r$code before querying
  r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
  code_id <- r$code %>% dplyr::filter(category == !!category, link_id == !!link_id) %>% dplyr::pull(id)
  
  # Replace ' with '' and store in the database
  DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(edited_code, "'", "''"), "' WHERE id = ", code_id)) -> query
  DBI::dbClearResult(query)
  r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
  
  # Notification to user
  show_message_bar(output, 4, "modif_saved", "success", language)
}
      
##########################################
# Execute the code in edit_code          #
########################################## 

#' Execute / test code after edition
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param edited_code New code, after editing it (character)
            
execute_settings_code <- function(output, r = shiny::reactiveValues(), edited_code = character()){
  
  # Replace %CODE% from code to real values
  code <- edited_code %>%
    stringr::str_replace_all("%datamart_id%", as.character(isolate(r$datamart_id))) %>%
    stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
    stringr::str_replace_all("%thesaurus_id%", as.character(isolate(r$thesaurus_id)))
  
  # Change this option to display correctly tibble in textbox
  eval(parse(text = "options('cli.num_colors' = 1)"))
  
  # Capture console output of our code
  captured_output <- capture.output(
    tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
  
  # Restore normal value
  eval(parse(text = "options('cli.num_colors' = NULL)"))
  
  # Display result
  paste(captured_output, collapse = "\n")
}