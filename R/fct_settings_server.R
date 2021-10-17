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
    new_data <- tibble::tribble(~id, ~name, ~description, last_row + 1, as.character(data$name), as.character(data$description))
    
    if (id == "settings_datamarts") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.integer(data$data_source)))
    if (id == "settings_studies") new_data <- new_data %>% dplyr::bind_cols(
      tibble::tribble(~datamart_id,  ~patient_lvl_module_family_id, ~aggregated_module_family_id,
        as.integer(data$datamart), as.integer(data$patient_lvl_module_family), as.integer(data$aggregated_module_family)))
    if (id == "settings_subsets") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~study_id, as.integer(data$study)))
    if (id == "settings_thesaurus") new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.character(data$data_source)))
    
    # These columns are also found in all of these tables
    # Add them at last to respect the order of cols
    new_data <- new_data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, r$user_id, as.character(Sys.time()), FALSE))
  }
  
  # Creation of new_data variable for plugins page
  if (table == "plugins"){
    new_data <- tibble::tribble(~id, ~name, ~description, ~module_type_id, ~datetime, ~deleted,
      last_row + 1, as.character(data$name), "", as.integer(data$module_type), as.character(Sys.time()), FALSE)
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
  
  # Add a row in code if table is datamarts, thesaurus or plugins
  if (table %in% c("datamarts", "thesaurus", "plugins")){
    
    DBI::dbAppendTable(r$db, "code",
      tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row_code + 1, get_singular(word = table), last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "code", language = language)
  }
  
  # For options of plugins, add one row for long description (RMarkdown)
  if (id == "settings_plugins"){
    DBI::dbAppendTable(r$db, "options",
      tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row_options + 1, "plugin", last_row + 1, "rmarkdown_description", "", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "options", language = language)
  }
  
  # For options of datamarts, need ot add two rows
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
# Generate datatable                     #
##########################################

#' Render management datatable
#' 
#' @description Renders a datatable (from library DT)
#' @details 
#' NB : don't forget Action column in the col_names argument.\cr\cr
#' For more informations about DT, see https://datatables.net/manual/options.\cr
#' See DOM documentation here : https://datatables.net/reference/option/dom\cr
#' See columnDefs doc here : https://datatables.net/reference/option/columnDefs
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param ns Shiny namespace
#' @param language Language used (charater)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param col_names A character vector containing colnames, already translated (character)
#' @param table Name of a table of database (character)
#' @param dropdowns Character vector with names of the dropdowns available in the datatable (character)
#' @param action_buttons Character vector with action_buttons needed (character)
#' @param datatable_dom Character containing DOM code for the datatable (character)
#' @param page_length Page length of the datatable, default to 10 rows (integer)
#' @param start Which page display (used when we save datatable state), default to 1 (integer)
#' @param editable_cols Which cols are editable (character vector)
#' @param sortable_cols Which cols are sortable (character vector)
#' @param centered_cols Which cols are centered (character vector)
#' @param column_widths Columns widths (named character vector)
#' @examples 
#' \dontrun{
#' data <- tibble::tribble(~id, ~name, ~description, ~data_source_id,
#'   2, "Name of the datamart", "Description of the datamart", 3)
#'   
#'
#' col_names <- c("ID", "Name", "Description", "Data source ID", "Action")
#' action_buttons <- c("delete", "edit_code")
#' editable_cols <- c("id", "name")
#' sortable_cols <- "id"
#' centered_cols <- "id"
#' column_widths <- c("name" = "200px", "description" = "300px")
#'
#' render_settings_datatable(
#'   output = output, r = r, 
#'   ns = NS("settings_datamart"), language = "EN",
#'   id = "settings_datamart",
#'   col_names = col_names, 
#'   table = "datamarts", 
#'   dropdowns = "data_source",
#'   action_buttons = action_buttons, 
#'   datatable_dom = "<'top'ft>",
#'   page_length = 20, 
#'   start = 1, 
#'   editable_cols = editable_cols, 
#'   sortable_cols = sortable_cols,
#'   centered_cols = centered_cols, 
#'   column_widths = column_widths)
#' }

render_settings_datatable <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN", id = character(),
  col_names = character(), table = character(), dropdowns = character(), action_buttons = character(),
  datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = 10, start = 1,
  editable_cols = integer(), sortable_cols = integer(), centered_cols = character(), column_widths = character()
){
  
  # Load temp data
  data <- r[[paste0(table, "_temp")]]
  
  # If page is plugins, remove column description from datatable (it will be editable from datatable row options edition)
  if (id == "settings_plugins") data <- data %>% dplyr::select(-description)
  
  # Add a column action in the DataTable
  data["action"] <- NA_character_

  # If no row in dataframe, stop here
  if (nrow(data) == 0) return(data)

  # Drop deleted column & modified column : we don't want to show them in the datatable
  if (nrow(data) != 0) data <- data %>% dplyr::select(-deleted, -modified)
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)

  # Transform dropdowns columns in the dataframe to character
  lapply(names(dropdowns), function(col_name) data %>% dplyr::mutate_at(col_name, as.character) ->> data)

  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name

  for (i in 1:nrow(data)){

    #############
    # DROPDOWNS #
    #############

    lapply(names(dropdowns), function(name){

      # Particularity with thesaurus, data_source_id column can contains multiple values (multiSelect = TRUE)
      # We have to split data_source_id column, to have an integer vector (saved with collapse by commas)

      # name here is like "data_source_id"
      # dropdowns[name] here is like "data_sources"
      # so r[[dropdowns[[name]]]] is like r$data_sources, var containing data_sources data

      if (id == "settings_thesaurus"){
        value <- NULL
        if (length(data[i, name] > 0)){
          if (!TRUE %in% grepl("[a-zA-Z]", stringr::str_split(data[i, name], ", ") %>% unlist())){
            value <- stringr::str_split(data[i, name], ", ") %>% unlist() %>% as.integer()
          }
        }
        data[i, name] <<- as.character(
          div(
            shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[name], data[i, "id"])),
              options = convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = FALSE),
              value = value,
              multiSelect = TRUE),
              onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[name], data[i, "id"]), "', {priority: 'event'})"),
              style = "width:100%")
        )
      }
      
      if (id %in% c("settings_data_sources", "settings_datamarts", "settings_studies", "settings_subsets", "settings_plugins")) {
        data[i, name] <<- as.character(
          div(
            # So ID is like "data_sources13" if ID = 13
          shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[name], data[i, "id"])),
            # To get options, convert data var to tibble (convert r$data_sources to list)
            options = convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = FALSE),
            # value is an integer, the value of the column like "data_source_id"
            value = as.integer(data[i, name])),
            # On click, we set variable "dropdown_updated" to the ID of the row (in our example, 13)
            onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[name], data[i, "id"]), "', {priority: 'event'})"),
            style = "width:100%")
        )
      }
      
    })

    ##################
    # ACTION BUTTONS #
    ##################

    # Action buttons : if in action_buttons vector, add action button
    actions <- tagList()

    # Add options button
    if ("options" %in% action_buttons){
      actions <- tagList(actions,
        shiny::actionButton(paste0("options_", data[i, 1]), "", icon = icon("cog"),
          onclick = paste0("Shiny.setInputValue('", id, "-options", "', this.id, {priority: 'event'})")), "")}

    # Add edit code button
    if ("edit_code" %in% action_buttons){
      actions <- tagList(actions,
        shiny::actionButton(paste0("edit_code_", data[i, 1]), "", icon = icon("file-code"),
          onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}

    # Add sub datatable button
    if ("sub_datatable" %in% action_buttons){
      actions <- tagList(actions,
        shiny::actionButton(paste0("sub_datatable_", data[i, 1]), "", icon = icon("table"),
          onclick = paste0("Shiny.setInputValue('", id, "-sub_datatable", "', this.id, {priority: 'event'})")), "")}

    # Add delete button
    if ("delete" %in% action_buttons){

      # If row is deletable (we havn't made a function argument for deletable or not, only default subsets are not deletable)
      # Could be changed later

      if (id != "settings_subsets" | data[i, "name"] %not_in% c("All patients", "Included patients", "Excluded patients")){
        actions <- tagList(actions, shiny::actionButton(paste0("delete_", data[i, 1]), "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})")))}
    }

    # Update action column in dataframe
    data[i, "action"] <- as.character(div(actions))

    ################
    # CREATOR NAME #
    ################

    if ("creator_id" %in% names(data)){
      data[i, "creator_id"] <-
        r$users %>% dplyr::filter(id == data[[i, "creator_id"]]) %>%
        dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
        dplyr::pull(creator)
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
  
  # Which columns are non editable
  # Test with :
  # data <- tibble::tribble(~id, ~name, ~description, ~action, 1, "name1", "description1", "my_action")

  cols <- c(1:length(names(data))) - 1
  editable_cols_vec <- integer()
  sapply(editable_cols, function(col){
    editable_cols_vec <<- c(editable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_editable_cols_vec <- cols[!cols %in% editable_cols_vec]

  # Which columns are non sortable
  sortable_cols_vec <- integer()
  sapply(sortable_cols, function(col){
    sortable_cols_vec <<- c(sortable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
  
  # Which cols are centered
  centered_cols_vec <- integer()
  sapply(centered_cols, function(col){
    centered_cols_vec <<- c(centered_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })

  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))

  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable

  output$management_datatable <- DT::renderDT(
    # Data
    data,

    # Options of the datatable
    options = list(
      dom = datatable_dom,
      stateSave = TRUE, stateDuration = 30,
      pageLength = page_length, displayStart = start,
      columnDefs = column_defs,
      language = list(
        paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
        search = translate(language, "DT_search"),
        lengthMenu = translate(language, "DT_length"))
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),

    # Default options
    rownames = FALSE, selection = "single", escape = FALSE, server = TRUE,

    # Javascript code allowing to have dropdowns & actionButtons on the DataTable
    callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
      var $this = $(this.node());
      $this.attr('id', this.data()[0]);
      $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
}

##########################################
# Save updates in datatable              #
##########################################

#' Update datatable
#' 
#' @param input Shiny input variable
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param dropdowns Dropdowns shown on datatable (character)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' update_settings_datatable(r = r, ns = ns, table = "datamarts", dropdowns = "data_source", language = "EN")
#' }

update_settings_datatable <- function(input, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), dropdowns = character(), language = "EN"){
  
  sapply(r[[table]] %>% dplyr::pull(id), function(id){
    sapply(dropdowns, function(dropdown){
      observeEvent(input[[paste0(get_plural(word = dropdown), id)]], {
        
        # When we load a page, every dropdown triggers the event
        # Change temp variable only if new value is different than old value
        old_value <- r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown, "_id")]]
        
        # If thesaurus, data_source_id can accept multiple values (converting to string)
        if (table == "thesaurus") new_value <- toString(input[[paste0("data_sources", id)]])
        if (table %in% c("data_sources", "datamarts", "studies", "subsets", "plugins")) new_value <- as.integer(input[[paste0(get_plural(word = dropdown), id)]])
        
        if (new_value != old_value){
          r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown, "_id")]] <- new_value
          # Store that this row has been modified
          r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
        }
      })
    })
  })
}

#' Save changes in datatable
#' 
#' @param output Shiny output variable
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' save_settings_datatable_updates(output = output, r = r, ns = ns, table = "datamarts", language = "EN")
#' }

save_settings_datatable_updates <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), language = "EN"){
  
  # Make sure there's no duplicate in names
  duplicates <- 0
  # Duplicates are allowed in thesaurus_items
  # if (table != "thesaurus_items"){
  duplicates <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
    dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
  # }
  if (duplicates > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
  req(duplicates == 0)
  
  # Save changes in database
  ids_to_del <- r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
  DBI::dbClearResult(query)
  DBI::dbAppendTable(r$db, table, r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified))
  
  # Notification to user
  show_message_bar(output, 2, "modif_saved", "success", language)
}
  
##########################################
# Delete a row in datatable              #
##########################################

#' Render delete react
#' 
#' @param r Shiny r reactive value to communicate between modules
#' @param ns Shiny namespace
#' @param table Name of the table used (character)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' render_settings_delete_react(r = r, table = "datamarts")
#' }

render_settings_delete_react <- function(r = shiny::reactiveValues(), ns = shiny::NS(), table = character(), language = "EN"){
  dialogContentProps <- list(
    type = 0,
    title = translate(language, paste0(table, "_delete")),
    closeButtonAriaLabel = "Close",
    subText = translate(language, paste0(table, "_delete_subtext"))
  )
  shiny.fluent::Dialog(
    hidden = !r[[paste0(table, "_delete_dialog")]],
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('hide_dialog', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns("delete_confirmed"), text = translate(language, "delete")),
      shiny.fluent::DefaultButton.shinyInput(ns("delete_canceled"), text = translate(language, "dont_delete"))
    )
  )
}

#' Delete a row in datatable
#' 
#' @param output Shiny output variable
#' @param r r Shiny reactive value used to communicate between modules
#' @param ns Shiny namespace
#' @param language Language used (character)
#' @param row_deleted ID of row to delete (integer) 
#' @param table Name of the table used (character)
#' @examples 
#' \dontrun{
#' delete_settings_datatable_row(output = output, r = r, ns = ns, language = "EN", row_deleted = 13, table = "datamarts")
#' }

delete_settings_datatable_row <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN",
  row_deleted = integer(), table = character()){
  
  # Close dialog box
  r[[paste0(table, "_delete_dialog")]] <- FALSE
  
  # Delete row in database
  DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted))
  
  # Update r vars
  update_r(r = r, table = table, language = language)
  
  # Notification to user
  show_message_bar(output = output, id = 3, paste0(get_singular(word = table), "_deleted"), type ="severeWarning", language = language)
}

##########################################
# Save updates of options                #
##########################################  

#' Save options
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "datamart", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param data New data to store in options table (list)
#' @param language Language used
#' @examples
#' \dontrun{
#' data <- list()
#' data$show_only_aggregated_data <- TRUE
#' data$users_allowed_read <- c(1, 3, 4)
#' save_settings_options(output = output, r = r, id = "settings_datamart", category = "datamart", code_id_input = "edit_code_3",
#'   data = data, language = "EN")
#' }

save_settings_options <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), data = data, language = "EN"){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("options_") + 1, nchar(code_id_input)))
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  # Get options with page ID
  page_options <- get_page_options(id = id)
  
  if("show_only_aggregated_data" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value_num = ", data$show_only_aggregated_data, " WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
  }
  
  if ("users_allowed_read" %in% page_options){
    
    # The idea is to delete every rows of options for this module, and then reinsert one row per user
    # Get unique ID (peoplePicker can select twice a user, if he's already checked at the initiation of the input)
    
    # Delete all users allowed in the options table
    rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("DELETE FROM options WHERE id IN (", paste(rows_to_del, collapse = ","),")")) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
    
    # Add users in the selected list
    if (length(data$users_allowed_read) != 0){
      data$users_allowed_read <- unique(data$users_allowed_read)
      last_row <- max(r$options["id"])
      DBI::dbAppendTable(r$db, "options",
        tibble::tibble(id = (last_row + (1:length(data$users_allowed_read))), category = category, link_id = link_id,
          name = "user_allowed_read", value = "", value_num = data$users_allowed_read, creator_id = as.integer(r$user_id),
          datetime = as.character(Sys.time()), deleted = FALSE))
      update_r(r = r, table = "options", language = language)
    }
  }
  
  if ("rmarkdown_description" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "rmarkdown_description") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = ", data$rmarkdown_description, " WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
  }
  
  show_message_bar(output, 4, "modif_saved", "success", language)
}

##########################################
# Save edition of the code               #
##########################################  

#' Save code edition
#' 
#' @description Save code in code table after editing it
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "datamart", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param edited_code New code, after editing it (character)
#' @param language Language used
#' @examples
#' \dontrun{
#' save_settings_code(output = output, r = r, id = "settings_datamart", category = "datamart", code_id_input = "edit_code_5",
#'   edited_code = "print('test code edition')", language = "EN")
#' }

save_settings_code <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), edited_code = character(), language = "EN"){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("edit_code_") + 1, nchar(code_id_input)))
  
  # Reload r$code before querying
  update_r(r = r, table = "options", language = language)
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
#' @description Execute code entered in the ShinyAce editor
#' @param output variable from Shiny, used to render messages on the message bar
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param edited_code New code, after editing it (character)
#' @examples 
#' \dontrun{
#' execute_settings_code(output = output, r = r, edited_code = "print('test')")
#' }

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




##########################################
# TO DELETE                              #
########################################## 


# delete asap
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


# delete asap
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
