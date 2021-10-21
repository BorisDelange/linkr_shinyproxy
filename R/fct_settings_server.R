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
  
  # Add a row in code if table is datamarts, thesaurus
  if (table %in% c("datamarts", "thesaurus")){
    
    DBI::dbAppendTable(r$db, "code",
      tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row_code + 1, get_singular(word = table), last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "code", language = language)
  }
  
  # For options of plugins, add one row for long description (Markdown) & a toggle for the status / the visibility of the plugin (In dev / Public)
  # The value is default syntax of a plugin description
  # For code of plugins, add two rows, ony for UI code & one for server code
  if (id == "settings_plugins"){
    
    # Add options rows
    value <- paste0("- Version : 1.0.0\n- Libraries : *put libraries needed here*\n- Data allowed : *put data allowed here*\n\n",
      "*Put full description here*")
    DBI::dbAppendTable(r$db, "options",
      tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row_options + 1, "plugin", last_row + 1, "markdown_description", value, NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_options + 2, "plugin", last_row + 1, "visibility", "dev_only", NA_integer_, as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "options", language = language)
    
    # Add code rows
    DBI::dbAppendTable(r$db, "code",
      tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row_code + 1, "plugin_ui", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE,
        last_row_code + 2, "plugin_server", last_row + 1, "", as.integer(r$user_id), as.character(Sys.time()), FALSE))
    update_r(r = r, table = "code", language = language)
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
    code <- paste0("run_datamart_code(output = output, r = r, datamart_id = %datamart_id%)\n",
                   "patients <- r$patients %>% dplyr::select(patient_id) %>% dplyr::mutate_at('patient_id', as.integer)\n",
                   "add_patients_to_subset(output = output, r = r, patients = patients, subset_id = %subset_id%, erase = FALSE)")
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
#' @param output_name Name of the datatable output
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
#' @param filter If TRUE, we can filter we search box each column (logical)
#' @param searchable_cols If filter is TRUE, choose which columns are searchable (character)
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
#' filter <- TRUE
#' searchable_cols <- c("name", "description")
#' column_widths <- c("name" = "200px", "description" = "300px")
#'
#' render_settings_datatable(
#'   output = output, r = r, 
#'   ns = NS("settings_datamart"), language = "EN",
#'   id = "settings_datamart",
#'   output_name = "management_datatable"
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
#'   filter = filter,
#'   searchable_cols = searchable_cols,
#'   column_widths = column_widths)
#' }

render_settings_datatable <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN", id = character(),
  output_name = character(), col_names = character(), table = character(), dropdowns = character(), action_buttons = character(),
  datatable_dom = "<'datatable_length'l><'top'ft><'bottom'p>", page_length = 10, start = 1,
  editable_cols = character(), sortable_cols = character(), centered_cols = character(), searchable_cols = character(), 
  filter = FALSE, factorize_cols = character(), column_widths = character()
){
  
  # Translation for datatable
  dt_translation <- list(
    paginate = list(previous = translate(language, "DT_previous_page"), `next` = translate(language, "DT_next_page")),
    search = translate(language, "DT_search"),
    lengthMenu = translate(language, "DT_length"),
    emptyTable = translate(language, "DT_empty"))
  
  # Load temp data
  data <- r[[paste0(table, "_temp")]]
  
  # If no row in dataframe, stop here
  if (nrow(data) == 0) return({
    data <- tibble::tribble(~id, ~name, ~description,  ~datetime)
    names(data) <- c(translate(language, "id"), translate(language, "name"), translate(language, "description"), translate(language, "datetime"))
    output[[output_name]] <- DT::renderDT(data, options = list(dom = 'tp'))
  })
  
  # If page is plugins, remove column description from datatable (it will be editable from datatable row options edition)
  # /!\ Careful : it changes the index of columns, use to update informations directy on datatable
  if (table == "plugins") data <- data %>% dplyr::select(-description)
  if (table == "thesaurus_items") data <- data %>% dplyr::select(-id, -thesaurus_id)
  
  # Add a column action in the DataTable
  # Action column is already loaded for thesaurus_items (cache system)
  if (table != "thesaurus_items" & length(action_buttons) != 0) data["action"] <- NA_character_

  # Drop deleted column & modified column : we don't want to show them in the datatable
  if (nrow(data) != 0) data <- data %>% dplyr::select(-deleted, -modified)
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)

  # Transform dropdowns columns in the dataframe to character
  if (length(dropdowns) != 0) lapply(names(dropdowns), function(col_name) data %>% dplyr::mutate_at(col_name, as.character) ->> data)

  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name

  # Loop over data only if necessary (eg not necessary for thesaurus_items, with a lot of rows...)
  if (table != "thesaurus_items" & (length(dropdowns) != 0 | length(action_buttons) != 0 | "creator_id" %in% names(data))){
  
    for (i in 1:nrow(data)){
  
      #############
      # DROPDOWNS #
      #############
  
      if (length(dropdowns) != 0){
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
                  style = "width:200px")
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
                style = "width:200px")
            )
          }
          
        })
      }
  
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
      if (length(action_buttons) != 0) data[i, "action"] <- as.character(div(actions))
  
      ################
      # CREATOR NAME #
      ################
  
      if ("creator_id" %in% names(data)){
        if (nrow(r$users %>% dplyr::filter(id == data[[i, "creator_id"]])) > 0){
          data[i, "creator_id"] <-
            r$users %>% dplyr::filter(id == data[[i, "creator_id"]]) %>%
            dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
            dplyr::pull(creator)
        }
        else data[i, "creator_id"] <- translate(language, "deleted_user")
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
  
  # Which cols are searchable
  searchable_cols_vec <- integer()
  sapply(searchable_cols, function(col){
    searchable_cols_vec <<- c(searchable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_searchable_cols_vec <- cols[!cols %in% searchable_cols_vec]
  
  # If filter is TRUE
  if (filter) filter_list <- list(position = "top")
  if (!filter) filter_list <- list()

  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
  
  # Add searchable cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(searchable = FALSE, targets = non_searchable_cols_vec))
  
  # Transform searchable cols to factor
  # Don't factorize name & description cols, except for subsets (name are usually included / excluded / all patients...)
  sapply(factorize_cols, function(col) data <<- data %>% dplyr::mutate_at(col, as.factor))

  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable

  output[[output_name]] <- DT::renderDT(
    # Data
    data,

    # Options of the datatable
    options = list(
      dom = datatable_dom,
      stateSave = TRUE, stateDuration = 30,
      pageLength = page_length, displayStart = start,
      columnDefs = column_defs,
      language = dt_translation
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    
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
# Create a cache for action buttons col  #
##########################################

#' Create cache for datatable data
#' 
#' @param output Shiny output value, to show message bars
#' @param r Shiny r reactive value, to communicate between modules
#' @param language Language used (character)
#' @param module_id ID of current page / module (character)
#' @param thesaurus_id ID of thesaurus, which thesaurus items depend on (integer)
#' @param datamart_id ID of datamart to count rows by item of the thesaurus (integer)
#' @param category Category of cache, depending of the page of Settings (character)

create_datatable_cache <- function(output, r, language = "EN", module_id = character(), 
  thesaurus_id = integer(), datamart_id = 0, category = character()){
  
  # Load join between our data and the cache
  
  # For action buttons (delete & plus_minus), don't use datamart_id / link_id_bis
  if (category %in% c("delete", "plus_minus")){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted, c.value
      FROM thesaurus_items t
      LEFT JOIN cache c ON c.link_id = t.id AND c.category = '", category, "'
      WHERE t.thesaurus_id = ", thesaurus_id, " AND t.deleted IS FALSE
      ORDER BY t.id")) %>% tibble::as_tibble()
  }
  # For count_patients_rows & count_items_rows, use datamart_id / link_id_bis (we count row for a specific datamart)
  if (category %in% c("count_patients_rows", "count_items_rows")){
    data <- DBI::dbGetQuery(r$db, paste0(
     "SELECT t.id, t.thesaurus_id, t.item_id, t.name, t.display_name, t.category, t.unit, t.datetime, t.deleted, c.value
      FROM thesaurus_items t
      LEFT JOIN cache c ON c.link_id = t.id AND c.link_id_bis = ", datamart_id, " AND c.category = '", category, "'
      WHERE t.thesaurus_id = ", thesaurus_id, " AND t.deleted IS FALSE
      ORDER BY t.id")) %>% tibble::as_tibble()
  }
  
  # If there are missing data in the cache, reload cache 
  
  reload_cache <- FALSE
  if (NA_character_ %in% data$value | "" %in% data$value) reload_cache <- TRUE
  
  # Reload cache if necessary
  if (reload_cache){

    # Reload data
    data <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM thesaurus_items WHERE thesaurus_id = ", thesaurus_id, " AND deleted IS FALSE ORDER BY id"))

    # Make action column, depending on category
    # If category is count_items_rows, add a count row column with number of rows by item in the datamart
    # If category is count_patients_rows, add a count row column with number of patients by item in the datamart
    # If category is delete, add a delete button only
    # If category is plus_minus, add plus and minus buttons

    if (category == "count_items_rows"){
      
      # Run datamart code
      # Could interfere with r data variables in patient-lvl & aggregated data, but rare case...
      # A solution could be adding a global variable indicating that r data variables have changed
      run_datamart_code(output = output, r = r, language = language, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_items_rows = integer())
      
      if (nrow(r$labs_vitals) != 0) rows_labs_vitals <- r$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(r$text) != 0) rows_text <- r$text %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      if (nrow(r$orders) != 0) rows_orders <- r$orders %>% dplyr::group_by(thesaurus_name, item_id) %>% dplyr::summarize(count_items_rows = dplyr::n()) %>% dplyr::ungroup()
      
      # Merge rows count vars
      count_items_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      # Check if argument thesaurus_id corresponds to thesaurus_name in the data
      # Select thesaurus from r$thesaurus with thesaurus_id
      # Inner join with correspondance between thesaurus_name

      count_items_rows <- count_items_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
          dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_items_rows) != 0){
        count_items_rows <- count_items_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data <- data %>% dplyr::left_join(count_items_rows, by = "item_id") %>% dplyr::rename(value = count_items_rows)
      }
      if (nrow(count_items_rows) == 0) data <- data %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data <- data %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", T ~ value))
    }
    
    if (category == "count_patients_rows"){
      
      run_datamart_code(output = output, r = r, language = language, datamart_id = datamart_id)
      
      # Initiate variables
      rows_labs_vitals <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_text <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      rows_orders <- tibble::tibble(thesaurus_name = character(), item_id = integer(), count_patients_rows = integer())
      
      if (nrow(r$labs_vitals) != 0) rows_labs_vitals <- r$labs_vitals %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(r$text) != 0) rows_text <- r$text %>% dplyr::group_by(thesaurus_name, item_id) %>% 
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      if (nrow(r$orders) != 0) rows_orders <- r$orders %>% dplyr::group_by(thesaurus_name, item_id) %>%
        dplyr::summarize(count_patients_rows = dplyr::n_distinct(patient_id)) %>% dplyr::ungroup()
      
      count_patients_rows <- rows_labs_vitals %>% dplyr::bind_rows(rows_text) %>% dplyr::bind_rows(rows_orders)
      
      count_patients_rows <- count_patients_rows %>% 
        dplyr::inner_join(r$thesaurus %>% dplyr::filter(id == thesaurus_id) %>% 
          dplyr::select(thesaurus_id = id, thesaurus_name = name), by = "thesaurus_name")
      
      if (nrow(count_patients_rows) != 0){
        count_patients_rows <- count_patients_rows %>% dplyr::select(-thesaurus_name, -thesaurus_id)
        data <- data %>% dplyr::left_join(count_patients_rows, by = "item_id") %>% dplyr::rename(value = count_patients_rows)
      }
      
      if (nrow(count_patients_rows) == 0) data <- data %>% dplyr::mutate(value = 0)
      
      # Set 0 when value is na
      # Convert value to character
      data <- data %>% dplyr::mutate_at("value", as.character) %>% dplyr::mutate(value = dplyr::case_when(is.na(value) ~ "0", T ~ value))
      
    }
    
    if (category == "delete"){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("sub_delete_", id), "", icon = icon("trash-alt"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-thesaurus_items_deleted_pressed', this.id, {priority: 'event'})")))))
    }
    if (category == "plus_minus"){
      data <- data %>% dplyr::rowwise() %>% dplyr::mutate(value = as.character(
        tagList(
          shiny::actionButton(paste0("select_", id), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_selected', this.id, {priority: 'event'})")),
          shiny::actionButton(paste0("remove_", id), "", icon = icon("minus"),
            onclick = paste0("Shiny.setInputValue('", module_id, "-item_removed', this.id, {priority: 'event'})")))))
    }

    # Delete old cache
    DBI::dbSendStatement(r$db, paste0("DELETE FROM cache WHERE category = '", category, "' AND link_id_bis = ", datamart_id)) -> query
    DBI::dbClearResult(query)

    # Get last row & insert new data
    last_row <- as.integer(DBI::dbGetQuery(r$db, "SELECT COALESCE(MAX(id), 0) FROM cache") %>% dplyr::pull())
    data_insert <-
      data %>%
      dplyr::transmute(
        category = !!category,
        link_id = id,
        link_id_bis = datamart_id,
        value,
        datetime = as.character(Sys.time()))
    data_insert$id <- seq.int(nrow(data_insert)) + last_row
    data_insert <- data_insert %>% dplyr::relocate(id)

    # Add data in cache table
    DBI::dbAppendTable(r$db, "cache", data_insert)
  }
  
  if (category %in% c("delete", "plus_minus")) data <- data %>% dplyr::rename(action = value)
  if (category %in% c("count_patients_rows", "count_items_rows")) data <- data %>% dplyr::rename(!!category := value) %>% dplyr::select(item_id, !!category)
  
  data
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
#' @param duplicates_allowed Are duplicates in the name column allowed (logical)
#' @param language Language used (character)
#' @examples 
#' \dontrun{
#' save_settings_datatable_updates(output = output, r = r, ns = ns, table = "datamarts", language = "EN")
#' }

save_settings_datatable_updates <- function(output, r = shiny::reactiveValues(), ns = shiny::NS(), table = character(),
  duplicates_allowed = FALSE, language = "EN"){
  
  # Make sure there's no duplicate in names, if duplicates_allowed is set to FALSE
  if (!duplicates_allowed){
    duplicates <- 0
    # Duplicates are allowed in thesaurus_items
    # if (table != "thesaurus_items"){
    duplicates <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
      dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    # }
    if (duplicates > 0) show_message_bar(output, 1, "modif_names_duplicates", "severeWarning", language)
    req(duplicates == 0)
  }
  
  # Save changes in database
  ids_to_del <- r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  DBI::dbSendStatement(r$db, paste0("DELETE FROM ", table, " WHERE id IN (", paste(ids_to_del, collapse = ","), ")")) -> query
  DBI::dbClearResult(query)
  
  # If action in columns, remove before insert into database (for thesaurus_items with cache system)
  # Same with count_items_rows (and count_patients_rows, always with count_items_rows)
  data <- r[[paste0(table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  if ("action" %in% names(data)) data <- data %>% dplyr::select(-action)
  if ("count_items_rows" %in% names(data)) data <- data %>% dplyr::select(-count_items_rows, -count_patients_rows)
  
  DBI::dbAppendTable(r$db, table, data)
  
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
  prefix <- ""
  if (table == "thesaurus_items") prefix <- "thesaurus_items_"
  
  dialogContentProps <- list(
    type = 0,
    title = translate(language, paste0(table, "_delete")),
    closeButtonAriaLabel = "Close",
    subText = translate(language, paste0(table, "_delete_subtext"))
  )
  shiny.fluent::Dialog(
    hidden = !r[[paste0(table, "_delete_dialog")]],
    onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", prefix, "hide_dialog', Math.random()); }")),
    dialogContentProps = dialogContentProps,
    modalProps = list(),
    shiny.fluent::DialogFooter(
      shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "delete_confirmed")), text = translate(language, "delete")),
      shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "delete_canceled")), text = translate(language, "dont_delete"))
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

delete_settings_datatable_row <- function(output, id = character(), r = shiny::reactiveValues(), ns = shiny::NS(), language = "EN",
  link_id = integer(), category = character(), row_deleted = integer(), table = character()){
  
  # Close dialog box
  r[[paste0(table, "_delete_dialog")]] <- FALSE
  
  # Delete row in database
  DBI::dbSendStatement(r$db, paste0("UPDATE ", table, " SET deleted = TRUE WHERE id = ", row_deleted))
  
  # Update r vars
  if (table == "thesaurus_items"){
    r$thesaurus_items <- create_datatable_cache(r = r, module_id = id, thesaurus_id = link_id, category = category)
    r$thesaurus_items_temp <- r$thesaurus_items %>% dplyr::mutate(modified = FALSE)
  }
  if (table != "thesaurus_items") update_r(r = r, table = table, language = language)
  
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
  
  if ("markdown_description" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "markdown_description") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", stringr::str_replace_all(data$markdown_description, "'", "''"), "' WHERE id = ", option_id)) -> query
    DBI::dbClearResult(query)
    update_r(r = r, table = "options", language = language)
  }
  
  if ("visibility" %in% page_options){
    option_id <- options %>% dplyr::filter(name == "visibility") %>% dplyr::pull(id)
    DBI::dbSendStatement(r$db, paste0("UPDATE options SET value = '", data$visibility, "' WHERE id = ", option_id)) -> query
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
  code_id <- r$code %>% dplyr::filter(category == !!category, link_id == !!link_id) %>% dplyr::pull(id) %>% as.integer()
  
  # Replace ' with '' and store in the database
  output$test <- renderText(paste0("code_id = ", code_id, " // link_id = ", link_id, " // edited_code = ", stringr::str_replace_all(edited_code, "'", "''"), 
                                   " // length = ", length(edited_code)))
  DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(edited_code, "'", "''"), "' WHERE id = ", code_id)) -> query
  # DBI::dbSendStatement(r$db, paste0("UPDATE code SET code = '", stringr::str_replace_all(edited_code, "'", "''"), "' WHERE id = ", code_id)) -> query
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
#' @description Execute code entered in the ShinyAce editor\cr
#' For plugins page, UI & server code are displayed in distinct outputs (uiOutput for UI code, textOutput for server code to display error messages)
#' @param input variable from Shiny, used to execute UI & server code (plugins page)
#' @param output variable from Shiny, used to render messages on the message bar
#' @param session variable from Shiny, used to execute UI & server code (plugins page)
#' @param id ID of the current page / module
#' @param ns Shiny namespace
#' @param language language used (character)
#' @param r The "petit r" object, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param edited_code New code, after editing it (character)
#' @param code_type For plugins page, code_type could be UI or server (character)
#' @param data A list containing dataframes / tibbles, if data need to be used in the evaluated code (list)
#' @examples 
#' \dontrun{
#' execute_settings_code(output = output, r = r, edited_code = "print('test')")
#' }

execute_settings_code <- function(input, output, session, id = character(), ns = shiny::NS(), language = "EN", r = shiny::reactiveValues(), 
  edited_code = character(), code_type = "", data = list()){

  result <- ""
  
  # If code is UI, execute it only
  if (code_type == "ui"){
    
    tryCatch(eval(parse(text = edited_code)), error = function(e) stop(e), warning = function(w) stop(w))
    result <- eval(parse(text = edited_code))
  }
  
  # If code is server, capture the console output
  # Server is also used by R_console page
  if (code_type == "server"){
    
    # Change this option to display correctly tibble in textbox
    eval(parse(text = "options('cli.num_colors' = 1)"))
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = edited_code)), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    eval(parse(text = "options('cli.num_colors' = NULL)"))
    
    # Display result
    paste(captured_output, collapse = "\n") -> result
  }
  
  # If code is not UI or server, capture the console output after replacing %% values
  if (code_type %not_in% c("ui", "server")){
    
    # Replace %CODE% from code to real values
    code <- edited_code %>%
      stringr::str_replace_all("%datamart_id%", as.character(isolate(r$datamart_id))) %>%
      stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
      stringr::str_replace_all("%thesaurus_id%", as.character(isolate(r$thesaurus_id)))
    
    # Change this option to display correctly tibble in textbox
    eval(parse(text = "options('cli.num_colors' = 1)"))
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = as.character(code))), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    eval(parse(text = "options('cli.num_colors' = NULL)"))
    
    # Display result
    paste(captured_output, collapse = "\n") -> result
  }
  
  result
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
