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
  # Toggle card                            #
  ##########################################
  
  settings_toggle_card <- function(language, ns, description_card = "", creation_card = "", datatable_card = "", 
                                   edit_code_card = "", options_card = "", activated = ""){
    toggles <- tagList()
    sapply(c("description_card", "creation_card", "datatable_card", "edit_code_card", "options_card"), function(card){
      label <- eval(parse(text = card))
      if (label != "") toggles <<- 
        tagList(toggles, make_toggle(language, ns, label = label, 
        id = paste0(card, "_toggle"), value = ifelse(card %in% activated, TRUE, FALSE), inline = TRUE))
    })
    make_card("",
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10), toggles
      )
    )
  }

  ##########################################
  # Edit code card                         #
  ##########################################
  
  settings_edit_code_card <- function(language, ns, type = "code", code, link_id, title){
    div(id = ns("edit_code_card"),
      make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
        div(
          div(shinyAce::aceEditor(ns("ace_edit_code"), code, mode = "r", height = "400px"), style = "width: 100%;"),
          shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), translate(language, "save")), " ",
          shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), 
          htmltools::br(), htmltools::br(),
          div(shiny::verbatimTextOutput(ns("code_result")), 
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
        )
      )
    )
  }

##########################################
# SERVER FUNCTIONS                       #
##########################################

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
  
  # ===== settings_datatable function =====
  # id = id of the module (eg : settings_datamarts)
  # name = name of the page or element of the page (eg : datamarts for datamarts page, users_accesses for subpage accesses of user page)
  # data = data used in datatable
  # data_variables = variables r$... containing data (eg : r$datamarts)
  # dropdowns = dropdowns shown in the datatable
  # action_buttons = actions buttons created (eg : "delete", "edit_code", "options")
  # subpages = if subpages TRUE, change names of actionButtons (possibly two delete buttons in the same module)
  
  settings_datatable <- function(ns, r, id, name, data, data_variables, dropdowns = NULL, action_buttons = "", new_colnames = "", subpages = FALSE){
    if (nrow(data) == 0) return(data)
    
    # Create vars with existing options (ie : for data_sources, a list of existing data_sources in the database)
    sapply(data_variables, function(data_var){
      assign(data_var, tibble_to_list(r[[data_var]], "id", "name", rm_deleted_rows = TRUE), envir = parent.env(environment()))
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
          data[i, name] <<- as.character(
            div(
              shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[name], data[i, "id"])),
                options = eval(parse(text = dropdowns[name])), value = as.integer(data[i, name])), style = "width:100%")
          )
        })
        
        # Action buttons : if in action_buttons vector, add action button
        actions <- tagList()
        
        # Add delete button
        if ("delete" %in% action_buttons){
          # input_name and not input_prefix, cause we are using r$ values for delete button reactivity
          if (subpages) input_name <- paste0(name, "_delete", data[i, 1]) else input_name <- paste0("delete", data[i, 1])
          actions <- tagList(actions, shiny::actionButton(input_name, "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-", name, "_deleted_pressed', this.id, {priority: 'event'})")))}
        
        # Add options button
        if ("options" %in% action_buttons){
          if (subpages) input_prefix <- paste0(name, "_options") else input_prefix <- "options"
          actions <- tagList(actions, shiny::actionButton(paste0(input_prefix, data[i, 1]), "", icon = icon("cog"),
          onclick = paste0("Shiny.setInputValue('", id, "-", input_prefix, "', this.id, {priority: 'event'})")), "")}
        
        # Add edit code button
        if ("edit_code" %in% action_buttons){
          if (subpages) input_prefix <- paste0(name, "_edit_code") else input_prefix <- "edit_code"
          actions <- tagList(actions, shiny::actionButton(paste0(input_prefix, data[i, 1]), "", icon = icon("file-code"),
          onclick = paste0("Shiny.setInputValue('", id, "-", input_prefix, "', this.id, {priority: 'event'})")), "")}
        
        # Update action column in dataframe
        data[i, "action"] <- as.character(div(actions))
        
        # Get creator name
        data[i, "creator_id"] <- r$users %>% dplyr::filter(id == data[[i, "creator_id"]]) %>% 
          dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator)
      }
    }
    
    # Change name of cols 
    colnames(data) <- new_colnames
    
    data
  }