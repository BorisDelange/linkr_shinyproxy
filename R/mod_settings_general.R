#' settings_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_general_ui <- function(id = character(), language = character(), words = tibble::tibble){
  ns <- NS(id)
  div(class = "main",
      
    # Hidden aceEditor, allows the other to be displayed...
    div(shinyAce::aceEditor("hidden"), style = "display: none;"),
    render_settings_default_elements(ns = ns),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "general_settings", text = translate(language, "general_settings", words))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "change_password_card", itemKey = "change_password", headerText = translate(language, "change_password", words))
    ),
    # render_settings_toggle_card(language = language, ns = ns, cards = list(
    #   list(key = "change_password_card", label = "change_password")), words = words),
    div(id = ns("change_password_card"),
      make_card(translate(language, "change_password", words),
        div(
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
            make_textfield(language = language, ns = ns, label = "old_password", type = "password", canRevealPassword = TRUE, width = "300px", words = words),
            make_textfield(language = language, ns = ns, label = "new_password", type = "password", canRevealPassword = TRUE, width = "300px", words = words),
            make_textfield(language = language, ns = ns, label = "new_password", id = "new_password_bis",
              type = "password", canRevealPassword = TRUE, width = "300px", words = words)
          ), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("save"), translate(language, "save", words))
        )
      )
    )
  )
}
    
#' settings_general Server Functions
#'
#' @noRd 

mod_settings_general_server <- function(id = character(), r = shiny::reactiveValues(), language = "EN", words = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    ##########################################
    # Data management / Show or hide cards   #
    ##########################################
    
    
    cards <- c("change_password_card")
    
    show_hide_cards_new(r = r, input = input, session = session, id = id, cards = cards)
    
    # Toggles IDs
    # toggles <- c("change_password_card")
    # 
    # show_hide_cards(r = r, input = input, session = session, id = id, toggles = toggles)
    
    ##########################################
    # Change password                        #
    ##########################################
    
    observeEvent(input$save, {
      
      # Check if textfields are not empty
      
      required_textfields <- c("old_password", "new_password", "new_password_bis")
      
      sapply(required_textfields, function(textfield){
        
        if (length(input[[textfield]]) == 0) shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = translate(language, "provide_valid_password", words))
        
        else {
          if (is.na(input[[textfield]])) shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = translate(language, "provide_valid_password", words))
          else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
        }
          
        req(length(input[[textfield]]) > 0)
        req(!is.na(input[[textfield]]))
      })
      
      # Check if the two new password fields contains the same value
      
      if (input$new_password != input$new_password_bis){
        shiny.fluent::updateTextField.shinyInput(session, "new_password", errorMessage = translate(language, "passwords_are_different", words))
        shiny.fluent::updateTextField.shinyInput(session, "new_password_bis", errorMessage = translate(language, "passwords_are_different", words))
      }
      else {
        shiny.fluent::updateTextField.shinyInput(session, "new_password", errorMessage = NULL)
        shiny.fluent::updateTextField.shinyInput(session, "new_password_bis", errorMessage = NULL)
      }
      
      req(input$new_password == input$new_password_bis)
      
      # Check if the old password is OK
      
      old_password <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM users WHERE id = ", r$user_id)) %>% dplyr::pull(password)
      
      if (as.character(rlang::hash(input$old_password)) != old_password) shiny.fluent::updateTextField.shinyInput(session, "old_password", 
        errorMessage = translate(language, "invalid_old_password", words))
      
      if (as.character(rlang::hash(input$old_password)) == old_password) {
        
        # Everything is OK, change password
        
        new_password <- as.character(rlang::hash(input$new_password))
        sql <- glue::glue_sql("UPDATE users SET password = {new_password} WHERE id = {r$user_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        # Notificate the user
        
        show_message_bar(output = output, id = 1, message = "password_changed", type = "success", language = language)
        
      }
      
    })
    
  })
}