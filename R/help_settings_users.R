help_settings_users <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_users_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("users_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("users_accesses"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("users_statuses"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_users_open_panel_light_dismiss,
      isBlocking = r$help_settings_users_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_users_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_users_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_users_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_users_open_modal <- TRUE
    r$help_settings_users_open_panel_light_dismiss <- FALSE
  }
  
  # Users management
  
  observeEvent(r$help_settings_users_page_1, {
    
    load_help_page(r)
    
    r$help_settings_users_modal_title <- i18n$t("users_management")
    
    if (language == "fr"){
      r$help_settings_users_modal_text <- div(
        p(strong("1) Créer un utilisateur")),
        p("Pour créer un utilisateur, vous devez renseigner les champs suivants :"),
        tags$ul(
          tags$li(tags$em("Pseudo"), " : c'est le login utilisé pour la connexion, il doit être unique. Il ne sera pas visible par les autres utilisateurs."),
          tags$li(tags$em("Prénom et Nom"), " : ces informations seront visibles par les autres utilisateurs"),
          tags$li(tags$em("Mot de passe"), " : le mot de passe sera crypté avant d'être stocké dans la base de données"),
          tags$li(tags$em("Accès utilisateur"), " : cela déterminera les droits associés au compte (voir la section ", tags$em("Accès utilisateurs"), ")"),
          tags$li(tags$em("Statut de l'utilisateur"), " : cette information sera visible par les autres utilisateurs, afin de mieux identifier le profil (data scientist, clinicien etc)")
        ),
        p("Cliquez ensuite sur ", tags$em("Ajouter"), "."),
        p("L'ensemble de ces données sera ", strong("modifiable"), " par la suite."),
        p(strong("2) Gérer les utilisateurs")),
        p("Vous pouvez ici modifier les informations concernant les utilisateurs depuis l'onglet ", tags$em("Gérer les utilisateurs"), "."),
        p("Pour modifier le pseudo, le prénom ou le nom, double-cliquez sur la ligne et la colonne correspondants, puis modifiez l'information."),
        p("Vous pouvez modifier l'accès et le statut de l'utilisateur via les menus déroulants."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer un ou plusieurs utilisateurs, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un utilisateur en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_users_modal_text <- div(
        p(strong("1) Create a user")),
        p("To create a user, you must fill in the following fields:"),
        tags$ul(
          tags$li(tags$em("Username"), " : this is the login used for connection, it must be unique. It will not be visible to other users."),
          tags$li(tags$em("First name and Last name"), " : this information will be visible to other users"),
          tags$li(tags$em("Password"), " : the password will be encrypted before being stored in the database"),
          tags$li(tags$em("User access"), " : this determines the rights associated with the account (see the section", tags$em(" Users Access"), ")"),
          tags$li(tags$em("User status"), " : this information will be visible to other users, in order to better identify the profile (data scientist, clinician etc.)")
        ),
        p("Then click on", tags$em(" Add"), "."),
        p("All of this data will be ", strong("modifiable"), " later."),
        p(strong("2) Manage users")),
        p("Here you can modify users informations from the", tags$em(" Manage Users"), " tab."),
        p("To modify the username, first or last name, double-click on the corresponding row and column, then modify the information."),
        p("You can modify the access and status of the user via the dropdown menus."),
        p("Once the information has been modified, click on", tags$em(" Save"), "."),
        p("To delete one or more users, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a user by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon.")
      )
    }
  })
  
  # Users accesses
  
  observeEvent(r$help_settings_users_page_2, {
    
    load_help_page(r)
    
    r$help_settings_users_modal_title <- i18n$t("users_accesses")
    
    if (language == "fr"){
      r$help_settings_users_modal_text <- div(
        p(strong("1) Créer un accès utilisateur")),
        p("Vous pouvez créer un nouvel accès utilisateur en renseignant au moins le champ ", tags$em("Nom"), "."),
        p(strong("2) Gérer les accès utilisateurs")),
        p("Vous pouvez modifier le nom et la description des accès en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Cliquez sur ", shiny::actionButton("edit_user_acces_code_button_help", "", icon = icon("cog")), " pour ", strong("configurer les droits"), " de l'accès utilisateur."),
        p("Pour supprimer un ou plusieurs accès utilisateurs, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un accès utilisateur en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        p(strong("3) Configurer les droits des utilisateurs")),
        p("Allez dans l'onglet ", tags$em("Configurer les acès"), " puis choisissez un accès utilisateur dans le menu déroulant."),
        p("Les droits des utilisateurs sont classés par catégories."),
        p(strong("Cochez les droits"), " que vous voulez pour l'accès utilisateur sélectionné puis cliquez sur ", tags$em("Sauvegarder"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_users_modal_text <- div(
        r$help_settings_users_modal_text <- div(
          p(strong("1) Create a user access")),
          p("You can create a new user access by filling in at least the ", tags$em("Name"), " field."),
          p(strong("2) Manage users accesses")),
          p("You can modify the name and description of the accesses by double-clicking on the corresponding row and column in the table."),
          p("Click on ", shiny::actionButton("edit_user_acces_code_button_help", "", icon = icon("cog")), " to ", strong("configure the rights"), " of the user access."),
          p("To delete one or more users accesses, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
          p("You can also delete a user access by clicking on the icon ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
          p(strong("3) Configure users rights")),
          p("Go to the ", tags$em("Configure users accesses"), " tab and choose a user access from the dropdown menu."),
          p("Users rights are classified by categories."),
          p(strong("Check the rights"), " you want for the selected user access, then click on ", tags$em("Save"), "."),
          br()
        )
      )
    }
  })
  
  # Users statuses
  
  observeEvent(r$help_settings_users_page_3, {
    
    load_help_page(r)
    
    r$help_settings_users_modal_title <- i18n$t("users_statuses")
    
    if (language == "fr"){
      r$help_settings_users_modal_text <- div(
        p(strong("1) Créer un statut")),
        p("Vous pouvez créer un nouveau statut en renseignant au moins le champ ", tags$em("Nom"), "."),
        p(strong("2) Gérer les statuts")),
        p("Vous pouvez modifier le nom et la description des statuts des utilisateurs en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer un ou plusieurs statuts, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un statut en cliquant sur l'icône ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_users_modal_text <- div(
        p(strong("1) Create a status")),
        p("You can create a new status by filling in at least the ", tags$em("Name"), " field."),
        p(strong("2) Manage statuses")),
        p("You can modify the name and description of user statuses by double-clicking on the corresponding row and column in the table."),
        p("Once the information has been modified, click on ", tags$em("Save"), "."),
        p("To delete one or more statuses, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a status by clicking on the ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " icon."),
        br()
      )
    }
  })
  
}