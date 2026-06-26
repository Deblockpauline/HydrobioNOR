#' Module UI du sélecteur de qualification
#' @param id Identifiant du module
#' @noRd

mod_selecteur_qualification_ui <- function(id) { # Fonction UI module
  ns <- shiny::NS(id) # Namespace module
  shiny::tagList( # Regroupe éléments UI
    shiny::selectizeInput( # Liste déroulante
      inputId = ns("qualification"), # ID input
      label = "Qualification", # Texte affiché
      choices = NULL, # Choix ajoutés côté serveur
      selected = "Toutes", # Valeur sélectionnée par défaut
      multiple = TRUE ) ) } # Sélection multiple

#' Module server du sélecteur de qualification
#' @noRd

mod_selecteur_qualification_server <- function(id, # ID module
                                               donnees) {

  shiny::moduleServer(id, function(input, output, session) { # Début serveur module
    shiny::observe({ # Observe les données
      shiny::req(donnees()) # Attend les données
      shiny::req("donnee_carte_taxon" %in% names(donnees())) # Vérifie table présente

      df <- donnees()$donnee_carte_taxon # Table utilisée
      choix_qualification <- df |> # Table qualifications
        dplyr::pull(libelle_qualification) |> # Extraction colonne
        unique() |> # Supprime doublons
        sort() # Trie alphabétique

      choix_qualification <- choix_qualification[ # Nettoyage valeurs
        !is.na(choix_qualification) & # Supprime NA
          choix_qualification != "" ] # Supprime chaînes vides

      # Texte affiché dans la liste pour "incorrecte"
      choix_qualification_affichage <- choix_qualification # Copie vecteur
      names(choix_qualification_affichage) <- dplyr::case_when( # Texte affiché
        choix_qualification == "incorrecte" ~ # Cas qualification incorrecte
          "incorrecte (pas dispo pour le sous-onglet Qualité et indices)", # Texte affiché
        TRUE ~ choix_qualification )# Sinon texte normal

      shiny::updateSelectizeInput( # Mise à jour liste déroulante
        session = session, # Session shiny
        inputId = "qualification", # ID input
        choices = c( # Liste des choix
          "Toutes" = "Toutes", # Valeur par défaut
          choix_qualification_affichage) , # Qualifications disponibles
        selected = "Toutes", # Valeur sélectionnée
        server = TRUE ) } )# Chargement côté serveu

    return( # Retour reactive
      shiny::reactive(input$qualification) ) # Qualification choisie
  } ) }

## À appeler dans l'UI
# mod_selecteur_qualification_ui("qualification")

## À appeler dans le server
# mod_selecteur_qualification_server("qualification")
