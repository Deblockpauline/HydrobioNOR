#' Module UI du sélecteur de réseau
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_reseau_ui <- function(id) {
  ns <- shiny::NS(id) # Namespace module
  shiny::tagList( # Regroupe éléments UI
    shiny::selectizeInput( # Liste déroulante
      inputId = ns("reseau"), # ID input
      label = "Réseau", # Texte affiché
      choices = NULL, # Choix ajoutés côté serveur
      selected = "Tous", # Valeur par défaut
      multiple = TRUE ) ) } # Sélection multiple


#' Module server du sélecteur de réseau
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données
#' @noRd

mod_selecteur_reseau_server <- function(id, # ID module
                                        donnees) {

  shiny::moduleServer(id, function(input, output, session) { # Début serveur module
    shiny::observe({ # Observe les données
      shiny::req(donnees()) # Attend les données
      shiny::req("donnee_carte" %in% names(donnees())) # Vérifie table présente

      df <- donnees()$donnee_carte # Table carte
      choix_reseau <- df |> # Table réseaux
        dplyr::pull(reseau) |> # Extraction colonne réseau
        stringr::str_split("[-/]") |> # Découpe valeurs multiples
        unlist() |> # Transforme liste en vecteur
        stringr::str_trim() |> # Supprime espaces
        unique() |> # Supprime doublons
        sort() # Trie alphabétique

      choix_reseau <- choix_reseau[ # Nettoyage valeurs
        !is.na(choix_reseau) & # Supprime NA
          choix_reseau != "" ] # Supprime chaînes vides

      shiny::updateSelectizeInput( # Mise à jour liste déroulante
        session = session, # Session shiny
        inputId = "reseau", # ID input
        choices = c( # Liste des choix
          "Tous", # Valeur par défaut
          choix_reseau ), # Réseaux disponibles
        selected = "Tous", # Valeur sélectionnée
        server = TRUE ) } ) # Chargement côté serveur

    return( # Retour reactive
      shiny::reactive(input$reseau) ) # Réseau sélectionné
  } ) }

## À appeler dans l'UI
# mod_selecteur_reseau_ui("reseau")

## À appeler dans le server
# mod_selecteur_reseau_server("reseau")
