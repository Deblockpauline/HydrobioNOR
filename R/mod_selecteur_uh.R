#' Module UI du sélecteur d'unité hydrographique
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_UH_ui <- function(id) {
  ns <- shiny::NS(id) # Permet de créer un namespace pour éviter les conflits d'ID
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("uh"), # ID du champ
      label = "Unité hydrographique", # Texte affiché
      choices = NULL, # Les choix seront définis côté serveur
      selected = "Toutes") ) } # Valeur par défaut

#' Module server du sélecteur d'unité hydrographique
#'
#' @param id Identifiant du module
#' @param stations Reactive contenant la table des stations
#' @return Une reactive contenant l'unité hydrographique sélectionnée
#' @noRd
library(dplyr)

mod_selecteur_UH_server <- function(id, stations, choix_departements) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({ # Bloc réactif exécuté dès que stations ou département change
      df <- stations() # Table des stations

      # Filtre selon le département sélectionné
      if (!is.null(choix_departements()) &&
          length(choix_departements()) > 0 &&
          !("Tous" %in% choix_departements()) &&
          "code_dep" %in% names(df)) {
        df <- dplyr::filter(
          df,
          code_dep %in% choix_departements() ) }

      # Extraction des UH uniques depuis la table filtrée
      choix_uh <- df |>
        sf::st_drop_geometry() |> # Supprime la géométrie si objet sf
        dplyr::pull(UH_calculee) |> # Récupère la colonne UH
        unique() |> # Garde uniquement les valeurs uniques
        sort() # Trie les valeurs
      choix_uh <- choix_uh[!is.na(choix_uh) & choix_uh != ""] # Nettoyage des valeurs

      # Mise à jour dynamique du menu déroulant
      shiny::updateSelectInput(
        session = session,
        inputId = "uh",
        choices = c("Toutes", choix_uh), # UH du département choisi
        selected = "Toutes") # Remet à Toutes
    } )

    return(shiny::reactive(input$uh) ) # Réactive retrounée contennant l'UH selectionnée
   } ) }

## À appeler dans l'UI
# mod_selecteur_UH_ui ("uh")

## À appeler dans le server
# mod_selecteur_UH_server("uh")
