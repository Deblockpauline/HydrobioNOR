#' Module de chargement des données
#' @description Module Shiny qui télécharge le fichier `data_hydrobioNOR.rda`
#' depuis le dépôt GitHub, charge les objets dans un environnement temporaire
#' et renvoie une liste de données.C'est inspiré de IDF mais
#' la seule difference c'est que ca renvoie une réactive (donnees$table)
#' @return Une reactive contenant la liste des objets chargés
#' @noRd

mod_load_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList( shiny::textOutput(ns("date") ) ) }  # Affichage de la date de mise à jour des données

#' Module serveur de chargement des données
#'
#' @param id Identifiant du module Shiny
#'
#' @return Une reactive contenant la liste des objets chargés
#' @noRd

mod_load_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    donnees <- shiny::reactiveVal(NULL)
    shiny::observeEvent(TRUE, {
      donnees(get_data_hydrobioNOR())  # <- va chercher le cache, télécharge seulement si vide
    }, once = TRUE)

    output$date <- shiny::renderText({
      shiny::req(donnees())
      if ("date_donnees" %in% names(donnees())) {
        paste("Date de mise à jour des données :",
              as.character(donnees()$date_donnees))
      } else { "Date non disponible" }
    })
    return(donnees)
  })
}


## À appeler dans l'UI
# mod_load_data_ui("donnees")

## À appeler dans le server
# mod_load_data_server("donnees")
