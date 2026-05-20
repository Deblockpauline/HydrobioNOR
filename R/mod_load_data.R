#' Module de chargement des données
#' @description
#' Module Shiny qui télécharge le fichier `data_hydrobioNOR.rda`
#' depuis le dépôt GitHub, charge les objets dans un environnement temporaire
#' et renvoie une liste de données.C'est inspiré de IDF mais
#' la seule difference c'est que ca renvoie une réactive (donnees$table)
#'
#' @param id Identifiant du module Shiny
#'
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
    donnees <- shiny::reactiveVal(NULL) # Stocke les données chargées
    shiny::observeEvent(TRUE, { # Se lance une seule fois
      fichier_temp <- tempfile(fileext = ".rda") # Fichier temporaire
      url_data <- "https://raw.githubusercontent.com/Deblockpauline/HydrobioNOR/main/dev/data_hydrobioNOR.rda"
      options(timeout = 300) # Temps max
      utils::download.file(
        url = url_data,
        destfile = fichier_temp,
        mode = "wb" )
      env <- new.env() # Environnement temporaire
      load(fichier_temp, envir = env) # Charge le .rda
      unlink(fichier_temp) # Supprime le temporaire
      donnees(as.list(env)) # Met les données dans reactiveVal
    }, once = TRUE) # Evite le rechargement

    output$date <- shiny::renderText({ # Affichage de la date de mise à jour des données
      shiny::req(donnees())
      if ("date_donnees" %in% names(donnees())) {
        paste( "Date de mise à jour des données :",
          as.character(donnees()$date_donnees))
      } else { "Date non disponible"} } )
    return(donnees) } )
}

## À appeler dans l'UI
# mod_load_data_ui("donnees")

## À appeler dans le server
# mod_load_data_server("donnees")
