#' Module de chargement des données
#' @description
#' Module Shiny qui télécharge le fichier `data_hydrobioNOR.rda`
#' depuis le dépôt GitHub, charge les objets dans un environnement temporaire
#' et renvoie une liste de données.
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
    donnees <- shiny::reactive({ # Reactive qui télécharge et charge les données
      fichier_temp <- tempfile(fileext = ".rda")  # Création d'un fichier temporaire pour stocker le .rda
      url_data <- "https://raw.githubusercontent.com/Deblockpauline/HydrobioNOR/main/dev/data_hydrobioNOR.rda"
      utils::download.file(   # Téléchargement du fichier
        url = url_data,
        destfile = fichier_temp,
        mode = "wb")

      env <- new.env() # Chargement dans un environnement temporaire
      load(fichier_temp, envir = env)
      unlink(fichier_temp) # Suppression du fichier temporaire
      as.list(env) } )  # Conversion en liste pour faciliter l'utilisation dans l'app

    output$date <- shiny::renderText({ # Affichage de la date de mise à jour des données
      shiny::req(donnees())

      if ("date_donnees" %in% names(donnees())) {
        paste(
          "Date de mise à jour des données :",
          as.character(donnees()$date_donnees))
      } else {"Date non disponible"}
    } )
    return(donnees) } ) } # Renvoie les données pour les autres modules
