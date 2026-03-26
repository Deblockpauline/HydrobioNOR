#' Module de chargement des données

#' @description
#' Module Shiny qui télécharge le fichier `data_hydrobioNOR.rda`
#' depuis le dépôt GitHub, charge les objets contenus dans ce fichier,
#' puis affiche la date de mise à jour des données.
#'
#' @param id Identifiant du module Shiny.
#'
#' @return La date de mise à jour des données sous forme de texte
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput moduleServer renderText

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("date")))}

#' Module serveur de chargement des données
#'
#' @param id Identifiant du module Shiny.
#'
#' @noRd

mod_load_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Création d'un fichier temporaire pour stocker le .rda téléchargé
    fichier_temp <- tempfile(fileext = ".rda")

    # URL du fichier de données hébergé sur GitHub
    url_data <- "https://raw.githubusercontent.com/Deblockpauline/HydrobioNOR/main/dev/data_hydrobioNOR.rda"

    # Téléchargement du fichier .rda depuis GitHub
    download.file(
      url = url_data,
      destfile = fichier_temp,
      mode = "wb")

    # Chargement des objets contenus dans le .rda dans l'environnement global
    load(fichier_temp, envir = .GlobalEnv)

    # Suppression du fichier temporaire après chargement
    unlink(fichier_temp)

    # Affichage de la date de mise à jour si l'objet date_donnees existe
    output$date <- renderText({
      if (exists("date_donnees", envir = .GlobalEnv)) {
        paste(
          "Date de mise à jour des données :",
          as.character(get("date_donnees", envir = .GlobalEnv)))
        } else {
        "Date de mise à jour non disponible" }
    })
  })
}

# À appeler dans l'UI --> mod_load_data_ui("load_data_1")
# À appeler dans le server --> mod_load_data_server("load_data_1")
