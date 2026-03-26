# load_data UI Function
# @description Module Shiny permettant de charger les données hydrobiologiques
# depuis un fichier distant et d'afficher la date de mise à jour.
#
# @param id Internal parameter for {shiny}.
#
# @return Un tagList contenant la date de mise à jour des données.
# @noRd
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    strong("Date de mise à jour des données :"),
    textOutput(ns("date"))
  )
}

#' load_data Server Function
#'
#' @description Télécharge le fichier data_hydrobio.rda depuis GitHub,
#' charge les objets qu'il contient dans un environnement temporaire,
#' puis renvoie les données utiles à l'application sous forme réactive.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @return Une liste réactive contenant au minimum :
#' \itemize{
#'   \item stations
#'   \item date_donnees
#' }
#' @noRd
mod_load_data_server <- function(id){
  moduleServer(id, function(input, output, session){

    # chemin vers le fichier
    fichier <- "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/dev/data_hydrobioNOR.rda"

    # charger les données
    e <- new.env()
    load(fichier, envir = e)

    # afficher la date
    output$date <- renderText({
      paste("Mise à jour le", format(e$date_donnees, "%d/%m/%Y"))
    })

    # retourner les données
    reactive({
      list(
        stations = e$stations,
        date_donnees = e$date_donnees
      )
    })

  })
}
