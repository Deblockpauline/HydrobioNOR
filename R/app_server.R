#'#' Application Server
#'
#' @noRd

app_server <- function(input, output, session) {
  # Chargement des données
  donnees <- mod_load_data_server("donnees")

  # Sélecteur département
  choix_departements <- mod_selecteur_dep_server(
    id = "departements",
    donnees = donnees)

  # Sélecteur compartiment bio
  choix_eqb <- mod_selecteur_eqb_server(id = "eqb")

  # Sélecteur UH
  choix_uh <- mod_selecteur_UH_server(
    id = "uh",
    stations = shiny::reactive(donnees()$stations) )

  # Carte des stations
  station_selectionnee <- mod_station_carte_server(
    id = "station_carte",
    donnees = donnees,
    choix_departements = choix_departements,
    choix_eqb = choix_eqb,
    choix_uh = choix_uh )

  # Infos station
  mod_station_infos_server(
    id = "station_infos",
    donnees = donnees,
    station_selectionnee = station_selectionnee) }
