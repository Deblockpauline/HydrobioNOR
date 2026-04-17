#' Application Server
#'
#' @description Fonction serveur principale de l'application Shiny.
#' Elle coordonne les différents modules serveur : chargement des données, gestion des filtres,
#' affichage de la carte des stations, affichage des informations de la station sélectionnée
#' Le server fait fonctionner les differents module
#' @noRd

app_server <- function(input, output, session) {
# Reactive = si les données changent -> MAJ automatique
  # Chargement des données
  donnees <- mod_load_data_server("donnees") # Renvoie la reactive donnees() avec les differentes tables

  # Sélecteur département
  choix_departements <- mod_selecteur_dep_server(
    id = "departements",  # Il utilise les données chargées pour proposer les départements
    donnees = donnees ) # Et renvoie le départements choisis par l'utilisateur

  # Sélecteur compartiment bio et renvoie le choix
  choix_eqb <- mod_selecteur_eqb_server( id = "eqb" )

  # Sélecteur UH et renvoie le choix
  choix_uh <- mod_selecteur_UH_server(
    id = "uh",
    stations = shiny::reactive(donnees()$stations) ) # On lui transmet seulement stations (car UH se trouve dedans) sous forme de réactive

  # Carte des stations
  station_selectionnee <- mod_station_carte_server(
    id = "station_carte",
    donnees = donnees, # On recupere les données et on applique les filtres
    choix_departements = choix_departements, # Cela permet de renvoyer la station selectionnee
    choix_eqb = choix_eqb,
    choix_uh = choix_uh )

  # Informations générales de la station
  mod_station_infos_server(
    id = "station_infos",
    donnees = donnees,
    station_selectionnee = station_selectionnee ) # Affiche les informations contenue dans donnees selon la selection

  # Contexte environnemental de la station selectionnée
  mod_station_contexte_env_server(
    id = "station_contexte_env",
    donnees = donnees,
    station_selectionnee = station_selectionnee )

  #   Contexte environnemental du BV
  mod_station_contexte_env_bv_server(
    id = "station_contexte_env_bv",
    donnees = donnees,
    station_selectionnee = station_selectionnee)
  }
