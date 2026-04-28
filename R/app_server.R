#' Application Server
#'
#' @description Fonction serveur principale de l'application Shiny.
#' Elle coordonne les différents modules serveur : chargement des données, gestion des filtres,
#' affichage de la carte des stations, affichage des onglets
#' Le server fait fonctionner les differents modules
#' @noRd

app_server <- function(input, output, session) {

  # Reactive = si les données changent -> mise a jour automatique
  # Chargement des données
  donnees <- mod_load_data_server("donnees") # Renvoie la reactive donnees() avec les differentes tables

  # Sélecteur département
  choix_departements <- mod_selecteur_dep_server(
    id = "departements", # Utilise les données chargées pour proposer les départements
    donnees = donnees) # Renvoie le ou les départements choisis par l'utilisateur

  # Sélecteur compartiment biologique
  choix_eqb <- mod_selecteur_eqb_server(
    id = "eqb") # Renvoie le compartiment biologique choisi

  # Sélecteur UH
  choix_uh <- mod_selecteur_UH_server(
    id = "uh",
    stations = shiny::reactive(donnees()$stations) ) # On transmet uniquement la table stations car la variable UH s'y trouve


  # Carte des stations dans l'onglet Station
  station_selectionnee <- mod_station_carte_server(
    id = "station_carte",
    donnees = donnees, # On récupère les données de l'application
    choix_departements = choix_departements, # On applique les filtres sélectionnés
    choix_eqb = choix_eqb,
    choix_uh = choix_uh) # Renvoie la station sélectionnée par clic sur la carte ou dans la liste

  # Informations générales de la station sélectionnée
  mod_station_infos_server(
    id = "station_infos",
    donnees = donnees,
    station_selectionnee = station_selectionnee) # Affiche les informations de la station choisie

  # Contexte environnemental de la station sélectionnée
  mod_station_contexte_env_server(
    id = "station_contexte_env",
    donnees = donnees,
    station_selectionnee = station_selectionnee )

  # Contexte environnemental du bassin versant associé à la station sélectionnée
  mod_station_contexte_env_bv_server(
    id = "station_contexte_env_bv",
    donnees = donnees,
    station_selectionnee = station_selectionnee)

  # Carte des stations dans l'onglet Communautés
  station_selectionnee_commu <- mod_station_carte_server(
    id = "station_carte_commu",
    donnees = donnees, # On récupère les mêmes données
    choix_departements = choix_departements, # On applique les mêmes filtres globaux
    choix_eqb = choix_eqb,
    choix_uh = choix_uh) # Renvoie la station sélectionnée dans l'onglet Communautés

  # Afficher le graph de la repartition des stations selon la classe/le cycle DCE et le compartiemnt bio
  mod_hist_qualite_server(
    id = "hist_qualite_commu",
    donnees = donnees,
    choix_departements = choix_departements,
    choix_eqb = choix_eqb,
    choix_uh = choix_uh)
}
