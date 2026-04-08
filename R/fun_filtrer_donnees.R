#' Fonctions de filtrage des données
#' @description
#' Fonctions utilitaires pour filtrer les données de l'application
#' selon les filtres globaux ou la station sélectionnée.
#' @noRd

#' Filtrer les stations selon les filtres globaux
#'
#' @description
#' Cette fonction filtre la table des stations selon :
#' - le département sélectionné
#' - l'unité hydrographique sélectionnée
#' Le filtre EQB est géré directement dans le module de carte à partir d'une table annexe
#'
#' @param station Table des stations
#' @param choix_departement Département sélectionné
#' @param choix_uh Unité hydrographique sélectionnée
#'
#' @return Une table des stations filtrées
#' @export

filtrer_stations <- function(station,
                             choix_departement = "Tous",
                             choix_uh = "Toutes") {
  if (is.null(station)) {return(NULL)} # Sécurité : si la table station est NULL, on retourne NULL
  station_filtree <- station # Initialisation : on travaille sur une copie de la table

  # Filtre département
  # Vérifie que : un choix existe et que la colonne existe dans la table
  if (!is.null(choix_departement) && # && = "ET" → toutes les conditions doivent être vraies
      length(choix_departement) > 0 &&
      !("Tous" %in% choix_departement) &&
      "code_dep" %in% names(station_filtree)) {
    station_filtree <- dplyr::filter( # Filtrage sur le code département
      station_filtree,
      code_dep %in% choix_departement)}

  # Filtre UH
  # Même logique que pour le département
  if (!is.null(choix_uh) &&
      length(choix_uh) > 0 &&
      !("Toutes" %in% choix_uh) &&
      "UH_calculee" %in% names(station_filtree)) {
    station_filtree <- dplyr::filter(  # Filtrage sur l’unité hydrographique
      station_filtree,
      UH_calculee %in% choix_uh) }
  return(station_filtree) } # Retour du résultat final

#' Filtrer une table selon les filtres globaux
#' @description
#' Cette fonction filtre une table de données selon :
#' - le ou les départements sélectionnés
#' - le ou les EQB sélectionnés
#' NB : pas de filtre UH ici car toutes les tables ne contiennent pas cette variable
#' Elle est utile notamment pour les tables contenant directement
#' une colonne `eqb`, `libelle_support` ou `libelle_indice`.
#'
#' @param data Table à filtrer
#' @param choix_departements Département sélectionné
#' @param choix_eqb EQB sélectionné
#' @return La table filtrée
#' @export

filtrer_donnees <- function(data,
                            choix_departements = NULL,
                            choix_eqb = NULL) {
  if (is.null(data)) { return(NULL) } # Sécurité : si pas de données
  data_filtree <- data

  # Filtre département
  if (!is.null(choix_departements) &&
      length(choix_departements) > 0 &&
      !("Tous" %in% choix_departements) &&
      "code_dep" %in% names(data_filtree)) {
    data_filtree <- dplyr::filter( # Filtrage sur le département
      data_filtree,
      code_dep %in% choix_departements)}

  # Filtre EQB
  if (!is.null(choix_eqb) &&
      length(choix_eqb) > 0 &&
      !("Tous" %in% choix_eqb)) {

    # Cas 1 : colonne eqb directe
    if ("eqb" %in% names(data_filtree)) {
      data_filtree <- dplyr::filter(
        data_filtree,
        eqb %in% choix_eqb)}

    # Cas 2 : colonne support biologique
    else if ("libelle_support" %in% names(data_filtree)) {
      data_filtree <- dplyr::filter(
        data_filtree,
        libelle_support %in% choix_eqb)}

    # Cas 3 : colonne indice biologique
    else if ("libelle_indice" %in% names(data_filtree)) {
      data_filtree <- dplyr::filter(
        data_filtree,
        libelle_indice %in% choix_eqb)} }
  return(data_filtree) } # Retour de la table filtrée

#' Filtrer une table selon la station sélectionnée
#'
#' @description
#' Cette fonction filtre une table selon le code de la station sélectionnée.
#' @param data Table contenant une colonne `code_station`
#' @param choix_station Code de la station sélectionnée
#' @return La table filtrée
#' @export

filtrer_par_station <- function(data, choix_station = NULL) {
  if (is.null(data)) { return(NULL) } # Sécurités de base
  if (is.null(choix_station) || length(choix_station) == 0) { return(data) }
  if (!("code_station" %in% names(data))) { return(data) }
  # Filtrage sur la station sélectionnée
  data_filtree <- dplyr::filter(
    data,
    code_station %in% choix_station)
  return(data_filtree) }  # Retour du résultat
