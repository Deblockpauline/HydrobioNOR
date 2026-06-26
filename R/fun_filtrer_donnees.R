#' Fonctions de filtrage des données
#' @description Fonctions pour filtrer les données de l'application
#' @noRd

#------------------------------------------------------------------------------------------------
#' 1) Filtrer les stations selon la géographie
#'
#' @description Cette fonction filtre la table des stations selon  le département,l'UH et le réseau
#' Le filtre EQB est géré  dans le module de carte à partir d'une autre table
#' @param station Table des stations
#' @param choix_departement Choix du département
#' @param choix_uh Choix de l'unité hydrographique
#' @param choix_reseau Choix du reseau
#' @return une table des stations filtrées
#' @export

filtrer_stations <- function(station,
                             choix_departement = "Tous",
                             choix_uh = "Toutes",
                             choix_reseau ="Tous") {

  station_filtree <- station # Copie de la table

  # Filtre département
  # && = "ET" → toutes les conditions doivent être vraies
  if (!is.null(choix_departement) && # Verifie qu'un choix existe,
      length(choix_departement) > 0 && # la colonne existe dans la table et
      !("Tous" %in% choix_departement) && # que tout n'est pas selectionné
      "code_dep" %in% names(station_filtree)) {
    station_filtree <- dplyr::filter( # Filtrage sur le code département
      station_filtree,
      code_dep %in% choix_departement)}

  # Filtre UH = même logique que pour le département
  if (!is.null(choix_uh) &&
      length(choix_uh) > 0 &&
      !("Toutes" %in% choix_uh) &&
      "UH_calculee" %in% names(station_filtree)) {
    station_filtree <- dplyr::filter(  # Filtrage sur l’unité hydrographique
      station_filtree,
      UH_calculee %in% choix_uh) }

  # Filtre reseau, meme logique mais attention séparateur différent
  if (!is.null(choix_reseau) &&
      length(choix_reseau) > 0 &&
      !("Tous" %in% choix_reseau) &&
      "reseau" %in% names(station_filtree)) {
    station_filtree <- dplyr::filter(
      station_filtree,
      stringr::str_detect( # Recherche
        reseau, # Dans la colonne reseau, les réseaux avec des séparateur - /
        paste0("(^|[-/])", paste(choix_reseau, collapse = "|"), "($|[-/])") ) ) }

  return(station_filtree) } # Retour du résultat final

#------------------------------------------------------------------------------------------------
#' 2) Filtrer une table selon les filtres globaux
#'
#' @description Cette fonction filtre une table (peu importe laquelle) selon le départements,l'EQB, le reseau et la qualification
#' Pas de filtre UH ici car toutes les tables ne contiennent pas cette variable
#' @param data Table à filtrer (nom générique)
#' @return La table filtrée
#' @export

filtrer_donnees <- function(data,
                            choix_departements = NULL,
                            choix_eqb = NULL,
                            choix_qualification = NULL,
                            choix_reseau = NULL) {
  data_filtree <- data # Création d'une copie

  # Filtre département
  if (!is.null(choix_departements) && # Verifie qu'un choix existe
      length(choix_departements) > 0 && # Au moins une valeur
      !("Tous" %in% choix_departements) && # Verifie que tous n'est pas selectionné
      "code_dep" %in% names(data_filtree)) { # Verifie la presence de colonne
    data_filtree <- dplyr::filter( # Filtre selon le choix
      data_filtree,
      code_dep %in% choix_departements) }

  # Filtre EQB, meme logique
  if (!is.null(choix_eqb) &&
      length(choix_eqb) > 0 &&
      !("Tous" %in% choix_eqb) &&
      "libelle_support" %in% names(data_filtree)) {
    data_filtree <- dplyr::filter(
      data_filtree,
      libelle_support %in% choix_eqb) }

  # Filtre qualification, meme logique
  if (!is.null(choix_qualification) &&
      length(choix_qualification) > 0 &&
      !("Toutes" %in% choix_qualification) &&
      "libelle_qualification" %in% names(data_filtree)) {
    data_filtree <- dplyr::filter(
      data_filtree,
      libelle_qualification %in% choix_qualification) }

  # Filtre réseau, meme logique mais attention au séparateur
  if (!is.null(choix_reseau) &&
      length(choix_reseau) > 0 &&
      !("Tous" %in% choix_reseau) &&
      "reseau" %in% names(data_filtree)) {
    data_filtree <- dplyr::filter(
      data_filtree,
      stringr::str_detect(
        reseau,
        paste0("(^|[-/])", paste(choix_reseau, collapse = "|"), "($|[-/])") ) ) }

  return(data_filtree) } # Retour de la table filtrée

#------------------------------------------------------------------------------------------------
#' 3) Filtrer une table selon la station sélectionnée
#' @param data Table contenant une colonne `code_station`
#' @param choix_station Code de la station sélectionnée
#' @return La table filtrée
#' @export

filtrer_par_station <- function(data, choix_station = NULL) {

  # Si aucune station n'est sélectionnée, retourne la table entière
  if (is.null(choix_station) || length(choix_station) == 0) { return(data) }

   # Filtrage sur la station sélectionnée
  data_filtree <- dplyr::filter(
    data,
    code_station %in% choix_station)

   return(data_filtree) }  # Retour du résultat
