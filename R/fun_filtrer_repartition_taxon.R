#' Filtre les données de répartition des taxons
#'
#' @description Cette fonction applique les filtres sur la table donnee_carte_taxon
#' @param donnees Liste contenant les objets de l'application
#' @param choix_departements Départements sélectionnés
#' @param choix_eqb EQB sélectionnés
#' @param choix_uh UH sélectionnées
#' @param taxon_selectionne Taxon sélectionné
#' @return Une table filtrée
#' @noRd

fun_filtrer_repartition_taxon <- function(donnees,
                                          choix_departements = NULL,
                                          choix_eqb = NULL,
                                          choix_uh = NULL,
                                          taxon_selectionne = NULL) {

  df <- donnees$donnee_carte_taxon # Table utilisée pour la carte
  if (nrow(df) == 0) {return(NULL) } # Verif, si la table est vide, arrête la fonction

# Filtre global
  # Filtre département
  if (!is.null(choix_departements) && # Si un choix existe
      length(choix_departements) > 0 && # Si au moins un département
      !("Tous" %in% choix_departements)) { # Si ce n'est pas Tous
    df <- dplyr::filter( # Filtre la table
      df,
      code_dep %in% choix_departements ) } # Garde les départements choisis

  # Filtre EQB ( meme fonctionnement)
  if (!is.null(choix_eqb) &&
      length(choix_eqb) > 0 &&
      !("Tous" %in% choix_eqb)) {
    df <- dplyr::filter(
      df,
      eqb %in% choix_eqb ) } # Garde les EQB choisis

  # Filtre UH -> on récupère les stations  depuis donnee_carte
  # Verification
  if (!is.null(choix_uh) && # Si un choix existe
      length(choix_uh) > 0 && # Si au moins une UH
      !("Toutes" %in% choix_uh) && # Si ce n'est pas Toutes
      !is.null(donnees$donnee_carte) && # Si donnee_carte existe
      "UH_calculee" %in% names(donnees$donnee_carte)) { # Si la colonne UH existe

     stations_uh <- donnees$donnee_carte |> # Table des stations
      sf::st_drop_geometry() |> # Supprime la géométrie
      dplyr::filter(UH_calculee %in% choix_uh) |> # Garde les UH choisies
      dplyr::distinct(code_station) |> # Garde les stations uniques
      dplyr::pull(code_station) # Extrait les codes station
    df <- dplyr::filter( # Filtre la table taxon
      df,
      code_station %in% stations_uh ) } # Garde les stations des UH

# Filtre du choix
  # Filtre taxon
  if (!is.null(taxon_selectionne) && # Si un choix existe
      length(taxon_selectionne) > 0) { # Si au moins un taxon est choisi
    taxon_selectionne <- taxon_selectionne[ # Nettoie la sélection
      !is.na(taxon_selectionne) & # Enlève les NA
        taxon_selectionne != "" ] # Enlève les valeurs vides
    if (length(taxon_selectionne) > 0) { # Si au moins un taxon reste
      df <- dplyr::filter( # Filtre la table
        df,
        libelle_taxon %in% taxon_selectionne ) } }# Garde les taxons choisis

  if (nrow(df) == 0) { return(NULL) } # Si plus aucune ligne, arrete la fonction
  sf::st_transform(df, 4326)  # Convertis pour leaflet
}
