#' Prépare les informations de surface du bassin versant
#'
#' @description
#' À partir du code station, cette fonction récupère l'identifiant du bv,
#' trouve la surface dans la table `surface_BV`puis calcule la surface en km².
#'
#' @param donnees Liste contenant les objets de l'application
#' @param station_id Code de la station sélectionnée
#' @return Les informations sous forme de tableau
#' @noRd

fun_prep_surface_bv <- function(donnees, station_id) {
  stations <- donnees$stations # Recupere les tables sous forme utilisable
  surface_BV <- donnees$surface_BV

  # Récupère l'id du bassin versant
  id_bv <- stations %>%
    dplyr::filter(code_station == station_id) %>% # Fitre la station selectionne
    dplyr::pull(id_BV) # Récupère l'id du bassin versant associé
  id_bv <- id_bv[1] # Prend le 1er

  # Cherche la surface correspondante
  surface_info <- surface_BV %>%
    dplyr::filter(CdOH == id_bv) # On filtre sur l'id du bassin versant

  # Conversion explicite en numérique
  surface_info <- surface_info %>%
    dplyr::mutate(
      Surface_m2 = as.numeric(Surface_m2), # Format numérique
      id_bv = CdOH, # Renomme
      surface_km2 = Surface_m2 / 1000000) %>% # Conversion en KM2
    dplyr::select(id_bv, Surface_m2, surface_km2) # Selection

   return(surface_info)}
