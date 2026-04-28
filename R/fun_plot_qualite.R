#' Fonction d'application des filtres aux données qualité
#'
#' @description On part de la table `etat_bio`, on applique les filtres ( dep, eqb, uh),
#' puis renvoie un tableau pour construire le graphique.
#' Le filtre UH est appliqué via la table `stations` car non present dans`etat_bio`.
#'
#' @return Un tableau filtré contenant les données de qualité
#' @noRd

fun_prep_qualite <- function(donnees,
                             choix_departements = NULL,
                             choix_eqb = NULL,
                             choix_uh = NULL) {

  if (is.null(donnees) || is.null(donnees$etat_bio) || is.null(donnees$stations)) {return(NULL)} # Verification
  data <- donnees$etat_bio # On recupere la table

  choix_eqb_etat_bio <- choix_eqb # Harmonisation du filtre EQB
  if (!is.null(choix_eqb_etat_bio) && length(choix_eqb_etat_bio) > 0) {
    choix_eqb_etat_bio <- dplyr::case_when( # Car les nom ne correspondait pas
      choix_eqb_etat_bio == "Diatomées" ~ "Diatomées benthiques",
      choix_eqb_etat_bio == "Macroinvertébrés" ~ "Macroinvertébrés aquatiques",
      choix_eqb_etat_bio == "Macrophytes" ~ "Macrophytes",
      choix_eqb_etat_bio == "Poissons" ~ "Poissons",
      choix_eqb_etat_bio == "Tous" ~ "Tous",
      TRUE ~ choix_eqb_etat_bio ) }

  data <- filtrer_donnees(
    data = data, # On applique les filtres
    choix_departements = choix_departements,
    choix_eqb = choix_eqb_etat_bio)

  stations_filtrees <- filtrer_stations( # On filtre l'UH dans la table stations
    station = donnees$stations,
    choix_departement = choix_departements,
    choix_uh = choix_uh )

  data <- data %>%  # On harmonise le type de code_station
    dplyr::mutate(code_station = as.character(code_station))
  stations_filtrees <- stations_filtrees %>%
    dplyr::mutate(code_station = as.character(code_station))

  data <- data %>% # On garde uniquement les données qualité des stations retenues
    dplyr::filter(code_station %in% stations_filtrees$code_station) # dans les 2 tables en 1
  if (nrow(data) == 0) {return(NULL)} # Si il reste aucune station

  return(data) # Retourne les données
}

#' Fonction de tracé des histogrammes de qualité
#'
#' @description Les données sont regroupées par cycle DCE et par indice biologique,
#' afin d'obtenir une vision synthétique de la répartition des classes de qualité.
#'
#' @param donnees_graphique Tableau contenant: code_station, annee, libelle_indice, classe_indice
#'
#' @return Un graphique ggplot
#' @noRd

fun_plot_qualite <- function(donnees_graphique) {

  if (is.null(donnees_graphique) || nrow(donnees_graphique) == 0){ # Vérifie que les données existent bien
    return( # Si non , affiche :
      ggplot2::ggplot() + # Graphique vide
        ggplot2::theme_void() +
        ggplot2::annotate(
          "text", # En texte
          x = 1, y = 1,
          label = "Aucune donnée disponible pour le graphique") ) } # Texte afficher

  data_plot <- donnees_graphique %>% # Preparation des données
    dplyr::distinct(code_station, annee, libelle_indice, classe_indice) %>% # Enleve les doublons
    dplyr::mutate(
      classe_indice = tidyr::replace_na(classe_indice, "Non renseigné"), # Remplace les NA
      cycle_dce = dplyr::case_when( # Creer les période DCE
        annee < 2010 ~ "< 2010",
        annee >= 2010 & annee <= 2015 ~ "2010-2015",
        annee >= 2016 & annee <= 2021 ~ "2016-2021",
        annee >= 2022 & annee <= 2027 ~ "2022-2027",
        TRUE ~ NA_character_) ) %>%
    dplyr::filter(!is.na(cycle_dce)) %>% # Enleve les NA
    dplyr::filter(libelle_indice != "IPS") %>% # Retire IPS car il n'a pas de classe
    dplyr::group_by(cycle_dce, libelle_indice, classe_indice) %>% # Regroupement pour compter les stations
    dplyr::summarise(
      n = dplyr::n_distinct(code_station), # Nombre de station unique
      .groups = "drop" )

  data_plot <- data_plot %>% # Mise en forme pour un affichage propre
    dplyr::mutate(
      cycle_dce = factor( # Ordre des cycles
        cycle_dce,
        levels = c("< 2010", "2010-2015", "2016-2021", "2022-2027") ),
      classe_indice = factor(
        classe_indice,
        levels = c( # Ordre des classes
          "Non renseigné",
          "TRES_BON",
          "BON",
          "MOYEN",
          "MEDIOCRE",
          "MAUVAIS" ) ),
      texte_survol = paste0( # Texte a afficher au survol
        "Indice : ", libelle_indice,
        "<br>Période : ", cycle_dce,
        "<br>Classe : ", classe_indice,
        "<br>Nombre de stations : ", n) )

  # Construction du graphique final
  ggplot2::ggplot( # Sous forme ggplot puis dans le mod conversion en ploty
    data_plot, # Données utilisées
    ggplot2::aes(
      x = cycle_dce, # Axe x = les cycles
      y = n, # Axe y = le nb de stations
      fill = classe_indice, # Couleur de base
      text = texte_survol ) ) +
    ggplot2::geom_col() + # Barre
    ggplot2::facet_wrap(~ libelle_indice, scales = "free_y") + # 1 graph par indices
    ggplot2::scale_fill_manual( # Onchange les coueleur manuellement
      values = c(
        "MEDIOCRE" = "#fc8d59",
        "MAUVAIS" = "#d73027",
        "MOYEN" = "#fee08b",
        "BON" = "#91cf60",
        "TRES_BON" = "#4575b4",
        "Non renseigné" = "grey80" ),
      drop = FALSE, # Permet de garder les classes même si absentes
      name = NULL ) + # Supprime le titre de la légende
    ggplot2::labs( # Parametres des axes
      x = NULL, # pas de noms
      y = "Nombre de stations" ) +
    ggplot2::theme_minimal() + # Theme graphique simple de base
    ggplot2::theme(
      legend.position = "none", # Supprime complètement la légende
      panel.grid.minor = ggplot2::element_blank(), # Enleve les petites grilles
      panel.grid.major.x = ggplot2::element_blank(), # Enleve les grilles verticales
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # Incline et ajuste le nom de x
      strip.text = ggplot2::element_text(face = "bold") ) # En gras
}
