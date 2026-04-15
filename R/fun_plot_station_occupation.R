#' Graphique d'occupation du sol pour une station
#'
#' @param table_occupation Table d'occupation du sol pour une année CLC
#' @param station_id Code de la station sélectionnée
#'
#' @return Un graphique ggplot, ou NULL si aucune donnée
#' @noRd

fun_plot_station_occupation <- function(table_occupation, station_id) {

  # Verification des données d'entrée
  if (is.null(table_occupation) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL) } # On verifie que les tables existe, qu'une satation est selctionnée et que ce n'est pas vide

  # Harmonisation des code_station en rajoutant a 0 si il en manque 1 (sur QGIS ce n'est pas automatique)
  station_id <- stringr::str_pad(
    as.character(station_id),
    width = 8,
    side = "left", # A gauche
    pad = "0")

  # Filtrage de la table pour ne conserver que la ligne correpondante
  station_occ <- table_occupation %>%
    dplyr::mutate(
      code_station = stringr::str_pad(
        as.character(.data$code_station), # Harmoniste les code_station
        width = 8,
        side = "left",
        pad = "0") ) %>%
    dplyr::filter(.data$code_station == station_id)

  #Verifie qu'une donnée existe sinon on arrete la fonction
  if (nrow(station_occ) == 0) { return(NULL) }

  #Permet de garder que 1 ligne (sécurité)
  station_occ <- station_occ[1, , drop = FALSE]

  # Permet de garder que les colonne utile au graphique
  occ_long <- station_occ %>%
    dplyr::select(
      .data$Artificial,
      .data$Agricultur,
      .data$Foret,
      .data$Zones_humi,
      .data$Eau ) %>%
    # Et de les trsanformer au format long apres le choix de la station réalisé
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "occupation",
      values_to = "pourcentage" ) %>%
    # Mise en forle
    dplyr::mutate(
      occupation = factor(
        .data$occupation, # On renome les catégorie
        levels = c("Artificial", "Agricultur", "Foret", "Zones_humi", "Eau"),
        labels = c("Artificialisation", "Agriculture", "Forêt", "Zones humides", "Eau") ),

      label = dplyr::if_else(
        .data$pourcentage >= 4, # Affiche les pourcentages sur le coté (seulement ceux > 4%)
        paste0(round(.data$pourcentage, 1), " %"), # Par la suite on va creer un affichage pour les plus petit
        "" ),

      texte_survol = paste0( # Creation de l'affichage des ù au survol
        as.character(.data$occupation),
        " : ",
        round(.data$pourcentage, 1),
        " %"),
      barre = "Occupation" ) # Création de la barre pour avec une barre unique empilé

  # Création du graphique
  ggplot2::ggplot(
    occ_long,
    # Initialise le graphique
    ggplot2::aes(
      x = .data$barre,
      y = .data$pourcentage,
      fill = .data$occupation,
      text = .data$texte_survol ) ) +

    ggplot2::geom_col(width = 0.5) + # Création des barre empilé
    ggplot2::geom_text( # Ajout des textes
      ggplot2::aes(label = .data$label),
      position = ggplot2::position_stack(vjust = 0.5), # Au milieu de chaque catégorie
      size = 4 ) +

    #Definition des couleur
    ggplot2::scale_fill_manual(
      values = c(
        "Artificialisation" = "#F8766D",
        "Agriculture" = "#A3A500",
        "Forêt" = "#00BF7D",
        "Zones humides" = "#00B0F6",
        "Eau" = "#004B8D" ) ) +

    # Reglage de l'axe des ordonnées
    ggplot2::scale_y_continuous(
      limits = c(0, 100), # De 0 à 100
      expand = c(0, 0) ) + # Supprime l'espace blanc supplémentaire

    # Titre et légende
     ggplot2::labs(
      x = NULL,
      y = "Pourcentage",
      fill = NULL ) +

    ggplot2::theme_minimal() + # theme_minimal() applique un thème sobre et léger

    # On enlève la grille verticale, le texte et les graduations de l'axe x
    # La légende est placée en bas du graphique.
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom")
}
