#' Préparation des données d'occupation du sol pour une station
#'
#' @description Cette 1ere fonction prepare les données pour le graphique et le tableau
#'
#' @param donnees Liste contenant les objets de l'application
#' @param station_id Code de la station sélectionnée
#'
#' @return Une liste contenant :
#' - table_large : tableau avec les années en lignes et les catégories en colonnes
#' - table_long : tableau au format long utilisé pour le graphique
#' ou NULL si aucune donnée n'est disponible
#' @noRd

fun_prep_station_occupation <- function(donnees, station_id) {

  if (is.null(donnees) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL) } # On vérifie que les données existent et qu'une station est bien sélectionnée

  # Harmonisation des code_station en rajoutant a 0 si il en manque 1 (sur QGIS ce n'est pas automatique)
  station_id <- stringr::str_pad(
    as.character(station_id),
    width = 8,
    side = "left", # A gauche
    pad = "0")

  annees <- c("1990", "2000", "2006", "2012", "2018") # Vecteur des années CLC

  # Construction d'une liste de tables, une par année
  liste_tables <- purrr::map(annees, function(an) { # Creation d'une boucle pour
    nom_table <- paste0("occupation_", an) # Nom de la table correspondant à l'année
    if (!nom_table %in% names(donnees)) {return(NULL) } # Si elle existe pas -> année suivante
    table_occ <- donnees[[nom_table]] # Récupération de la table d'occupation pour l'année

    # Filtrage sur la station choisie
    station_occ <- table_occ %>%
      dplyr::mutate(
        code_station = stringr::str_pad(
          as.character(.data$code_station), # Harmonise les code_station = c'est une sécurité
          width = 8,
          side = "left",
          pad = "0") ) %>%
      dplyr::filter(.data$code_station == station_id) # Garde la ligne de la station choisie

    station_occ <- station_occ[1, , drop = FALSE] # On garde uniquement la 1ere ligne

    # On garde uniquement les colonnes utiles et on ajoute l'année
    station_occ %>%
      dplyr::transmute(
        annee = as.integer(an),
        Artificial = as.numeric(.data$Artificial),
        Agricultur = as.numeric(.data$Agricultur),
        Foret = as.numeric(.data$Foret),
        Zones_humi = as.numeric(.data$Zones_humi),
        Eau = as.numeric(.data$Eau) )
    }) # Fin de la boucle

  table_large <- dplyr::bind_rows(liste_tables)  # Assemblage de tous en un seul tableau

  # Transformation du tableau large en format long pour le graphique
  table_long <- table_large %>%
    tidyr::pivot_longer(
      cols = c("Artificial", "Agricultur", "Foret", "Zones_humi", "Eau"), #Info
      names_to = "occupation", # Nom de la colonne
      values_to = "pourcentage" ) %>%
    dplyr::mutate(
      occupation = factor(
        .data$occupation, # Renomme les catégories pour l'affichage
        levels = c("Artificial", "Agricultur", "Foret", "Zones_humi", "Eau"),
        labels = c("Artificialisation", "Agriculture", "Forêt", "Zones humides", "Eau") ),

      texte_survol = paste0( # Texte affiché au survol dans plotly
        "Année : ", .data$annee,
        "<br>Catégorie : ", as.character(.data$occupation),
        "<br>Pourcentage : ", round(.data$pourcentage, 1), " %") )

  # Renommage aussi du tableau large pour le tableau
  table_large <- table_large %>%
    dplyr::rename(
      "Artificialisation" = .data$Artificial,
      "Agriculture" = .data$Agricultur,
      "Forêt" = .data$Foret,
      "Zones humides" = .data$Zones_humi,
      "Eau" = .data$Eau ) %>%
    dplyr::arrange(.data$annee) # Trie les lignes par année croissante

  # Retourne les deux formats de table utiles pour le module
  return(list(
    table_large = table_large,
    table_long = table_long ))
}


#' Graphique d'évolution de l'occupation du sol pour une station
#'
#' @param table_long Tableau au format long issu de fun_prep_station_occupation()
#'
#' @return Un graphique ggplot, ou NULL si aucune donnée
#' @noRd

fun_plot_station_occupation <- function(table_long) {
  if (is.null(table_long) || nrow(table_long) == 0) { return(NULL) } # On vérifie que le tableau existe et contient des données

  # Création du graphique multicourbe
  ggplot2::ggplot(
    table_long, # Definition des bases
    ggplot2::aes(
      x = .data$annee, # Année à l'horizontale
      y = .data$pourcentage, # Pourcentage a la verticale
      color = .data$occupation, # Une couleur diff par catégories
      group = .data$occupation, # Une courbes diff par catégorie
      text = .data$texte_survol ) ) + # Affichage du texte
    ggplot2::geom_line(linewidth = 1) + # Trace les courbes d'évolution
    ggplot2::geom_point(size = 2.8) + # Ajoute un point sur chaque année

    # Définition des couleurs des catégories
    ggplot2::scale_color_manual(
      values = c(
        "Artificialisation" = "#F8766D",
        "Agriculture" = "#A3A500",
        "Forêt" = "#00BF7D",
        "Zones humides" = "#7B3A96",
        "Eau" = "#004B8D" ) ) +

    # Réglage de l'axe des ordonnées
    ggplot2::scale_y_continuous(
      limits = c(0, 100), # Pourcentage de 0 à 100
      expand = c(0, 0) ) + # Supprime l'espace blanc supplémentaire

    # Réglage de l'axe des abscisses
    ggplot2::scale_x_continuous(
      breaks = sort(unique(table_long$annee)) ) + # Affiche uniquement les années présentes

    # Titre et légende
    ggplot2::labs(
      title = "Évolution de l'occupation du sol dans un rayon de 5 km autour de la station",
      x = "Année",
      y = "Pourcentage",
      color = NULL ) +
    ggplot2::theme_minimal() + # Theme sobre et léger

    # Mise en forme finale du graphique
    ggplot2::theme(
      legend.position = "bottom", # Légende sous le graphique
      panel.grid.minor = ggplot2::element_blank(), # Supprime la grille secondaire
      plot.title = ggplot2::element_text( # Centre et met en gras le titre
        hjust = 0.5,
        face = "bold",
        size = 6 ) )
}
