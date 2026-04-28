#' Préparation des données d'occupation du sol du bv d'une station
#'
#' @description Cette 1ere fonction prepare les données pour le graphique et le tableau
#' Le script est un peu plus detaillé dans les scripts pour l'occupation de la station
#' La station sélectionnée est identifiée par son `code_station`,
#' dans la table `stations`, on récupère son identifiant de bassin versant `id_BV`
#' et cet identifiant est recherché dans la colonne `CdOH` des tables d'occupation
#'
#' @param donnees Liste contenant les objets de l'application
#' @param station_id Code de la station sélectionnée
#'
#' @return Une liste contenant :
#' - table_large : tableau avec les années en lignes et les catégories en colonnes
#' - table_long : tableau au format long utilisé pour le graphique
#' - id_bv : identifiant du bassin versant retrouvé
#' ou NULL si aucune donnée n'est disponible
#' @noRd

fun_prep_bv_occupation <- function(donnees, station_id) {

  if (is.null(donnees) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL) }  # On vérifie que les données existent et qu'une station est bien sélectionnée
  stations <- donnees$stations # On recupere la table

  # Harmonisation du code selectionné
  station_id <- stringr::str_pad(
    string = as.character(station_id),
    width = 8, # On rajoute un 0 à gauche si il le faut
    side = "left",
    pad = "0" )

  stations <- stations %>% # Harmonisation aussi
    dplyr::mutate(
      code_station = stringr::str_pad(
        string = as.character(.data$code_station),
        width = 8,
        side = "left",
        pad = "0" ),
      id_BV = as.character(.data$id_BV) ) #Transformer en caractere car c'est l'iddentifiant

  # Récupération du bassin versant de la station
  station_info <- stations %>% # On filtre la table pour garder la station selectionnee
    dplyr::filter(.data$code_station == station_id)
  id_bv <- station_info$id_BV[1] # On recupere le 1er ID-BV
  if (is.null(id_bv) || is.na(id_bv) || id_bv == "") { return(NULL) } # Vérifie que l'identifiant du bassin versant existe bien

  annees <- c("1990", "2000", "2006", "2012", "2018") # Vecteur des années

  # Construction d'une liste de tables, une par année
  liste_tables <- purrr::map(annees, function(an) { # Creation d'une bloucle pour les diff années
    nom_table <- paste0("occupation_BV_", an) # Nom des tables
    if (!nom_table %in% names(donnees)) { return(NULL) }  # Si elle existe pas -> année suivante
    table_occ <- donnees[[nom_table]] # Récupération de la table d'occupation pour l'année

    # Harmonisation de CdOH = on le transforme en caractère pour pouvoir le comparer à id_bv
    table_occ <- table_occ %>%
      dplyr::mutate(
        CdOH = as.character(.data$CdOH))

    # Filtrage
    bv_occ <- table_occ %>%
      dplyr::filter(.data$CdOH == id_bv)  # Filtre la table pour garder uniquement le BV correspondant
    if (nrow(bv_occ) == 0) {return(NULL)} # Sécurité: si y'a pas = année suivante
    bv_occ <- bv_occ[1, , drop = FALSE] # Sécurité : on garde la 1ere ligne ( drop= FALSE -> tableau et non un vecteur)

    # On garde uniquement les colonnes utiles + année
    bv_occ %>%
      dplyr::transmute(
        annee = as.integer(an),
        Artificial = as.numeric(.data$Artificial),
        Agricultur = as.numeric(.data$Agricultur),
        Foret = as.numeric(.data$Foret),
        Zones_humi = as.numeric(.data$Zones_humi),
        Eau = as.numeric(.data$Eau))
  })

  table_large <- dplyr::bind_rows(liste_tables) # Assemblage en 1 seul tableau
  if (nrow(table_large) == 0) {return(NULL)} # Si aucune donnée n'a pu être récupérée au final, on arrête

  # Format long pour le graphique
  table_long <- table_large %>%
    tidyr::pivot_longer(
      cols = c("Artificial", "Agricultur", "Foret", "Zones_humi", "Eau"),
      names_to = "occupation", # Nom des colonnes
      values_to = "pourcentage" ) %>%
    dplyr::mutate(
      occupation = factor(
        .data$occupation, # Renommage
        levels = c("Artificial", "Agricultur", "Foret", "Zones_humi", "Eau"),
        labels = c("Artificialisation", "Agriculture", "Forêt", "Zones humides", "Eau") ),
      texte_survol = paste0( # Texte de survol
        "Année : ", .data$annee,
        "<br>Catégorie : ", as.character(.data$occupation),
        "<br>Pourcentage : ", round(.data$pourcentage, 1), " %" ) )

  # Renommage pour le tableau
  table_large <- table_large %>%
    dplyr::rename(
      "Artificialisation" = .data$Artificial,
      "Agriculture" = .data$Agricultur,
      "Forêt" = .data$Foret,
      "Zones humides" = .data$Zones_humi,
      "Eau" = .data$Eau ) %>%
    dplyr::arrange(.data$annee)

  return(list( # Retour une liste pour le graph
    table_large = table_large,
    table_long = table_long,
    id_bv = id_bv ))
}


#' Graphique d'évolution de l'occupation du sol du bassin versant
#' @param table_long Tableau au format long issu de fun_prep_bv_occupation()
#' @return Un graphique ggplot, ou NULL si aucune donnée
#' @noRd

fun_plot_bv_occupation <- function(table_long) {

  if (is.null(table_long) || nrow(table_long) == 0) { # On vérifie que le tableau existe et contient des données
    return(NULL) }

  #Creation du graph multicourbe
  ggplot2::ggplot(
    table_long, # Definition des bases
    ggplot2::aes(
      x = .data$annee, # Année a l'horizontal
      y = .data$pourcentage, # Pourcentage a la verticale
      color = .data$occupation, # Une couleur diff par catégorie
      group = .data$occupation, # Une courbe diff par catégorie
      text = .data$texte_survol ) ) + # Texte de survol
    ggplot2::geom_line(linewidth = 1) + # Trace les courbes
    ggplot2::geom_point(size = 2.8) + # Ajoute les points

    ggplot2::scale_color_manual( # Definition des couleurs
      values = c(
        "Artificialisation" = "#F8766D",
        "Agriculture" = "#A3A500",
        "Forêt" = "#00BF7D",
        "Zones humides" = "#7B3A96",
        "Eau" = "#004B8D")) +

    # Réglage de l'axe des ordonnées
    ggplot2::scale_y_continuous(
      limits = c(0, 100), # De 0 à 100
      expand = c(0, 0)) + # Suprime l'espace blanc supplémentaire
    # Axe des abscisses
    ggplot2::scale_x_continuous(
      breaks = sort(unique(table_long$annee))) + # Uniquement les années presentes

    ggplot2::labs( # Titre
      title = "Évolution de l'occupation du sol du bassin versant",
      x = "Année",
      y = "Pourcentage",
      color = NULL) +

    ggplot2::theme_minimal() + # Theme simple
    ggplot2::theme(
      legend.position = "bottom", # Legnde en bas
      panel.grid.minor = ggplot2::element_blank(), # Enleve la grille
      plot.title = ggplot2::element_text(
        hjust = 0.5, # Centre le titre
        face = "bold", # Titre en gras
        size = 8 ) ) # Definition de la taille
}
