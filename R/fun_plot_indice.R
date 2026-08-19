#' Graphiques des indices biologiques par station
#' @description Trace les graphiques des indices biologiques pour une station.
#' Si EQR et classe sont disponibles : fond coloré + axe EQR.
#' Si EQR et classe absents : résultat brut par année et l'I2M2 reste toujours sur une échelle 0-1.
#' @return Une liste avec graphiques plotly et tables
#' @noRd

fun_plot_indices_station <- function(etat_bio, station_id) {

# Sécurité
  if (is.null(etat_bio) || is.null(station_id) || is.na(station_id) || # Si table absente, station absente et station NA
      station_id == "") { return(NULL) } # Station vide = arret

# Préparation des données
  data_indices <- etat_bio %>% # Table état bio
    dplyr::filter(code_station == station_id) %>% # Station choisie
    dplyr::mutate( # Modifie
      annee_num = as.integer(annee), # Année numérique pour triage
      annee = as.character(annee), # Année affichée
      resultat_indice = as.numeric(resultat_indice), # Résultat numérique
      eqr_indice = as.numeric(eqr_indice), # EQR numérique
      code_indice = as.character(code_indice), # Code indice texte
      libelle_indice = as.character(libelle_indice), # Nom indice texte
      classe_indice = as.character(classe_indice), # Classe texte
      classe_indice = dplyr::if_else(
        is.na(classe_indice) | classe_indice == "", # Si classe absente
        "Non renseigné", # Valeur remplacée par Non renseigné
        classe_indice ) ) # Sinon, on conserve la valeur existane

# Definition des couleurs
  couleurs_classes <- c(
    "TRES_BON" = "#4575b4", # Très bon
    "BON" = "#91cf60", # Bon
    "MOYEN" = "#fee08b", # Moyen
    "MEDIOCRE" = "#fc8d59", # Médiocre
    "MAUVAIS" =  "#d73027", # Mauvais
    "Non renseigné" = "grey90" ) # Sans classe

# Fonction pour tracer un graph par indice qui sera ensuite injecter dans une boucle purr pour l'etendre a tout les indices de la station
    tracer_un_indice <- function(data_indice) {

    nom_indice <- unique(data_indice$libelle_indice)[1] # Recuperation du nom de l'indice
    code_indice <- unique(data_indice$code_indice)[1] # Et du code de l'indice
    est_i2m2 <- grepl("I2M2", nom_indice) || code_indice == "7613" # Test si I2M2 ou 7613 = True dans la colonne est_i2m2
    est_ips <- grepl("IPS", nom_indice) || code_indice == "1022"
    est_ipr <- grepl("IPR", nom_indice) || code_indice == "7036"
    eqr_dispo <- any(!is.na(data_indice$eqr_indice)) # Verifie si au moins un EQR disponible
    classe_dispo <- any( # Verifie si la classe est disponible
      !is.na(data_indice$classe_indice) & # Classe non NA
        data_indice$classe_indice != "Non renseigné" ) # Classe renseignée

    # Preparation
    data_indice <- data_indice %>% # Données d'un indice
      dplyr::arrange(annee_num) %>% # Tri année

       dplyr::mutate( # Modifie
        x_id = dplyr::row_number(), # Position axe x
        y_plot = dplyr::case_when( # Pour l'axe y, si c'est
          est_i2m2 & eqr_dispo ~ eqr_indice, # I2M2 -> EQR
          TRUE ~ resultat_indice ), # Sinon résultat

        texte_survol = paste0( # Texte de survol
          "Année : ", annee, # Année
          "<br>Indice : ", libelle_indice, # Indice
          "<br>Résultat : ", round(resultat_indice, 2), # Résultat
          ifelse(
            !is.na(eqr_indice), # Si EQR existe
            paste0("<br>EQR : ", round(eqr_indice, 3)), # Ajout EQR
            "" ), # Sinon rien
          ifelse(
            classe_dispo, # Si classe dispo
            paste0("<br>Classe : ", classe_indice), # Ajout classe
            "" ) ) ) %>% # Sinon rien
      dplyr::filter(!is.na(y_plot)) # Ne garde que les lignes avec un valeur affichable en y (EQB ou resultats)

    # Tableau exportable
    table_indice <- data_indice %>%
      dplyr::select(
        code_station, # Station
        annee, # Année
        libelle_indice, # Nom indice
        code_indice, # Code indice
        resultat_indice, # Résultat
        eqr_indice, # EQR
        classe_indice, # Classe
        code_qualification, # Code qualification
        libelle_qualification ) # Qualification

    # Fond coloré correspondant au classe
    data_fond <- data_indice %>%
      dplyr::mutate(
        xmin = x_id - 0.5, # Début rectangle
        xmax = x_id + 0.5, # Fin rectangle
        ymin = 0, # Bas rectangle
        ymax = dplyr::case_when(
          est_i2m2 ~ 1, # Hauteur I2M2
          est_ipr ~ 70, # Hauteur IPR
          TRUE ~ 20 ) ) # Hauteur autres indices

  #  Création du graph
    # Base
    p <- ggplot2::ggplot(
      data_indice, # Données
      ggplot2::aes(
        x = x_id, # Axe x numérique
        y = y_plot, # Axe y
        group = 1 ) ) # Une seule courbe relié

    # Fond de couleur
    if (!est_ips && classe_dispo) { # Fond si classe dispo et si c'est pas IPS
      p <- p + # Ajout
        ggplot2::geom_rect( # Rectangles fond
          data = data_fond, # Données fond
          ggplot2::aes(
            xmin = xmin, # Début x
            xmax = xmax, # Fin x
            ymin = ymin, # Bas y
            ymax = ymax, # Haut y
            fill = classe_indice ), # Couleur classe
          alpha = 0.30, # Transparence de 0.30
          inherit.aes = FALSE ) + # Pas aes globales
        ggplot2::scale_fill_manual(
          values = couleurs_classes, # Couleurs classes
          guide = "none" ) } # Pas légende

    # Pour les lignes et points
    p <- p +
      ggplot2::geom_line( # Ligne
        color = "grey35", # Couleur ligne
        linewidth = 0.7 ) + # Épaisseur ligne
      ggplot2::geom_point( # Point
        ggplot2::aes(text = texte_survol), # Texte au survol deja defini
        color = "grey25", # Couleur points
        size = 2.8 ) # Taille points

    # Courbe de tendance
    if (nrow(data_indice) >= 3) { # Si au moins 3 points
      p <- p +
        ggplot2::geom_smooth( # Création de la courbe
          method = "loess", # Courbe lissée
          se = FALSE, # Pas intervalle
          linewidth = 0.7, # Épaisseur
          linetype = "dotted", # Pointillés
          color = "black" ) } # Couleur tendance

    # Style de graph
    p <- p +
      ggplot2::facet_wrap(~ libelle_indice) + # Titre par indice
      ggplot2::scale_x_continuous(
        breaks = data_indice$x_id, # Positions x
        labels = data_indice$annee ) + # Années affichées
      ggplot2::theme_light() + # Thème clair
      ggplot2::theme(
        legend.position = "none", # Pas légende
        strip.background = ggplot2::element_rect(
          fill = "grey70", # Fond bandeau
          color = NA), # Pas contour
        strip.text = ggplot2::element_text(
          color = "white", # Texte blanc
          face = "bold"), # Texte gras
        axis.title.x = ggplot2::element_blank(), # Pas titre x
        axis.title.y = ggplot2::element_text(face = "bold") ) # Titre y gras

    # Cas I2M2
    if (est_i2m2) {
      p <- p +
        ggplot2::scale_y_continuous(
          name = ifelse(eqr_dispo, "EQR", "Résultat indice"), # Nom axe y
          limits = c(0, 1), # Echelle de 0 à 1
          breaks = seq(0, 1, 0.25) ) # Graduations
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"), # Graphique interactif
          table = table_indice ) ) } # Table associée

    # Cas IPR
    if (est_ipr) {
      p <- p +
        ggplot2::scale_y_continuous(
          name = "Résultat indice", # Nom axe y
          limits = c(0, 70), # Limites IPR fixé à 70
          breaks = seq(0, 70, 10) ) # Graduations
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"), # Graphique interactif
          table = table_indice ) ) } # Table associée

    # Cas général
    p <- p +
      ggplot2::scale_y_continuous(
        name = "Résultat indice", # Nom axe y
        limits = c(0, 20), # Limites classiques
        breaks = seq(0, 20, 5) ) # Graduations
    if (!eqr_dispo || est_ips) { # Sans EQR ou IPS
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"), # Graphique simple
          table = table_indice ) ) } # Table associée

    # Conversion en ploty
    graph_indice <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::add_trace(
        data = data_indice, # Données
        x = ~x_id, # Même x
        y = ~eqr_indice, # EQR
        type = "scatter", # Nuage de points
        mode = "markers", # Points
        yaxis = "y2", # Axe secondaire
        marker = list(opacity = 0), # Points invisibles
        showlegend = FALSE, # Pas légende
        hoverinfo = "skip", # Pas tooltip
        inherit = FALSE ) %>% # Pas héritage
      plotly::layout(
        margin = list(
          l = 70, # Marge gauche
          r = 110, # Marge droite pour voir EQR
          b = 60, # Marge bas
          t = 40), # Marge haut
        yaxis = list( # Axe gauche qui correpond aux résultats
          range = c(0, 20), # De 0 à 20
          automargin = TRUE), # Ajuste les marges automatiquement
        yaxis2 = list( # Axe droit qui correspond à l'EQR
          title = "EQR", # Titre
          overlaying = "y", # Superposé a y
          side = "right",
          range = c(0, 1), # De 0 à 1
          tickvals = seq(0, 1, 0.25), # Séparation tout les 0.25
          ticktext = c("0", "0.25", "0.50", "0.75", "1"),
          automargin = TRUE),
        showlegend = FALSE) # Pas de légende

    list( # Renvoie pour un indice par station selectionnée
      graph = graph_indice, # Graphique final
      table = table_indice ) } # Table finale

# Création de tous les graph
  graphiques <- data_indices %>% # Données station
    dplyr::group_by(libelle_indice) %>% # Groupe par indice
    dplyr::group_split() %>% # Sépare en listes
    purrr::map(tracer_un_indice) %>% # Trace chaque indice
    purrr::compact() # Retire les NULL
  graphiques } # Retour liste graphiques


#' Graphique des métriques I2M2
#' @description Trace les métriques de l'I2M2 pour une station.
#' @return Un graphique plotly
#' @noRd

fun_plot_metriques_i2m2 <- function(metriques, station_id) {

  if (is.null(metriques) || is.null(station_id) ||is.na(station_id) || station_id == "") { # Si table absente, station absente, NA ou vide
    return(NULL) } # Arrêt

# Preparation des données
  data_metriques <- metriques %>% # Table métriques
    dplyr::filter(code_station == station_id) %>% # Station choisie
    dplyr::mutate(
      annee_num = as.integer(annee), # Année numérique
      annee = as.character(annee), # Année affichée
      resultat_indice = as.numeric(resultat_indice), # Valeur numérique
      libelle_indice = as.character(libelle_indice), # Nom métrique
      texte_survol = paste0( # Texte tooltip
        "Année : ", annee, # Année
        "<br>Métrique : ", libelle_indice, # Métrique
        "<br>Valeur : ", round(resultat_indice, 3) ) ) %>% # Valeur
    dplyr::filter(
      !is.na(annee_num), # Enleve les années NA
      !is.na(resultat_indice) ) %>% # Enleve les resultats NA
    dplyr::arrange(annee_num) # Tri année

  if (nrow(data_metriques) == 0) { return(NULL) }# Aucune donnée, arret

# Graph
  p <- ggplot2::ggplot( # Base ggplot
    data_metriques, # Données
    ggplot2::aes(
      x = annee, # Axe x
      y = resultat_indice, # Axe y
      color = libelle_indice, # Couleur par métrique
      group = libelle_indice, # Courbe par métrique
      text = texte_survol ) ) + # Tooltip
    ggplot2::geom_line(linewidth = 0.7) + # Lignes
    ggplot2::geom_point(size = 2.3) + # Points
    ggplot2::scale_x_discrete(
      limits = sort(unique(data_metriques$annee)) ) + # Années triées
    ggplot2::scale_y_continuous(
      name = "Valeur de la métrique", # Nom axe y
      limits = c(0, 1), # Limites
      breaks = seq(0, 1, 0.25) ) + # Graduations
    ggplot2::labs(
      x = NULL, # Pas titre x
      color = NULL ) + # Pas titre légende
    ggplot2::theme_light() + # Thème clair
    ggplot2::theme(
      legend.position = "bottom", # Légende bas
      axis.title.y = ggplot2::element_text(face = "bold") ) # Titre y gras

  plotly::ggplotly(p, tooltip = "text") } # Graphique interactif
