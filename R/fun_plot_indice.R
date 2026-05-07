#' Graphiques des indices biologiques par station
#'
#' @description
#' Trace les graph des indices biologiques pour une station.
#' La couleur de fond utilise directement classe_indice dans la table etat_bio, année par année.
#' IBD, IBMR, IBG équivalent : résultat à gauche et EQR à droite
#' I2M2 : EQR uniquement
#' IPR : résultat uniquement avec fond coloré
#' IPS : résultat uniquement, sans fond coloré
#'
#' @param etat_bio Table des états biologiques
#' @param station_id Code de la station sélectionnée
#' @return Une liste avec les graphiques plotly et les tables associées
#' @noRd

fun_plot_indices_station <- function(etat_bio, station_id) {

  if (is.null(etat_bio) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL)} # Verif de base

  data_indices <- etat_bio %>% # Preparation des données dans une nouvelle table
    dplyr::filter(code_station == station_id) %>% # Station choisie
    dplyr::mutate(
      annee_num = as.integer(annee), # Annee pour trier
      annee = as.character(annee), # Annee pour afficher
      resultat_indice = as.numeric(resultat_indice), # Resultat en nombre
      eqr_indice = as.numeric(eqr_indice), # EQR en nombre
      code_indice = as.character(code_indice), # Code en texte
      libelle_indice = as.character(libelle_indice), # Nom en texte
      classe_indice = as.character(classe_indice), # Classe en texte
      classe_indice = dplyr::if_else( # Quand il y a un NA = Non renseigné
        is.na(classe_indice) | classe_indice == "",
        "Non renseigné",
        classe_indice)) # Remplace les classes vides

  couleurs_classes <- c( # Definition des couleurs de classe
    "TRES_BON" = "#4575b4",
    "BON" = "#91cf60",
    "MOYEN" = "#fee08b",
    "MAUVAIS" = "#fc8d59",
    "MEDIOCRE" = "#d73027",
    "Non renseigné" = "grey90")

  # Fonction pour tracer un indice a la fois
  tracer_un_indice <- function(data_indice) {

    nom_indice <- unique(data_indice$libelle_indice)[1] # Nom de l'indice
    code_indice <- unique(data_indice$code_indice)[1] # Code de l'indice
    est_i2m2 <- grepl("I2M2", nom_indice) || grepl("I2M2", code_indice) # Cas particulier, ils ne construisent pas de la meme facon
    est_ips <- grepl("IPS", nom_indice) || grepl("IPS", code_indice) # TRUE si l'indice est IPS
    est_ipr <- grepl("IPR", nom_indice) || grepl("IPR", code_indice) # grepl permet de chercher du texte

    # Donnees du graphique
    data_indice <- data_indice %>% # On recupere la table pour cet indice
      dplyr::arrange(annee_num) %>% # Tri par annee
      dplyr::mutate(
        x_id = dplyr::row_number(), # Position pour chaque année sur l'axe x
        y_plot = if (est_i2m2) eqr_indice else resultat_indice, # Valeur affichee, si c'est I2M2 affiche seulement EQR sinon affiche resultat
        texte_survol = paste0( # Texte au survol
          "Année : ", annee,
          "<br>Indice : ", libelle_indice,
          "<br>Résultat : ", round(resultat_indice, 2), # Resultat arrondi
          ifelse(!is.na(eqr_indice), paste0("<br>EQR : ", round(eqr_indice, 3)), ""), # Seulement EQR quand il existe
          "<br>Classe : ", classe_indice)) %>%
      dplyr::filter(!is.na(y_plot)) # Enleve les valeurs vides

    # Table exportable
    table_indice <- data_indice %>% # Table associee au graphique
      dplyr::select(
        code_station,
        annee,
        libelle_indice,
        code_indice,
        resultat_indice,
        eqr_indice,
        classe_indice) # Colonnes utiles pour export CSV

    # Fond de couleur de classe
    data_fond <- data_indice %>% # Table pour les fond coloré
      dplyr::mutate( # Limite de "cadre"
        xmin = x_id - 0.5,
        xmax = x_id + 0.5,
        ymin = 0, # Debute à 0
        ymax = dplyr::case_when(
          est_i2m2 ~ 1, # Pour adapter les case de couleur a la grandeur de l'axe
          est_ipr ~ 70,
          TRUE ~ 20) )

    # Base du graphique
    p <- ggplot2::ggplot( # Création en ggplot
      data_indice, # donnée utilisé
      ggplot2::aes( # Les axes
        x = x_id,
        y = y_plot,
        group = 1)) # Une seule courbe par graph

    # Fond couleur, sauf pour IPS
    if (!est_ips) { # IPS n'a pas de fond
      p <- p + # Ajoute sur le graph de base
        ggplot2::geom_rect( # Ajoute des rectangle
          data = data_fond, # Utilise la table de fond pour ca
          ggplot2::aes( # Dimensions
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = classe_indice), # Couleur selon la classe écologique
          alpha = 0.30) + # Transparance du fond
        ggplot2::scale_fill_manual(
          values = couleurs_classes, # Reprend les couleur qu'on a defini au début
          guide = "none") } # Pas de legende

    # Ligne et points
    p <- p + # On ajoute la courbe
      ggplot2::geom_line( # Ligne entre les années
        color = "grey35", # Couleur
        linewidth = 0.7) + # Epaisseur
      ggplot2::geom_point( # Point pour chaque année
        ggplot2::aes(text = texte_survol), # Texte a afficher
        color = "grey25", # Couleur
        size = 2.8) # Taille

    # Création d'une tendance si assez de points
    if (nrow(data_indice) >= 3) { # Au moins 3 points
      p <- p +
        ggplot2::geom_smooth( # Courbe lissée
          method = "loess", # Methode de lissage
          se = FALSE, # Pas d'intervalle de confiance
          linewidth = 0.7, # Epaisseur
          linetype = "dotted", # En pointillé
          color = "black")} # Noir

    # Mise en forme
    p <- p +
      ggplot2::facet_wrap(~ libelle_indice) + # Titre avec le nom de l'indice
      ggplot2::scale_x_continuous( # Reglage de l'axe X
        breaks = data_indice$x_id, # Graduation
        labels = data_indice$annee) + # Affiche les années
      ggplot2::theme_light() + # Theme claire
      ggplot2::theme( # Details
        legend.position = "none", # Pas de légende
        strip.background = ggplot2::element_rect(fill = "grey70", color = NA), # Fond pour le titre
        strip.text = ggplot2::element_text(color = "white", face = "bold"), # Texte du titre
        axis.title.x = ggplot2::element_blank(), # Pas de titre pour X
        axis.title.y = ggplot2::element_text(face = "bold")) # En gras pour Y

    if (est_i2m2) { # Si c'est l'I2M2
      p <- p +
        ggplot2::scale_y_continuous(
          name = "EQR", # Met seulement EQR de 0 à 1
          limits = c(0, 1),
          breaks = seq(0, 1, 0.25)) # Graduation tout les 0.25
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"), # Retourne le graph en interactif
          table = table_indice)) } # Table pour export CSV

    if (est_ips) { # Si c'est IPR
      p <- p +
        ggplot2::scale_y_continuous(
          name = "Résultat indice", # Seulement resultat pas de EQR
          limits = c(0, 20), # De 0 à 20
          breaks = seq(0, 20, 5))
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"), # Retourne en interactif
          table = table_indice) ) } # Table pour export CSV

    if (est_ipr) { # IPR
      p <- p +
        ggplot2::scale_y_continuous(
          name = "Résultat indice",
          limits = c(0, 70), # de 0 à 70
          breaks = seq(0, 70, 10))
      return(
        list(
          graph = plotly::ggplotly(p, tooltip = "text"),
          table = table_indice) ) }

    p <- p + # Autres indices : resultat + axe EQR
      ggplot2::scale_y_continuous(
        name = "Résultat indice", # Nom de l'axe gauche
        limits = c(0, 20), # Resultat indice entre 0 et 20
        breaks = seq(0, 20, 5)) # Graduations axe gauche

    # Passage en plotly et ajout de EQR a droite
    graph_indice <- plotly::ggplotly(p, tooltip = "text") %>% # Transforme le ggplot en graphique interactif
      plotly::add_trace( # Trace invisible pour faire apparaitre l'axe EQR
        data = data_indice, # Donnees de l'indice
        x = ~x_id, # Meme axe x que le graphique
        y = ~eqr_indice, # Valeurs EQR
        type = "scatter", # Type nuage de points
        mode = "markers", # Points
        yaxis = "y2", # Utilise le deuxieme axe y a droite
        marker = list(opacity = 0), # Points invisibles
        showlegend = FALSE, # Pas de legende
        hoverinfo = "skip", # Pas de survol pour ces points
        inherit = FALSE) %>% # Ne reprend pas les reglages ggplot
      plotly::layout(
        yaxis = list(range = c(0, 20)),
        yaxis2 = list(
          title = "EQR", # Mettre EQR a droite
          overlaying = "y",
          side = "right",
          range = c(0, max(data_indice$eqr_indice / data_indice$resultat_indice, na.rm = TRUE) * 20),
          tickvals = seq(0, 1, 0.25), # Attention l'axe doit etre adapté
          ticktext = c("0", "0.25", "0.50", "0.75", "1")),
        showlegend = FALSE) # Pas de legende

    list(
      graph = graph_indice, # Graphique final
      table = table_indice) } # Table pour export CSV , Fin de la petite fonction par indices

  # Un graphique par indice, applique la fonction a tous
  graphiques <- data_indices %>% # On repart de toutes les données de la station
    dplyr::group_by(libelle_indice) %>% # On regroupe par indice
    dplyr::group_split() %>% # On sépare en une table par indice
    purrr::map(tracer_un_indice) %>% # On applique la fonction à chaque indice
    purrr::compact() # On enlève les résultats NULL

  graphiques } # Renvoie la liste des graphiques, Fin de la fonction


#' Graphique des métriques I2M2
#'
#' @description Trace les métriques de l'I2M2 sous forme de graphique multicourbe
#' @param metriques Table des métriques I2M2
#' @param station_id Code de la station sélectionnée
#' @return Un graphique plotly
#' @noRd

fun_plot_metriques_i2m2 <- function(metriques, station_id) { # Fonction graphique metriques

  if (is.null(metriques) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL)} # Stop si rien

  # Preparation des donnees
  data_metriques <- metriques %>% # Table metriques
    dplyr::filter(code_station == station_id) %>% # Filtre station selectionnee
    dplyr::mutate(
      annee_num = as.integer(annee), # Annee en nombre pour trier
      annee = as.character(annee), # Annee en texte pour affichage
      resultat_indice = as.numeric(resultat_indice), # Valeur en nombre
      libelle_indice = as.character(libelle_indice), # Nom metrique
      texte_survol = paste0( # Texte au survol
        "Année : ", annee,
        "<br>Métrique : ", libelle_indice,
        "<br>Valeur : ", round(resultat_indice, 3) ) ) %>%
    dplyr::filter(!is.na(annee), !is.na(resultat_indice)) %>% # Enleve NA
    dplyr::arrange(annee_num) # Trie par annee

  if (nrow(data_metriques) == 0) {return(NULL) } # Si il n'y a pas de données

  # Creation du graphique
  p <- ggplot2::ggplot(
    data_metriques,
    ggplot2::aes(
      x = annee, # Axe x = annee
      y = resultat_indice, # Axe y = valeur
      color = libelle_indice, # Couleur par metrique
      group = libelle_indice, # Une courbe par metrique
      text = texte_survol ) ) + # Texte plotly
    ggplot2::geom_line(linewidth = 0.7) + # Ligne
    ggplot2::geom_point(size = 2.3) + # Points
    ggplot2::scale_x_discrete(
      limits = sort(unique(data_metriques$annee)) ) + # Ordre des annees
    ggplot2::labs(
      x = NULL, # Pas de titre x
      y = "Valeur de la métrique", # Titre axe y
      color = NULL ) + # Pas de titre legende
    ggplot2::theme_light() + # Theme clair
    ggplot2::theme(
      legend.position = "bottom", # Legende en bas
      axis.title.y = ggplot2::element_text(face = "bold") ) # Titre y en gras

  # Passage en plotly
  plotly::ggplotly(p, tooltip = "text") # Graph interactif
}
