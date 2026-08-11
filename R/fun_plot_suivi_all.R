#' Fonction : Création du diagramme alluvial de suivi
#'
#' Permet de représenté l'évolution pour le ou les réseaux d'appartenance
#' de la station sélectionnée en fonction de la classe la plus declassante.
#' @return Un objet ggplot.
#' @noRd

fun_plot_suivi_alluvial <- function( etat_bio,
                                     stations,
                                     station_selectionnee) {

# Préparation
  station_selectionnee <- as.character( station_selectionnee) # Code en texte

  if (inherits(stations, "sf")) { # Si presence d'une géometrie spatiale
    stations <- sf::st_drop_geometry(stations ) } # Suprime

  couleurs_classes <- c( # Définition des classes de couleurs
    "Mauvais" = "#d73027",
    "Médiocre" = "#fc8d59",
    "Moyen" = "#fee08b",
    "Bon" = "#91cf60",
    "Très bon" = "#4575b4",
    "Non évalué" = "#FFFFFF")

#----------------Pour les réseaux----------------------------
# Préparation des réseaux d'appartenance des stations
  # Une station peut evoir plusieur réseaux
  stations_reseaux <- stations |>
    dplyr::select( # Selection des colonnes utiles ici
      .data$code_station,
      .data$libelle_station,
      .data$reseau  ) |>
    dplyr::mutate( # Conversion de format
      code_station = as.character(
        .data$code_station ),
      libelle_station = as.character(
        .data$libelle_station ),
      reseau = as.character(
        .data$reseau ) ) |>
    dplyr::filter(
      !is.na(.data$code_station), # Garde les stations avec un code
      !is.na(.data$reseau), # Garde les stations avec un réseau renseigné
      .data$reseau != "" ) |> # Supprime les réseaux vides

    tidyr::separate_rows( # Séparateur des réseaux dans la table
      .data$reseau,
      sep = "[/-]" ) |>

    dplyr::mutate( # Nettoyage des noms de réseaux pour uniformiser
      reseau = stringr::str_trim( # Suppresion des espaces
        .data$reseau ),
      reseau = stringr::str_to_upper( # Tout en majuscules
        .data$reseau ) ) |>
    dplyr::filter( # Suppression des réseaux devenu vide
      .data$reseau != "" ) |>
    dplyr::distinct( # Suppression des doublons
      .data$code_station,
      .data$libelle_station,
      .data$reseau )

# Recherche du réseaux de la station sélectionnée
  reseaux_station_selectionnee <- stations_reseaux |>
    dplyr::filter( # Grade la station selectionnee
      .data$code_station ==
        station_selectionnee) |>
    dplyr::distinct( .data$reseau ) |> # Conserve une seule ligne par réseaux qi y'en a plusieur ( ex: RCB/RCS)
    dplyr::pull( .data$reseau) # Met le réseaux en vecteur
  if (length(reseaux_station_selectionnee) == 0) { # Verif
    stop( # Arret si y'a plus rien avec le message suivant
      paste(
        "Aucun réseau n'est renseigné pour la station",
        station_selectionnee) ) }

# Sélection de toutes les stations appartenant au même réseau que la station choisie
  stations_reseaux_selectionnes <- stations_reseaux |>
    dplyr::filter( # Filtre selon le reseau de la station sel
      .data$reseau %in%
        reseaux_station_selectionnee ) |>
    dplyr::distinct( # Suppression des doublons
      .data$reseau,
      .data$code_station,
      .data$libelle_station)

#---------------------- Pour les classes-----------------------
# Préparation de la table contenant les états biologiques
  table_etat_preparee <- etat_bio |>
    dplyr::mutate( # Harmonisation des formats
      code_station = as.character(
        .data$code_station ),
      annee = as.integer(
        .data$annee),
      code_indice = as.character(
        .data$code_indice ),
      libelle_indice = as.character(
        .data$libelle_indice ),
      classe_indice = as.character(
        .data$classe_indice ) ) |>

    dplyr::inner_join( # Conserve uniquement les stations presentes dans les réseaux sel
      stations_reseaux_selectionnes, # Association aux resaux
      by = "code_station") |>

    dplyr::mutate( # Harmonisation
      rang_classe = dplyr::case_when( # Création d'un rang qui permet de trouver le plus déclassant
        .data$classe_indice == "TRES_BON" ~ 1,
        .data$classe_indice == "BON" ~ 2,
        .data$classe_indice == "MOYEN" ~ 3,
        .data$classe_indice == "MEDIOCRE" ~ 4,
        .data$classe_indice == "MAUVAIS" ~ 5,
        TRUE ~ NA_real_  ) ) |>

    dplyr::filter( # Suppression des lignes sans année ou classe
      !is.na(.data$annee),
      !is.na(.data$rang_classe) ) |>

    dplyr::distinct( # Suppression des doublons
      .data$reseau,
      .data$code_station,
      .data$libelle_station,
      .data$annee,
      .data$code_indice,
      .data$libelle_indice,
      .data$classe_indice,
      .data$rang_classe)


  if (nrow(table_etat_preparee) == 0) { # Verif
    stop( # Stop si plus rien
      paste(
        "Aucune donnée d'état biologique exploitable",
        "n'est disponible pour les réseaux de la station sélectionnée.") ) }

# Sélection des stations possédant au moins une classe
  stations_avec_donnees <- table_etat_preparee |> # Evite d'ajouter des stations "Non évalué"
    dplyr::distinct(
      .data$reseau,
      .data$code_station,
      .data$libelle_station)

# Détermination du plus déclassant pour chaque station, réseau et année
  etat_declassant <- table_etat_preparee |>
    dplyr::group_by( # Groupe par réseaux, station et année
      .data$reseau,
      .data$code_station,
      .data$annee ) |>
    dplyr::filter( # Conservation du rang de classe defini en haut le plus élévé
      .data$rang_classe ==
        max(
          .data$rang_classe,
          na.rm = TRUE ) ) |>
    dplyr::summarise( # Création d'une seule ligne par station et année
      rang_classe = dplyr::first(
        .data$rang_classe   ), # Conservation du rang le plus déclassant
      classe_indice = dplyr::first(
        .data$classe_indice ), # Conservation de la classe la plus déclassante
      indice_declassant = paste( # Par exemple si IBD et IBMR sont declassant = IBD/IBMR
        sort( # Ordre alpha
          unique( # Pas de doublon
            .data$libelle_indice[!is.na(.data$libelle_indice) ] ) ), # Enleve les Na
        collapse = " / " ), # Regroupement des indices responsables du déclassement
      .groups = "drop") # Suppression des groupes créés précédemment


# Détermination des années disponibles pour chaque réseau
  annees_reseaux <- etat_declassant |>
    dplyr::group_by( # Création d'un groupe pour chaque reseau
      .data$reseau ) |>
    dplyr::summarise(
      annee_min = min(
        .data$annee,
        na.rm = TRUE ), # Première année disponible pour le réseau
      annee_max = max(
        .data$annee,
        na.rm = TRUE ), # Dernière année disponible pour le réseau
      .groups = "drop" ) |> # Suppression des groupes
    dplyr::rowwise() |> # Traitement de chaque réseau ligne par ligne
    dplyr::mutate( # Création d'une suite
      annee = list(
        seq(
          from = .data$annee_min, # Début de la suite
          to = .data$annee_max, # Fin de la suite
          by = 1) ) ) |>  # Progression année par année
    dplyr::ungroup() |>  # Suppression du traitement ligne par ligne
    tidyr::unnest( # Liste d'année en plusieurs lignes
      cols = .data$annee) |>
    dplyr::select( # Conservation des colones utiles
      .data$reseau,
      .data$annee)

# Création d'une ligne pour chaque station et chaque année disponible dans son réseau
  suivi_complet <- stations_avec_donnees |>
    dplyr::inner_join( # Association de chaque station à toutes les années de son réseau
      annees_reseaux,
      by = "reseau") |>
    dplyr::left_join( # Ajout de la classe
      etat_declassant,
      by = c(
        "reseau",
        "code_station",
        "annee" ) ) |>

    dplyr::mutate( # Création de variable pour le graph
      classe_indice = dplyr::coalesce(
        .data$classe_indice, # Pas de Na = remplacer par Non Evalue
        "NON_EVALUE" ),
      etat = dplyr::case_when( # Création des nom pour le graph
        .data$classe_indice == "MAUVAIS" ~ "Mauvais",
        .data$classe_indice == "MEDIOCRE" ~ "Médiocre",
        .data$classe_indice == "MOYEN" ~ "Moyen",
        .data$classe_indice == "BON" ~ "Bon",
        .data$classe_indice == "TRES_BON" ~ "Très bon",
        TRUE ~"Non évalué" ),
      etat = factor(
        .data$etat, # Transormation en facteur ordonnée pour l'affichage
        levels = c(
          "Mauvais",
          "Médiocre",
          "Moyen",
          "Bon",
          "Très bon",
          "Non évalué"),
        ordered = TRUE ),
      station_suivie = # Iddentification de la station sel
        .data$code_station ==
        station_selectionnee,
      identifiant_alluvion = paste( # Creation d'un iddentifiant unique pour suivre la station
        .data$reseau,
        .data$code_station,
        sep = "_" ),
      annee_facteur = factor( # Transformation de l'année en facteur ordonné
        .data$annee,
        levels = sort(
          unique(
            .data$annee ) ),
        ordered = TRUE ) ) |>

    dplyr::arrange( # Organisation des lignes avant la création, Attention : la station sel est dessinée apres les autres
      .data$reseau,
      .data$station_suivie,
      .data$code_station,
      .data$annee )

  if (!any(suivi_complet$station_suivie)) { # Verif
    stop( # Arret de la fonction
      paste(
        "La station ne possède aucune donnée biologique exploitable." ) ) }

#---------------------------- Graphique-------------------------
# Création du graph
  graphique <- ggplot2::ggplot(
    data = suivi_complet, # Table utilisée

    mapping = ggplot2::aes( # Variables communes au differentes couches
      x = .data$annee_facteur, # Années sur l'axe horizontal
      stratum = .data$etat, # Classes formant les blocs verticaux
      alluvium = .data$identifiant_alluvion, # Suivi individuel des stations
      y = 1, # Chaque ligne représente une station
      fill = .data$etat ) ) + # Couleur selon l'état biologique

    # Création des flux entre les années
    ggalluvial::geom_flow(
      mapping = ggplot2::aes(
        colour = .data$station_suivie, # Rouge pour la station sélectionnée
        linewidth = .data$station_suivie, # Flux plus épais pour cette station
        alpha = .data$station_suivie), # Flux plus visible pour cette station
      stat = "alluvium", # Calcul des trajectoires alluviales
      curve_type = "xspline", # Forme arrondie des flux
      reverse = FALSE) + # Conservation de l'ordre défini des classes

    # Création des rectangles représentant les classes et leurs effectifs pour chaque année
    ggalluvial::geom_stratum(
      colour = "grey35", # Couleur du contour des rectangles
      linewidth = 0.35, # Épaisseur du contour
      alpha = 0.9, # Transparence des rectangles
      reverse = FALSE) + # Conservation de l'ordre des classes

    # Création d'un graphique séparé pour chaque réseau ( si y'en a 2 par ex)
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        .data$reseau ), # Une facette par réseau
      ncol = 1, # Réseaux placés les uns sous les autres
      scales = "free_y") + # Axe vertical adapté à l'effectif de chaque réseau

    # Attribution des couleurs aux classes
    ggplot2::scale_fill_manual(
      values = couleurs_classes, # Couleurs définies au début
      breaks = c(
        "Mauvais",
        "Médiocre",
        "Moyen",
        "Bon",
        "Très bon",
        "Non évalué" ), # Ordre des classes dans la légende
      drop = FALSE) + # Conservation des classes absentes dans la légende

    # Attribution de la couleur des contours des flux
    ggplot2::scale_colour_manual(
      values = c(
        "FALSE" = "grey75", # Autres stations en gris
        "TRUE" = "red"),  # Station sélectionnée en rouge
      breaks = TRUE, # Affichage uniquement de la station suivie dans la légende
      labels = paste(
        "Station sélectionnée :",
        station_selectionnee ), # Texte affiché dans la légende
      name = NULL ) + # Aucun titre pour cette légende

    # Attribution de l'épaisseur des flux
    ggplot2::scale_linewidth_manual(
      values = c(
        "FALSE" = 0.1, # Flux fins pour les autres stations
        "TRUE" = 1.1 ), # Flux épais pour la station sélectionnée
      guide = "none") + # Masquage de la légende de l'épaisseur

    # Attribution de la transparence des flux
    ggplot2::scale_alpha_manual(
      values = c(
        "FALSE" = 0.3, # Autres stations plus transparentes
        "TRUE" = 1 ), # Station sélectionnée entièrement visible
      guide = "none") +  # Masquage de la légende de transparence

    # Pour les axes
    ggplot2::scale_x_discrete( drop = FALSE ) + # Empêche la suppression des années sans données
    ggplot2::scale_y_continuous( # Axe vertical
      breaks = scales::breaks_pretty(), # Création de graduations lisibles
      expand = ggplot2::expansion(
        mult = c(
          0, # Aucun espace supplémentaire sous le graphique
          0.02 ) ) ) + # Petit espace supplémentaire au-dessus
    ggplot2::labs( # Titre
      title = paste(
        "Évolution de l'état biologique des stations par réseau" ),
      subtitle = paste(
        "L'état retenu correspond à l'indice biologique le plus déclassant pour chaque station et chaque année.",
        "La station",
        station_selectionnee,
        "est suivie en rouge." ),
      x = "Années", # Nom de l'axe horizontal
      y = "Nombre de stations", # Nom de l'axe vertical
      fill = "État biologique" ) +  # Titre de la légende des couleurs

    # Personnalisation
    ggplot2::theme_bw() + # Theme blanc
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 12, # Taille du titre defini juste en haut
        face = "bold" ), # Titre en gras
      plot.subtitle = ggplot2::element_text(
        size = 8), # Taille du sous-titre
      strip.background = ggplot2::element_rect(
        fill = "grey88", # Fond du nom des réseaux
        colour = "grey60"), # Contour du fond
      strip.text = ggplot2::element_text(
        size = 11, # Taille du nom des réseaux
        face = "bold"), # Nom des réseaux en gras
      axis.text.x = ggplot2::element_text(
        size = 8, # Taille des années
        angle = 45, # Inclinaison des années
        hjust = 1), # Alignement des années

      panel.grid.major.x = ggplot2::element_blank(),  # Suppression des lignes principales verticales
      panel.grid.minor = ggplot2::element_blank(), # Suppression des lignes secondaires
      legend.position = "bottom", # Placement des légendes sous le graphique
      legend.box = "vertical", # Placement des différentes légendes les unes sous les autres
      legend.title = ggplot2::element_text(
        face = "bold") ) + # Titre de la légende en gras

    # Organisation des différentes légendes
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1, # Légende des états affichée en premier
        nrow = 1, # Légende affichée sur une seule ligne
        byrow = TRUE) , # Remplissage de la légende ligne par ligne
      colour = ggplot2::guide_legend(
        order = 2, # Légende de la station suivie affichée en deuxième
        override.aes = list(
          linewidth = 1.1, # Trait rouge épais dans la légende
          alpha = 1 ) ) ) # Trait totalement visible dans la légende

  return(graphique) }
