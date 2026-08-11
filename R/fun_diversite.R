#' Fonction 1 : Calcul des indices de diversité
#' => abondance totale, richesse taxonomique,
#' indice de diversité de Shannon et l'équitabilité de Pielou
#' pour chaque prélèvement et chaque communauté biologique.
#'
#' Attention : le calucl est fait manuellement, ce qui me permet
#'  de controler si y'a un soucis et de se baser sur le travail de Julia.
#'
#' @return Une table contenant les métriques de diversité.
#' @noRd

fun_calcul_diversite <- function(table_taxons) {

# Conditions
  colonnes_attendues <- c( # Colonnes nécessaires au calcul dans la table taxons
    "code_station",
    "libelle_station",
    "date_prelevement",
    "code_prelevement",
    "code_support",
    "libelle_support",
    "code_appel_taxon",
    "libelle_taxon",
    "resultat_taxon")

  colonnes_absentes <- setdiff( # Compare les colonnes attendues et disponibles
    colonnes_attendues, # Colonnes nécessaires
    names(table_taxons) ) # Colonnes présentes dans la table

  if (length(colonnes_absentes) > 0) { # Au moins une colonne manquante
    stop( # Arrêt de la fonction
      paste( # Création du message d'erreur
        "Colonnes absentes de la table taxons :", # debut du message + affiche ce qui manquent pour le debug si jamais
        paste(
          colonnes_absentes, # Colonnes manquantes
          collapse = ", " ) ) ) }


# Préparation
  table_taxons <- table_taxons |> # On part de la table des taxons
    dplyr::mutate(
      date_prelevement = as.Date(date_prelevement), # Conversion en date
      resultat_taxon = as.numeric(resultat_taxon)) # Conversion en valeur numérique

  taxons_agreges <- table_taxons |> # On va regruper les taxons qui peuvent etre present dans les diffrentes phases
    dplyr::filter( # Filtre et garde les lignes ou il y a :
      !is.na(code_appel_taxon), # Un taxon renseigné
      !is.na(resultat_taxon), # Une abondance renseignée
      resultat_taxon > 0) |> # Et que l'abondance ets positive
    dplyr::group_by( # Regroupement par taxon et prélèvement
      code_station,
      libelle_station,
      date_prelevement,
      code_prelevement,
      code_support,
      libelle_support,
      code_appel_taxon,
      libelle_taxon ) |>
    dplyr::summarise( # Calcul de l'abondance totale de chaque taxon
      abondance = sum( # Somme des
        resultat_taxon, # Valeurs d'abondance
        na.rm = TRUE), # Ignore les valeurs manquante
      .groups = "drop" ) # Supprime le regroupement apres le calcul


# Calcul des indices de diversité pour chaque prélèvement

  table_diversite <- taxons_agreges |>
    dplyr::group_by( # Regroupement par prélèvement et communauté
      code_station,
      libelle_station,
      date_prelevement,
      code_prelevement,
      code_support,
      libelle_support  ) |>
     dplyr::summarise( # Calcul des métriques

      # 1) Abondance totale
      abondance_totale = sum(
        abondance, # Abondances des taxons
        na.rm = TRUE), # Ignore les valeurs manquantes

      # 2) Richesse taxo
      richesse_taxonomique = dplyr::n_distinct( # Nombre de taxons différents
        code_appel_taxon[abondance > 0] ), # Taxons avec une abondance positive

      # 3) Indice de diversité de Shannon
      diversite_shannon = {
        total <- sum( # Calcul de l'abondance totale et ignore les NA
          abondance,
          na.rm = TRUE)
        proportions <- abondance / total # Proportion de chaque taxon
        -sum( # Somme négative des proportions logarithmiques
          proportions * log(proportions), # Formule de l'indice de Shannon
          na.rm = TRUE ) },
      .groups = "drop" ) |> # Suppression des regroupements

    # Ajout de pielou + des années
     dplyr::mutate(

      #  4) Équitabilité de Pielou
      equitabilite_pielou = dplyr::if_else(
        richesse_taxonomique > 1, # Au moins deux taxons présents
        diversite_shannon / log(richesse_taxonomique), # Formule de Pielou
        NA_real_ ), # Valeur manquante si un seul taxon

      # Année du prélèvement
      annee = lubridate::year(date_prelevement) ) |> # Extraction de l'année
    dplyr::arrange(
      libelle_support, # Classement par communauté
      annee, # Puis par année
      date_prelevement ) # Puis par date

  return(table_diversite) } # Retourne la table des métriques


#-------------------------------------------------------------------------------------------------------
#' Mise au format long des indices de diversité pour le tableau final
#' @param table_diversite Table produite precedement
#' @noRd

fun_prep_diversite <- function(table_diversite) {

  table_diversite_longue <- table_diversite |>
    tidyr::pivot_longer(
      cols = c( # Colonnes à regrouper
        abondance_totale,
        diversite_shannon,
        equitabilite_pielou,
        richesse_taxonomique),
      names_to = "indice", # Colonne  indices qui va contenir les 4 indices
      values_to = "valeur"  ) |> # Colonne contenant la valeur associée

    dplyr::mutate( # Modification de la colonne indices
      indice = dplyr::recode( # Remplacement des noms
        indice,
        abondance_totale = "Abondance totale",
        diversite_shannon = "Diversité de Shannon",
        equitabilite_pielou = "Équitabilité de Pielou",
        richesse_taxonomique = "Richesse taxonomique"  ),

      indice = factor( # Transformation en facteur ordonné
        indice, # Colonne
        levels = c( # Ordre d'affichage
          "Abondance totale",
          "Diversité de Shannon",
          "Équitabilité de Pielou",
          "Richesse taxonomique" ) ) )

  return(table_diversite_longue) } # Retourne la table au format long


#----------------------------------------------------------------------------------------------------------------
#' Création du graphique des métriques de diversité
#' @return Un graphique ggplot.
#' @noRd

fun_plot_diversite <- function(table_diversite_longue) {

  shiny::validate( # Verification
    shiny::need(
      nrow(table_diversite_longue) > 0, # Au moins une ligne disponible
      paste( # Message affiché sinon
        "Aucune donnée de diversité disponible pour cette station." ) ) )

# Préparation des données du graphique
  table_graphique <- table_diversite_longue |>
    dplyr::filter( # Filtre et garde les lignes où il y a :
      !is.na(date_prelevement), # Une date de prélèvement
      !is.na(annee), # Une année
      !is.na(valeur), # Une valeur
      !is.na(libelle_support), # Une communauté
      !is.na(indice) ) |> # Un indice
    dplyr::mutate(
      date_prelevement = as.Date(date_prelevement ), # Conversion de la date
      support_court = dplyr::case_when( # Attribution d'un nom court notamment pour les titres
        grepl( # Recherche du texte
          pattern = "Macroinvert", # Texte recherché
          x = libelle_support, # Colonne analysée
          ignore.case = TRUE ) ~ "MIV", # Ignore les majuscules + nom donné
        grepl(
          pattern = "Diatom",
          x = libelle_support,
          ignore.case = TRUE ) ~ "Diatomées",
        grepl(
          pattern = "Macrophyt",
          x = libelle_support,
          ignore.case = TRUE ) ~ "Macrophytes",
        grepl(
          pattern = "Poisson",
          x = libelle_support,
          ignore.case = TRUE ) ~ "Poissons",
        TRUE ~ libelle_support ) ) |> # Conservation du nom initial sinon
    dplyr::arrange(
      support_court, # Par communauté
      indice, # Puis par indice
      date_prelevement, # Puis par date
      code_prelevement ) # Puis par prélèvement

  # Sécurité
  shiny::validate(
    shiny::need(
      nrow(table_graphique) > 0, # Au moins une valeur calculable
      paste( # Message affiché sinon
        "Aucune valeur calculable pour les communautés sélectionnées." ) ) )

# Ordre
  # Ordre des communautés dans le graphique
  ordre_supports_reference <- c(
    "Diatomées",
    "Macrophytes",
    "MIV",
    "Poissons")
  ordre_supports <- c(
    ordre_supports_reference[ # garde les communautés de référence uniquement si elles sont présentes dans les données
      ordre_supports_reference %in% table_graphique$support_court]) # Ex: si on a que MIV et diat ca va sortir Diat et MIV selon l'odre defini

  # Ordre des indices dans le graphique
  ordre_indices_reference <- c( # Defini l'ordre
    "Abondance totale",
    "Diversité de Shannon",
    "Équitabilité de Pielou",
    "Richesse taxonomique")
  ordre_indices <- ordre_indices_reference[ # Pour ceux present, meme fonctionnement que avant
    ordre_indices_reference %in% as.character(
      unique(table_graphique$indice)  ) ]

  # Ordre des panneaux sous forme de communautés-indices
  ordre_panneaux <- unlist( # Transforme la liste des combinaison en 1 seul vecteur utilisé pour l'ordre
    lapply(
      ordre_supports,
      function(support) {
        paste( # Associe
          support,
          ordre_indices,
          sep = " - " ) } ) ) # Separation entre la commu et l'indice


# Création des variables utilisées pour l'affichage
  table_graphique <- table_graphique |>
    dplyr::mutate(
      support_court = factor( # Met en facteur
        support_court, # Ordre des communautés
        levels = ordre_supports), # Impose l'ordre defini
      indice = factor(
        indice, # Ordre des indices
        levels = ordre_indices),
      panneau = paste( # Titre des panneaux
        support_court,
        indice,
        sep = " - " ),
      panneau = factor(
        panneau,# Ordre des panneaux
        levels = ordre_panneaux ),
      date_axe = factor( # Date en facteur
        date_prelevement, # Permet d'avoir un espaces régulier entre chaque date
        levels = sort(  # Classe de facon chronologique
          unique(date_prelevement) ) ),# Ne prend pas de doublons
      etiquette_annee = format( # Pour l'affichage
        date_prelevement,
        format = "%Y")) # Extrait uniquement l'année a partir de la date

  # Association entre chaque date et son année
  etiquettes_dates <- table_graphique |> # Sous forme de table
    dplyr::distinct( # Garde une seule fois chaque date et son année
      date_axe,
      etiquette_annee) |>
    dplyr::arrange( # Classe les dates dans l'ordre chronologique
      date_axe)
  # Correspondance entre les dates et les années affichées sur l'axe, sous forme de vecteur pour le scale
  etiquettes_annees <- stats::setNames( # Associe chaque date à son année
    object = etiquettes_dates$etiquette_annee, # Années à afficher
    nm = as.character(
      etiquettes_dates$date_axe) ) # Dates utilisées sur l'axe



# Création du graphique
  graphique <- ggplot2::ggplot(
    data = table_graphique,
    mapping = ggplot2::aes(
      x = date_axe,
      y = valeur,
      group = 1, # Relie les points d'un même panneau

      # Survol
      text = paste0(
        "Communauté : ",
        libelle_support,
        "<br>Indice : ",
        indice,
        "<br>Date : ",
        format(
          date_prelevement,
          format = "%d/%m/%Y" ),
        "<br>Année : ",
        annee,
        "<br>Valeur : ",
        round( # Arrondi la valeur
          valeur,
          digits = 3 ) ) ) ) + # Trois chiffres après la virgule

    # Ajout des lignes et des points
    ggplot2::geom_line(
      linewidth = 0.6, # Épaisseur des lignes
      na.rm = TRUE ) + # Ignore les valeurs manquantes
    ggplot2::geom_point(
      size = 2, # Taille des points
      na.rm = TRUE ) +

    # Un panneau par communauté et par indice
    ggplot2::facet_wrap( # Séparation en panneaux (= plusieurs petits graphs)
      facets = ggplot2::vars(
        panneau ), # Variable utilisée pour les panneaux
      ncol = 4, # Quatre panneaux par ligne
      scales = "free" ) + # Axes x et y propres à chaque panneau

    # Paramétrage de l'axe des prélèvements
    ggplot2::scale_x_discrete(
      labels = etiquettes_annees, # Affichage de l'année à la place de la date
      drop = TRUE, # Supprime les dates absentes dans chaque panneau
      expand = ggplot2::expansion(
        add = c(0.15,  0.15 ) ) ) + # Petite marge de chaque coté

    # Paramétrage de l'axe vertical
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        NA ), # Tous les axes commencent à zéro et la limite sup y'en a pas
      expand = ggplot2::expansion(
        mult = c( 0, 0.05 ) ) ) + # Pas de marge sous zéro et petite marge au-dessus

    # Nom des axes
    ggplot2::labs(
      x = "Année", # Titre de l'axe horizontal
      y = NULL ) + # Aucun titre général pour l'axe vertical

    # Réglage de base
    ggplot2::theme_bw() + # Thème blanc du graphique
    ggplot2::theme(
      strip.text = ggplot2::element_text( # Texte des titres
        size = 9, # Taille du texte
        face = "bold" ), # Texte en gras
      strip.background = ggplot2::element_rect( # Fond des titres
        linewidth = 0.5 ), # Epaisseur du contour
      axis.text.x = ggplot2::element_text( # Texte de l'axe des années
        angle = 45, # Inclinaison des années
        hjust = 1, # Alignement à droite
        vjust = 1, # Alignement vertical
        size = 7 ),
      axis.text.y = ggplot2::element_text( # Texte de l'axe vertical
        size = 8 ),
      axis.title.x = ggplot2::element_text( # Titre de l'axe horizontal
        size = 10,
        margin = ggplot2::margin( t = 5) ),
      panel.grid.major.x = ggplot2::element_line( # Grille verticale
        linewidth = 0.25 ),
      panel.grid.major.y = ggplot2::element_line( # Grille horizontale = elles facilitent la lecture
        linewidth = 0.25 ),
      panel.grid.minor = ggplot2::element_blank(), # Suppression de la grille secondaire
      panel.spacing.x = grid::unit( # Espace horizontal entre les graphiques
        1,# Permt de bien voir les années
        units = "lines" ),
      panel.spacing.y = grid::unit( # Espace vertical entre les graphiques
        0.8,
        units = "lines" ),
      plot.margin = ggplot2::margin( # Marges générales du graphique
        t = 3,
        r = 3,
        b = 3,
        l = 3 ),
      legend.position = "none" ) # Suppression de la légende

  return(graphique)}# Retourne le graphique
