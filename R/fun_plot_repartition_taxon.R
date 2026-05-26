#' Graphique du nombre de stations par année pour un taxon
#'
#' @description Cette fonction affiche le nombre de stations où le taxon est present par an
#'
#' @param donnees Liste contenant les objets de l'application
#' @param choix_departements Départements sélectionnés
#' @param choix_eqb EQB sélectionnés
#' @param choix_uh UH sélectionnées
#' @param choix_reseau Reseau sélectionné
#' @param choix_qualification
#' @param taxon_selectionne Taxon sélectionné
#' @return Un graphique plotly
#' @noRd

fun_plot_repartition_taxon <- function(donnees,
                                       choix_departements = NULL,
                                       choix_eqb = NULL,
                                       choix_uh = NULL,
                                       choix_reseau = NULL,
                                       choix_qualification = NULL,
                                       taxon_selectionne = NULL) {

  df <- fun_filtrer_repartition_taxon( # Appel la fonction selon les filtres
    donnees = donnees,
    choix_departements = choix_departements,
    choix_eqb = choix_eqb,
    choix_uh = choix_uh,
    choix_reseau = choix_reseau,
    choix_qualification = choix_qualification,
    taxon_selectionne = taxon_selectionne)

  # Vérification
  if (is.null(df) || nrow(df) == 0) { return (NULL) } # Si aucune donnée, arrete

  # Permet d'avoir le nombre de stations où le taxon est présent par année
  df_plot <- df |>
    sf::st_drop_geometry() |> # Supprime la géométrie
    dplyr::mutate( # Extraction + nettoyage des années
      annee = purrr::map( # Boucle pour chaque ligne
        resume, # Texte résumé
        ~ { annees <- stringr::str_extract_all( # Extraction des années
            .x, "\\b(19|20)\\d{2}\\b" )[[1]] # Commence par un 19 ou 20 et suivi de 2 chiffres
          annees <- as.integer(annees) # Conversion en entier
          annees <- annees[ # Garde les années réalistes
            annees >= 1980 &
              annees <= lubridate::year(Sys.Date())]
          if (length(annees) == 0) {return(NULL)} # Si aucune année
          seq( # Création de la séquence continue
            min(annees),
            max(annees) )  } ) ) |>

    tidyr::unnest(annee) |> # Déplie les années
    dplyr::group_by( # Regroupement
      libelle_taxon, # Taxon
      annee ) |> # Année
    dplyr::summarise( # Calcul du nombre de stations
      nb_stations = dplyr::n_distinct(code_station), # Nombre de stations uniques
      .groups = "drop" ) # Supprime le regroupement

# Création du graph
  gg <- ggplot2::ggplot(
    df_plot, # Données du graphique
    ggplot2::aes(
      x = annee, # Axe X = année
      y = nb_stations, # Axe Y = nombre de stations
      fill = libelle_taxon, # Couleur par taxon
      text = paste0( # Texte affiché au survol
        "Taxon : ", libelle_taxon,
        "<br>Année : ", annee,
        "<br>Nombre de stations : ", nb_stations ) ) ) +
    ggplot2::geom_col( # Histogramme
      color = "black", # Couleur contour
      position = "dodge" ) + # Barres côte à côte
    ggplot2::scale_x_continuous( # Paramètres axe X
      breaks = seq( # Affiche 1 année sur 5
        min(df_plot$annee, na.rm = TRUE), # Année min
        max(df_plot$annee, na.rm = TRUE), # Année max
        by = 5 ) ) + # Pas de 5 ans
    ggplot2::labs( # Titres du graphique
      title = "Nombre de stations où les taxons sont présents par année", # Titre
      x = "Année", # Titre axe X
      y = "Nombre de stations", # Titre axe Y
      fill = "Taxon" ) + # Titre légende
    ggplot2::theme_minimal() + # Theme minimal
    ggplot2::theme( # Personnalisation du thème
      axis.text.x = ggplot2::element_text(
        angle = 45, # Inclinaison du texte
        hjust = 1 ) ) # Alignement du texte

  plotly::ggplotly( # Conversion en ploty
    gg,
    tooltip = "text") %>% # Texte affiché au survol
    plotly::config(
      toImageButtonOptions = list(
        format = "png", # Format
        filename = paste0(
          "repartition_taxon_",
          taxon_selectionne ), # Nom fichier
        height = 800, # Hauteur
        width = 1200, # Largeur
        scale = 2 ) )# Qualité
}
