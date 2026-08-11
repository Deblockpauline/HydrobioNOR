#' Graphique des taxons par station
#' @description Affiche les taxons observés par année pour une station.
#' La taille des points correspond à l'abondance relative
#' @return Graphique plotly
#' @noRd

fun_plot_taxons_station <- function(taxons, eee) {
  df <- taxons # Copie de la table filtrée

# Liste des EEE
  eee_taxons <- eee |> # Table des EEE
    dplyr::filter(!is.na(libelle_taxon)) |> # Enlève les noms vides
    dplyr::distinct(libelle_taxon) |> # Garde un nom par espèce
    dplyr::pull(libelle_taxon) # Extrait les noms des EEE

  ordre_taxons <- df |> # Création de l'ordre des taxons
    dplyr::distinct(libelle_taxon) |> # Garde 1 ligne par taxon
    dplyr::arrange(libelle_taxon) |> # Trie par ordre alphabétique
    dplyr::pull(libelle_taxon) # Extrait le nom des taxons

  df <- df |> # Modification de la table
    dplyr::mutate(
      libelle_taxon = factor( # Transformation en facteur
        libelle_taxon,
        levels = rev(ordre_taxons) ) ) # Inverse l'ordre affiché

  hauteur_graph <- max( # Définit la hauteur du graphique
    650, # Hauteur minimale
    length(unique(df$libelle_taxon)) * 28) # Augmente selon le nb de taxons


# Création des labels de l'axe Y
  labels_taxons <- setNames(
    vapply(
      ordre_taxons,
      function(x) {
        if (x %in% eee_taxons) {
          paste0(
            "<span style='color:#2E8B57;'><b>",
            x,
            "</b></span>" )
        } else { x }  },
      character(1)),
    ordre_taxons  )

  p <- ggplot2::ggplot( # Création du ggplot
    df, # Données utilisées
    ggplot2::aes( # Variables du graphique
      x = annee, # Axe X = année
      y = libelle_taxon, # Axe Y = taxons
      size = abondance_relative, # Taille des points = abondance
      text = paste0( # Texte affiché au survol
        "<b>Taxon :</b> ", libelle_taxon, # Nom taxon
        "<br><b>Année :</b> ", annee, # Année
        "<br><b>Prélèvement :</b> ", code_prelevement, # Code prélèvement
        "<br><b>Support :</b> ", libelle_support, # Support
        "<br><b>Résultat taxon :</b> ", resultat_taxon, # Valeur taxon
        "<br><b>Abondance relative :</b> ",
        round(abondance_relative, 4),
        "<br><b>Qualification :</b> ", libelle_qualification, # Qualifiaction
        "<br><b>Réseau :</b> ", reseau ) ) ) + # Reseau
    ggplot2::geom_point( # Ajout des points
      alpha = 0.8) + # Transparence
    ggplot2::scale_size_continuous( # Taille des points
      range = c(1, 10)) + # Taille min et max
    ggplot2::scale_x_continuous( # Paramètres axe X
      breaks = sort(unique(df$annee) ) ) + # Affiche toutes les années
    ggplot2::scale_y_discrete( # Paramètres axe Y
      labels = labels_taxons ) + # Colore les noms des EEE
    ggplot2::labs( # Titres du graphique
      x = NULL, # Pas de titre axe X
      y = NULL, # Pas de titre axe Y
      size = "Abondance relative") + # Titre légende taille
    ggplot2::theme_minimal() + # Thème minimal
    ggplot2::theme( # Personnalisation graphique
      axis.text.y = ggplot2::element_text(size = 9), # Taille texte Y
      axis.text.x = ggplot2::element_text(size = 9), # Taille texte X
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey90"), # Lignes horizontales
      panel.grid.minor = ggplot2::element_blank(), # Supprime grille mineure
      legend.position = "none") # Supprime légende

  plotly::ggplotly( # Transformation en plotly interactif
    p, # Graphique ggplot
    tooltip = "text", # Tooltip personnalisé
    height = hauteur_graph) |> # Hauteur dynamique
    plotly::layout( # Mise en page plotly
      margin = list( # Marges du graphique
        l = 230, # Marge gauche
        r = 20, # Marge droite
        t = 30, # Marge haut
        b = 60), # Marge bas
      xaxis = list( # Paramètres axe X
        side = "top", # Axe affiché en haut
        title = ""), # Pas de titre
      yaxis = list( # Paramètres axe Y
        title = "", # Pas de titre
        automargin = TRUE)) # Ajuste marges automatiquement

}
