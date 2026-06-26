#' Fonction de nettoyage et reférentiel
#' @noRd

# 1) Convertir l'occurrence en pourcentage numérique
fun_parse_occurrence_inv <- function(x) {
  x |> # Prend X en entrée ( colonne defini par la suite)
    as.character() |> # Transforme les valeurs en texte
    stringr::str_replace("%", "") |> # Enlève le symbole %
    stringr::str_replace(",", ".") |>  # Remplace les virgules par des points
    as.numeric()} # Retransforme en numérique


# 2) Référentiel des substrats macroinvertébrés
fun_ref_substrats_inv <- function() {
  tibble::tribble( # Crée un tableau de correspondance
    ~Code, ~Substrat, # Nom des colonnes
    1,  "Bryophytes",
    2,  "Hydrophytes",
    3,  "Litières",
    4,  "Branch, racines",
    5,  "Pierres, galets",
    6,  "Blocs",
    7,  "Granulats",
    8,  "Hélophytes",
    9,  "Vases",
    10, "Sables, limons",
    11, "Algues",
    12, "Dalles, argiles" )}

# -----------------------------------------------------------------------------------
#' Préparer les données du graphique macroinvertébrés
#'
#' @noRd

fun_prep_plan_inv_graph <- function(plan_inv_facies,
                                    plan_inv_phases,
                                    code_station_selectionne = NULL) {

  ref_substrats <- fun_ref_substrats_inv() # Récupère le référentiel créé plus haut avec la fonction

### Prepare la table des substrats + occurence
  facies_station <- plan_inv_facies |> # Part de la table des faciès
    dplyr::filter(Type == "Substrat") # Garde seulement les lignes de type substrat (car on veut que les occurences)
  if (!is.null(code_station_selectionne)) { # Si une station est sélectionnée
    facies_station <- facies_station |>
      dplyr::filter(code_station == code_station_selectionne) } # Et garde que cette station

  facies_station <- facies_station |> # Reprend les données de substrats et occurence
    dplyr::mutate(
      Code = as.numeric(Code), # Transforme le code du substrat en nombre
      Substrat = as.character(Faciès), # Crée une colonne Substrat à partir de la colonne Faciès
      occurrence_pct = fun_parse_occurrence_inv(Occurence) ) |> # Convertit l'occurrence dans le bon format grace a la fonction
    dplyr::select( # Garde seulement les colonnes utiles
      code_station,
      date_prelevement,
      annee,
      Code,
      Substrat,
      occurrence_pct ) |>
    dplyr::filter(!is.na(occurrence_pct)) |> # Enlève les lignes où l'occurrence est vide
    dplyr::mutate(
      Substrat = factor(Substrat, levels = ref_substrats$Substrat) ) # Met les substrats dans l'ordre du référentiel

### Prepare la table des phases
  phases_station <- plan_inv_phases |> # Part de la table des phases
    dplyr::filter(!is.na(`Numéro bocal`)) |> # Garde seulement les lignes avec une phase
    dplyr::mutate(
      zone_facies = stringr::str_remove(  # Nettoie le début du texte
        `Zones de faciès`,
        "^\\s*\\d+\\s*-\\s*Couple substrat/vitesse\\s*:\\s*" ), # Pour garder seulement vitesse + substrat
      Substrat = stringr::str_trim( # Enlève la vitesse pour garder sueelment le substrat
        stringr::str_remove(zone_facies, "^[^,]+,\\s*") ),
      phase = as.character(`Numéro bocal`) ) # Crée une colonne phase à partir du numéro de bocal

  if (!is.null(code_station_selectionne)) { # Si une station est sélectionnée
    phases_station <- phases_station |>
      dplyr::filter(code_station == code_station_selectionne) } # Garde seulement cette station

  phases_station <- phases_station |> # Reprend les données de phases nettoyées
    dplyr::select( # Garde seulement les colonnes
      code_station,
      date_prelevement,
      annee,
      Substrat,
      phase ) |>
    dplyr::left_join(
      ref_substrats,
      by = "Substrat" ) |> # Ajoute le code du substrat grâce au référentiel
    dplyr::filter(!is.na(Code)) |> # Enlève les substrats qui ne sont pas reconnus
    dplyr::select( # Réorganise les colonnes
      code_station,
      date_prelevement,
      annee,
      Code,
      Substrat,
      phase ) |>
    dplyr::distinct() |> # Supprime les doublons éventuels
    dplyr::mutate(
      Substrat = factor(Substrat, levels = ref_substrats$Substrat) ) # Met les substrats dans l'ordre du référentiel

### Prepare la table qui sert au graph
  facies_station |> # Part des données de substrats avec occurrence
    dplyr::left_join( # Joins les deux avec ca
      phases_station,
      by = c(
        "code_station",
        "date_prelevement",
        "annee",
        "Code",
        "Substrat" ) ) |> # Ajoute les phases A/B/C aux substrats correspondants
    dplyr::mutate( # Ajoite une colonne qui :
      phase_A = phase == "Phase A", # TRUE si le substrat est prélevé en phase A
      phase_B = phase == "Phase B", # TRUE si le substrat est prélevé en phase B
      phase_C = phase == "Phase C" ) |> # TRUE si le substrat est prélevé en phase C
    dplyr::group_by( # Regroupe les données par station, date, année et substrat
      code_station,
      date_prelevement,
      annee,
      Code,
      Substrat,
      occurrence_pct ) |>
    dplyr::summarise(
      phase_A = any(phase_A, na.rm = TRUE), # Indique si la phase A existe au moins une fois
      phase_B = any(phase_B, na.rm = TRUE), # Indique si la phase B existe au moins une fois
      phase_C = any(phase_C, na.rm = TRUE), # Indique si la phase C existe au moins une fois
      .groups = "drop"  ) |> # Enlève le regroupement après le résumé
    dplyr::arrange(annee, Code) |> # Trie les lignes par année puis par code substrat
    dplyr::mutate(
      annee = factor(annee), # Transforme l'année en facteur pour l'affichage du graphique
      Substrat = factor(Substrat, levels = ref_substrats$Substrat) ) }# Garde l'ordre des substrats

# -----------------------------------------------------------------------------------
#' Creation des graphiques et tableaux
#' #' @noRd

# 1) Graphique plan d'échantillonnage macroinvertébrés
fun_plot_plan_inv <- function(df,
                              code_station_selectionne = NULL) {

  if (nrow(df) == 0) {return(NULL) } # Si le tableau est vide, ne retourne aucun graphique
  ref_substrats <- fun_ref_substrats_inv() # Récupère le référentiel des substrats

  df <- df |> # Reprend le tableau préparé
    dplyr::mutate(
      Substrat = factor(
        as.character(Substrat), # Transforme le substrat en texte
        levels = ref_substrats$Substrat) ) # Garde l'ordre des substrats du référentiel

  # Mise en place du graph
  p <- ggplot2::ggplot(
    df, # Données utilisées pour le graphique
    ggplot2::aes(
      x = annee, # Années en abscisse
      y = Substrat, # Substrats en ordonnée
      fill = occurrence_pct ) ) + # Couleur des cases selon l'occurrence
    ggplot2::geom_tile(
      color = "grey80", # Couleur des contours des cases
      linewidth = 0.4) + # Épaisseur des contours
    ggplot2::scale_y_discrete(
      limits = rev(ref_substrats$Substrat), # Inverse l'ordre des substrats sur l'axe Y
      drop = FALSE ) +  # Affiche tous les substrats même s'ils n'ont pas de valeur

    # Pour les coulerus des tuiles et sa legende
    ggplot2::scale_fill_gradientn(
      colours = c(
        "white", # 0 % d'occurrence
        "#FFF59D", # Jaune clair pour les faibles occurrences
        "#FBC02D", # Jaune plus marqué
        "#FFCDD2", # Rouge clair
        "#EF5350", # Rouge moyen
        "#B71C1C" ),  # Rouge foncé pour les fortes occurrences
      values = scales::rescale(
        c(0, 1, 5, 6, 50, 100)  ), # Place les couleurs selon les valeurs d'occurrence
      limits = c(0, 100), # Limite l'échelle de 0 à 100 %
      breaks = c(
        0, 1, 5,
        25, 50, 75, 100 ), # Valeurs affichées dans la légende
      name = "Occurrence (%)" ) # Nom de la légende

  # Pour le graph final avec les figurés phase A B et C
  # Phase A
  p <- p +
    ggpattern::geom_tile_pattern(
      data = dplyr::filter(df, phase_A), # Garde uniquement les substrats présents en phase A
      ggplot2::aes(
        x = annee, # Années en abscisse
        y = Substrat) , # Substrats en ordonnée
      inherit.aes = FALSE, # Ne reprend pas automatiquement les aes du graphique principal
      fill = NA, # Ne remet pas de couleur de fond
      color = NA, # Pas de contour supplémentaire
      pattern = "stripe", # Ajoute des hachures
      pattern_angle = 45 , # Hachures inclinées dans un sens
      pattern_fill = "black", # Couleur du remplissage des hachures
      pattern_colour = "black", # Couleur des hachures
      pattern_density = 0.25, # Densité des hachures
      pattern_spacing = 0.05, # Espacement entre les hachures
      show.legend = FALSE) # N'affiche pas cette couche dans la légende

  # Phase B
  p <- p +
    ggpattern::geom_tile_pattern(
      data = dplyr::filter(df, phase_B), # Garde uniquement les substrats présents en phase B
      ggplot2::aes(
        x = annee, # Années en abscisse
        y = Substrat),# Substrats en ordonnée
      inherit.aes = FALSE, # Ne reprend pas les aes du graphique principal
      fill = NA, # Ne remet pas de couleur de fond
      color = NA, # Pas de contour supplémentaire
      pattern = "stripe", # Ajoute des hachures
      pattern_angle = - 45,# Hachures inclinées dans l'autre sens
      pattern_fill = "black", # Couleur du remplissage des hachures
      pattern_colour = "black", # Couleur des hachures
      pattern_density = 0.25, # Densité des hachures
      pattern_spacing = 0.05, # Espacement entre les hachures
      show.legend = FALSE) # N'affiche pas cette couche dans la légende

  # Phase C
  p <- p +
    ggpattern::geom_tile_pattern(
      data = dplyr::filter(df, phase_C), # Garde uniquement les substrats présents en phase C
      ggplot2::aes(
        x = annee, # Années en abscisse
        y = Substrat), # Substrats en ordonnée
      inherit.aes = FALSE, # Ne reprend pas les aes du graphique principal
      fill = NA, # Ne remet pas de couleur de fond
      color = NA, # Pas de contour supplémentaire
      pattern = "circle", # Ajoute des points
      pattern_fill = "white", # Couleur de remplissage des points
      pattern_colour = "black", # Couleur des points
      pattern_density = 0.25, # Densité des points
      pattern_spacing = 0.05, # Espacement entre les points
      show.legend = FALSE)  # N'affiche pas cette couche dans la légende

# Titre légende etc
  p +
    ggplot2::labs(
      title = "Plan d'échantillonnage macroinvertébrés", # Titre du graphique
      subtitle = paste(
        "Station",
        code_station_selectionne), # Sous-titre avec le code de la station
      x = "Année", # Nom de l'axe X
      y = "Substrat", # Nom de l'axe Y
      caption = "Jaune : occurrence marginale (1-5 %)    Rouge : occurrence dominante (> 5 %)" ) +# Note sous le graphique
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = "Occurrence (%)", # Titre de la légende
        barheight = grid::unit(8, "cm"), # Hauteur de la barre de légende
        barwidth = grid::unit(0.5, "cm") ) ) + # Largeur de la barre de légende
    ggplot2::theme_minimal() + # Applique un thème simple
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, # Incline les années
        hjust = 1 ), # Aligne les années inclinées
      plot.caption = ggplot2::element_text(
        hjust = 0, # Aligne la légende en bas à gauche
        size = 9) , # Taille du texte de la note
      legend.position = "right") } # Place la légende à droite


# 2) Préparer le tableau des substrats macroinvertébrés
fun_table_plan_inv_facies <- function(plan_inv_facies,
                                      code_station_selectionne = NULL) {

  ref_substrats <- fun_ref_substrats_inv() # Récupère le référentiel des substrats
  df <- plan_inv_facies |> # Part de la table des faciès
    dplyr::filter(Type == "Substrat") # Garde seulement les lignes correspondant aux substrats
  if (!is.null(code_station_selectionne)) { # Si une station est sélectionnée
    df <- df |>
      dplyr::filter(code_station == code_station_selectionne) } # Garde seulement cette station

  df |> # Reprend les données filtrées
    dplyr::mutate(
      Code = as.numeric(Code), # Transforme le code du substrat en nombre
      Substrat = as.character(Faciès), # Crée une colonne Substrat à partir de la colonne Faciès
      recouvrement_num = fun_parse_occurrence_inv(Occurence), # Convertit l'occurrence en valeur numérique
      Recouvrement = paste0(recouvrement_num, "%") ) |> # Recrée une colonne avec le symbole %
    dplyr::filter(
      !is.na(Code), # Enlève les lignes sans code substrat
      !is.na(Substrat) ) |> # Enlève les lignes sans nom de substrat
    dplyr::mutate(
      Substrat = factor(
        Substrat, # Colonne à ordonner
        levels = ref_substrats$Substrat) ) |># Garde l'ordre du référentiel
    dplyr::select(
      code_station,
      date_prelevement,
      annee,
      Code,
      Substrat,
      Recouvrement, # Recouvrement avec le symbole %
      recouvrement_num ) |># Recouvrement en valeur numérique
    dplyr::arrange(
      annee, # Trie par année
      Code ) } # Puis par code substrat


# 3) Préparer le tableau des phases macroinvertébrés
fun_table_plan_inv_phases <- function(plan_inv_phases,
                                      code_station_selectionne = NULL) {

  ref_substrats <- fun_ref_substrats_inv() # Récupère le référentiel des substrats
  df <- plan_inv_phases # Part de la table des phases
  if (!is.null(code_station_selectionne)) { # Si une station est sélectionnée
    df <- df |>
      dplyr::filter(code_station == code_station_selectionne)} # Garde seulement cette station

  df |> # Reprend les données filtrées
    dplyr::filter(!is.na(`Numéro bocal`)) |> # Garde seulement les lignes avec une phase
    dplyr::mutate(
      zone_facies = stringr::str_remove(
        `Zones de faciès`,
        "^\\s*\\d+\\s*-\\s*Couple substrat/vitesse\\s*:\\s*" ), # Nettoie le début du texte pour garder uniquement vitesse + substrat
      Vitesse = stringr::str_trim(
        stringr::str_extract(zone_facies, "^[^,]+") ), # Extrait la vitesse (texte avant la première virgule)
      Substrat = stringr::str_trim(
        stringr::str_remove(zone_facies, "^[^,]+,\\s*") ), # Extrait le substrat (texte après la première virgule)
      `Numéro bocal` = as.character(`Numéro bocal`) ) |> # Transforme le numéro de bocal en texte

    dplyr::select( # Garde seulement les colonnes utiles
      code_station,
      date_prelevement,
      annee,
      `Numéro bocal`,
      Vitesse,
      Substrat ) |>
    dplyr::left_join(
      ref_substrats,
      by = "Substrat" ) |> # Ajoute le code du substrat grâce au référentiel
    dplyr::filter(!is.na(Code)) |> # Enlève les substrats non reconnus dans le référentiel
    dplyr::mutate(
      Substrat = factor(
        Substrat,
        levels = ref_substrats$Substrat ) ) |> # Met les substrats dans l'ordre du référentiel
    dplyr::select( # Réorganise les colonnes du tableau final
      code_station,
      date_prelevement,
      annee,
      Code,
      `Numéro bocal`,
      Vitesse,
      Substrat) |>
    dplyr::arrange(
      annee, # Trie d'abord par année
      Code, # Puis par code substrat
      `Numéro bocal`) } # Puis par phase (bocal)
