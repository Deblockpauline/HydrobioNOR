#' Fonction de preparation des données
#' @noRd

# 1) Convertir les classes de vitesse diatomées
fun_convertir_vitesse_diat <- function(vitesse) {

  vitesse <- as.character(vitesse) # Transforme la vitesse en texte
  dplyr::case_when(
    stringr::str_detect(vitesse, "N1") ~ 5, # N1 correspond à < 5 cm/s
    stringr::str_detect(vitesse, "N3") ~ 25, # N3 correspond à 5-25 cm/s
    stringr::str_detect(vitesse, "N5") ~ 75, # N5 correspond à 25-75 cm/s
    stringr::str_detect(vitesse, "N6") ~ 100, # N6 correspond à >= 75 cm/s
    stringr::str_detect(vitesse, "N4") ~ 150, # N4 correspond à 75-150 cm/s
    stringr::str_detect(vitesse, "N2") ~ 175, # N2 correspond à >= 150 cm/s
    TRUE ~ NA_real_) } # Si aucune classe est reconnue

# 2) Définir les couleurs des substrats diatomées
fun_couleurs_substrats_diat <- function() {

  c(
    "S24 - Pierres, galets" = "snow4",
    "D5 - pierres" = "seashell3",
    "D22 - CAILPIERGALET" = "snow4",
    "S26 - Roches, dalles" = "grey22",
    "D10 - roches, dalles, blocs" = "navy",
    "S30 - Blocs" = "grey37",
    "O5 - Petits blocs" = "slategrey",
    "D13 - Cailloux" = "aquamarine3",
    "S9 - Granulats" = "wheat3",
    "S29 - Dalles, argiles" = "wheat",
    "S10 - Hélophytes" = "darkolivegreen2",
    "D7 - Hélophytes" = "darkolivegreen2",
    "S2 - Hydrophytes" = "palegreen4",
    "D2 - Hydrophytes" = "palegreen4",
    "D11 - Algues" = "springgreen",
    "D15 - Béton hors piles pont" = "cyan",
    "D17 - Briques" = "firebrick",
    "D18 - Tuiles" = "darkorange",
    "D19 - Piles de pont" = "sienna4",
    "D20 - Palplanches hors bois" = "coral2",
    "D21 - Autres mat.artificiels" = "darkorchid",
    "D14 - Métal" = "cyan4",
    "M7 - Artificiel" = "mediumorchid1",
    "0 - Inconnu" = "black"  ) }

# -----------------------------------------------------------------------------------
#' Fonction pour preparer et creer le graph + tableau
#' @noRd

# 1) Préparer le plan d'échantillonnage diatomées pour le graph et tabelau
fun_prep_plan_diat <- function(df, code_station_selectionne = NULL) {

  if (!is.null(code_station_selectionne)) { # Si une station est sélectionnée
    df <- df |>
      dplyr::filter(code_station == code_station_selectionne) } # Garde uniquement cette station

  df |>
    dplyr::mutate(
      annee = as.character(annee), # Transforme l'année en texte pour l'axe x
      vitesse_max = fun_convertir_vitesse_diat(Vitesse) ) |> # Convertit la classe de vitesse pour l'affichage
    dplyr::filter(
      !is.na(annee), # Garde seulement les lignes avec une année
      !is.na(vitesse_max) ) |> # Garde seulement les lignes avec une vitesse reconnue
    dplyr::select(
      eqb,
      code_station,
      date_prelevement,
      annee,
      Faciès,
      Vitesse,
      vitesse_max,
      Substrat ) } # Garde les colonnes utiles

# 2) Préparer le tableau affiché/exporté diatomées
fun_table_plan_diat <- function(df) {

  df |>
    dplyr::select(
      annee,
      Faciès,
      Vitesse,
      Substrat) } # Garde seulement les colonnes à afficher dans le tableau

# 3) Graphique plan d'échantillonnage diatomées
fun_plot_plan_diat <- function(df) {

  if (nrow(df) == 0) { return(NULL)} # Si le tableau est vide, n'affiche rien
  couleurs_substrats <- fun_couleurs_substrats_diat() # Récupère les couleurs logiques par substrat

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = annee, # Années en abscisse
      y = vitesse_max, # Hauteur des barres selon la classe de vitesse
      fill = Substrat, # Couleur des barres selon le substrat
      text = paste0( # Texte affiché au survol
        "Année : ", annee,
        "<br>Vitesse : ", Vitesse,
        "<br>Valeur affichée : ", vitesse_max,
        "<br>Substrat : ", Substrat,
        "<br>Faciès : ", Faciès ) ) ) +
    ggplot2::geom_col(
      width = 0.7,
      alpha = 0.9,
      colour = "grey30" ) + # Crée les barres avec un contour discret
    ggplot2::scale_fill_manual(
      values = couleurs_substrats, # Applique les couleurs définies selon les substrats
      na.value = "grey90" ) + # Couleur utilisée si le substrat est manquant
    ggplot2::scale_y_continuous(
      breaks = c(5, 25, 75, 100, 150, 175), # Valeurs affichées sur l'axe y
      labels = c(
        "N1\n<5",
        "N3\n5-25",
        "N5\n25-75",
        "N6\n>=75",
        "N4\n75-150",
        "N2\n>=150" ), # Libellés des classes de vitesse
      limits = c(0, 180), # Limites de l'axe y
      expand = ggplot2::expansion(mult = c(0, 0.03) ) ) +
    ggplot2::labs(
      title = "Plan d'échantillonnage diatomées",
      x = "Année",
      y = "Vitesse maximale (cm/s)",
      fill = "Substrat" ) + # Titres du graphique
    ggplot2::theme_minimal() + # Thème simple
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), # Incline les années
      plot.title = ggplot2::element_text(face = "bold"), # Met le titre en gras
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 8) ) # Réduit la taille du texte de la légende

  plotly::ggplotly(
    p,
    tooltip = "text" ) |>
    plotly::config(
      toImageButtonOptions = list(
        format = "png", # Format d'export
        filename = "plan_echantillonnage_diatomees", # Nom du fichier exporté
        height = 800,
        width = 1200,
        scale = 2 ) ) }
