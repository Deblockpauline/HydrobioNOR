#' Fonction pour preparer les données
#'
#' @noRd

# 1) Convertir une valeur texte en nombre (ex: 3,5% en 3.5 pour les graph)
fun_parse_num_macro <- function(x) {
  x |> # Prend une colonne defini x en entrée
    as.character() |> # Transforme les valeurs en texte
    stringr::str_replace("%", "") |> # Enlève le symbole %
    stringr::str_replace(",", ".") |> # Remplace les virgules par des points
    as.numeric() }# Transforme le texte en valeur numérique

# 2) Extraire la classe de présence (ex : "5: x >= 75 %" devient 5)
fun_extraire_classe_presence <- function(x) {
  x |>  # Prend une colonne defini x en entrée
    as.character() |> # Transforme la valeur en texte
    stringr::str_extract("^\\d+") |> # Garde uniquement le chiffre au début du texte
    as.numeric()} # Transforme ce chiffre en numérique

# 3) Nettoyer le texte de la colonne Type pour eviter les erreurs
fun_nettoyer_type_macro <- function(x) {
  x |> # Prend une colonne defini x en entrée
    as.character() |> # Transforme les valeurs en texte
    stringr::str_squish() } # Supprime les espaces inutiles au début, à la fin et entre les mots


# -----------------------------------------------------------------------------------
#' Préparer les données macrophytes pour une station
#'
#' @noRd

fun_prep_plan_macro_station <- function(plan_macrophytes,
                                        code_station_selectionne) {
  plan_macrophytes |> # Prend la table des plans macrophytes
    dplyr::filter(code_station == code_station_selectionne) |> # Filtre selon la station sélectionnée
    dplyr::mutate( # Crée ou modifie plusieurs colonnes
      Type_clean = fun_nettoyer_type_macro(Type), # Appel la fonction pour nettoyer la colonne Type
      annee = as.factor(annee), # Transforme l'année en facteur pour les graphiques
      prels_elem = stringr::str_squish(as.character(`Préls. élem.`)), # Nettoie le numéro de prélèvement
      occurrence_num = fun_parse_num_macro(Occurence), # Appel la fonction 1 pour convertir l'occurrence en numérique
      largeur_num = fun_parse_num_macro(Largeur),
      longueur_num = fun_parse_num_macro(Longueur), # Fais pareil pour les autres
      surface_veg_num = fun_parse_num_macro(`Surface végétalisée (%)`),
      classe_presence = fun_extraire_classe_presence(`Précisions sur la présence`)) } # Extrait la classe de présence


# -----------------------------------------------------------------------------------
# Partie 1 : Graph et Tableau correspodnant aux UR
#' @description
#' Fonctions qui va preparer le tableau, le graph et creer le graph
#' #' @noRd


# 1) Tableau unité de relevé macrophytes
# Extrait uniquement les informations concernant les unités de relevé
# et supprime les autres types de données (substrat, vitesse, etc.).
fun_table_macro_unite <- function(df_macro) {
  df_macro |> # Prend les données macrophytes préparées deja appellé dans le mod
    dplyr::filter( # Filtre les lignes
      stringr::str_detect( # Cherche un texte  dans Type_clean
        Type_clean,
        stringr::regex(
          "Unité de relevé macrophytes en cours d'eau", # Texte recherché
          ignore_case = TRUE ) ) ) |># Ignore les majuscules/minuscules
    dplyr::select( # Sélectionne les colonnes à afficher dans le tableau
      code_station,
      date_prelevement,
      annee,
      `Préls. élem.`,
      Type,
      Faciès,
      Occurence,
      Largeur,
      Longueur,
      `Présence de périphyton`,
      `Surface végétalisée (%)` ) |>
    dplyr::distinct() |> # Supprime les doublons
    dplyr::arrange(annee) } # Trie les données par année


# 2) Préparer les données du graphique général multicourbe macrophytes
fun_prep_macro_courbes <- function(df_macro) {
  df_macro |> # Prend les données macrophytes préparées
    dplyr::filter( # Garde seulement les unités de relevé macrophytes
      stringr::str_detect(
        Type_clean,
        stringr::regex(
          "Unité de relevé macrophytes en cours d'eau",
          ignore_case = TRUE) ) ) |>
     dplyr::select( # Sélectionne uniquement les colonnes utiles au graphique
      code_station,
      date_prelevement,
      annee,
      prels_elem,
      largeur_num,
      longueur_num,
      occurrence_num,
      surface_veg_num,
      `Présence de périphyton` ) |>

    dplyr::distinct() |> # Supprime les doublons
    tidyr::pivot_longer( # Passe les données d'un format large à un format long
      cols = c(
        longueur_num,
        largeur_num,
        occurrence_num,
        surface_veg_num),
      names_to = "variable", # Nom de la nouvelle colonne contenant le nom des variables
      values_to = "valeur") |> # Nom de la nouvelle colonne contenant les valeurs
    dplyr::mutate( # Renomme les variables pour que la légende soit plus lisible
      variable = dplyr::case_when(
        variable == "longueur_num" ~ "Longueur",
        variable == "largeur_num" ~ "Largeur",
        variable == "occurrence_num" ~ "Occurrence",
        variable == "surface_veg_num" ~ "Surface végétalisée (%)",
        TRUE ~ variable)) |>
    dplyr::filter(!is.na(valeur) ) } # Supprime les lignes sans valeur


# 3) Graphique général multicourbe des UR macrophytes
fun_plot_macro_courbes <- function(df_macro, annees_station = NULL) {

  donnees_courbes <- fun_prep_macro_courbes(df_macro) # Appel la fonction pour préparer les données
  donnees_courbes <- donnees_courbes |>
    dplyr::mutate(
      annee = as.numeric(as.character(annee)) )
  if (nrow(donnees_courbes) == 0) { return(NULL)} # Si aucune donnée, ne renvoie rien
  if (is.null(annees_station)) {
    annees_station <- sort(unique(donnees_courbes$annee))
  } else { annees_station <- as.numeric(as.character(annees_station))}

  # Differenciation entre longeur, largeur, occurence et avec surface végétalisée car on ajoute des données de periphyton
  donnees_lignes <- donnees_courbes |> # Crée les données en excluant la surface végétalisée
    dplyr::filter(variable != "Surface végétalisée (%)")
  donnees_surface <- donnees_courbes |> # Crée les données pour la surface végétalisée
    dplyr::filter(variable == "Surface végétalisée (%)")

  # Debut du graphqiue
  ggplot2::ggplot() + # Initialise le graphique

    # Pour les courbes classiques (longeur, largeur etc
    ggplot2::geom_line( # Ajoute les courbes des variables classiques
      data = donnees_lignes,
      ggplot2::aes(
        x = annee, # Année en abscisse
        y = valeur, # Valeur en ordonnée
        colour = variable, # Couleur diff selon la variable
        group = variable, # Une courbe par variable
        text = paste0( # Texte affiché au survol
          "Année : ", annee,
          "<br>Variable : ", variable,
          "<br>Valeur : ", valeur )),
      linewidth = 0.9  ) +# Épaisseur des lignes
    ggplot2::geom_point( # Ajoute les points sur les courbes classiques
      data = donnees_lignes,
      ggplot2::aes(
        x = annee,
        y = valeur,
        colour = variable,
        text = paste0(
          "Année : ", annee,
          "<br>Variable : ", variable,
          "<br>Valeur : ", valeur) ),
      size = 2.8) + # Taille des points

    # Pour la courbe de surface végétalisée
    ggplot2::geom_line( # Ajoute la courbe
      data = donnees_surface, # Données de surface_végétalisée
      ggplot2::aes(
        x = annee,
        y = valeur,
        colour = variable, # Une couleur differente selon la variable
        group = variable,
        text = paste0(
          "Année : ", annee,
          "<br>Variable : Surface végétalisée (%)",
          "<br>Valeur : ", valeur,
          "<br>Périphyton : ", `Présence de périphyton` ) ), # Dans le survol on affiche la valeur de periphyton
      linewidth = 0.9 ) +
    ggplot2::geom_point( # Ajoute les points de surface végétalisée
      data = donnees_surface,
      ggplot2::aes(
        x = annee,
        y = valeur,
        colour = variable, # Même couleur que la surface végétalisée
        shape = `Présence de périphyton`, # Forme selon la présence de périphyton
        text = paste0(
          "Année : ", annee,
          "<br>Surface végétalisée (%) : ", valeur,
          "<br>Périphyton : ", `Présence de périphyton`) ),
      size = 4 ) + # Points plus gros pour bien voir le périphyton

    # Ajout des point selon le periphyton
    ggplot2::scale_shape_manual( # Définit les formes utilisées pour le périphyton
      values = c(
        "0 - Inconnu" = 16, # Rond
        "1 - Absent" = 8, # Rond
        "2 - Peu abondant" = 15, # Carré
        "3 - Abondant" = 17, # Triangle
        "4 - Très abondant" = 18 ) ) + # Losange

    # Parametre général du graph
    ggplot2::labs( # Définit les titres des axes et légendes
      x = "Année",
      y = "Valeur",
      colour = "Variable", # Titre de la legende des couleurs
      shape = "Périphyton" ) + # Titre de la legende des forme
    ggplot2::theme_minimal() + # Applique un thème simple
    ggplot2::scale_x_continuous(
      breaks = annees_station,
      limits = range(annees_station, na.rm = TRUE) ) +
    ggplot2::theme( # Personnalise l'apparence du graphique
      legend.position = "bottom", # Place la légende en bas
      legend.box = "vertical", # Met les légendes les unes sous les autres
      legend.title = ggplot2::element_text(face = "bold"), # Met les titres de légende en gras
      legend.text = ggplot2::element_text(size = 10), # Taille du texte de légende
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1) ) } # Incline les années


# -----------------------------------------------------------------------------------
#' Partie 2: Fonctions pour preparer les donnees, tableau grapher et la creation du graph
#' #' @noRd

# 1) Préparer les données des tuiles macrophytes
fun_prep_macro_tuiles <- function(df_macro) {
  df_macro |> # Prend les données macrophytes préparées
    dplyr::filter( # Garde seulement les types utilisés pour les tuiles
      Type_clean %in% c(
        "Eclairement",
        "Substrat",
        "Profondeur",
        "Vitesse",
        "Faciès morphodynamique") ) |>

     dplyr::mutate( # Crée les colonnes nécessaires au graphique
      modalite = dplyr::case_when( # Récupère la bonne colonne selon le type
        Type_clean == "Eclairement" ~ Eclairement,
        Type_clean == "Substrat" ~ Substrat,
        Type_clean == "Profondeur" ~ Profondeur,
        Type_clean == "Vitesse" ~ Vitesse,
        Type_clean == "Faciès morphodynamique" ~ Faciès,
        TRUE ~ NA_character_ ),
      Type_clean = factor( # Définit l'ordre d'affichage des graphiques
        Type_clean,
        levels = c(
          "Eclairement",
          "Faciès morphodynamique",
          "Profondeur",
          "Substrat",
          "Vitesse") ) ) |>
    dplyr::filter(!is.na(modalite)) |> # Supprime les lignes sans modalité
    dplyr::group_by( # Regroupe les données pour éviter les doublons
      code_station,
      date_prelevement,
      annee,
      prels_elem,
      Type_clean,
      modalite ) |>

    dplyr::summarise( # Résume les données par année, type et modalité
      classe_presence = max(classe_presence, na.rm = TRUE), # Garde la classe de présence maximale att qi que des NA renvoie - Infini )
      precision_presence = paste( # Regroupe les précisions de présence
        unique(`Précisions sur la présence`),
        collapse = " ; "),
      .groups = "drop" ) |> # Enlève le regroupement après le résumé
    dplyr::mutate( # Corrige les valeurs infinies en NA  pour debug le -inf
      classe_presence = dplyr::if_else(
        is.infinite(classe_presence),
        NA_real_,
        classe_presence) ) }


# 2) Graphiques en tuiles macrophytes
fun_plot_macro_tuiles <- function(df_macro) {
  donnees_tuiles <- fun_prep_macro_tuiles(df_macro) # Appel la fonction pour prépare les données
  if (nrow(donnees_tuiles) == 0) { return(NULL) }  # Si aucune donnée, ne renvoie rien

  #Récuperation et préparation des données
  donnees_tuiles <- donnees_tuiles |> # Reprend les données des tuiles
    dplyr::mutate(
      modalite_plot = modalite ) # Crée une colonne utilisée pour l'axe Y (rappel: modalité est crée dans la fonction avant)
  annees_facettes <- donnees_tuiles |> # Crée une table vide pour afficher les années sous chaque graphique
    dplyr::distinct(Type_clean, annee) |> # Garde une seule ligne par type et par année
    dplyr::mutate( # Tout est vide car on veut que les années
      modalite_plot = " ",
      classe_presence = NA_real_, #
      modalite = " ", #
      precision_presence = NA_character_)

  # Création du graph
  ggplot2::ggplot() + # Initialise le graphique

    # Ajoute les tuiles
    ggplot2::geom_tile(
      data = donnees_tuiles,
      ggplot2::aes(
        x = annee, # Année en abscisse
        y = modalite_plot, # Modalité en ordonnée
        fill = classe_presence, # Couleur selon la classe de présence
        text = paste0( # Texte affiché au survol
          "Année : ", annee,
          "<br>Type : ", Type_clean,
          "<br>Modalité : ", modalite,
          "<br>Classe : ", classe_presence,
          "<br>Précision : ", precision_presence ) ),
      colour = "white", # Bordure blanche entre les tuiles
      linewidth = 0.6, # Épaisseur des bordures
      height = 0.9 ) +# Hauteur des tuiles

    # Ajoute la classe au centre de chaque tuiles
    ggplot2::geom_text(
      data = donnees_tuiles,
      ggplot2::aes(
        x = annee,
        y = modalite_plot,
        label = classe_presence ),
      size = 3.4, # Taille du texte
      na.rm = TRUE ) +# Ignore les valeurs manquantes

    # Aficche les années sous chaque graph
    ggplot2::geom_text(
      data = annees_facettes,
      ggplot2::aes(
        x = annee,
        y = modalite_plot,
        label = annee),
      angle = 45, # Incline les années
      hjust = 1, # Aligne le texte incliné
      size = 3.2 ) + # Taille du texte des années

    # Crée un graph séparé par type
    ggplot2::facet_wrap(
      ~ Type_clean,
      scales = "free_y", # Chaque graphique a son propre axe Y
      ncol = 1, # Affiche les graphiques les uns sous les autres
      strip.position = "top") + # Place le titre des facettes en haut

    # Defini le degradé de coueleurs pour les tuiles
    ggplot2::scale_fill_gradient(
      low = "white", # Classe faible en blanc
      high = "darkgreen", # Classe forte en vert foncé
      limits = c(0, 5), # Classes de 0 à 5
      na.value = "white") + # Valeurs manquantes en blanc

    # Parametres generaux des graphs
    ggplot2::labs( # Définit les titres du graphique
      x = NULL, # Pas de titre pour l'axe X
      y = NULL, # Pas de titre pour l'axe Y
      caption = "0 = absent ; 1 à 5 = présence croissante." ) + # Légende explicative
    ggplot2::theme_minimal() + # Applique un thème simple
    ggplot2::theme( # Personnalise l'apparence du graphique
      legend.position = "none", # Masque la légende de couleur
      strip.placement = "outside", # Place les titres de facettes à l'extérieur
      strip.text = ggplot2::element_text( # Style des titres des facettes
        face = "bold",
        size = 15,
        margin = ggplot2::margin(t = 15, b = 10) ),
      axis.text.x = ggplot2::element_blank(), # Cache les années de l'axe X classique
      axis.ticks.x = ggplot2::element_blank(), # Cache les graduations de l'axe X
      axis.text.y = ggplot2::element_text(size = 10), # Taille du texte de l'axe Y
      panel.grid = ggplot2::element_blank(), # Supprime le quadrillage
      panel.spacing.y = grid::unit(4, "lines"), # Ajoute de l'espace entre les graphiques
      plot.caption = ggplot2::element_text( # Style de la légende en bas
        size = 9,
        face = "italic",
        hjust = 0) ) }


# 3) Tableau global des tuiles macrophytes
fun_table_macro_global <- function(df_macro) {
  df_macro |> # Prend les données macrophytes préparées
    dplyr::filter( # Garde seulement les types utilisés dans les tuiles
      Type_clean %in% c(
        "Eclairement",
        "Substrat",
        "Profondeur",
        "Vitesse",
        "Faciès morphodynamique")) |>
    dplyr::select( # Sélectionne les colonnes utiles pour le tableau
      code_station,
      date_prelevement,
      annee,
      `Préls. élem.`,
      Type,
      Faciès,
      Vitesse,
      Profondeur,
      Eclairement,
      Substrat,
      `Précisions sur la présence` ) |>
    dplyr::distinct() |> # Supprime les doublons
    dplyr::arrange(annee, Type) } # Trie par année puis par type
