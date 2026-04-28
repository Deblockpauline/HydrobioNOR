#' Préparation des données détaillées d'occupation du sol du bv
#'
#' @description On part du `code_station`, on récupère le bv correspondant via `id_BV`
#' qui est recherché dans la colonne `CdOH` de la table `occupation_BV_details_xxxx`
#' C'est le meme fonctionnement que fun_plot_bv_occupation sauf que ici, pas de boucle car on travaille sur 1 année
#'
#' @param donnees Liste contenant les objets de l'application
#' @param station_id Code de la station sélectionnée
#' @param annee Année choisie pour afficher le camembert
#' @return Un tableau détaillé prêt pour le graphique, ou NULL si aucune donnée
#' @noRd

fun_prep_bv_occupation_detail <- function(donnees, station_id, annee) {

  #Verification
  if (is.null(donnees) || is.null(station_id) || is.na(station_id) || station_id == "") {
    return(NULL)} # Vérifie que les données existent et qu'une station est bien sélectionnée
  stations <- donnees$stations # Récupère la table stations dans un objet plus simple

  # Harmonisation du code station sélectionné
  station_id <- stringr::str_pad(
    string = as.character(station_id), # Transforme en texte
    width = 8, # Force un format sur 8 caractères
    side = "left", # Rajoute les 0 à gauche
    pad = "0" )# Caractère utilisé pour compléter

  # Harmonisation des identifiants dans la table stations
  stations <- stations %>%
    dplyr::mutate(
      code_station = stringr::str_pad(
        string = as.character(.data$code_station), # Transforme aussi les codes station en texte
        width = 8, # Même format sur 8 caractères
        side = "left", # Rajoute les 0 à gauche si besoin
        pad = "0"),
      id_BV = as.character(.data$id_BV)) # Transforme id_BV en caractère pour la comparaison

  station_info <- stations %>%
    dplyr::filter(.data$code_station == station_id) # Garde uniquement la ligne de la station choisie

  # Récupération de l'identifiant du bassin versant
  id_bv <- station_info$id_BV[1] # On prend le 1er identifiant de BV trouvé
  nom_table <- paste0("occupation_BV_details_", annee) # Construit le nom de la table selon l'année choisie
  table_occ <- donnees[[nom_table]] # Récupère la table d'occupation détaillée de l'année choisie

  # Harmonisation de CdOH
  table_occ <- table_occ %>%
    dplyr::mutate(
      CdOH = as.character(.data$CdOH)) # Transforme CdOH en caractère pour la comparaison avec id_bv

  bv_occ <- table_occ %>%
    dplyr::filter(.data$CdOH == id_bv) # Garde uniquement la ligne du BV correspondant à la station
  bv_occ <- bv_occ[1, , drop = FALSE] # Sécurité : on garde la 1ere ligne et on reste en tableau

  # Transformation du format large au format long = colonne d'occupation -> ligne
  table_detail <- bv_occ %>%
    tidyr::pivot_longer(
      cols = -CdOH, # On transforme toutes les colonnes sauf CdOH
      names_to = "occupation_detail", # Nom de la colonne qui contiendra les catégories
      values_to = "pourcentage" ) %>% # Nom de la colonne qui contiendra les valeurs
    dplyr::mutate(
      occupation_detail = as.character(.data$occupation_detail), # Transforme les noms de catégories en texte
      pourcentage = as.numeric(.data$pourcentage)  ) %>% # Transforme les valeurs en numérique
    dplyr::arrange(
      dplyr::desc(.data$pourcentage)) %>% # Trie du plus grand au plus petit
    dplyr::mutate(
      texte_survol = paste0( # Crée le texte qui s'affichera au survol
        "Catégorie : ", .data$occupation_detail,
        "<br>Pourcentage : ", round(.data$pourcentage, 1), " %"))

  return(table_detail) # Retourne le tableau final prêt pour le camembert
}


#' Camembert des occupations détaillées du bassin versant
#'
#' @param table_detail Tableau issu de fun_prep_bv_occupation_detail()
#'
#' @return Un graphique plotly, ou NULL si aucune donnée
#' @noRd

fun_plot_bv_occupation_detail <- function(table_detail) {

  # On enlève les catégories à 0 pour qu'elles ne s'affichent pas dans la légende
  table_detail <- table_detail %>%
    dplyr::filter(.data$pourcentage > 0)
  if (nrow(table_detail) == 0) { return(NULL)}

  plotly::plot_ly(
    data = table_detail, # Tableau utilisé pour créer le graphique
    labels = ~occupation_detail, # Libellé des parts du graph
    values = ~pourcentage, # Taille selon le pourcentage
    type = "pie", # Type de graphique = camembert
    text = ~paste0(
      "Catégorie : ", occupation_detail,
      "<br>Pourcentage : ", round(pourcentage, 1), " %" ), # Texte affiché au survol
    hoverinfo = "text", # Au survol, affiche seulement le texte défini au-dessus
    textinfo = "label+percent") %>% # Affiche sur le graphique le nom + le pourcentage
    plotly::layout(
      title = list(
        text = "Répartition détaillée de l'occupation du sol du BV", # Titre du graphique
        x = 0.5), # Centre le titre
      legend = list( orientation = "v" ) ) # Légende verticale
}
