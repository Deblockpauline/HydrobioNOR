#' Préparer les dates SEEE
#' @description Les dates ne sont pas au bons format pour l'affichage dans l'outil
#' @noRd

fun_date_seee <- function(x) { # Conversion de la date
  dplyr::case_when( # Conditions
    grepl("/", as.character(x)) ~ as.Date(x, format = "%d/%m/%Y"), # Quand c'est en jour/mois/année
    TRUE ~ as.Date(x))} # Juste l'année

#--------------------------------------------------------------------------------------------------------------------
#' Lancer diagnostic SEEE local
#' @description Cette fonction permet de lancer le calcul, elle est appelée dans le script prep_data_hydrobio car le calcul est
#' trop lourd pour le faire au moment du lancement de l'outil
#' @noRd

fun_lancer_diagnostic_seee_local <- function(df_entree, # Table d'entrée (peut etre entree_diat ou inv)
                                             type_diag, # Type de diagnostic (diat ou inv)
                                             dossier_algo = "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/algo_SEEE") { # Dossier SEEE

# Cas invertébrés
  if (type_diag == "Macroinvertébrés") {
    dossier_script <- file.path(
      dossier_algo,
      "ODInvertebres_v1.0.2_Documentation_scripts (1)") # Dossier script inv
    script_calc <- file.path(
      dossier_script,
      "ODInvertebres_v1.0.2_calc_consult.R") # Script calcul inv
    fichier_entree <- file.path(
      dossier_script,
      "ODInvertebres_entree_01.txt") # Fichier entrée inv, format necessaire fourni, ma table entree_inv va etre inscrite dedans

# Cas diatomées
  } else { # Meme chose
    dossier_script <- file.path(
      dossier_algo,
      "ODDiatomees_v1.0.0_Documentation_scripts" ) # Dossier script diat
    script_calc <- file.path(
      dossier_script,
      "ODDiatomees_v1.0.0_calc_consult.R" ) # Script calcul diat
    fichier_entree <- file.path(
      dossier_script,
      "ODDiatomees_entree_01.txt") } # Fichier entrée diat

# Verification
  if (!file.exists(script_calc)) { # Si script absent
    stop("Script SEEE introuvable : ", script_calc) } # Message erreur

# Ecriture de la table dans le fichier attendu
  readr::write_delim(
    df_entree, # Données à écrire qui peut etre entree_inv ou diat
    file = fichier_entree, # Chemin fichier entrée
    delim = "\t", # Séparateur tabulation
    na = "" ) # NA en vide

# Differents dossier géré
  ancien_dossier <- getwd() # Dossier R actuel sauvegardé, car on va changer apres
  on.exit( # Sécurité
    setwd(ancien_dossier), # Revient au dossier initial
    add = TRUE) # Garde autres on.exit
  setwd(dossier_script) # Se place dans dossier SEEE

  env_seee <- new.env() # Environnement isolé

# Lecture du script SEEE (pas éxécuté)
  script_lignes <- readLines(
    script_calc, # Script SEEE
    encoding = "UTF-8", # Encodage
    warn = FALSE ) # Pas d'avertissement

# Correction dans le script as.tbl car plus valable avec le nouveau package sous forme de dplyr:: ou juste as.
  script_lignes <- gsub(
    "dplyr::as\\.tbl\\(", # Ancienne fonction avec dplyr::
    "tibble::as_tibble(", # Nouvelle fonction
    script_lignes) # Texte du script
  script_lignes <- gsub(
    "as\\.tbl\\(", # Ancienne fonction sans dplyr::
    "tibble::as_tibble(", # Nouvelle fonction
    script_lignes ) # Texte du script

  script_patch <- tempfile(fileext = ".R") # Script temporaire corrigé
  writeLines( # Ecriture du script corrigé temporaire
    script_lignes, # Script corrigé
    con = script_patch, # Fichier temporaire
    useBytes = TRUE) # Garde encodage

# Lance le script corrigé SEEE
  source(
    script_patch,
    local = env_seee ) # Dans environnement isolé

# Récuperation des résultats

  # Cas diatomées
  if (type_diag == "Diatomées") {
    if (exists("data_sortie", envir = env_seee)) { # Si objet résultat existe

      return( # 1er possibilité
        tibble::as_tibble( # Tranfome en tibble, sécurité si plusieur format
          get("data_sortie", envir = env_seee) )  # Récupère résultat dans l'environnement si data_sortie existe
      ) }# Retourne résultat diat en tibble et arrete la fonction

    # 2eme possibilité (csv)
    fichier_resultat <- file.path( # Construction du chemin
      dossier_script,
      "ODDiatomees_v1.0.0_resultats.csv") # Fichier résultat diat

    if (file.exists(fichier_resultat)) { # Si fichier existe
      return(
        readr::read_delim( # Lis le fichier avce read_delim
          file = fichier_resultat, # Fichier sortie
          delim = ";", # Séparateur CSV
          skip = 1, # Saute la 1ere ligne
          show_col_types = FALSE, # Cache types colonnes
          locale = readr::locale(decimal_mark = ".") ) # Décimales avec point
      ) } # Retourne fichier lu

    stop("Aucun résultat diatomées trouvé.") } # Si aucun résultat, renvoie ca

# Si le diagnostic n’est pas diatomées, la fonction continue -> donc partie INV

  objets <- ls(env_seee) # Liste objets créés

  # Garde seulement tables
  tables <- objets[ # Gardre seulement certains objets stocké ensuite dans tables
    vapply( # Applique une fonction sur chaque objet
      objets, # Liste des objets a tester
      function(x) { # Fonction à appliquer pour tous
        is.data.frame(get(x, envir = env_seee) ) }, # Verifie si c'est une table et recupere l'objet et le nom dans l'environnement
      logical(1) ) ] # Résultat TRUE/FALSE si c'est bien une table

  # Cherche la table résultat et la garde
  tables_resultats <- tables[
    vapply(
      tables, # On parcours chaque table trouvée
      function(x) { # Fonction à appliquer pour chaque
        tab <- get(x, envir = env_seee) # Récupère table
        all( # Verification des colones presentes
          c( # Liste des colonnes attendues
            "CODE_STATION",
            "DATE",
            "CODE_OPERATION",
            "MATIERES_ORGANIQUES",
            "MATIERES_PHOSPHOREES",
            "MATIERES_AZOTEES",
            "NITRATES",
            "HAP",
            "PESTICIDES",
            "RIPISYLVE",
            "VOIES_COMMUNICATION",
            "URBANISATION_100M",
            "RISQUE_COLMATAGE",
            "INSTABILITE_HYDROLOGIQUE",
            "ANTHROPISATION_BV"
          ) %in% names(tab) ) }, # donne les noms des colonnes et permet de vérifier avec les attendues
      logical(1) ) ]  # Résultat TRUE/FALSE

# Si rien n'est trouvé
  if (length(tables_resultats) == 0) { # Si aucune table résultat
    stop(
      "Aucune table résultat invertébrés trouvée. Tables trouvées : ",
      paste(tables, collapse = ", ") ) } # Liste tables trouvées si jamais ca ne fonctionne pas (prévoyance des bug..)

# Retourne la table résultat si presence d'une
  tibble::as_tibble(
    get(tables_resultats[1], envir = env_seee) ) } # Récupère 1ère table résultat en tibble (qui est une sorte de data frame)


# --------------------------------------------------------------------------------------------------------------------------------
#' Préparer radar SEEE
#'
#' @noRd

fun_preparer_radar_seee <- function(df,
                                    type_diag,
                                    groupe) { # Groupe à afficher

  df <- df |> # Utilise les données en entrée
    dplyr::mutate( # Modifie certaines colonnes
      DATE = fun_date_seee(DATE), # Convertit la date SEEE
      annee = lubridate::year(DATE) )# Extrait l'année


# Cas des macroinvertébrés

   if (type_diag == "Macroinvertébrés") {

    # Pressions chimiques
    if (groupe == "chimie") {
      df_plot <- df |> # Utilise les données macroinvertébrés
        dplyr::select( # Sélectionne les pressions chimiques
          annee,
          MATIERES_ORGANIQUES,
          MATIERES_AZOTEES,
          NITRATES,
          MATIERES_PHOSPHOREES,
          HAP,
          PESTICIDES ) |>
        tidyr::pivot_longer( # Passe en format long
          cols = -annee, # Garde l'année fixe
          names_to = "pression", # Colonne des pressions
          values_to = "resultat"  ) |> # Colonne des résultats
        dplyr::mutate( # Renomme les pressions
          pression = dplyr::recode(
            pression,
            MATIERES_ORGANIQUES = "MO",
            MATIERES_AZOTEES = "Mat.N",
            NITRATES = "NO3",
            MATIERES_PHOSPHOREES = "Mat.P",
            HAP = "HAP",
            PESTICIDES = "Pest.") )

      # Dégradations de l'habitat
    } else {
      df_plot <- df |> # Utilise les données macroinvertébrés
        dplyr::select( # Sélectionne les pressions habitat
          annee,
          RIPISYLVE,
          VOIES_COMMUNICATION,
          URBANISATION_100M,
          RISQUE_COLMATAGE,
          INSTABILITE_HYDROLOGIQUE,
          ANTHROPISATION_BV) |>
        tidyr::pivot_longer( # Passe en format long
          cols = -annee, # Garde l'année fixe
          names_to = "pression", # Colonne des pressions
          values_to = "resultat" ) |> # Colonne des résultats
        dplyr::mutate( # Renomme les pressions
          pression = dplyr::recode(
            pression,
            RIPISYLVE = "Ripisylve",
            VOIES_COMMUNICATION = "Voies com.",
            URBANISATION_100M = "Urbanisation",
            RISQUE_COLMATAGE = "Colmatage",
            INSTABILITE_HYDROLOGIQUE = "Instab. hydro.",
            ANTHROPISATION_BV = "Anthrop. BV" ) ) }


 # Cas des diatomées, pas le meme tableau

      } else {

    # Pressions chimiques
    if (groupe == "chimie") {
      df_plot <- df |> # Utilise les données diatomées
        dplyr::filter( # Garde uniquement les paramètres chimiques
          grepl("^CHEM", LIB_PAR) ) |>
        dplyr::mutate( # Regroupe les paramètres en pressions lisibles
          pression = dplyr::case_when( # Differents car pas la meme forme
            grepl("Acidification", LIB_PAR) ~ "Acidif.",
            grepl("Fungicides|Herbicides|Insecticides", LIB_PAR) ~ "Pest.", # Regroupement
            grepl("HAP", LIB_PAR) ~ "HAP",
            grepl("Matieres.azotees", LIB_PAR) ~ "Mat.N",
            grepl("Matieres.organiques", LIB_PAR) ~ "MO",
            grepl("Matieres.Phosphorees", LIB_PAR) ~ "Mat.P",
            grepl("MES", LIB_PAR) ~ "MES",
            grepl("Micropolluants.mineraux", LIB_PAR) ~ "Micropoll. min.",
            grepl("Micropolluants.organiques|PCB", LIB_PAR) ~ "Micropoll. org.",
            grepl("Nitrates", LIB_PAR) ~ "NO3",
            TRUE ~ NA_character_) ) |>
        dplyr::filter( # Supprime les paramètres non reconnus
          !is.na(pression) ) |>
        dplyr::group_by( # Regroupe par année et pression
          annee,
          pression ) |>
        dplyr::summarise( # Calcule une valeur moyenne si plusieurs paramètres sont regroupés
          resultat = mean(as.numeric(RESULTAT), na.rm = TRUE),
          .groups = "drop" )

      # Dégradations de l'habitat
    } else {
      df_plot <- df |> # Utilise les données diatomées
        dplyr::filter( # Garde uniquement les paramètres hydromorphologiques
          grepl("^HYDMORP", LIB_PAR) ) |>
        dplyr::mutate( # Renomme les paramètres habitat
          pression = dplyr::case_when(
            grepl("HM.RIV", LIB_PAR) ~ "Ripisylve",
            grepl("HM.TRAN", LIB_PAR) ~ "Voies com.",
            grepl("HM.URB", LIB_PAR) ~ "Urbanisation",
            grepl("HM.CLOG", LIB_PAR) ~ "Colmatage",
            grepl("HM.HINST", LIB_PAR) ~ "Instab. hydro.",
            grepl("HM.CATCH", LIB_PAR) ~ "Anthrop. BV",
            TRUE ~ NA_character_) ) |>
        dplyr::filter( # Supprime les paramètres non reconnus
          !is.na(pression) ) |>
        dplyr::select( # Garde les colonnes utiles au graphique
          annee,
          pression,
          resultat = RESULTAT) } }

  # Mise en forme finale
  df_plot |>
    dplyr::mutate( # Harmonise les formats
      annee = as.character(annee), # Année en texte pour plotly
      resultat = as.numeric(resultat)  ) |> # Résultat en numérique
    dplyr::filter( # Supprime les résultats manquants
      !is.na(resultat) ) }


#------------------------------------------------------------------------------------------------------
#' Radar diagnostic SEEE
#' @noRd

fun_plot_diagnostic_seee <- function(df,
                                     titre,
                                     seuil = 0.6) { # Seuil de significativité

  ordre_pressions <- unique(df$pression) # Récupère l'ordre des pressions pour les axes du radar
  annees <- df |> # Utilise les données du graphique
    dplyr::distinct(annee) |> # Garde chaque année une seule fois
    dplyr::mutate(
      annee_num = as.numeric(annee)  ) |> # Convertit l'année en numérique pour le tri
    dplyr::arrange(annee_num) |> # Trie les années dans l'ordre croissant
    dplyr::pull(annee) # Récupère les années sous forme de vecteur

  couleurs <- grDevices::colorRampPalette( # Crée une palette de couleurs
    c(
      "blue", # Bleu
      "chartreuse", # Vert
      "darkorchid", # Violet
      "orange3", # Orange
      "#17becf", # Turquoise
      "grey39", # Gris foncé
      "deeppink", # Rose
      "yellow" ) # Jaune
  )(length(annees)) # Crée autant de couleurs que d'années
  names(couleurs) <- annees # Associe une couleur à chaque année

  p <- plotly::plot_ly() # Crée un graphique ploty vide


# Boucle

  for (annee_i in annees) { # Boucle sur chaque année sélectionnée
    couleur_i <- unname(couleurs[annee_i]) # Récupère la couleur correspondant à l'année

     # Prend les données d'une année
    df_i <- df |> # Utilise les données du radar
      dplyr::filter(annee == annee_i  ) |> # Garde uniquement l'année en cours
      dplyr::mutate( # Modifie la colonne pression
        pression = factor(
          pression, # Transforme les pressions en facteur
          levels = ordre_pressions )  ) |># Respecte l'ordre des axes
      dplyr::arrange(pression) # Trie les pressions dans le bon ordre
    df_i <- dplyr::bind_rows( # Ajoute la première ligne à la fin
      df_i,
      df_i[1, ]) # Permet de fermer la ligne du radar, sinon la courbe ne se referme pas

     # Ajoute la courbe sur le graph
    p <- p |> # Reprend le graphique existant
      plotly::add_trace( # Ajoute une courbe au radar
        data = df_i, # Données de l'année
        type = "scatterpolar", # Type de graphique radar
        mode = "lines+markers", # Affiche les lignes et les points
        r = ~resultat, # Valeurs sur l'axe radial
        theta = ~pression, # Axes du radar
        name = annee_i, # Nom affiché dans la légende
        line = list( # Paramètres de la ligne
          color = couleur_i, # Couleur de la ligne deja defini
          width = 2 ), # Épaisseur de la ligne
        marker = list( # Paramètres des points
          color = couleur_i, # Couleur des points
          size = 5 ), # Taille des points
        fill = "none", # Ne remplit pas le polygone
        opacity = 1, # Courbe  opaque
        text = ~annee, # Texte utilisé dans le survol
        hovertemplate = paste( # Texte
          "Année : %{text}<br>",
          "Pression : %{theta}<br>",
          "Résultat : %{r:.3f}<extra></extra>") ) }
# Fin de la boucle par année


# Pour le seuil
  df_seuil <- tibble::tibble( # Crée les données de la ligne du seuil
    pression = c(
      ordre_pressions, # Reprend toutes les pressions
      ordre_pressions[1] ), # Ajoute la première pression à la fin pour ferme le cercle
    resultat = seuil) # Attribue la valeur du seuil à tous les axes

  p |> # Reprend le graphique avec les courbes des années
    # Ajout du seuil
    plotly::add_trace(
      data = df_seuil, # Données du seuil
      type = "scatterpolar", # Même type de graphique radar
      mode = "lines", # Affiche seulement une ligne
      r = ~resultat, # Valeur du seuil sur l'axe radial
      theta = ~pression, # Axes du radar
      name = paste0("Seuil ", seuil), # Nom affiché dans la légende
      line = list( # Paramètres de la ligne du seuil
        color = "red", # Couleur rouge pour le seuil
        dash = "dash", # Ligne en pointillés
        width = 2), # Épaisseur de la ligne
      hovertemplate = paste( # Texte affiché au survol du seuil
        "Seuil : %{r:.2f}<br>",
        "Pression : %{theta}<extra></extra>" ) ) |>

    # Mise en forme final
    plotly::layout(
      title = titre, # Ajoute le titre du graphique
      polar = list( # Paramètres du radar
        radialaxis = list( # Paramètres de l'axe radial
          visible = TRUE, # Affiche l'axe radial
          range = c(0, 1) ) ), # Fixe l'échelle entre 0 et 1
      showlegend = TRUE ) # Affiche la légende
}
