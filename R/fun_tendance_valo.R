#' Calcul des tendances temporelles des indices biologiques
#'
#' @description Cette fonction calcule, pour chaque compartiment biologique présent
#' - les valeurs réellement observées dans la station et sa tendance temporelle
#' - la tendance temporelle générale de chaque réseau + intervalle de confiance à 95%
#' @return Une liste contenant les données calc,le tableau, les modèles ajustés et les réseaux de la station
#' @export

fun_calcul_tendances_indices <- function( etat_bio,
                                          stations,
                                          code_station_selectionnee) {

# Si pas de données
  resultat_vide <- function( reseaux = character()) { # Par défaut, aucun réseau n'est renseigné
    return( # Renvoie immédiatement une liste vide
      list(
        graphiques = list(), # Liste vide pour les données des graph
        tableau = tibble::tibble(), # Tableau vide
        modeles = list(), # Liste vide pour les modèles statistiques
        reseaux = reseaux ) ) } # Réseaux éventuellement déjà identifiés
  code_station_selectionnee <- as.character( code_station_selectionnee) # Code en caractere
  if ( # Verification de la presence des données
    is.null(etat_bio) || # Vérifie si presence de la table
    is.null(stations) ||
    length(code_station_selectionnee) == 0 || # Vérifie si code present
    is.na(code_station_selectionnee) ) # Vérifie si c'est un Na
    { return( resultat_vide() ) }  # Arret avec un résultat vide


# Correspondance entre les indices et les compartiments biologiques
  correspondance_indices <- tibble::tribble( # Table permettant d'associer chaque indice à son :
    ~code_indice, # Code SANDRE
    ~compartiment, # Compartiment biologique
    ~ordre, # Ordre d'affichage dans l'application
    ~variable, # Colonne utilisée comme valeur à modéliser
    # Possibilité selon ce qui est defini juste en haut
    "7613", "Macroinvertébrés", 1, "eqr_indice", # I2M2 : utilisation de l'EQR
    "5856", "Diatomées", 2, "eqr_indice",
    "2928", "Macrophytes", 3, "eqr_indice",
    "7036", "Poissons", 4, "resultat_indice" ) # IPR : utilisation du résultat brut


# Préparation de la table des stations avec selection et nettoyage
  stations_preparees <- stations |>
    dplyr::transmute( # Conserve  les colonnes
      code_station = as.character( code_station  ), # Code en caractère
      libelle_station = as.character(libelle_station ),
      reseau = as.character(reseau ) ) |>
    dplyr::mutate( # Modification de la colonne réseau
      reseau = dplyr::na_if(
        stringr::str_squish( reseau ), # Supprime les espaces inutiles dans le nom du réseau
        "") ) |> # Transforme les chaînes vides en NA
    dplyr::distinct() # Supprime les lignes strictement identiques


# Séparation des stations appartenant à plusieurs réseaux
  stations_reseaux <- stations_preparees |>
    dplyr::mutate( # Modification de la colonne réseau
      reseau = stringr::str_split(
        reseau, # Texte contenant le ou les réseaux
        "\\s*[/;-]\\s*") ) |># Sépare le texte au niveau de /, ; ou -
    tidyr::unnest_longer(reseau ) |> # Transforme chaque réseau de la liste en une ligne différente
    dplyr::mutate( # Nettoyage
      reseau = stringr::str_squish(
        as.character( reseau) ) ) |> # Conversion en caractere
    dplyr::filter(
      !is.na(reseau), # Retire les réseaux manquants
      reseau != "" ) |> # Retire les réseaux vides
    dplyr::distinct( # Supprime les doublons
      code_station, # Code de la station
      libelle_station, # Nom de la station
      reseau) # Réseau auquel appartient la station


# Recherche des réseaux de la station sélectionnée
  reseaux_station <- stations_reseaux |>
    dplyr::filter(
      code_station == code_station_selectionnee ) |> # Conserve uniquement la station sélectionnée
    dplyr::pull(reseau ) |> # Extrait uniquement la colonne contenant les réseaux
    unique() # Ne conserve qu'une seule fois chaque réseau
  if (length(reseaux_station) == 0) { return( resultat_vide() ) } # Arret si rien


# Préparation de la table contenant les indices
  table_indices_source <- etat_bio |> # Supprime pour eviter des doublons apres jointure
    dplyr::select(
      -dplyr::any_of( # Supprime uniquement les colonnes si elles existent
        c(  "libelle_station", # Nom de la station
          "reseau") ) )  # Réseau de la station


  # Création de la table complète utilisée pour les calculs
  table_indices <- table_indices_source |>
    dplyr::mutate(
      code_station = as.character(code_station), # En caractère
      code_indice = as.character(code_indice),
      annee = suppressWarnings( # Pas d'avertissement
        as.numeric(annee) ), # Conversion de l'année en nombre, pareil pour apres
      eqr_indice = suppressWarnings(
        as.numeric( eqr_indice) ),
      resultat_indice = suppressWarnings(
        as.numeric(resultat_indice) )  ) |>
    dplyr::inner_join( # Jointure
      correspondance_indices, # De la table de correspondance crée avec le compartiment et la variable à utiliser
      by = "code_indice"  ) |> # Jointure à partir du code de l'indice
    dplyr::left_join( # Jointure
      stations_reseaux, # De la table avec le nom de station et du réseau
      by = "code_station" ) |> # Jointure à partir du code de station
    dplyr::filter(
      !is.na(annee), # Supprime les lignes sans année
      is.finite(annee), # Supprime les années infinies ou non valides
      !is.na(reseau), # Supprime les lignes sans réseau
      reseau %in% reseaux_station ) # Conserve les données appartennat au réseaux de la station sel


# Recherche des indices présents dans la station sélectionnée
  indices_station <- table_indices |>
    dplyr::filter( code_station == code_station_selectionnee) |> # Conserve uniquement les données de la station sélectionnée
    dplyr::distinct(
      code_indice, # Code de l'indice présent
      compartiment, # Compartiment biologique correspondant
      ordre, # Ordre d'affichage du compartiment
      variable ) |># Variable utilisée dans le modèle
    dplyr::arrange(ordre ) # Trie les indices dans l'ordre souhaité pour l'affichage

  if (nrow(indices_station) == 0) { # Arret si rien
    return(
      resultat_vide(
        reseaux = reseaux_station) ) } # Réseaux conservé meme si pas d'indices


# Création des listes qui recevront les résultats
  liste_graphiques <- list()# Contiendra les observations et les tendances utilisées par les graphiques
  liste_tableaux <- list()# Contiendra les différentes parties du tableau exportable
  liste_modeles <- list() # Contiendra tous les modèles statistiques calculés
  constante_ipr <- 0.01 # Constante  ajouté à l'IPR avant le log, evite de calculer log(0) = -INF

# --------- Boucle sur chauqe indice de la station------------
  for (numero_indice in seq_len(nrow(indices_station))) {  # Genere des numeros allant de 1 au nombre d'indice

    # Récupération des informations sur l'indice actuellement traité
    code_indice_courant <-
      indices_station$code_indice[ numero_indice ]
    compartiment_courant <-
      indices_station$compartiment[ numero_indice ]
    variable_courante <-
      indices_station$variable[ numero_indice]
    ordre_courant <-
      indices_station$ordre[numero_indice]

    # Sélection des données correspondant à l'indice actuel
    table_indice <- table_indices |>
      dplyr::filter(
        code_indice == code_indice_courant ) |> # Conserve uniquement l'indice actuellement traité
      dplyr::mutate(
        valeur = suppressWarnings( # Colonne nommée valeur
          as.numeric(
            .data[[variable_courante ]] ) ) ) |> # Select la colonne indiquée par variable_courante
      dplyr::filter(
        !is.na(valeur), # Supprime les valeurs manquantes
        is.finite(valeur)) # Supprime les valeurs infinies ou non valides


    # Récupération des observations de la station sélectionnée
    observations_station <- table_indice |>
      dplyr::filter(
        code_station == code_station_selectionnee) |> # Conserve uniquement la station sélectionnée
      dplyr::distinct(
        code_station, # Code de la station
        annee, # Année de l'observation
        valeur, # Valeur observée
        .keep_all = TRUE ) |> # Conserve toutes les autres colonnes de la première ligne trouvée
      dplyr::arrange(annee ) # Trie les observations dans l'ordre chronologique

    if ( nrow(observations_station) < 2 || # Verif qu'il y a au moins 2 lignes car sinon pas possible de calculer une tendance
     dplyr::n_distinct( observations_station$annee ) < 2 ) {# Au moins 2 années diff
      next } # Passe à l'indice suivant sans exécuter la suite

    # Centrage des années, plus simple pour la suite
    annee_reference <- mean( # Calcule l'année au milieu de la période dispo (ex: entre 2010 et 2020 =2015 et donc 2015 devient 0 et 2014 par ex -1)
      range(
        table_indice$annee, # Ensemble des années disponibles pour cet indice
        na.rm = TRUE) )  # Ignore les éventuelles valeurs manquantes

    # Transformation de la variable utilisée dans le modèle
    if (code_indice_courant == "7036") { # Pour IPR
      table_indice <- table_indice |> # Tranformation en log
        dplyr::mutate(
          valeur_modele = log(
            pmax( valeur, 0 )+ # 0 remplace les valeurs négatives
            constante_ipr ),  # Ajoute 0,01 pour éviter le logarithme de zéro
          annee_c = annee - annee_reference)  # Centre l'année autour de l'année de référence
    } else {
      table_indice <- table_indice |> # Pour les autres = EQR
        dplyr::mutate(
          valeur_modele = valeur, # La valeur utilisée par le modèle reste inchangée
          annee_c = annee - annee_reference) } # Centre l'année autour de l'année de référence

    # Récupération des données transformées de la station
    observations_station_modele <- table_indice |>
      dplyr::filter( code_station == code_station_selectionnee) |>
      dplyr::distinct(
        code_station,
        annee,
        valeur,
        valeur_modele,
        .keep_all = TRUE ) |>
      dplyr::arrange( annee)

    # Création des années sur lesquelles les tendances seront prédites
    annees_prediction_station <- tibble::tibble(
      annee = seq(
        min( # 1er année dispo
          observations_station_modele$annee,
          na.rm = TRUE ),
        max( # Derniere année
          observations_station_modele$annee,
          na.rm = TRUE ),
        by = 1 ) ) |># Génère une valeur pour chaque année comprise dans la période
      dplyr::mutate(
        annee_c = annee - annee_reference)# Centre également les années utilisées pour les prédictions

   # Création des listes utilisées dans la boucle sur les réseaux
    liste_tendances_reseaux <- list() # Tendance pour chaque réseaux
    liste_tendances_station <- list() # Tendace propre a la station

    # -------------------------------------------------------------------------
    # -------------------Boucle sur chaque réseau -----------------------------
    # -------------------------------------------------------------------------

    for (reseau_courant in reseaux_station) {

      # Préparation des données du réseau courant
      table_reseau <- table_indice |>
        dplyr::filter(
          reseau == reseau_courant) |> # Conserve uniquement les stations du réseau actuel
        dplyr::distinct(
          code_station, # Code de station
          annee, # Année
          valeur, # Valeur initiale
          valeur_modele, # Valeur utilisée dans le modèle
          .keep_all = TRUE ) |>
        dplyr::group_by(
          code_station ) |> # Regroupe les données station par station
        dplyr::filter(
          dplyr::n_distinct(
            annee ) >= 2 ) |> # Conserve uniquement les stations ayant au moins deux années
        dplyr::ungroup() |> # Supprime le regroupement par station
        dplyr::mutate(
          code_station = factor( code_station ) ) # Transforme la station en facteur pour le modèle mixte

      nombre_stations <- dplyr::n_distinct(  table_reseau$code_station ) # Compte le nombre de stations différentes présentes dans le réseau
      station_presente <- code_station_selectionnee %in% # Verif que la station sel est encore presente
        as.character(
          table_reseau$code_station) # Après le filtrage des stations ayant au moins deux années

      donnees_suffisantes <-
        station_presente && # La station sélectionnée doit être présente
        nombre_stations >= 2 &&# Le réseau doit contenir au moins deux stations
        nrow(table_reseau) >= 6 &&# La table doit contenir au moins six observations
        dplyr::n_distinct(
          table_reseau$annee) >= 3 # Le réseau doit posséder au moins trois années différentes
      if (!donnees_suffisantes) {next } # Si les cond ne sont pas respecté passe au réseau suivant

      # Calcul du modèle linéaire mixte
      modele_reseau <- tryCatch(# Permet d'empêcher l'arrêt complet de la fonction si erreur
        suppressWarnings(# Empêche l'affichage des avertissements produits par lmer()
          lme4::lmer(
            valeur_modele ~ # Variable biologique que le modèle doit expliquer
              annee_c + # Effet fixe de l'années: représente la tendance moyenne de l'ensemble du réseau
              (1 + annee_c | code_station), # Effets aléatoires propres à chaque station : 1= intercept diff pour chaque sttaion et annee_c =pente diff
            data = table_reseau,# Table utilisée pour ajuster le modèle
            REML = TRUE,# Utilise la méthode REML pour estimer les variances des effets aléatoires
            control = lme4::lmerControl(
              optimizer = "bobyqa",# Optimiseur utilisé pour rechercher les paramètres du modèle
              optCtrl = list(
                maxfun = 200000) ,# Autorise jusqu'à 200 000 évaluations pour la convergence
             check.conv.singular = "ignore",# N'affiche pas les avertissements de modèle singulier
              check.conv.grad = "ignore",# N'affiche pas les avertissements liés au gradient
              check.conv.hess = "ignore") ) ) ,# N'affiche pas les avertissements liés à la matrice hessienne
        error = function(erreur) { NULL} )# Si une erreur survient, le modèle est remplacé par NULL

      if (is.null(modele_reseau)) { next} # Si le modele peux pas etre ajusté = réseaux suivant

    # Récupération des effets fixes du modèle (= Tendance moyenne du reseaux)
      coefficients_fixes <- tryCatch(
        lme4::fixef(modele_reseau ), # fixef() récupère uniquement les coefficients fixes = intercept et pente moyenne du reseaux
        error = function(erreur) { NULL} ) # Renvoie NULL si les coef ne peuvent pas etre recup
      if (is.null(coefficients_fixes) || # Vérifie que les coefficients ont bien été récupérés
        !all(
          c(
            "(Intercept)", # Niveau moyen du réseau à l'année de référence
            "annee_c" # Évolution moyenne annuelle du réseau
          ) %in% names(coefficients_fixes) ) ) { next} # Passe au suivant si les coef sont abs

      # Calcul de la tendance générale du réseau
      ajustement_reseau_modele <- # Calcule de la valeur prédite pour chaque année
        unname(
          coefficients_fixes[
            "(Intercept)" ] ) +# Récupère l'intercept moyen du réseau sans conserver son nom
        unname(
          coefficients_fixes[
            "annee_c" ] ) *  # Multiplie la pente moyenne par chaque année centrée
        annees_prediction_station$annee_c # Formule : valeur prédite = intercept du réseau + pente du réseau × année centrée

      # Création de la matrice des prédicteurs
      matrice_prediction <- stats::model.matrix(
        ~ annee_c, # Crée une colonne pour l'intercept et une colonne pour l'année centrée ( Intercept 1 annee-c -5 par exemple)
        data = annees_prediction_station) # Utilise les années sur lesquelles effectuer les prédictions

      # Récupération de la variance des coefficients fixes
      matrice_variance <- tryCatch(
        as.matrix(
          stats::vcov(  # vcov() récupère l'incertitude des coefficients fixes du modèle
            modele_reseau ) ),
        error = function(erreur) { NULL } ) # Renvoie NULL si la matrice ne peut pas être récupérée

      # Calcul de l'erreur standard de chaque prédiction
      if (
        !is.null(matrice_variance) &&# Vérifie que la matrice de variance existe
        ncol(matrice_variance) == ncol(matrice_prediction) ) { # Vérifie que ses dimensions correspondent aux prédicteurs
        erreur_standard <- sqrt(
          pmax(
            diag(
              matrice_prediction %*%
                matrice_variance %*%  # Calcule la variance de chaque valeur prédite :  X × Var(beta) × X'
                t(matrice_prediction) ),   # La racine carrée transforme la variance en erreur standar
            0) ) # Remplace les éventuelles petites valeurs négatives par zéro

      } else { # Si l'erreur standard ne peut pas être calculée
        erreur_standard <- rep(
          NA_real_,# Crée des valeurs manquantes de type numérique
          nrow(annees_prediction_station) ) } # Crée une valeur pour chaque année de prédiction

      # Création des bornes de confiance du réseau
      tendance_reseau <- annees_prediction_station |>
        dplyr::mutate( # A la fin on a la valeur moyen et les intervale de confiance
          ajustement_modele =
            as.numeric(
              ajustement_reseau_modele), # Valeur prédite par le modèle pour chaque année
          borne_basse_modele =
            ajustement_modele -
            1.96 * erreur_standard, # Limite inférieure approximative de l'intervalle de confiance à 95 %
          borne_haute_modele =
            ajustement_modele +
            1.96 * erreur_standard)# Limite supérieure approximative de l'intervalle de confiance à 95 %

      # Retour à l'échelle biologique initiale notamment pour l'IPR
      if (code_indice_courant == "7036") {
        tendance_reseau <- tendance_reseau |>
          dplyr::mutate(
            ajustement = pmax( # Vleur moyenne
              exp( # Inverse le log
                ajustement_modele ) - # Constante retiré
                constante_ipr,0),# 0 empeche les valeur neg
            borne_basse = pmax( # Meme chose pour chaque borne
              exp(
                borne_basse_modele ) -
                constante_ipr,  0 ),
            borne_haute = pmax(
              exp(
                borne_haute_modele ) -
                constante_ipr, 0 ) )
      } else {
        tendance_reseau <- tendance_reseau |> # Pour les eqr, pas de tranformation inverse
          dplyr::mutate(
            ajustement = ajustement_modele,# Valeur ajustée sur l'échelle de l'EQR
            borne_basse = borne_basse_modele, # Borne basse sur l'échelle de l'EQR
            borne_haute = borne_haute_modele) } # Borne haute sur l'échelle de l'EQR

      # Ajout des informations décrivant la tendance du réseau
      tendance_reseau <- tendance_reseau |>
        dplyr::mutate(
          reseau = reseau_courant,# Nom du réseau actuellement traité
          compartiment = compartiment_courant, # Compartiment biologique correspondant
          code_indice = code_indice_courant,# Code de l'indice biologique
          nombre_stations = nombre_stations  ) |># Nombre de stations utilisées dans le modèle
        dplyr::select(
          annee, # Année réelle
          annee_c, # Année centrée
          ajustement, # Valeur moyenne prédite pour le réseau
          borne_basse, # Borne inférieure de l'intervalle de confiance
          borne_haute, # Borne supérieure de l'intervalle de confiance
          reseau, # Réseau concerné
          compartiment, # Compartiment biologique
          code_indice, # Code de l'indice
          nombre_stations) # Nombre de stations du modèle

      # Tendance propre à la station
      coefficients_stations <- tryCatch(
        stats::coef( # Recupere les coef complet de chaque station avec les fixes et aléatoire
          modele_reseau )$code_station,# Récupère la table des coefficients associés au facteur code_station
        error = function(erreur) {
          NULL } )# Renvoie NULL si les coefficients ne peuvent pas être récupérés


      # Vérification de la présence des coefficients de la station sélectionnée
      coefficients_station_disponibles <-
        !is.null(coefficients_stations) && # La table des coefficients doit exister
        code_station_selectionnee %in%
        rownames(coefficients_stations) && # La station sélectionnée doit apparaître dans les lignes
        all(
          c("(Intercept)", # Intercept propre à la station
            "annee_c" # Pente propre à la station
          ) %in% colnames(coefficients_stations)) # Les deux coefficients doivent apparaître dans les colonnes

      if (coefficients_station_disponibles) { # Si les coef sont dispo
        intercept_station <- as.numeric( # Recupe  le niveau propre à la station à l’année de référence.
          coefficients_stations[
            code_station_selectionnee, # Ligne de la station choisie
            "(Intercept)"]) # Colonne de l'intercept
        pente_station <- as.numeric( # Recup de l'evolution annuelle de la la station
          coefficients_stations[
            code_station_selectionnee, # Ligne de la station choisie
            "annee_c" ] )# Colonne de la pente temporelle

        coefficients_valides <- # Verif des coef
          is.finite(intercept_station) && # Vérifie que l'intercept n'est ni NA ni infini
          is.finite(pente_station) # Vérifie que la pente n'est ni NA ni infinie

        if (coefficients_valides) { # Si les coef sont valides

          # Calcul de la tendance propre à la station
          ajustement_station_modele <-
            intercept_station +
            pente_station * # Formule
            annees_prediction_station$annee_c

          # Création de la table de tendance de la station
          tendance_station_reseau <-
            annees_prediction_station |>
            dplyr::mutate(
              ajustement_modele =
                as.numeric(
                  ajustement_station_modele) ) # Ajoute les valeurs prédites par le modèle

          # Retour à l'échelle initiale pour l'IPR, meme fonctionnement que haut
          if (code_indice_courant == "7036") {
            tendance_station_reseau <-
              tendance_station_reseau |>
              dplyr::mutate(
                ajustement = pmax(
                  exp(
                    ajustement_modele) -
                    constante_ipr, 0 ))
          } else { # Pas besoin pour les EQR
            tendance_station_reseau <-
              tendance_station_reseau |>
              dplyr::mutate(
                ajustement = ajustement_modele) }

          # Ajout des informations décrivant la tendance de la station
          tendance_station_reseau <-
            tendance_station_reseau |>
            dplyr::mutate(
              code_station = code_station_selectionnee,
              reseau_modele = reseau_courant,
              compartiment = compartiment_courant,
              code_indice = code_indice_courant,
              intercept_station = intercept_station,
              pente_station = pente_station ) |>
            dplyr::select(
              annee,
              annee_c,
              ajustement,
              code_station,
              reseau_modele,
              compartiment,
              code_indice,
              intercept_station,
              pente_station  )

          # Enregistrement de la tendance de la station sous le nom du réseau
          liste_tendances_station[[reseau_courant]] <-tendance_station_reseau }} # Fin de la boucle sur chaque réseaux

      # Enregistrement de la tendance générale du réseau
      liste_tendances_reseaux[[reseau_courant]] <-tendance_reseau

      # Enregistrement du modèle statistique
      liste_modeles[[
        paste(
          code_indice_courant, # Code de l'indice
          reseau_courant, # Nom du réseau
          sep = "_" )# Sépare les deux éléments par un tiret bas
      ]] <- modele_reseau } # Exemple de nom : "7613_RCS"

# --------------------Fin de la boucle ----------------------------------

# Assemblage verticalement les tendances pour les différents réseaux
    tendances_reseaux <- dplyr::bind_rows(liste_tendances_reseaux)
# Assemble les tendances de la station obtenues avec les différents réseaux
    tendances_station_lmm <- dplyr::bind_rows(liste_tendances_station)

# Création de la tendance définitive de la station
    if (nrow(tendances_station_lmm) > 0) { # Verifie si on a au moisn une tendance propre a la station
      tendance_station <- tendances_station_lmm |> # Si la station appartient a plusieur réseaux = plusieurs models avec plusieurs valeurs
        dplyr::group_by( # Regroupe
          annee,
          annee_c,
          code_station,
          compartiment,
          code_indice  ) |>
        dplyr::summarise(
          ajustement = mean( # Calcule la moyenne des prédictions obtenues
            ajustement, # Avec les diff modele du réseaux
            na.rm = TRUE ),
          reseau_modele = paste(
            sort(
              unique(
                reseau_modele) ),  # Récupère les réseaux différents et les trie
            collapse = " / "), # Regroupe les noms des réseaux dans une seule chaîne
          methode = "Modèle linéaire mixte", # Indique la méthode utilisée pour calculer la tendance
          .groups = "drop") # Supprime les regroupements après le résumé

    } else { # Si jaamis pas de tendance propre a la station = modele linéeaire simple de secours
      modele_station_secours <- tryCatch(
        stats::lm(
          valeur_modele ~ annee_c,# Régression linéaire de la valeur en fonction de l'année
          data = observations_station_modele), # Utilise uniquement les observations de la station sélectionnée
       error = function(erreur) { NULL} ) # Renvoie NULL si la régression linéaire ne fonctionne pas

      if (is.null(modele_station_secours)) {next} # Si jamais ca fonctionne pas non plus = indice suivant

# ---------------------- Secours si jamais -------------------------------------------------------------
      prediction_station <- tryCatch( # Calcule des prédictions du modele de secours
        as.numeric(
          stats::predict(
            modele_station_secours, # Modèle linéaire simple calculé pour la station
            newdata = annees_prediction_station) ), # Années sur lesquelles les valeurs doivent être prédites
        error = function(erreur) {
          rep(
            NA_real_,# Crée des valeurs manquantes numériques
            nrow(annees_prediction_station) ) } )# Une valeur manquante pour chaque année

      tendance_station <- annees_prediction_station |> # Crée la table de tendance de secours
        dplyr::mutate(
          ajustement_modele = prediction_station) # Ajoute les valeurs prédites par la régression simple

      if (code_indice_courant == "7036") { # Retour à l'initiale pour IPR
        tendance_station <- tendance_station |>
          dplyr::mutate(
            ajustement = pmax(
              exp(
                ajustement_modele) -
                constante_ipr, 0 ))
      } else {
        tendance_station <- tendance_station |> # Pas besoin pour EQR
          dplyr::mutate(
            ajustement = ajustement_modele )  }

      tendance_station <- tendance_station |> # Ajout des info decrivant la tendnace de secours
        dplyr::mutate(
          code_station = code_station_selectionnee,
          compartiment =compartiment_courant,
          code_indice =code_indice_courant,
          reseau_modele = paste(
            reseaux_station,
            collapse = " / " ),
          methode =
            "Modèle linéaire propre à la station") |>
        dplyr::select(
          annee,
          annee_c,
          ajustement,
          code_station,
          compartiment,
          code_indice,
          reseau_modele,
          methode  )

      liste_modeles[[ # Enregistrement
        paste0(
          code_indice_courant,
          "_station_secours")# Exemple de nom : "7613_station_secours"
      ]] <- modele_station_secours}

    # --------------------Fin du secours --------------------------------------

# Vérification de la tendance finale de la station
    tendance_station <- tendance_station |>
      dplyr::filter(
        !is.na(ajustement), # Supprime les prédictions manquantes
        is.finite(ajustement)) # Supprime les prédictions infinies
    if (nrow(tendance_station) < 2) { next } # Au moins 2 valeurs pour tracer la ligne, si y'a pas passe au suivant


# Préparation des observations affichées sur le graphique
    observations_graphique <- observations_station |>
      dplyr::transmute(
        code_station,
        libelle_station,
        annee,
        valeur,
        compartiment = compartiment_courant,
        code_indice = code_indice_courant )


# Enregistrement des données nécessaires au graphique
    liste_graphiques[[code_indice_courant]] <- list(
      observations = observations_graphique, # Points correspondant aux valeurs réellement observées
      tendance_station = tendance_station, # Courbe de tendance propre à la station
      tendances_reseaux = tendances_reseaux,# Courbes moyennes des réseaux et leurs intervalles de confiance
      compartiment = compartiment_courant, # Compartiment biologique du graphique
      code_indice = code_indice_courant, # Code de l'indice représenté
      ordre = ordre_courant) # Position du graphique dans l'ordre d'affichage


# -------------------Tableau exportable----------------------
# Preparation
# Partie 1 : valeurs réellement observées
    tableau_observations <- observations_graphique |>
      dplyr::transmute(
        code_station,
        libelle_station,
        compartiment,
        code_indice,
        annee, #
        reseau = paste(
          reseaux_station,
          collapse = " / "), # Regroupe tous les réseaux de la station dans une même cellule
        type_donnee = "Observation",# Indique qu'il s'agit d'une valeur réellement observée
        methode = NA_character_, # Aucune méthode statistique n'est associée à une observation brute
        valeur_observee = valeur,# Place la valeur dans la colonne des observations
        valeur_ajustee = NA_real_,# Aucune valeur ajustée pour cette ligne
        borne_basse = NA_real_,# Aucun intervalle de confiance pour l'observation brute
        borne_haute = NA_real_)# Aucun intervalle de confiance pour l'observation brute

# Partie 2 : tendance propre à la station
    tableau_station <- tendance_station |>
      dplyr::transmute(
        code_station,
        libelle_station =observations_graphique$libelle_station[1], # Récupère le nom de la station depuis la première observation
        compartiment,
        code_indice,
        annee,
        reseau = reseau_modele, # Réseau ou réseaux ayant servi à calculer la tendance
        type_donnee = "Tendance station",# Indique qu'il s'agit de la tendance propre à la station
        methode, # Méthode utilisée : LMM ou modèle linéaire simple
        valeur_observee = NA_real_,
        valeur_ajustee = ajustement, # Valeur prédite pour la station
        borne_basse = NA_real_,
        borne_haute = NA_real_)

# Partie 3 : tendances générales des réseaux
  if (nrow(tendances_reseaux) > 0) { # Si des tendaneces existent
      tableau_reseaux <- tendances_reseaux |>
        dplyr::transmute(
          code_station = code_station_selectionnee,
          libelle_station = observations_graphique$libelle_station[1],
          compartiment,
          code_indice,
          annee,
          reseau,
          type_donnee = "Tendance réseau", # Indique qu'il s'agit de la tendance moyenne du réseau
          methode = "Modèle linéaire mixte",# Les tendances réseau proviennent toujours du LMM
          valeur_observee = NA_real_,
          valeur_ajustee = ajustement,
          borne_basse,# Borne inférieure de l'intervalle de confiance
          borne_haute ) # Borne supérieure de l'intervalle de confiance
    } else { tableau_reseaux <- tibble::tibble()} # Création d'un tableau vide


# Assemblage des trois types de données du compartiment
    liste_tableaux[[code_indice_courant]] <-
      dplyr::bind_rows(
        tableau_observations,
        tableau_station,
        tableau_reseaux ) }


# Assemblage des résultats de tout les indices
  tableau_final <- dplyr::bind_rows(liste_tableaux )

  if (nrow(tableau_final) > 0) {  # Trie le tableau uniquement s'il contient au moins une ligne
    tableau_final <- tableau_final |>
      dplyr::arrange(
        compartiment,
        annee,
        type_donnee,
        reseau ) }


# Renvoi des résultats finaux
  return(
    list(
      graphiques = liste_graphiques, # Données nécessaires pour construire les différents graphiques
      tableau = tableau_final, # Tableau complet destiné à l'affichage et à l'export
      modeles = liste_modeles, # Modèles LMM et modèles linéaires simples conservés pour vérification
      reseaux = reseaux_station ) ) }# Réseaux auxquels appartient la station sélectionnée




#--------------------------------------------------------------------------------------------

#' Création d'un graphique des tendances temporelles
#'
#' @description Cette fonction affiche les observations de la station, sa tendance temporelle et la tendance de ses réseaux.
#' @return Un graphique ggplot.
#' @export

fun_plot_tendance_indice <- function(donnees_tendance) {

# Récupération des données
  observations <-
    donnees_tendance$observations
  tendance_station <-
    donnees_tendance$tendance_station
  tendances_reseaux <-
    donnees_tendance$tendances_reseaux
  compartiment <-
    donnees_tendance$compartiment
  code_indice <-
    donnees_tendance$code_indice

# Création du graphique

  graphique <- ggplot2::ggplot()

# Ajout des points observés
  graphique <- graphique +
    ggplot2::geom_point( # Points
      data = observations,
      mapping = ggplot2::aes( # Définition des variables utilisées par le graphique
        x = annee,
        y = valeur,
        text = paste0( # Texte affiché au survol
          "<b>Station :</b> ", # Titre de la station en gras
          libelle_station, # Nom de la station
          "<br><b>Année :</b> ", # Retour à la ligne puis titre de l'année
          annee, # Année de l'observation
          "<br><b>Valeur observée :</b> ", # Retour à la ligne puis titre de la valeur
          round( # Arrondi de la valeur observée
            valeur,
            3) ) ), # Trois chiffres après la virgule
      colour = "#B94FC2", # Couleur violette des points observés
      size = 3, # Taille des points
      alpha = 0.95) # Transparence légère des points

# Ajout de la tendance de la station
  graphique <- graphique +
    ggplot2::geom_line( # Ligne
      data = tendance_station,
      mapping = ggplot2::aes( # Définition des variables utilisées pour la ligne
        x = annee,
        y = ajustement,
        colour = "Station", # Nom utilisé pour la couleur et la légende
        group = 1, # Regroupement de toutes les valeurs dans une seule ligne
        text = paste0(
          "<b>Tendance de la station</b>",
          "<br><b>Année :</b> ",
          annee,
          "<br><b>Valeur ajustée :</b> ",
          round(
            ajustement,
            3) ) ),
      linewidth = 1.3, # Épaisseur de la ligne de tendance
      lineend = "round", # Extrémités arrondies de la ligne
      na.rm = TRUE) # Suppression des valeurs manquantes sans afficher d'avertissement

# Ajout des tendances des réseaux
  if ( # Vérification de la présence de tendances pour les réseaux
    !is.null(tendances_reseaux) && # Objet des réseaux existe
    nrow(tendances_reseaux) > 0 #Table contient au moins une ligne
  ) {
    # Borne basse de l'intervalle
    graphique <- graphique +
      ggplot2::geom_line(
        data = tendances_reseaux,
        mapping = ggplot2::aes(
          x = annee,
          y = borne_basse,
          colour = paste0("Réseau ", reseau),
          group = reseau),
        linewidth = 0.75,
        linetype = "dotted",
        alpha = 0.9,
        na.rm = TRUE,
        show.legend = FALSE)

    # Borne haute de l'intervalle
    graphique <- graphique +
      ggplot2::geom_line(
        data = tendances_reseaux,
        mapping = ggplot2::aes(
          x = annee,
          y = borne_haute,
          colour = paste0("Réseau ", reseau),
          group = reseau),
        linewidth = 0.75,
        linetype = "dotted",
        alpha = 0.9,
        na.rm = TRUE,
        show.legend = FALSE)

# Tendance générale des réseaux
    graphique <- graphique +
      ggplot2::geom_line( # Ajout de la tendance moyenne de chaque réseau
        data = tendances_reseaux,
        mapping = ggplot2::aes(
          x = annee,
          y = ajustement,
          colour = paste0( # Création du nom affiché dans la légende
            "Réseau ", # Texte placé avant le nom du réseau
            reseau), # Nom du réseau
          group = reseau, # Création d'une ligne différente pour chaque réseau
          text = paste0( # Texte affiché au survol
            "<b>Réseau :</b> ",
            reseau,
            "<br><b>Année :</b> ",
            annee,
            "<br><b>Valeur ajustée :</b> ",
            round(
              ajustement,
              3),
            "<br><b>Intervalle à 95 % :</b> ",
            round(
              borne_basse,
              3),
            " à ",
            round(
              borne_haute,
              3) ) ),
        linewidth = 1.2,
        linetype = "dashed",
        na.rm = TRUE)}

# Recherche des réseaux présents
  if ( # Vérification de la présence de tendances pour les réseaux
    !is.null(tendances_reseaux) && # Vérifie que l'objet existe
    nrow(tendances_reseaux) > 0 # Vérifie que la table n'est pas vide
  ) { noms_reseaux <- sort( # Classement des noms des réseaux par ordre alphabétique
      unique( # Conservation d'une seule occurrence de chaque réseau
        tendances_reseaux$reseau) ) # Colonne contenant le nom des réseaux
  } else { # Cas où aucune tendance de réseau n'est disponible
    noms_reseaux <- character()} # Création d'un vecteur de caractères vide


# Création des couleurs des réseaux
  # Création d'une couleur différente pour chaque réseau
  couleurs_reseaux <- stats::setNames(
    grDevices::hcl.colors(
      n = length(noms_reseaux),
      palette = "Dark 3"),
    paste0(
      "Réseau ",
      noms_reseaux) )


# Couleurs de la légende
  couleurs_tendances <- c(
    "Station" = "#A600B8",
    couleurs_reseaux)


# Recherche des années disponibles
  annees_disponibles <- sort( # Classement des années dans l'ordre croissant
    unique( # Une seule fois chaque année
      c( # Regroupement
        observations$annee, # Années des observations de la station
        tendance_station$annee, # Années de la tendance de la station
        if ( # Vérification des réseaux
          !is.null(tendances_reseaux) && # Vérifie que l'objet existe
          nrow(tendances_reseaux) > 0 # Vérifie que la table n'est pas vide
        ) {tendances_reseaux$annee # Ajout des années des tendances des réseaux
        } else { numeric()} ) ) ) # Ajout d'un vecteur numérique vide si aucun réseau n'est présent


  # Suppression des années invalides
  annees_disponibles <- annees_disponibles[ # Filtrage des années disponibles
    !is.na(annees_disponibles) & # Conservation des années qui ne sont pas manquantes
      is.finite(annees_disponibles)] # Conservation des années qui sont des nombres finis


  # Graduations des années
  if (length(annees_disponibles) <= 10) { # Cas où dix années ou moins sont disponibles
    graduations_annees <-
      annees_disponibles # Affichage de toutes les années disponibles
  } else { # Cas où plus de dix années sont disponibles
    graduations_annees <- pretty( # Création automatique de graduations lisibles
      range( # Recherche de l'année minimale et de l'année maximale
        annees_disponibles), # Ensemble des années disponibles
      n = 8 ) } # Demande d'environ huit graduations sur l'axe


  # Mise en forme générale
  graphique <- graphique +
    ggplot2::scale_colour_manual( # Attribution manuelle des couleurs des tendances
      name = "Tendance", # Titre
      values = couleurs_tendances, # Couleurs
      breaks = names( # Ordre des éléments affichés dans la légende
        couleurs_tendances ) , # Noms contenus dans le vecteur de couleurs
      drop = FALSE)+  # Conservation de tous les niveaux indiqués dans la légende
    ggplot2::scale_x_continuous( # Mise en forme de l'axe des années
      breaks = graduations_annees ) + # Graduations affichées sur l'axe des abscisses
    ggplot2::labs( # Définition des titres du graphique et des axes
      title = paste( # Création du titre principal du graphique
        "Indice",
        compartiment),
      x = "Année", # Titre de l'axe des abscisses
      y = if ( # Choix du titre de l'axe des ordonnées selon l'indice
        code_indice == "7036" # Vérifie si l'indice correspond à l'IPR
      ) {"Résultat IPR" # Pour l'IPR
      } else { "EQR"}, # Titre utilisé pour les autres indices
      colour = "Tendance" ) +
    ggplot2::theme_bw() + # Application d'un thème blanc avec bordures noires
    ggplot2::theme( # Personnalisation de l'apparence du graphique
      plot.title = ggplot2::element_text( # Mise en forme du titre principal
        size = 14, # Taille du titre
        face = "bold", # Titre affiché en gras
        hjust = 0.5), # Centrage horizontal du titre
      axis.title.x = ggplot2::element_text( # Mise en forme du titre de l'axe des abscisses
        size = 11, # Taille du texte
        face = "bold", # Texte affiché en gras
        margin = ggplot2::margin( # Ajout d'une marge autour du titre
          t = 10) ), # Marge de dix pixels au-dessus du titre
      axis.title.y = ggplot2::element_text( # Mise en forme du titre de l'axe des ordonnées
        size = 11, # Taille du texte
        face = "bold", # Texte affiché en gras
        margin = ggplot2::margin( # Ajout d'une marge autour du titre
          r = 10) ), # Marge de dix pixels à droite du titre
      axis.text = ggplot2::element_text( # Mise en forme des valeurs des axes
        size = 10 ), # Taille des graduations des axes
      panel.grid.minor =
        ggplot2::element_blank(), # Suppression des lignes secondaires de la grille
      legend.position =
        "bottom", # Positionnement de la légende sous le graphique
      legend.justification =
        "left", # Alignement du contenu de la légende vers la gauche
      legend.box.just =
        "left", # Alignement de la boîte de légende vers la gauche
      legend.title = ggplot2::element_text( # Mise en forme du titre de la légende
        size = 10, # Taille du titre de la légende
        face = "bold"), # Titre de la légende affiché en gras
      legend.text = ggplot2::element_text( # Mise en forme du texte de la légende
        size = 9), # Taille du nom des tendances
      plot.margin = ggplot2::margin( # Définition des marges extérieures du graphique
        t = 10, # Marge supérieure
        r = 10, # Marge à droite
        b = 5, # Marge inférieure
        l = 10) ) # Marge à gauche


# Échelle particulière de l'IPR
  if (code_indice == "7036") { # Vérifie si le graphique concerne l'IPR
    graphique <- graphique + # Les faibles valeurs correspondent au meilleur état
      ggplot2::scale_y_reverse( # Inversion de l'axe des ordonnées
        expand = ggplot2::expansion( # Ajout d'un espace autour des valeurs
          mult = c( # Définition de l'espace en bas et en haut de l'axe
            0.05, # Ajout de 5 % d'espace d'un côté de l'axe
            0.08 ) ) )# Ajout de 8 % d'espace de l'autre côté de l'axe

  } else { # Cas des indices exprimés sous forme d'EQR
    graphique <- graphique +
      ggplot2::scale_y_continuous( # Création d'un axe des ordonnées continu
        limits = c( # Définition des limites de l'axe
          0, # L'axe commence à zéro
          1.1), # L'axe se termine à 1,1
        breaks = seq( # Création régulière des graduations de l'axe
          0, # Première graduation à zéro
          1, # Dernière graduation affichée à un
          by = 0.25), # Espacement de 0,25 entre chaque graduation
        expand = ggplot2::expansion( # Définition de l'espace autour de l'axe
          mult = c( # Pourcentage d'espace en bas et en haut
            0, # Aucun espace sous la valeur zéro
            0.03) ) , # Ajout de 3 % d'espace au-dessus
        oob = scales::squish) } # Ramène dans les limites les valeurs dépassant l'axe

  # Renvoi du graphique

  return( graphique ) } # Renvoi du graph
