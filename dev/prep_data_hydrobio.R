#### Packages ####
library(hubeau)# Package pour la recup des données
library(shiny)
library(dplyr) # Package pour le traitement
library(purrr) # Permet de remplacer des boucles for
library(lubridate) # Permet de filter les années par la suite
library(stringr) # Travailler proprement avec les chaînes de caractères
library(tidyr) # Pour la mise en forme des données
library(sf) # Manipuler des données géographiques (cartes)
library(SEEEapi) #Permet de calculer les indices et le diag
# |> = prends ca et passe-le à la fonction
# %>% = meme chose mais + souple et issue de dplyr et . =place l'object

#### Recuperer les données####
# Stations
stations <- get_hydrobio_stations_hydrobio(code_region = "28") %>%
  distinct(code_station_hydrobio, .keep_all = TRUE) # Sert a gardrer 1 ligne par station
codes <- stations$code_station_hydrobio # Extraction des codes stations pour la suite

# Fonction qui permet la sécurité
safe_get <- function(fun, ..., sleep = 0.2) { # Permet de faire des pauses de 0.2 secondes
  out <- tryCatch(
    fun(...),
    error = function(e) {
      message("Erreur: ", conditionMessage(e))   #si jamais une erreur se produit pour que le script continue
      return(NULL) })
  Sys.sleep(sleep)
  out }

# Taxon
taxons <- map_dfr(codes, function(cd) { # Fonction pour recuperer pour chaque code
  res <- safe_get(   # Recupere les données hub'eau en gerant les erreurs
    get_hydrobio_taxons,
    code_station_hydrobio = cd )
  if (is.null(res)) {   # Permet de voir quelle station echoue
    return(tibble(code_station_hydrobio = cd, .error = TRUE))}
  res %>%
    mutate(code_station_hydrobio = cd,
           .error = FALSE)})

# Indices
indices <- map_dfr(codes, function(cd) {
  res <- safe_get(
    get_hydrobio_indices,
    code_station_hydrobio = cd)
  if (is.null(res)) {
    return(tibble(code_station_hydrobio = cd, .error = TRUE))}
  res %>%
    mutate(code_station_hydrobio = cd,
           .error = FALSE)})
# Meme fonctionnement que Taxon

#### Table Stations avec toutes les info####
dates_station <- bind_rows(
  taxons %>%
    transmute(
      code_station = as.character(code_station),
      date_prelevement = as.Date(date_prelevement) ),
  indices %>%
    transmute(
      code_station = as.character(code_station),
      date_prelevement = as.Date(date_prelevement) ) ) %>%
  filter(!is.na(code_station), !is.na(date_prelevement) ) %>%
  group_by(code_station) %>%
  summarise(
    date_premier_prelevement = min(date_prelevement),
    date_dernier_prelevement = max(date_prelevement),
    .groups = "drop" )

# Nettoyer stations avec seulement les informations utiles
stations <- stations %>%
  dplyr::select(
    code_station = code_station_hydrobio,
    libelle_station = libelle_station_hydrobio,
    uri_station = uri_station_hydrobio,
    coordonnee_x,
    coordonnee_y,
    code_cours_eau,
    libelle_cours_eau,
    code_masse_eau,
    libelle_masse_eau,
    code_dep = code_departement)

# Joindre les dates de prelevement
stations <- stations %>%
  dplyr::left_join(
    dates_station,
    by = "code_station")

# Afficher la geometry comme IDF
stations <- stations %>% mutate(coordonnees = paste0("(", coordonnee_x, ", ", coordonnee_y, ")"))

# Aller chercher les données de typo, altitude et reseaux
Base_station <- readxl::read_excel("Base station active.xlsx", sheet = "Base active")
base_station <- Base_station %>%
  dplyr::select(
    code_station = CD_STATION,
    typologie = TYPE,
    altitude = ALTITUDE,
    reseau = RESEAU)

# Attention, pas la meme facon de noter code_station
base_station$code_station <- stringr::str_pad(
  as.character(base_station$code_station),
  width = 8,
  pad = "0")

# Joindre les 2
stations <- dplyr::left_join(
  stations,
  base_station,
  by = "code_station")

# Aller chercher les rang de stralher
extraction_strahler <- readxl::read_excel("extraction REF MESU EDL 2025.xlsx")
strahler <- extraction_strahler %>%
  dplyr::select(
    code_masse_eau = CdEuMasseD,
    StrahlMax)
# Joindre les 2
stations <- stations %>%
  dplyr::left_join(strahler, by = "code_masse_eau")

# Aller chercher les typo
extraction_typo <- readxl::read_excel("extraction_typo.xlsx")
typo <- extraction_typo %>%
  dplyr::select(
    code_station = Code,
    typologie = `Type Masse Deau CoursDEau`)
stations <- stations %>%
  select(-typologie) %>%
  left_join(typo, by = "code_station")

# Pour les uh
# Lecture du shapefile des UH
uh <- sf::st_read(
  "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/données/UH/UH_actives_simplifiees.shp",
  quiet = TRUE)
stations <- stations %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154, remove = FALSE)

# Vérification des systèmes de coordonnées (CRS)
sf::st_crs(uh)
sf::st_crs(stations)

# Transformation de la couche UH dans le CRS des stations
uh <- sf::st_transform(uh, sf::st_crs(stations))

# Jointure spatiale
stations <- sf::st_join(stations, uh, join = sf::st_within)

# Vérification des colonnes après jointure
names(stations)

# On standardise le nom pour l'application Shiny
stations <- stations %>%
  dplyr::mutate(
    UH_calculee = NOM)

# Vérification du nombre de stations sans UH attribuée
table(is.na(stations$UH_calculee))

# Comptage du nombre de stations par unité hydrographique
stations %>%
  sf::st_drop_geometry() %>%
  dplyr::count(UH_calculee, sort = TRUE)

#On garde seulement les stations avec des indices
stations <-stations %>%
  dplyr::semi_join(indices, by = "code_station")

#On ajoute l'ID du bassin versant
station_bv <- read.csv("station_bv.csv", stringsAsFactors = FALSE) # Import de la table station_bv
station_bv <- station_bv %>% # On renome
  dplyr::rename( id_BV = CdOH)

station_bv <- station_bv %>% # Harmonisation des codes station sur 8 caractères
  dplyr::mutate(
    code_station = stringr::str_pad(
      as.character(code_station),
      width = 8,
      side = "left",
      pad = "0" ) )

stations <- stations %>% #Jointure
  dplyr::left_join(
    station_bv %>%
      dplyr::select(code_station, id_BV),
    by = "code_station" )

#### Mettre en forme Taxons#####
taxons <- taxons %>%
  dplyr::select(
    code_station= code_station_hydrobio ,
    libelle_station= libelle_station_hydrobio,
    coordonnee_x,
    coordonnee_y,
    date_prelevement,
    code_prelevement,
    code_support,
    libelle_support,
    code_appel_taxon,
    libelle_taxon= libelle_appel_taxon,
    resultat_taxon,
    code_phase= code_lot,
    code_remarque= code_type_resultat
  ) %>%
  dplyr::mutate(date_prelevement = as.Date(date_prelevement)) %>% # Mettre au bon format l'année
  dplyr::group_by(code_prelevement) %>%
  dplyr::mutate(abondance_relative = resultat_taxon / sum(resultat_taxon, na.rm = TRUE)) %>%
  dplyr::ungroup()  # Permet de ajouter et de calculer l'adondance pour le graph

#Garder seulement ceux qui nous interresse
taxons <- taxons %>%
  dplyr::filter(code_station %in% stations$code_station)
#### Mettre en forme indice#####
indices <- indices %>%
  dplyr::select(
    code_station = code_station_hydrobio,
    code_support,
    libelle_support,
    date_prelevement,
    code_prelevement,
    code_indice,
    libelle_indice,
    resultat_indice,
    code_qualification,
    libelle_qualification
  ) %>%
  dplyr::mutate(
    date_prelevement = as.Date(date_prelevement),
    annee = year(date_prelevement)
  ) %>%
  dplyr::filter(code_indice %in% c(  # Filtrer les indices
    "1022","5856","7613","5910","7036",
    "2928","8058","8056","8057","8054","8050"))

# On a toutes les informations necessaires pour la suite , comme dans IDF on va creer notre jeu de données etat_bio contenant l'EQR calculé

#### Table etat_bio#####
# Harmonisation des indices afin de pas avoir des soucis avec la manip suivante
indices <- indices %>%
  mutate(
    code_station = as.character(code_station),
    code_indice = as.numeric(code_indice),
    annee = as.numeric(annee))
# On garde les codes d'indices utile pour le calcul de l'EQR
codes_etat_bio <- c(1022, 5856, 7613, 5910, 7036, 2928)

# Creation de la nouvelle table issue d'indice en gardant seulement IPR, I2M2, IBMR, IPS, IBD et IBG eq
indices_etat_bio <- indices %>%
  filter(code_indice %in% codes_etat_bio) %>%
  mutate(
    libelle_indice = case_when(
      code_indice == 1022 ~ "IPS",
      code_indice == 5856 ~ "IBD",
      code_indice == 7613 ~ "I2M2",
      code_indice == 5910 ~ "IBG équivalent",
      code_indice == 7036 ~ "IPR",
      code_indice == 2928 ~ "IBMR",
      TRUE ~ libelle_indice))

# Preparation stations SEEE car SEEE.API a besoin d'un format particulier
stations_seee <- stations %>%
  mutate(
    CODE_STATION = as.character(code_station),
    TYPO_NATIONALE = typologie,
    TG_BV = if_else(str_detect(typologie, "TG"), "OUI", "NON"), # Permet d'afficher si y'a TG dans la typo ( important pour diatomée)
    PERIODE_DEBUT = year(as.Date(date_premier_prelevement)),
    PERIODE_FIN = year(as.Date(date_dernier_prelevement)),
    PERIODE_DEBUT = coalesce(PERIODE_DEBUT, PERIODE_FIN),
    PERIODE_FIN = coalesce(PERIODE_FIN, PERIODE_DEBUT)) %>% # Permet de gerer les NA d'année
  filter(!is.na(TYPO_NATIONALE), !is.na(PERIODE_DEBUT), !is.na(PERIODE_FIN)) %>%
  rowwise() %>%
  mutate(ANNEE = list(seq(PERIODE_DEBUT, PERIODE_FIN))) %>%
  unnest(ANNEE) %>% # Une ligne par station et par année
  select(
    CODE_STATION,
    TYPO_NATIONALE,
    TG_BV,
    PERIODE_DEBUT = ANNEE,
    PERIODE_FIN = ANNEE)

# Preparation de indice_SEEE car il faut un format particulier aussi sur indice
indices_seee <- indices_etat_bio %>%
  transmute(
    CODE_OPERATION = code_prelevement,
    CODE_STATION   = as.character(code_station),
    DATE           = format(as.Date(date_prelevement), "%d/%m/%Y"),
    CODE_PAR       = as.character(code_indice),
    LIB_PAR        = libelle_indice,
    RESULTAT       = resultat_indice ) %>%
  filter(
    !is.na(CODE_OPERATION),
    !is.na(CODE_STATION),
    !is.na(DATE),
    !is.na(CODE_PAR),
    !is.na(RESULTAT))

# On ajoute ALT pour l'IPR = obligatoire
# La valeur de 200m est utilisée par défaut car elle n'influence pas le calcul ici
alt_ipr <- indices_etat_bio %>%
  filter(code_indice == 7036) %>%
  transmute(
    CODE_OPERATION = code_prelevement,
    CODE_STATION   = as.character(code_station),
    DATE           = format(as.Date(date_prelevement), "%d/%m/%Y")) %>%
  distinct() %>%
  mutate(
    CODE_PAR = "NA",
    LIB_PAR  = "ALT",
    RESULTAT = 200)

indices_seee <- bind_rows(indices_seee, alt_ipr) # On joins les 2

 #On cree des sous-table par compartiment
indices_diat <- indices_seee %>%
  filter(CODE_PAR %in% c("1022", "5856"))
indices_macro <- indices_seee %>%
  filter(CODE_PAR == "2928")
indices_invert <- indices_seee %>%
  filter(CODE_PAR == "7613")
indices_poisson <- indices_seee %>%
  filter(CODE_PAR %in% c("7036", "NA")) %>%
  arrange(CODE_STATION, CODE_OPERATION, CODE_PAR)

as.tbl <- tibble::as_tibble
# On calcule le SEEE 2018 comme IDF
etat_2018 <- SEEEapi::calc_indic(
  indic = "EBio_CE_2018",
  version = "1.0.2",
  locally = TRUE,
  dir_algo = "algo_seee", # Ficher telechargé de Cedric mondy qui contient l'agorythme necessaire
  data = list(
    stations_seee,
    indices_diat,
    indices_macro,
    indices_invert,
    indices_poisson
  ))$result

# Puis le 2015
etat_2015 <- SEEEapi::calc_indic(
  indic = "EBio_CE_2015",
  version = "1.0.1",
  locally = TRUE,
  dir_algo = "algo_seee",
  data = list(
    stations_seee,
    indices_seee %>% slice(0),
    indices_seee %>% slice(0),
    indices_seee %>% filter(CODE_PAR == "5910"),
    indices_seee %>% slice(0)
  ))$result

# Construction de la table finale des états biologiques :
# on regroupe les résultats issus des calculs SEEE 2018 et 2015,
# puis on les met dans un format lisible.

etat_bio <- bind_rows(etat_2018, etat_2015) %>%
  filter(!is.na(RESULTAT)) %>%
  transmute(
    code_station    = as.character(CODE_STATION),
    annee           = as.numeric(PERIODE_DEBUT),
    code_indice     = suppressWarnings(as.numeric(as.character(CODE_PAR))),
    libelle_indice  = as.character(LIB_PAR),
    resultat_indice = as.numeric(gsub(",", ".", as.character(RESULTAT))),
    eqr_indice      = as.numeric(gsub(",", ".", as.character(EQR))),
    classe_indice   = na_if(as.character(CLASSE), "") ) %>%
  left_join(
    indices_etat_bio %>%
      distinct(code_indice, code_support, libelle_support),
    by = "code_indice" ) %>%
  mutate(
    libelle_indice = case_when(
      code_indice == 1022 ~ "IPS",
      code_indice == 5856 ~ "IBD",
      code_indice == 7613 ~ "I2M2",
      code_indice == 5910 ~ "IBG équivalent",
      code_indice == 7036 ~ "IPR",
      code_indice == 2928 ~ "IBMR",
      TRUE ~ libelle_indice ) ) %>%
  distinct() %>%
  arrange(code_station, annee, code_indice)

# Ajouter l'IPS à titre informatif dans etat_bio sans calculer EQR
ips_info <- indices_etat_bio %>%
  filter(code_indice == 1022) %>%
  transmute(
    code_station    = as.character(code_station),
    annee           = year(as.Date(date_prelevement)),
    code_indice     = 1022,
    libelle_indice  = "IPS",
    resultat_indice = as.numeric(resultat_indice),
    eqr_indice      = NA_real_,
    classe_indice   = NA_character_,
    code_support,
    libelle_support ) %>%
  distinct()
# Ajouter dans etat_bio
etat_bio <- bind_rows(etat_bio, ips_info) %>%
  distinct() %>%
  arrange(code_station, annee, code_indice)

#### Table pour les metriques ####
metriques <- indices %>%
  filter(code_indice %in% c(8058, 8056, 8057, 8054, 8050)) # On filtre pour avoir que les metriques de l'I2M2

#### Table occupation du sol de la station issue de QGIS####
# Pour 2018
occupation_2018 <- read.csv("occupation_2018.csv", stringsAsFactors = FALSE)
# Ayant deja réaliser sur QGIS ma table avec un buffer, une interserction avec CLC et des calculs dans la table attributaire et l'extraction de cette table
# j'ai juste a importer la table ici
occupation_2018 <- occupation_2018 %>%
  mutate(code_station = stringr::str_pad(as.character(code_station), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2012
occupation_2012 <- read.csv("occupation_2012.csv", stringsAsFactors = FALSE)
occupation_2012 <- occupation_2012 %>%
  mutate(code_station = stringr::str_pad(as.character(code_station), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regoupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2006
occupation_2006 <- read.csv("occupation_2006.csv", stringsAsFactors = FALSE)
occupation_2006 <- occupation_2006 %>%
  mutate(code_station = stringr::str_pad(as.character(code_station), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2000
occupation_2000 <- read.csv("occupation_2000.csv", stringsAsFactors = FALSE)
occupation_2000 <- occupation_2000 %>%
  mutate(code_station = stringr::str_pad(as.character(code_station), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe.regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 1990
occupation_1990 <- read.csv("occupation_90.csv", stringsAsFactors = FALSE)
occupation_1990 <- occupation_1990 %>%
  mutate(code_station = stringr::str_pad(as.character(code_station), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

#### Table occupation du sol du BV #####
# Pour 2018
occupation_BV_2018 <- read.csv("occupation_BV_2018.csv", stringsAsFactors = FALSE)
occupation_BV_2018 <- occupation_BV_2018 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2012
occupation_BV_2012 <- read.csv("occupation_BV_2012.csv", stringsAsFactors = FALSE)
occupation_BV_2012 <- occupation_BV_2012 %>%
  mutate( CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2006
occupation_BV_2006 <- read.csv("occupation_BV_2006.csv", stringsAsFactors = FALSE)
occupation_BV_2006 <- occupation_BV_2006 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2000
occupation_BV_2000 <- read.csv("occupation_BV_2000.csv", stringsAsFactors = FALSE)
occupation_BV_2000 <- occupation_BV_2000 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 1990
occupation_BV_1990 <- read.csv("occupation_BV_1990.csv", stringsAsFactors = FALSE)
occupation_BV_1990 <- occupation_BV_1990 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

#### Table occupation BV en details#####
# Pour 2018
occupation_BV_details_2018 <- read.csv("occupation_BV_details_2018.csv", stringsAsFactors = FALSE)
occupation_BV_details_2018 <- occupation_BV_details_2018 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2012
occupation_BV_details_2012 <- read.csv("occupation_BV_details_2012.csv", stringsAsFactors = FALSE)
occupation_BV_details_2012 <- occupation_BV_details_2012 %>%
  mutate( CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2006
occupation_BV_details_2006 <- read.csv("occupation_BV_details_2006.csv", stringsAsFactors = FALSE)
occupation_BV_details_2006 <- occupation_BV_details_2006 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 2000
occupation_BV_details_2000 <- read.csv("occupation_BV_details_2000.csv", stringsAsFactors = FALSE)
occupation_BV_details_2000 <- occupation_BV_details_2000 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)

# Pour 1990
occupation_BV_details_1990 <- read.csv("occupation_BV_details_1990.csv", stringsAsFactors = FALSE)
occupation_BV_details_1990 <- occupation_BV_details_1990 %>%
  mutate(CdOH = stringr::str_pad(as.character(CdOH), width = 8, side = "left", pad = "0")) %>%
  mutate(pourcentage = as.numeric(pourcentage)) %>%
  pivot_wider(
    names_from = classe_regroupee,
    values_from = pourcentage,
    values_fill = 0)
#### Table pour les cartes#######
# Table donnee_carte: Résumé taxons par station
resume_taxons_station <- taxons %>%
  mutate(
    date_prelevement = as.Date(date_prelevement),
    annee = lubridate::year(date_prelevement) ) %>%
  group_by(code_station) %>%
  summarise(
    dernier_annee = max(annee, na.rm = TRUE),
    nombre_annee = n_distinct(annee),
    .groups = "drop")

# Dernier indice disponible par station dans etat_bio
dernier_indice_station <- etat_bio %>%
  mutate(annee = as.integer(annee)) %>%
  arrange(code_station, desc(annee)) %>%
  group_by(code_station) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    code_station,
    code_support_taxon = code_support,
    libelle_support = libelle_support,
    code_dernier_indice = code_indice,
    libelle_dernier_indice = libelle_indice,
    dernier_resultat = resultat_indice)

# Creation table final
donnee_carte <- stations %>%
  left_join(resume_taxons_station, by = "code_station") %>%
  left_join(dernier_indice_station, by = "code_station") %>%
  st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154, remove = FALSE) %>% # Creation en object spatial
  select(
    code_station,
    libelle_station,
    uri_station,
    coordonnee_x,
    coordonnee_y,
    code_cours_eau,
    libelle_cours_eau, # Selection des données
    code_masse_eau,
    libelle_masse_eau,
    code_dep,
    UH_calculee,
    date_premier_prelevement,
    date_dernier_prelevement,
    geometry,
    typologie,
    code_support_taxon,
    libelle_support,
    dernier_annee,
    nombre_annee,
    code_dernier_indice,
    libelle_dernier_indice,
    dernier_resultat)

# Table donnee_carte_taxon##
# Table donnee_carte_taxon##
donnee_carte_taxon <- taxons %>%
  mutate(
    date_prelevement = as.Date(date_prelevement), # Pour les dates
    annee = year(date_prelevement) ) %>%
  group_by(
    code_station,
    libelle_station, # Regroupement
    code_support,
    libelle_taxon ) %>%
  summarise(
    abondance_moyenne = mean(resultat_taxon, na.rm = TRUE),
    annee_min = min(annee, na.rm = TRUE),
    annee_max = max(annee, na.rm = TRUE), # Calcul de l'abondance, année min et max
    .groups = "drop" ) %>%
  mutate(
    eqb = dplyr::case_when(
      code_support == "10" ~ "Diatomées",
      code_support == "13" ~ "Macroinvertébrés",
      code_support == "27" ~ "Macrophytes",
      code_support == "4"  ~ "Poissons",
      TRUE ~ NA_character_ ), # Associer chaque code_support à son EQB pour le filtre dans l'application
    abondance_affichee = sub("\\.?0+$", "", sprintf("%.3f", abondance_moyenne)),
    resume = paste0(
      "abondance: ",
      abondance_affichee,
      " (",
      annee_min,
      "-",
      annee_max,
      ")"  ), # Permet de bien afficher le resume comme dans IDF
    hover = paste0(
      "<b>", libelle_taxon, "</b><br>",
      "<em>", libelle_station, "</em><br><br>", # Permet de bien afficher comme dans IDF
      resume ) ) %>%
  left_join(
    stations %>%
      st_drop_geometry() %>%
      select(code_station, code_dep, coordonnee_x, coordonnee_y), # Recuperer les données utiles
    by = "code_station" ) %>%
  filter(!is.na(coordonnee_x), !is.na(coordonnee_y)) %>% # Retirer les lignes sans coordonnées
  st_as_sf(coords = c("coordonnee_x", "coordonnee_y"), crs = 2154, remove = FALSE) %>% # Transformation en objet spatial
  select(
    code_dep,
    code_station,
    geometry,
    libelle_station,
    code_support,
    eqb,
    libelle_taxon,
    abondance_moyenne, # Selection finale
    resume,
    hover )

#### Table pour le diag####
#Construction table entree_miv_seee
taxons_inv <- taxons %>%
  filter(code_support == "13")  # Prendre que les MIV

entree_inv <- taxons_inv %>%
  left_join(
    stations %>% select(code_station, typologie),# Il faut la typologie
    by = "code_station") %>%
  transmute(
    CODE_OPERATION = code_prelevement, # Colonne obliagtoire pour le SEEE
    CODE_STATION   = code_station,
    DATE           = format(as.Date(date_prelevement), "%d/%m/%Y"),
    TYPO_NATIONALE = typologie,
    CODE_PHASE     = code_phase,
    CODE_TAXON     = as.integer(code_appel_taxon),
    RESULTAT       = as.integer(resultat_taxon),
    CODE_REMARQUE  = code_remarque)

# Table entree_dia
tc_diat <- readxl::read_excel("TCv1.3_DIAT.xlsx")
taxons_diat <- taxons %>%
  filter(code_support == "10")  # Prendre que les DIA

# Lecture des 2 fichiers exportés du Sandre
export_diat_a <- read_csv2(
  "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/données/export_1775210580.csv")
export_diat_b <- read_csv2(
  "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/données/export_diat1.csv")

# Attention une table contient tout les compartiements
# Filtrer chaque table sur CdThemeTaxon = 5 puis les fusionner
export_diat <- bind_rows(
  export_diat_a %>% filter(as.character(CdThemeTaxon) == "5"),
  export_diat_b %>% filter(as.character(CdThemeTaxon) == "5")) %>%
  distinct()

#Vérifier que les colonnes CdAlternatif1 à CdAlternatif5 existent
colonnes_alt <- paste0("CdAlternatif", 1:5)
for (col in colonnes_alt) {
  if (!col %in% names(export_diat)) {
    export_diat[[col]] <- NA_character_ } }

# Nettoyer les colonnes utiles de export_diat
export_diat <- export_diat %>%
  mutate(
    CdAppelTaxon  = trimws(as.character(CdAppelTaxon)),
    CdAlternatif1 = toupper(trimws(as.character(CdAlternatif1))),
    CdAlternatif2 = toupper(trimws(as.character(CdAlternatif2))),
    CdAlternatif3 = toupper(trimws(as.character(CdAlternatif3))),
    CdAlternatif4 = toupper(trimws(as.character(CdAlternatif4))),
    CdAlternatif5 = toupper(trimws(as.character(CdAlternatif5))) )


# Fonction pour vérifier si un code =  4 lettres
est_code_valide <- function(x) {
  x <- trimws(as.character(x))
  if (length(x) == 0 || is.na(x) || x == "") {
    return(FALSE)}
  str_detect(x, "^[A-Z]{4}$") }

# Construire la table de correspondance
# Pour chaque CdAppelTaxon, on garde la liste des codes valides dans l'ordre
correspondance_taxon <- export_diat %>%
  rowwise() %>%
  mutate(
    codes_valides = list(c(
      if (est_code_valide(CdAlternatif1)) CdAlternatif1 else NULL,
      if (est_code_valide(CdAlternatif2)) CdAlternatif2 else NULL,
      if (est_code_valide(CdAlternatif3)) CdAlternatif3 else NULL,
      if (est_code_valide(CdAlternatif4)) CdAlternatif4 else NULL,
      if (est_code_valide(CdAlternatif5)) CdAlternatif5 else NULL )) ) %>%
  ungroup() %>%
  distinct(CdAppelTaxon, .keep_all = TRUE) %>%
  select(CdAppelTaxon, codes_valides)


# Préparer la table taxons_diat
# On crée :
# - n = nombre d'occurrences d'un taxon dans une opération
# - rang_doublon = rang de la ligne dans ce groupe
taxons_diat_prep <- taxons_diat %>%
  mutate(
    code_appel_taxon = trimws(as.character(code_appel_taxon)),
    code_prelevement = as.character(code_prelevement),
    code_station     = as.character(code_station) ) %>%
  group_by(code_prelevement, code_appel_taxon) %>%
  mutate(
    n = n(),
    rang_doublon = row_number() ) %>%
  ungroup()

# Lecture du fichier contenant les corrections manuelles
na_uniques <- readxl::read_excel("NA_uniques.xlsx") %>%
  mutate(
    CODE_OPERATION   = as.character(CODE_OPERATION),
    CODE_STATION     = as.character(CODE_STATION),
    DATE             = as.character(DATE),
    code_appel_taxon = trimws(as.character(code_appel_taxon)),
    RESULTAT         = as.integer(RESULTAT),
    n                = as.integer(n),
    Nouveau_code     = as.character(Nouveau_code) )

# Créer entree_diat
# Si un taxon apparaît plusieurs fois dans une même opération,
# on attribue les codes valides selon le rang :
# 1re ligne = 1er code valide
# 2e ligne = 2e code valide
# Puis si un Nouveau_code existe dans NA_uniques.xlsx, il remplace CODE_TAXON
entree_diat <- taxons_diat_prep %>%
  left_join(
    correspondance_taxon,
    by = c("code_appel_taxon" = "CdAppelTaxon")) %>%
  rowwise() %>%
  mutate(
    CODE_TAXON = if (
      !is.null(codes_valides) &&
      length(codes_valides) >= rang_doublon) {
      as.character(codes_valides[[rang_doublon]])
    } else {  NA_character_ }) %>%
  ungroup() %>%
  transmute(
    CODE_OPERATION   = code_prelevement,
    CODE_STATION     = code_station,
    DATE             = format(as.Date(date_prelevement), "%d/%m/%Y"),
    code_appel_taxon = code_appel_taxon,
    CODE_TAXON       = CODE_TAXON,
    RESULTAT         = as.integer(resultat_taxon),
    n                = n ) %>%
  left_join(
    na_uniques %>%
      select(CODE_OPERATION, CODE_STATION, DATE, code_appel_taxon, RESULTAT, n, Nouveau_code),
    by = c("CODE_OPERATION", "CODE_STATION", "DATE", "code_appel_taxon", "RESULTAT", "n") ) %>%
  mutate( CODE_TAXON = dplyr::coalesce(Nouveau_code, CODE_TAXON) ) %>%
  select(
    CODE_OPERATION,
    CODE_STATION,
    DATE,
    code_appel_taxon,
    CODE_TAXON,
    RESULTAT )



#### Table valeur_seuil_taxon ####
# Récupération de la liste des fichiers de paramètres des indices dans le dossier des algorithmes SEEE version 2018.
# Full.names = TRUE permet d'obtenir le chemin complet des fichiers.
fichiers_parametres <- list.files(
  path = "algo_seee/EBio_CE_2018/1.0.1",
  pattern = "params",
  full.names = TRUE)

# Ajout du fichier IBG-DCE de la méthode 2015, meme fonctionnement que etat_bio (cas exceptionnel)
fichiers_parametres <- c( fichiers_parametres[!stringr::str_detect(fichiers_parametres, "IBG-DCE")],
  "algo_seee/EBio_CE_2015/1.0.1/EBio_CE_2015_params_IBG-DCE.csv")

# Noms d'indices à partir des noms de fichiers
noms_indices_param <- fichiers_parametres |>
  stringr::str_remove("algo_seee/EBio_CE_201\\d/1.0.1/EBio_CE_201\\d_params_") |>
  stringr::str_remove("\\.csv")

# Import des seuils des différents indices
# Decimal_mark = "," pour bien lire les valeurs numériques
valeurs_seuils <- fichiers_parametres |>
  purrr::map(~ vroom::vroom(.x, locale = vroom::locale(decimal_mark = ","))) |>
  purrr::set_names(noms_indices_param)

# Mise au même format des seuils
for (x in noms_indices_param) {

  if (x == "IPR") { # cas particulier pour IPR: les seuils sont utilisés comme bornes supérieures
    valeurs_seuils[[x]] <- valeurs_seuils[[x]] |>
      dplyr::mutate(
        BON = ifelse(is.na(BON), BASSE_ALTITUDE, BON) #manque la classe BON
        ) |>
      dplyr::select(TYPO_NATIONALE, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
      tidyr::pivot_longer( #format large à long
        cols = -TYPO_NATIONALE,
        names_to = "classe",
        values_to = "seuil_haut" ) |>
      dplyr::mutate(
        seuil_bas = ifelse(classe == "TRES_BON", 0, dplyr::lag(seuil_haut)))

    } else if (x == "IBD") { #attention pour IBD, on prend BV_TG
    valeurs_seuils[[x]] <- valeurs_seuils[[x]] |>
      dplyr::select(TYPO_NATIONALE, TG_BV, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
      tidyr::pivot_longer(
        cols = -c(TYPO_NATIONALE, TG_BV),
        names_to = "classe",
        values_to = "seuil_bas" ) |>
      dplyr::mutate(
        seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas))) #tres bon = 1(borne superieur)

     } else { #cas pour les autres indices
    valeurs_seuils[[x]] <- valeurs_seuils[[x]] |>
      dplyr::select(TYPO_NATIONALE, TRES_BON, BON, MOYEN, MEDIOCRE, MAUVAIS) |>
      tidyr::pivot_longer(
        cols = -TYPO_NATIONALE,
        names_to = "classe",
        values_to = "seuil_bas" ) |>
      dplyr::mutate(
        seuil_haut = ifelse(classe == "TRES_BON", 1, dplyr::lag(seuil_bas)))}}

# Construction de la table finale des seuils par station.
# On part de la table des stations SEEE en gardant une seule ligne par station, typologie et TG_BV.
valeur_seuil_taxon <- stations_seee |>
  dplyr::distinct(CODE_STATION, TYPO_NATIONALE, TG_BV) |>
  dplyr::rename(
    code_station = CODE_STATION,
    typologie = TYPO_NATIONALE ) |>
  dplyr::mutate(TG_BV = as.character(TG_BV)) |>

   (\(df) {
    dplyr::bind_rows(
      # IBD : jointure sur typologie + TG_BV
      df |>
        dplyr::mutate(indice = "IBD") |>
        dplyr::left_join(
          valeurs_seuils$IBD,
          by = c("typologie" = "TYPO_NATIONALE", "TG_BV" = "TG_BV")),

      # autres indices : jointure seulement sur typologie
      df |>
        dplyr::mutate(indice = "IBMR") |>
        dplyr::left_join(
          valeurs_seuils$IBMR,
          by = c("typologie" = "TYPO_NATIONALE")),
      df |>
        dplyr::mutate(indice = "IPR") |>
        dplyr::left_join(
          valeurs_seuils$IPR,
          by = c("typologie" = "TYPO_NATIONALE")),
      df |>
        dplyr::mutate(indice = "IBG-DCE") |>
        dplyr::left_join(
          valeurs_seuils$`IBG-DCE`,
          by = c("typologie" = "TYPO_NATIONALE")),
      df |>
        dplyr::mutate(indice = "I2M2") |>
        dplyr::left_join(
          valeurs_seuils$I2M2,
          by = c("typologie" = "TYPO_NATIONALE")),
      )
  })() |>
  dplyr::mutate(
    seuil_haut = ifelse(is.na(seuil_bas), NA, seuil_haut)) |>
  dplyr::select(
    code_station,
    typologie,
    TG_BV,
    indice,
    classe,
    seuil_haut,
    seuil_bas ) |> dplyr::arrange(code_station, indice, classe)

#### Table resume pour les taxons ####
resume_liste <- taxons %>%
  mutate( # Prend les données dans taxons, nettoie la date, les noms etc
    date_prelevement = as.Date(date_prelevement),
    annee = lubridate::year(date_prelevement),
    libelle_taxon = stringr::str_squish(libelle_taxon) ) %>%
  filter(!is.na(libelle_taxon), libelle_taxon != "") %>% # Filtre les taxons presents (evite les NA)
  distinct(code_station, code_support, libelle_support, annee, libelle_taxon) %>% # Supprime les doublons
  group_by(code_station, code_support, libelle_support, annee) %>%
  summarise( # Nb de taxon par année/support/station
    nb_taxons = n(),
    .groups = "drop" ) %>%
  group_by(code_station, code_support, libelle_support) %>% # Par station/support
  summarise( # Création finale
    periode = dplyr::if_else(
      min(annee, na.rm = TRUE) == max(annee, na.rm = TRUE),# Au niveau des années
      as.character(max(annee, na.rm = TRUE)),
      paste0(min(annee, na.rm = TRUE), "-", max(annee, na.rm = TRUE) ) ),
    annee = max(annee, na.rm = TRUE), # Année la plus recente
    taxon = dplyr::if_else( # Intervalle taxon
      min(nb_taxons, na.rm = TRUE) == max(nb_taxons, na.rm = TRUE),
      as.character(min(nb_taxons, na.rm = TRUE)),
      paste0(min(nb_taxons, na.rm = TRUE), "-", max(nb_taxons, na.rm = TRUE) ) ),
    .groups = "drop")

#### Définition des acronymes des indices#####
# Les codes des indices =  acronymes pour une meilleure lisibilité
acronymes_indices <- c(
  "1022" = "IPS",
  "5856" = "IBD",
  "2928" = "IBMR",
  "7613" = "I2M2",
  "5910" = "IBG équivalent",
  "7036" = "IPR",
  "8058" = "Shannon",
  "8057" = "ASPT",
  "8056" = "Polyvoltinisme",
  "8054" = "Richesse Taxonomique",
  "8050" = "Nombre de taxons contributifs" )


#### Enregister en rda maintenant####
date_donnees <- Sys.Date() # Date de mise à jour des données
save( stations,
      taxons,
      indices,
      etat_bio,
      date_donnees,
      indices_etat_bio,
      acronymes_indices,
      valeur_seuil_taxon,
      resume_liste,
      entree_inv,
      entree_diat,
      metriques,
      occupation_2018,
      occupation_2012,
      occupation_2006,
      occupation_2000,
      occupation_1990,
      occupation_BV_1990,
      occupation_BV_2000,
      occupation_BV_2006,
      occupation_BV_2012,
      occupation_BV_2018,
      occupation_BV_details_1990,
      occupation_BV_details_2000,
      occupation_BV_details_2006,
      occupation_BV_details_2012,
      occupation_BV_details_2018,
      donnee_carte,
      donnee_carte_taxon,
  file = "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/dev/data_hydrobioNOR.rda")
