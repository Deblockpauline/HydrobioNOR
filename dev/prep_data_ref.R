# Departement chemin : "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BDCARTO\BDC_4-0_SHP_LAMB93_R28-ED211\ADMINISTRATIF\DEPARTEMENT.shp"
#region chemin: "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BDCARTO\BDC_4-0_SHP_LAMB93_R28-ED211\ADMINISTRATIF\REGION.shp"
#bassin hydro : "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BD TOPAGE\NOR_BassinHydrographique_FXX.gpkg"
# Bassins versants : "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/données/couche BV/BV_NOR.shp"
#mettre les limites de region de NOR
limites_region <-  sf::st_read("//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BDCARTO/BDC_4-0_SHP_LAMB93_R28-ED211/ADMINISTRATIF/DEPARTEMENT.shp") %>%
  dplyr::filter(INSEE_REG == "28") %>% #filtre de normandie
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify() #pour simplifier

limites_region_l <- limites_region %>%
  sf::st_cast(to = "LINESTRING") #trace les contours comme IDF


# Faire les limites des bassins, meme fonctionnement que region
limites_cours_eau <- sf::st_read(
  dsn ="//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BD TOPAGE/NOR_CoursEau_FXX.gpkg") %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

#### Bassins versants ####
limites_bv <- sf::st_read(
  "C:/Users/pauline.deblock/Documents/stage Pauline/R/hydrobioNOR/données/couche BV/BV_NOR.shp",
  quiet = TRUE) %>%
  sf::st_make_valid() %>%                # Sécurise les géométries si besoin
  sf::st_transform(crs = 4326) %>%       # Projection compatible leaflet
  rmapshaper::ms_simplify(keep = 0.08)   # Allège un peu la couche

# On garde uniquement les contours des bassins versants pour ne pas surcharger la carte
limites_bv_l <- limites_bv %>%
  sf::st_cast("MULTILINESTRING")


#### Enregistrement dans les données internes du package ####
usethis::use_data(
  limites_region,
  limites_region_l,
  limites_cours_eau,
  limites_bv,
  limites_bv_l,
  internal = TRUE,
  overwrite = TRUE)
