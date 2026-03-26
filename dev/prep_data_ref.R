# Departement chemin : "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BDCARTO\BDC_4-0_SHP_LAMB93_R28-ED211\ADMINISTRATIF\DEPARTEMENT.shp"
#region chemin: "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BDCARTO\BDC_4-0_SHP_LAMB93_R28-ED211\ADMINISTRATIF\REGION.shp"
#bassin hydro : "\\ad.intra\dfs\COMMUNS\REGIONS\nor\DR\OFB\SIG\DR\REFERENTIEL\BD TOPAGE\NOR_BassinHydrographique_FXX.gpkg"

#mettre les limites de region de NOR
limites_region <-  sf::st_read("//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BDCARTO/BDC_4-0_SHP_LAMB93_R28-ED211/ADMINISTRATIF/DEPARTEMENT.shp") %>%
  dplyr::filter(INSEE_REG == "28") %>% #filtre de normandie
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify() #pour simplifier

limites_region_l <- limites_region %>%
  sf::st_cast(to = "LINESTRING") #trace les contours comme IDF

##Afin d'avoir le nom de la couche
sf::st_layers("//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BD TOPAGE/NOR_BassinHydrographique_FXX.gpkg")
#faire les limites des bassins, meme fonctionnement que region
limites_bassin <- sf::st_read(
  dsn = "//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BD TOPAGE/NOR_BassinHydrographique_FXX.gpkg",
  layer = "NOR_BassinHydrographique_FXX"
) %>%
  dplyr::filter(LbBH == "Seine-Normandie") %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

limites_bassin_l <- limites_bassin %>%
  sf::st_cast(to = "LINESTRING")

masque_metropole <- sf::st_read("//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BDCARTO/BDC_4-0_SHP_LAMB93_R28-ED211/ADMINISTRATIF/REGION.shp") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_difference(limites_bassin) %>%
  dplyr::filter(INSEE_REG != "28") %>%
  dplyr::summarise() %>%
  rmapshaper::ms_simplify()

usethis::use_data(
  limites_region, limites_region_l,
  limites_bassin, limites_bassin_l,
  masque_metropole,
  internal = TRUE, overwrite = TRUE
)
