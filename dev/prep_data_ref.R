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


# Faire les limites des bassins, meme fonctionnement que region
limites_cours_eau <- sf::st_read(
  dsn ="//ad.intra/dfs/COMMUNS/REGIONS/nor/DR/OFB/SIG/DR/REFERENTIEL/BD TOPAGE/NOR_CoursEau_FXX.gpkg"
) %>%
  sf::st_transform(crs = 4326) %>%
  rmapshaper::ms_simplify()

usethis::use_data(
  limites_region, limites_region_l,
  limites_cours_eau,
  internal = TRUE, overwrite = TRUE)
