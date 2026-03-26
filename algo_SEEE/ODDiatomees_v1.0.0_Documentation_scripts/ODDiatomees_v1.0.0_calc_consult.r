# Type d'algorithme : ODDiatomees
# Auteur(s)         : Benjamin Alric, Floriane Larras, Romain Coulaud, Philippe Usseglio-Polatera (LIEC), Integration : UDAM
# Date              : 2025-02-13 2023-03-03
# Version           : 1.0.0 0.9.2.5
# Interpreteur	   	: R version 4.1.1 (2021-08-10) R version 4.1.1 (2021-08-10)
# Pre-requis        : Packages dplyr, tidyr, ade4, purrr, tibble, ranger, mlr, stringr, ggplot2 Packages dplyr, tidyr, ade4, purrr, tibble, ranger, mlr, stringr, ggplot2
# Fichiers lies   	: ODDiatomees_params_base_trait_totr.csv, ODDiatomees_params_base_trait_totrNA.csv, ODDiatomees_params_correspondance3.csv, ODDiatomees_params_index_ibd.csv, ODDiatomees_params_indices_diato.csv, ODDiatomees_params_tax_a_garder.csv, ODDiatomees_params_traits_modalites.csv ODDiatomees_params_base_trait_totr.csv, ODDiatomees_params_correspondance3.csv, ODDiatomees_params_index_ibd.csv, ODDiatomees_params_indices_diato.csv, ODDiatomees_params_tax_a_garder.csv, ODDiatomees_params_traits_modalites.csv
# Commentaires 	  	: Version mlr 2.19.0, data.table 1.14.2, ranger 0.13.0 obligatoires (ou pas...!)

# Copyright 2023 Floriane Larras, Romain Coulaud, Philippe Usseglio-Polatera (LIEC)
# Ce programme est un logiciel libre; vous pouvez le redistribuer ou le modifier
# suivant les termes de la GNU General Public License telle que publiee par la
# Free Software Foundation; soit la version 3 de la licence, soit (a votre gre)
# toute version ulterieure.
# Ce programme est distribue dans l'espoir qu'il sera utile, mais SANS AUCUNE
# GARANTIE; sans meme la garantie tacite de QUALITE MARCHANDE ou d'ADEQUATION A
# UN BUT PARTICULIER. Consultez la GNU General Public License pour plus de
# details.
# Vous devez avoir recu une copie de la GNU General Public License en meme temps
# que ce programme; si ce n'est pas le cas, consultez
# <http://www.gnu.org/licenses>.

## VERSION ----
indic  <- "ODDiatomees"
vIndic <- "v1.0.0"

## CHARGEMENT DES PACKAGES ----
dependencies <- c("dplyr","tidyr","ade4","purrr","tibble","ranger", "mlr","stringr","ggplot2") 

loadDependencies <- function(dependencies) {
  suppressAll <- function(expr) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  }

  lapply(dependencies,
         function(x)
         {
           suppressAll(library(x, character.only = TRUE))
         }
  )
  invisible()
}

loadDependencies(dependencies)

## IMPORT DES FICHIERS DE CONFIGURATION ----

base_trait <- read.csv2("ODDiatomees_params_base_trait_totr.csv", 
                        header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
                        colClasses = c(CODE_TAXON   = "character",
                                       TYPE = "character",
                                       TAXO = "character",
                                       REFUNI = "character",
                                       FAM = "character",
                                       GENRE = "character",
                                       TERATO = "character",
                                       TAX_IBD = "character")) # reste numeric

tax_a_garder <- read.csv2("ODDiatomees_params_tax_a_garder.csv", encoding = "UTF-8",
                          header = TRUE, stringsAsFactors = FALSE,
                          colClasses = "character")

indices_diato <- read.csv2("ODDiatomees_params_indices_diato.csv", encoding = "UTF-8",
                           header = TRUE, stringsAsFactors = FALSE,
                           colClasses = c(CODE_TAXON = "character")) # reste numeric

index_ibd <- read.csv2("ODDiatomees_params_index_ibd.csv", encoding = "UTF-8",
                       header = TRUE, stringsAsFactors = FALSE,
                       colClasses = c(AFNOR = "character")) # reste numeric

Traits1_S3E2 <- read.csv2("ODDiatomees_params_traits_modalites.csv", encoding = "UTF-8",
                          header = TRUE, stringsAsFactors = FALSE,
                          colClasses = "character") 

correspondanceNomColonnes <- read.csv2("ODDiatomees_params_correspondance3.csv", encoding = "UTF-8",
                                       header = TRUE, stringsAsFactors = FALSE,
                                       colClasses = "character") 

## DECLARATION DES FONCTIONS ----

## Modification de la fonction divc de ade4
divc <- function (df, dis = NULL, scale = FALSE) {
  if (!inherits(df, "data.frame")) 
    stop("Non convenient df")
  if (any(df < 0)) 
    stop("Negative value in df")
  if (!is.null(dis)) {
    if (!inherits(dis, "dist")) 
      stop("Object of class 'dist' expected for distance")
    # if (!is.euclid(dis)) # Modif. Albin
    #   warning("Euclidean property is expected for distance") # Modif. Albin
    dis <- as.matrix(dis)
    if (nrow(df) != nrow(dis)) 
      stop("Non convenient df")
    dis <- as.dist(dis)
  }
  if (is.null(dis)) dis <- as.dist((matrix(1, nrow(df), nrow(df)) - diag(rep(1, nrow(df)))) * sqrt(2))
  div <- as.data.frame(rep(0, ncol(df)))
  names(div) <- "diversity"
  rownames(div) <- names(df)
  for (i in 1:ncol(df)) {
    if (sum(df[, i]) < 1e-16) 
      div[i, ] <- 0
    else div[i, ] <- (t(df[, i]) %*% (as.matrix(dis)^2) %*% 
                        df[, i])/2/(sum(df[, i])^2)
  }
  if (scale == TRUE) {
    divmax <- divcmax(dis)$value
    div <- div/divmax
  }
  return(div)
}
#' Title
#'
#' @param base_trait : tableau des traits
#' @param mat_flo_ref : matrice floristique
#' @param tab_trait_mod : tableau des traits en fonction des modalités
#'
#' @return
#' @export
#'
#' @examples
fun_entropie_rao <- function(base_trait, mat_flo_ref, tab_trait_mod) {
  
  profils_taxo_S3E2 <- base_trait %>% 
    filter(CODE_TAXON %in% rownames(mat_flo_ref)) %>%
    select(1,9:69) 
  
  distGroupsTrait_S3E2 <- profils_taxo_S3E2 %>%
    ## pourquoi ???!!!!!!!!!!
    arrange(CODE_TAXON) %>%
    select(order(colnames(.))) %>%
    tibble::column_to_rownames("CODE_TAXON") %>%
    # mise au bon format permettre l'ajout de l'info sur les MODALITE et pour le nest
    t() %>% 
    data.frame() %>%
    tibble::rownames_to_column("MODALITE") %>%
    left_join(tab_trait_mod, by = c("MODALITE"="MODALITE")) %>%
    group_by(TRAIT) %>% # grouper par trait
    nest()
  
  ## Fonction nécessaire uniquement au map si dessous.
  RAO_map <- function(tab_zz){
    # mise au bon format pour la suite
    temp <- tab_zz %>% 
      tibble::column_to_rownames("MODALITE") %>% 
      t() %>% 
      as.data.frame()
    # calcul de la distance euclidienne entre les trait
    raoDis <- dist(temp, method = "euclidean")
    # calcul de la diversite de Rao des echantillons
    Rao <- divc(mat_flo_ref, raoDis)
    return(Rao)
  }
  appelMapRao_S3E2 <- map(.x = distGroupsTrait_S3E2$data, .f = ~RAO_map(tab_zz = .))
  names(appelMapRao_S3E2) <- paste0("Rao_",distGroupsTrait_S3E2$TRAIT)
  
  tabRao_S3E2 <- do.call(cbind,appelMapRao_S3E2)
  names(tabRao_S3E2) <- paste0("Rao_",distGroupsTrait_S3E2$TRAIT)
  
  return(tabRao_S3E2)
}

##-------------------------------------------------------------------------------------------------------##
#' Title
#'
#' @param tab_flo_ref_taxo_long : table floristique de reférence en format long
#' @param var : la variable a considerer
#'
#' @return
#' @export
#'
#' @examples
fun_richesseRelative <- function(tab_flo_ref_taxo_long, var) {
  nomCol <- rlang::sym(paste0("NBSP",var)) # creation du nom de la colonne nbSp"Var"
  nomCol2 <- rlang::sym(paste0("NBSP",var,"_RELAT")) # creation du nom de la colonne nbSp"Var"Relat
  var <- rlang::sym(var)
  tab_richRel_long <- tab_flo_ref_taxo_long %>%
    select(CODE_OPERATION,REFUNI,!!var) %>% # retrait de la colonne
    group_by(CODE_OPERATION, !!var) %>%  
    mutate(!!nomCol := n()) %>% # nombre d'sp avec le meme genre
    select(-REFUNI) %>% # retrait de la colonne
    ungroup() %>%
    distinct() %>%
    group_by(CODE_OPERATION) %>% # grouper par code_operation
    mutate(NBTOT = sum(!!nomCol)) %>% # 
    mutate(!!nomCol2 := (!!nomCol / NBTOT) * 100) %>% ## ??!
    ungroup() %>%
    distinct()
  return(tab_richRel_long)
}

##-------------------------------------------------------------------------------------------------------##
#' Title
#'
#' @param tab_flo_ref_taxo_long : table floristique de reférence en format long
#' @param valeurs_indic_diato : Valeurs des indicateurs sur les diatomées
#'
#' @return
#' @export
#'
#' @examples
fun_saproTrophieWana <- function(tab_flo_ref_taxo_long, valeurs_indic_diato){
  tab_indic <- tab_flo_ref_taxo_long %>%
    merge(., valeurs_indic_diato, 
          by.x = "REFUNI", by, 
          by.y = "CODE_TAXON") %>% # selection des taxons directement avec le merge
    group_by(CODE_OPERATION) %>% # par releve
    mutate(IPS = sum(RESULTAT * IPS_S * IPS_V) / sum(RESULTAT * IPS_V)) %>%
    mutate(DESCY = sum(RESULTAT * DESCY_S * DESCY_V) / sum(RESULTAT * DESCY_V))%>%
    mutate(SLADECEK = sum(RESULTAT * SLADECEK_S * SLADECEK_V) / sum(RESULTAT * SLADECEK_V))%>%
    mutate(ROTT = sum(RESULTAT * ROTT_SAPRO_S * ROTT_SAPRO_V) / sum(RESULTAT * ROTT_SAPRO_V)) %>%
    mutate(KELLY = sum(RESULTAT * KELLY_S * KELLY_V) / sum(RESULTAT * KELLY_V)) %>%
    ## 05.7.5. Calcul de l'indice de watanabe
    mutate(WatabeTmp = ((sum(RESULTAT[WATA_SAPRO == 2]) * 100) / sum(RESULTAT)) -
             ((sum(RESULTAT[WATA_SAPRO == 1]) * 100) / sum(RESULTAT))) %>%
    mutate(WATANABE = 50 + 0.5 * WatabeTmp) %>%
    select(CODE_OPERATION,IPS,DESCY,SLADECEK,ROTT,KELLY,WATANABE) %>%
    distinct() %>%
    ungroup()
  
  return(tab_indic)
}

##-------------------------------------------------------------------------------------------------------##
## fonctions extrait du calcul de l'IBD2007 du S3E (version ...)
#' funAx : Fait partit du calcul de l'IBD
#'
#' @param data_entree : matrice spécifique pour le calcul des abondances
#'
#' @return
#' @export
#'
#' @examples funAx(data_entree = tab_cont_ref_S3E_calcul_IBD)
funAx <- function(data_entree) {
  group_by(data_entree, CODE_OPERATION, CODE_TAXON) %>% ## regroupement des taxons avec codes identiques
    summarise(RESULTAT = sum(RESULTAT))           %>% ## somme des abondances
    group_by(CODE_OPERATION)                      %>% 
    mutate(TOTAL = sum(RESULTAT))                 %>% ## abondance total de l'operation
    ungroup()                                     %>% 
    mutate(Ax = 1000 * RESULTAT / TOTAL)         
}

##-------------------------------------------------------------------------------------------------------##
#' funFi: Fait partit du calcul de l'IBD
#'
#' @param table : matrice spécifique
#' @param param : selon quelle variable est réalisé le calcul des valeur de Fi
#'
#' @return
#' @export
#'
#' @examples funFi(table = ., param = classes_IBD)
funFi <- function(table, param) {
  left_join(x = table, y = param, 
            by = c("CODE_TAXON" = "AFNOR")) %>% 
    select(-RESULTAT,-TOTAL) %>%  # - SANDRE
    gather(key = CLASSE, value = Px, 
           -CODE_OPERATION, -CODE_TAXON, -Ax, -Val.Ind.) %>% 
    group_by(CODE_OPERATION, CLASSE) %>% 
    summarise(Fi = sum(Ax * Px * Val.Ind.) / sum(Ax * Val.Ind.))
}

##-------------------------------------------------------------------------------------------------------##
#' funB : Fait partit du calcul de l'IBD
#'
#' @param table 
#'
#' @return
#' @export
#'
#' @examples
funB <- function(table) {
  weights <- data.frame(CLASSE = paste0("CL", seq(7)),
                        POIDS  = seq(7),
                        stringsAsFactors = FALSE)
  
  left_join(x = table, y = weights, by = "CLASSE") %>% 
    group_by(CODE_OPERATION) %>% 
    summarise(B = sum(Fi * POIDS)/sum(Fi))
}

##-------------------------------------------------------------------------------------------------------##
#' funIBD : Fait partit du calcul de l'IBD
#'
#' @param table 
#'
#' @return
#' @export
#'
#' @examples
funIBD <- function(table) {
  mutate(table,
         IBD = case_when(B >= 0 & B<= 2  ~ 1,
                         B > 2  & B < 6  ~ funArrondi(4.75 * B - 8.5, 3), ## comme dans version 1.1.2
                         B >= 6 & B <= 7 ~ 20,
                         TRUE            ~ NA_real_))
}

##-------------------------------------------------------------------------------------------------------##
#' fun_synonymie : Permet de gerer la synonimie des taxons present dans la matrice spécifique
#'
#' @param tab_cont_Long_taxo : matrice d'espece (spécifique) en format long
#' @param var : variable de reférence à utiliser pour realiser la mise à jour taxo
#'
#' @return
#' @export
#'
#' @examples fun_synonymie(tab_cont_Long_taxo = tab_cont_Long_S3E2, var = "REFUNI")
fun_synonymie <- function(tab_cont_Long_taxo,
                          var = "REFUNI") {
  var = rlang::sym(var) # Convert the string to a symbol using sym() 
  tab_flo_long_syno <- tab_cont_Long_taxo %>%
    select(CODE_OPERATION,!!var, RESULTAT) %>%
    group_by(CODE_OPERATION, !!var) %>% 
    mutate(RESULTAT = sum(RESULTAT, na.rm = T)) %>% # somme des abondances pour les memes taxons (refuni)
    ungroup() %>%
    distinct()
  return(tab_flo_long_syno)
}

## Fonction permettant de faire les arrondis a l'inferieur si 0 a 4 et au superieur si 5 a 9
funArrondi <- function (x, digits = 0) {
  .local <- function(x, digits) {
    x <- x * (10^digits)
    ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5,
           ceiling(x)/(10^digits),
           round(x)/(10^digits))
  }

  if (is.data.frame(x))
    return(data.frame(lapply(x, .local, digits)))
  .local(x, digits)
}


##-------------------------------------------------------------------------------------------------------##
#' fun_diversite
#'
#' @param mat_flo_ref matrice floristique (spécifique, d'espèce) sur laquelle on va calculer les 
#' fonctions de diversité shannon, simpson, richesse spécifique et pielou
#'
#' @return
#' @export
#'
#' @examples fun_diversite(mat_flo_ref = tab_cont_ref_S3E2) 
fun_diversite <- function(mat_flo_ref = tab_cont_ref_S3E2) {
  tabDiv <- cbind(SHANNON = vegan::diversity(mat_flo_ref, 
                                             index = "shannon", MARGIN = 2, base = exp(2)),
                  SIMPSON = vegan::diversity(mat_flo_ref, 
                                             index = "simpson", MARGIN = 2, base = exp(2)),
                  RICH_SPE = vegan::specnumber(mat_flo_ref, MARGIN = 2),
                  PIELOU = ((vegan::diversity(mat_flo_ref,MARGIN = 2) / log(vegan::specnumber(mat_flo_ref, MARGIN = 2))))) %>%
    as.data.frame()
  return(tabDiv)
}

##-------------------------------------------------------------------------------------------------------##
#' fun_creation_matrice_metrique : Fonction permettant de creer la matrice des différentes métriques utilisé
#' par les modèles 
#'
#' @param data_entree : matrice floristique
#' @param tab_tax_conserve : tableau des taxon a conserver pour la suite (genre taxon indicateur)
#' @param base_trait : tableau de la base des traits écologique des diatomées
#' @param valeurs_indic_diato  : Valeurs d'indice 
#' @param classes_IBD : Tableau des classes d'IBD
#' @param trait_modalites : Tableau avec les différents trait des taxons et leur modalité
#'
#' @return
#' @export
#'
#' @examples data_entree = data_entree,
#' tab_tax_conserve = tax_a_garder,
#' base_trait = base_trait,
#' valeurs_indic_diato = indices_diato,
#' classes_IBD = index_ibd,
#' trait_modalites = Traits1_S3E2)
fun_creation_matrice_metrique <- function(data_entree = data_entree,
                                          tab_tax_conserve = tax_a_garder,
                                          base_trait = base_trait,
                                          valeurs_indic_diato = indices_diato,
                                          classes_IBD = index_ibd,
                                          trait_modalites = Traits1_S3E2) {
  
  # 01. Supression des opecont pour lesquelles N < 300 cellules (risque de bias lors du calcul des indices de diversité)
  matrice_floLong_S3E2 <- data_entree %>%
    group_by(CODE_OPERATION) %>%
    mutate(SUM_RQ = sum(RESULTAT[CODE_TAXON %in% tab_tax_conserve$tax_a_garder])) %>%
    filter(SUM_RQ < 50) %>% # retrait des valeurs superieur ou egales a 50
    select(-SUM_RQ) %>%
    ungroup()
  
  # Redistribution au prorata des N > 400 cellules (r?gle de trois)
  tabcont1_S3E2 <- matrice_floLong_S3E2 %>%
    group_by(CODE_OPERATION) %>%
    mutate(RESULTAT = (RESULTAT * 400) / sum(RESULTAT)) %>%
    ungroup()
  
  # 02. Filtrer la liste floristique (enlever les taxons non identifiés au niveau de l'espèce et ceux absent dans la table de traits)
  tab_cont4_S3E2 <- tabcont1_S3E2 %>%
    filter(CODE_TAXON %in% base_trait$CODE_TAXON)
  
  ## liste floristique avec information sur les codes genre, familles...
  tab_cont_Long_S3E2 <- tab_cont4_S3E2 %>%
    merge(., base_trait %>% 
            select(CODE_TAXON, REFUNI), all.x = T, by = "CODE_TAXON") 
  
  # 03. Gestion des synonymes
  tab_cont_refLong_S3E2_taxo <- fun_synonymie(tab_cont_Long_taxo = tab_cont_Long_S3E2,
                                              var = "REFUNI") %>%
    merge(., base_trait %>% 
            select(CODE_TAXON, TYPE,FAM, GENRE), all.x = T,
          by.x = "REFUNI", by.y = "CODE_TAXON") 
  
  ## mise en forme matricielle du tableau tab_cont_refLong_S3E2_taxo. Utilisation pour les calculs
  # d'entropie et des indices de diversites
  tab_cont_ref_S3E2 <- spread(tab_cont_refLong_S3E2_taxo %>% select(-TYPE, -FAM, -GENRE), 
                              key = CODE_OPERATION, 
                              value = RESULTAT, fill = 0) %>%
    tibble::column_to_rownames("REFUNI") %>%
    as.data.frame()
  
  # 04. Création de la matrice de traits pour les taxons de références
  traits_refuni_S3E2 <- base_trait %>%
    select(-TYPE, -TAXO) %>% # selection des traits
    # selection des taxons presents dans le tableau de flore (basé sur refuni)
    filter(CODE_TAXON %in% unique(tab_cont_refLong_S3E2_taxo$REFUNI)) %>%
    select(-CODE_TAXON) # CODE_TAXON = REFUNI suite a la selection
  
  # 05. Calcul des métriques à l'échelle de la station
  ## 05.1. Profile des traits par opecont 
  traits_S3E2 <- traits_refuni_S3E2 %>%
    select(-FAM, -GENRE, -TERATO, -TAX_IBD)
  
  ## transfo en log(x+1)
  tab_cont_refLong_Log <- tab_cont_refLong_S3E2_taxo %>%
    mutate(RESULTAT = log1p(RESULTAT))
  
  metriqueTmp <- tab_cont_refLong_Log %>%
    left_join(., traits_S3E2, by = c("REFUNI" = "REFUNI")) %>%
    select(-REFUNI) %>%
    group_by(CODE_OPERATION) %>%
    summarise_at(vars(GUILDE_H:GF4), funs(sum(RESULTAT * .) / sum(RESULTAT))) %>%
    ungroup()
  
  # 05.2. Calcul de l'abondance relative des formes tératologiques des taxons de diatomées
  tera1_Tot_S3E2 <- tab_cont_refLong_S3E2_taxo %>%
    left_join(., traits_refuni_S3E2 %>% select(REFUNI,TERATO), by = c("REFUNI" = "REFUNI")) %>% # ou CODE_TAXON...
    group_by(CODE_OPERATION) %>%
    ## 05.2.1. Effectif total/opecont
    mutate(Ntot = sum(RESULTAT)) %>%
    ## 05.2.2. Effectif terato/opecont
    mutate(Nterato = sum(RESULTAT[TERATO == "oui"])) %>%
    # 05.3. Calcul de l'abondance relative finale des formes tératologiques des taxons de diatomées
    mutate(terato = (Nterato / Ntot) * 100) %>% # Calcul de l'abondance relative des formes terato
    select(-TERATO, -REFUNI, -RESULTAT, -Nterato) %>%
    distinct() %>%
    ungroup()
  
  # 05.4. Entropie quadratique de Rao
  tabRao_S3E2 <- fun_entropie_rao(base_trait = base_trait, 
                                  mat_flo_ref = tab_cont_ref_S3E2,
                                  tab_trait_mod = trait_modalites)
  
  # 05.5. Cacul des métriques taxonomiques/diversité
  ## 05.5.1. Effectif par genre/opecont
  tab_cont_genreLong_S3E2 <- fun_synonymie(tab_cont_Long_taxo = tab_cont_refLong_S3E2_taxo, var = "GENRE") # ok
  
  ## calcul abondance relative du tableau des genres
  tab_cont_genreRelLong_S3E2 <- tab_cont_genreLong_S3E2  %>%
    group_by(CODE_OPERATION) %>%
    mutate(ABRELATIVE = RESULTAT / sum(RESULTAT) * 100) %>%
    ungroup() %>%
    select(-RESULTAT)
  
  ## mise en forme matricielle
  tab_cont_genre_S3E2 <- spread(tab_cont_genreRelLong_S3E2,
                                key = CODE_OPERATION , value = ABRELATIVE, fill = 0) %>%
    tibble::column_to_rownames("GENRE") %>%
    t() %>%
    as.data.frame()
  colnames(tab_cont_genre_S3E2) <- paste0(colnames(tab_cont_genre_S3E2),"q")
  
  ## 05.5.2. Calcul de la richesse spécifique par genre
  # nombre de taxons du releve egal au genre
  # exemple genre ACHD site 17399. 2 especes remontent au genre ACHD dans ce site donc richesse = 2
  richesseGenreRelativeLong_S3E2 <- fun_richesseRelative(tab_flo_ref_taxo_long = tab_cont_refLong_S3E2_taxo,
                                                         var = "GENRE")
  ## mise en forme matricielle
  richesseGenreRelativemat_S3E2 <- richesseGenreRelativeLong_S3E2 %>%
    select(-NBSPGENRE, -NBTOT) %>%
    spread(., key = CODE_OPERATION, value = NBSPGENRE_RELAT, fill = 0) %>%
    tibble::column_to_rownames("GENRE") %>%
    t() %>%
    as.data.frame()
  colnames(richesseGenreRelativemat_S3E2) <- paste0(colnames(richesseGenreRelativemat_S3E2),"s")
  
  ## 05.5.3. Calcul de l'effectif par famille/opecont
  tab_cont_famLong_S3E2 <- fun_synonymie(tab_cont_Long_taxo = tab_cont_refLong_S3E2_taxo, var = "FAM")
  
  ## calcul abondance relative du tableau des fams
  tab_cont_famRelLong_S3E2 <- tab_cont_famLong_S3E2  %>%
    group_by(CODE_OPERATION) %>%
    mutate(ABRELATIVE = RESULTAT / sum(RESULTAT) * 100) %>%
    ungroup() %>%
    select(-RESULTAT)
  
  ## mise en forme matricielle
  tab_cont_fam_S3E2 <- spread(tab_cont_famRelLong_S3E2,
                              key = CODE_OPERATION , value = ABRELATIVE, fill = 0) %>%
    tibble::column_to_rownames("FAM") %>%
    t() %>%
    as.data.frame()
  colnames(tab_cont_fam_S3E2) <- paste0(colnames(tab_cont_fam_S3E2),"q")
  
  ## 05.5.4. Calcul de la richesse spécifique par famille
  richesseFamRelativeLong_S3E2 <- fun_richesseRelative(tab_flo_ref_taxo_long = tab_cont_refLong_S3E2_taxo, var = "FAM")
  
  ## mise en forme matricielle
  richesseFamRelativemat_S3E2 <- richesseFamRelativeLong_S3E2 %>%
    select(-NBSPFAM, -NBTOT) %>%
    spread(., key = CODE_OPERATION, value = NBSPFAM_RELAT, fill = 0) %>%
    tibble::column_to_rownames("FAM") %>%
    t() %>%
    as.data.frame()
  colnames(richesseFamRelativemat_S3E2) <- paste0(colnames(richesseFamRelativemat_S3E2),"s")
  
  ## 05.5.5. Abondance relative des ADMI (code OMNIDIA de Achanthidium minutissimum)
  # browser()
  ADMI <- tab_cont_refLong_S3E2_taxo %>%
    group_by(CODE_OPERATION) %>%
    mutate(Ntot = sum(RESULTAT)) %>%
    filter(REFUNI == "ADMI") %>%
    mutate(ABREL_ADMI = (RESULTAT / Ntot)*100) %>%
    ungroup()
  
  # 05.6. Indices de diversité et d'équitabilité
  tabDiv <- fun_diversite(mat_flo_ref = tab_cont_ref_S3E2) 
  
  # 05.7. Calcul des différents indices biologiques diatomées: IBD, de type saprobique et trophique
  tab_cont_ref_S3E_calcul_IBD <- tab_cont_refLong_S3E2_taxo %>%
    ## 05.7.1. Filtrer la liste des taxons pour obtenir la liste des taxons avec seulement les taxons retenus dans l'indice IBD
    filter(REFUNI %in% classes_IBD$AFNOR)  %>%
    rename(CODE_TAXON = REFUNI) %>% # bien attention ici, normalement ce sont bien les memes par construction
    # mais ça peut être confondant...
    ## 05.7.2. Enlever les taxons dont Ni < 0.75% (Norme IBD-2006)
    mutate(RESULTAT = if_else(RESULTAT < 3, 0, RESULTAT))  %>%
    filter(RESULTAT != 0) 
  
  ## 05.7.3. Calcul de l'IBD
  ### 05.7.3.1. Etape 1 : calcul de B (note IBD sur 7)
  resultatsAx <- funAx(data_entree = tab_cont_ref_S3E_calcul_IBD)
  resultats <- resultatsAx  %>% 
    funFi(table = ., param = classes_IBD)
  
  ### 05.7.3.2. Etape 2 : Transo note IBD/20
  resultats3 <- resultats %>%
    funB(table = .) %>% 
    funIBD(table = .)   
  
  ## 05.7.4. Calcul des indices de type saprobique et trophique (note sur X)
  indices1_S3E <- fun_saproTrophieWana(tab_flo_ref_taxo_long = tab_cont_refLong_S3E2_taxo,
                                       valeurs_indic_diato = valeurs_indic_diato)
  ## Transformation note /20
  indices1_S3E20 <- indices1_S3E %>%
    mutate(IPS = 4.75 * IPS - 3.75) %>%
    mutate(DESCY = 4.75 * DESCY - 3.75)%>%
    mutate(SLADECEK = 20 - 4.75 * SLADECEK)%>%
    mutate(ROTT = 26.786 - 6.786 * ROTT) %>%
    mutate(KELLY = -4.75 * KELLY + 24.75) %>%
    mutate(WATANABE = 0.190 * WATANABE + 1)
  
  #### compilation dans metrique
  
  metrique <- list(metriqueTmp,
                   tera1_Tot_S3E2 %>% select(CODE_OPERATION, terato) %>% distinct(),
                   tabRao_S3E2 %>% tibble::rownames_to_column("CODE_OPERATION"),
                   tab_cont_genre_S3E2 %>% tibble::rownames_to_column("CODE_OPERATION"),
                   richesseGenreRelativemat_S3E2 %>% tibble::rownames_to_column("CODE_OPERATION"),
                   tab_cont_fam_S3E2 %>% tibble::rownames_to_column("CODE_OPERATION"),
                   richesseFamRelativemat_S3E2 %>% tibble::rownames_to_column("CODE_OPERATION"),
                   ADMI %>% select(CODE_OPERATION, ABREL_ADMI),
                   tabDiv %>% tibble::rownames_to_column("CODE_OPERATION"),
                   resultats3 %>% select(CODE_OPERATION, IBD),
                   indices1_S3E20) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="CODE_OPERATION"), .)
  
  return(metrique)
}
##-------------------------------------------------------------------------------------------------------##
### Partie 2 : Calcul outils diagnostique
##-------------------------------------------------------------------------------------------------------##

#' fun_predictWrap : fonction permettant de calculer les predictions via les différents modeles
#' Fonction interne utilisée dans un lapply dans la fonction funPredODD()
#' @param obj : foret d'arbre de format mlr
#' @param newdata : Nouvelles données pour réaliser la prédiction
#'
#' @return
#' @export
#'
#' @examples
fun_predictWrap <- function(obj,newdata){
  predNewData = mlr:::predict.WrappedModel(object = obj, newdata = newdata)
  return(predNewData)
}

## fonction interne de prediction
#' Title
#'
#' @param object : foret d'arbre objet mlr
#' @param newdata : nouvelles données pour le calcul de la prediction
#' @param pred.all : booleen
#'
#' @return
#' @export
#'
#' @examples
predict_DT2 <- function(object,
                        newdata,
                        pred.all = TRUE) {
  ## modif 08/03/22
  ## ??
  funScale <- approxfun(x = c(0, object$threshold, 1),
                        y = c(0, 0.5, 1))
  
  # browser()
  ## attention, utilisation de mlr et non mlr3 (-> car plus les mêmes objets)
  # IP_all <- lapply(object$models,
  #                  mlr:::predict.WrappedModel, #predict,
  #                  newdata = newdata) %>%
  
  ## calcul de la prediction selon le modele
  IP_all <- lapply(object$models,
                   fun_predictWrap, #predict,
                   newdata = newdata) %>%
    lapply(function(pred) {
      funScale(pred$data$prob.impaired)
    })                                           %>%
    do.call(what = cbind)                        %>%
    data.frame()                                 %>%
    dplyr::as.tbl()
  colnames(IP_all) <- paste("iter", 1:length(object$models), sep = "_")
  
  IP_summ <- rowMeans(IP_all) %>%
    as.data.frame()
  colnames(IP_summ) <- "average"
  
  if (pred.all) {
    return(list(IP_all = IP_all, IP_summ = IP_summ))
  } else {
    return(IP_summ)
  }
}

##-------------------------------------------------------------------------------------------------------##
## script de l'ODI
## Fonction permettant d'obtenir les predictions de l'OD
#' basé sur la fonction predict_DT et run_DT du package ecodiag
#'
#' @param newdata : dataframe contenant les metrique biologiques calculées par la fonction xxx

funPredODD <- function(newdata) {
  
  ##------ selection des modeles
  modelList <- list.files(pattern    = "ODDiatomees_model_*")
  
  pressureList <- gsub(modelList,
                       pattern     = "ODDiatomees_model_",
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")
  
  #-------- lancement des calculs
  preds <- lapply(1:length(modelList),
                  function(i) {
                    DTunit <- NULL
                    model <- load(modelList[i]) # chargement du modele
                    calc_pred <- predict_DT2(object      = DTunit,
                                             newdata     = newdata,
                                             pred.all    = FALSE)
                    rm(model, envir = .GlobalEnv)
                    return(calc_pred)
                  })   
  
  preds2 <- preds %>%
    do.call(what = cbind) %>%
    data.frame(CODE_OPERATION = rownames(newdata), .) 
  
  colnames(preds2) <- c("CODE_OPERATION", pressureList)
  # gc(full = T, reset = T)
  return(preds2)
}



##-------------------------------------------------------------------------------------------------------##
##Fonction permettant de creer les diagrammes radars
# issus du script IDGF
#' fun_radar
#'
#' @param resultats 
#' @param nameOutput 
#'
#' @return
#' @export
#'
#' @examples
fun_radar <- function(resultats, nameOutput) {
  
  # Récupération des CODE_OPERATION uniques
  id_releves <- resultats %>%
    pull(CODE_OPERATION) %>%
    unique()
  
  # Enregistrement sous format PDF
  pdf(file = nameOutput)
  #pdf(paste0(chemin_radar,indic,"_", vIndic,"radar.pdf"))
  
  # Boucle sur chaque CODE_OPERATION
  for (code_op in id_releves) {
    
    # Filtrer les données pour la CODE_OPERATION en cours
    result_graph <- resultats %>%
      filter(CODE_OPERATION == code_op) %>%
      mutate(radar_metric = RESULTAT)
    
    # Radar pour les id_releves commençant par CHEM.1
    data_chem <- result_graph %>%
      filter(grepl("^CHEM", LIB_PAR)) %>% 
      mutate(LIB_PAR = str_remove(LIB_PAR, "CHEM.1_"))
    
    # calcul de l'angle pour l'affichage des différentes etiquettes en fonction du nombre de metrique (branches)
    # angleC <- 360 / (2 * pi) * rev(pi / 2 + seq(pi / 14, 2 * pi - pi / 14, len = 13 ))
    
    plot_chem <- ggplot(data_chem, aes(x = LIB_PAR, y = radar_metric)) +
      geom_bar(aes(fill = RESULTAT), stat = "identity", color = "black") +
      scale_y_continuous(limits = c(0,1))+
      scale_fill_gradientn(breaks = c(0,0.2,0.4,0.6,0.8,1.0), limits = c(0,1),
                           colours = c("#FC4E07", "#E7B800", "#00AFBB"))+
      theme(plot.margin = margin(10, 10, 10, 100))+
      coord_polar() +
      theme_bw() +
      theme(panel.grid.major = element_line(color = "grey75"))+
      ggtitle(paste("Radar Chimie - OPERATION:", code_op)) +
      theme(plot.title = element_text(hjust = 0.5, face= "bold"))+
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank()) +
      theme(axis.text.x = element_text(face = "bold", size = 9, color="black"))
    print(plot_chem)
    
    # Radar pour les id_releves commençant par HYDROMORPH
    data_hydro <- result_graph %>%
      filter(grepl("^HYDMORP", LIB_PAR)) %>% 
      mutate(LIB_PAR = str_remove(LIB_PAR, "HYDMORP_"))
    
    # calcul de l'angle pour l'affichage des différentes etiquettes en fonction du nombre de metrique (branches)
    angleH <- 360 / (2 * pi) * rev(pi / 2 + seq(pi / 14, 2 * pi - pi / 14, len = 6 ))
    
    plot_hydro <- 
      ggplot(data_hydro, aes(x = LIB_PAR, y = radar_metric)) +
      geom_bar(aes(fill = RESULTAT), stat = "identity", color="black") +
      scale_y_continuous(limits = c(0,1))+
      scale_fill_gradientn(breaks = c(0,0.2,0.4,0.6,0.8,1.0), limits = c(0,1),
                           colours = c("#FC4E07", "#E7B800", "#00AFBB"))+
      theme(plot.margin = margin(10, 10, 10, 100))+
      coord_polar() +
      theme_bw() +
      theme(panel.grid.major = element_line(color = "grey75"))+
      ggtitle(paste("Radar Hydromorphologie - OPERATION:", code_op)) +
      theme(plot.title = element_text(hjust = 0.5, face= "bold"))+
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank()) +
      theme(axis.text.x = element_text(face = "bold", size = 9, color="black"))
    print(plot_hydro)
  }
  # Fermeture du fichier PDF
  sink() # ajout 23/08/22
  invisible(dev.off())
  
}

##-------------------------------------------------------------------------------------------------------##
#' funCommentairesStation : Fonction generant les commentaires, cas particulier pour les stations
#'  car apres on perd l'info des codes stations
#' si un code station n'est pas dans le tableau de param physico-chimique alors l'indiquer
#' @param Table : Tableau de sortie
#' @param metriques_qualite : Tableau de resumé des taxons contributifs
#'
#' @return : Tableau de commentaire des résultats
#' 
#' @examples  funCommentaires(Table = data_sortieTmp, metriques_qualite = metriques_qualite)
funCommentairesStation <- function(data_entree, metriques_qualite) {
  ## seldection des metriques correspondant à de la chimie
  tab_chimie = dplyr::left_join(data_entree, 
                                metriques_qualite %>% 
                                  mutate(chimie = "chimie") %>%
                                  mutate(cd_site = as.character(cd_site)) %>%
                                  select(cd_site, chimie), by = c("CODE_STATION" = "cd_site")) 
  
  dplyr::bind_rows(
    dplyr::group_by(tab_chime, CODE_STATION) %>%
      dplyr::summarise(COMMENTAIRE = 
                         ifelse(is.na(chimie),
                                "Code station non present dans le tableau des metriques de qualité chimique. Ne pas prendre en compte xxx",
                                "")) %>%
      dplyr::filter(COMMENTAIRE != "") %>%
      distinct()
  )
}
## Fonction initialisant le fichier de sortie
funSortie <- function(data_entree, paramsOut, ...) {
  select(data_entree, ...) %>%
    distinct()             %>%
    (function(df) {
      df[rep(1:nrow(df), each = nrow(paramsOut)),] %>%
        as.tbl()
    })                     %>%
    mutate(CODE_PAR = rep(paramsOut$CODE_PAR,
                          n() / nrow(paramsOut)),
           LIB_PAR  = rep(paramsOut$LIB_PAR,
                          n() / nrow(paramsOut)))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult 		<- function(indic, vIndic, heure_debut,
                        data_sortie, data_complementaire, complementaire,
                        file, file_complementaire,file_radars)
{
  # determination du temps de calcul
  heure_fin       <- Sys.time()
  heure_dif       <- heure_fin - heure_debut
  temps_execution <- paste0(round(heure_dif, 2),
                            attr(heure_dif, "units"))
  
  # creation du bandeau d'information
  etiquette <- paste(indic, vIndic, Sys.Date(),
                     "Temps d'execution :", temps_execution,
                     sep = ";")
  
  # sortie du bandeau d'information
  cat(paste0(etiquette, "\n"), file = file, sep = "")
  
  # sortie du fichier de sortie
  write.table(data_sortie, row.names = FALSE, quote = FALSE, sep = ";",
              file = file, append = TRUE)
  
  # Sortie complementaire
  if(complementaire)
  {
    if (file == "") {
      print("Fichier")
    }
    
    cat(paste0(etiquette, "\n"), file = file_complementaire, sep = "")
    write.table(data_complementaire, row.names = FALSE, quote = FALSE,
                sep = ";", file = file_complementaire, append = TRUE)
    # a séparer par un complémentaire2 (test pour export)
    fun_radar(resultats = data_sortie, 
              nameOutput = file_radars)
  }
  
}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----
# Ne pas afficher les messages d'avis
#options(warn = -1)

# Recuperation du fichier d'entree
File            <- "ODDiatomees_entree_01.txt" 
complementaire  <- TRUE 
#complementaire2 <- TRUE
chemin_radar    <- ""

# Initialisation de l'heure
heure_debut <- Sys.time()

##  IMPORT DES FICHIERS ----
# Import du fichier d'entree
data_entree <- read.table(File, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE, quote = "\"",
                          colClasses = c(CODE_OPERATION = "character",
                                         CODE_STATION   = "character",
                                         CODE_TAXON     = "character"))
## CALCUL DE L'INDICE ----

##-------------------------------------------------------------------------------------------------------##
# 01. Calcul des metriques

## remarque : Lancement un peu long du à l'appel de la fonction div de ade4.
tab_metrique = fun_creation_matrice_metrique(data_entree = data_entree,
                                             tab_tax_conserve = tax_a_garder,
                                             base_trait = base_trait,
                                             valeurs_indic_diato = indices_diato,
                                             classes_IBD = index_ibd,
                                             trait_modalites = Traits1_S3E2)

metriques_qualiteTmp <- tab_metrique %>% as.data.frame()

## Attention, les noms des variables de metriques_qualite, doivent être identiques aux
# variables des arbres du modeles. Il faut donc changer les noms (au moins temporairement)
# Noms variables lecture du tableau ODDiatomees_params_correspondance
# De plus, toutes les colonnes doivent être presentent même si correspond a des sp non presentes dans l'inventaire...
# Pas de NA mais remplacer par des 0

###################################### c'est cette version qui marche...
## mise en forme de la matrice de qualité pour la suite 
metriques_qualiteTmp3 <- metriques_qualiteTmp %>%
  ##   pivot_longer(!CODE_OPERATION, names_to = "metrics", values_to = "values") %>%
  gather(., "metrics","values", -CODE_OPERATION) %>%
  right_join(., correspondanceNomColonnes, by = c("metrics" = 'new_name')) %>%
  select(-metrics) %>%
  ##   pivot_wider(names_from = old_name, values_from = values, values_fill  = 0) %>%
  spread(., old_name, values, fill = 0) %>%
  filter(!is.na(CODE_OPERATION)) %>% # il doit y avoir des doublons dans les codes... ?!!!
  column_to_rownames(var = "CODE_OPERATION")

## colonne ADMI.y ou ABREL_ADMI, quand on a pas d'admi...
metriques_qualiteTmp3[is.na(metriques_qualiteTmp3)] <- 0

# --------------------------------------------------------------------- #
## 03. Calcul des probabilités d'impact

# Calcul des predictions facon ODI

# test misee à jour avec dependances du package mlr, tres long... (> 5min) + pb make : abord...
predictions <- funPredODD(newdata = metriques_qualiteTmp3 %>% 
                            as.data.frame()) 
#gc(full = T, reset = T)

## tableau long
predictions_long <-   predictions %>%
  # pivot_longer(!CODE_OPERATION, names_to = "LIB_PAR", values_to = "RESULTAT")
  gather(., key = "LIB_PAR", value = "RESULTAT", -CODE_OPERATION)

## RESULTATS COMPLEMENTAIRES ----

# --------------------------------------------------------------------- #
## 04. Diagramme radar et metriques complementaires

if (complementaire) {
  data_complementaire <- metriques_qualiteTmp
} else {
  data_complementaire <- NULL
}


## SORTIE DES RESULTATS ----
## INITIALISATION DU FICHIER DE SORTIE ----

paramsOut <- data.frame(CODE_PAR = c(1:19), # codes sandre # pif..!
                        LIB_PAR  = c("CHEM.1_Acidification",
                                     "CHEM.1_Fungicides",
                                     "CHEM.1_HAP",
                                     "CHEM.1_Herbicides",
                                     "CHEM.1_Insecticides" ,            
                                     "CHEM.1_Matieres.azotees",
                                     "CHEM.1_Matieres.organiques",
                                     "CHEM.1_Matieres.Phosphorees"  ,   
                                     "CHEM.1_MES",
                                     "CHEM.1_Micropolluants.mineraux",
                                     "CHEM.1_Micropolluants.organiques",
                                     "CHEM.1_Nitrates",
                                     "CHEM.1_PCB",
                                     "HYDMORP_HM.CATCH",
                                     "HYDMORP_HM.CLOG" ,            
                                     "HYDMORP_HM.HINST",
                                     "HYDMORP_HM.RIV",
                                     #"HYDMORP_HM.STRAI"   , # pas de consideration car AUC < 0.7             
                                     "HYDMORP_HM.TRAN",
                                     "HYDMORP_HM.URB"),
                        stringsAsFactors = FALSE)

data_sortie <- funSortie(data_entree = data_entree,
                         paramsOut   = paramsOut,
                         CODE_OPERATION, CODE_STATION, DATE) %>%
  dplyr::mutate(CODE_OPERATION = as.character(CODE_OPERATION),
                CODE_STATION   = as.character(CODE_STATION),
                DATE           = as.character(DATE))


## attention, pour l'instant les codes operation entre les deux tableaux ne sont pas les memes...
data_sortie <- left_join(x  = data_sortie,
                         y  = predictions_long,
                         by = c("CODE_OPERATION", "LIB_PAR")) 
fichierResultat               <- paste0(indic, "_", vIndic, "_resultats.csv")
fichierResultatComplementaire <- paste0(indic, "_", vIndic,
                                       "_resultats_complementaires.csv")
fichierRadars                 <- paste0(indic, '_', vIndic, "_radars.pdf")

funResult(indic               = indic,
          vIndic              = vIndic,
          heure_debut         = heure_debut,
          data_sortie         = data_sortie,
          data_complementaire = data_complementaire,
          complementaire      = complementaire,
          file                = fichierResultat,
          file_complementaire = fichierResultatComplementaire,
          file_radars         = fichierRadars)
