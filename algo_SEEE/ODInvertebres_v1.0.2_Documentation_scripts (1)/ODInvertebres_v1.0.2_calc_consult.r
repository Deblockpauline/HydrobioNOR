# Type d'algorithme : ODInvertebres
# Auteur(s)         : Cedric MONDY
# Date              : 2020-03-30
# Version           : 1.0.2
# Interpreteur	   	: R version 3.5.3 (2019-03-11)
# Pre-requis        : Packages dplyr, tidyr, sfsmisc, vegan, ade4, ranger, mlr
# Fichiers lies   	: ODInvertebres_params_normalisation.csv, ODInvertebres_params_traits.csv, ODInvertebres_params_transcodage.csv
# Commentaires 	  	:

# Copyright 2020 Cedric MONDY
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
indic  <- "ODInvertebres"
vIndic <- "v1.0.2"

## CHARGEMENT DES PACKAGES ----
dependencies <- c("dplyr", "tidyr", "sfsmisc", "vegan", "ade4", "ranger", "mlr")

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
transcodage <- read.csv2("ODInvertebres_params_transcodage.csv",
                         header = TRUE, stringsAsFactors = FALSE,
                         colClasses = c(CODE_TAXON   = "character",
                                        CODE_METHODE = "character"))

traits <- read.csv2("ODInvertebres_params_traits.csv",
                    skip             = 1,
                    header           = TRUE,
                    stringsAsFactors = FALSE,
                    colClasses       = c(CODE_TAXON = "character")) %>%
  gather(key = "CATEGORIE", value = "VALEUR", -CODE_TAXON)        %>%
  filter((! grepl(x = CATEGORIE, pattern = "SPEAR") & VALEUR > 0) |
           grepl(x = CATEGORIE, pattern = "SPEAR"),
         !is.na(VALEUR))                                          %>%
  mutate(TRAIT = strsplit(CATEGORIE, split = "_")                 %>%
           sapply(function(x){
             x[-length(x)] %>%
               paste(collapse = "_")
           }))

normalisation <- read.csv2("ODInvertebres_params_normalisation.csv",
                           header = TRUE,
                           stringsAsFactors = FALSE)



## DECLARATION DES FONCTIONS ----


## Fonction permettant d'harmoniser les listes taxonomiques
funHarmonisation <- function(Table, Transcodage) {

  getAllTaxa <- function(df) {
    summ <- group_by(df, CODE_OPERATION) %>%
      summarise(taxa = paste(unique(CODE_TAXON), collapse = ", "))

    left_join(df, summ, by = "CODE_OPERATION")      %>%
      split(x = ., f = .$CODE_OPERATION)            %>%
      lapply(function(df.i) {
        expand.grid(CODE_OPERATION = unique(df.i$CODE_OPERATION),
                    CODE_TAXON     = strsplit(df.i$taxa, split = ", ")[[1]],
                    stringsAsFactors = FALSE)
      })                                            %>%
      bind_rows()                                   %>%
      left_join(x = ., y = df,
                by = c("CODE_OPERATION",
                       "CODE_TAXON")) %>%
      mutate(RESULTAT = ifelse(is.na(RESULTAT), 0,
                               RESULTAT))           %>%
      tibble::as_tibble()
  }

  funRedistribution <- function(df) {
    if (nrow(filter(df, !is.na(REDISTRIBUTION))) == 0 |
        is.na(unique(df$FAMILLE))                     |
        sum(df$RESULTAT) == 0) {
      0
    } else {
      filter(df, !is.na(REDISTRIBUTION)) %>%
        '$'("RESULTAT")                %>%
        (function(x) {
          roundfixS(x * df$PROP)
        })
    }
  }

  Table_transformee <- mutate(Table,
                              # Taxons en presence/absence
                              RESULTAT = ifelse(CODE_REMARQUE == 4,
                                                1, RESULTAT))               %>%
    group_by(CODE_OPERATION,
             CODE_TAXON)                   %>%
    summarise(RESULTAT       = sum(RESULTAT))          %>%
    getAllTaxa(df = .)                                 %>%
    left_join(Transcodage, by = "CODE_TAXON")          %>%
    filter(!is.na(CODE_METHODE))                       %>%
    # Regroupe les taxons determines a un niveau plus precis que demande
    group_by(CODE_OPERATION,
             CODE_METHODE)                 %>%
    summarise(RESULTAT = sum(RESULTAT))                %>%
    left_join(select(Transcodage, -CODE_METHODE),
              by = c("CODE_METHODE" = "CODE_TAXON"))   %>%
    # Taxons en presence/absence
    mutate(RESULTAT = ifelse(QUANTIFICATION == "presence" &
                               RESULTAT > 0,
                             1, RESULTAT))             %>%
    ungroup()                                          %>%
    mutate(op_famille = paste0(CODE_OPERATION, "_", FAMILLE))

  # Redisribue les taxons determines a un niveau insuffisant
  ## liste les op x familles pour lesquelles doit redistribuer
  redistributions <- Table_transformee                       %>%
    group_by(op_famille)                                     %>%
    summarise(nb_redistri = length(na.omit(REDISTRIBUTION)),
              nb_non_redistri = sum(is.na(REDISTRIBUTION)))  %>%
    filter(nb_redistri > 0 & nb_non_redistri > 0)            %>%
    '$'("op_famille")

  Table_sans_redistribution <- filter(Table_transformee,
                                      ! op_famille %in% redistributions)
  Table_avec_redistribution <- filter(Table_transformee,
                                      op_famille %in% redistributions)

  Table_redistribuee <- Table_avec_redistribution %>%
    ## Calcul des proportions des taxons enfants
    left_join(x = .,
              y = group_by(., CODE_OPERATION, FAMILLE) %>%
                filter(is.na(REDISTRIBUTION))          %>%
                summarise(TOTAL = sum(RESULTAT)),
              by = c("CODE_OPERATION", "FAMILLE")) %>%
    left_join(x = .,
              y = group_by(., CODE_OPERATION, FAMILLE,
                           CODE_METHODE) %>%
                summarise(PROP = sum(RESULTAT) / unique(TOTAL)),
              by = c("CODE_OPERATION", "CODE_METHODE",
                     "FAMILLE"))                   %>%
    mutate(PROP = ifelse(is.na(TOTAL), 0,
                         ifelse(!is.na(REDISTRIBUTION), -1,
                                PROP)))            %>%
    ungroup(.)                                     %>%
    ## Redistribue les effectifs
    mutate(fac = paste0(CODE_OPERATION, "_",
                        FAMILLE)) %>%
    (function(df) {
      split(df, df$fac) %>%
        lapply(function(df) {
          mutate(df, RESULTAT = RESULTAT + funRedistribution(df))
        })              %>%
        bind_rows()
    })

  if (nrow(Table_redistribuee) > 0) {
    bind_rows(
      select(Table_sans_redistribution,
             CODE_OPERATION,
             CODE_METHODE, RESULTAT),
      select(Table_redistribuee,
             CODE_OPERATION,
             CODE_METHODE, RESULTAT)
    )                                                 %>%
      arrange(CODE_OPERATION,
              CODE_METHODE) %>%
      filter(RESULTAT > 0)

  } else {
    select(Table_sans_redistribution,
           CODE_OPERATION,
           CODE_METHODE, RESULTAT) %>%
      arrange(CODE_OPERATION,
              as.numeric(CODE_METHODE)) %>%
      filter(RESULTAT > 0)
  }
}

## Fonction permettant le calcul des métriques basées sur les traits
funTrait <- function(Table, Traits) {

  Table <- filter(Table, CODE_METHODE %in% Traits$CODE_TAXON)

  # Profils moyens des communautés
  abondance <-  group_by(Table, CODE_OPERATION) %>%
    summarise(richness = n_distinct(CODE_METHODE),
              abTot    = sum(RESULTAT),
              logAbTot = sum(log(RESULTAT + 1)))

  profils <- left_join(Table, Traits,
                       by = c("CODE_METHODE" = "CODE_TAXON")) %>%
    filter(! TRAIT %in% c("GROUPE", "SPEAR"))       %>%
    mutate(VALEUR = as.numeric(VALEUR))                     %>%
    left_join(abondance, by = "CODE_OPERATION")             %>%
    group_by(CODE_OPERATION, TRAIT, CATEGORIE)              %>%
    summarise(CWM = sum(log(RESULTAT + 1) * VALEUR) /
                unique(logAbTot))                         %>%
    ungroup()                                               %>%
    select(-TRAIT)                                          %>%
    mutate(CATEGORIE = factor(CATEGORIE,
                              levels = unique(Traits$CATEGORIE))) %>%
    spread(key = "CATEGORIE", value = "CWM", fill = 0)

  # Specialisation des communautes
  tsi <-  filter(Traits,
                 ! TRAIT %in% c("GROUPE", "SPEAR")) %>%
    mutate(VALEUR = as.numeric(VALEUR))                   %>%
    left_join(group_by(Traits, TRAIT) %>%
                summarise(k = n_distinct(CATEGORIE)),
              by = "TRAIT")                               %>%
    group_by(CODE_TAXON, TRAIT)                           %>%
    summarise(TSI = (sum(VALEUR^2) - 1 / unique(k)) /
                (1 - 1 / unique(k)))

  specialisation <- left_join(Table, tsi,
                              by = c("CODE_METHODE" = "CODE_TAXON")) %>%
    filter(!is.na(TRAIT))                                          %>%
    left_join(abondance, by = "CODE_OPERATION")                    %>%
    group_by(CODE_OPERATION, TRAIT)                                %>%
    summarise(CSI = sum(TSI * log(RESULTAT + 1)) /
                unique(logAbTot))                                %>%
    mutate(TRAIT = paste("CSI", TRAIT, sep = "_"))                 %>%
    spread(key = "TRAIT", value = "CSI")

  # Recouvrement de niches
  calc_piankaDist <- function(m) {
    designdist(m, method = "J/sqrt(A*B)", terms = "quadratic")
  }

  distances <- filter(Traits,
                      ! TRAIT %in% c("GROUPE", "SPEAR"),
                      CODE_TAXON %in% unique(Table$CODE_METHODE)) %>%
    split(.$TRAIT)                                             %>%
    lapply(select, -TRAIT)                                     %>%
    lapply(spread, key = "CATEGORIE",
           value = "VALEUR", fill = 0)                         %>%
    lapply(function(df) {
      taxa <- df$CODE_TAXON

      df <- as.matrix(df[, -1]) %>%
        apply(MARGIN = 2, as.numeric)
      rownames(df) <- taxa

      return(df)
    })                                                         %>%
    lapply(calc_piankaDist)                                    %>%
    lapply(as.matrix)

  calc_recouvrement <- function(taxa, dist) {
    tx <- unique(taxa) %>%
      as.character() %>%
      (function(x) {x[x %in% colnames(dist)]})

    mean(as.dist(dist[tx, tx]))
  }

  recouvrement <- lapply(names(distances),
                         function(i) {
                           d <- distances[[i]]

                           group_by(Table, CODE_OPERATION)                %>%
                             summarise(pianka =
                                         calc_recouvrement(taxa = CODE_METHODE,
                                                           dist = d)) %>%
                             mutate(TRAIT = i)
                         })                              %>%
    bind_rows()                                        %>%
    mutate(TRAIT = paste("SIMILARITE", TRAIT, sep = "_")) %>%
    spread(key = "TRAIT", value = "pianka")


  # Entropie quadratique de Rao
  distancesEuclid <- filter(Traits,
                            ! TRAIT %in% c("GROUPE", "SPEAR"),
                            CODE_TAXON %in% unique(Table$CODE_METHODE)) %>%
    split(.$TRAIT)                                                    %>%
    lapply(select, -TRAIT)                                            %>%
    lapply(spread, key = "CATEGORIE",
           value = "VALEUR", fill = 0)                                %>%
    lapply(function(df) {
      taxa <- df$CODE_TAXON

      df <- as.matrix(df[, -1]) %>%
        apply(MARGIN = 2, as.numeric)
      rownames(df) <- taxa

      return(df)
    })                                                                %>%
    lapply(dist, method = "euclidean")                                %>%
    lapply(as.matrix)

  calc_rao <- function(df, dist) {
    df.sub <- filter(df, CODE_METHODE %in% colnames(dist))

    taxa <- as.character(df.sub$CODE_METHODE)

    df.sub <- df.sub          %>%
      select(-CODE_METHODE) %>%
      as.data.frame()       %>%
      (function(m) {
        rownames(m) <- taxa
        return(m)
      })

    d <- dist[taxa, taxa] %>%
      as.dist()

    divc(df    = df.sub,
         dis   = d,
         scale = FALSE)
  }

  entropie <- lapply(names(distancesEuclid),
                     function(i) {
                       d.i <- distancesEuclid[[i]]

                       spread(Table, key = "CODE_OPERATION",
                              value = "RESULTAT",
                              fill = 0)                 %>%
                         calc_rao(df = ., dist = d.i) %>%
                         transmute(CODE_OPERATION = rownames(.),
                                   TRAIT = i,
                                   rao   = diversity)
                     })                              %>%
    bind_rows()                                    %>%
    mutate(TRAIT = paste("RAO", TRAIT, sep = "_")) %>%
    spread(key = "TRAIT", value = "rao")           %>%
    tibble::as_tibble()

  # Metriques SPEAR
  spearTraits <- filter(traits, TRAIT == "SPEAR") %>%
    mutate(CODE_TAXON = CODE_TAXON,
           VALEUR     = as.numeric(VALEUR))     %>%
    tibble::as_tibble()

  spear <- left_join(Table, spearTraits,
                     by = c("CODE_METHODE" = "CODE_TAXON"))       %>%
    left_join(abondance, by = "CODE_OPERATION")                  %>%
    filter(!is.na(TRAIT))                                        %>%
    group_by(CODE_OPERATION)                                     %>%
    summarise(S      = unique(richness),
              Q      = unique(logAbTot),
              SPEAR_S = n_distinct(CODE_METHODE[!is.na(VALEUR)  &
                                                  VALEUR == 1 &
                                                  CATEGORIE %in% "SPEAR_CLASSE"]),
              SPEAR_Q = sum(log(RESULTAT[!is.na(VALEUR)  &
                                           VALEUR == 1 &
                                           CATEGORIE %in% "SPEAR_CLASSE"] + 1)),
              SPEAR_SENSIBILITE_MAX  = max(VALEUR[CATEGORIE %in% "SPEAR_SENSIBILITE"],
                                           na.rm = TRUE),
              avgSensitivityS = sum(VALEUR[CATEGORIE %in% "SPEAR_SENSIBILITE"],
                                    na.rm = TRUE),
              avgSensitivityQ = sum(log(RESULTAT[CATEGORIE %in% "SPEAR_SENSIBILITE"] + 1) *
                                      VALEUR[CATEGORIE %in% "SPEAR_SENSIBILITE"],
                                    na.rm = TRUE))              %>%
    mutate(SPEAR_S      = SPEAR_S / S,
           SPEAR_Q      = SPEAR_Q / Q,
           SPEAR_SENSIBILITE_MOY_S = avgSensitivityS / S,
           SPEAR_SENSIBILITE_MOY_Q = avgSensitivityQ / Q)                %>%
    select(CODE_OPERATION,
           SPEAR_S, SPEAR_Q,
           SPEAR_SENSIBILITE_MAX,
           SPEAR_SENSIBILITE_MOY_S, SPEAR_SENSIBILITE_MOY_Q)

  # Groupes bio-ecologiques
  bioeco1 <- left_join(Table,
                       filter(traits, TRAIT %in% "GROUPE"),
                       by = c("CODE_METHODE" = "CODE_TAXON")) %>%
    filter(!is.na(CATEGORIE))                               %>%
    group_by(CODE_OPERATION, CATEGORIE, VALEUR)             %>%
    summarise(s = n_distinct(CODE_METHODE),
              q = sum(RESULTAT))                            %>%
    left_join(transmute(abondance,
                        CODE_OPERATION = CODE_OPERATION,
                        Stot = richness,
                        Qtot = abTot),
              by = "CODE_OPERATION")                        %>%
    mutate(S = s / Stot,
           Q = q / Qtot)                                    %>%
    select(-s, -q, -Stot, -Qtot)

  bioeco2 <- bioeco1                             %>%
    group_by(CODE_OPERATION, CATEGORIE)        %>%
    summarise(SHANNON = -sum(Q * log2(Q))) %>%
    gather(key = "METRIQUE", value = "RESULTAT",
           -CODE_OPERATION, -CATEGORIE)

  bioeco1 <- bioeco1                                   %>%
    gather(key = "METRIQUE", value = "RESULTAT",
           -CODE_OPERATION, -CATEGORIE, -VALEUR)     %>%
    ungroup()                                        %>%
    mutate(METRIQUE = paste0(VALEUR, "_", METRIQUE)) %>%
    select(-VALEUR)

  bioeco <- bind_rows(bioeco1, bioeco2)              %>%
    mutate(METRIQUE = paste(CATEGORIE,
                            METRIQUE, sep = "_"))  %>%
    select(-CATEGORIE)                             %>%
    spread(key = "METRIQUE", value = "RESULTAT", fill = 0)

  # regroupe et retourne toutes les metriques
  full_join(profils, specialisation, by = "CODE_OPERATION") %>%
    full_join(recouvrement,        by = "CODE_OPERATION") %>%
    full_join(entropie,            by = "CODE_OPERATION") %>%
    full_join(spear,               by = "CODE_OPERATION") %>%
    full_join(bioeco,              by = "CODE_OPERATION")

}

## Fonction permettant d'obtenir les predictions de l'OD
funPred <- function(data, normalisation) {

  data <- as.data.frame(data)
  rownames(data) <- data$CODE_OPERATION

  predict_DT <- function(object,
                         newdata,
                         pred.all = TRUE) {

    IP_all <- lapply(object$models,
                     predict,
                     newdata = newdata) %>%
      lapply(function(pred) {
        pred$data$prob.impaired
      })                                           %>%
      do.call(what = cbind)                        %>%
      data.frame()                                 %>%
      tibble::as_tibble()
    colnames(IP_all) <- paste("iter", 1:length(object$models), sep = "_")

    IP_summ <- apply(IP_all, 1, mean) %>%
      data.frame()                  %>%
      tibble::as_tibble()
    colnames(IP_summ) <- "average"

    if (pred.all) {
      return(list(IP_all = IP_all, IP_summ = IP_summ))
    } else {
      return(IP_summ)
    }
  }

  funTransfo <- function(df, norme) {
    mini <- norme$MINI
    best <- norme$BEST
    maxi <- norme$MAXI

    mutate(df,
           IR = ifelse(average <= best,
                       0.5 * (average - mini) / (best - mini),
                       0.5 + 0.5 * (average - best) / (maxi - best))) %>%
      select(IR)
  }

  modelList <- list.files(pattern    = "ODInvertebres_model_*")

  pressureList <- gsub(modelList,
                       pattern     = "ODInvertebres_model_",
                       replacement = "") %>%
    gsub(pattern     = ".rda",
         replacement = "")

  preds <- lapply(1:length(modelList),
                  function(i) {
                    DTunit <- NULL
                    load(modelList[i])

                    predict_DT(object      = DTunit,
                               newdata     = data[, -1],
                               pred.all    = FALSE) %>%
                      funTransfo(norme =
                                   filter(normalisation,
                                          PRESSION == pressureList[i])) %>%
                      mutate(IR = round(IR, 4))

                  })    %>%
    do.call(what = cbind) %>%
    data.frame(CODE_OPERATION = data$CODE_OPERATION, .)

  colnames(preds) <- c("CODE_OPERATION", pressureList)

  return(preds)
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

## Fonction initialisant le fichier de sortie
funSortie <- function(data_entree, paramsOut, ...) {
  select(data_entree, ...) %>%
    distinct()             %>%
    (function(df) {
      df[rep(1:nrow(df), each = nrow(paramsOut)),] %>%
        tibble::as_tibble()
    })                     %>%
    mutate(CODE_PAR = rep(paramsOut$CODE_PAR,
                          n() / nrow(paramsOut)),
           LIB_PAR  = rep(paramsOut$LIB_PAR,
                          n() / nrow(paramsOut)))
}

## Fonction permettant d'ecrire le fichier de sortie
funResult 		<- function(indic, vIndic, heure_debut,
                        data_sortie, data_complementaire, complementaire,
                        file, file_complementaire)
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
    }

}# fin de la fonction funResult

## INITIALISATION DU TRAITEMENT ----
# Ne pas afficher les messages d'avis
options(warn = -1)

# Recuperation du fichier d'entree


File <- "ODInvertebres_entree_01.txt"
complementaire <- TRUE

# Initialisation de l'heure
heure_debut <- Sys.time()

##  IMPORT DES FICHIERS ----
# Import du fichier d'entree
data_entree <- read.table(File, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE, quote = "\"",
                          colClasses = c(CODE_OPERATION = "character",
                                         CODE_STATION   = "character",
                                         CODE_TAXON     = "character")) %>%
  filter(RESULTAT > 0)

# Traitements preliminaires
## Harmonisation des listes faunistiques
tableFaunistique <- funHarmonisation(data_entree, transcodage) %>%
  filter(RESULTAT > 0)

## CALCUL DE L'INDICE ----
# Calcul des metriques

allMetrics <- c(paste0("TAILLE_",          seq(5)),
                paste0("LONGEVITE_",       seq(2)),
                paste0("VOLTINISME_",      seq(3)),
                paste0("STADE_AQUATIQUE_", seq(4)),
                paste0("REPRODUCTION_",     seq(7)),
                paste0("DISPERSION_",      seq(4)),
                paste0("RESISTANCE_",      seq(4)),
                paste0("RESPIRATION_",     seq(3)),
                paste0("LOCOMOTION_",      seq(6)),
                paste0("NOURRITURE_",      seq(7)),
                paste0("ALIMENTATION_",    seq(7)),
                paste0("TRANSVERSAL_",     seq(6)),
                paste0("LONGITUDINAL_",    seq(8)),
                paste0("ALTITUDE_",        seq(3)),
                paste0("SUBSTRAT_",        seq(9)),
                paste0("COURANT_",         seq(4)),
                paste0("TROPHIE_",         seq(3)),
                paste0("SALINITE_",        seq(2)),
                paste0("TEMPERATURE_",     seq(3)),
                paste0("SAPROBIE_",        seq(5)),
                paste0("PH_",              seq(6)),
                paste0("CSI_",
                       c("TAILLE", "LONGEVITE", "VOLTINISME",
                         "STADE_AQUATIQUE", "REPRODUCTION",
                         "DISPERSION", "RESISTANCE", "RESPIRATION",
                         "LOCOMOTION", "NOURRITURE", "ALIMENTATION",
                         "TRANSVERSAL", "LONGITUDINAL", "ALTITUDE",
                         "SUBSTRAT", "COURANT", "TROPHIE", "SALINITE",
                         "TEMPERATURE", "SAPROBIE", "PH")),
                paste0("SIMILARITE_",
                       c("TAILLE", "LONGEVITE", "VOLTINISME",
                         "STADE_AQUATIQUE", "REPRODUCTION",
                         "DISPERSION", "RESISTANCE", "RESPIRATION",
                         "LOCOMOTION", "NOURRITURE", "ALIMENTATION",
                         "TRANSVERSAL", "LONGITUDINAL", "ALTITUDE",
                         "SUBSTRAT", "COURANT", "TROPHIE", "SALINITE",
                         "TEMPERATURE", "SAPROBIE", "PH")),
                paste0("RAO_",
                       c("TAILLE", "LONGEVITE", "VOLTINISME",
                         "STADE_AQUATIQUE", "REPRODUCTION",
                         "DISPERSION", "RESISTANCE", "RESPIRATION",
                         "LOCOMOTION", "NOURRITURE", "ALIMENTATION",
                         "TRANSVERSAL", "LONGITUDINAL", "ALTITUDE",
                         "SUBSTRAT", "COURANT", "TROPHIE", "SALINITE",
                         "TEMPERATURE", "SAPROBIE", "PH")),
                paste0("SPEAR_",
                       c("S", "Q",
                         "SENSIBILITE_MAX",
                         "SENSIBILITE_MOY_S", "SENSIBILITE_MOY_Q")),
                paste0("GROUPE_BIO_", letters[seq(8)], "_S"),
                paste0("GROUPE_BIO_", letters[seq(8)], "_Q"),
                paste0("GROUPE_ECO_", LETTERS[seq(7)], "_S"),
                paste0("GROUPE_ECO_", LETTERS[seq(7)], "_Q"),
                paste0("GROUPE_BIOECO_",
                       c("a", "b", "d", "e", "g", "z"), "_S"),
                paste0("GROUPE_BIOECO_",
                       c("a", "b", "d", "e", "g", "z"), "_Q"),
                paste0("GROUPE_", c("BIO", "ECO", "BIOECO"),
                       "_SHANNON"))

metriques <- funTrait(tableFaunistique, traits)

missingMetrics <- allMetrics[! allMetrics %in% colnames(metriques)]

if (length(missingMetrics) > 0) {
  metriques <- bind_cols(metriques,
                         matrix(data = 0,
                                nrow = nrow(metriques),
                                ncol = length(missingMetrics),
                                dimnames = list(NULL, missingMetrics)) %>%
                           as.data.frame()) %>%
    '['(c("CODE_OPERATION", allMetrics))
}


# Calcul des predictions

predictions <- funPred(data = metriques, normalisation = normalisation) %>%
  mutate(CODE_OPERATION = as.character(CODE_OPERATION))

## RESULTATS COMPLEMENTAIRES ----
if (complementaire) {
  data_complementaire <-
    select_at(metriques,
              .vars = c("CODE_OPERATION", allMetrics))
} else {
  data_complementaire <- NULL
}

## SORTIE DES RESULTATS ----

data_sortie <- left_join(predictions,
                         select(data_entree,
                                CODE_OPERATION, CODE_STATION, DATE) %>%
                           group_by(CODE_OPERATION) %>%
                           summarise(CODE_STATION = unique(CODE_STATION),
                                     DATE = unique(DATE)),
                         by = "CODE_OPERATION") %>%
  select(CODE_STATION, DATE, CODE_OPERATION,
         MATIERES_ORGANIQUES, MATIERES_PHOSPHOREES,
         MATIERES_AZOTEES, NITRATES,
         HAP, PESTICIDES,
         RIPISYLVE, VOIES_COMMUNICATION, URBANISATION_100M,
         RISQUE_COLMATAGE, INSTABILITE_HYDROLOGIQUE, ANTHROPISATION_BV)

fichierResultat               <- paste0(indic, "_", vIndic, "_resultats.csv")
fichierResultatComplementaire <- paste0(indic, "_", vIndic,
                                        "_resultats_complementaires.csv")
funResult(indic               = indic,
          vIndic              = vIndic,
          heure_debut         = heure_debut,
          data_sortie         = data_sortie,
          data_complementaire = data_complementaire,
          complementaire      = complementaire,
          file                = fichierResultat,
          file_complementaire = fichierResultatComplementaire)
