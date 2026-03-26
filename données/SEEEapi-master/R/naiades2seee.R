#' Title
#'
#' @param export_naiades
#' @param stations_types
#'
#' @export
#'

#' @importFrom archive archive_read
#' @importFrom dplyr pull distinct anti_join select filter inner_join
#' @importFrom vroom vroom
naiades2seee_mib <- function(export_naiades, stations_types) {
  listes <- archive::archive_read(archive = export_naiades, file = "ListesFauneFlore.CSV") |> 
    vroom::vroom()

  non_traites <- listes |>
    dplyr::anti_join(
      stations_types |>
        dplyr::filter(!is.na(TypeCEStationMesureEauxSurface)) |>
        dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface),
      by = "CdStationMesureEauxSurface"
    ) |>
    dplyr::distinct(CdStationMesureEauxSurface) |>
    dplyr::pull(CdStationMesureEauxSurface)


  if (length(non_traites) > 0)
    warning("Les stations suivantes ne seront pas traitées car n'ayant pas de type définis:\n", paste(non_traites, collapse = ", "))

  listes |>
    dplyr::inner_join(
      stations_types |>
        dplyr::select(CdStationMesureEauxSurface, TypeCEStationMesureEauxSurface),
      by = "CdStationMesureEauxSurface"
    ) |>
    dplyr::select(
      CODE_OPERATION = RefOperationPrelBio,
      CODE_STATION = CdStationMesureEauxSurface,
      DATE = DateDebutOperationPrelBio,
      TYPO_NATIONALE = TypeCEStationMesureEauxSurface,
      CODE_PHASE = CdListeFauFlor,
      CODE_TAXON = CdAppelTaxon,
      RESULTAT = RsTaxRep,
      CODE_REMARQUE = CdRqNbrTaxRep
    )
}

#' Title
#'
#' @param export_naiades
#'
#' @export
#'

#' @importFrom archive archive_read
#' @importFrom dplyr left_join, select
#' @importFrom tidyr drop_na
#' @importFrom vroom vroom
naiades2seee_diat <- function(export_naiades) {
  ref_taxo_sandre <- vroom::vroom("http://mdm.sandre.eaufrance.fr/mdm_sandre/exports/codes_taxonomiques") 

  listes_seee <- archive::archive_read(archive = export_naiades, file = "ListesFauneFlore.CSV") |> 
    vroom::vroom() |>
    dplyr::left_join(
      ref_taxo_sandre |>
        dplyr::select(CdAppelTaxon, dplyr::contains("OMNIDIA")) |>
        dplyr::filter(!dplyr::if_all(dplyr::contains("OMNIDIA"), is.na)),
       by = "CdAppelTaxon"
      ) |>
    dplyr::transmute(
      CODE_STATION = CdStationMesureEauxSurface,
      cd_sandre = CdAppelTaxon,
      CODE_TAXON = dplyr::case_when(
        !is.na(REF_OMNIDIA) ~ REF_OMNIDIA,
        !is.na(SYN_OMNIDIA) ~ SYN_OMNIDIA,
        !is.na(TERA_OMNIDIA) ~ TERA_OMNIDIA,
        !is.na(SP_OMNIDIA) ~ SP_OMNIDIA
      ),
      RESULTAT = RsTaxRep,
      DATE = DateDebutOperationPrelBio,
      CODE_OPERATION = RefOperationPrelBio
    )
  
    non_pris_en_compte <- listes_seee |> 
      dplyr::filter(is.na(CODE_TAXON)) |> 
      dplyr::distinct(cd_sandre) |> 
      dplyr::pull(cd_sandre)

  if (length(non_pris_en_compte) > 0)
    warning(paste0("Aucun code OMNIDIA n'a été trouvé pour les taxons suivants: : ", paste(sort(non_pris_en_compte), collapse = ", ")))

  listes_seee
}