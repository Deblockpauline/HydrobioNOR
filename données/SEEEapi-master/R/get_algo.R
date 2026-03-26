#' Title
#'
#' @param indic
#' @param version
#' @param dir_path
#'
#' @return
#' @export
#' @importFrom dplyr '%>%' pull
#' @importFrom httr GET content
#'
#' @examples
get_algo <- function(indic, version = NULL, dir_algo, uncompress = TRUE) {
  if (is.null(version)) {
    version <- get_indic(indic = indic) %>%
      pull(Version)                     %>%
      find_latest_version()
  }

  if (!dir.exists(file.path(dir_algo, indic, version)))
    dir.create(file.path(dir_algo, indic, version), recursive = TRUE)

  GET(url = file.path("http://seee.eaufrance.fr/api/indicateurs/algo", indic, version)) %>%
    content("raw") %>%
    writeBin(file.path(dir_algo, paste0(indic, "_", version, ".zip")))

  if (uncompress) {
    unzip(zipfile = file.path(dir_algo, paste0(indic, "_", version, ".zip")),
          exdir = file.path(dir_algo, indic, version))
  }
}