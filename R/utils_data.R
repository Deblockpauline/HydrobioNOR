#' Cache des données en mémoire, partagé entre toutes les sessions Shiny
#' @noRd
.cache_donnees <- new.env(parent = emptyenv())

#' Télécharge et charge les données (une seule fois par instance R)
#' @noRd
get_data_hydrobioNOR <- function() {
  if (is.null(.cache_donnees$donnees)) {
    fichier_temp <- tempfile(fileext = ".rda")
    url_data <- "https://raw.githubusercontent.com/Deblockpauline/HydrobioNOR/main/dev/data_hydrobioNOR.rda"
    options(timeout = 300)
    utils::download.file(
      url = url_data,
      destfile = fichier_temp,
      mode = "wb" )
    env <- new.env()
    load(fichier_temp, envir = env)
    unlink(fichier_temp)
    .cache_donnees$donnees <- as.list(env)}
  .cache_donnees$donnees}
