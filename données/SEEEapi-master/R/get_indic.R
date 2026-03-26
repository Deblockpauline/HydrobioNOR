#' Get the indicators available on SEEE and their version
#'
#' @param indic optional, a character vector with the names of the indicators
#'   for which one want information. If not provided, will return all the
#'   available indicators.
#' @param type optional, a character vector with the type of the indicators
#'   (among "beta", "evaluation" and "diagnostic")
#'
#' @return a data frame with four columns for each indicator version available
#'   on the SEEE: its name, version, type and the number of required input files
#'
#' @importFrom httr GET content
#' @importFrom dplyr '%>%' tibble bind_rows mutate filter select left_join
#'   group_by
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#'
#' @export
get_indic <- function(indic = NULL, type = NULL) {

  types <- c(beta       = "beta-test",
             evaluation = "Outil d'évaluation",
             diagnostic = "Outil de diagnostic")

  indicators <- GET(url = "http://seee.eaufrance.fr/api/indicateurs") %>%
    read_html()                                                       %>%
    html_text()                                                       %>%
    gsub(x = ., pattern = "[", replacement = "", fixed = TRUE)        %>%
    gsub(pattern = "]", replacement = "", fixed =  TRUE)              %>%
    strsplit(split = "},{", fixed = TRUE)                             %>%
    '[['(1)                                                           %>%
    gsub(pattern = "{", replacement = "", fixed = TRUE)               %>%
    gsub(pattern = "}", replacement = "", fixed = TRUE)               %>%
    gsub(pattern = "\"", replacement = "")                            %>%
    sapply(strsplit, split = ",")                                     %>%
    lapply(function(x) {
      tibble(Indicator = gsub(x           = x[1],
                              pattern     = "indicateur:",
                              replacement = ""),
             Version = gsub(x           = x[2],
                            pattern     = "version:",
                            replacement = ""))
    })                                                                %>%
    bind_rows()                                                       %>%
    mutate(ID = seq(n()))

  if (!is.null(indic)) {
    indicators <- filter(indicators, Indicator %in% indic)
  }

  y <- split(indicators, indicators$ID) %>%
    lapply(FUN = function(x, ...) {
      GET(paste0("http://seee.eaufrance.fr/api/indicateurs/",
                 x[1], "/", x[2])) %>%
        content()                  %>%
        (function(y) {
          if ("type" %in% names(y)) {
            type <- y$type
          } else {
            type <- NA_character_
          }
          tibble(ID = x$ID,
                 Type          = type,
                 `Input files` = length(y$entree))
        })
    }) %>%
    bind_rows()

  indicators <- left_join(
    indicators,
    y,
    by = "ID") %>%
    select(-ID)

  if (!is.null(type)) {
    indicators <- filter(indicators, Type %in% types[type])
  }

  select(indicators, Type, Indicator, Version, `Input files`)
}