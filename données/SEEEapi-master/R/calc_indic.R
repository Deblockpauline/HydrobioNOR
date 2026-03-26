#' Calculate SEEE indicators
#'
#' @param indic the name of the indicator
#' @param version version number of the indicator. If not provided, the latest
#'   is used
#' @param file_paths character vector with the paths of the files used to
#'   calculate the indicator. The order must be the ones described in the
#'   documentation of the indicator
#' @param data a list of data.frames. Used if file_paths is not provided. The
#'   order must be the ones described in the documentation of the indicator
#' @param locally
#' @param dir_algo
#'
#' @return a list with two elements:
#'
#' @importFrom dplyr '%>%' pull bind_cols filter if_else n as_tibble
#' @importFrom purrr map map_lgl map_chr walk
#' @importFrom readr write_delim
#' @importFrom httr upload_file
#'
#' @export
calc_indic <- function(indic, version = NULL, file_paths = NULL, data = NULL,
                       locally = FALSE, dir_algo = NULL) {

  if (is.null(version)) {
    version <- get_indic(indic = indic) %>%
      pull(Version)                     %>%
      find_latest_version()
  }

  if (!is.null(file_paths)) {
    files <- map(file_paths, upload_file)
    names(files) <- paste0("file", seq(length(files)))
  } else {
    if (!is.null(data)) {

      if (grepl("data.frame", class(data))) {
        file_paths <- tempfile(fileext = ".txt")

        write_delim(x = data, file = file_paths, delim = "\t")

      } else {
        if (grepl("list", class(data)) &
            all(map_lgl(data,
                       function(df) {
                         any(grepl("data.frame", class(df)))
                       }))) {
          delim = rep("\t" ,length(data))

          if (grepl("EBio_", indic))
            delim[-1] <- ";"

          file_paths <- map_chr(seq(length(data)),
                               (function(i) tempfile(fileext = ".txt")))

          walk(seq(length(file_paths)),
               function(i) {
                 write_delim(x     = data[[i]],
                             file  = file_paths[i],
                             delim = delim[i])
               })
        }
      }

      files <- map(file_paths, upload_file)
      names(files) <- paste0("file", seq(length(files)))

    } else {
      stop("Either one of file_paths or data must be provided")
    }
  }

  if (!locally) {

    run_distant(indic = indic, version = version, files = files)

  } else {
    if (is.null(dir_algo))
      stop("When locally is set to TRUE, the path of the directory where the algorithms are saved must be given")

    if (!file.exists(file.path(dir_algo, indic, version,
                               paste0(indic, "_v", version, "_calc_consult.r"))))
      get_algo(indic = indic, version = version, dir_algo = dir_algo, uncompress = TRUE)

    files <- map_chr(files, .f = function(x) x$path)

    run_local(indic = indic, version = version, files = files, dir_algo = dir_algo)
  }

}