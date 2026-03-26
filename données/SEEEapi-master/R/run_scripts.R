#' @importFrom dplyr '%>%'
#' @importFrom httr POST
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#' @importFrom readr read_delim write_delim cols
#' @importFrom stringr str_match

run_distant <- function(indic, version, files) {
  body <- list(indicateur = indic,
               version    = version)
  body <- c(body, files)

  POST(url    = "http://seee.eaufrance.fr/api/calcul/",
       body   = body,
       encode = "multipart") %>%
    read_html()              %>%
    html_text()              %>%
    (function(x) {
      if (grepl("Calcul non réalisé", x)) {
        x
      } else {
        if (grepl("asynchrone", x)) {
          id_calcul <- str_match(string  = x,
                                 pattern = "\\{.*id_calcul\\\":\\\"(.*)\\\"\\}")[,2]

          cat("\nCalcul asynchrone\n")

          res <- "en cours"

          while (grepl(pattern = "en cours", x = res)) {
            Sys.sleep(5)

            res <- POST(url  = "http://seee.eaufrance.fr/api/resultat/",
                        body = list(id_calcul = id_calcul)) %>%
              read_html()                                   %>%
              html_text()

            cat(".")
            flush.console()
          }

          if (grepl("erreur", res)) {
            res
          } else {
            list(info = strsplit(res, split = "\n")[[1]][1] %>%
                   gsub(pattern = ";", replacement = " "),
                 result =  read_delim(res, delim = ";", skip = 1,
                                      col_type = cols(.default = "c")))

          }

        } else {
          list(info = strsplit(x, split = "\n")[[1]][1] %>%
                 gsub(pattern = ";", replacement = " "),
               result =  read_delim(x, delim = ";", skip = 1,
                                    col_type = cols(.default = "c")))

        }
      }
    })
}

run_local <- function(indic, version, files, dir_algo) {
  complementaire = FALSE #not handled for now

  script <- file.path(dir_algo, indic, version,
                      paste0(indic, "_v", version, "_calc_consult.r"))

  file.copy(from = files, to = file.path(dir_algo, indic, version))

  inputs <- basename(files)

  script_content <- readLines(con = script)

  start_input <- which(grepl(x = script_content,
                             pattern = "options\\(warn"))

  end_input <- which(grepl(x = script_content,
                           pattern = "# *Initialisation de l'heure"))

  script_inputs <- script_content[start_input:end_input] %>%
    strsplit(split = " <- ") %>%
    (function(x) x[sapply(x, length) > 1 & !grepl(x, pattern = "complementaire")])

  if (length(inputs) < length(script_inputs))
    stop("Missing input value: required ", length(script_inputs), " got ", length(inputs))

  for (i in seq(length(script_inputs))) {
      script_inputs[[i]] <- paste0(script_inputs[[i]][1], " <- \"", inputs[i], "\"")
  }

  script_inputs <- c(script_inputs, paste0("complementaire <- ", complementaire)) %>%
    unlist()

  script_temp <- tempfile(fileext = ".R",
                          tmpdir = file.path(dir_algo, indic, version))

  writeLines(c(script_content[seq(start_input)],
               script_inputs,
               script_content[end_input:length(script_content)]),
             con = script_temp)

  consoleOutput <- tempfile()

  sink(file = consoleOutput)
  source(file = script_temp, chdir = TRUE, echo = FALSE)
  sink(file = NULL)

  res <- readLines(file.path(dir_algo, indic, version,
                             paste0(indic, "_v", version, "_resultats.csv")))

  file.remove(c(script_temp,
              file.path(dir_algo, indic, version, inputs),
              consoleOutput))

  list(info = strsplit(res, split = "\n")[[1]][1] %>%
         gsub(pattern = ";", replacement = " "),
       result =  read_delim(
         file.path(dir_algo, indic, version,
                   paste0(indic, "_v", version, "_resultats.csv")),
         delim = ";", skip = 1,
         col_type = cols(.default = "c")))
}
