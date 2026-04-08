#' Module UI des informations station
#'
#' @param id Identifiant du module
#'
#' @noRd

mod_station_infos_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList( # Emplacement où seront affichées les informations de la station
    shiny::uiOutput(ns("infos_station") ) ) }

#' Module server des informations station
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données de l'application
#' @param station_selectionnee Reactive contenant le code station sélectionné
#'
#' @noRd

mod_station_infos_server <- function(id, donnees, station_selectionnee) {
  shiny::moduleServer(id, function(input, output, session) {
    output$infos_station <- shiny::renderUI ( { # Génère dynamiquement l'UI des infos station
      shiny::req(donnees()) # Vérifie que les données sont disponibles
      code_station_sel <- station_selectionnee() # Récupère le code de la station sélectionnée

      # Si aucune station n'est sélectionnée = message par défaut au debut de l'appli
      if (is.null(code_station_sel) || is.na(code_station_sel) || code_station_sel == "") {
        return(
          shiny::tagList(
            shiny::h4("Informations générales"),
            shiny::p("Cliquez sur une station de la carte pour afficher ses informations.") ) ) }

      stations <- NULL # Initialisation de la table des stations

      # On récupère la bonne table contenant les informations station
      if ("stations" %in% names(donnees())) {
        stations <- donnees()$stations
      } else if ("donnee_carte" %in% names(donnees())) {
        stations <- donnees()$donnee_carte
      } else if ("donnees_carte" %in% names(donnees())) {
        stations <- donnees()$donnees_carte }
      shiny::req(stations) # Vérifie qu'une table a bien été trouvée

      # Filtre la ligne correspondant à la station sélectionnée
      station <- stations[stations$code_station == code_station_sel, , drop = FALSE]

      # Si aucune station trouvée :
      if (nrow(station) == 0) {
        return(
          shiny::tagList(
            shiny::h4("Informations générales"),
            shiny::p("Aucune information trouvée pour cette station.") ) ) }

      # Si plusieurs lignes → on garde seulement la première
      station <- station[1, , drop = FALSE]

      # Fonction utilitaire :
      # récupère une valeur de colonne si elle existe, sinon retourne "Non renseigné"
      valeur_si_existe <- function(nom_col) {
        if (nom_col %in% names(station)) {
          valeur <- as.character(station[[nom_col]][1]) # Extraction de la valeur
          # Gestion des NA ou valeurs vides
          if (is.na(valeur) || valeur == "") { return("Non renseigné") }
          return(valeur) } else { return("Non renseigné") } }

      # Récupération du nom et de l'URL de la station
      nom_station <- valeur_si_existe("libelle_station")
      uri_station <- valeur_si_existe("uri_station")

      # Si une URL existe → le nom devient cliquable
      nom_station_affichage <- if (!identical(uri_station, "Non renseigné")) {
        shiny::tags$a(
          href = uri_station, # Lien vers EauFrance
          target = "_blank", # Ouvre dans un nouvel onglet
          rel = "noopener noreferrer",
          nom_station )
      } else { nom_station } # Pas de lien

      # Affichage des informations générales de la station
      shiny::tagList(
        shiny::h4("Informations générales"),
        shiny::tags$ul( # Liste des informations

          shiny::tags$li(
            shiny::strong("Nom de la station : "),
            nom_station_affichage ),

          shiny::tags$li(
            shiny::strong("Code station : "),
            valeur_si_existe("code_station") ),

          shiny::tags$li(
            shiny::strong("Cours d'eau : "),
            valeur_si_existe("libelle_cours_eau") ),

          shiny::tags$li(
            shiny::strong("Code masse d'eau : "),
            valeur_si_existe("code_masse_eau") ),

          shiny::tags$li(
            shiny::strong("Libellé masse d'eau : "),
            valeur_si_existe("libelle_masse_eau") ),

          shiny::tags$li(
            shiny::strong("Département : "),
            valeur_si_existe("code_dep") ),

          shiny::tags$li(
            shiny::strong("Coordonnée X : "),
            valeur_si_existe("coordonnee_x") ),

          shiny::tags$li(
            shiny::strong("Coordonnée Y : "),
            valeur_si_existe("coordonnee_y") ),

          shiny::tags$li(
            shiny::strong("Date du premier prélèvement : "),
            valeur_si_existe("date_premier_prelevement") ),

          shiny::tags$li(
            shiny::strong("Date du dernier prélèvement : "),
            valeur_si_existe("date_dernier_prelevement") ),

          shiny::tags$li(
            shiny::strong("Typologie : "),
            valeur_si_existe("typologie") ),

          shiny::tags$li(
            shiny::strong("Altitude : "),
            valeur_si_existe("altitude") ),

          shiny::tags$li(
            shiny::strong("Rang de strahler : "),
            valeur_si_existe("StrahlMax") )
        ) ) } )
  } ) }
