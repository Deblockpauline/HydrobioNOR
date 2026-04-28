#' carte UI Function
#'
#' @description Module Shiny permettant d'afficher une carte interactive des stations de suivi
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
#' @export

mod_station_carte_ui <- function(id, hauteur = "700px") { # Hauteur de la carte
  ns <- shiny::NS(id)
  shiny::tagList( # Sert à regrouper plusieurs éléments

    # Barre de recherche pour retrouver une station par son nom
    shiny::selectizeInput(
      inputId = ns("recherche_station"), # ID
      label = "Rechercher une station", # Texte affiché
      choices = NULL,
      selected = NULL,
      multiple = FALSE, # 1 Station a la fois
      options = list(
        placeholder = "Tapez le nom ou le code d'une station", # Liste déroulante
        maxOptions = 10000) ), # Afin de voir toutes les stations

    # Affichage de la carte leaflet
    leaflet::leafletOutput(ns("carte_stations"), height = hauteur) ) }

#' Carte server Function
#' @description Module shiny qui permet d'afficher les données sur la carte
#'
#' @param choix_departements Reactive contenant les départements sélectionnés
#' @param choix_eqb Reactive contenant les EQB sélectionnés
#' @param choix_uh Reactive contenant les UH sélectionnées
#'
#' @return Reactive du code de la station sélectionnée
#' @noRd

mod_station_carte_server <- function(id,
                                     donnees,
                                     choix_departements, # Reactive contenant les départements sélectionnés
                                     choix_eqb,          # Reactive contenant les EQB sélectionnés
                                     choix_uh) {         # Reactive contenant les UH sélectionnées
  shiny::moduleServer(id, function(input, output, session) { # Structure standard

    # Données utilisées pour afficher les stations sur la carte
    stations_filtrees <- shiny::reactive({ # Objet réactif recalculé dès qu'une donnée change
      shiny::req(donnees()) # On continue seulement si les données existent
      shiny::req(donnees()$donnee_carte) # Vérifie la présence de la table des stations
      shiny::req(donnees()$donnee_carte_taxon) # Vérifie la présence de la table utilisée pour le filtre EQB

      df <- donnees()$donnee_carte # Table principale des stations affichées sur la carte
      df_taxon <- donnees()$donnee_carte_taxon # Table annexe pour les indices

      shiny::req(inherits(df, "sf")) # Vérifie que la table principale est bien un objet spatial sf
      shiny::req(nrow(df) > 0) # Vérifie qu'elle n'est pas vide

      # Application du filtre département
      if (!is.null(choix_departements()) && # && = ET logique, les conditions doivent être vraies en même temps
          length(choix_departements()) > 0 && # Si un département est sélectionné
          !("Tous" %in% choix_departements())) { # et que ce n'est pas "Tous", alors on filtre
        df <- dplyr::filter(  # On garde uniquement les stations des départements sélectionnés
          df,
          code_dep %in% choix_departements() ) }

      # Application du filtre EQB
      if (!is.null(choix_eqb()) &&
          length(choix_eqb()) > 0 &&
          !("Tous" %in% choix_eqb())) {
        stations_eqb <- df_taxon |>
          sf::st_drop_geometry() |> # Supprime la géométrie pour travailler comme sur une table classique
          dplyr::filter(eqb %in% choix_eqb()) |> # Garde les lignes correspondant aux EQB sélectionnés
          dplyr::distinct(code_station) |> # Garde une seule fois chaque station
          dplyr::pull(code_station) # Extrait uniquement les codes station
        df <- dplyr::filter(  # On garde uniquement les stations ayant l'EQB sélectionné
          df,
          code_station %in% stations_eqb ) }

      # Application du filtre UH
      if (!is.null(choix_uh()) &&
          length(choix_uh()) > 0 &&
          !("Toutes" %in% choix_uh()) &&
          "UH_calculee" %in% names(df)) {
        df <- dplyr::filter( # On garde uniquement les stations appartenant aux UH sélectionnées
          df,
          UH_calculee %in% choix_uh()) }

      shiny::req(nrow(df) > 0) # Vérifie qu'il reste au moins une station après filtrage
      sf::st_transform(df, 4326) # Conversion en WGS84 (EPSG:4326), obligatoire
    } )

    # Limites régionales affichées sur la carte
    # Données préparées dans le script de préparation des données de référence
    limites_region_carte <- shiny::reactive({
      sf::st_transform(limites_region_l, 4326) } )  # Conversion en WGS84 pour leaflet

    # Couche des cours d'eau affichée sur la carte
    limites_cours_eau_carte <- shiny::reactive({
      sf::st_transform(limites_cours_eau, 4326) } ) # Conversion en WGS84 pour leaflet

    # Couche des bassins versants affichée sur la carte
    limites_bv_carte <- shiny::reactive({
      sf::st_transform(limites_bv_l, 4326) } ) # Conversion en WGS84 pour leaflet

    # Mise à jour de la liste déroulante selon les stations actuellement filtrées
    shiny::observe({ # Bloc réactif qui se réexécute dès qu'on change le choix
      df <- stations_filtrees() # Récupère les stations filtrées
      df_choix <- df |>
        sf::st_drop_geometry() |> # Supprime la géométrie
        dplyr::distinct(code_station, libelle_station) |> # Évite les doublons dans la liste
        dplyr::arrange(libelle_station) # Trie les stations par ordre alphabétique

      choix <- stats::setNames( # Crée les choix du menu sous la forme "Nom station (code)"
        object = df_choix$code_station, # Shiny récupère seulement le code
        nm = paste0(df_choix$libelle_station, " (", df_choix$code_station, ")") ) # Affichage de Nom station (code)

      # Met à jour la liste de recherche quand les filtres sont appliqués
      shiny::updateSelectizeInput(
        session = session,
        inputId = "recherche_station",
        choices = choix,
        selected = isolate(input$recherche_station), # Conserve la valeur actuelle si possible
        server = TRUE )
    } )

    # Création initiale de la carte
    output$carte_stations <- leaflet::renderLeaflet({ # Carte interactive
      df <- stations_filtrees() # Stations filtrées à afficher
      limites_region <- limites_region_carte() # Limites administratives
      cours_eau <- limites_cours_eau_carte() # Cours d'eau
      bassins_versants <- limites_bv_carte() # Bassins versants
      coords <- sf::st_coordinates(df) # Extraction des coordonnées des stations

      # Calcul du centre moyen pour centrer la carte au chargement
      centre_lng <- mean(coords[, 1], na.rm = TRUE)
      centre_lat <- mean(coords[, 2], na.rm = TRUE)

      leaflet::leaflet(data = df) %>% # Initialise la carte avec les stations
        leaflet::addTiles() %>% # Ajoute le fond de carte OpenStreetMap
        leaflet::addMapPane("hydro", zIndex = 405) %>% # Plan dédié aux cours d'eau
        leaflet::addMapPane("bv", zIndex = 408) %>% # Plan dédié aux bassins versants
        leaflet::addMapPane("limites", zIndex = 410) %>% # Plan dédié aux limites régionales
        leaflet::addMapPane("points", zIndex = 420) %>% # Plan dédié aux stations, affiché au-dessus
        leaflet::setView(
          lng = centre_lng, # Centre initial de la carte
          lat = centre_lat,
          zoom = 8 ) %>%
        leaflet::addPolylines( # Ajout des cours d'eau
          data = cours_eau,
          color = "#2C7FB8",
          opacity = 0.7,
          weight = 1,
          group = "Cours d'eau",
          options = leaflet::pathOptions(pane = "hydro") ) %>%
        leaflet::addPolylines( # Ajout des limites des bassins versants
          data = bassins_versants,
          color = "red",
          opacity = 0.8,
          weight = 1.2,
          group = "Bassins versants",
          options = leaflet::pathOptions(pane = "bv") ) %>%
        leaflet::addPolylines( # Ajout des limites régionales
          data = limites_region,
          color = "black",
          opacity = 1,
          weight = 2,
          group = "Limites administratives",
          options = leaflet::pathOptions(pane = "limites") ) %>%
        leaflet::addCircleMarkers( # Ajout des points représentant les stations
          radius = 6, # Taille des points
          stroke = TRUE, # Affiche un contour
          color = "black", # Couleur du contour
          weight = 1,
          fillColor = "#D9D9D9", # Couleur intérieure du point
          fillOpacity = 1,
          layerId = ~code_station, # Identifiant du point = code station
          label = ~libelle_station, # Étiquette au survol
          options = leaflet::pathOptions(pane = "points"),
          popup = ~paste0( # Petite etiquette
            "<b>", libelle_station, "</b><br/>", # <b> = texte en gras
            "Code station : ", code_station, "<br/>", # <br/> = retour à la ligne
            "Cours d'eau : ", libelle_cours_eau, "<br/>",
            "Département : ", code_dep ) ) %>%
        leaflet::addLayersControl( # Panneau pour afficher ou masquer les couches de fond
          overlayGroups = c("Limites administratives", "Cours d'eau", "Bassins versants"),
          options = leaflet::layersControlOptions(collapsed = FALSE) ) %>%
        leaflet::hideGroup("Bassins versants") # Masque les bassins versants au démarrage pour alléger la carte
    } )

    # Mise à jour des points affichés quand les filtres changent
    shiny::observe({ # Mise à jour automatique de la carte
      df <- stations_filtrees()
      coords <- sf::st_coordinates(df)

      # Recalcul du centre de la carte selon les stations restantes
      centre_lng <- mean(coords[, 1], na.rm = TRUE)
      centre_lat <- mean(coords[, 2], na.rm = TRUE)

      leaflet::leafletProxy("carte_stations", session = session) %>% # Modifie la carte existante sans la recréer
        leaflet::clearMarkers() %>% # Supprime les anciens points
        leaflet::setView( # Recentre la carte
          lng = centre_lng,
          lat = centre_lat,
          zoom = 8) %>%
        leaflet::addCircleMarkers( # Réaffiche les nouvelles stations filtrées
          data = df,
          radius = 6,
          stroke = TRUE,
          color = "black",
          weight = 1,
          fillColor = "#D9D9D9",
          fillOpacity = 1,
          layerId = ~code_station,
          label = ~libelle_station,
          options = leaflet::pathOptions(pane = "points"),
          popup = ~paste0( # Petite etiquette
            "<b>", libelle_station, "</b><br/>",
            "Code station : ", code_station, "<br/>",
            "Cours d'eau : ", libelle_cours_eau, "<br/>",
            "Département : ", code_dep ) )
    } )

    # Stocke la station sélectionnée par l'utilisateur
    station_selectionnee <- shiny::reactiveVal(NULL)

    # Sélection d'une station via la liste déroulante
    shiny::observeEvent(input$recherche_station, { # S'active seulement au clic dans la liste
      shiny::req(input$recherche_station) # Vérifie qu'une valeur a bien été choisie
      station_selectionnee(input$recherche_station)}, # Met à jour la station sélectionnée
      ignoreNULL = TRUE)

    # Sélection d'une station via un clic sur un point de la carte
    shiny::observeEvent(input$carte_stations_marker_click, { # Déclenché au clic sur une station
      clic <- input$carte_stations_marker_click # Stocke les infos du clic
      shiny::req(clic$id) # Vérifie que l'identifiant du point existe
      station_selectionnee(clic$id) # Met à jour la station sélectionnée avec le code station cliqué

      # Mise à jour de la barre de recherche pour synchroniser la sélection
      shiny::freezeReactiveValue(input, "recherche_station") # Fige la barre après sélection
      shiny::updateSelectizeInput( # En affichant le nom de la station sélectionnée
        session = session,
        inputId = "recherche_station",
        selected = clic$id ) }, ignoreNULL = TRUE)

    # Recentrage de la carte et ouverture du popup sur la station sélectionnée
    shiny::observeEvent(station_selectionnee(), { # Déclenché quand la station sélectionnée change
      code_station_sel <- station_selectionnee() # Récupère le code station sélectionné
      shiny::req(code_station_sel)
      df <- stations_filtrees() # Récupère les stations actuellement filtrées
      station_choisie <- df |> # Recherche la station correspondant au code sélectionné
        dplyr::filter(code_station == code_station_sel)
      shiny::req(nrow(station_choisie) == 1) # Vérifie qu'une seule station correspond

      coords <- sf::st_coordinates(station_choisie) # Récupère ses coordonnées

      leaflet::leafletProxy("carte_stations", session = session) %>%
        leaflet::setView( # Centre la carte sur la station choisie
          lng = coords[1, 1],
          lat = coords[1, 2],
          zoom = 12 ) %>%
        leaflet::clearPopups() %>% # Ferme les anciens popups ouverts
        leaflet::addPopups( # Ouvre un popup sur la station sélectionnée
          lng = coords[1, 1],
          lat = coords[1, 2],
          popup = paste0( # Petite etiquette
            "<b>", station_choisie$libelle_station, "</b><br/>",
            "Code station : ", station_choisie$code_station, "<br/>",
            "Cours d'eau : ", station_choisie$libelle_cours_eau, "<br/>",
            "Département : ", station_choisie$code_dep ) )
    }, ignoreNULL = TRUE) # Ne se déclenche pas au démarrage

    # Le module renvoie le code de la station actuellement sélectionnée
    return(shiny::reactive(station_selectionnee()))
  } ) }

## À appeler dans l'UI
# mod_station_carte_ui("station_carte")

## À appeler dans le server
# mod_station_carte_server("station_carte")
