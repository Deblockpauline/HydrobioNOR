#' carte UI Function
#' @description Module Shiny permettant d'afficher la carte interactive des stations
#' @noRd

mod_station_carte_ui <- function(id, hauteur = "700px") { # Hauteur de la carte
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Sert à regrouper plusieurs éléments
    shiny::selectizeInput( # Barre de recherche pour retrouver une station par son nom
      inputId = ns("recherche_station"), # ID
      label = "Rechercher une station", # Texte affiché
      choices = NULL, # Choix ajoutés côté serveur
      selected = NULL, # Aucune station au départ
      multiple = FALSE, # 1 Station a la fois
      options = list(
        placeholder = "Tapez le nom ou le code d'une station", # Liste déroulante
        maxOptions = 10000) ), # Afin de voir toutes les stations

    leaflet::leafletOutput(
      outputId = ns("carte_stations"), # ID de la carte
      height = hauteur) ) } # Affichage de la carte leaflet


#' Carte server Function
#' @description Module shiny qui permet d'afficher les données sur la carte
#' @return Reactive du code de la station sélectionnée
#' @noRd

mod_station_carte_server <- function(id, # Recoit les données et les choix
                                     donnees,
                                     choix_departements,
                                     choix_eqb,
                                     choix_uh,
                                     choix_reseau,
                                     choix_qualification = NULL) {

  shiny::moduleServer(id, function(input, output, session) { # Structure standard

# Stocke la station sélectionnée par l'utilisateur
    station_selectionnee <- shiny::reactiveVal(NULL) # Aucune station au démarrage

# Données utilisées pour afficher les stations sur la carte
    stations_filtrees <- shiny::reactive({ # Recalculé dès qu'une donnée change
      shiny::req(donnees()) # On continue seulement si les données existent
      shiny::req(donnees()$donnee_carte) # Vérifie la présence de la table donnees_carte
      shiny::req(donnees()$donnee_carte_taxon) # Vérifie la présence de la table donnees_carte_taxon

      df <- donnees()$donnee_carte # Table principale des stations affichées sur la carte
      df_taxon <- donnees()$donnee_carte_taxon # Table annexe pour les taxons
      df_etat <- donnees()$etat_bio # Table annexe pour les indices


# Applocation des filtres

      # Département
      if (!is.null(choix_departements()) && # Filtre existe
          length(choix_departements()) > 0 && # Au moins un choix
          !("Tous" %in% choix_departements())) { # Pas tous
        df <- dplyr::filter( # Garde seulement les départements choisis
          df,
          code_dep %in% choix_departements() ) }

      # EQB
      if (!is.null(choix_eqb()) && # Filtre existe
          length(choix_eqb()) > 0 && # Au moins un choix
          !("Tous" %in% choix_eqb())) { # Pas tous
        stations_eqb <- df_taxon |>
          sf::st_drop_geometry() |> # Supprime la géométrie
          dplyr::filter(eqb %in% choix_eqb()) |> # Garde EQB choisi
          dplyr::distinct(code_station) |> # Stations uniques
          dplyr::pull(code_station) # Codes stations
        df <- dplyr::filter( # Garde les stations avec EQB choisi
          df,
          code_station %in% stations_eqb) }

      # Qualification
      if (!is.null(choix_qualification) && # Filtre existe
          !is.null(choix_qualification()) && # Valeur existe
          length(choix_qualification()) > 0 && # Au moins un choix
          !("Toutes" %in% choix_qualification())) { # Pas toutes

        stations_qualification_etat <- df_etat |> # Table état bio
          dplyr::filter(
            libelle_qualification %in% choix_qualification()) |> # Qualification choisie
          dplyr::distinct(code_station) |> # Stations uniques
          dplyr::pull(code_station) # Codes stations
        stations_qualification_taxon <- df_taxon |> # Table taxons
          sf::st_drop_geometry() |> # Supprime géométrie
          dplyr::filter(
            libelle_qualification %in% choix_qualification()) |> # Qualification choisie
          dplyr::distinct(code_station) |> # Stations uniques
          dplyr::pull(code_station) # Codes stations
        stations_qualification <- unique(c( # Fusion sans doublons
          stations_qualification_etat,
          stations_qualification_taxon))

        df <- dplyr::filter( # Garde les stations compatibles
          df,
          code_station %in% stations_qualification) }

      # UH
      if (!is.null(choix_uh()) && # Filtre existe
          length(choix_uh()) > 0 && # Au moins un choix
          !("Toutes" %in% choix_uh()) && # Pas toutes
          "UH_calculee" %in% names(df)) { # Colonne existe
        df <- dplyr::filter( # Garde les UH choisies
          df,
          UH_calculee %in% choix_uh())  }

      # Réseau
      if (!is.null(choix_reseau()) && # Filtre existe
          length(choix_reseau()) > 0 && # Au moins un choix
          !("Tous" %in% choix_reseau()) && # Pas tous
          "reseau" %in% names(df)) { # Colonne existe
        df <- dplyr::filter( # Garde les réseaux choisis
          df,
          stringr::str_detect(
            reseau,
            paste(choix_reseau(), collapse = "|") ) ) }

# Message si rien
       shiny::validate(
        shiny::need(
          nrow(df) > 0,
          "Aucune donnée disponible pour cette combinaison de filtres."))
      sf::st_transform(df, 4326) } ) # Conversion en WGS84


# Création de la carte

    # Couches préparées dans le script de préparation des données de référence
    limites_region_carte <- shiny::reactive({ # Limite des régions
      sf::st_transform(limites_region_l, 4326) } ) # Conversion WGS84
    limites_cours_eau_carte <- shiny::reactive({ # Cours d'eau
      sf::st_transform(limites_cours_eau, 4326) } )
    limites_bv_carte <- shiny::reactive({ # Bassins versants
      sf::st_transform(limites_bv_l, 4326) } )

    # Création initiale de la carte
    output$carte_stations <- leaflet::renderLeaflet({ # Carte interactive
      limites_region <- limites_region_carte() # Limites administratives
      cours_eau <- limites_cours_eau_carte() # Cours d'eau
      bassins_versants <- limites_bv_carte() # Bassins versants

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          preferCanvas = TRUE)) |> # Affichage plus léger
        leaflet::addTiles() |> # Ajoute le fond de carte OpenStreetMap
        leaflet::addMapPane("hydro", zIndex = 405) |> # Plan dédié aux cours d'eau
        leaflet::addMapPane("bv", zIndex = 408) |> # Plan dédié aux bassins versants
        leaflet::addMapPane("limites", zIndex = 410) |> # Plan dédié aux limites régionales
        leaflet::addMapPane("points", zIndex = 420) |> # Plan dédié aux stations
        leaflet::setView(
          lng = 0.2, # Centre général Normandie
          lat = 49.1, # Centre général Normandie
          zoom = 8) |> # Zoom régional
        leaflet::addPolylines(
          data = cours_eau, # Données cours d'eau
          color = "#2C7FB8", # Couleur bleue
          opacity = 0.7, # Transparence
          weight = 1, # Épaisseur
          group = "Cours d'eau", # Groupe
          options = leaflet::pathOptions(pane = "hydro")) |> # Plan hydro
        leaflet::addPolylines(
          data = bassins_versants, # Données bassins
          color = "red", # Couleur rouge
          opacity = 0.8, # Transparence
          weight = 1.2, # Épaisseur
          group = "Bassins versants", # Groupe
          options = leaflet::pathOptions(pane = "bv")) |> # Plan BV
        leaflet::addPolylines(
          data = limites_region, # Données limites
          color = "black", # Couleur noire
          opacity = 1, # Pas de transparence
          weight = 2, # Épaisseur
          group = "Limites administratives", # Groupe
          options = leaflet::pathOptions(pane = "limites")) |> # Plan limites
        leaflet::addLayersControl(
          overlayGroups = c(
            "Limites administratives",
            "Cours d'eau",
            "Bassins versants"),
          options = leaflet::layersControlOptions(collapsed = FALSE)) |> # Contrôle ouvert
        leaflet::hideGroup("Bassins versants") } )# Masque les bassins au démarrage

    shiny::outputOptions( # Empêche la suspension de la carte lorsqu'elle est cachée
      output,  # (ex. changement d'onglet) afin qu'elle continue à se mettre à jour
      "carte_stations",
      suspendWhenHidden = FALSE)

# Mise à jour de la liste déroulante selon les stations actuellement filtrées
    shiny::observe({ # Mise à jour automatique de la liste
      df <- stations_filtrees() # Récupère les stations filtrées
      df_choix <- df |>
        sf::st_drop_geometry() |> # Supprime la géométrie
        dplyr::distinct(code_station, libelle_station) |> # Stations uniques
        dplyr::arrange(libelle_station) # Tri alphabétique
      choix <- stats::setNames( # Format nom affiché / valeur renvoyée
        object = df_choix$code_station, # Valeur = code station
        nm = paste0(df_choix$libelle_station, " (", df_choix$code_station, ")")) # Affichage
      selected_station <- station_selectionnee() # Station déjà sélectionnée

      if (is.null(selected_station) || # Aucune station
          length(selected_station) == 0 || # Sélection vide
          !selected_station %in% df_choix$code_station) { # Station plus disponible
        selected_station <- character(0) # Aucune sélection dans la liste
        station_selectionnee(NULL) } # Aucune station sélectionné

      shiny::updateSelectizeInput(
        session = session,
        inputId = "recherche_station",
        choices = choix,
        selected = selected_station, # Vide au démarrage
        server = TRUE) } )

# Mise à jour des points affichés quand les filtres changent
    shiny::observe({ # Mise à jour automatique des points
      df <- stations_filtrees() # Récupère les stations filtrées

      coords <- sf::st_coordinates(df) # Récupère les coordonnées
      centre_lng <- mean(coords[, 1], na.rm = TRUE) # Centre longitude
      centre_lat <- mean(coords[, 2], na.rm = TRUE) # Centre latitude

      station_sel <- station_selectionnee() # Station sélectionnée
      station_choisie <- df[0, ] # Table vide par défaut

      if (!is.null(station_sel) && # Une station existe
          length(station_sel) > 0 && # Sélection non vide
          station_sel %in% df$code_station) { # Station visible
        station_choisie <- df |>
          dplyr::filter(code_station == station_sel) } # Station choisie
      proxy <- leaflet::leafletProxy("carte_stations", session = session) |> # Modifie la carte existante
        leaflet::clearGroup("stations") |> # Supprime seulement les points gris
        leaflet::clearGroup("station_selectionnee") |> # Supprime seulement le point bleu
        leaflet::clearPopups() |> # Ferme les popups
        leaflet::addCircleMarkers(
          data = df, # Données stations
          group = "stations", # Groupe des points gris
          radius = 6, # Taille des points
          stroke = TRUE, # Contour
          color = "black", # Couleur contour
          weight = 1, # Épaisseur contour
          fillColor = "#D9D9D9", # Couleur intérieure
          fillOpacity = 1, # Opacité
          layerId = ~code_station, # ID station
          label = ~libelle_station, # Survol
          options = leaflet::pathOptions(pane = "points"), # Plan points
          popup = ~paste0(
            "<b>", libelle_station, "</b><br/>",
            "Code station : ", code_station, "<br/>",
            "Cours d'eau : ", libelle_cours_eau, "<br/>",
            "Département : ", code_dep))

      if (nrow(station_choisie) == 1) { # Si une station est sélectionnée
        coords_sel <- sf::st_coordinates(station_choisie) # Coordonnées station
        proxy |>
          leaflet::addCircleMarkers(
            data = station_choisie, # Station sélectionnée
            group = "station_selectionnee", # Groupe du point bleu
            radius = 9, # Point plus gros
            stroke = TRUE, # Contour
            color = "black", # Couleur contour
            weight = 2, # Contour plus épais
            fillColor = "blue", # Couleur station sélectionnée
            fillOpacity = 1, # Opacité
            layerId = ~code_station, # ID station
            label = ~libelle_station, # Survol
            options = leaflet::pathOptions(pane = "points"), # Plan points
            popup = ~paste0(
              "<b>", libelle_station, "</b><br/>",
              "Code station : ", code_station, "<br/>",
              "Cours d'eau : ", libelle_cours_eau, "<br/>",
              "Département : ", code_dep)) |>
          leaflet::addPopups(
            lng = coords_sel[1, 1],
            lat = coords_sel[1, 2],
            popup = paste0(
              "<b>", station_choisie$libelle_station, "</b><br/>",
              "Code station : ", station_choisie$code_station, "<br/>",
              "Cours d'eau : ", station_choisie$libelle_cours_eau, "<br/>",
              "Département : ", station_choisie$code_dep))
      } else { # Si aucune station sélectionnée
        proxy |>
          leaflet::setView(
            lng = centre_lng, # Centre sur les stations filtrées
            lat = centre_lat,
            zoom = 8) } } )

    # Sélection d'une station via la liste déroulante
    shiny::observeEvent(input$recherche_station, { # S'active seulement au clic dans la liste
      shiny::req(input$recherche_station) # Vérifie qu'une valeur a bien été choisie
      station_selectionnee(input$recherche_station) # Met à jour la station sélectionnée
    }, ignoreNULL = TRUE)

    # Sélection d'une station via un clic sur un point de la carte
    shiny::observeEvent(input$carte_stations_marker_click, { # Déclenché au clic sur une station
      clic <- input$carte_stations_marker_click # Stocke les infos du clic
      shiny::req(clic$id) # Vérifie que l'identifiant du point existe
      station_selectionnee(clic$id) # Met à jour la station sélectionnée
      shiny::freezeReactiveValue(input, "recherche_station") # Fige la barre après sélection
      shiny::updateSelectizeInput(
        session = session,
        inputId = "recherche_station",
        selected = clic$id) # Synchronise la barre de recherche
    }, ignoreNULL = TRUE)

    # Recentrage de la carte et ouverture du popup sur la station sélectionnée
    shiny::observeEvent(station_selectionnee(), { # Déclenché quand la station sélectionnée change
      code_station_sel <- station_selectionnee() # Récupère le code station sélectionné
      shiny::req(code_station_sel) # Vérifie qu'une station est sélectionnée
      df <- stations_filtrees() # Récupère les stations actuellement filtrées
      station_choisie <- df |>
        dplyr::filter(code_station == code_station_sel) # Garde uniquement la station choisie
      shiny::req(nrow(station_choisie) == 1) # Vérifie qu'une seule station correspond

      coords <- sf::st_coordinates(station_choisie) # Récupère ses coordonnées

      leaflet::leafletProxy("carte_stations", session = session) |> # Modifie la carte existante
        leaflet::clearGroup("station_selectionnee") |> # Supprime seulement l'ancien point bleu
        leaflet::clearPopups() |> # Ferme les anciens popups
        leaflet::addCircleMarkers(
          data = station_choisie, # Station choisie
          group = "station_selectionnee", # Groupe du point bleu
          radius = 9, # Point plus gros
          stroke = TRUE, # Contour
          color = "black", # Couleur contour
          weight = 2, # Contour plus épais
          fillColor = "blue", # Couleur station sélectionnée
          fillOpacity = 1, # Opacité
          layerId = ~code_station, # Identifiant du point
          label = ~libelle_station, # Étiquette au survol
          options = leaflet::pathOptions(pane = "points"), # Affiche au-dessus
          popup = ~paste0(
            "<b>", libelle_station, "</b><br/>",
            "Code station : ", code_station, "<br/>",
            "Cours d'eau : ", libelle_cours_eau, "<br/>",
            "Département : ", code_dep)) |>
        leaflet::setView(
          lng = coords[1, 1],
          lat = coords[1, 2],
          zoom = 12) |>
        leaflet::addPopups(
          lng = coords[1, 1],
          lat = coords[1, 2],
          popup = paste0(
            "<b>", station_choisie$libelle_station, "</b><br/>",
            "Code station : ", station_choisie$code_station, "<br/>",
            "Cours d'eau : ", station_choisie$libelle_cours_eau, "<br/>",
            "Département : ", station_choisie$code_dep))
    }, ignoreNULL = TRUE)

    return(shiny::reactive(station_selectionnee())) # Renvoie le code de la station sélectionnée
  } ) }

## À appeler dans l'UI
# mod_station_carte_ui("station_carte")

## À appeler dans le server
# mod_station_carte_server("station_carte")
