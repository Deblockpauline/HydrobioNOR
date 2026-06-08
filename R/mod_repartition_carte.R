#' Carte de répartition des taxons UI
#'
#' @description Module Shiny permettant d'afficher la répartition des taxons
#' Inspiration de mod_station_carte
#' @noRd

mod_repartition_carte_ui <- function(id, hauteur = "700px") { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments UI

    shiny::selectizeInput( # Liste déroulanter
      inputId = ns("taxon_selectionne"), # ID du ou des taxons choisis
      label = "Choisir un ou plusieurs taxons", # Texte affiché au-dessus
      choices = NULL, # Choix ajoutés côté serveur
      selected = NULL, # Aucun taxon au départ
      multiple = TRUE, # Plusieurs taxons à la fois
      options = list( # Options de la liste
        placeholder = "Choisir le nom d'un ou plusieurs taxons", # Texte par défaut
        maxOptions = 10000 ) ), # Affiche beaucoup de taxons

    shiny::br(), # Espace
    leaflet::leafletOutput( # Carte
      outputId = ns("carte_repartition"), # ID de la carte
      height = hauteur ), # Hauteur de la carte
    shiny::br(), # Espace
    plotly::plotlyOutput( # Graphique
      outputId = ns("plot_repartition_taxon"), # ID du graphique
      height = "350px" ), # Hauteur du graphique
    shiny::br(), # Espace
    shiny::h4("Tableau des prélèvements"), # Titre
    shiny::downloadButton( # Bouton export CSV
      outputId = ns("download_repartition_taxon"), # ID bouton
      label = "Télécharger les données (.csv)" ), # Texte bouton
    shiny::br(), # Espace
    DT::DTOutput(
      outputId = ns("table_repartition_taxon") ) ) # Tableau des données
}

#' Carte de répartition des taxons server
#'
#' @description Module Shiny qui affiche les stations où les taxons sont présents
#' @param id Identifiant du module
#' @param donnees Reactive avec les données
#' @param choix_departements Reactive avec départements sélectionnés
#' @param choix_eqb Reactive avec EQB sélectionnés
#' @param choix_uh Reactive avec UH sélectionnées
#' @param choix_reseau Reactive avec réseau sélectionnés
#' @param choix_qualification Reactive avec qualif sélectionnées
#' @noRd

mod_repartition_carte_server <- function(id,
                                         donnees,
                                         choix_departements,
                                         choix_eqb,
                                         choix_uh,
                                         choix_reseau,
                                         choix_qualification) {

  shiny::moduleServer(id, function(input, output, session) { # Structure serveur du module

# Créer la liste des taxons disponibles selon les filtres
    taxons_filtres <- shiny::reactive({ # Objet réactif pour les taxons disponibles
      shiny::req(donnees()) # Vérifie que les données existent
      fun_filtrer_repartition_taxon( # Appel de la fonction de filtrage
        donnees = donnees(), # Données
        choix_departements = choix_departements(), # Départements choisis
        choix_eqb = choix_eqb(), # EQB choisis
        choix_uh = choix_uh(), # UH choisies
        choix_reseau = choix_reseau(), # Reseau choisis
        choix_qualification = choix_qualification(), # Qualif choisies
        taxon_selectionne = NULL ) } )# Pas encore de filtre taxon

# Mise à jour de la liste des taxons
    shiny::observeEvent(taxons_filtres(), { # Se relance si les filtres changent
      df <- taxons_filtres() # Récupère les taxons filtrés

      if (is.null(df) || nrow(df) == 0) { # Si aucune donnée disponible
        shiny::updateSelectizeInput( # Met à jour la liste
          session = session, # Session du module
          inputId = "taxon_selectionne", # ID sans ns ici
          choices = character(0), # Aucun choix
          selected = character(0), # Aucune sélection
          server = TRUE ) # Chargement côté serveur
        return() } # Arrête l'observe

      choix_taxons <- df |> # Crée la liste des taxons
        sf::st_drop_geometry() |> # Supprime la géométrie
        dplyr::filter(!is.na(libelle_taxon)) |> # Enlève les taxons vides
        dplyr::distinct(libelle_taxon) |> # Garde un nom par taxon
        dplyr::arrange(libelle_taxon) |> # Trie par ordre alphabétique
        dplyr::pull(libelle_taxon) # Extrait le vecteur

      taxons_choisis <- shiny::isolate(input$taxon_selectionne) # Lit sans relancer l'observe
      taxons_choisis <- taxons_choisis[ # Sécurise la sélection
        taxons_choisis %in% choix_taxons ] # Garde seulement les taxons encore disponibles

      shiny::updateSelectizeInput( # Met à jour la liste
        session = session, # Session du module
        inputId = "taxon_selectionne", # ID du selectize
        choices = choix_taxons, # Liste des taxons
        selected = taxons_choisis, # Garde la sélection
        server = TRUE ) } ) # Chargement côté serveur

# Données des taxons sélectionnés pour la carte
    repartition_taxon <- shiny::reactive({ # Données affichées sur la carte
      shiny::req(donnees()) # Vérifie les données
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon
      fun_filtrer_repartition_taxon( # Filtre avec les taxons choisis
        donnees = donnees(), # Liste de données
        choix_departements = choix_departements(), # Filtre département
        choix_eqb = choix_eqb(), # Filtre EQB
        choix_uh = choix_uh(), # Filtre UH
        choix_reseau = choix_reseau(), # Filtre reseau
        choix_qualification = choix_qualification(), # Filtre qualif
        taxon_selectionne = input$taxon_selectionne ) } ) # Taxons choisis

# Données détaillées des taxons pour le tableau exportable
    taxons_export <- shiny::reactive({ # Table source filtrée pour export
      shiny::req(donnees()) # Vérifie que les données existent
      shiny::req(donnees()$taxons) # Vérifie la table taxons
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné

      filtre_dep <- choix_departements() # Récupère le filtre département
      filtre_eqb <- choix_eqb() # Récupère le filtre EQB
      filtre_uh <- choix_uh() # Récupère le filtre UH
      filtre_reseau <- choix_reseau() # Récupère le filtre réseau
      filtre_qualification <- choix_qualification() # Récupère le filtre qualification

      df_export <- donnees()$taxons |> # Table taxons brute
        dplyr::mutate(
          eqb = dplyr::case_when( # Creation EQB
            code_support == "10" ~ "Diatomées", # Association support
            code_support == "13" ~ "Macroinvertébrés", # Association support
            code_support == "27" ~ "Macrophytes", # Association support
            code_support == "4" ~ "Poissons", # Association support
            TRUE ~ NA_character_), # Sécurité
          date_prelevement = as.Date(date_prelevement)) |> # Conversion date
        dplyr::filter(
          libelle_taxon %in% input$taxon_selectionne) # Filtre les taxons sélectionnés

      if (!is.null(donnees()$stations) && "reseau" %in% names(donnees()$stations)) { # Si stations existe
        stations_infos <- donnees()$stations |> # Table stations
          sf::st_drop_geometry() |> # Supprime la géométrie si sf
          dplyr::select(code_station, code_dep, reseau, UH_calculee) |> # Colonnes utiles
          dplyr::distinct() # Supprime les doublons
        df_export <- df_export |> # Ajoute infos stations
          dplyr::left_join(
            stations_infos, # Infos stations
            by = "code_station" ) } # Jointure station

      df_export <- filtrer_donnees( # Applique les filtres globaux
        data = df_export, # Table export
        choix_departements = filtre_dep, # Filtre département
        choix_eqb = filtre_eqb, # Filtre EQB
        choix_reseau = filtre_reseau, # Filtre réseau
        choix_qualification = filtre_qualification ) # Filtre qualification

      if (!is.null(filtre_uh) && # Filtre UH existe
          length(filtre_uh) > 0 && # Au moins 1 choix
          !("Toutes" %in% filtre_uh) && # Pas toutes
          "UH_calculee" %in% names(df_export)) { # Colonne existe
        df_export <- df_export |>
          dplyr::filter(UH_calculee %in% filtre_uh) } # Garde UH
      if (is.null(df_export) || nrow(df_export) == 0) { return(NULL) } # Si aucune donnée

      df_export |> # Table finale
        dplyr::select(
          code_station,
          libelle_station,
          code_dep,
          UH_calculee,
          reseau,
          date_prelevement,
          code_prelevement,
          eqb,
          code_support,
          libelle_support,
          code_appel_taxon,
          libelle_taxon,
          resultat_taxon,
          abondance_relative,
          code_qualification,
          libelle_qualification ) |>
        dplyr::arrange(
          libelle_taxon,
          code_station,
          date_prelevement) } ) # Tri final

#### Partie Carte###
# Fond de la carte
    limites_region_carte <- shiny::reactive({ # Limites régionales
      sf::st_transform(limites_region_l, 4326) } ) # Conversion WGS84
    limites_cours_eau_carte <- shiny::reactive({ # Cours d'eau
      sf::st_transform(limites_cours_eau, 4326) } )
    limites_bv_carte <- shiny::reactive({ # Bassins versants
      sf::st_transform(limites_bv_l, 4326) } )

# Carte de base, meme base que mod_station_carte
    output$carte_repartition <- leaflet::renderLeaflet({ # Création de la carte
      limites_region <- limites_region_carte() # Récupère les limites
      cours_eau <- limites_cours_eau_carte() # Récupère les cours d'eau
      bassins_versants <- limites_bv_carte() # Récupère les bassins
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          preferCanvas = TRUE ) ) |> # Affichage plus léger
        leaflet::addTiles() |> # Fond OpenStreetMap
        leaflet::addMapPane("hydro", zIndex = 405) |> # Plan cours d'eau
        leaflet::addMapPane("bv", zIndex = 408) |> # Plan bassins versants
        leaflet::addMapPane("limites", zIndex = 410) |> # Plan limites
        leaflet::addMapPane("points", zIndex = 420) |> # Plan points
        leaflet::setView(
          lng = 0.5, # Longitude
          lat = 49.2, # Latitude
          zoom = 8 ) |> # Zoom
        leaflet::addPolylines(
          data = cours_eau, # Données cours d'eau
          color = "#2C7FB8", # Couleur bleue
          opacity = 0.7, # Transparence
          weight = 1, # Épaisseur
          group = "Cours d'eau", # Groupe
          options = leaflet::pathOptions(pane = "hydro") ) |>
        leaflet::addPolylines(
          data = bassins_versants, # Données BV
          color = "red", # Couleur rouge
          opacity = 0.8, # Transparence
          weight = 1.2, # Épaisseur
          group = "Bassins versants", # Groupe
          options = leaflet::pathOptions(pane = "bv") ) |>
        leaflet::addPolylines(
          data = limites_region, # Données limites
          color = "black", # Couleur noire
          opacity = 1, # Pas de transparence
          weight = 2, # Épaisseur
          group = "Limites administratives", # Groupe
          options = leaflet::pathOptions(pane = "limites") ) |>
        leaflet::addLayersControl(
          overlayGroups = c(
            "Limites administratives", # Limites
            "Cours d'eau", # Cours d'eau
            "Bassins versants" ), # Bassins versants
          options = leaflet::layersControlOptions(collapsed = FALSE) ) |>
        leaflet::hideGroup("Bassins versants") } ) # Cache les BV au départ

# Meme si l'onglet est pas actif,
    shiny::outputOptions(
      output,
      "carte_repartition",
      suspendWhenHidden = FALSE) # Garde la carte active

# Mise à jour des points selon les taxons choisis
    shiny::observe({ # Se relance quand la sélection change
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon choisi
      df <- repartition_taxon() # Récupère les données des taxons
      shiny::req(df) # Vérifie que la table existe
      shiny::req(nrow(df) > 0) # Vérifie qu'il y a des lignes

      crs_df <- sf::st_crs(df) # Récupère le système de coordonnées
      geom_df <- sf::st_geometry(df) # Récupère les géométries

      df <- df |> # Agrège les données par station
        dplyr::mutate(
          id_ligne = dplyr::row_number() ) |> # Numéro de ligne
        sf::st_drop_geometry() |> # Supprime temporairement la géométrie
        dplyr::group_by(
          code_station, # Code station
          libelle_station ) |> # Nom station
        dplyr::summarise(
          nb_taxons = dplyr::n_distinct(libelle_taxon), # Nombre de taxons sélectionnés présents
          taxons_resume = paste(
            unique(paste0(
              "<b>", libelle_taxon, "</b><br/>",
              resume,
              "<br/>Qualification : ", libelle_qualification ) ),
            collapse = "<br/><br/>" ),
          eqb = paste(sort(unique(eqb)), collapse = ", "), # Liste des EQB
          reseau = paste(sort(unique(reseau)), collapse = ", "), # Liste des réseaux
          abondance_totale = sum(abondance_moyenne, na.rm = TRUE), # Somme des abondances moyennes
          id_ligne = dplyr::first(id_ligne), # Ligne pour récupérer la géométrie
          .groups = "drop" ) |> # Supprime le regroupement
        dplyr::mutate(
          geometry = geom_df[id_ligne] ) |> # Géométrie de la station
        sf::st_as_sf(crs = crs_df) |> # Recrée un objet sf
        sf::st_transform(4326) # Sécurité WGS84 pour leaflet
      max_abondance <- 1000 # Valeur max fixe

      df <- df |> # Ajoute le rayon des cercles
        dplyr::mutate(
          rayon_cercle = 5 + (sqrt(abondance_totale) / sqrt(max_abondance)) * 20) # Taille cercle

      coords <- sf::st_coordinates(df) # Coordonnées des stations
      centre_lng <- mean(coords[, 1], na.rm = TRUE) # Longitude moyenne
      centre_lat <- mean(coords[, 2], na.rm = TRUE) # Latitude moyenne

      leaflet::leafletProxy("carte_repartition", session = session) |> # Met à jour la carte
        leaflet::clearGroup("taxons") |> # Supprime seulement les anciens points
        leaflet::clearPopups() |> # Ferme les popups
        leaflet::setView(
          lng = centre_lng, # Longitude moyenne
          lat = centre_lat, # Latitude moyenne
          zoom = 8 ) |> # Zoom régional
        leaflet::addCircleMarkers(
          data = df, # Données agrégées
          group = "taxons", # Groupe des points taxons
          radius = ~rayon_cercle, # Taille proportionnelle
          stroke = TRUE, # Contour du cercle
          color = "black", # Couleur du contour
          weight = 1, # Épaisseur du contour
          fillColor = "#B2182B", # Couleur intérieure
          fillOpacity = 0.8, # Transparence intérieure
          layerId = ~code_station, # ID unique par station
          label = ~lapply(
            paste0(
              "<b>", libelle_station, "</b><br/>",
              "Code station : ", code_station, "<br/>",
              "Nombre de taxons : ", nb_taxons ),
            htmltools::HTML ),
          options = leaflet::pathOptions(pane = "points"), # Plan des points
          popup = ~paste0(
            "<b>", libelle_station, "</b><br/>",
            "Code station : ", code_station, "<br/>",
            "Nombre de taxons sélectionnés présents : ", nb_taxons, "<br/>",
            "Abondance totale : ", abondance_totale, "<br/>",
            "Rayon affiché : ", round(rayon_cercle, 1), "<br/>",
            "EQB : ", eqb, "<br/>",
            "Réseau : ", reseau, "<br/>",
            "<br/><b>Taxons :</b><br/>", taxons_resume ) ) } )

# Graphique
    output$plot_repartition_taxon <- plotly::renderPlotly({ # Création du graphique en plotly
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné
      plot <- fun_plot_repartition_taxon( # Appelle la fonction graphique
        donnees = donnees(), # Liste de données
        choix_departements = choix_departements(), # Filtre département
        choix_eqb = choix_eqb(), # Filtre EQB
        choix_uh = choix_uh(), # Filtre UH
        choix_reseau = choix_reseau(), # Filtre réseau
        choix_qualification = choix_qualification(), # Filtre qualification
        taxon_selectionne = input$taxon_selectionne ) # Taxons choisis
      shiny::validate(
        shiny::need(!is.null(plot), "Aucune donnée disponible pour ce ou ces taxons.") )
      plot } ) # Affiche le graphique

# Tableau
    output$table_repartition_taxon <- DT::renderDT({ # Création du tableau
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné
      df_table <- taxons_export() # Récupère les lignes filtrées
      shiny::validate(
        shiny::need(!is.null(df_table) && nrow(df_table) > 0, "Aucune donnée à afficher."))
      DT::datatable(
        df_table, # Données affichées
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # Nombre de lignes affichées
          scrollX = TRUE ) ) # Scroll horizontal
    }, server = TRUE )

# Export CSV du tableau
    output$download_repartition_taxon <- shiny::downloadHandler( # Téléchargement CSV
      filename = function() { # Nom automatique du fichier
        taxons <- input$taxon_selectionne # Taxons sélectionnés
        if (is.null(taxons) || length(taxons) == 0) { # Si aucun taxon
          return("repartition_taxons.csv") } # Nom par défaut
        taxons <- paste(taxons, collapse = "_") # Assemble les taxons
        paste0(
          "repartition_",
          taxons,
          ".csv" ) },  # Nom final
      content = function(file) { # Contenu du fichier
        table_export <- taxons_export() # Table exportée
        utils::write.csv2(
          table_export, # Table exportée
          file, # Chemin fichier
          row.names = FALSE, # Pas de noms de lignes
          fileEncoding = "UTF-8") } )# Encodage
  } ) }

## À appeler dans l'UI
# mod_repartition_carte_ui("repartition_carte")

## À appeler dans le server
# mod_repartition_carte_server("repartition_carte")
