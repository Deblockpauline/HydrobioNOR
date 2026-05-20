#' Carte de répartition des taxons UI
#'
#' @description Module Shiny permettant d'afficher la répartition des taxons
#' Inspiration de mod_station_carte
#' @noRd

mod_repartition_carte_ui <- function(id, hauteur = "700px") { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments UI
    shiny::selectizeInput( # Liste déroulante avec recherche
      inputId = ns("taxon_selectionne"), # ID du ou des taxons choisis
      label = "Choisir un ou plusieurs taxons", # Texte affiché au-dessus
      choices = NULL, # Choix ajoutés côté serveur
      selected = NULL, # Aucun taxon au départ
      multiple = TRUE, # Plusieurs taxons à la fois
      options = list( # Options de la liste
        placeholder = "Choisir le nom d'un ou plusieurs taxons", # Texte par défaut
        maxOptions = 10000 ) ), # Affiche beaucoup de taxons
    shiny::br(), # Espace
    leaflet::leafletOutput( # Emplacement de la carte
      outputId = ns("carte_repartition"), # ID de la carte
      height = hauteur ), # Hauteur de la carte
    shiny::br(), # Espace
    plotly::plotlyOutput( # Emplacement du graphique
      outputId = ns("plot_repartition_taxon"), # ID du graphique
      height = "350px" ), # Hauteur du graphique
    shiny::br(), # Espace
    shiny::h4("Tableau des prélèvements"), # Titre du tableau
    shiny::downloadButton( # Bouton export CSV
      outputId = ns("download_repartition_taxon"),
      label = "Télécharger les données (.csv)" ), # texte dans le bouton
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
#' @noRd

mod_repartition_carte_server <- function(id,
                                         donnees,
                                         choix_departements,
                                         choix_eqb,
                                         choix_uh) {

  shiny::moduleServer(id, function(input, output, session) { # Structure serveur du module

# Créer la liste des taxons disponibles selon les filtres
    taxons_filtres <- shiny::reactive({ # Objet réactif pour les taxons disponibles
      shiny::req(donnees()) # Vérifie que les données existent
      fun_filtrer_repartition_taxon( # Appel de la fonction de filtrage
        donnees = donnees(), # Données
        choix_departements = choix_departements(), # Départements choisis
        choix_eqb = choix_eqb(), # EQB choisis
        choix_uh = choix_uh(), # UH choisies
        taxon_selectionne = NULL ) } ) # Pas encore de filtre taxon

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
        server = TRUE ) # Chargement côté serveur
    } )

# Données des taxons sélectionnés pour la carte
    repartition_taxon <- shiny::reactive({ # Données affichées sur la carte
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon
      fun_filtrer_repartition_taxon( # Filtre avec les taxons choisis
        donnees = donnees(), # Liste de données
        choix_departements = choix_departements(), # Filtre département
        choix_eqb = choix_eqb(), # Filtre EQB
        choix_uh = choix_uh(), # Filtre UH
        taxon_selectionne = input$taxon_selectionne ) } ) # Taxons choisis

# Données détaillées des taxons pour le tableau exportable
    taxons_export <- shiny::reactive({ # Table source filtrée pour export
      shiny::req(donnees()) # Vérifie que les données existent
      shiny::req(donnees()$taxons) # Vérifie que la table taxons existe
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné
      df_export <- donnees()$taxons |> # Table des taxons
        dplyr::mutate( # Ajoute les colonnes utiles
          eqb = dplyr::case_when( # Recrée l'EQB depuis le code support
            code_support == "10" ~ "Diatomées", # Diatomées
            code_support == "13" ~ "Macroinvertébrés", # Macroinvertébrés
            code_support == "27" ~ "Macrophytes", # Macrophytes
            code_support == "4" ~ "Poissons"), # Poissons
          date_prelevement = as.Date(date_prelevement) ) |> # Convertit la date
        dplyr::filter( # Filtrage
          libelle_taxon %in% input$taxon_selectionne ) # Garde les taxons sélectionnés

      if (!is.null(choix_eqb()) && # Si un choix EQB existe
          length(choix_eqb()) > 0 && # Si au moins un EQB
          !("Tous" %in% choix_eqb())) { # Si ce n'est pas Tous
        df_export <- df_export |> # Table export
          dplyr::filter(eqb %in% choix_eqb()) } # Garde les EQB choisis
      if (nrow(df_export) == 0) { return(NULL) }  # Si aucune ligne restante, arrete

      df_export |> # Table finale export
        dplyr::arrange( # Tri
          libelle_taxon, # Par taxon
          code_station, # Puis station
          date_prelevement ) # Puis date
    } )

# Pour la carte
# Fond de carte
    limites_region_carte <- shiny::reactive({ # Limites régionales
      sf::st_transform(limites_region_l, 4326) } ) # Conversion WGS84
    limites_cours_eau_carte <- shiny::reactive({ # Cours d'eau
      sf::st_transform(limites_cours_eau, 4326) } )
    limites_bv_carte <- shiny::reactive({ # Bassins versants
      sf::st_transform(limites_bv_l, 4326) } )

# Carte de base
    output$carte_repartition <- leaflet::renderLeaflet({ # Création de la carte
      limites_region <- limites_region_carte() # Récupère les limites
      cours_eau <- limites_cours_eau_carte() # Récupère les cours d'eau
      bassins_versants <- limites_bv_carte() # Récupère les bassins

      leaflet::leaflet() %>% # Initialise la carte
        leaflet::addTiles() %>% # Fond OpenStreetMap
        leaflet::addMapPane("hydro", zIndex = 405) %>% # Plan cours d'eau
        leaflet::addMapPane("bv", zIndex = 408) %>% # Plan bassins versants
        leaflet::addMapPane("limites", zIndex = 410) %>% # Plan limites
        leaflet::addMapPane("points", zIndex = 420) %>% # Plan points et zIndex = ordre de superposition
        leaflet::setView( # Vue initiale
          lng = 0.5, # Longitude
          lat = 49.2, # Latitude
          zoom = 8 ) %>% # Zoom
        leaflet::addPolylines( # Ajoute les cours d'eau
          data = cours_eau, # Données cours d'eau
          color = "#2C7FB8", # Couleur bleue
          opacity = 0.7, # Transparence
          weight = 1, # Épaisseur
          group = "Cours d'eau", # Groupe
          options = leaflet::pathOptions(pane = "hydro") ) %>% # Plan hydro
        leaflet::addPolylines( # Ajoute les bassins versants
          data = bassins_versants, # Données BV
          color = "red", # Couleur rouge
          opacity = 0.8, # Transparence
          weight = 1.2, # Épaisseur
          group = "Bassins versants", # Groupe
          options = leaflet::pathOptions(pane = "bv") ) %>% # Plan BV
        leaflet::addPolylines( # Ajoute les limites régionales
          data = limites_region, # Données limites
          color = "black", # Couleur noire
          opacity = 1, # Pas de transparence
          weight = 2, # Épaisseur
          group = "Limites administratives", # Groupe
          options = leaflet::pathOptions(pane = "limites") ) %>% # Plan limites
        leaflet::addLayersControl( # Contrôle des couches
          overlayGroups = c(
            "Limites administratives", # Limites
            "Cours d'eau", # Cours d'eau
            "Bassins versants" ), # Bassins versants
          options = leaflet::layersControlOptions(collapsed = FALSE) ) %>% # Contrôle ouvert
        leaflet::hideGroup("Bassins versants") # Cache les BV au départ
    } )

# Mise à jour des points selon les taxons choisis
    shiny::observe({ # Se relance quand la sélection change
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon choisi
      df <- repartition_taxon() # Récupère les données des taxons
      shiny::req(df) # Vérifie que la table existe
      shiny::req(nrow(df) > 0) # Vérifie qu'il y a des lignes

      crs_df <- sf::st_crs(df) # Récupère le système de coordonnées
      geom_df <- sf::st_geometry(df) # Récupère les géométries

      df <- df |> # Agrège les données par station
        dplyr::mutate( # Crée un identifiant temporaire
          id_ligne = dplyr::row_number() ) |> # Numéro de ligne
        sf::st_drop_geometry() |> # Supprime temporairement la géométrie
        dplyr::group_by( # Regroupement par station
          code_station, # Code station
          libelle_station ) |> # Nom station
        dplyr::summarise( # Résume les taxons présents
          nb_taxons = dplyr::n_distinct(libelle_taxon), # Nombre de taxons sélectionnés présents
          taxons_resume = paste( # Taxon + résumé
            unique(paste0(
              "<b>", libelle_taxon, "</b><br/>", # Nom du taxon
              resume ) ), # Texte de la colonne résumé
            collapse = "<br/><br/>" ), # Séparation entre taxons/résumés
          eqb = paste(sort(unique(eqb)), collapse = ", "), # Liste des EQB
          abondance_totale = sum(abondance_moyenne, na.rm = TRUE), # Somme des abondances moyennes
          id_ligne = dplyr::first(id_ligne), # Ligne pour récupérer la géométrie
          .groups = "drop" ) |> # Supprime le regroupement
        dplyr::mutate( # Réassocie la géométrie
          geometry = geom_df[id_ligne] ) |> # Géométrie de la station
        sf::st_as_sf(crs = crs_df) # Recrée un objet sf

      # Pour la grandeur des cercles
      max_abondance <- 1000 # Valeur max fixe
      df <- df |> # Ajoute le rayon des cercles
        dplyr::mutate(
          rayon_cercle = 5 + (sqrt(abondance_totale) / sqrt(max_abondance)) * 20) # Permet de pas avoir de trop grand cercle

      coords <- sf::st_coordinates(df) # Coordonnées des stations
      centre_lng <- mean(coords[, 1], na.rm = TRUE) # Longitude moyenne
      centre_lat <- mean(coords[, 2], na.rm = TRUE) # Latitude moyenne

      leaflet::leafletProxy("carte_repartition", session = session) %>% # Met à jour la carte
        leaflet::clearMarkers() %>% # Supprime les anciens points
        leaflet::clearPopups() %>% # Ferme les popups
        leaflet::clearControls() %>% # Supprime les anciennes légendes
        leaflet::setView( # Recentre la carte
          lng = centre_lng, # Longitude moyenne
          lat = centre_lat, # Latitude moyenne
          zoom = 8 ) %>% # Zoom régional
        leaflet::addCircleMarkers( # Ajoute les stations
          data = df, # Données agrégées
          radius = ~rayon_cercle, # Taille proportionnelle à l'abondance totale
          stroke = TRUE, # Contour du cercle
          color = "black", # Couleur du contour
          weight = 1, # Épaisseur du contour
          fillColor = "#B2182B", # Couleur intérieure
          fillOpacity = 0.8, # Transparence intérieure
          layerId = ~code_station, # ID unique par station
          label = ~lapply( # Texte au survol
            paste0(
              "<b>", libelle_station, "</b><br/>", # Nom station
              "Code station : ", code_station, "<br/>", # Code station
              "Nombre de taxons : ", nb_taxons ), # Nombre de taxons
            htmltools::HTML ), # Convertit en HTML
          options = leaflet::pathOptions(pane = "points"), # Plan des points
          popup = ~paste0( # Popup au clic
            "<b>", libelle_station, "</b><br/>", # Nom station
            "Code station : ", code_station, "<br/>", # Code station
            "Nombre de taxons sélectionnés présents : ", nb_taxons, "<br/>", # Nombre de taxons
            "Abondance totale : ", abondance_totale, "<br/>",
            "Rayon affiché : ", round(rayon_cercle, 1), "<br/>",
            "EQB : ", eqb, "<br/>", # EQB
            "<br/><b>Taxons :</b><br/>", taxons_resume ) ) # Taxons + résumé
    } )

# Graphique du nombre de stations par année
    output$plot_repartition_taxon <- plotly::renderPlotly({ # Création du graphique en plotly
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné
      plot <- fun_plot_repartition_taxon( # Appelle la fonction graphique
        donnees = donnees(), # Liste de données
        choix_departements = choix_departements(), # Filtre département
        choix_eqb = choix_eqb(), # Filtre EQB
        choix_uh = choix_uh(), # Filtre UH
        taxon_selectionne = input$taxon_selectionne ) # Taxons choisis
      shiny::validate( # Si pas de graphique
        shiny::need(!is.null(plot), "Aucune donnée disponible pour ce ou ces taxons.") ) # Message
      plot } ) # Affiche le graphique

#Tableau des prélèvements selon les taxons choisis
    output$table_repartition_taxon <- DT::renderDT({ # Création du tableau
      shiny::req(input$taxon_selectionne) # Attend au moins un taxon sélectionné
      df_table <- taxons_export() # Récupère les lignes dans taxons
      DT::datatable( # Tableau interactif
        df_table, # Données affichées
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # Nombre de lignes affichées
          scrollX = TRUE ) ) # Scroll horizontal
    }, server = TRUE ) # Permet d'afficher toutes les lignes correctement

# Export CSV du tableau des prélèvements
    output$download_repartition_taxon <- shiny::downloadHandler( # Téléchargement CSV
      filename = function() { # Nom automatique du fichier
        taxons <- input$taxon_selectionne # Taxons sélectionnés
        if (is.null(taxons) || length(taxons) == 0) { # Si aucun taxon
          return("repartition_taxons.csv")}
        taxons <- paste(taxons, collapse = "_") # Assemble les taxons
        paste0( # Nom final
          "repartition_",
          taxons,
          ".csv" )},

      content = function(file) { # Contenu du fichier
        table_export <- taxons_export() # Table exportée
        utils::write.csv2(
          table_export,
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8") } )
  } ) }
