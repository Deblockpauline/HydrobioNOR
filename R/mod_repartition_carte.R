#' Carte de répartition des taxons UI
#' @description Module Shiny permettant d'afficher la répartition des taxons
#' Inspiration de mod_station_carte
#' @noRd

mod_repartition_carte_ui <- function(id, hauteur = "700px") { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments UI

    shiny::selectizeInput( # Liste déroulante des taxons
      inputId = ns("taxon_selectionne"),
      label = "Choisir un ou plusieurs taxons", # Texte affiché au-dessus
      choices = NULL, # Choix ajoutés côté serveur
      selected = NULL, # Aucun taxon au départ
      multiple = TRUE, # Plusieurs taxons à la fois
      options = list( # Options de la liste
        placeholder = "Choisir le nom d'un ou plusieurs taxons", # Texte par défaut
        maxOptions = 10000 ) ), # Affiche beaucoup de taxons
    shiny::br(), # Espace
    shiny::selectizeInput( # Liste déroulante des EEE
      inputId = ns("eee_selectionne"),
      label = "Choisir une ou plusieurs espèces exotiques envahissantes", # Texte affiché au-dessus
      choices = NULL, # Choix ajoutés côté serveur
      selected = NULL, # Aucune EEE au départ
      multiple = TRUE, # Plusieurs EEE à la fois
      options = list( # Options de la liste
        placeholder = "Choisir une ou plusieurs EEE", # Texte par défaut
        maxOptions = 100 ) ), # Affiche les EEE
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
      outputId = ns("download_repartition_taxon"), # ID bouton export
      label = "Télécharger les données (.csv)" ), # Texte bouton
    shiny::br(), # Espace
    DT::DTOutput(
      outputId = ns("table_repartition_taxon") ) ) } # Tableau des données

#' Carte de répartition des taxons server
#' @description Module Shiny qui affiche les stations où les taxons sont présents
#' @noRd

mod_repartition_carte_server <- function(id,
                                         donnees,
                                         choix_departements,
                                         choix_eqb,
                                         choix_uh,
                                         choix_reseau,
                                         choix_qualification) {
  shiny::moduleServer(id, function(input, output, session) {

# Listes des especes
    # Liste des EEE disponibles
    choix_eee_disponibles <- shiny::reactive({ # Liste complète des EEE disponibles
      donnees()$eee |>
        dplyr::filter(!is.na(libelle_taxon)) |> # Enlève les noms vides
        dplyr::distinct(libelle_taxon) |> # Garde un nom par espèce
        dplyr::arrange(libelle_taxon) |> # Trie par ordre alphabétique
        dplyr::pull(libelle_taxon) }) # Extrait les noms

    # Créer la liste de tout les taxons disponibles selon les filtres
    taxons_filtres <- shiny::reactive({ # Objet réactif pour les taxons disponibles
      shiny::req(donnees()) # Vérifie que les données existent
      fun_filtrer_repartition_taxon( # Appel de la fonction de filtrage
        donnees = donnees(), # Données
        choix_departements = choix_departements(), # Départements choisis
        choix_eqb = choix_eqb(), # EQB choisis
        choix_uh = choix_uh(), # UH choisies
        choix_reseau = choix_reseau(), # Reseau choisis
        choix_qualification = choix_qualification(), # Qualif choisies
        taxon_selectionne = NULL ) }) # Pas encore de filtre taxon

    # Mise à jour de la liste des taxons
    choix_taxons_disponibles <- shiny::reactive({ # Liste complète des taxons disponibles
      df <- taxons_filtres() # Récupère les taxons filtrés
      if (is.null(df) || nrow(df) == 0) { return(character(0)) } # si rien, retourne une liste vide
      df |>
        sf::st_drop_geometry() |> # Supprime la géométrie
        dplyr::filter(!is.na(libelle_taxon)) |>
        dplyr::distinct(libelle_taxon) |>
        dplyr::arrange(libelle_taxon) |>
        dplyr::pull(libelle_taxon) })

    shiny::observeEvent(choix_taxons_disponibles(), {
      choix_taxons <- choix_taxons_disponibles() # Récupère les taxons disponibles
      taxons_choisis <- shiny::isolate(input$taxon_selectionne) # Lit sans relancer l'observe
      if (is.null(taxons_choisis)) { # Vérifie si aucune sélection
        taxons_choisis <- character(0) } # Crée une sélection vide
      taxons_choisis <- taxons_choisis[ # Sécurise la sélection
        taxons_choisis %in% choix_taxons ] # Garde seulement les taxons encore disponibles
      shiny::updateSelectizeInput( # Met à jour la liste
        session = session, # Session du module
        inputId = "taxon_selectionne", # ID du selectize
        choices = choix_taxons, # Liste des taxons
        selected = taxons_choisis, # Garde la sélection
        server = TRUE ) }) # Chargement côté serveur

    # Mise à jour de la liste des EEE
    shiny::observeEvent(choix_eee_disponibles(), {
      choix_eee <- choix_eee_disponibles() # Récupère les EEE disponibles
      eee_choisies <- shiny::isolate(input$eee_selectionne)
      if (is.null(eee_choisies)) {
        eee_choisies <- character(0) }
      eee_choisies <- eee_choisies[
        eee_choisies %in% choix_eee ] # Garde seulement les EEE disponibles
      shiny::updateSelectizeInput(
        session = session,
        inputId = "eee_selectionne",
        choices = choix_eee, # Liste des EEE
        selected = eee_choisies,
        server = TRUE ) })


# Données des séléctions
    # Sélection active regroupant les taxons et les EEE
    selection_active <- shiny::reactive({ # Regroupe les deux types de sélection
      taxons <- input$taxon_selectionne # Récupère les taxons sélectionnés
      eee_choisies <- input$eee_selectionne # Récupère les EEE sélectionnées
      selection <- c( taxons, eee_choisies) # Combine les deux listes
      selection <- selection[ # Nettoie la sélection
        !is.na(selection) & # Enlève les NA
          selection != "" ] # Enlève les valeurs vides
      unique(selection) }) # Supprime les doublons

    # Données des taxons sélectionnés pour la carte
    repartition_taxon <- shiny::reactive({ # Données affichées sur la carte
      shiny::req(donnees()) # Vérifie que les données existent
      shiny::req(selection_active()) # Attend au moins une sélection
      fun_filtrer_repartition_taxon( # Filtre avec les taxons choisis
        donnees = donnees(),
        choix_departements = choix_departements(),
        choix_eqb = choix_eqb(),
        choix_uh = choix_uh(),
        choix_reseau = choix_reseau(),
        choix_qualification = choix_qualification(),
        taxon_selectionne = selection_active() ) })


    # Données détaillées des taxons pour le tableau exportable
    taxons_export <- shiny::reactive({ # Table source filtrée pour export
      shiny::req(donnees())
      shiny::req(donnees()$taxons)
      shiny::req(selection_active())
      filtre_dep <- choix_departements() # Récupère le filtre
      filtre_eqb <- choix_eqb()
      filtre_uh <- choix_uh()
      filtre_reseau <- choix_reseau()
      filtre_qualification <- choix_qualification()

      df_export <- donnees()$taxons |> # Table taxons brute
        dplyr::mutate(
          eqb = dplyr::case_when( # Creation EQB
            code_support == "10" ~ "Diatomées",
            code_support == "13" ~ "Macroinvertébrés",
            code_support == "27" ~ "Macrophytes",
            code_support == "4" ~ "Poissons",
            TRUE ~ NA_character_), # Sécurité
          date_prelevement = as.Date(date_prelevement)) |> # Conversion date
        dplyr::filter(
          libelle_taxon %in% selection_active() ) # Filtre les taxons sélectionnés

      # Ajout du type d'EEE
      if (!is.null(donnees()$eee) &&
          "libelle_taxon" %in% names(donnees()$eee) &&
          "type_EEE" %in% names(donnees()$eee)) {

        eee_infos <- donnees()$eee |>
          dplyr::filter(!is.na(libelle_taxon)) |>
          dplyr::group_by(libelle_taxon) |>
          dplyr::summarise(
            type_EEE = paste(
              unique(
                type_EEE[!is.na(type_EEE) & type_EEE != ""]
              ),
              collapse = ", " ),
            .groups = "drop" )

        df_export <- df_export |>
          dplyr::left_join(
            eee_infos,
            by = "libelle_taxon" )
      } else {
        # Si la table EEE ou la colonne type_EEE n'existe pas
        df_export <- df_export |>
          dplyr::mutate(
            type_EEE = NA_character_ )
      }

      if (!is.null(donnees()$stations) && # Si stations existe
          "reseau" %in% names(donnees()$stations)) { # Vérifie la colonne réseau
        stations_infos <- donnees()$stations |> # Table stations
          sf::st_drop_geometry() |> # Supprime la géométrie si sf
          dplyr::select(
            code_station,
            code_dep,
            reseau,
            UH_calculee ) |> # Colonnes utiles
          dplyr::distinct() # Supprime les doublons
        df_export <- df_export |> # Ajoute infos stations
          dplyr::left_join(
            stations_infos, # Infos stations
            by = "code_station" ) } # Jointure station

      df_export <- filtrer_donnees( # Applique les filtres globaux
        data = df_export, # Table export
        choix_departements = filtre_dep,
        choix_eqb = filtre_eqb,
        choix_reseau = filtre_reseau,
        choix_qualification = filtre_qualification )

      if (is.null(filtre_uh) && # Filtre UH existe
          length(filtre_uh) > 0 && # Au moins 1 choix
          !("Toutes" %in% filtre_uh) &&
          "UH_calculee" %in% names(df_export)) { # Colonne existe
        df_export <- df_export |>
          dplyr::filter(
            UH_calculee %in% filtre_uh) } # Garde UH

      if (is.null(df_export) || nrow(df_export) == 0) {
        return(NULL) } # Si aucune donnée

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
          type_EEE,
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
    limites_region_carte <- shiny::reactive({
      shiny::req(donnees()$limites_region_l)
      sf::st_transform(
        donnees()$limites_region_l,4326) })

    limites_cours_eau_carte <- shiny::reactive({
      shiny::req(donnees()$limites_cours_eau)
      sf::st_transform(
        donnees()$limites_cours_eau, 4326)})

    limites_bv_carte <- shiny::reactive({
      shiny::req(donnees()$limites_bv_l)
      sf::st_transform(
        donnees()$limites_bv_l, 4326) })

# Carte de base, meme base que mod_station_carte
    output$carte_repartition <- leaflet::renderLeaflet({ # Création de la carte
      limites_region <- limites_region_carte() # Récupère les limites
      cours_eau <- limites_cours_eau_carte() # Récupère les cours d'eau
      bassins_versants <- limites_bv_carte() # Récupère les bassins
      leaflet::leaflet(
        options = leaflet::leafletOptions(
          preferCanvas = TRUE ) ) |> # Affichage plus léger
        leaflet::addTiles() |> # Fond OpenStreetMap
        leaflet::addMapPane(
          "hydro", zIndex = 405) |> # Plan cours d'eau
        leaflet::addMapPane(
          "bv",zIndex = 408) |> # Plan bassins versants
        leaflet::addMapPane(
          "limites", zIndex = 410) |> # Plan limites
        leaflet::addMapPane(
          "points",zIndex = 420) |> # Plan points
        leaflet::setView(
          lng = 0.5, # Longitude
          lat = 49.2, # Latitude
          zoom = 8 ) |> # Zoom

# Definition des couches
        leaflet::addPolylines(
          data = cours_eau, # Données cours d'eau
          color = "#2C7FB8", # Couleur bleue
          opacity = 0.7, # Transparence
          weight = 1, # Épaisseur
          group = "Cours d'eau", # Groupe
          options = leaflet::pathOptions(
            pane = "hydro") ) |>
        leaflet::addPolylines(
          data = bassins_versants, # Données BV
          color = "red", # Couleur rouge
          opacity = 0.8, # Transparence
          weight = 1.2, # Épaisseur
          group = "Bassins versants", # Groupe
          options = leaflet::pathOptions(
            pane = "bv") ) |>
        leaflet::addPolylines(
          data = limites_region, # Données limites
          color = "black", # Couleur noire
          opacity = 1, # Pas de transparence
          weight = 2, # Épaisseur
          group = "Limites administratives", # Groupe
          options = leaflet::pathOptions(
            pane = "limites") ) |>
        leaflet::addLayersControl(
          overlayGroups = c(
            "Limites administratives",
            "Cours d'eau", # Ordre
            "Bassins versants" ),
          options = leaflet::layersControlOptions(
            collapsed = FALSE) ) |>
        leaflet::hideGroup( # Pas choisi au demarage
          "Bassins versants") })

# Meme si l'onglet est pas actif,
    shiny::outputOptions(
      output,
      "carte_repartition",
      suspendWhenHidden = FALSE) # Garde la carte active

# Mise à jour des points selon les taxons choisis
    shiny::observe({ # Se relance quand la sélection change
      selection <- selection_active() # Récupère toutes les espèces sélectionnées
      if (is.null(selection) || # Si aucune sélection n'est faite
          length(selection) == 0) {
        leaflet::leafletProxy(
          "carte_repartition",
          session = session ) |>
          leaflet::clearGroup(
            "taxons") |> # Supprime les anciens points
          leaflet::clearPopups() # Ferme les popups
        return() } # Arrête l'observe

      df <- repartition_taxon() # Récupère les données des espèces sélectionnées
      shiny::req(df) # Vérifie que la table existe
      shiny::req(nrow(df) > 0) # Vérifie qu'il y a des lignes
      crs_df <- sf::st_crs(df) # Récupère le système de coordonnées
      geom_df <- sf::st_geometry(df) # Récupère les géométries

      df <- df |> # Ajoute l'information EEE
        dplyr::mutate(
          est_eee = libelle_taxon %in% choix_eee_disponibles() ) # TRUE si EEE

      df <- df |> # Agrège les données par station et par type de taxon
        dplyr::mutate(
          id_ligne = dplyr::row_number() ) |> # Numéro de ligne
        sf::st_drop_geometry() |> # Supprime temporairement la géométrie
        dplyr::group_by( # Regroupe par station ET type
          code_station, # Code station
          libelle_station, # Nom station
          est_eee ) |> # Sépare EEE et taxons classiques
        dplyr::summarise( # Resume
          nb_taxons = dplyr::n_distinct(
            libelle_taxon), # Nombre de taxons présents
          taxons_resume = paste( # Construction du texte à afficher au clic
            unique(paste0(
              "<b>", libelle_taxon, "</b><br/>",
              resume,
              "<br/>Qualification : ",
              libelle_qualification ) ),
            collapse = "<br/><br/>" ), # Sépare les taxons
          eqb = paste(
            sort(unique(eqb)),
            collapse = ", "), # Liste des EQB présents
          reseau = paste(
            sort(unique(reseau)),
            collapse = ", "), # Liste des réseaux
          abondance_totale = sum(
            abondance_moyenne,
            na.rm = TRUE), # Somme des abondances moyennes
          id_ligne = dplyr::first(
            id_ligne), # Ligne pour récupérer la géométrie
          .groups = "drop" ) |> # Supprime le regroupement
        dplyr::mutate(
          geometry = geom_df[id_ligne] ) |> # Géométrie de la station
        sf::st_as_sf(crs = crs_df) |> # Recrée un objet sf
        sf::st_transform(4326) # Sécurité WGS84 pour leaflet

# Ajoute le rayon des cercles
      max_abondance <- 1000 # Valeur max fixe
      df <- df |> # Ajoute le rayon
        dplyr::mutate(
          rayon_cercle = 5 + (sqrt(abondance_totale) /  sqrt(max_abondance)) * 20 ) # Taille proportionnelle à l'abondance

# Décalage des cercles lorsque les deux types sont présents
      coords <- sf::st_coordinates(df) # Coordonnées des stations
      df <- df |> # Ajoute les coordonnées
        dplyr::mutate(
          longitude = coords[, 1],
          latitude = coords[, 2] )

# Décalage léger pour afficher les deux cercles
      df <- df |>
        dplyr::group_by(code_station) |>
        dplyr::mutate(
          n_types = dplyr::n(),
          position_type = dplyr::row_number(),
          longitude = dplyr::case_when(
            n_types == 2 & position_type == 1 ~ longitude - 0.002,
            n_types == 2 & position_type == 2 ~ longitude + 0.002,
            TRUE ~ longitude),
          latitude = latitude ) |>
        dplyr::ungroup()
      centre_lng <- mean(
        coords[, 1],
        na.rm = TRUE) # Longitude moyenne
      centre_lat <- mean(
        coords[, 2],
        na.rm = TRUE) # Latitude moyenne

# Prépare les textes des popups
      df <- df |>
        dplyr::mutate(
          couleur = dplyr::if_else(
            est_eee,
            "firebrick", # Rpuge pour les EEE
            "darkgreen"), # VERT pour les taxons classiques
          type_taxon = dplyr::if_else(
            est_eee,
            "Espèce exotique envahissante",
            "Taxon"),
          popup_type = dplyr::if_else(
            est_eee,
            "<b style='color:#2E8B57;'>EEE</b>",
            "<b style='color:#B2182B;'>Taxon</b>" ) )


# Mise à jour de la carte avec une nouvelle selection
      leaflet::leafletProxy(
        "carte_repartition",
        session = session ) |> # Met à jour la carte
        leaflet::clearGroup(
          "taxons") |> # Supprime seulement les anciens points
        leaflet::clearPopups() |> # Ferme les popups
        leaflet::setView(
          lng = centre_lng, # Longitude moyenne
          lat = centre_lat, # Latitude moyenne
          zoom = 8 ) |> # Zoom régional
        leaflet::addCircleMarkers(
          data = df, # Données agrégées
          lng = ~longitude, # Longitude
          lat = ~latitude, # Latitude
          group = "taxons", # Groupe des points taxons
          radius = ~rayon_cercle, # Taille proportionnelle
          stroke = TRUE, # Contour du cercle
          color = "black", # Couleur du contour
          weight = 1, # Épaisseur du contour
          fillColor = ~couleur, # Vert EEE / rouge taxons
          fillOpacity = 0.8, # Transparence intérieure
          layerId = ~paste0(
            code_station,
            "_",
            est_eee), # ID unique station + type
          label = ~lapply(
            paste0(
              "<b>", libelle_station, "</b><br/>",
              "Code station : ",
              code_station, "<br/>",
              type_taxon, "<br/>",
              "Nombre de taxons : ",
              nb_taxons ),
            htmltools::HTML ),
          options = leaflet::pathOptions(
            pane = "points"), # Plan des points
          popup = ~paste0( # remet a jour le popup au clic
            "<b>",
            libelle_station,
            "</b><br/>",
            "Code station : ",
            code_station,
            "<br/>",
            popup_type,
            "<br/>",
            "Nombre de taxons présents : ",
            nb_taxons,
            "<br/>",
            "Abondance totale : ",
            round(abondance_totale, 2),
            "<br/>",
            "Rayon affiché : ",
            round(rayon_cercle, 1),
            "<br/>",
            "EQB : ",
            eqb,
            "<br/>",
            "Réseau : ",
            reseau,
            "<br/>",
            "<br/><b>Taxons :</b><br/>",
            taxons_resume ) ) })


# Graphique
    output$plot_repartition_taxon <- plotly::renderPlotly({ # Création du graphique en plotly
      shiny::req(selection_active()) # Attend une sélection
      plot <- fun_plot_repartition_taxon( # Appelle la fonction graphique
        donnees = donnees(), # Données
        choix_departements = choix_departements(),
        choix_eqb = choix_eqb(),
        choix_uh = choix_uh(),
        choix_reseau = choix_reseau(),
        choix_qualification = choix_qualification(),
        taxon_selectionne = selection_active() )
      shiny::validate(
        shiny::need(
          !is.null(plot),
          "Aucune donnée disponible pour ce ou ces taxons.") )
      plot }) # Affiche le graphique


# Tableau
    output$table_repartition_taxon <- DT::renderDT({ # Création du tableau
      shiny::req(selection_active()) # Attend une sélection
      df_table <- taxons_export() # Récupère les lignes filtrées
      shiny::validate(
        shiny::need(
          !is.null(df_table) &&
            nrow(df_table) > 0,
          "Aucune donnée à afficher."))
      DT::datatable(
        df_table, # Données affichées
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # Nombre de lignes affichées
          scrollX = TRUE ) ) # Scroll horizontal
    }, server = TRUE )


# Export CSV du tableau
    output$download_repartition_taxon <- shiny::downloadHandler( # Téléchargement CSV
      filename = function() { # Nom automatique
        taxons <- selection_active() # Taxons et EEE sélectionnés
        if (is.null(taxons) ||
            length(taxons) == 0) { # Si aucun taxon
          return("repartition_taxons.csv") } # Nom par défaut
        taxons <- paste(
          taxons,
          collapse = "_") # Assemble les noms
        paste0(
          "repartition_",
          taxons,
          ".csv" ) }, # Nom final

      content = function(file) { # Contenu fichier
        table_export <- taxons_export() # Table exportée
        utils::write.csv2(
          table_export, # Table exportée
          file, # Chemin fichier
          row.names = FALSE, # Pas de noms de lignes
          fileEncoding = "UTF-8") } ) # Encodage

  } ) }


## À appeler dans l'UI
# mod_repartition_carte_ui("repartition_carte")

## À appeler dans le server
# mod_repartition_carte_server("repartition_carte")

