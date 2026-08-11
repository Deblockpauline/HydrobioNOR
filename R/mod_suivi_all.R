#' Interface utilisateur du module de suivi
#' @param id Identifiant du module.
#' @noRd

mod_suivi_ui <- function(id) {
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments de l'interface
    shiny::h4( # Titre
      "Suivi temporel de l'état biologique"),
    shiny::p(
      paste( # Texte explicatif
        "Le diagramme présente l'évolution du nombre",
        "de stations par classe d'état biologique",
        "pour le ou les réseaux d'appartenance",
        "de la station sélectionnée." ) ),
    shiny::downloadButton(
      outputId = ns( # Bouton d'export du graphique
        "export_graphique_suivi" ),
      label = "Télécharger le graphique (.png)",
      icon = shiny::icon(
        "download")),
    shiny::br(),
    shiny::br(),
    shiny::uiOutput( # ui dynamique du graphique defini plus tard
      outputId = ns(
        "ui_graphique_suivi" ) )
  ) }


#' Serveur du module de suivi
#' @param id Identifiant du module.
#' @param donnees Données chargées dans l'application.
#' @param station_selectionnee Station actuellement sélectionnée.
#' @noRd

mod_suivi_server <- function( id,
                              donnees,
                              station_selectionnee) {
  shiny::moduleServer( id, function(input, output, session) {

  # Préparation des données nécessaires au module
      donnees_suivi <- shiny::reactive({
        shiny::req(donnees())# Attend que les données soient chargées
        shiny::req(station_selectionnee()) # Attend qu'une station soit sélectionnée
        etat_bio <- donnees()$etat_bio  # Récupération des tables
        stations <- donnees()$stations

        code_station_selectionnee <- as.character(station_selectionnee() ) # Code station en texte
        stations_verification <- stations # Creation d'une copie de la table
        if (inherits(stations_verification, "sf")) {# Verification si la table est un object spatial
          stations_verification <- # Supression temporaire de la géometrie
            sf::st_drop_geometry(
              stations_verification ) }

        reseau_station <- stations_verification |> # Recuperation du réseau de la station sel
          dplyr::filter( # Sélection de la station
            as.character(
              .data$code_station ) == code_station_selectionnee) |>
          dplyr::pull( # Extraction de la colonne reseau
            .data$reseau)
        shiny::validate( # Vérification de la présence d'un réseau
          shiny::need(
            length(reseau_station) > 0 &&
              any(
                !is.na(reseau_station) &
                  reseau_station != ""),
            paste(
              "Aucun réseau n'est renseigné pour la station",
              code_station_selectionnee) ) )

        shiny::validate( # Vérification de la présence de données biologiques
          shiny::need(
            code_station_selectionnee %in%
              as.character(
                etat_bio$code_station),
            paste(
              "Aucune donnée biologique n'est disponible",
              "pour la station",
              code_station_selectionnee) ) )

        return( # Retour des données nécessaires
          list(
            etat_bio = etat_bio,
            stations = stations,
            station = code_station_selectionnee,
            reseau = reseau_station) ) } )


# Calcul du nombre de réseaux associés à la station sélectionnée
      nombre_reseaux <- shiny::reactive({
        tables <- donnees_suivi() # Recup des données preparer avant
        texte_reseaux <- paste( # Regroupement des réseaux en une ligne
          as.character(
            tables$reseau ),
          collapse = "/" )

        reseaux <- stringr::str_split( # Séparation des différents réseaux
          string = texte_reseaux, # Texte contenant les réseaux
          pattern = "[/-]" ) |> # Caractère séparant les réseaux
          unlist() |> # Transformation de la liste en vecteur
          stringr::str_trim() |> # Suppression des espaces inutiles
          stringr::str_to_upper() # Passage des noms en majuscules
        reseaux <- unique( # Supression des NA, doublons
          reseaux[
            !is.na(reseaux) &
              reseaux != ""] )
        return(length(reseaux) ) } ) # Retourne le nombre de réseaux


 # Création dynamique de l'ui du graphique
      output$ui_graphique_suivi <-
        shiny::renderUI({
          hauteur_graphique <- max( # Calcule la hauteur du graphique selon le nombre de réseaux car si 2 réseaux = 2 graph
            550, # Hauteur minimale
            nombre_reseaux() * 420) # Hauteur ajoutée pour chaque réseau
          shinycssloaders::withSpinner( # Création du graph + indique que le graph charge
            shiny::plotOutput(
              outputId = session$ns(
                "graphique_suivi" ), # Identifiant du graphique dans le module
              width = "100%", # Largeur du graphique
              height = paste0(
                hauteur_graphique, # Hauteur
                "px") ) ) } )


# Création du graphique de suivi
      graphique_suivi <- shiny::reactive({
        tables <- donnees_suivi() # Récup des tables
        graphique <- fun_plot_suivi_alluvial( # Appel de la fonction pour creer le graph
          etat_bio = tables$etat_bio,
          stations = tables$stations, #
          station_selectionnee = tables$station)
        return(graphique) } ) # Retour du graphique


# Affichage du graphique dans l'application
      output$graphique_suivi <-shiny::renderPlot({
          graphique_suivi() },
        res = 96) # Résolution du graphique


# Export du graphique au format PNG
      output$export_graphique_suivi <- shiny::downloadHandler(
        filename = function() { # Création du nom
          paste0(
            "suivi_etat_biologique_station_",
            donnees_suivi()$station,
            ".png" ) },
        content = function(file) { # Création du fichier
          nombre_reseaux_export <- nombre_reseaux()    # Récupération du nombre de réseaux affichés
          hauteur_export <- max(  # Calcul de la hauteur de l'image selon le nombre de réseaux
            7, # Hauteur minimale
            nombre_reseaux_export * 5 )# Hauteur ajoutée pour chaque réseau

          ggplot2::ggsave( # Enregistrement du graphique
            filename = file,
            plot = graphique_suivi(),
            device = "png",
            width = 16, # Largeur de l'image
            height = hauteur_export,
            units = "in",
            dpi = 300, # Résolution de l'image
            bg = "white") } ) # Couleur de fond de l'image
    } ) }
