#' Module UI des tendances temporelles des indices biologiques
#'
#' @description Interface du module présentant un graphique par EQB et son un tableau pour afficher les tendances.
#' @noRd

mod_tendance_ui <- function(id) {
  ns <- shiny::NS(id) # Création du namespace propre au module
  shiny::tagList( # Regroupe tous les éléments de l'interface
    shiny::h4( # Titre
      "Tendances temporelles des indices biologiques"),
    shiny::p( # Création d'un paragraphe explicatif
      paste(
        "Comparaison entre la tendance temporelle de la station sélectionnée",
        "et la tendance générale du ou des réseaux auxquels elle appartient.") ),
    shiny::uiOutput( # Message qi aucune station n'est selectionnée
      ns("message_selection_station")),
    shiny::br(),
    shiny::uiOutput( # Zone contennat les graph
      ns("zone_graphiques")),
    shiny::br(),
    shiny::h5( # Titre du tableau
      "Données utilisées pour le calcul des tendances"),
    shiny::downloadButton( # Création du bouton de téléchargement
      outputId = ns("download_table_tendances"),
      label = "Télécharger le tableau (.csv)" ),
    shiny::br(),
    shiny::br(),
    shiny::div( # Création d'un bloc contenant le tableau
      style = "width: 90%; margin: auto;", # Largeur de 90 % et centrage automatique
      DT::DTOutput(
        ns("table_tendances") ) ) ) }


#' Module server des tendances temporelles des indices biologiques
#'
#' @description Module serveur affichant, pour la station sélectionnée :
#' les observations de chaque compartiment biologique et leurs tendance
#' ainsi que la la tendance générale du ou des réseaux d'appartenance ;
#' @noRd

mod_tendance_server <- function( id,
                                 donnees,
                                 station_selectionnee) {

  shiny::moduleServer( id, function(input, output, session) {

# Message si aucune station est sélectionnée
      output$message_selection_station <- shiny::renderUI({
        if ( # Vérifie qu'aucune station est sélectionnée
          is.null(station_selectionnee()) ||
          station_selectionnee() == ""   ) {
          shiny::div( # Création du message
            style = "color: #666; font-style: italic;", # Texte gris et en italique
            paste(
              "Veuillez sélectionner une station sur la carte pour afficher les tendances temporelles.") )
        } else {NULL} } )  # Si une station est bien sélectionnée = aucun message


# Reactive calculant les tendances pour la station sélectionnée
      resultats_tendances <- shiny::reactive({
        shiny::req( donnees() )# Attend les données
        shiny::req( station_selectionnee()) # Attend la station selectionne
        shiny::validate( # Vérifie que les conditions nécessaires sont respectées
          shiny::need( # Première condition
            "etat_bio" %in% names(donnees()), # Vérifie que la table etat_bio existe
            "La table etat_bio est absente des données." ),# Message si elle est absente
          shiny::need( # Deuxième condition
            "stations" %in% names(donnees()), # Vérifie que la table stations existe
            "La table stations est absente des données.") ) # Message si elle est absente

        resultats <- fun_calcul_tendances_indices( # Appel de la fonction pour calculer
          etat_bio = donnees()$etat_bio, # Donne les tables necessaires
          stations = donnees()$stations,
          code_station_selectionnee = station_selectionnee() )# Transmission de la station sélectionnée
        shiny::validate( # Vérification
          shiny::need(
            length(resultats$graphiques) > 0, # Vérifie qu'au moins un graphique est disponible
            paste( # Message d'erreur
              "Aucune tendance temporelle ne peut être calculée pour cette station.") ) )
        return( resultats) } ) #  Retourne les résultats


# Reactive préparant le tableau affiché et exporté
      table_tendances_preparee <- shiny::reactive({
        resultats <- resultats_tendances() # Récuperation des résultats
        shiny::validate(
          shiny::need(
            !is.null(resultats$tableau) && # Vérifie que le tableau n'est pas NULL ET
              nrow(resultats$tableau) > 0, # Vérifie que le tableau contient au moins une ligne
            "Aucune donnée tabulaire disponible pour cette station.") ) # Message si le tableau est vide


# Préparation du tableau
        table_preparee <- resultats$tableau %>% # Utilisation du tableau produit par la fonction de calcul
          dplyr::mutate( # Modification de plusieurs colonnes du tableau
            valeur_observee = round( # Arrondi de la valeur
              .data$valeur_observee, # Colonne  à arrondir
              4 ), # Conservation de quatre chiffres après la virgule
            valeur_ajustee = round(
              .data$valeur_ajustee,
              4 ),
            borne_basse = round(
              .data$borne_basse,
              4),
            borne_haute = round(
              .data$borne_haute,
              4 ) ) %>%
          dplyr::select( # Sélectionne les colonnes et définit leur ordre
            code_station,
            libelle_station,
            compartiment,
            code_indice,
            annee,
            reseau,
            type_donnee,
            methode,
            valeur_observee,
            valeur_ajustee,
            borne_basse,
            borne_haute ) %>%
          dplyr::rename( # Rennome les colonnes
            "Code station" = .data$code_station,
            "Station" = .data$libelle_station,
            "Compartiment" = .data$compartiment,
            "Code indice" = .data$code_indice,
            "Annee" = .data$annee,
            "Reseau" = .data$reseau,
            "Type de donnee" = .data$type_donnee,
            "Methode" =  .data$methode,
            "Valeur observee" = .data$valeur_observee,
            "Valeur ajustee" = .data$valeur_ajustee,
            "Borne basse a 95 %" = .data$borne_basse,
            "Borne haute a 95 %" = .data$borne_haute )
        return(table_preparee) } )  # TRenvoie le tableau préparé


# Création dynamique des graphiques
      output$zone_graphiques <- shiny::renderUI({
        resultats <- resultats_tendances() # Récupère les résultats de la station
        codes_indices <- names(resultats$graphiques) # Récupere les noms  des indices dispo

        # Création d'un bloc pour chaque indice
        blocs_graphiques <- lapply( # Répète pour chaque code indice
          codes_indices, # Liste des codes d'indices disponibles
          function(code_indice_courant) {

            identifiant_graphique <- paste0( # Construction d'un identifiant pour chaque graph
              "graphique_", # Début
              code_indice_courant) # Ajout du code de l'indice

            local({ # Crée un environnement local pour formater
              code_indice_local <- code_indice_courant # Création d'une copie locale du code indice
              identifiant_local <- identifiant_graphique # Création d'une copie locale de l'identifiant
              output[[identifiant_local]] <- # Création d'une sortie avec un nom automatique
                plotly::renderPlotly({ # Génération du graphique Plotly

                  donnees_graphique <- # Récupération des données du graphique actuel
                    resultats_tendances()$graphiques[[ # Accès à la liste des graphiques
                      code_indice_local]] # Sélection de l'indice actuel

                   graphique <- # Stockage du graphique créé
                    fun_plot_tendance_indice( # Appel de la fonction de création du graphique
                      donnees_tendance = donnees_graphique) # Données de l'indice actuel

                  nom_compartiment <- # Création d'un nom du compartiment utilisable dans le nom du graphique exporter
                    donnees_graphique$compartiment %>% # Récupération du nom du compartiment
                    stringr::str_to_lower() # Passage de toutes les lettres en minuscules

                  nom_station <- # Création d'un code station utilisable dans le nom du graph exporter
                    as.character(station_selectionnee()) # Code de la station sélectionnée en caractere

                  plotly::ggplotly( # Conversion du graphique ggplot en Ploty
                    graphique,
                    tooltip = "text") %>% # Contenu affiché au passage de la souris
                    plotly::layout( # Modification de la légende
                      legend = list(
                        orientation = "h", # Affichage horizontal de la légende
                        x = 0, # Position horizontale à gauche
                        y = -0.2), # Position verticale sous le graphique
                      margin = list( # Définition des marges autour du graphique
                        l = 70, # Marge à gauche
                        r = 25, # Marge à droite
                        b = 100, # Marge en bas
                        t = 60) ) %>% # Marge en haut

                    plotly::config( # Configuration de  l'export
                      displaylogo = FALSE, # Suppression du logo Plotly
                      toImageButtonOptions = list( # Paramètres du bouton d'export en image
                        format = "png", # Format du fichier téléchargé
                        filename = paste0( # Construction du nom du fichier
                          "tendance_", # Début du nom du fichier
                          nom_compartiment, # Ajout du compartiment
                          "_", # Séparation
                          nom_station),  # Ajout du code de la station
                        height = 700, # Hauteur de l'image exportée en pixels
                        width = 1100, # Largeur de l'image exportée en pixels
                        scale = 1) ) } ) } ) # Échelle normale de l'image

            shiny::column( # Création d'une colonne encadré an haut des graphique
              width = 6, # Largeur de 6
              shiny::div( # Création d'un encadré autour du graphique
                style = paste(
                  "margin-bottom: 20px;", # Espace de 20 pixels sous le graphique
                  "padding: 10px;", # Espace intérieur autour du graphique
                  "background-color: white;", # Fond blanc
                  "border: 1px solid #dddddd;", # Bordure grise fine
                  "border-radius: 5px;" ),# Coins légèrement arrondis
                plotly::plotlyOutput( # Emplacement visible du graphique Plotly
                  outputId = session$ns( # Application du namespace du module
                    identifiant_graphique), # Identifiant dynamique du graphique
                  height = "500px") ) ) } ) # Hauteur du graphique dans l'application

         # Fin de la boucle pour chaque indice

        groupes_graphiques <- split( # Découpe la liste des graphiques en groupes de 2 pour les afficher 2 par 2
          blocs_graphiques,
          ceiling(seq_along(blocs_graphiques) / 2) )
        shiny::tagList(
          lapply( # Répète la création d'une ligne titre pour chaque groupe
            groupes_graphiques, # Groupes contenant au maximum deux graphiques
            function(groupe) { # Fonction exécutée pour chaque groupe
              shiny::fluidRow(groupe) } ) ) # Création de la ligne
      } )


# Tableau récapitulatif affiché sous les graphiques
      output$table_tendances <- DT::renderDT({
        table_affichee <- table_tendances_preparee() # Récupération du tableau préparé
        DT::datatable( # Transformation du tableau R en tableau intéractif
          table_affichee,
          rownames = FALSE, # N'affiche pas les numéros automatiques à gauche
          options = list( # Paramètres d'affichage du tableau
            pageLength = 10, # Affiche dix lignes par page
            scrollX = TRUE, # Active le défilement horizontal
            searching = FALSE, # Supprime la barre de recherche
            lengthChange = FALSE) ) # Empêche de modifier le nombre de lignes par page
        }, server = TRUE) # Traitement réalisé côté serveur


# Export du tableau récapitulatif
      output$download_table_tendances <- shiny::downloadHandler(
        filename = function() { # Nom du fichier
          code_station <- station_selectionnee() # Code de la station actuellement sélectionnée
          paste0(
            "tendances_indices_", # Début du nom du fichier
            code_station, # Ajout du code de la station
            ".csv") }, # Extension du fichier
        content = function(file) { # Contenu
          table_export <- table_tendances_preparee() # Même tableau que celui affiché
          if ( # Vérifie si :
            is.null(table_export) || # Le tableau n'existe pas
            nrow(table_export) == 0 # Le tableau ne contient aucune ligne
          ) { return( NULL)}# Arrête la fonction
          utils::write.csv2( # Écriture du tableau dans un fichier CSV
            table_export, # Tableau à exporter
            file,
            row.names = FALSE, # N'ajoute pas les numéros de lignes
            fileEncoding = "UTF-8") } ) # Encodage permettant de conserver les caractères français
    } ) }


## À appeler dans l'UI
# mod_tendance_ui("tendance")

## À appeler dans le server
# mod_tendance_server("tendance")
