#' Interface utilisateur du module diversité
#' @param id Identifiant du module.
#' @noRd

mod_diversite_ui <- function(id) { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments UI

    # Titre principal du module
    shiny::h4("Évolution des indices de diversité"),

    # Sélection des communautés affichées
    shiny::div(
      style = "width: 40%;", # Le sélecteur prend 40 % de la largeur
      shiny::selectInput(
        inputId = ns("choix_communaute"), # Identifiant de la sélection
        label = "Communautés affichées", # Texte affiché au-dessus
        choices = NULL, # Choix ajoutés depuis le serveur
        selected = NULL, # Sélection ajoutée depuis le serveur
        multiple = TRUE ) ), # Choix multiple
    shiny::br(), # Ajout d'un espace vertical

    # Graphique interactif centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # Le bloc prend 90 % de la largeur et est centré
      shiny::uiOutput(
        outputId = ns(
          "conteneur_graphique_diversite") ) ), # Conteneur dynamique du graphique
    shiny::br(), # Ajout d'un espace vertical
    shiny::hr(), # Trait de séparation
    shiny::br(), # Ajout d'un espace vertical

    # Titre du tableau
    shiny::h5("Valeurs des indices de diversité"),

    # Bouton de téléchargement du tableau
    shiny::downloadButton(
      outputId = ns("export_tableau"), # Identifiant du téléchargement
      label = "Télécharger le tableau (.csv)", # Texte du bouton
      icon = shiny::icon(
        "download") ), # Icône du bouton
    shiny::br(), # Ajout d'un espace vertical
    shiny::br(), # Ajout d'un espace vertical

    # Tableau interactif centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # Le tableau prend 90 % de la largeur et est centré
      DT::DTOutput(
        outputId = ns(
          "tableau_diversite") ) ) # Emplacement du tableau interactif
  ) }


#' Serveur du module diversité
#' @noRd

mod_diversite_server <- function( id,
                                  donnees,
                                  station_selectionnee) {

  shiny::moduleServer( id,function(input, output, session) {


# Récuperation des données de la station sel
      taxons_station <- shiny::reactive({
        shiny::req(donnees()) # Attend le chargement des données
        shiny::req(station_selectionnee()) # Attend la sélection d'une station
        shiny::validate(
          shiny::need(
            "taxons" %in% names(donnees()), # Vérifie la présence de la table taxons
            "La table taxons est absente des données." ) ) # Message affiché sinon

        table_taxons_station <- donnees()$taxons |> # Récupération de la table taxons
          dplyr::filter(
            code_station == station_selectionnee() ) # Garde uniquement la station sélectionnée
        shiny::validate(
          shiny::need(
            nrow(table_taxons_station) > 0, # Vérifie la présence d'au moins une ligne
            paste(
              "Aucune donnée taxonomique disponible",
              "pour cette station." ) ) ) # Message affiché sinon
        return(table_taxons_station) } ) # Retourne les taxons de la station


# Calcul des métriques de diversité
      diversite_station <- shiny::reactive({
        table_diversite <- fun_calcul_diversite( # Appel de la focntion de calcul
          table_taxons = taxons_station() ) # Utilise les taxons de la station
        return(table_diversite) } )


# Mise à jour des communautés disponibles
      shiny::observeEvent(diversite_station(),{ # Déclenchement lors du calcul des métriques
          communautes_disponibles <- diversite_station() |>
            dplyr::filter( # Filtre
              !is.na(libelle_support) ) |> # Supprime les communautés non renseignées
            dplyr::distinct( # Supprime les doublons
              libelle_support ) |> # Garde une seule ligne par communauté
            dplyr::arrange( # Classe les communautés
              libelle_support ) |> # Classement par ordre alphabétique
            dplyr::pull( # Transforme la colonne en vecteur
              libelle_support ) # Récupère les noms des communautés
          shiny::updateSelectInput( # Mise à jour de la liste de sélection
            session = session, # Session du module
            inputId = "choix_communaute", # Identifiant du sélecteur
            choices = communautes_disponibles, # Communautés proposées
            selected = communautes_disponibles ) }, # Sélectionne toutes les communautés par défaut
        ignoreInit = FALSE ) # Exécute aussi lors de l'initialisation


# Filtrage selon les communautés sélectionnées
      diversite_filtree <- shiny::reactive({
        shiny::req(input$choix_communaute) # Attend la sélection d'au moins une communauté
        table_diversite_filtree <- diversite_station() |> # Récupération des indices
          dplyr::filter(
            libelle_support %in%
              input$choix_communaute ) # Garde uniquement les communautés sélectionnées
        shiny::validate(
          shiny::need(
            nrow(table_diversite_filtree) > 0, # Vérifie la présence d'au moins une ligne
            paste(
              "Aucune donnée disponible pour",
              "les communautés sélectionnées." ) ) ) # Message affiché sinon
        return(table_diversite_filtree)} ) # Retourne les données filtrées


# Passage des indices au format long pour l'affichage
      diversite_longue <- shiny::reactive({
        fun_prep_diversite( # Appel la fonction
          table_diversite =
            diversite_filtree() ) } ) # Utilise la table de diversité filtrée


# Nombre de communautés affichées pour la hauteur du graph
      nombre_communautes <- shiny::reactive({
        nombre <- diversite_filtree() |> # Récupération des données filtrées
          dplyr::filter(
            !is.na(libelle_support) ) |> # Supprime les communautés non renseignées
          dplyr::distinct( # Supprime les doublons
            libelle_support ) |> # Garde une ligne par communauté
          nrow() # Compte le nombre de communautés
        return(nombre)} ) # Retourne le nombre de communautés


# Hauteur automatique du graphique
      hauteur_graphique <- shiny::reactive({
        hauteur <- nombre_communautes() * 245 # Calcule la hauteur selon le nombre de communautés pour que ca soit lisible et proportionnel
        hauteur <- max( # Garde la valeur la plus élevée
          hauteur, # Hauteur calculée
          300 ) # Hauteur minimale du graphique
        return(hauteur) } ) # Retourne la hauteur en pixels


# Création dynamique du ui du graphique
      output$conteneur_graphique_diversite <- shiny::renderUI({
        plotly::plotlyOutput( # Emplacement du graphique Plotly
          outputId = session$ns( # Ajout du namespace du module
            "graphique_diversite" ), # Identifiant du graphique
          height = paste0( # Création de la hauteur au format texte
            hauteur_graphique(), # Hauteur calculée
            "px" ) ) } ) # Ajout de l'unité en pixels


# Création du graphique ggplot
      graphique_diversite <- shiny::reactive({
        fun_plot_diversite( # Appel la fonction
          table_diversite_longue =
            diversite_longue() ) } ) # Utilise les données au format long


# Affichage sous forme ploty
      output$graphique_diversite <- plotly::renderPlotly({
        graphique_interactif <- plotly::ggplotly( # Conversion du graphique
          graphique_diversite(),
          tooltip = "text", # Informations affichées au survol
          dynamicTicks = FALSE ) # Conserve les graduations définies dans ggplot

        # Modif de la mise en page
        graphique_interactif <- plotly::layout(
          graphique_interactif,
          hovermode = "closest", # Affiche les informations du point le plus proche lordque la souris passe dessus
          margin = list( # Réglage des marges du graphique
            l = 45, # Marge à gauche
            r = 15, # Marge à droite
            b = 55, # Marge en bas
            t = 15 ) ) # Marge en haut

        # Ploty peut creer plusieur axes verticaux donc on les recupere et on les regles
        noms_axes_y <- names( graphique_interactif$x$layout )
        noms_axes_y <- noms_axes_y[ # Garde uniquement les axes verticaux
          grepl( # Recherche une expression dans les noms
            pattern = "^yaxis[0-9]*$", # Noms correspondant aux axes verticaux
            x = noms_axes_y ) ] # Noms analysés

        for (nom_axe_y in noms_axes_y) { # Répète le réglage pour chaque axe vertical
          graphique_interactif$x$layout[[
            nom_axe_y ]]$rangemode <- "tozero" # Force chaque axe vertical à commencer à zéro
          graphique_interactif$x$layout[[
            nom_axe_y ]]$automargin <- TRUE } # Ajuste automatiquement les marges

        # Meme chose pour les horizontaux
        noms_axes_x <- names( graphique_interactif$x$layout)
        noms_axes_x <- noms_axes_x[
          grepl(
            pattern = "^xaxis[0-9]*$",
            x = noms_axes_x ) ]

        for (nom_axe_x in noms_axes_x) { # Répète le réglage pour chaque axe horizontal
          graphique_interactif$x$layout[[
            nom_axe_x ]]$automargin <- TRUE # Ajuste automatiquement les marges
          graphique_interactif$x$layout[[
            nom_axe_x ]]$tickangle <- -45 # Incline les années à 45 degrés
          graphique_interactif$x$layout[[
            nom_axe_x ]]$tickfont <- list( # Modification du texte des graduations
            size = 8 ) } # Taille du texte des années

        # Pour l'export du graph
        nom_export <- paste0(  # Création du nom
          "diversite_station_",
          station_selectionnee() )
        hauteur_export <- max( # Garde la valeur la plus élevée pour la hauteur
          500, # Hauteur minimale de l'image
          nombre_communautes() *
            300 ) # Hauteur selon le nombre de communautés

        graphique_interactif <- plotly::config( # Configuration du graphique
          graphique_interactif,
          toImageButtonOptions = list( # Réglages de l'export en image
            format = "png", # Format de l'image
            filename = nom_export, # Nom du fichier téléchargé
            width = 1800, # Largeur de l'image
            height = hauteur_export, # Hauteur de l'image
            scale = 1 ), # Échelle de l'image
          displaylogo = FALSE ) # Supprime le logo Plotly

        return(graphique_interactif) } ) # Retourne le graphique interactif


# Affichage du tableau
      output$tableau_diversite <- DT::renderDT({
        table_affichage <- diversite_filtree() |> # Récupération des données filtrées
          dplyr::arrange( # Classe les données
            libelle_support,
            annee,
            date_prelevement ) |>
          dplyr::transmute( # Sélectionne et transforme les colonnes
            code_station = code_station,
            annee = annee,
            libelle_support = libelle_support,
            code_prelevement = code_prelevement,
            abondance_totale = abondance_totale,
            richesse_taxonomique = richesse_taxonomique,
            diversite_shannon = round( # Arrondi de l'indice de Shannon et Pielou
              diversite_shannon,
              digits = 3 ), # Trois chiffres après la virgule
            equitabilite_pielou = round( #
              equitabilite_pielou,
              digits = 3 ) )

        DT::datatable( # Création du tableau interactif
          data = table_affichage, # Données affichées
          rownames = FALSE, # Supprime les numéros de lignes
          filter = "none", # Supprime les filtres sous les colonnes
          options = list(
            pageLength = 10, # Nombre de lignes affichées par défaut
            lengthMenu = c(
              10,
              25,
              50,
              100 ), # Choix du nombre de lignes affichées
            scrollX = TRUE, # Ajoute un défilement horizontal
            autoWidth = TRUE) ) } ) # Ajuste automatiquement la largeur des colonnes


# Export du tableau au format CSV
      output$export_tableau <- shiny::downloadHandler(
        filename = function() { # Nom du fichier
          paste0( # Assemble les éléments du nom
            "diversite_station_", # Début du nom
            station_selectionnee(), # Code de la station
            "_", # Séparateur
            Sys.Date(), # Date du téléchargement
            ".csv" ) }, # Extension du fichier
        content = function(file) { # Contenu
          table_export <- diversite_filtree() |> # Récupération des données filtrées
            dplyr::arrange(
              libelle_support,
              annee,
              date_prelevement ) |>
            dplyr::transmute(
              code_station = code_station,
              libelle_station = libelle_station,
              annee = annee,
              code_prelevement = code_prelevement,
              code_support = code_support,
              libelle_support = libelle_support,
              abondance_totale = abondance_totale,
              richesse_taxonomique =richesse_taxonomique,
              diversite_shannon = round( # Arrondi
                diversite_shannon,
                digits = 3 ),
              equitabilite_pielou = round(
                equitabilite_pielou,
                digits = 3 ) )
          utils::write.csv2( # Écriture du fichier au format CSV
            x = table_export, # Table à exporter
            file = file, # Emplacement du fichier
            row.names = FALSE, # Supprime les numéros de lignes
            fileEncoding = "UTF-8" ) } ) # Encodage des caractères

  } ) }

## À appeler dans l'UI
# mod_diversite_ui("diversite")

## À appeler dans le SERVER
# mod_diversite_server ("diversite")

