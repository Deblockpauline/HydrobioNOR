#' Module UI plan d'échantillonnage
#'
#' @noRd

mod_plan_echantillonnage_ui <- function(id) {
  ns <- shiny::NS(id) # Crée un NS
  shiny::tagList( # Regroupe plusieurs éléments de l'interface
    shiny::h4("Plan d'échantillonnage"), # Titre de la section
    shiny::selectInput( # Menu déroulant pour choisir l'eqb
      inputId = ns("choix_eqb_plan"), # Identifiant du sélecteur
      label = "Élément biologique", # Texte affiché au-dessus du menu
      choices = c("Diatomées", "Macroinvertébrés", "Macrophytes"), # Liste des choix possibles
      selected = "Diatomées"), # Valeur sélectionnée par défaut
    shiny::br(), # Saut de ligne pour aérer l'interface
    shiny::uiOutput(ns("message_plan")), # Zone d'affichage des messages d'information
    shiny::uiOutput(ns("ui_plan_classique")), # Zone affichant les contenus Diatomées ou Macroinvertébrés
    shiny::uiOutput(ns("ui_plan_macrophytes")) # Zone affichant les contenus spécifiques aux Macrophytes
  ) }

#' Module server plan d'échantillonnage
#'
#' @noRd

mod_plan_echantillonnage_server <- function(id,
                                            donnees,
                                            station_selectionnee,
                                            choix_eqb) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns # Récupère l'espace de noms du module

# Synchronisation avec le filtre EQB
    shiny::observeEvent(choix_eqb(), {
      eqb_general <- choix_eqb() # Récupère l'EQB choisi dans le filtre global
      if (eqb_general %in% c("Diatomées", "Macroinvertébrés", "Macrophytes")) { # Si c'est un de ces 3 la, cet observe met a jour avec la selection
        shiny::updateSelectInput(
          session = session,
          inputId = "choix_eqb_plan", # Menu déroulant du plan d'échantillonnage
          selected = eqb_general) }  # Met le même choix que le filtre global
    }, ignoreInit = FALSE) # Applique aussi le filtre au chargement

# Construction de l'interphase ui des macrophyte car c'est different des autres (UR 1 ou UR 2)
    ui_macro_bloc <- function(prel, ns) {
      shiny::tagList( # Regroupe tous les éléments du bloc
        shiny::h5("Graphique général macrophytes"), # Titre du graphique général
        shiny::br(), # Saut de ligne
        shiny::br(), # Saut de ligne
        plotly::plotlyOutput(
          outputId = ns(paste0("graph_macro_courbes_", prel)), # Zone du graphique général
          height = "500px"), # Hauteur du graphique

        shiny::hr(), # Ligne de séparation
        shiny::h5("Graphiques en tuiles"), # Titre des graphiques en tuiles
        shiny::br(), # Saut de ligne
        shiny::br(), # Saut de ligne
        plotly::plotlyOutput(
          outputId = ns(paste0("graph_macro_tuiles_", prel)), # Zone du graphique en tuiles
          height = "1600px"), # Hauteur du graphique

        shiny::hr(), # Ligne de séparation
        shiny::h5("Tableau unité de relevé macrophytes"), # Titre du tableau unité
        shiny::downloadButton(
          outputId = ns(paste0("download_macro_unite_", prel)), # Bouton de téléchargement du tableau unité
          label = "Télécharger le tableau unité (.csv)" ),# Texte du bouton
        shiny::br(), # Saut de ligne
        shiny::br(), # Saut de ligne
        DT::DTOutput(ns(paste0("table_macro_unite_", prel))), # Zone du tableau unité

        shiny::hr(), # Ligne de séparation
        shiny::h5("Tableau global des tuiles macrophytes"), # Titre du tableau global
        shiny::downloadButton(
          outputId = ns(paste0("download_macro_global_", prel)), # Bouton de téléchargement du tableau global
          label = "Télécharger le tableau global (.csv)" ), # Texte du bouton
        shiny::br(), # Saut de ligne
        shiny::br(), # Saut de ligne
        DT::DTOutput(ns(paste0("table_macro_global_", prel))) # Zone du tableau global
      ) }


# Préparation des données du graph diat ou INV (autre reactive pour les macrophytes)
    donnees_graph_plan <- shiny::reactive({ # Prépare les données du graphique pour diatomées ou macroinvertébrés

      if (input$choix_eqb_plan == "Diatomées") { # Si l'utilisateur choisit les diatomées
        shiny::req(donnees()$plan_diatomees) # Vérifie que la table plan_diat existe
        fun_prep_plan_diat( # Appel de la fonction de preparation
          df = donnees()$plan_diatomees, # Données du plan diatomées
          code_station_selectionne = station_selectionnee() ) # Station sélectionnée

      } else if (input$choix_eqb_plan == "Macroinvertébrés") { # Si l'utilisateur choisit les macroinvertébrés
        shiny::req(donnees()$plan_inv_facies) # Vérifie les 2 tables existent
        shiny::req(donnees()$plan_inv_phases)
        fun_prep_plan_inv_graph( # Appel de la fonction de preparation pour les INV
          plan_inv_facies = donnees()$plan_inv_facies, # Données des substrats
          plan_inv_phases = donnees()$plan_inv_phases, # Données des phases
          code_station_selectionne = station_selectionnee() )# Station sélectionnée

      } else { NULL } } )# Ne renvoie rien pour les macrophytes ici


# Préparation des données pour les macrophytes
    donnees_macro_station <- shiny::reactive({
      shiny::req(input$choix_eqb_plan == "Macrophytes") # Lance seulement si Macrophytes est sélectionné
      shiny::req(donnees()$plan_macrophytes) # Vérifie que la table plan_macrophytes existe
      shiny::req(station_selectionnee()) # Vérifie qu'une station est sélectionnée
      fun_prep_plan_macro_station( # Appel la fonction de preparation
        plan_macrophytes = donnees()$plan_macrophytes, # Table macrophytes
        code_station_selectionne = station_selectionnee() ) } ) # Station sélectionnée


# Preparation des tableaux

    # Tableau 1 : contient pour les diat l'annee, vitesse et substrat et pour les INV l'occurence par substrats
    table_plan_1 <- shiny::reactive({ # Prépare le premier tableau selon l'EQB choisi

      if (input$choix_eqb_plan == "Diatomées") { # Cas diatomées
        fun_table_plan_diat(donnees_graph_plan()) # Prépare le tableau diatomées en appellant la fonction et en injectant les données preparés

      } else if (input$choix_eqb_plan == "Macroinvertébrés") { # Cas macroinvertébrés
        fun_table_plan_inv_facies( # Appel la fonction pour le tableau et injecte:
          plan_inv_facies = donnees()$plan_inv_facies, # La table des substrats et recouvrements
          code_station_selectionne = station_selectionnee() )# Et la station sélectionnée

      } else { NULL } } ) # Aucun tableau 1 pour les macrophytes ici car structure diffrentes


    # Tableau 2: 2eme tableau uniquement pour les INV qui va presenter les phases
    table_plan_2 <- shiny::reactive({ # Prépare le tableau des phases macroinvertébrés
      if (input$choix_eqb_plan == "Macroinvertébrés") { # Lance seulement pour les macroinvertébrés
        fun_table_plan_inv_phases(
          plan_inv_phases = donnees()$plan_inv_phases, # Table des phases
          code_station_selectionne = station_selectionnee()) # Station sélectionnés
      } else { NULL } } ) # Pas de tableau 2 pour les autres EQB


# Messages d'informations
    output$message_plan <- shiny::renderUI({ # Affiche le message d'information du module

      if (is.null(station_selectionnee())) { # Si aucune station n'est sélectionnée
        shiny::div(
          style = "color:#777; font-style:italic;", # Style du message
          "Cliquez sur une station pour afficher le plan d'échantillonnage.") # Message affiché

        # Cas des macrophytes
      } else if (input$choix_eqb_plan == "Macrophytes") {
        df <- donnees_macro_station() # Récupère les données macrophytes
        if (is.null(df) || nrow(df) == 0) { # Si aucune donnée n'est disponible
          shiny::div(
            style = "color:#777; font-style:italic;",
            "Aucune donnée de plan d'échantillonnage macrophytes disponible pour cette station." )# Message affiché
        } else {
          shiny::div( # Si oui il y a des données
            style = "color:#777; font-style:italic;",
            paste("Station sélectionnée :", station_selectionnee()) ) }# Affiche le code station

        # Pour diat et INV
      } else if (is.null(donnees_graph_plan()) || nrow(donnees_graph_plan()) == 0) { # Si aucune donnée classique n'est disponible
        shiny::div(
          style = "color:#777; font-style:italic;",
          "Aucune donnée de plan d'échantillonnage disponible pour cette station." ) # Message affiché
      } else {
        shiny::div( # Si oui, il y a des données
          style = "color:#777; font-style:italic;", # Style du message
          paste("Station sélectionnée :", station_selectionnee()) ) }# Affiche le code station
    } )

# A partir d'ici, on s'occupe seulement des diat et INV.

# Interphase classqiue pour diat et INV
    output$ui_plan_classique <- shiny::renderUI({ # Affiche l'interface classique diatomées / macroinvertébrés
      if (input$choix_eqb_plan == "Macrophytes") {return(NULL)} # Si macrophytes est sélectionné, affiche rien

      shiny::tagList( # Regroupe les éléments classiques
        shiny::uiOutput(ns("titre_graph_plan")), # Titre du graphique
        shiny::uiOutput(ns("ui_download_graph_plan")), # Bouton de téléchargement du graphique
        shiny::uiOutput(ns("ui_graph_plan")), # Zone du graphique
        shiny::uiOutput(ns("legende_phases_inv")), # Légende des phases
        shiny::hr(), # Ligne de séparation
        shiny::uiOutput(ns("titre_table_plan_1")), # Titre du tableau 1
        shiny::downloadButton(
          outputId = ns("download_table_plan_1"), # Bouton de téléchargement du tableau 1
          label = "Télécharger le tableau (.csv)" ), # Texte du bouton
        shiny::br(), # Saut de ligne
        shiny::br(), # Saut de ligne
        DT::DTOutput(ns("table_plan_1")), # Tableau 1
        shiny::br(), # Saut de ligne
        shiny::uiOutput(ns("ui_table_plan_2") ) ) } ) # Tableau 2 si macroinvertébrés


# Titre des graph pour les diat et les INV
    output$titre_graph_plan <- shiny::renderUI({
      if (input$choix_eqb_plan == "Diatomées") { # Cas diatomées
        shiny::h5("Graphique du plan d'échantillonnage diatomées") # Titre diatomées
      } else if (input$choix_eqb_plan == "Macroinvertébrés") { # Cas macroinvertébrés
        shiny::h5("Graphique du plan d'échantillonnage macroinvertébrés") # Titre macroinvertébrés
      } else {NULL} } )# Aucun titre sinon


# Bouton de telechargement
    output$ui_download_graph_plan <- shiny::renderUI({
      if (input$choix_eqb_plan == "Macroinvertébrés") { # Bouton seulement pour les macroinvertébrés
        shiny::tagList(
          shiny::downloadButton(
            outputId = ns("download_graph_plan"), # Bouton de téléchargement du graphique
            label = "Télécharger le graphique (.png)"),# Texte du bouton
          shiny::br(), # Saut de ligne
          shiny::br() )# Saut de ligne
      } else { NULL } } )# Pas de bouton pour les diatomée


# Creation de l'emplacement des graphiques
    output$ui_graph_plan <- shiny::renderUI({
      if (input$choix_eqb_plan == "Diatomées") { # Si diatomées
        plotly::plotlyOutput(
          outputId = ns("graph_plan_diat"), # Graphique interactif diatomées
          height = "550px") # Hauteur du graphique

      } else if (input$choix_eqb_plan == "Macroinvertébrés") { # Si macroinvertébrés
        shiny::plotOutput(
          outputId = ns("graph_plan_inv"), # Graphique ggplot macroinvertébrés
          height = "700px") # Hauteur du graphique
      } else { NULL}  } ) # Aucun graphique sinon


# Création de la petites légendes pour les phases chez les INV
    output$legende_phases_inv <- shiny::renderUI({
      if (input$choix_eqb_plan == "Macroinvertébrés") { # Légende seulement pour les macroinvertébrés
        shiny::tags$div(
          style = "margin-top:8px; margin-bottom:15px; font-size:13px;", # Style de la légende
          shiny::tags$strong("Phases : "), # Texte en gras
          shiny::tags$span(" / = Phase A "), # Légende phase A /
          shiny::tags$span(" | \\ = Phase B "), # Légende phase B \
          shiny::tags$span(" | • = Phase C")) # Légende phase C un point le tout spérater par |
      } else {NULL } } )# Pas de légende pour les autres EQB


# Création des graphiques

     # Diat
    output$graph_plan_diat <- plotly::renderPlotly({
      shiny::req(input$choix_eqb_plan == "Diatomées") # Vérifie que diatomées est sélectionné
      shiny::req(!is.null(station_selectionnee())) # Vérifie qu'une station est sélectionnée
      df <- donnees_graph_plan() # Récupère les données du graphique
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0, # Vérifie qu'il y a des données
          "Aucune donnée disponible pour cette station." ) )# Message si pas de données
      fun_plot_plan_diat(df) } )# Crée le graphique diatomées en appellant la fonction

    # INV, meme logique
    output$graph_plan_inv <- shiny::renderPlot({
      shiny::req(input$choix_eqb_plan == "Macroinvertébrés")
      shiny::req(!is.null(station_selectionnee()))
      df <- donnees_graph_plan()
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0,
          "Aucune donnée disponible pour cette station." ) )
      fun_plot_plan_inv( # Appel de la fonction
        df = df, # Données préparées
        code_station_selectionne = station_selectionnee() ) } )# Station sélectionnée


# Telechargement du graphique INV (car c'est un ggplot et non un graphique ploty)
    output$download_graph_plan <- shiny::downloadHandler(
      filename = function() { # Crée le nom du fichier
        paste0(
          "plan_echantillonnage_macroinvertebres_", # Début du nom
          station_selectionnee(), # Code station
          ".png") }, # Extension
      content = function(file) { # Crée le contenu du fichier
        p <- fun_plot_plan_inv( # Rappel la fonction avec les donnèes a injecter
          df = donnees_graph_plan(),
          code_station_selectionne = station_selectionnee())
        ggplot2::ggsave(
          filename = file, # Chemin du fichier
          plot = p, # Graphique à enregistrer
          width = 14, # Largeur
          height = 8, # Hauteur
          dpi = 300) } ) # Résolution

# Tableaux

    # Titre du 1er tableau
    output$titre_table_plan_1 <- shiny::renderUI({
      if (input$choix_eqb_plan == "Diatomées") { # Cas diatomées
        shiny::h5("Tableau du plan d'échantillonnage diatomées") # Titre diatomées
      } else if (input$choix_eqb_plan == "Macroinvertébrés") { # Cas macroinvertébrés
        shiny::h5("Tableau des substrats et recouvrements") # Titre macroinvertébrés
      } else { NULL } } ) # Aucun titre sinon


    # Affichage du 1er tableau
    output$table_plan_1 <- DT::renderDT({
      shiny::req(input$choix_eqb_plan %in% c("Diatomées", "Macroinvertébrés")) # Lance seulement pour ces deux EQB
      shiny::req(!is.null(station_selectionnee())) # Vérifie qu'une station est sélectionnée
      df <- table_plan_1() # Récupère les données du tableau
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0, # Vérifie qu'il y a des données
          "Aucune donnée disponible pour cette station.") ) # Message si pas de données

      if (input$choix_eqb_plan == "Macroinvertébrés") { # Mise en forme spéciale macroinvertébrés
        DT::datatable(
          df, # Données du tableau
          rownames = FALSE, # Enlève les numéros de lignes
          options = list(
            pageLength = 10, # Nombre de lignes affichées
            scrollX = TRUE, # Active le défilement horizontal
            columnDefs = list(
              list( # Colonne qui sert a mettre en gras caché
                targets = which(names(df) == "recouvrement_num") - 1,
                visible = FALSE ) ) ) ) |># Cache
          DT::formatStyle( # Mettre en gras les dominants
            "Recouvrement", # Colonne à mettre en forme
            valueColumns = "recouvrement_num", # Colonne utilisée pour la condition
            fontWeight = DT::styleInterval(
              4, # Seuil de recouvrement
              c("normal", "bold") ) ) # Normal si < 5, gras si => 5

      } else { # Si c'est les diat
        DT::datatable(
          df, # Données du tableau
          rownames = FALSE, # Enlève les numéros de lignes
          options = list(
            pageLength = 10, # Nombre de lignes affichées
            scrollX = TRUE) ) } } ) # Active le défilement horizontal


    # Interphase du 2eme tableaux pour les INV
    output$ui_table_plan_2 <- shiny::renderUI({
      if (input$choix_eqb_plan == "Macroinvertébrés") { # Seulement pour les macroinvertébrés
        shiny::tagList(
          shiny::h5("Tableau des phases prélevées"), # Titre du tableau phases
          shiny::downloadButton(
            outputId = ns("download_table_plan_2"), # Bouton de téléchargement du tableau phases
            label = "Télécharger le tableau phases (.csv)"), # Texte du bouton
          shiny::br(), # Saut de ligne
          shiny::br(), # Saut de ligne
          DT::DTOutput(ns("table_plan_2")) ) # Zone du tableau phases
      } else {NULL} } ) # Pas de tableau phases pour les autres EQB


    # Affichage du 2eme tableau
    output$table_plan_2 <- DT::renderDT({
      shiny::req(input$choix_eqb_plan == "Macroinvertébrés") # Lance seulement pour les macroinvertébrés
      shiny::req(!is.null(station_selectionnee())) # Vérifie qu'une station est sélectionnée
      df <- table_plan_2() # Récupère les données du tableau phases
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0,
          "Aucune donnée disponible pour cette station.") ) # Message si pas de données
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE) ) } )


# Export des tableaux

     # Tableau 1
    output$download_table_plan_1 <- shiny::downloadHandler(
      filename = function() { # Crée le nom du fichier
        paste0(
          "plan_echantillonnage_", # Début du nom
          stringr::str_to_lower(input$choix_eqb_plan), # Met l'EQB en minuscules
          "_", # Séparateur
          station_selectionnee(), # Code station
          "_tableau_1.csv" )},# Fin du nom
      content = function(file) { # Crée le contenu du fichier
        df <- table_plan_1() # Récupère le tableau 1
        if (input$choix_eqb_plan == "Macroinvertébrés") { # Si macroinvertébrés
          df <- df |> dplyr::select(-recouvrement_num)} # Enlève la colonne technique cachée
        utils::write.csv2(
          df, # Tableau à exporter
          file = file, # Chemin du fichier
          row.names = FALSE, # Ne garde pas les numéros de lignes
          fileEncoding = "UTF-8") } ) # Encodage du fichier


    # Tableau 2
    output$download_table_plan_2 <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "plan_echantillonnage_macroinvertebres_", # Début du nom
          station_selectionnee(), # Code station
          "_phases.csv") }, # Fin du nom
      content = function(file) { # Crée le contenu du fichier
        utils::write.csv2(
          table_plan_2(), # Tableau des phases à exporter
          file = file,
          row.names = FALSE,
          fileEncoding = "UTF-8" ) } )


# Partie macrophyte

# Interohase utilisateur spécialement pour les macrophytes
    output$ui_plan_macrophytes <- shiny::renderUI({ #
      if (input$choix_eqb_plan != "Macrophytes") { return(NULL)} # Si macrophytes n'est pas sélectionné, n'affiche rien
      shiny::req(!is.null(station_selectionnee())) # Vérifie qu'une station est sélectionnée
      df <- donnees_macro_station() # Récupère les données macrophytes
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0, # Vérifie qu'il y a des données
          "Aucune donnée macrophytes disponible pour cette station.")) # Message si pas de données

      # On va regarder si il y a 1 ou 2 UR
      prels <- df |>
        dplyr::distinct(prels_elem) |> # Garde les prélèvements différents
        dplyr::arrange(prels_elem) |> # Trie les prélèvements
        dplyr::pull(prels_elem) # Récupère la colonne en vecteur
      prels <- prels[!is.na(prels)] # Enlève les valeurs manquantes

      # Si que 1 UR
      if (length(prels) == 1) {
        shiny::tagList(
          shiny::h4(paste("Prélèvement élémentaire", prels[1])), # Titre du prélèvement
          ui_macro_bloc(prels[1], ns = ns)) # Affiche le bloc du prélèvement

        # Si 2 UR
      } else {
        shiny::fluidRow( # Affiche deux colonnes si deux prélèvements existent
          shiny::column(
            width = 6, # Largeur de la première colonne
            shiny::h4("Prélèvement élémentaire 1"), # Titre du prélèvement 1
            ui_macro_bloc("1", ns = ns)), # Bloc du prélèvement 1
          shiny::column(
            width = 6, # Largeur de la deuxième colonne
            style = "border-left:2px solid #BDBDBD;", # Ajoute une séparation verticale
            shiny::h4("Prélèvement élémentaire 2"), # Titre du prélèvement 2
            ui_macro_bloc("2", ns = ns) ) ) } # Bloc du prélèvement 2
    } )

# Boucles pour cerre les sorties des UR 1 et 2
    purrr::walk(c("1", "2"), function(prel) {

      # Filtre les données pour 1 UR donnée
      donnees_macro_prel <- shiny::reactive({
        donnees_macro_station() |> # Données préparé au haut du script
          dplyr::filter(prels_elem == prel) } ) # Garde uniquement le prélèvement concerné

      # Graphique des caractérisiques des UR
      output[[paste0("graph_macro_courbes_", prel)]] <- plotly::renderPlotly({
        shiny::req(input$choix_eqb_plan == "Macrophytes") # Lance seulement si macrophytes est sélectionné
        df <- donnees_macro_prel() # Récupère les données du prélèvement
        annees_station <- donnees_macro_station() |>
          dplyr::mutate(
            annee = as.numeric(as.character(annee))) |>
          dplyr::pull(annee) |>
          unique() |>
          sort()
        p <- fun_plot_macro_courbes( # Appel de la fonction de courbe
          df_macro = df,
          annees_station = annees_station  )
        shiny::validate(
          shiny::need(
            !is.null(p), # Vérifie que le graphique existe
            "Aucune donnée disponible pour le graphique général." ))# Message si pas de graphique
        plotly::ggplotly(p, tooltip = "text") |> # Ren le graph interactif
          plotly::config( # Nom de l'export
            toImageButtonOptions = list(
              format = "png",
              filename = paste0(
                "plan_echantillonnage_macrophytes_",
                station_selectionnee(),
                "_prelevement_",
                prel,
                "_graphique_general"),
              height = 1200,
              width = 800,
              scale = 2 ) ) } )

      # Graphique en tuiles des parametres
      output[[paste0("graph_macro_tuiles_", prel)]] <- plotly::renderPlotly({
        shiny::req(input$choix_eqb_plan == "Macrophytes") # Lance seulement si macrophytes est sélectionné
        p <- fun_plot_macro_tuiles(donnees_macro_prel()) # Appel la fonction pour crée les graphiques en tuiles en injectant les données préparées
        shiny::validate(
          shiny::need(
            !is.null(p), # Vérifie que le graphique existe
            "Aucune donnée disponible pour les tuiles.")) # Message si pas de graphique
        plotly::ggplotly(p, tooltip = "text") |> # Rend le graph interactif
          plotly::config( # Pour le nom de l'export
            toImageButtonOptions = list(
              format = "png",
              filename = paste0(
                "plan_echantillonnage_macrophytes_",
                station_selectionnee(),
                "_prelevement_",
                prel,
                "_tuiles" ),
              height = 1600,
              width = 1200,
              scale = 2) ) } ) # Rend le graphique interactif


      # Tableau 1 : Caractéristique de l'UR
      output[[paste0("table_macro_unite_", prel)]] <- DT::renderDT({
        shiny::req(input$choix_eqb_plan == "Macrophytes") # Lance seulement si macrophytes est sélectionné
        df <- fun_table_macro_unite(donnees_macro_prel()) # Appel la fonction pour préparer le tableau unité
        shiny::validate(
          shiny::need(
            nrow(df) > 0, # Vérifie qu'il y a des lignes
            "Aucune donnée disponible pour ce tableau.") ) # Message si pas de données
        DT::datatable(
          df, # Données du tableau
          rownames = FALSE, # Enlève les numéros de lignes
          options = list(
            pageLength = 10, # Nombre de lignes affichées
            scrollX = TRUE) ) } ) # Active le défilement horizontal

      # Tableau 2: Concerne les parametres de l'UR
      output[[paste0("table_macro_global_", prel)]] <- DT::renderDT({
        shiny::req(input$choix_eqb_plan == "Macrophytes") # Lance seulement si macrophytes est sélectionné
        df <- fun_table_macro_global(donnees_macro_prel()) # Appel la fo,ction pour préparer le tableau global
        shiny::validate(
          shiny::need(
            nrow(df) > 0, # Vérifie qu'il y a des lignes
            "Aucune donnée disponible pour ce tableau.") ) # Message si pas de données
        DT::datatable(
          df,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE) ) } )


      # Export du Tableau 1
      output[[paste0("download_macro_unite_", prel)]] <- shiny::downloadHandler(
        filename = function() { # Crée le nom du fichier
          paste0(
            "plan_echantillonnage_macrophytes_", # Début du nom
            station_selectionnee(), # Code station
            "_prelevement_", # Texte prélèvement
            prel, # Numéro du prélèvement
            "_unite_releve.csv" ) }, # Fin du nom
        content = function(file) { # Crée le contenu du fichier
          utils::write.csv2(
            fun_table_macro_unite(donnees_macro_prel()), # Tableau unité à exporter
            file = file, # Chemin du fichier
            row.names = FALSE, # Ne garde pas les numéros de lignes
            fileEncoding = "UTF-8" ) } ) # Encodage du fichier

      # Export du Tableau 2
      output[[paste0("download_macro_global_", prel)]] <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "plan_echantillonnage_macrophytes_",
            station_selectionnee(),
            "_prelevement_",
            prel,
            "_global_tuiles.csv" ) } ,
        content = function(file) {
          utils::write.csv2(
            fun_table_macro_global(donnees_macro_prel()),
            file = file,
            row.names = FALSE,
            fileEncoding = "UTF-8") }
      ) } )
  } ) }

## À appeler dans l'UI
# mod_plan_echantillonnage_ui("plan_echantillonnage")

## À appeler dans le server
#  mod_plan_echantillonnage_server("plan_echantillonnage")
