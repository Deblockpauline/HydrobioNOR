#' Module UI diagnostic écologique SEEE
#' @description Ce module sert à afficher les graphiques et tableaux concernant le diagnostic.
#' Il fait appel aux fonctions définies dans fun_diagnostic_seee.
#' @noRd

mod_communaute_diagnostic_eco_ui <- function(id) {
  ns <- shiny::NS(id) # Création du namespace
  shiny::tagList( # Regroupe tous les éléments
    shiny::h4("Diagnostic écologique SEEE"), # Titre
    shiny::fluidRow( # Création d'une ligne

      # Affichage de l'EQB choisi et sélection des années
      shiny::column( # Première colonne
        width = 4, # Largeur de la colonne
        shiny::uiOutput(ns("ui_type_diag") ) ),# Affiche soit l'EQB choisi, soit un choix si EQB = Tous
      shiny::column( # Deuxième colonne
        width = 8, # Largeur de la colonne
        shiny::uiOutput(ns("ui_annees_diag") ) ) ), # Choix des années
    shiny::br(), # Saut de ligne
    shiny::uiOutput(ns("message_diag")), # Message d'information
    shiny::br(), # Saut de ligne

    # Mise en place des graphiques
    shiny::fluidRow(
      shiny::column( # Graphique des pressions chimiques
        width = 6, # Moitié de la largeur
        plotly::plotlyOutput( # Emplacement du graphique
          outputId = ns("radar_chimie"), # Identifiant du graphique
          height = "450px"), # Hauteur du graphique
        shiny::HTML( # Ajout d'une légende
          "<div style='font-size:10px; color:#666; line-height:1.25; margin-top:4px;'>
          <b>Légende :</b>
          MO = matières organiques ;
          Mat.N = matières azotées ;
          NO3 = nitrates ;
          Mat.P = matières phosphorées ;
          MES = matières en suspension ;
          HAP = hydrocarbures aromatiques polycycliques ;
          Pest. = moyenne des fongicides, herbicides et insecticides ;
          Micropoll. org. = micropolluants organiques et PCB ;
          Micropoll. min. = micropolluants minéraux ;
          Acidif. = acidification.
          </div>") ),

      shiny::column( # Graphique des dégradations de l'habitat
        width = 6, # Moitié de la largeur
        plotly::plotlyOutput( # Emplacement du deuxième graphique
          outputId = ns("radar_habitat"), # Identifiant du graphique habitat
          height = "450px"), # Hauteur du graphique
        shiny::HTML( # Légende des abréviations liées à l'habitat
          "<div style='font-size:10px; color:#666; line-height:1.25; margin-top:4px;'>
          <b>Légende :</b>
          Ripisylve = végétation rivulaire ;
          Colmatage = colmatage du substrat ;
          Voies com. = voies de communication ;
          Urbanisation = urbanisation ;
          Instab. hydro. = instabilité hydrologique ;
          Anthrop. BV = anthropisation du bassin versant.
          </div>") ) ),

     # Partie tableau
    shiny::br(), # Saut de ligne
    shiny::h4("Données"), # Titre de la partie tableau
    shiny::downloadButton( # Bouton de téléchargement
      outputId = ns("download_diagnostic"), # Identifiant du bouton
      label = "Télécharger les données (.csv)"), # Texte du bouton
    shiny::br(), # Saut de ligne
    shiny::br(), # Deuxième saut de ligne
    DT::DTOutput(ns("table_diagnostic") ) ) # Emplacement du tableau interactif
}


#' Module server diagnostic écologique SEEE
#' @noRd

mod_communaute_diagnostic_eco_server <- function(id,
                                                 donnees,
                                                 station_selectionnee,
                                                 choix_eqb) {

  shiny::moduleServer(id, function(input, output, session) {

    # Harmonisation du nom de l'EQB
    choix_eqb_filtre <- shiny::reactive({
      shiny::req(choix_eqb()) # Attend que le filtre global EQB existe
      dplyr::case_when( # Fais la correspondance
        choix_eqb() %in% c("Macroinvertébrés", "Macroinvertébrés benthiques") ~ "Macroinvertébrés",
        choix_eqb() %in% c("Diatomées", "Diatomées benthiques") ~ "Diatomées",
        choix_eqb() == "Tous" ~ "Tous",
        TRUE ~ as.character(choix_eqb() ) ) } )

    # EQB réellement utilisé pour le diagnostic
    choix_eqb_diag <- shiny::reactive({
      shiny::req(choix_eqb_filtre()) # Attend l'EQB
      if (choix_eqb_filtre() == "Tous") { # Si le filtre global est sur Tous
        shiny::req(input$choix_type_diag) # Attend le choix interne
        input$choix_type_diag # Utilise le choix fait dans l'onglet diagnostic
      } else { choix_eqb_filtre() } } ) # Sinon utilise directement le filtre global

    # Interface du choix d'EQB
    output$ui_type_diag <- shiny::renderUI({
      shiny::req(choix_eqb_filtre()) # Attend l'EQB harmonisé
      if (choix_eqb_filtre() == "Tous") { # Si tous les EQB sont sélectionnés
        shiny::selectInput( # On laisse choisir le diagnostic à afficher
          inputId = session$ns("choix_type_diag"), # Identifiant du champ
          label = "Élément biologique", # Texte affiché
          choices = c("Macroinvertébrés", "Diatomées"), # Choix possibles
          selected = "Macroinvertébrés") # Choix par défaut
      } else {
        shiny::div( # Sinon on affiche simplement l'EQB actif
          shiny::strong("Élément biologique : "),
          choix_eqb_filtre() ) } } )

    # Chargement des données
    donnees_diag <- shiny::reactive({
      shiny::req(donnees()) # Vérifie que les données sont chargées
      shiny::req(choix_eqb_diag()) # Vérifie qu'un EQB diagnostic est disponible
      shiny::validate( # Vérifie que le diagnostic existe pour cet EQB
        shiny::need(
          choix_eqb_diag() %in% c("Macroinvertébrés", "Diatomées"),
          "Le diagnostic SEEE est uniquement disponible pour les macroinvertébrés et les diatomées." ) )
      if (choix_eqb_diag() == "Macroinvertébrés") { # Si macroinvertébrés
        donnees()$diagnostic_inv # Utilise la table diagnostic des invertébrés
      } else { # Sinon diatomées
        donnees()$diagnostic_diat } } ) # Utilise la table diagnostic des diatomées

    # Données de la station sélectionnée
    donnees_station <- shiny::reactive({
      shiny::req(donnees_diag()) # Vérifie que la table diagnostic existe
      shiny::req(station_selectionnee()) # Vérifie qu'une station est sélectionnée
      donnees_diag() |> # Prend les données du diagnostic choisi
        dplyr::mutate( # Modifie ou crée certaines colonnes
          DATE = fun_date_seee(DATE), # Convertit la date SEEE
          annee = lubridate::year(DATE) ) |> # Extrait l'année
        dplyr::filter( # Filtre la station sélectionnée
          CODE_STATION == as.character(station_selectionnee()) ) } )

    # Sélecteur d'année
    output$ui_annees_diag <- shiny::renderUI({
      shiny::req(donnees_station()) # Vérifie que les données de la station existent
      annees <- donnees_station() |> # Récupère les années disponibles
        dplyr::filter(!is.na(annee)) |> # Retire les années manquantes
        dplyr::distinct(annee) |> # Garde chaque année une seule fois
        dplyr::arrange(annee) |> # Trie les années
        dplyr::pull(annee) # Transforme la colonne en vecteur
      shiny::validate( # Vérifie qu'il existe au moins une année
        shiny::need(
          length(annees) > 0,
          "Aucune année disponible pour cette station.") )

      shiny::selectizeInput( # Crée un menu de sélection multiple
        inputId = session$ns("choix_annees_diag"), # Identifiant du sélecteur
        label = "Année(s)", # Texte affiché
        choices = annees, # Années proposées
        selected = tail(annees, min(2, length(annees))), # Sélectionne les 2 dernières années
        multiple = TRUE, # Autorise plusieurs années
        options = list(
          placeholder = "Choisir une ou plusieurs années" ) ) } )

    # Données filtrées par année
    donnees_filtrees <- shiny::reactive({
      shiny::req(donnees_station()) # Vérifie que les données station existent
      shiny::req(input$choix_annees_diag) # Vérifie qu'au moins une année est sélectionnée
      donnees_station() |> # Prend les données de la station
        dplyr::filter(
          annee %in% as.numeric(input$choix_annees_diag) ) } ) # Garde seulement les années choisies

    # Message d'information
    output$message_diag <- shiny::renderUI({
      if (!choix_eqb_diag() %in% c("Macroinvertébrés", "Diatomées")) { # Si EQB non compatible
        shiny::div(
          style = "color:#666; font-style:italic;",
          paste(
            "Aucun diagnostic SEEE disponible pour l'EQB sélectionné :",
            choix_eqb_filtre() ) )
      } else if (is.null(station_selectionnee())) { # Si aucune station n'est sélectionnée
        shiny::div(
          style = "color:#666; font-style:italic;",
          "Cliquez sur une station pour afficher le diagnostic écologique." )
      } else if (nrow(donnees_station()) == 0) { # Si la station n'a aucune donnée SEEE
        shiny::div(
          style = "color:#666; font-style:italic;",
          paste(
            "Aucune donnée SEEE disponible pour la station",
            station_selectionnee() ) )
      } else { # Si une station est sélectionnée et possède des données
        shiny::div(
          style = "color:#666; font-style:italic;",
          paste(
            "Station sélectionnée :",
            station_selectionnee(),
            "- diagnostic",
            choix_eqb_diag() ) ) } } )

    # Graphique pour la chimie
    output$radar_chimie <- plotly::renderPlotly({
      shiny::req(donnees_filtrees()) # Vérifie que les données filtrées existent
      df_plot <- fun_preparer_radar_seee( # Prépare les données avec la fonction
        df = donnees_filtrees(), # Données utilisées
        type_diag = choix_eqb_diag(), # Type biologique choisi
        groupe = "chimie") # Groupe de variables à afficher
      shiny::validate( # Vérifie que des données existent pour ce radar
        shiny::need(
          nrow(df_plot) > 0,
          "Aucune pression chimique disponible." ) )

      fun_plot_diagnostic_seee( # Crée le graphique radar
        df = df_plot, # Données préparées
        titre = "Pressions chimiques", # Titre du graphique
        seuil = 0.6) |> # Seuil de significativité
        plotly::config( # Options d'export du graphique
          toImageButtonOptions = list(
            format = "png",
            filename = paste0(
              "diagnostic_chimie_",
              station_selectionnee(),
              "_",
              paste(input$choix_annees_diag, collapse = "-")),
            height = 800,
            width = 800,
            scale = 2) ) } )

    # Graphique habitat
    output$radar_habitat <- plotly::renderPlotly({
      shiny::req(donnees_filtrees()) # Vérifie que les données filtrées existent
      df_plot <- fun_preparer_radar_seee( # Prépare les données avec la fonction
        df = donnees_filtrees(), # Données utilisées
        type_diag = choix_eqb_diag(), # Type biologique choisi
        groupe = "habitat") # Groupe de variables habitat
      shiny::validate( # Vérifie qu'il existe des données habitat
        shiny::need(
          nrow(df_plot) > 0,
          "Aucune dégradation de l'habitat disponible." ) )

      fun_plot_diagnostic_seee( # Crée le graphique radar habitat
        df = df_plot, # Données préparées
        titre = "Dégradations de l'habitat", # Titre du graphique
        seuil = 0.6 ) |># Seuil de significativité
        plotly::config( # Options d'export du graphique
          toImageButtonOptions = list(
            format = "png",
            filename = paste0(
              "diagnostic_habitat_",
              station_selectionnee(),
              "_",
              paste(input$choix_annees_diag, collapse = "-") ),
            height = 800,
            width = 800,
            scale = 2 ) ) } )

    # Tableau
    output$table_diagnostic <- DT::renderDT({
      shiny::req(donnees_filtrees()) # Vérifie que les données filtrées existent
      donnees_filtrees() |> # Prend les données filtrées
        dplyr::mutate( # Modifie les colonnes numériques
          dplyr::across(
            where(is.numeric), # Sélectionne les colonnes numériques
            ~ round(.x, 3) ) ) |> # Arrondit à 3 décimales
        DT::datatable( # Crée le tableau interactif
          rownames = FALSE, # N'affiche pas les noms de lignes
          options = list(
            pageLength = 10, # Affiche 10 lignes par page
            scrollX = TRUE ) )# Active le défilement horizontal
    }, server = TRUE) # Traitement côté serveur pour alléger l'affichage

    # Téléchargement des données
    output$download_diagnostic <- shiny::downloadHandler(
      filename = function() { # Définit le nom du fichier téléchargé
        paste0(
          "diagnostic_SEEE_",
          choix_eqb_diag(),
          "_",
          station_selectionnee(),
          ".csv" ) },
      content = function(file) { # Définit le contenu du fichier CSV
        donnees_filtrees() |> # Prend les données filtrées
          dplyr::mutate( # Arrondit les colonnes numériques
            dplyr::across(
              where(is.numeric),
              ~ round(.x, 3) ) ) |>
          utils::write.csv2( # Écrit le fichier au format CSV avec séparateur ;
            file = file,
            row.names = FALSE,
            fileEncoding = "UTF-8" ) } )
  } ) }
