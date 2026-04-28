#' Module UI du contexte environnemental du bassin versant
#'
#' @description
#' Interface du module affichant l'évolution de l'occupation du sol
#' du bassin versant
#'
#' @param id Identifiant du module
#'
#' @noRd

mod_station_contexte_env_bv_ui <- function(id) {
  ns <- shiny::NS(id) # Definie le NS
  shiny::tagList(

    # Titre principal du module
    shiny::h4("Contexte environnemental du bassin versant"),

    # Message affiché tant qu'aucune station n'est sélectionnée
    shiny::uiOutput(ns("message_selection_station")),
    shiny::br(),

    # Affichage de la surface du bassin versant
    shiny::uiOutput(ns("surface_bv")),
    shiny::br(),# Retour a la ligne

    # Graphique interactif centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # Le bloc prend 90 % de la largeur et est centré
      plotly::plotlyOutput(
        outputId = ns("plot_occupation_bv"),
        height = "450px" ) ), # Hauteur du graphique
    shiny::br(),

    # Titre du tableau
    shiny::h5("Tableau récapitulatif de l'évolution de l'occupation du sol du bassin versant"),

    # Bouton de téléchargement du tableau
    shiny::downloadButton(
      outputId = ns("download_table_occupation_bv"),
      label = "Télécharger le tableau (.csv)" ),# Nom a afficher
    shiny::br(), # Retour a la ligne
    shiny::br(),

    # Tableau centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # 90% de la largeur et est centré
      DT::DTOutput(ns("table_occupation_bv") ) ), # Zone où s'affichera le tableau global
    shiny::br(),
    shiny::hr(), # Trait de séparation
    shiny::br(),

    # Titre de la partie détaillée
    shiny::h5("Répartition détaillée du bassin versant pour une année donnée"),

    # Sélecteur de l'année du camembert
    shiny::div(
      style = "width: 40%;", # Prend 40% de la largeur
      shiny::selectInput(
        inputId = ns("annee_bv_detail"), # Input qui servira a choisir l'année du camembert
        label = "Choisir une année", ,# Texte afficher
        choices = c("1990", "2000", "2006", "2012", "2018"), #Années possible
        selected = "2018" ) ), #Par defaut
    shiny::br(),

    # Camembert détaillé centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # Centre le camembert dans le panneau
      plotly::plotlyOutput(
        outputId = ns("plot_occupation_bv_detail"),# Zone où s'affichera le camembert
        height = "500px" ) ), #Hauteur
    shiny::br(),

    # Titre du tableau détaillé du camembert
    shiny::h5("Tableau récapitulatif des occupations détaillées pour l'année choisie"),

    # Bouton de téléchargement du tableau détaillé
    shiny::downloadButton(
      outputId = ns("download_table_occupation_bv_detail"),
      label = "Télécharger le tableau détaillé (.csv)" ),
    shiny::br(),
    shiny::br(),

    # Tableau détaillé centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;",
      DT::DTOutput(ns("table_occupation_bv_detail") ) )
  ) }

#' Module server du contexte environnemental du bassin versant
#'
#' @description
#' Module serveur affichant l'évolution de l'occupation du sol
#' du bassin versant associé à la station sélectionnée :
#' - graphique multicourbe interactif,
#' - tableau récapitulatif,
#' - export du tableau au format .csv,
#' - camembert détaillé pour une année choisie,
#' - tableau détaillé du camembert,
#' - export du tableau détaillé au format .csv.
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données de l'application
#' @param station_selectionnee Reactive contenant le code de la station sélectionnée
#'
#' @noRd

mod_station_contexte_env_bv_server <- function(id, donnees, station_selectionnee) {
  shiny::moduleServer(id, function(input, output, session) { # Lance la partie server du module

    # Message simple si aucune station n'est encore sélectionnée
    output$message_selection_station <- shiny::renderUI({
      if (is.null(station_selectionnee()) || station_selectionnee() == "") {
        shiny::div(
          style = "color: #666; font-style: italic;",
          "Veuillez sélectionner une station sur la carte pour afficher le contexte environnemental.")
      } else {NULL } })

# Preparation des réactives

    occupation_bv <- shiny::reactive({ # Réactive préparant les données du graphique + tableau
      shiny::req(donnees()) # Obligatoire : les données doivent être chargées
      shiny::req(station_selectionnee()) # Obligatoire : une station doit être sélectionnée

      fun_prep_bv_occupation( # Appel de la fonction qui prépare les données globales du BV
        donnees = donnees(), # Données de l'application
        station_id = station_selectionnee() ) } ) # Code de la station sélectionnée

    occupation_bv_detail <- shiny::reactive({ # Réactive préparant les données du camembert
      shiny::req(donnees()) # Obligatoire : les données doivent être chargées
      shiny::req(station_selectionnee()) # Obligatoire : une station doit être sélectionnée
      shiny::req(input$annee_bv_detail) # Obligatoire : une année doit être choisie

      fun_prep_bv_occupation_detail( # Appel de la fonction qui prépare les données détaillées
        donnees = donnees(), # Données de l'application
        station_id = station_selectionnee(), # Code de la station sélectionnée
        annee = input$annee_bv_detail ) } ) # Année choisie dans le selectInput

    surface_bv <- shiny::reactive({ # Reactive preparant les données pour afficher la surface
      shiny::req(donnees()) # Obligatoire : les données doivent etre chargées
      shiny::req(station_selectionnee()) # Obligatoire : une station doit etre selectionnée

      fun_prep_surface_bv( # Appel de la fonction
        donnees = donnees(),
        station_id = station_selectionnee())}) # Code de la station


#Developpement
    # Affichage de la surface
    output$surface_bv <- shiny::renderUI({
      surface_info <- surface_bv()
      shiny::tagList(
        shiny::p(
          shiny::strong("Identifiant du bassin versant : "),
          surface_info$id_bv[1]),
        shiny::p(
          shiny::strong("Surface du bassin versant : "),
          paste0(round(surface_info$surface_km2[1], 2), " km²")))})

    # Graphique interactif de l'évolution de l'occupation du sol du bassin versant
    output$plot_occupation_bv <- plotly::renderPlotly({ # Ce qui s'affiche dans plotlyOutput(ns("plot_occupation_bv"))
      occ <- occupation_bv() # Récupère les objets créés dans la fonction (les tableaux)

      shiny::validate(
        shiny::need( # Si occ est nul, n'essaie pas de tracer le graphique et affiche ce message:
          !is.null(occ),
          "Aucune donnée d'occupation du sol disponible pour le bassin versant de cette station." ) )

      p <- fun_plot_bv_occupation( # Appel de la fonction qui crée uniquement le graphique multicourbe
        table_long = occ$table_long ) # Utilise le tableau au format long

      plotly::ggplotly( # Transformation du ggplot en plotly pour avoir le zoom, le survol, etc.
        p, # Nom de l'object qui contient la fonction
        tooltip = "text" ) %>% # Au survol-> affiche le texte défini dans la fonction de préparation
        plotly::layout( # Mise en forme de la légende
          legend = list(
            orientation = "h", # Légende horizontale
            x = 0, # Alignée à gauche
            y = -0.2 ) ) %>% # Placée sous le graphique
        plotly::config( # Paramètres d'affichage et d'export
          displaylogo = FALSE, # Enlève le logo plotly
          toImageButtonOptions = list( # Caractéristiques du fichier exporté
            format = "png", # Export en PNG
            filename = paste0( "evolution_occupation_sol_bv_", # Nom du fichier
              if (!is.null(occ) && "id_bv" %in% names(occ)) occ$id_bv else "bv_inconnu"),
            height = 700, # Hauteur du fichier exporté
            width = 1100, # Largeur du fichier exporté
            scale = 1 ) ) # Résolution
    })

    # Tableau récapitulatif affiché sous le graphique
    output$table_occupation_bv <- DT::renderDT({ # Ce qui s'affiche dans DTOutput(ns("table_occupation_bv"))
      occ <- occupation_bv() # Récupération des données globales
      shiny::validate(
        shiny::need( # Validation : si occ est vide, affiche ce message
          !is.null(occ),
          "Aucune donnée tabulaire disponible pour le bassin versant de cette station.") )

      table_affichee <- occ$table_large %>% # Préparation du tableau à afficher
        dplyr::mutate(
          dplyr::across(
            .cols = c(
              "Artificialisation",
              "Agriculture",
              "Forêt",
              "Zones humides",
              "Eau"), # Colonnes à arrondir
            .fns = ~ round(.x, 1) ) ) %>% # Arrondit les valeurs à 1 décimale
        dplyr::rename(
          "Année" = .data$annee ) # Renomme la colonne annee pour l'affichage

      DT::datatable( # Affichage du tableau interactif
        table_affichee,
        rownames = FALSE, # N'affiche pas les numéros de ligne
        options = list(
          pageLength = 10, # 10 lignes par page
          scrollX = TRUE, # Autorise le scroll horizontal
          searching = FALSE, # Enlève la barre de recherche
          lengthChange = FALSE ) ) # Empêche de changer le nombre de lignes
    })

    # Export du tableau récapitulatif au format .csv
    output$download_table_occupation_bv <- shiny::downloadHandler(

      # Nom du fichier téléchargé
      filename = function() {
        occ <- occupation_bv() # Récupération des données globales
        id_bv <- if (!is.null(occ) && "id_bv" %in% names(occ)) occ$id_bv else "bv_inconnu" # Récupère l'id du BV si disponible
        paste0("occupation_sol_bv_", id_bv, ".csv") }, # Mise en forme du nom du fichier

      # Contenu du fichier exporté
      content = function(file) { # Fonction exécutée quand l'utilisateur clique sur télécharger
        occ <- occupation_bv() # Récupération des données

        table_export <- occ$table_large %>% # On récupère le tableau large (1 ligne = 1 année)
          dplyr::mutate( # Permet de modifier plusieurs colonnes du tableau
            dplyr::across( # Applique la même transformation sur plusieurs colonnes
              .cols = c( # Liste des colonnes concernées
                "Artificialisation",
                "Agriculture",
                "Forêt",
                "Zones humides",
                "Eau"),
              .fns = ~ round(.x, 1) ) ) %>% # Arrondit toutes les valeurs à 1 décimale
          dplyr::rename(
            "Annee" = .data$annee ) # Renomme la colonne pour le fichier exporté

        utils::write.csv( # Création du fichier csv
          x = table_export, # Tableau à exporter
          file = file, # Chemin du fichier créé par Shiny
          row.names = FALSE, # N'exporte pas les numéros de ligne
          fileEncoding = "UTF-8" ) # Encodage du fichier
      } )

    # Camembert détaillé du bassin versant pour l'année choisie
    output$plot_occupation_bv_detail <- plotly::renderPlotly({ # Ce qui s'affiche dans plotlyOutput(ns("plot_occupation_bv_detail"))
      occ_detail <- occupation_bv_detail() # Récupère les données détaillées du camembert

      shiny::validate(
        shiny::need( # Si occ_detail est nul, affiche ce message et n'essaie pas de tracer le graphique
          !is.null(occ_detail),
          "Aucune donnée détaillée disponible pour cette année et ce bassin versant.") )

      p <- fun_plot_bv_occupation_detail( # Appel de la fonction qui crée le camembert
        table_detail = occ_detail ) # Données détaillées utilisées pour le camembert

      p %>% # Le graphique est déjà en plotly, donc pas besoin de ggplotly
        plotly::config( # Paramètres d'affichage et d'export
          displaylogo = FALSE, # Enlève le logo plotly
          toImageButtonOptions = list( # Caractéristiques du fichier exporté
            format = "png", # Export en PNG
            filename = paste0("camembert_occupation_bv_", # Nom du fichier
              if (!is.null(occupation_bv()) && "id_bv" %in% names(occupation_bv())) occupation_bv()$id_bv else "bv_inconnu",
              "_",
              input$annee_bv_detail),
            height = 700, # Hauteur du fichier exporté
            width = 1100, # Largeur du fichier exporté
            scale = 1 ) ) # Résolution
    })

    # Tableau détaillé du camembert affiché sous le graphique
    output$table_occupation_bv_detail <- DT::renderDT({ # Ce qui s'affiche dans DTOutput(ns("table_occupation_bv_detail"))
      occ_detail <- occupation_bv_detail() # Récupération des données détaillées

      shiny::validate(
        shiny::need( # Validation : si occ_detail est vide, affiche ce message
          !is.null(occ_detail),
          "Aucune donnée détaillée disponible pour cette année et ce bassin versant.") )

      table_affichee <- occ_detail %>% # Préparation du tableau à afficher
        dplyr::select(
          .data$occupation_detail, # Colonne des catégories
          .data$pourcentage ) %>% # Colonne des pourcentages
        dplyr::mutate(
          pourcentage = round(.data$pourcentage, 1) ) %>% # Arrondit à 1 décimale
        dplyr::rename(
          "Occupation" = .data$occupation_detail, # Renomme pour l'affichage
          "Pourcentage" = .data$pourcentage ) # Renomme pour l'affichage

      DT::datatable( # Affichage du tableau interactif
        table_affichee,
        rownames = FALSE, # N'affiche pas les numéros de ligne
        options = list(
          pageLength = 15, # 15 lignes par page
          scrollX = TRUE, # Autorise le scroll horizontal
          searching = FALSE, # Enlève la barre de recherche
          lengthChange = FALSE ) ) # Empêche de changer le nombre de lignes
    })

    # Export du tableau détaillé du camembert au format .csv
    output$download_table_occupation_bv_detail <- shiny::downloadHandler(

      # Nom du fichier téléchargé
      filename = function() {
        occ <- occupation_bv() # Récupération des données BV
        id_bv <- if (!is.null(occ) && "id_bv" %in% names(occ)) occ$id_bv else "bv_inconnu" # Récupère l'id du BV
        annee <- input$annee_bv_detail # Récupère l'année choisie
        paste0("occupation_sol_bv_detail_", id_bv, "_", annee, ".csv") }, # Mise en forme du nom du fichier

      # Contenu du fichier exporté
      content = function(file) { # Fonction exécutée quand l'utilisateur clique sur télécharger
        occ_detail <- occupation_bv_detail() # Récupération des données détaillées

        if (is.null(occ_detail)) { return(NULL) } # Si aucune donnée n'existe, on arrête l'export

        table_export <- occ_detail %>% # Préparation du tableau à exporter
          dplyr::select(
            .data$occupation_detail, # Colonne des catégories
            .data$pourcentage ) %>% # Colonne des pourcentages
          dplyr::mutate(
            pourcentage = round(.data$pourcentage, 1) ) %>% # Arrondit à 1 décimale
          dplyr::rename(
            "Occupation" = .data$occupation_detail, # Renomme pour le fichier exporté
            "Pourcentage" = .data$pourcentage ) # Renomme pour le fichier exporté

        utils::write.csv( # Création du fichier csv
          x = table_export, # Tableau à exporter
          file = file, # Chemin du fichier créé par Shiny
          row.names = FALSE, # N'exporte pas les numéros de ligne
          fileEncoding = "UTF-8" ) # Encodage du fichier
      } )

  } )
}

## À appeler dans l'UI
# mod_station_contexte_env_bv_ui ("station_contexte_env_bv")

## À appeler dans le server
# mod_station_contexte_env_bv_server("station_contexte_env_bv")
