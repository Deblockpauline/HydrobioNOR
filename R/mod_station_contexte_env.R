#' Module UI du contexte environnemental de la station
#'
#' @description Interface du module affichant l'évolution de l'occupation du sol
#' d'une station hydrobiologique au fil des années, sous forme
#' d'un graphique multicourbe interactif et d'un tableau récapitulatif exportable.
#' @noRd

mod_station_contexte_env_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    # Titre principal du module
    shiny::h4("Contexte environnemental Station"),

    # Message affiché tant qu'aucune station n'est sélectionnée
    shiny::uiOutput(ns("message_selection_station")),
    shiny::br(),

    # Graphique interactif centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;", # le bloc prend 90% au centre
      plotly::plotlyOutput(
        outputId = ns("plot_occupation"),
        height = "450px" ) ), # Hauter
    shiny::br(),

    # Titre du tableau
    shiny::h5("Tableau récapitulatif de l'évolution de l'occupation du sol"),

    # Bouton de téléchargement du tableau
    shiny::downloadButton(
      outputId = ns("download_table_occupation"),
      label = "Télécharger le tableau (.csv)" ),
    shiny::br(),
    shiny::br(),

    # Tableau centré dans le panneau
    shiny::div(
      style = "width: 90%; margin: auto;",
      DT::DTOutput(ns("table_occupation") ) )
  ) }


#' Module server du contexte environnemental de la station
#'
#' @description Module serveur affichant l'évolution de l'occupation du sol
#' de la station sélectionnée :
#' - graphique multicourbe interactif,
#' - tableau récapitulatif,
#' - export du tableau au format .csv.
#' @noRd

mod_station_contexte_env_server <- function(id, donnees, station_selectionnee) {
  shiny::moduleServer(id, function(input, output, session) {

    # Message simple si aucune station n'est encore sélectionnée
    output$message_selection_station <- shiny::renderUI({
      if (is.null(station_selectionnee()) || station_selectionnee() == "") {
        shiny::div(
          style = "color: #666; font-style: italic;",
          "Veuillez sélectionner une station sur la carte pour afficher le contexte environnemental.")
      } else {NULL } })

    # Reactive préparant les données d'occupation du sol
    # pour la station sélectionnée sur l'ensemble des années
    occupation_station <- shiny::reactive({ # Réactive preparant les données
      shiny::req(donnees()) # Obligatoire : donnees + stations selectionnee
      shiny::req(station_selectionnee())

      fun_prep_station_occupation( # Appel de la fonction
        donnees = donnees(),
        station_id = station_selectionnee() ) } )

    # Graphique interactif de l'évolution de l'occupation du sol
    output$plot_occupation <- plotly::renderPlotly({
      occ <- occupation_station() # Recupere les 2 tables cree dans le fun
      shiny::validate( # Validation pour plus tard
        shiny::need( # Si c'est nul affiche ce message et essaie pas.
          !is.null(occ),
          "Aucune donnée d'occupation du sol disponible pour cette station.") )

      p <- fun_plot_station_occupation( # Appel la fonction= ne fait que tracer le graph
        table_long = occ$table_long)

      plotly::ggplotly( # Transformation en Ploty pour avoir le zoom, le survol etc..
        p,
        tooltip = "text") %>% # Au sruvol, affiche ca (definie dans le fun)
        plotly::layout( # Mise en forme de la légende
          legend = list(
            orientation = "h", # Horizontale
            x = 0, # Aligné a gauche
            y = -0.2 ) ) %>% # Sous le graph
        plotly::config( # Pour l'export
          displaylogo = FALSE, # Pas le logo Ploty
          toImageButtonOptions = list( # Caracteristique
            format = "png", # Export en PNG
            filename = paste0( "evolution_occupation_sol_station_",
              if (is.null(station_selectionnee()) || station_selectionnee() == "") {
                "station_inconnue" }
              else { station_selectionnee() } ),
            height = 700,
            width = 1100, # Taille du fichier
            scale = 1 ) ) # Résolution
    })

    # Tableau récapitulatif affiché sous le graphique
    output$table_occupation <- DT::renderDT({ # Ce qui va s'afficher dans DTOutput(ns("table_occupation"))
      occ <- occupation_station() # on Recupere les donnees
      shiny::validate(
        shiny::need( # Validation , si c'est vide = on affiche:
          !is.null(occ),
          "Aucune donnée tabulaire disponible pour cette station." ) )

      table_affichee <- occ$table_long %>% # Permetde mettre une clé d'identif
        dplyr::mutate(
          id_occupation = dplyr::case_when(
            as.character(.data$occupation) == "Artificialisation" ~ 1,
            as.character(.data$occupation) == "Agriculture" ~ 2,
            as.character(.data$occupation) == "Forêt" ~ 3,
            as.character(.data$occupation) == "Zones humides" ~ 4,
            as.character(.data$occupation) == "Eau" ~ 5,
            TRUE ~ NA_real_ ),
          pourcentage = round(.data$pourcentage, 1)) %>% # Arrondir
        dplyr::select(
          annee,
          id_occupation,
          occupation,
          pourcentage ) %>%
        dplyr::rename(
          "Annee" = .data$annee,
          "Occupation" = .data$occupation,
          "Pourcentage" = .data$pourcentage )

      DT::datatable( # Affichage de tableau interactif
        table_affichee,
        rownames = FALSE, # N'affiche pas les numéros de ligne
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = FALSE,
          lengthChange = FALSE ) )
    }, server = TRUE)

    # Export du tableau récapitulatif au format .csv
    output$download_table_occupation <- shiny::downloadHandler(

      # Nom du fichier téléchargé
      filename = function() {
        code_station <- station_selectionnee() # Recupere le code station
        if (is.null(code_station) || is.na(code_station) || code_station == "") {
          code_station <- "station_inconnue" } # Si pas de code
        paste0("occupation_sol_", code_station, ".csv") }, # Mise en forme

      # Contenu du fichier exporté
      content = function(file) { # Fonction exécutée lorsque l'utilisateur clique sur télécharger
        occ <- occupation_station() # Recuperation des données
        if (is.null(occ)) { return(NULL)} # Si aucune donnée n'existe, on arrête l'export

        table_export <- occ$table_long %>% # Permetde mettre une clé d'identif
          dplyr::mutate( # Permet de modifier plusieurs colonnes du tableau
            id_occupation = dplyr::case_when( # Systeme de clé
              as.character(.data$occupation) == "Artificialisation" ~ 1,
              as.character(.data$occupation) == "Agriculture" ~ 2,
              as.character(.data$occupation) == "Forêt" ~ 3,
              as.character(.data$occupation) == "Zones humides" ~ 4,
              as.character(.data$occupation) == "Eau" ~ 5,
              TRUE ~ NA_real_ ),
            pourcentage = round(.data$pourcentage, 1)) %>% # Arrondir
          dplyr::select(
            annee,
            id_occupation,
            occupation,
            pourcentage ) %>%
          dplyr::rename(
            "Annee" = .data$annee,
            "Occupation" = .data$occupation,
            "Pourcentage" = .data$pourcentage )

        utils::write.csv2(
          table_export,
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8")
      } ) } )
}

## À appeler dans l'UI
# mod_station_contexte_env_ui ("station_contexte_env")

## À appeler dans le server
# mod_station_contexte_env_server("station_contexte_env")
