#' Module UI du contexte environnemental de la station
#'
#'@description Interface du module affichant le contexte environnemental
#' d'une station hydrobiologique
#'
#' @param id Identifiant du module
#'
#' @noRd

mod_station_contexte_env_ui <- function(id) {
  ns <- shiny::NS(id) # Création de l'espace de noms du module

  shiny::tagList(
    shiny::h4("Contexte environnemental Station"),  # Titre affiché dans le panneau

    # Menu déroulant permettant de choisir l'année CLC
    shiny::selectInput(
      inputId = ns("annee_clc"),
      label = "Année CLC",
      choices = c("1990", "2000", "2006", "2012", "2018"),
      selected = "2018" ),

    # Zone de sortie du graphique interactif Plotly
    plotly::plotlyOutput(ns("plot_occupation"), height = "450px") )
}

#' Module server du contexte environnemental de la station
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données de l'application
#' @param station_selectionnee Reactive contenant le code station sélectionné
#'
#' @noRd

mod_station_contexte_env_server <- function(id, donnees, station_selectionnee) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive qui récupère la table d'occupation du sol de l'année choisie
    table_occupation <- shiny::reactive({
      shiny::req(donnees()) # Vérifie que les données sont disponibles
      shiny::req(input$annee_clc) # Vérifie qu'une année a bien été choisie
      nom_table <- paste0("occupation_", input$annee_clc) #paste0 = ecrit avec un espace -> Nom de la table
      if (!nom_table %in% names(donnees())) {return(NULL) } # Si la table n'existe pas dans les données,on renvoie NULL
      donnees()[[nom_table]] # Retourne la table correspondant à l'année sélectionnée
    })

    # Graphique interactif d'occupation du sol
    output$plot_occupation <- plotly::renderPlotly({

      shiny::req(station_selectionnee())  # Il faute une station selectionnée sinon pas de graphique
      table_occ <- table_occupation() # Récupère la table correspondant à l'année choisie
      shiny::req(table_occ)  # Vérifie que la table existe bien = Sécurité

      p <- fun_plot_station_occupation( # Appelle de la fonction qui va tracer le graph
        table_occupation = table_occ, # En fonction de l'année et de la station sélectionner
        station_id = station_selectionnee() )

      shiny::validate( # Si aucun graphique n'a pu etre creer, ce message s'affiche:
        shiny::need(
          !is.null(p),
          "Aucune donnée d'occupation du sol disponible pour cette station."))

      plotly::ggplotly( # Conversion du ggplot en graphique interactif Plotly
        p,
        tooltip = "text"  ) %>% # lors du survol, Plotly affichera la variable text definie dans ggplot

        # Mise en forme de la légende
        plotly::layout(
          legend = list(
            orientation = "h", # légende horizontale
            x = 0,             # position à gauche
            y = -0.15   ) )      # position sous le graphique
    } ) } )
}
