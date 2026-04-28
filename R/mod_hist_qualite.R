#' Module UI des histogrammes de qualité
#'
#' @description
#' Module ui affichant les histogrammes de qualité écologique.
#' Ce module contient :le graphique plotly et un texte explicatif
#'
#' @param id Identifiant du module Shiny
#'
#' @return Un tagList contenant le graphique et le texte explicatif
#' @noRd

mod_hist_qualite_ui <- function(id) {
  ns <- shiny::NS(id) # Création du namespace pour éviter les conflits d'ID
  shiny::tagList(

    # Graphique interactif des classes de qualité
    plotly::plotlyOutput(ns("plot_qualite"), height = "450px"),
    shiny::br(), # Retour à la ligne pour espacer

    # Bloc de texte explicatif sous le graphique
    shiny::div(
      style = "font-size: 12px; color: #555; line-height: 1.4;", # Style discret et lisible
      shiny::p( # Description générale du graphique
        "Ce graphique présente la répartition des stations selon leur classe de qualité écologique pour différents indices biologiques (I2M2, IBD, IBMR, IPR, IBG équivalent), en fonction des cycles de la Directive Cadre sur l’Eau (DCE)." ),
      shiny::p(  # Explication de la structure des barres
        "Chaque barre correspond à une période donnée et est empilée par classe de qualité (Très bon, Bon, Moyen, Médiocre, Mauvais ou Non évalué). La hauteur totale de la barre représente le nombre de stations distinctes pour cet indice et cette période, tandis que les couleurs indiquent la répartition des classes de qualité. Cela permet de voir :"),
      shiny::tags$ul(   # Liste des interprétations possibles
        shiny::tags$li("la distribution des états écologiques des stations pour chaque indice biologique"),
        shiny::tags$li("leur évolution dans le temps"),
        shiny::tags$li("les différences entre indices biologiques") ) ) )
}

#' Module server des histogrammes de qualité
#'
#' @description
#' Permet de préparer les données , de générer le graphique et de l'afficher en ploty
#'
#' @param id Identifiant du module Shiny
#' @param donnees Reactive contenant les données de l'application
#' @param choix_departements Reactive contenant le ou les départements sélectionnés
#' @param choix_eqb Reactive contenant le compartiment biologique sélectionné
#' @param choix_uh Reactive contenant l'UH sélectionnée
#'
#' @return Un graphique interactif affiché dans l'UI
#' @noRd

mod_hist_qualite_server <- function(id, donnees, choix_departements, choix_eqb, choix_uh) {
  shiny::moduleServer(id, function(input, output, session) {

    donnees_qualite <- shiny::reactive({ # Reactive contenant les données de qualité filtrée
      shiny::req(donnees()) # Vérifie que les données sont chargées
      fun_prep_qualite( # Appel de la fonction de prep
        donnees = donnees(),
        choix_departements = choix_departements(),
        choix_eqb = choix_eqb(),
        choix_uh = choix_uh() )})

    # Affichage du graphique
    output$plot_qualite <- plotly::renderPlotly({
      shiny::req(donnees_qualite()) # Vérifie que les données sont disponibles
      p <- fun_plot_qualite(donnees_qualite())  # Création du graphique ggplot
      plotly::ggplotly(p, tooltip = "text") %>% # Conversion en ploty
        plotly::layout(
          margin = list(
            l = 60, # marge gauche (axe Y)
            r = 20, # marge droite
            b = 60, # marge bas (axe X)
            t = 30 )) # marge haut
    } )
  } )
}

## À appeler dans l'UI
# mod_hist_qualite_ui("hist_qualite_commu")

## À appeler dans le SERVER
# mod_hist_qualite_server ("hist_qualite_commu")
