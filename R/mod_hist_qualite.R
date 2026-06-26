#' Module UI des histogrammes de qualité
#' @description Module ui affichant les histogrammes de qualité écologique.
#' Contient le graphique plotly et un texte explicatif
#' @noRd

mod_hist_qualite_ui <- function(id) {
  ns <- shiny::NS(id) # Création du namespace pour éviter les conflits d'ID
  shiny::tagList(

    # Graphique interactif des classes de qualité
    plotly::plotlyOutput(ns("plot_qualite"), height = "450px"),
    shiny::br(), # Retour à la ligne pour espacer

    # Bloc de texte explicatif sous le graphique
    shiny::div(
      style = "font-size: 12px; color: #555; line-height: 1.4;", # Style
      shiny::p(
        "Pour rappel, ce graphique est incompatble avec le filtre qualité incorrecte car il n'exsite pas de données"),
      shiny::p(
        "Ce graphique présente la répartition des stations selon leur classe de qualité écologique pour différents indices biologiques (I2M2, IBD, IBMR, IPR, IBG équivalent), en fonction des cycles de la Directive Cadre sur l’Eau (DCE)." ),
      shiny::p(
        "Chaque barre correspond à une période donnée et est empilée par classe de qualité (Très bon, Bon, Moyen, Médiocre, Mauvais ou Non évalué). La hauteur totale de la barre représente le nombre de stations distinctes pour cet indice et cette période, tandis que les couleurs indiquent la répartition des classes de qualité. Cela permet de voir :"),
      shiny::tags$ul(   # Liste des interprétations possibles
        shiny::tags$li("la distribution des états écologiques des stations pour chaque indice biologique"),
        shiny::tags$li("leur évolution dans le temps"),
        shiny::tags$li("les différences entre indices biologiques") ) ) )
}

#' Module server des histogrammes de qualité
#' @description Permet de préparer les données , de générer le graphique et de l'afficher en ploty
#' @return Un graphique interactif affiché dans l'UI
#' @noRd

mod_hist_qualite_server <- function(id,
                                    donnees,
                                    choix_departements,
                                    choix_eqb,
                                    choix_uh,
                                    choix_reseau,
                                    choix_qualification = NULL) {

   shiny::moduleServer(id, function(input, output, session) {

    donnees_qualite <- shiny::reactive({ # Reactive contenant les données de qualité filtrée
      shiny::req(donnees()) # Vérifie que les données sont chargées
      fun_prep_qualite( # Appel de la fonction de prep
        donnees = donnees(),# Selon les données et les filtres
        choix_departements = choix_departements(),
        choix_eqb = choix_eqb(),
        choix_uh = choix_uh(),
        choix_reseau = choix_reseau(),
        choix_qualification = choix_qualification())})

# Graphique
    output$plot_qualite <- plotly::renderPlotly({
      df <- donnees_qualite() # Données filtrées
      shiny::validate(
        shiny::need(
          !is.null(df) && nrow(df) > 0,
          "Aucune donnée disponible pour cette combinaison de filtres.") )
      p <- fun_plot_qualite(df) # Création du graphique ggplot en appellant la fonction
      plotly::ggplotly(
        p,
        tooltip = "text" ) %>%
        plotly::layout(
          margin = list(
            l = 60, # Marge gauche
            r = 20, # Marge droite
            b = 60, # Marge bas
            t = 30 ) ) %>% # Marge haut
        plotly::config(
          toImageButtonOptions = list(
            format = "png", # Format
            filename = "qualite_indices", # Nom du fichier
            height = 800, # Hauteur image
            width = 1200, # Largeur image
            scale = 2 ) )# Qualité
       } ) } ) }

## À appeler dans l'UI
# mod_hist_qualite_ui("hist_qualite_commu")

## À appeler dans le SERVER
# mod_hist_qualite_server ("hist_qualite_commu")
