#' Interface utilisateur de l'application
#'
#' @param request Paramètre interne shiny
#'
#' @noRd

app_ui <- function(request) {
  shiny::fluidPage( # Page principale de type fluide

    shiny::titlePanel("HydrobioNOR"), # Titre principal de l'application
    shiny::fluidRow( # Zone d'affichage du module de chargement des données
      shiny::column(
        width = 12,
        mod_load_data_ui("donnees") ) ),
    shiny::sidebarLayout( # Organisation de la page en panneau latéral + panneau principal

      # Panneau latéral contenant les filtres
      shiny::sidebarPanel(
        width = 2, # Largeur du panneau de filtres
        style = "padding: 10px;",
        shiny::tags$h5("Filtres"), # Titre de la zone de filtres
        mod_selecteur_dep_ui("departements"),
        mod_selecteur_eqb_ui("eqb"),
        mod_selecteur_UH_ui("uh")   # Titre des filtres
      ),

      # Panneau principal contenant les onglets de l'application
      shiny::mainPanel(
        width = 10, # Zone principale plus large pour afficher les résultats
        shiny::tabsetPanel( # Ensemble des onglets principaux
          shiny::tabPanel(
            title = "Station",  # Onglet station
            shiny::br(),
            shiny::fluidRow( # Organisation en deux colonnes : carte à gauche, détails à droite

               shiny::column(
                width = 7, # Carte interactive des stations
                mod_station_carte_ui("station_carte", hauteur = "600px") ),

              shiny::column(
                width = 5,
                shiny::tabsetPanel(
                  shiny::tabPanel( # Panneau à droite avec les informations
                    title = "Informations générales",
                    mod_station_infos_ui("station_infos") ),
                  shiny::tabPanel(
                    title = "Occupation",
                    shiny::br(),
                    shiny::h4("À venir") ) # Pour plus tard
                ) ) ) ),

          shiny::tabPanel(
            title = "Communautés", # Onglet communautés
            shiny::br(),
            shiny::h4("À venir") )
        ) ) ) ) }
