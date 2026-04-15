#' Interface utilisateur de l'application
#'
#'#' @description
#' Fonction UI principale de l'application Shiny.
#' Elle définit l'organisation visuelle de l'application avec le titre,
#' la zone de chargement des données, le panneau de filtres,
#'  les onglets principaux, la carte et panneaux d'information
#'
#' @param request Paramètre interne shiny
#'
#' @noRd

app_ui <- function(request) { # Fonction qui defini l'interphase utilisateur
  shiny::fluidPage( # Page principale de type fluide qui s'adapte

    shiny::titlePanel("HydrobioNOR"), # Titre principal de l'application

    shiny::fluidRow( # Zone d'affichage du module de chargement des données
      shiny::column( # Creer une colonne et une ligne
        width = 12,
        mod_load_data_ui("donnees") ) ),

    shiny::sidebarLayout( # Organisation de la page en panneau latéral + panneau principal

      # Panneau latéral contenant les filtres
      shiny::sidebarPanel(
        width = 2, # Largeur du panneau de filtres
        style = "padding: 10px;", # Espace intérieur
        shiny::tags$h5("Filtres"), # Titre de la zone de filtres
        mod_selecteur_dep_ui("departements"),
        mod_selecteur_eqb_ui("eqb"), # Affichage des filtres
        mod_selecteur_UH_ui("uh") ),

      # Panneau principal contenant les onglets de l'application
      shiny::mainPanel(
        width = 10, # Zone principale plus large pour afficher les résultats
        shiny::tabsetPanel( # Ensemble des onglets principaux

          #  1er onglet : Station
          shiny::tabPanel(
            title = "Station",
            shiny::br(),
            shiny::fluidRow( # Organisation en deux colonnes : carte à gauche, détails à droite

              shiny::column(
                width = 7, # Carte interactive des stations
                mod_station_carte_ui("station_carte", hauteur = "600px") ),

              shiny::column(
                width = 5, # Panneau de droite avec les sous-onglets d'information
                shiny::tabsetPanel(

                  # 1 er sous onglet
                  shiny::tabPanel(
                    title = "Informations générales",
                    mod_station_infos_ui("station_infos") ),

                  # 2 eme sous onglet
                  shiny::tabPanel(
                    title = "Contexte environnemental Station",
                    shiny::br(),
                    mod_station_contexte_env_ui("station_contexte_env") )
                ) ) ) ),

          # 2 eme Onglet
          shiny::tabPanel(
            title = "Communautés", # Onglet communautés
            shiny::br(),
            shiny::h4("À venir") )
        ) ) ) )
}
