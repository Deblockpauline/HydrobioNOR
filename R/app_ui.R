#' Interface utilisateur de l'application
#'
#' @description
#' Fonction UI principale de l'application Shiny.
#' Elle définit l'organisation visuelle de l'application avec le titre,
#' la zone de chargement des données, le panneau de filtres,
#' les onglets principaux, la carte et les panneaux d'information.
#'
#' @param request Paramètre interne shiny
#'
#' @noRd

app_ui <- function(request) { # Fonction qui définit l'interface utilisateur
  shiny::fluidPage( # Page principale de type fluide qui s'adapte

    shiny::titlePanel("HydrobioNOR"), # Titre principal de l'application
    shiny::fluidRow( # Zone d'affichage du module de chargement des données
      shiny::column(
        width = 12,
        mod_load_data_ui("donnees") ) ),

    shiny::sidebarLayout( # Organisation de la page en panneau latéral + panneau principal

      # Panneau latéral contenant les filtres
      shiny::sidebarPanel(
        width = 2, # Largeur du panneau de filtres
        style = "padding: 10px;", # Espace intérieur
        shiny::tags$h5("Filtres"), # Titre de la zone de filtres
        mod_selecteur_dep_ui("departements"),
        mod_selecteur_eqb_ui("eqb"),
        mod_selecteur_UH_ui("uh") ),

      # Panneau principal contenant les onglets de l'application
      shiny::mainPanel(
        width = 10, # Zone principale plus large pour afficher les résultats
        shiny::tabsetPanel(

          # 1er onglet : Station
          shiny::tabPanel(
            title = "Station",
            shiny::br(),

            # Carte en haut sur toute la largeur
            shiny::fluidRow(
              shiny::column(
                width = 12,
                mod_station_carte_ui("station_carte", hauteur = "500px") ) ),

            shiny::br(), # Retour a la ligne

            # Sous-onglets sous la carte
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::tabsetPanel(

                  # 1er sous-onglet
                  shiny::tabPanel(
                    title = "Informations générales",
                    mod_station_infos_ui("station_infos")),

                  # 2e sous-onglet
                  shiny::tabPanel(
                    title = "Contexte env. Station",
                    shiny::br(),
                    mod_station_contexte_env_ui("station_contexte_env")),

                  # 3e sous-onglet
                  shiny::tabPanel(
                    title = "Contexte env. BV",
                    shiny::br(),
                    mod_station_contexte_env_bv_ui("station_contexte_env_bv") )
                ) ) ) ),

          # 2e onglet : Communautés
          shiny::tabPanel(
            title = "Communautés",
            shiny::br(),
            shiny::h4("RDV la semaine pro") )
 ) ) ) )
}
