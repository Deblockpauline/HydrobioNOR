#' Interface utilisateur de l'application
#' @description Fonction UI principale de l'application Shiny.
#' @param request Paramètre interne shiny
#' @noRd

app_ui <- function(request) { # Fonction qui définit l'interface utilisateur

  chemin_logo <- normalizePath(  # Chemin local vers le logo dans le projet
    file.path("inst", "app", "www", "logo_ofb.png"),# Construit le chemin vers le fichier
    winslash = "/",  # Remplace les "\" par "/"
    mustWork = FALSE) # Ne plante pas si le ficher est pas trouvé

  shiny::fluidPage( # Page principale de type fluide qui s'adapte

   # Police Marianne pour toute l'application
    shiny::tags$head( # Permet de changer sur toute l'app
      shiny::tags$style(shiny::HTML(" # Permet d'ecrire du code CSS et que R ne le transforme pas
        @font-face {
          font-family: 'Marianne'; # Nom de la police
          src: url('fonts/Marianne-Regular.woff2') format('woff2'); # Chemin
          font-weight: normal;} # Définit que ce fichier correspond au texte normal
        @font-face {
          font-family: 'Marianne';# Même nom pour regrouper les variantes de la police
          src: url('fonts/Marianne-Bold.woff2') format('woff2'); # Chemin pour le texte en gras
          font-weight: bold; } # Definit que c'est l'ecriture en gras
        body, button, input, select, textarea {
          font-family: 'Marianne', Arial, sans-serif; }"))), # Ordre d'utilisation


    # Ligne du haut avec le titre a gauche et le logo a droite
    shiny::fluidRow(
      shiny::column(
        width = 10,
        shiny::titlePanel("HydrobioNOR") ), # Le titre
      shiny::column(
        width = 2, # Largeur
        shiny::tags$img(
          src = knitr::image_uri(chemin_logo), # Convertit l'image en base64 pour affichage direct
          alt = "Logo OFB",
          style = "float:right; margin-top: 10px; width: 120px;" ) ) ), # Style

    # Zone d'affichage du module de chargement des données
    shiny::fluidRow(
      shiny::column(
        width = 12,
        mod_load_data_ui("donnees") ) ),

    # Organisation de la page en panneau latéral + panneau principal
    shiny::sidebarLayout(

      # Panneau latéral contenant les filtres
      shiny::sidebarPanel(
        width = 2, # Largeur du panneau de filtres
        style = "padding: 10px;", # Espace intérieur
        shiny::tags$h5("Filtres"), # Titre de la zone de filtres
        mod_selecteur_dep_ui("departements"),
        mod_selecteur_eqb_ui("eqb"),
        mod_selecteur_UH_ui("uh")),

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
                    mod_station_infos_ui("station_infos") ),

                  # 2e sous-onglet
                  shiny::tabPanel(
                    title = "Contexte env. Station",
                    shiny::br(),
                    mod_station_contexte_env_ui("station_contexte_env") ),

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

            # Carte en haut sur toute la largeur
            shiny::fluidRow(
              shiny::column(
                width = 12,
                mod_station_carte_ui("station_carte_commu", hauteur = "500px") ) ),
            shiny::br(), # Retour a la ligne

            # Sous-onglets sous la carte
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::tabsetPanel(

                  # 1er sous-onglet : Qualité
                  shiny::tabPanel(
                    title = "Qualité",
                    shiny::br(),
                    mod_hist_qualite_ui("hist_qualite_commu") ),

                  # 2e sous-onglet : Indices
                  shiny::tabPanel(
                    title = "Indices",
                    shiny::br(),
                    shiny::p("A venir version 4 ") ),

                  # 3e sous-onglet : Taxons
                  shiny::tabPanel(
                    title = "Taxons",
                    shiny::br(),
                    shiny::p("À venir, Version 5") ),

                  # 4e sous-onglet : Diagnostic
                  shiny::tabPanel(
                    title = "Diagnostic",
                    shiny::br(),
                    shiny::p("À venir Version 6") ),

                  # 5e sous-onglet : Plan d'échantillonnage
                  shiny::tabPanel(
                    title = "Plan d'échantillonnage",
                    shiny::br(),
                    shiny::p("À venir Version 7") )
              ) ) ) ) )
      ) ) ) }
