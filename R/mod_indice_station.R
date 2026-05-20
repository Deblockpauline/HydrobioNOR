#' Module UI des indices biologiques
#'
#' @description
#' Interface du sous-onglet Indices.
#' @param id Identifiant du module
#' @noRd

mod_communaute_indices_ui <- function(id) { # Fonction UI du module

  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les elements UI
    shiny::h4("Indices biologiques"), # Titre partie indices
    shiny::uiOutput(ns("message_indices")), # Message si pas de station ou pas de donnees
    shiny::uiOutput(ns("plots_indices")), # Zone des graphiques + tableaux
    shiny::br(), # Espace
    shiny::h4("Métriques de l'I2M2"), # Titre partie metriques
    shiny::uiOutput(ns("bloc_metriques_i2m2") ) ) } # Bloc dynamique metriques )

#' Module server des indices biologiques
#'
#' @description
#' Serveur du sous-onglet Indices. Test d'une boucle purr a la ligne 67
#' @param id Identifiant du module
#' @param donnees Reactive contenant la liste des données
#' @param station_selectionnee Reactive contenant le code de la station sélectionnée
#' @noRd

mod_communaute_indices_server <- function(id, donnees, station_selectionnee) { # Fonction server du module

  shiny::moduleServer(id, function(input, output, session) { # Debut module server
    ns <- session$ns # Namespace cote serveur

    # Graphiques des indices biologiques
    graphiques_indices <- shiny::reactive({ # Reactive qui cree les graphiques
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station
      fun_plot_indices_station( # Appelle la fonction graphique
        etat_bio = donnees()$etat_bio, # Table des indices
        station_id = station_selectionnee() ) } )  # Station choisie

    # Message au-dessus des graphiques d'indices
    output$message_indices <- shiny::renderUI({ # Cree le message

      if (is.null(station_selectionnee()) || is.na(station_selectionnee()) || station_selectionnee() == "") { # Si pas de station
        return(
          shiny::div(
            style = "color: #666; font-style: italic;",
            "Cliquez sur une station de la carte pour afficher ses indices biologiques.") ) }
      plots <- graphiques_indices() # Recupere les graphiques
      if (is.null(plots) || length(plots) == 0) { # Si rien a afficher
        return(
          shiny::div(
            style = "color: #666; font-style: italic;",
            "Aucune donnée d'indice biologique disponible pour cette station." ) ) }
      NULL } ) # Sinon pas de message

    # Création dynamique des graphiques + tableaux + boutons CSV
    # Sorties dynamiques car le nombre d'indices varie selon la station
    # Maximum attendu : 6 indices biologiques
    output$plots_indices <- shiny::renderUI({ # Cree les sorties selon le nombre d'indices

      plots <- graphiques_indices() # Recupere la liste graph + table
      shiny::tagList( # Regroupe tous les blocs
        purrr::map( # Boucle sur les indices
          seq_along(plots), # Retorune le numero de chaque indice 1, 2 ,3...

          function(i) { # Fonction pour 1 indice
            shiny::tagList( # Bloc pour 1 indice
              plotly::plotlyOutput( # Sortie graphique
                outputId = ns(paste0("plot_indice_", i)), # Id unique du graphique
                height = "430px" ), # Hauteur graphique

              shiny::br(), # Espace
              shiny::downloadButton( # Bouton CSV
                outputId = ns(paste0("download_indice_", i)), # Id unique du bouton
                label = "Télécharger les données (.csv)"), # Texte bouton

              shiny::br(), # Espace
              DT::DTOutput( # Sortie tableau
                outputId = ns(paste0("table_indice_", i) ) ), # Id unique du tableau
              shiny::br(), # Espace
              shiny::hr() ) } )# Ligne separation
      ) } )

    # Rendu des graphiques, tableaux et exports
    shiny::observe({ # Observe pour creer les sorties

      plots <- graphiques_indices() # Recupere les graphiques/table
      purrr::walk( # Boucle sans creer de liste
        seq_along(plots), # Numero de chaque indice
        function(i) { # Pour chaque indice

          local({ # Evite les soucis de boucle Shiny
            ii <- i # Garde le bon numero , evite 3 3 3 ( ecrase tout par le dernier) par exemple et garde 1 2 3

            output[[paste0("plot_indice_", ii)]] <- plotly::renderPlotly({ # Rendu du graphique
              plots[[ii]]$graph  } )# Affiche le graphique de l'indice

            output[[paste0("table_indice_", ii)]] <- DT::renderDT({ # Rendu du tableau
              table_affichage <- plots[[ii]]$table %>% # Table associee au graphique
                dplyr::mutate(
                  dplyr::across(
                    where(is.numeric), # Colonnes numeriques
                    ~ round(.x, 3) ) )# Arrondi a 0.001

              DT::datatable( # Cree le tableau interactif
                table_affichage, # Table arrondie pour affichage
                rownames = FALSE, # Pas de noms de lignes
                options = list(
                  pageLength = 10, # 10 lignes par page
                  scrollX = TRUE ) ) } , server = TRUE )  # Scroll horizontal si besoin

            # Export CSV
            output[[paste0("download_indice_", ii)]] <- shiny::downloadHandler( # Telechargement CSV

              filename = function() { # Nom du fichier
                nom_indice <- unique(plots[[ii]]$table$libelle_indice)[1] # Recupere le nom de l'indice
                paste0("donnees_indice_", nom_indice, "_", station_selectionnee(), ".csv") },  # Nom final

              content = function(file) { # Contenu du fichier
                table_export <- plots[[ii]]$table %>% # Table a exporter
                  dplyr::mutate(
                    dplyr::across(
                      where(is.numeric), # Colonnes numeriques
                      ~ round(.x, 3) ) ) # Arrondi a 0.001
                utils::write.csv2( # Ecrit le CSV
                  table_export, # Table arrondie exportee
                  file, # Chemin du fichier
                  row.names = FALSE,
                  fileEncoding = "UTF-8") } # Pas de noms de lignes
            ) } ) } )
    } )

    #### PARTIE I2M2 METRIQUES

    # Bloc dynamique des métriques I2M2
    output$bloc_metriques_i2m2 <- shiny::renderUI({ # Affiche ou non la partie metriques
      shiny::req(donnees()) # Attend les donnees
      table_metriques <- donnees()$metriques %>%
        dplyr::filter(code_station == station_selectionnee())

      if (nrow(table_metriques) == 0) { # Si pas de lignes
        return(
          shiny::div(
            style = "color: #666; font-style: italic;",
            "Aucune donnée de métrique I2M2 disponible pour cette station.") ) }

      shiny::tagList( # Si donnees OK, on affiche les sorties
        plotly::plotlyOutput(
          outputId = ns("plot_metriques_i2m2"),
          height = "450px"),

        shiny::br(),
        shiny::downloadButton(
          outputId = ns("download_metriques_i2m2"),
          label = "Télécharger les métriques (.csv)"),

        shiny::br(),
        DT::DTOutput(
          outputId = ns("table_metriques_i2m2") ) ) } )

    # Graphique
    output$plot_metriques_i2m2 <- plotly::renderPlotly({
      shiny::req(donnees()) # Obligatoire
      shiny::req(station_selectionnee())
      graph <- fun_plot_metriques_i2m2( # Recupere la fonction
        metriques = donnees()$metriques,
        station_id = station_selectionnee())
      shiny::req(graph) # Bloque si null
      graph } )

    # Tableau des métriques I2M2
    output$table_metriques_i2m2 <- DT::renderDT({ # Rendu tableau metriques
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station
      table_metriques <- donnees()$metriques %>% # Table metriques
        dplyr::filter(code_station == station_selectionnee()) # Station choisie

      shiny::req(nrow(table_metriques) > 0) # Bloque le tableau si aucune ligne
      table_metriques <- table_metriques %>% # Reprend la table filtree
        dplyr::mutate(
          dplyr::across(
            where(is.numeric), # Colonnes numeriques
            ~ round(.x, 3) ) ) # Arrondi

      DT::datatable(
        table_metriques, # Table affichee
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # 10 lignes par page
          scrollX = TRUE ) ) }, server = TRUE ) # Scroll horizontal

    # Export CSV des métriques I2M2
    output$download_metriques_i2m2 <- shiny::downloadHandler(
      filename = function() { # Nom du fichier
        paste0("metriques_I2M2_", station_selectionnee(), ".csv") },
      content = function(file) { # Contenu du fichier
        table_export <- donnees()$metriques %>%
          dplyr::filter(code_station == station_selectionnee()) %>%
          dplyr::mutate(
            dplyr::across(
              where(is.numeric),
              ~ round(.x, 3) ) ) %>%
          dplyr::relocate(id_metrique, .before = code_indice)
        utils::write.csv2(
          table_export,
          file,
          row.names = FALSE,
          fileEncoding = "UTF-8") } )
  } ) }

## À appeler dans l'UI
# mod_station_carte_ui("communaute_indices")

## À appeler dans le server
# mod_station_carte_server("communaute_indices")
