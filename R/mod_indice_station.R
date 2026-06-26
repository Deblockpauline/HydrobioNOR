#' Module UI des indices biologiques
#' @description Interface du sous-onglet Indices.
#' @param id Identifiant du module
#' @noRd

mod_communaute_indices_ui <- function(id) { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les elements UI
    shiny::h4("Indices biologiques"), # Titre partie indices
    shiny::div(
      style = "font-size: 12px; color: #555; line-height: 1.4;", # Style discret et lisible
      shiny::p(
        "Selon l'arrété du 27 juillet 2018, version EBio_CE_2018_v1.0.2.")),
    shiny::uiOutput(ns("message_indices")), # Message si pas de station ou pas de donnees
    shiny::uiOutput(ns("plots_indices")), # Zone des graphiques + tableaux
    shiny::br(), # Espace
    shiny::hr(), # Ligne
    shiny::h4("Métriques de l'I2M2"), # Titre partie metriques
    shiny::uiOutput(ns("bloc_metriques_i2m2") ) ) } # Bloc dynamique metriques )

#' Module server des indices biologiques
#' @description Serveur du sous-onglet Indices.
#' @noRd

mod_communaute_indices_server <- function(id, donnees, station_selectionnee, choix_qualification = NULL, choix_eqb = NULL) {

  shiny::moduleServer(id, function(input, output, session) { # Debut module server
    ns <- session$ns # Namespace cote serveur

# Graphiques des indices biologiques
    graphiques_indices <- shiny::reactive({ # Reactive qui cree les graphiques
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station selectionnée
      etat_bio_filtre <- donnees()$etat_bio # Table etat bio
      etat_bio_filtre <- etat_bio_filtre %>% # Filtre station avant les graph
        dplyr::filter(code_station == station_selectionnee()) # Station choisie

      # Filtre EQB
      if (!is.null(choix_eqb) && # Filtre existe
          !is.null(choix_eqb()) && # Valeur existe
          length(choix_eqb()) > 0 && # Au moins 1 choix
          !("Tous" %in% choix_eqb()) && # Pas tous
          "libelle_support" %in% names(etat_bio_filtre)) { # Colonne existe

        supports_gardes <- dplyr::case_when( # Correspondance EQB/support
          choix_eqb()[1] == "Diatomées" ~ "Diatomées benthiques",
          choix_eqb()[1] == "Macrophytes" ~ "Macrophytes",
          choix_eqb()[1] == "Poissons" ~ "Poissons",
          choix_eqb()[1] == "Macroinvertébrés" ~ "Macroinvertébrés aquatiques",
          TRUE ~ NA_character_) # Sécurité
        etat_bio_filtre <- etat_bio_filtre %>% # Filtre par support
          dplyr::filter(libelle_support %in% supports_gardes) } # En réalisant la correspondance

      # Filtre qualification
      if (!is.null(choix_qualification) && # Filtre existe
          !is.null(choix_qualification()) && # Valeur existe
          length(choix_qualification()) > 0 && # Au moins 1 choix
          !("Toutes" %in% choix_qualification()) && # Pas toutes
          "libelle_qualification" %in% names(etat_bio_filtre)) { # Colonne existe
        etat_bio_filtre <- etat_bio_filtre %>% # Filtre dans etat_bio
          dplyr::filter(libelle_qualification %in% choix_qualification()) } # Garde qualification

      # Appel de la fonction pour la création des graphiques pour les indices
      fun_plot_indices_station(
        etat_bio = etat_bio_filtre, # Donnees filtrées
        station_id = station_selectionnee()) } ) # Station choisie

# Message au-dessus des graphiques d'indices
    output$message_indices <- shiny::renderUI({ # Cree le message
      if (is.null(station_selectionnee()) || is.na(station_selectionnee()) || station_selectionnee() == "") { # Si pas de station
        return(
          shiny::div(
            style = "color: #666; font-style: italic;",
            "Cliquez sur une station de la carte pour afficher ses indices biologiques.") ) } # Message
      plots <- graphiques_indices() # Recupere les graphiques
      if (is.null(plots) || length(plots) == 0) { # Si rien a afficher
        return(
          shiny::div(
            style = "color: #666; font-style: italic;",
            "Aucune donnée d'indice biologique disponible pour cette station.") ) } # Message
      NULL } ) # Sinon pas de message

# Création de l'interphase
    output$plots_indices <- shiny::renderUI({
      plots <- graphiques_indices() # Recupere la liste graph + table
      if (is.null(plots) || length(plots) == 0) { return(NULL) } # Si c'est NULL on affiche rien

      shiny::tagList( # Regroupe tous les blocs
        purrr::map( # Boucle sur les indices
          seq_along(plots), # Retourne le numero de chaque indice exemple : IBD =1 IBMR=2 etc...

          function(i) { # Fonction pour 1 indice
            nom_indice <- unique(plots[[i]]$table$libelle_indice)[1] # Nom indice pour en faire un titre
            shiny::tagList( # Bloc pour 1 indice
              shiny::h5( # Titre graphique
                paste0("Graphique de l'", nom_indice),
                style = "font-weight: bold; margin-top: 15px;"), # Style titre
              plotly::plotlyOutput( # Sortie graphique
                outputId = ns(paste0("plot_indice_", i)), # Id unique du graphique
                height = "430px"), # Hauteur graphique
              shiny::br(), # Espace
              shiny::downloadButton( # Bouton CSV
                outputId = ns(paste0("download_indice_", i)), # Id unique du bouton
                label = "Télécharger les données (.csv)"), # Texte bouton
              shiny::br(), # Espace
              DT::DTOutput( # Sortie tableau
                outputId = ns(paste0("table_indice_", i))), # Id unique du tableau
              shiny::br(), # Espace
              shiny::hr() ) } ) # Ligne séparation
     ) } )

# Remplissage des emplacement crée au dessus
    shiny::observe({ # Observe pour creer les sorties
      plots <- graphiques_indices() # Recupere les graphiques/table
      if (is.null(plots) || length(plots) == 0) {return(NULL) }
       purrr::walk( # Boucle sans creer de liste
        seq_along(plots), # Numero de chaque indice

        # Fonction pour chaque indice
        function(i) {
          local({ # Evite les soucis de boucle Shiny
            ii <- i # A chaque tout R mémorise 1, puis au suivant 2 etc...car sans cela, si par exemple on avait 3 indices R confondrait a afficherai 3 fois le meme

            # Nom des graphs
            output[[paste0("plot_indice_", ii)]] <- plotly::renderPlotly({ # Rendu du graphique
              nom_indice <- unique(plots[[ii]]$table$libelle_indice)[1] # Nom indice
              plots[[ii]]$graph %>% # Graphique plotly
                plotly::config(
                  toImageButtonOptions = list(
                    format = "png", # Format
                    filename = paste0(
                      "indice_",
                      nom_indice,
                      "_",
                      station_selectionnee() ), # Nom fichier
                    height = 800, # Hauteur
                    width = 1200, # Largeur
                    scale = 2 ) ) } )# Qualité

            # Nom de la table
            output[[paste0("table_indice_", ii)]] <- DT::renderDT({ # Rendu du tableau
              table_affichage <- plots[[ii]]$table %>% # Table associee au graphique
                dplyr::mutate(
                  dplyr::across(
                    where(is.numeric), # Colonnes numeriques
                    ~ round(.x, 3))) # Arrondi a 0.001

              DT::datatable( # Cree le tableau interactif
                table_affichage, # Table arrondie pour affichage
                rownames = FALSE, # Pas de noms de lignes
                options = list(
                  pageLength = 10, # 10 lignes par page
                  scrollX = TRUE)) # Scroll horizontal si besoin
            }, server = TRUE)

            # Export
            output[[paste0("download_indice_", ii)]] <- shiny::downloadHandler(
              filename = function() { # Nom du fichier
                nom_indice <- unique(plots[[ii]]$table$libelle_indice)[1] # Recupere le nom de l'indice
                paste0("donnees_indice_", nom_indice, "_", station_selectionnee(), ".csv") }, # Nom final
              content = function(file) { # Contenu du fichier
                table_export <- plots[[ii]]$table %>% # Table a exporter
                  dplyr::mutate(
                    dplyr::across(
                      where(is.numeric), # Colonnes numeriques
                      ~ round(.x, 3))) # Arrondi a 0.001
                utils::write.csv2( # Ecrit le CSV
                  table_export, # Table arrondie exportee
                  file, # Chemin du fichier
                  row.names = FALSE, # Pas de noms de lignes
                  fileEncoding = "UTF-8") } ) # Encodage

          } ) } )
    } )

#### PARTIE I2M2 METRIQUES

# Table des métriques filtrées
    metriques_filtrees <- shiny::reactive({ # Reactive des metriques filtrees
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station
      table_metriques <- donnees()$metriques %>% # Table metriques
        dplyr::filter(code_station == station_selectionnee()) # Station choisie

      # Filtre EQB
      if (!is.null(choix_eqb) && # Filtre existe
          !is.null(choix_eqb()) && # Valeur existe
          length(choix_eqb()) > 0 && # Au moins 1 choix
          !("Tous" %in% choix_eqb()) && # Pas tous
          choix_eqb()[1] != "Macroinvertébrés") { # Si le choix n'est pas MIV
        table_metriques <- table_metriques[0, ]  }# Alors les metriques ne sont pas affichées

      # Filtre qualification
      if (!is.null(choix_qualification) && # Filtre existe
          !is.null(choix_qualification()) && # Valeur existe
          length(choix_qualification()) > 0 && # Au moins 1 choix
          !("Toutes" %in% choix_qualification()) && # Pas toutes
          "libelle_qualification" %in% names(table_metriques)) { # Colonne existe
        table_metriques <- table_metriques %>% # Table metriques filtree
          dplyr::filter(libelle_qualification %in% choix_qualification()) } # Qualification choisie
      table_metriques} ) # Retour de la table filtree

# Interphase des métriques I2M2
    output$bloc_metriques_i2m2 <- shiny::renderUI({ # Affiche ou non la partie metriques
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station
      table_metriques <- metriques_filtrees() # Table metriques filtree

      if (nrow(table_metriques) == 0) { # Si pas de lignes
        return(
          shiny::div( # Message si pas de données
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
    output$plot_metriques_i2m2 <- plotly::renderPlotly({ # Rendu graphique metriques
      shiny::req(donnees()) # Obligatoire
      shiny::req(station_selectionnee()) # Attend une station
      shiny::req(nrow(metriques_filtrees()) > 0) # Attend des lignes
      graph <- fun_plot_metriques_i2m2( # Recupere la fonction
        metriques = metriques_filtrees(), # Table metriques filtree
        station_id = station_selectionnee()) # Station choisie

      shiny::req(graph) # Bloque si null
      graph %>%
        plotly::config( # Nom de l'export
          toImageButtonOptions = list(
            format = "png", # Format
            filename = paste0(
              "metriques_I2M2_",
              station_selectionnee() ), # Nom fichier
            height = 800, # Hauteur
            width = 1200, # Largeur
            scale = 2 ) ) } )# Qualité

# Tableau des métriques I2M2
    output$table_metriques_i2m2 <- DT::renderDT({ # Rendu tableau metriques
      shiny::req(donnees()) # Attend les donnees
      shiny::req(station_selectionnee()) # Attend une station
      shiny::req(nrow(metriques_filtrees()) > 0) # Bloque le tableau si aucune ligne
      table_metriques <- metriques_filtrees() %>% # Reprend la table filtree
        dplyr::mutate(
          dplyr::across(
            where(is.numeric), # Colonnes numeriques
            ~ round(.x, 3))) # Arrondi
      DT::datatable(
        table_metriques, # Table affichee
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # 10 lignes par page
          scrollX = TRUE)) # Scroll horizontal
    }, server = TRUE) # Allege acr ces' R qui le gere

# Export CSV des métriques I2M2
    output$download_metriques_i2m2 <- shiny::downloadHandler( # Export metriques
      filename = function() { # Nom du fichier
        paste0("metriques_I2M2_", station_selectionnee(), ".csv") }, # Nom final

      content = function(file) { # Contenu du fichier
        table_export <- metriques_filtrees() %>% # Table metriques filtree
          dplyr::mutate(
            dplyr::across(
              where(is.numeric), # Colonnes numeriques
              ~ round(.x, 3))) # Arrondi
        if ("id_metrique" %in% names(table_export) &&
            "code_indice" %in% names(table_export)) { # Colonnes presentes
          table_export <- table_export %>% # Reorganise les colonnes
            dplyr::relocate(id_metrique, .before = code_indice) }# id avant indice
        utils::write.csv2(
          table_export, # Table exportee
          file, # Chemin du fichier
          row.names = FALSE, # Pas noms lignes
          fileEncoding = "UTF-8") } )# Encodage
     } ) }

## À appeler dans l'UI
# mod_station_carte_ui("communaute_indices")

## À appeler dans le server
# mod_station_carte_server("communaute_indices")
