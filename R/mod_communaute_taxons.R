#' Module UI des taxons par station
#'
#' @description Montre un graph des taxons présents dans la station et
#' un tableau exportable des données.
#' @param id Identifiant du module
#' @noRd

mod_communaute_taxons_ui <- function(id) { # Fonction UI du module
  ns <- shiny::NS(id) # Namespace du module
  shiny::tagList( # Regroupe les éléments UI
    shiny::h4("Taxons observés"), # Titre partie graphique

    shiny::selectInput( # Liste déroulante des groupes
      inputId = ns("groupe_taxon"), # ID du selectInput
      label = "Groupe biologique", # Texte affiché
      choices = c( # Choix disponibles
        "Diatomées",
        "Macrophytes",
        "Poissons",
        "Macroinvertébrés" ),
      selected = "Diatomées"), # Choix par défaut

    shiny::uiOutput(ns("message_taxons")), # Message utilisateur
    shiny::div( # Zone de défilement du graphique
      style = "
        height: 650px;# Hauter max
        overflow-y: scroll; # Ajoute la barre de defilement
        overflow-x: hidden; # Pas de defilement horizontal
        border: 1px solid #eeeeee; # Bordure grise
        padding: 5px;
      ", # Style CSS de la zone

      plotly::plotlyOutput( # Sortie du graphique plotly
        outputId = ns("plot_taxons"), # ID du graphique
        height = "2000px") ), # Hauteur fixe dans la zone scroll

    shiny::br(), # Espace
    shiny::h4("Données"), # Titre partie tableau
    shiny::downloadButton( # Bouton d'export CSV
      outputId = ns("download_taxons"), # ID du bouton
      label = "Télécharger les données (.csv)"), # Texte du bouton
    shiny::br(), # Espace
    DT::DTOutput(ns("table_taxons") ) ) # Tableau des taxons
}

#' Module server des taxons par station
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données
#' @param station_selectionnee Reactive contenant le code station sélectionné
#' @noRd

mod_communaute_taxons_server <- function(id, # ID du module
                                         donnees,
                                         station_selectionnee) {

  shiny::moduleServer(id, function(input, output, session) {

    taxons_filtres <- shiny::reactive({ # Données filtrées
      shiny::req(donnees()) # Vérifie les données
      shiny::req(donnees()$taxons) # Vérifie la table taxons
      shiny::req(station_selectionnee()) # Vérifie la station
      shiny::req(input$groupe_taxon) # Vérifie le groupe choisi
      df <- donnees()$taxons # Table taxons
      df <- df |> # Filtre la station
        dplyr::filter(code_station == station_selectionnee()) # Station sélectionnée
      df <- df |> # Ajoute l'année
        dplyr::mutate(annee = lubridate::year(date_prelevement)) # Année prélèvement
      df <- df |> # Filtre le groupe biologique
        dplyr::filter(
          dplyr::case_when(
            input$groupe_taxon == "Diatomées" ~ libelle_support == "Diatomées benthiques", # Diatomées
            input$groupe_taxon == "Macrophytes" ~ libelle_support == "Macrophytes", # Macrophytes
            input$groupe_taxon == "Poissons" ~ libelle_support == "Poissons", # Poissons
            input$groupe_taxon == "Macroinvertébrés" ~ libelle_support == "Macroinvertébrés aquatiques", # Macroinvertébrés
            TRUE ~ FALSE) ) # Sécurité
      df <- df |> # Garde les taxons présents
        dplyr::filter(
          !is.na(resultat_taxon), # Résultat non vide
          resultat_taxon > 0) # Résultat positif
      df <- df |> # Garde une ligne par taxon/année
        dplyr::group_by(
          code_station, # Code station
          libelle_station, # Nom station
          annee, # Année
          libelle_support, # Support
          code_appel_taxon, # Code taxon
          libelle_taxon) |> # Nom taxon
        dplyr::slice_max(
          order_by = resultat_taxon, # Valeur utilisée
          n = 1, # Une seule ligne
          with_ties = FALSE) |> # Pas d'égalité
        dplyr::ungroup() # Supprime les groupes
      df } ) # Retourne la table


    table_taxons_affichage <- shiny::reactive({ # Table affichée/exportée
      shiny::req(taxons_filtres()) # Vérifie les données filtrées
      taxons_filtres() |> # Table filtrée
        dplyr::select(
          date_prelevement, # Date prélèvement
          annee, # Année
          code_prelevement, # Code prélèvement
          code_support, # Code support
          libelle_support, # Nom support
          code_appel_taxon, # Code taxon
          libelle_taxon, # Nom taxon
          resultat_taxon, # Résultat taxon
          abondance_relative) } ) # Abondance relative

    output$message_taxons <- shiny::renderUI({ # Message utilisateur
      if (is.null(station_selectionnee()) || station_selectionnee() == "") { # Si aucune station
        return(
          shiny::div(
            style = "color: #666; font-style: italic;", # Style message
            "Cliquez sur une station de la carte pour afficher les taxons.") ) } # Texte
      df <- taxons_filtres() # Données filtrées
      if (nrow(df) == 0) { # Si aucune donnée
        return(
          shiny::div(
            style = "color: #666; font-style: italic;", # Style message
            "Aucune donnée disponible pour ce groupe biologique sur cette station." ) ) } # Texte
      NULL } )# Aucun message si données présentes


    output$plot_taxons <- plotly::renderPlotly({ # Graphique taxons
      shiny::validate(
        shiny::need(station_selectionnee(), "Cliquez sur une station."), # Besoin station
        shiny::need(nrow(taxons_filtres()) > 0, "Aucune donnée disponible.") ) # Besoin données
      fun_plot_taxons_station( # Appel de la fonction
        taxons = taxons_filtres() ) } ) # Création graphique

    output$table_taxons <- DT::renderDT({ # Tableau taxons
      shiny::validate(
        shiny::need(nrow(table_taxons_affichage()) > 0, "Aucune donnée à afficher.") ) # Besoin données

      DT::datatable(
        table_taxons_affichage(), # Table à afficher
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # 10 lignes par page
          scrollX = TRUE) ) } ) # Défilement horizontal

    output$download_taxons <- shiny::downloadHandler( # Export CSV
      filename = function() { # Nom du fichier
        nom_groupe <- input$groupe_taxon |> # Groupe choisi
          stringr::str_replace_all("é", "e") |> # Nettoie é
          stringr::str_replace_all("è", "e") |> # Nettoie è
          stringr::str_replace_all("ê", "e") |> # Nettoie ê
          stringr::str_replace_all(" ", "_") # Remplace espaces
        paste0(
          "donnees_taxons_", # Préfixe fichier
          nom_groupe, # Groupe biologique
          "_", # Séparateur
          station_selectionnee(), # Code station
          ".csv") }, # Extension

      content = function(file) { # Contenu exporté
        table_export <- table_taxons_affichage() # Table export
        utils::write.csv2(
          table_export, # Données exportées
          file, # Chemin fichier
          row.names = FALSE, # Pas noms lignes
          fileEncoding = "UTF-8") } # Encodage
    ) } ) }
