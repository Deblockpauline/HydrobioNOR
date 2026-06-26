#' Module UI des taxons par station
#' @description Montre un graph des taxons présents dans la station et
#' un tableau exportable des données.
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
      selected = "Diatomées" ), # Choix par défaut

    shiny::uiOutput(ns("message_taxons")), # Message utilisateur

    shiny::div( # Zone de défilement du graphique
      style = "
        height: 650px;
        overflow-y: scroll;
        overflow-x: hidden;
        border: 1px solid #eeeeee;
        padding: 5px;
      ", # Style CSS de la zone
      plotly::plotlyOutput( # Sortie du graphique plotly
        outputId = ns("plot_taxons"), # ID du graphique
        height = "2000px" ) ), # Hauteur fixe dans la zone scroll

    shiny::br(), # Espace
    shiny::h4("Données"), # Titre partie tableau
    shiny::downloadButton( # Bouton d'export CSV
      outputId = ns("download_taxons"), # ID du bouton
      label = "Télécharger les données (.csv)"), # Texte du bouton
    shiny::br(), # Espace
    DT::DTOutput(ns("table_taxons") ) ) # Tableau des taxons
}

#' Module server des taxons par station
#' @noRd

mod_communaute_taxons_server <- function(id,
                                         donnees,
                                         station_selectionnee,
                                         choix_eqb = NULL,
                                         choix_reseau = NULL,
                                         choix_qualification = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

# Met à jour le groupe taxon selon le filtre EQB global
    shiny::observe({ # Observe le filtre EQB
      if (!is.null(choix_eqb) && # Filtre existe
          !is.null(choix_eqb()) && # Valeur existe
          length(choix_eqb()) > 0 && # Au moins 1 choix
          !("Tous" %in% choix_eqb())) { # Pas tous
        shiny::updateSelectInput( # Met à jour la liste
          session = session, # Session du module
          inputId = "groupe_taxon", # ID
          selected = choix_eqb() ) } } ) # Choix global

# Filtre les données
    taxons_filtres <- shiny::reactive({ # Données filtrées
      shiny::req(donnees()) # Vérifie les données
      shiny::req(!is.null(donnees()$taxons)) # Vérifie la table taxons
      shiny::req(station_selectionnee()) # Vérifie la station
      shiny::req(input$groupe_taxon) # Vérifie le groupe choisi

      df <- donnees()$taxons # Table taxons
      if (!is.null(donnees()$stations) && "reseau" %in% names(donnees()$stations)) { # Si stations existe et qu'on a un reseau
        stations_reseau <- donnees()$stations |> # Table stations
          sf::st_drop_geometry() |> # Supprime la géométrie si sf
          dplyr::select(code_station, reseau) |> # Garde station + réseau
          dplyr::distinct() # Supprime les doublons

        df <- df |> # Ajout réseau depuis les stations
          dplyr::left_join(
            stations_reseau, # Table réseau
            by = "code_station") } # Jointure station

      df <- df |> # Filtre la station
        dplyr::filter(code_station %in% station_selectionnee()) # Station sélectionnée

      df <- filtrer_donnees( # Applique les filtres globaux
        data = df, # Table taxons
        choix_eqb = NULL, # Pas ici, évite le double filtre EQB
        choix_reseau = if (!is.null(choix_reseau)) choix_reseau() else NULL, # Filtre réseau
        choix_qualification = if (!is.null(choix_qualification)) choix_qualification() else NULL ) # Filtre qualification

      df <- df |> # Ajoute l'année
        dplyr::mutate(
          annee = lubridate::year(date_prelevement) ) # Année prélèvement

      df <- df |> # Filtre le groupe biologique avec la correspondance
        dplyr::filter(
          dplyr::case_when(
            input$groupe_taxon == "Diatomées" ~ libelle_support == "Diatomées benthiques",
            input$groupe_taxon == "Macrophytes" ~ libelle_support == "Macrophytes",
            input$groupe_taxon == "Poissons" ~ libelle_support == "Poissons",
            input$groupe_taxon == "Macroinvertébrés" ~ libelle_support == "Macroinvertébrés aquatiques",
            TRUE ~ FALSE) ) # Sécurité

      df <- df |> # Garde les taxons présents
        dplyr::filter(
          !is.na(resultat_taxon), # Résultat non vide
          resultat_taxon > 0) # Résultat positif

      df <- df |> # Garde une ligne par taxon/année
        dplyr::group_by(
          code_station,
          libelle_station,
          annee,
          libelle_support,
          code_appel_taxon,
          libelle_taxon,
          code_qualification,
          libelle_qualification,
          reseau) |>
        dplyr::slice_max( # Prendre la plus grande valeur
          order_by = resultat_taxon, # Valeur utilisée
          n = 1, # Une seule ligne
          with_ties = FALSE) |> # Pas d'égalité
        dplyr::ungroup() # Supprime les groupes

      df } )# Retourne la table

# Table a afficher
    table_taxons_affichage <- shiny::reactive({ # Table affichée/exportée
      shiny::req(taxons_filtres()) # Vérifie les données filtrées
      taxons_filtres() |> # Table filtrée
        dplyr::select(
          date_prelevement,
          annee,
          code_prelevement,
          code_support,
          libelle_support,
          code_appel_taxon,
          libelle_taxon,
          resultat_taxon,
          abondance_relative,
          code_qualification,
          libelle_qualification,
          reseau) } )

# Message
    output$message_taxons <- shiny::renderUI({ # Message utilisateur
      if (is.null(station_selectionnee()) || length(station_selectionnee()) == 0 || station_selectionnee() == "") { # Si aucune station
        return(
          shiny::div(
            style = "color: #666; font-style: italic;", # Style message
            "Cliquez sur une station de la carte pour afficher les taxons.") ) } # Texte
      df <- taxons_filtres() # Données filtrée
      if (nrow(df) == 0) { # Si aucune donnée
        return(
          shiny::div(
            style = "color: #666; font-style: italic;", # Style message
            "Aucune donnée disponible pour ce groupe biologique sur cette station.") ) } # Texte
      NULL } ) # Aucun message si données présentes

# Graph
    output$plot_taxons <- plotly::renderPlotly({ # Graphique taxons
      shiny::validate(
        shiny::need(station_selectionnee(), "Cliquez sur une station."), # Besoin station
        shiny::need(nrow(taxons_filtres()) > 0, "Aucune donnée disponible.") ) # Besoin données

      fun_plot_taxons_station( # Appel de la fonction
        taxons = taxons_filtres()) %>% # Données filtrées
        plotly::config(
          toImageButtonOptions = list(
            format = "png", # Format
            filename = paste0(
              "taxons_",
              input$groupe_taxon,
              "_",
              station_selectionnee() ), # Nom fichier
            height = 2000, # Hauteur image
            width = 1200, # Largeur image
            scale = 2) ) } ) # Qualité

# Tableau
    output$table_taxons <- DT::renderDT({ # Tableau taxons
      shiny::validate(
        shiny::need(nrow(table_taxons_affichage()) > 0, "Aucune donnée à afficher.") ) # Besoin données
      DT::datatable(
        table_taxons_affichage(), # Table à afficher
        rownames = FALSE, # Pas de noms de lignes
        options = list(
          pageLength = 10, # 10 lignes par page
          scrollX = TRUE) ) } ) # Défilement horizontal

# Table exportable
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
          ".csv") },
      content = function(file) { # Contenu exporté
        table_export <- table_taxons_affichage() # Table export
        utils::write.csv2(
          table_export, # Données exportées
          file, # Chemin fichier
          row.names = FALSE, # Pas noms lignes
          fileEncoding = "UTF-8") } ) # Encodage
 } ) }

## À appeler dans l'UI
# mod_communaute_taxons_ui("taxons")

## À appeler dans le SERVER
# mod_communaute_taxons_server ("taxons")

