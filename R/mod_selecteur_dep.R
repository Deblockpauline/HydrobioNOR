#' Module UI du sélecteur de département
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_dep_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList( # Menu déroulant pour choisir un département
    shiny::selectizeInput(
      inputId = ns("departement"),
      label = "Département", # Nom
      choices = "Tous", # Initialisé avec tous
      selected = "Tous",
      multiple = TRUE) ) } # Tous par defaut

#' Module server du sélecteur de département
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant les données
#' @noRd

mod_selecteur_dep_server <- function(id, donnees) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({  # Mise à jour de la liste des départements à partir des données
      shiny::req(donnees()) # Verifie que données existe
      shiny::req("donnee_carte" %in% names(donnees())) # Verifie que donnee_carte est present
      df <- donnees()$donnee_carte # Stockage
      shiny::req(!is.null(df))  # Sécurité : pas que ca soit nul et qu'on est un code_dep
      shiny::req("code_dep" %in% names(df)) # Presence de la colonne code_dep

      liste_departements <- df |>  # Récupération des départements
        dplyr::pull(code_dep) |> # Prend la colonne code_dep
        unique() |> # Enleve les doublons
        sort() # Trie

      shiny::updateSelectizeInput( # Mise à jour du selectInput avec les départements
        session = session,
        inputId = "departement", # Defini dans le ui
        choices = c("Tous", liste_departements),# Tous et ceux trouvé
        selected = "Tous") } )
    return(shiny::reactive(input$departement) ) # Renvoie le dep séléctionné en réactive
  } ) }

## À appeler dans l'UI
# mod_selecteur_dep_ui("departements")

## À appeler dans le server
# mod_selecteur_dep_server("departements")
