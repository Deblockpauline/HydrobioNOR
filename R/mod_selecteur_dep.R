#' Module UI du sélecteur de département
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_dep_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList( # Menu déroulant pour choisir un département
    shiny::selectInput(
      inputId = ns("departement"),
      label = "Département",
      choices = "Tous", # Initialisé avce tous
      selected = "Tous" ) ) } # Tous par defaut

#' Module server du sélecteur de département
#'
#' @param id Identifiant du module
#' @param donnees Reactive contenant la liste renvoyée par mod_load_data_server()
#' @noRd

mod_selecteur_dep_server <- function(id, donnees) {
  shiny::moduleServer(id, function(input, output, session) {

    # Mise à jour de la liste des départements à partir des données
    shiny::observe({
      shiny::req(donnees()) # Verifie que données existe
      shiny::req("donnee_carte" %in% names(donnees())) # Verifie que donnee_carte est present
      df <- donnees()$donnee_carte
      shiny::req(!is.null(df))  # Sécurité
      shiny::req("code_dep" %in% names(df))

      # Récupération des départements uniques présents dans les données
      liste_departements <- df |>
        dplyr::pull(code_dep) |> # Prend le colonne
        unique() |> # Enleve les doublons
        sort() # Trie

      # Mise à jour du selectInput avec les départements disponibles
      shiny::updateSelectInput(
        session = session,
        inputId = "departement",
        choices = c("Tous", liste_departements),
        selected = "Tous")
    } )
    return(shiny::reactive(input$departement)) # Renvoie le dep séléctionné
  } ) }
