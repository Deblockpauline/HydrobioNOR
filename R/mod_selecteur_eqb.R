#' Module UI du sélecteur des elements de qualité biologique (EQB)
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_eqb_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      inputId = ns("eqb"), # Definition NS
      label = "Élément de qualité biologique",
      choices = c( # Liste definie en avance car on connait les EQB
        "Tous",
        "Diatomées",
        "Macroinvertébrés",
        "Macrophytes",
        "Poissons" ),
      selected = "Tous",
      multiple=TRUE) ) } # Par defaut

#' Module server du sélecteur EQB
#'
#' @param id Identifiant du module
#' @return Une reactive contenant l'EQB sélectionné
#' @noRd

mod_selecteur_eqb_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive( {input$eqb} ) # Reactive qui prend la selection de l'EQB
  } ) }

## À appeler dans l'UI
# mod_selecteur_eqb_ui ("eqb")

## À appeler dans le server
# mod_selecteur_eqb_server("eqb")
