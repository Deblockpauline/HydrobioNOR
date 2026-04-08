#' Module UI du sélecteur EQB
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_eqb_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList( # Menu déroulant pour choisir l'élément de qualité biologique
    shiny::selectInput(
      inputId = ns("indice"),
      label = "Élément de qualité biologique",
      choices = c(
        "Tous",
        "Diatomées",
        "Macroinvertébrés",
        "Macrophytes",
        "Poissons"),
      selected = "Tous" # Valeur par défauts
    ) ) }

#' Module server du sélecteur EQB
#'
#' @param id Identifiant du module
#' @noRd

mod_selecteur_eqb_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    return(shiny::reactive(input$indice) )  # Renvoie la valeur sélectionnée (reactive)
  } ) }
# Module assez simple car les compartiments biologique sont deja connus et ne change pas donc une simple réactie suffit
