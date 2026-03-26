#' Application UI
#'
#' @param request Internal parameter for {shiny}
#' @noRd
app_ui <- function(request) {
  shiny::fluidPage(
    shiny::titlePanel("Hydrobio Normandie - Version 1"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("Chargement des données"),
        mod_load_data_ui("load_data")
      ),

      shiny::mainPanel(
        shiny::h3("Bienvenue"),
        shiny::p("Version 1 de l'application."),
        shiny::p("Les autres modules seront ajoutés progressivement.")
      )
    )
  )
}
