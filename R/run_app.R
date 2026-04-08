#' Lancer l'application
#'
#' @param ... Arguments passés à shiny::shinyApp
#'
#' @return Une application shiny
#' @export
#'
run_app <- function(...) {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )
}
