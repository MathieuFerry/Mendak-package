#' Lance l'application Shiny
#'
#' Cette fonction lance l'application définie dans app_ui() et app_server().
#'
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
