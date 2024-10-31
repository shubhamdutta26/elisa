#' Launch the Shiny App
#'
#' @description A function to launch the Shiny app within the package.
#' @export
runShinyApp <- function() {
  app_dir <- system.file("shiny", package = "elisa")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `elisa`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
