runMultiFlowApp <- function() {
  appDir <- system.file("shinyapp", "MultiFlowShinyApp.R", package = "MultiFlow")
  if (appDir == "") {
    stop("Could not find directory of shiny app. Try re-installing `MultiFlow`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}