#' Generate GUI interface
#' @name sGUI
#' @description GUI function
#'
#' This app allows to generate a GUI that plots the data of COVID19
#'
#' @examples
#' \dontrun{
#' sGUI()
#' }
#' @return shiny app
#' @export sGUI





sGUI <- function(){
  dir <- system.file("shinyApp", package = "CovidPack")
  shiny::runApp(dir)
}

