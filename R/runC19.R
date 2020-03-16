#' Shiny app with exploratory analysis of covid19
#' 
#' The function works with no arguments. The function depends on the dataset \code{\link{covid19}}
#'
#' @return Runs the Shiny app retreiving the data, running the analysis and showing the report with interactive visualisations.
#' 
#' @seealso \code{\link{covid19}}
#' 
#' @export
runC19 <- function() {

# run app -----------------------------------------------------------------
  shiny::runApp(system.file("shiny/C19", package = "covid19"), launch.browser = T)

}
