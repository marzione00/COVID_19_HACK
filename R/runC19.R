#' Shiny app with exploratory analysis of the term structure of interest rates
#' 
#' The function works with no arguments. The function depends on the dataset \code{\link{term_structure}}
#'
#' @return Runs the Shiny app retreiving the data, running the analysis and showing the report with interactive visualisations.
#' 
#' @seealso \code{\link{term_structure}}
#' 
#' @export
runC19 <- function() {

# run app -----------------------------------------------------------------
  shiny::runApp(system.file("shiny/C19", package = "Covid19"), launch.browser = T)

}