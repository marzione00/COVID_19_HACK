#' Fit of the data and forecast via the parameters estimed
#'
#' @param sample_cases values for growth fitting.
#' @param sample_date sample dates as serial consecutive dates (should be included in \code{days}).
#' @param days full vector of serial dates.
#'
#' @return a list of fitting results
#' \describe{
#'   \item{out_fit}{The output of the fitting performed by \link[growthcurver]{SummarizeGrowth}.}
#'   \item{fittedPoints}{A data frame containing dates and corresponding fitted values listed by columns}
#'   \item{out_chisq}{The output of the chi-squared independence test of residuals}
#' }
#'
#' @examples
#' \dontrun{
#'   data <- get_regionTS()
#'   data <- subset(data, denominazione_regione == "Lombardia")
#'   exe_fit(data$totale_casi, data$data_seriale, c(1:50))
#' }
#'
#' @export
exe_fit <- function(sample_cases, sample_date, days) {
  # Curve fitting
  out_fit <- growthcurver::SummarizeGrowth(sample_date, sample_cases)
  k=out_fit[["vals"]][["k"]]
  n0=out_fit[["vals"]][["n0"]]
  r=out_fit[["vals"]][["r"]]

  # Creation of fitted points
  yFitted <- (n0*k)/(n0 + (k-n0) * exp(-r*days))
  fittedPoints <- data.frame(days,yFitted)

  # Chi-squared test
  yFitted_chi <- (n0*k)/(n0 + (k-n0) * exp(-r*sample_date))
  output_chisquared <- stats::chisq.test(sample_cases,yFitted_chi)

  output_fin <- list("out_fit" = out_fit, "fittedPoints" = fittedPoints,
                     "out_chisq" = output_chisquared)
  return(output_fin)
}
