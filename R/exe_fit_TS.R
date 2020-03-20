#' Times curve fitting and forecasting 
#'
#' @param sample_cases_TS values for ARIMA FITTING.
#' @param p autoregressive parameter.
#' @param I integration order
#' @param q MA order
#' @param TS_t full lenght of the forecasting.
#'
#' @return a list of fitting results. Specifically:
#' \describe{
#'   \item{out_fit_TS}{The output of the the fit the ARIMA model (p,I,q)}
#' }
#'
#' @examples
#' \dontrun{
#'   data <- get_regionTS()
#'   data <- subset(data, denominazione_regione == "Lombardia")
#'   exe_fit_TS(data$totale_casi, data$data_seriale, c(1:50))
#' }
#'
#' @export
exe_fit_TS <- function(sample_cases_TS,p,I,q) {
  #log rescale
  #sample_cases_log=log(sample_cases_TS)
  
  #analysis
  #acf_plot<-acf(sample_cases_TS)
  #pacf_plot<-pacf(sample_cases_TS)
  
  # Curve fitting
  TS_cases<-arima(sample_cases_TS,order=c(p,I,q))
  
  #forecasting 
  #TS_forecast<-forecast::forecast(TS_cases,TS_t_forecast)
  
  # Chi-squared test
  #plot_residuals<-forecast::checkresiduals(TS_cases)
  
  #output_fin_TS <- list("out_fit_TS" = TS_cases)
  return(TS_cases)
}