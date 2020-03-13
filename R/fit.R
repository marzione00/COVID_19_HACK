#' Fit of the data and forecast via the parameters estimed
#'
#' @return To be done
#'
#' @examples
#' To be done
#' To be done
#'
#' @export
exe_fit<-fit(cases,date,date_forecast) {
out_fit <- growthcurver::SummarizeGrowth(date,Cases)
k=out_fit[["vals"]][["k"]]
n0=out_fit[["vals"]][["k"]]
r=out_fit[["vals"]][["r"]]
# Creation of fitted points
yFitted <- (n0*k)/(n0+(K-n0) * exp(-r*Days) )
yFitted_chi <- (n0*k)/(n0+(K-n0) * exp(-r*date))
fittedPoints <- data.frame(date_forecast,yFitted)
output_chisquared<-chisq.test(cases,yFitted)
output_fin<-data.frame(coeff,fittedPoints,output_chisquared[["p.value"]])
return(output_fin)
}



