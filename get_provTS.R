#' Data acquisition sorted by province
#' 
#' @return A list containing the original dataset splitted into single provinces.
#' 
#' @examples 
#' provTS <- get_provTS()
#' names(provTS) # List of provinces names
#'
get_provTS <- function() {
  data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv",
                   header = TRUE, na.strings = "In fase di definizione/aggiornamento")
  
  data$data <- as.POSIXct(data$data, format = "%Y-%m-%d %H", tz = "Europe/Berlin")
  
  out <- list()
  for(province in unique(data$denominazione_provincia)) {
    out[[as.character(province)]] <- subset(data, denominazione_provincia == province)
  }
  
  return(out)
}
