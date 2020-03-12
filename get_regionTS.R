get_regionTS <- function() {
  data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
                   header = TRUE)
  
  data$data <- as.POSIXct(data$data, format = "%Y-%m-%d %H", tz = "Europe/Berlin")
  
  out <- list()
  for(region in unique(data$denominazione_regione)) {
    out[[as.character(region)]] <- subset(data, denominazione_regione == region)
  }
    
  return(out)
}
