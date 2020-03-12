#' Data acquisition
#'
#' @return the national dataset
#'
#' @export
get_countryTS <- function() {
  data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv",
                   header = TRUE)

  data$data <- as.POSIXct(data$data, format = "%Y-%m-%d %H", tz = "Europe/Berlin")

  return(data)

}
