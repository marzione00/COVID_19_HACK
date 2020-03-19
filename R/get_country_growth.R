#' Data manipulation
#'
#' @export
get_country_growth <- function() {
  
  get_countryTS() %>%
    select(totale_casi) %>%
    mutate(growth=round(((totale_casi-lag(totale_casi))/lag(totale_casi))*100,2) ) %>%
    mutate(growth_change=round(((growth-lag(growth))/lag(growth))*100,2) )

}
