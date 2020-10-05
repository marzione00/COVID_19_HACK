# to be removed later on
regionTS = covid19:::get_regionTS()

# we need totale_positivi
regionTS$Abruzzo$totale_positivi %>% plot()

ent_df <- data.frame()

ent_df %>% dplyr::bind_rows(regionTS$Abruzzo)

ent_df <- data.frame()
for (i in names(regionTS)) {
  ent_df <- regionTS[[i]] %>%
    dplyr::select(denominazione_regione, data, totale_positivi) %>% 
    dplyr::rename(regione = denominazione_regione, positivi=totale_positivi) %>%
    dplyr::bind_rows(ent_df)
}

ent_df %>%
  dplyr::group_by(regione, year = lubridate::year(data), week = lubridate::week(data)) %>% 
  dplyr::summarise_if(is.numeric, mean)


