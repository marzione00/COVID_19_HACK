#================================
#====== GENERAL DATA ACQUISITION =====
countryTS = covid19:::get_countryTS()
regionTS = covid19:::get_regionTS()
provTS = covid19:::get_provTS()
country_growth = covid19:::get_country_growth()
intensivecare_capacity = covid19:::get_intensivecare_cap(regionTS)


#================================


#================================
#====== MODULE 1 - HOME ====== 

# --- region ---
map <- "https://raw.githubusercontent.com/stefanocudini/leaflet-geojson-selector/master/examples/italy-regions.json" %>% 
  httr::GET() %>% 
  httr::content() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE)

dfita1 <-  map$features %>% 
  purrr::map_df(function(x){
    as_data_frame(x$properties)
  })

pc_data <- regionTS

names(pc_data) <- tolower(names(pc_data))

pc_df <- purrr::map_df(names(pc_data), function(x){
  casi <- tail(pc_data[[x]],1)$totale_casi
  data_frame(name=x,cases=casi)
})

pc_df$name

# integrate population info
pop_region <- italy_pop$region %>% 
  rename(name=territorio,pop=valore) %>%
  mutate(name=tolower(name))

territory_region <- italy_ext$region %>%
  rename(name=territorio,ext=valore) %>% 
  mutate(name=tolower(name))


pc_df <- pc_df %>%
  left_join(pop_region) %>% 
  left_join(territory_region) %>%
  filter(!name%in%c("friuli v. g. ")) %>%
  mutate(name=ifelse(name%in%c("trento","bolzano","p.a. trento","p.a. bolzano"),
                     "trentino-alto adige/sudtirol",name)) %>%
  group_by(name) %>%
  summarise(cases=sum(cases), 
            pop=sum(pop),
            ext=sum(ext)) %>%
  mutate(name=ifelse(name=="emilia romagna","emilia-romagna",name))



a <- pc_df$name

b <- dfita1$name

setdiff(b,a)

dfita1 <- dfita1 %>%
  left_join(pc_df) %>%
  ungroup() %>% 
  mutate(percentage=(cases/pop)*100) %>%
  mutate(density=(cases/ext)*1000) %>%
  rename(absolute=cases) %>%
  mutate(percentage = round(percentage,2)) %>%
  mutate(density = round(density, 2))




# --- province ---

clean_prov <- map_df(names(provTS), function(x) {
  tail(provTS[[x]],1)$totale_casi
  data_frame(
    name=x,
    cases=tail(provTS[[x]],1)$totale_casi
  )
})


url <- "http://code.highcharts.com/mapdata/countries/it/it-all.geo.json"
tmpfile <- tempfile(fileext = ".json")
download.file(url, tmpfile)

ita <- readLines(tmpfile)

ita <- gsub(".* = ", "", ita)
ita <- jsonlite::fromJSON(ita, simplifyVector = FALSE)

x <- ita$features[[1]]
x$properties

dfita2 <-  ita$features %>% 
  map_df(function(x){
    data_frame(hasc = x$properties$hasc, name = x$properties$name)
  }) %>%  # extract the keys
  mutate(random = runif(nrow(.))) # create random value

head(dfita2)




# add population
pop_prov <- rename(italy_pop$province, name=territorio,pop=valore)

# add territory
territory_prov <- italy_ext$province %>%
  rename(name=territorio,ext=valore)


# make names consistent

clean_prov <- clean_prov %>%
  left_join(pop_prov) %>% 
  left_join(territory_prov) %>%
  mutate(name = ifelse(name=="Massa Carrara","Massa-Carrara",name)) %>%
  mutate(name = ifelse(name=="Reggio nell'Emilia","Reggio Emilia",name)) %>% 
  mutate(name = ifelse(name=="Bolzano","Bozen",name)) %>%
  mutate(name = ifelse(name=="Aosta","Aoste",name)) %>% 
  mutate(name = ifelse(name=="Monza e della Brianza","Monza e Brianza",name)) %>%
  mutate(name = ifelse(name=="Reggio di Calabria","Reggio Calabria",name)) %>%
  mutate(name = ifelse(name=="Torino","Turin",name)) %>%
  mutate(name = ifelse(name=="Oristano","Oristrano",name)) %>%
  mutate(name = ifelse(name=="Barletta-Andria-Trani","Barletta-Andria Trani",name))

dfita2 <- dfita2 %>%
  left_join(clean_prov) %>% 
  ungroup() %>% 
  mutate(percentage=(cases/pop)*100) %>%
  mutate(density=(cases/ext)*1000) %>%
  rename(absolute=cases)



#================================


#================================
#====== MODULE 2 - INSPECTION ====== 


# plot growth monitoring --------------------------------------------------------------------

growth <- data.frame(date=get_countryTS()$data,
                     growth=get_country_growth()$growth)

growth_xts <- xts::xts(growth[,-1], order.by=growth[,1])

growth_change <- data.frame(date=get_countryTS()$data,
                            growth=get_country_growth()$growth_change)

growth_change_xts <- xts::xts(growth_change[,-1], order.by=growth_change[,1])

hc <- highcharter::highchart(type = "stock") %>% 
  highcharter::hc_title(text = "% growth and growth change of total cases") %>%
  highcharter::hc_add_series(growth_xts, name="growth", color = "red", type = "spline") %>% 
  highcharter::hc_add_series(growth_change_xts, name="growth_change", color = "orange", type = "spline")


# tamponi graph -----------------------------------------------------------
tamp_data <- tibble(
  data=countryTS$data,
  tamponi=countryTS$tamponi,
  totale_casi=countryTS$totale_casi
) %>% 
  mutate(casi_giornalieri=totale_casi-lag(totale_casi)) %>%
  mutate(casi_giornalieri=ifelse(data==as.Date("2020-02-24"),totale_casi,casi_giornalieri)) %>% 
  mutate(tamponi_giornalieri=tamponi-lag(tamponi)) %>%
  mutate(tamponi_giornalieri=ifelse(data==as.Date("2020-02-24"),tamponi,tamponi_giornalieri)) %>%
  mutate(share_infected_discovered = casi_giornalieri/tamponi_giornalieri) %>%
  select(data,casi_giornalieri,tamponi_giornalieri,share_infected_discovered) %>%
  rename(daily_cases=casi_giornalieri,daily_tests=tamponi_giornalieri,date=data) %>%
  mutate(share_infected_discovered=round(share_infected_discovered,2))

tamp_data_1 <- tamp_data %>% select(1:3) %>%
  gather(key="key",value="value",-date)

#================================




#================================
#====== MODULE 3 - ANALYSIS ====== 

# Inital and final dates of samples
init_date <- min(countryTS$data)
fin_date <- max(countryTS$data)

# Total population sizes in 2020 winter
country_tot_pop <- 6.048e+07
region_tot_pop <- NULL


regNames <- names(regionTS)
provNames <- names(provTS)

# Time horizon of all graphs
days <- (1:50)


#================================
