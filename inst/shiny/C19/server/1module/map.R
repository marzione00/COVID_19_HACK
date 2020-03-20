# map ---------------------------------------------------------------------
  
  map <- "https://raw.githubusercontent.com/stefanocudini/leaflet-geojson-selector/master/examples/italy-regions.json" %>% 
    GET() %>% 
    content() %>% 
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  dfita1 <-  map$features %>% 
    map_df(function(x){
      as_data_frame(x$properties)
    })
  
  pc_data <- regionTS
  
  names(pc_data) <- tolower(names(pc_data))
  
  pc_df <- map_df(names(pc_data), function(x){
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
  

output$map_region <- output$map_region_modal <- highcharter::renderHighchart(
      if(input$map_value=="absolute") {
        highcharter::highchart(type = "map") %>% 
          highcharter::hc_chart(zoomType = "xy") %>%
          highcharter::hc_add_series_map(map = map, df = dfita1,
                                         joinBy = "id", value = input$map_value, name="absolute (total cases)") %>%
        highcharter::hc_colorAxis(
          stops = highcharter::color_stops(4,c("#FFE4B5","#FFA500","#FF4500","#cc0000")))
      } else if(input$map_value=="percentage") {
        highcharter::highchart(type = "map") %>% 
          highcharter::hc_chart(zoomType = "xy") %>%
          
          highcharter::hc_add_series_map(map = map, df = dfita1,
                                         joinBy = "id", value = input$map_value, name="percentage (cases/pop * 100)")
      } else {
        highcharter::highchart(type = "map") %>% 
          highcharter::hc_chart(zoomType = "xy") %>%
          
          highcharter::hc_add_series_map(map = map, df = dfita1,
                                         joinBy = "id", value = input$map_value, name="density (cases/km^2 * 1000)") %>%
          highcharter::hc_colorAxis(
            stops = highcharter::color_stops(4,c("#d8ebb5","#639a67","#2b580c","#003000")))
      }
  )

  

# map by province ---------------------------------------------------------

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

# integrating civil protection data
  prov_TS <- provTS
  
clean_prov <- map_df(names(prov_TS), function(x) {
  tail(prov_TS[[x]],1)$totale_casi
  data_frame(
    name=x,
    cases=tail(prov_TS[[x]],1)$totale_casi
  )
})

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
  
output$map_province <- output$map_province_modal <- highcharter::renderHighchart(
  if(input$map_value=="absolute") {
    highcharter::highchart(type = "map") %>% 
      highcharter::hc_chart(zoomType = "xy") %>%
      highcharter::hc_add_series_map(map = ita, df = dfita2, 
                                     joinBy = "hasc", value = input$map_value, name="absolute (total cases)") %>%
      highcharter::hc_colorAxis(
        stops = highcharter::color_stops(4,c("#FFE4B5","#FFA500","#FF4500","#cc0000")))
  } else if(input$map_value=="percentage") {
    highcharter::highchart(type = "map") %>% 
      highcharter::hc_chart(zoomType = "xy") %>%
      highcharter::hc_add_series_map(map = ita, df = dfita2, 
                                     joinBy = "hasc", value = input$map_value, name="percentage (cases/pop * 100)")
  } else {
    highcharter::highchart(type = "map") %>% 
      highcharter::hc_chart(zoomType = "xy") %>%
      
      highcharter::hc_add_series_map(map = ita, df = dfita2, 
                                     joinBy = "hasc", value = input$map_value, name="density (cases/km^2 * 1000)") %>%
      highcharter::hc_colorAxis(
        stops = highcharter::color_stops(4,c("#d8ebb5","#639a67","#2b580c","#003000")))
  }
  
  
)


# tabbox ------------------------------------------------------------------

  output$tabset1Selected <- renderText({
    input$tabset1
  })


# modal dialog ------------------------------------------------------------

observeEvent(input$show, {
  showModal(modalDialog(
    title = NULL, easyClose = TRUE, size = "l",
    
        tags$head(tags$style(HTML('
 /* tabBox background */

 .nav-tabs-custom > .nav-tabs > li.active {
     border-top-color: red !important;
 }
 
 .btn-default {
    background-color: #dd4b39 !important;
    color: white !important;
    border-color: #dd4b39 !important;
}
 
                                  '))),
        
        tabBox(
          width = 12,
          # height = "250px",
          title = NULL,
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          tabPanel("By Province",
                   shinycssloaders::withSpinner(
                   highcharter::highchartOutput('map_province_modal',
                                                width = "100%",
                                                height = "800px"
                   ),
                   color="#dd4b39"
                   )
          ),
          tabPanel("By Region", 
                   shinycssloaders::withSpinner(
                   highcharter::highchartOutput('map_region_modal',
                                                width = "100%",
                                                height = "800px"
                   ),
                   color="#dd4b39"
                   )
          )
        )
    
  ))
})
  