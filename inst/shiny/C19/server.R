library(tidyverse)
library(plotly)
library(devtools)
library(shinydashboard)
library(httr)

## server.R ##

server <- function(input, output, session) {
  
  shinyjs::runjs('
        var el2 = document.querySelector(".skin-red");
        el2.className = "skin-red sidebar-mini";
        ')
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")

  waiter::waiter_hide()

# sidebard ----------------------------------------------------------------
  output$menu <- renderMenu({
    sidebarMenu(id="tabs",
                menuItem("Home", tabName = "tab_1", icon=icon("home")),
                menuItem("Inspection", tabName = "tab_2", icon=icon("search")),
                menuItem("Analysis", tabName = "tab_3", icon=icon("chart-line")),
                menuItem("Conclusion", tabName = "tab_4", icon=icon("calendar-check"))
    )
  })
  
# map section -------------------------------------------------------------
  regions <- get_regionTS()
  
  data <- map_df(names(regions), function(x) {
    casi <- regions[[x]] %>% select(totale_casi) %>% tail(1) %>% pull()
    
    data_frame(region=x,cases=casi)
  })
  
  data <- data %>%
    mutate(region=ifelse(region%in%c("P.A. Trento", "P.A. Bolzano"), "Trentino-Alto Adige", region)) %>%
    group_by(region) %>%
    summarise(cases = sum(cases)) %>%
    ungroup() %>%
    mutate(region=ifelse(region=="Emilia Romagna", "Emilia-Romagna", region)) %>% 
    mutate(region=ifelse(region=="Friuli Venezia Giulia", "Friuli-Venezia Giulia", region))
  

# map ---------------------------------------------------------------------
  
  map <- "https://raw.githubusercontent.com/stefanocudini/leaflet-geojson-selector/master/examples/italy-regions.json" %>% 
    GET() %>% 
    content() %>% 
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  dfita1 <-  map$features %>% 
    map_df(function(x){
      as_data_frame(x$properties)
    })
  
  pc_data <- get_regionTS()
  
  names(pc_data) <- tolower(names(pc_data))
  
  pc_df <- map_df(names(pc_data), function(x){
    casi <- tail(pc_data[[x]],1)$totale_casi
    data_frame(name=x,cases=casi)
  })
  
  pc_df$name
  
  pc_df <- pc_df %>%
    filter(!name%in%c("friuli v. g. ")) %>%
    mutate(name=ifelse(name%in%c("trento","bolzano","p.a. trento","p.a. bolzano"),
                       "trentino-alto adige/sudtirol",name)) %>%
    group_by(name) %>%
    summarise(cases=sum(cases)) %>%
    mutate(name=ifelse(name=="emilia romagna","emilia-romagna",name))
  
  
  
  a <- pc_df$name
  
  b <- dfita1$name
  
  setdiff(b,a)
  
  dfita1 <- dfita1 %>%
    left_join(pc_df)
  
  output$map <- highcharter::renderHighchart(
    highcharter::highchart(type = "map") %>% 
      highcharter::hc_add_series_map(map = map, df = dfita1,
                                     joinBy = "id", value = "cases", name="total cases") %>%
      highcharter::hc_colorAxis(minColor = "#ffcc00", maxColor = "#cc0000")
  )
  
  
}

