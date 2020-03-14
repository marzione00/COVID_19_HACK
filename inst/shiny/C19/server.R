library(tidyverse)
library(plotly)
library(mapIT)
library(devtools)
library(shinydashboard)

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
  install_github("quantide/mapIT")
  
  cases_region <- data.frame(
    region = c("Abruzzo","Basilicata","Calabria","Campania",
               "Emilia-Romagna","Friuli-Venezia Giulia","Lazio",
               "Liguria","Lombardia","Marche","Molise","Piemonte",
               "Puglia","Sardegna","Sicilia","Toscana",
               "Trentino-Alto Adige","Umbria","Valle d\'Aosta","Veneto")
  )
  
  cases_region <- cases_region %>%
    left_join(data)
  
  gp <- list(low="#fff0f0", high="red3")
  
  
  output$map <- renderPlot(
    mapIT(cases, region, data=cases_region,
          guide.label="Number of\ncases",  graphPar=gp) +
      theme_void() +
      coord_fixed()
  )
  
}
