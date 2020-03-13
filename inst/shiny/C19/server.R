library(tidyverse)
library(plotly)
library(mapIT)
library(devtools)

## server.R ##

server <- function(input, output, session) {
  
  shinyjs::runjs('
        var el2 = document.querySelector(".skin-red");
        el2.className = "skin-red sidebar-mini";
        ')
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  # data ---------------------------------------------------------------
  # get_countryTS() %>% View()
  # 
  # get_provTS() %>% View()
  # 
  # get_regionTS() %>% View()

  # section 2 ---------------------------------------------------------------

  waiter::waiter_hide()
  
  output$menu <- shinydashboard::renderMenu({
    
    shinydashboard::sidebarMenu(id="tabs",
                                shinydashboard::menuItem("Home", tabName = "tab_1", icon=icon("home")),
                                shinydashboard::menuItem("Inspection", tabName = "tab_2", icon=icon("search")),
                                shinydashboard::menuItem("Analysis", tabName = "tab_3", icon=icon("chart-line")),
                                shinydashboard::menuItem("Conclusion", tabName = "tab_4", icon=icon("calendar-check"))
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
  
  
  # map attempt -------------------------------------------------------------
  
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
      # theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
      coord_fixed()
    
  )
  

# animations --------------------------------------------------------------

  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_1', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_2', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_3', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_4', 'fadeIn')
  })
  
}
