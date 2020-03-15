library(plotly)
library(shinydashboard)
library(shinydashboardPlus)
## ui.R ##
ui <- dashboardPagePlus(skin = "red", title = "Covid-19",
    
    dashboardHeader(title = img(src = "coronavirus_white.png")),
    
    dashboardSidebar(collapsed = T,
      sidebarMenuOutput("menu")
    ),
    
    dashboardBody(
      shinyjs::useShinyjs(),
      
      # css
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "shortcut icon",
                  href = "coronavirus_black.png")
      ),
      
      waiter::use_waiter(),
      waiter::waiter_show_on_load(html = waiter::spin_rotating_plane()), # will show on load
      

# tabs --------------------------------------------------------------------
      tabItems(

# tab 1 -------------------------------------------------------------------
        tabItem(tabName = "tab_1",
                valueBox("Coronavirus in Italy", 
                         paste("Most recent update:",
                               tail(get_countryTS()$data,1)),
                         icon=icon("history"),
                         color = "red", width = NULL),
                 fluidRow(
                   column(6,
                          
                      box(title="Number of cases by region", solidHeader = T,
                          highcharter::highchartOutput('map',
                                                       width = "100%", 
                                                       height = "530px"),
                    width = NULL, status = "danger"
                          )
                    
                   ),
                   column(6,
                          
                          
                          fluidRow(
                            valueBox(tail(get_countryTS()$totale_casi,1), "Total cases", icon = icon("notes-medical"),
                                     color = "red", width = 6),
                            valueBox(tail(get_countryTS()$terapia_intensiva,1), "Intensive care", icon = icon("procedures"),
                                     color = "orange", width = 6)
                          ),
                          fluidRow(
                            valueBox(tail(get_countryTS()$totale_ospedalizzati,1), "Hospitalised", icon = icon("hospital"),
                                     color = "yellow", width = 6),
                            valueBox(tail(get_countryTS()$isolamento_domiciliare,1), "Home isolation", icon = icon("home"),
                                     color = "maroon", width = 6)
                          ),
                          
                          flipBox(
                            id = 1,
                            main_img = "https://cdn1.vectorstock.com/i/1000x1000/68/55/data-analysis-round-vector-7826855.jpg",
                            header_img = "https://cdn.mos.cms.futurecdn.net/JtVH5Khvihib7dBDFY9ZDR.jpg",
                            front_title = "Covid-19 Hack",
                            back_title = "About us",
                            "We are a team of data science students from Università degli Studi di Milano. 
                            This project is aimed at tracking and modelling the Covid-19 spread in Italy.
                             The data is synchronised with the civil protection database. Each time the app is run it checks for
                            updates in the civil protection database",
                            back_content = tagList(
                              
                            )
                          )
                          
                          
                          
                          )
                 
                    )
                         
        ),
        

# tab 2 -------------------------------------------------------------------
        tabItem(tabName = "tab_2",
                         HTML("<h2>Data Inspection</h2>")
        ),
        

# tab 3 -------------------------------------------------------------------
        tabItem(tabName = "tab_3",
                         HTML("<h2>Test</h2>")
        ),


# tab 4 -------------------------------------------------------------------
        tabItem(tabName = "tab_4",
                         
                         HTML("<h2>Conclusions</h2>")
                
        )
      )
    )
  )

