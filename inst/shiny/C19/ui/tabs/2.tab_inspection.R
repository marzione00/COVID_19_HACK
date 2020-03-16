tabItem(tabName = "tab_2",
        HTML("<h2>Data Inspection</h2>"),
        
        valueBox("Data Inspection", "Descriptive analysis",
                 icon=icon("search"),
                 color = "red", width = NULL),
        
        
        #  fluidRow(
        #    column(12,
        #           
        #           box(title="Regional intensive care capacity ", solidHeader = T,
        #               highcharter::highchartOutput('map',
        #                                            width = "100%", 
        #                                            height = "530px"),
        #               width = NULL, status = "danger"
        #           )
        #           
        #    )
        #    
        #    )
        
     
        
        fluidRow(
          column(6,
                 
                 box(title="Intensive care percentage occupation/capacity", solidHeader = T,
                    plotlyOutput("intensivecare_cap_perc"), 
                     width = NULL, status = "danger"
                 )
                 
          ),
          column(6,
                 
                 box(title="Intensive care capacity vs. occupation", solidHeader = T,
                     plotlyOutput("intensivecare_cap"), 
                     width = NULL, status = "danger"
                 )
                 
          )
        )
)