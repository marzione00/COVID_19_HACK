tabItem(tabName = "tab_2",
        #HTML("<h2>Data Inspection</h2>"),
        
        valueBox("Data Inspection", "Descriptive analysis",
                 icon=icon("search"),
                 color = "red", width = NULL),
        
     
        
        fluidRow(
          column(6,
                 
                 box(title="Intensive care percentage occupation/capacity", solidHeader = T,
                    
                     
                     shinycssloaders::withSpinner(
                             plotlyOutput("intensivecare_cap_perc"),
                             color="#dd4b39"
                     ), 
                     width = NULL, status = "danger"
                 )
                 
          ),
          column(6,
                 
                 box(title="Intensive care capacity vs. occupation", solidHeader = T,
                     shinycssloaders::withSpinner(
                             plotlyOutput("intensivecare_cap"), 
                             color="#dd4b39"
                     ), 
                     width = NULL, status = "danger"
                 )
                 
          )
        )
)