tabItem(tabName = "tab_3",
        
        valueBox("Data Analysis", "Analysis",
                 icon=icon("chart-bar"),
                 color = "red", width = NULL),
        
       
        fluidRow(
          column(12,
                 
                 
                 tabBox( width = 12,
                         title = "Analisys",
                         id = "tabset1",
                         height = NULL,
                         
                         #---------- TAB COUNTRY -----------
                         tabPanel(title = "Country", shiny::uiOutput("countryPanel")),
                         
                         
                         
                         #---------- TAB REGION -----------
                         tabPanel(title = "Region",
                                  
                                  # ---- selector + plot row
                                  fluidRow(
                                    
                                    column(4,
                                           shinydashboard::box(
                                             width = 12,
                                             status = "danger", 
                                             solidHeader = TRUE,
                                             title = "Input", 
                                             shiny::uiOutput("regionInput")
                                           )
                                    ),
                                    column(8,
                                           shinydashboard::box(
                                             color = "red", 
                                             status ="danger",
                                             title = "Charts", 
                                             plotly::plotlyOutput("coolplot_region"),
                                             width = 12),
                                           )
                                    ),
                                  
                                  # ----- summary row
                                  fluidRow(
                                    
                                    shinydashboard::tabBox(
                                      width=6,
                                      title = "Technical data", id = "tech_tab",
                                      shiny::tabPanel("Fitting output", shiny::verbatimTextOutput("fit_smry")),
                                      shiny::tabPanel("Chi-squared test", shiny::verbatimTextOutput("chisq_smry"))
                                    )
                                    
                                    
                                    
                                  )
                                  
                                  
                         ),
                         
                         #---------- TAB PROVINCE -----------
                         
                         shiny::tabPanel(title = "Province", shiny::uiOutput("provPanel"))
                 )
                 
          )
          
        )
)

