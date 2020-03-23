shinydashboard::tabItem(
  tabName = "tab_3",
  
  shinydashboard::valueBox(
    "Data Analysis",
    "Analysis",
    icon = icon("chart-bar"),
    color = "red",
    width = NULL
  ),
  
  
  fluidRow(column(
    12,
    shinydashboard::tabBox(
      width = 12,
      title = "Analisys",
      id = "tabset1",
      height = NULL,
      
      #---------- TAB COUNTRY -----------
      tabPanel(
        title = h4("Country"),
        fluidRow(
          column(
            4,
            shinydashboard::box(
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              title = "Input",
              shiny::uiOutput("countryInput")
            )
          ),
          column(
            8,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Charts",
              plotly::plotlyOutput("coolplot_country"),
              width = 12
            )
          )
        ),
        
        # ----- summary row
        fluidRow(
          shinydashboard::tabBox(
            width = 6,
            title = "Technical data",
            id = "tech_tab",
            shiny::tabPanel("Fitting output",
                            status = "danger",
                            verbatimTextOutput("fit_smry_country")
            ),
            
            shiny::tabPanel("Tests",
                            status = "danger",
                            verbatimTextOutput("resid_smry_country")
            )
          )
        )
      ),
      
      
      #---------- TAB REGION -----------
      tabPanel(
        title = h4("Region"),
        
        # ---- selector + plot row
        fluidRow(
          column(
            4,
            shinydashboard::box(
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              title = "Input",
              shiny::uiOutput("regionInput")
            )
          ),
          column(
            8,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Charts",
              plotly::plotlyOutput("coolplot_region")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            )
          )
        ),
        
        # ----- summary row
        fluidRow(
          column(5,
                 
                 shinydashboard::tabBox(
                   width = 12,
                   title = "Technical data",
                   id = "tech_tab",
                   shiny::tabPanel("Fitting output",
                                   status = "danger",
                                   verbatimTextOutput("fit_smry_region")),
                   
                   shiny::tabPanel("Tests",
                                   status = "danger",
                                   verbatimTextOutput("resid_smry_region"))
                 )
          ),
          column(7,
                 shinydashboard::box(width = 12,
                                       status = "danger",
                                       title = "Residuals",
                                     plotly::plotlyOutput("Plot_residual")%>% shinycssloaders::withSpinner( color="#dd4b39")
                                     
                 )
                 
          )
        ),
        
        hr(),
        
        
        # --------  ARIMA ------
        shinydashboard::valueBox(
          "ARIMA model",
          "Autoregressive integrated moving average",
          icon = icon("analytics"),
          color = "red",
          width = NULL
        ),
        
        
        fluidRow(
          column(
            4,
            shinydashboard::box(
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              title = "Input",
              shiny::uiOutput("regionInput_TS") 
            )
          ),
          column(
            8,
            
            shinydashboard::box(
              color = "red",
              status = "danger",
              
              title = "ARIMA forecast",
              h3(shiny::htmlOutput("parameters_sugg")),
              hr(),
              
              plotly::plotlyOutput("Arima_coolplot")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            ))
        ),
        
        
        fluidRow(
          column(
            3,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Autocorrelations",
              plotly::plotlyOutput("Arima_coolplot0")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            )),
          column(
            3,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Partial autocorrelations",
              plotly::plotlyOutput("Arima_coolplot00")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            )),
          column(
            6,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Arima Check residuals",
              shiny::plotOutput("Arima_coolplot2")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            ))
        ),
        
        
        fluidRow(
          
          column(
            6,
            shinydashboard::box(
              color = "red",
              status = "danger",
              title = "Arima Data Output",
              shiny::verbatimTextOutput("Arima_shell_output")%>% shinycssloaders::withSpinner( color="#dd4b39"),
              width = 12
            )
          )
        )
        
      ),
      
      
      
      #---------- TAB PROVINCE -----------
      
      shiny::tabPanel(title = h4("Province"), shiny::uiOutput("provPanel"))
      
    )
  )
  )
)
