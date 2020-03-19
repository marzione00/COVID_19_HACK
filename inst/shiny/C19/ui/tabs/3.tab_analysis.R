tabItem(
  tabName = "tab_3",
  
  valueBox(
    "Data Analysis",
    "Analysis",
    icon = icon("chart-bar"),
    color = "red",
    width = NULL
  ),
  
  
  fluidRow(column(
    12,
    tabBox(
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
        )
      ),
      
      
      #---------- TAB PROVINCE -----------
      
      shiny::tabPanel(title = h4("Province"), shiny::uiOutput("provPanel"))
      
    )
  )
  )
)
