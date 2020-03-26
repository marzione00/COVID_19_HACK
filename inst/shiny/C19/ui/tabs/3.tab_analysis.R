shinydashboard::tabItem(
  tabName = "tab_3",
  
  shinydashboard::valueBox(
    "Data Analysis",
    "Analysis",
    icon = icon("chart-bar"),
    color = "red",
    width = NULL
  ),
  
  
  fluidRow(
    shinydashboard::box(width = 12,
                        status= "danger",
                        solidHeader = FALSE,
                        title = "Territory selection",
                        shiny::uiOutput("terrInput"),
                        shiny::htmlOutput("selected_info")
    )
  ),
  
  fluidRow(
    shiny::textOutput("log")
  ),
  
  shinydashboard::valueBox(
    "Logistic model",
    "Curve fitting on confirmed cases of infection's time series",
    icon = icon("analytics"),
    color = "navy",
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
        shiny::uiOutput("fitInput")
      )
    ),
    column(
      8,
      shinydashboard::box(
        color = "red",
        status = "danger",
        title = "Plot",
        plotly::plotlyOutput("coolplot1") %>% shinycssloaders::withSpinner(color =
                                                                                   "#dd4b39"),
        width = 12
      )
    )
  ),
  
  # ----- summary row
  fluidRow(
    column(
      5,
      
      shinydashboard::tabBox(
        width = 12,
        title = "Technical data",
        id = "tech_tab",
        shiny::tabPanel(
          "Fitting output",
          status = "danger",
          verbatimTextOutput("fit_smry")
        ),
        
        shiny::tabPanel("Tests",
                        status = "danger",
                        verbatimTextOutput("resid_smry"))
      )
    ),
    column(
      7,
      shinydashboard::box(
        width = 12,
        status = "danger",
        title = "Residuals",
        plotly::plotlyOutput("plot_residual") %>% shinycssloaders::withSpinner(color =
                                                                                 "#dd4b39")
        
      )
      
    )
  ),
  
  hr(),
  
  
  # --------  ARIMA ------
  
  
  
  shinydashboard::valueBox(
    "ARIMA model",
    "ARIMA(p,i,q) on confirmed cases of infection's time series",
    icon = icon("analytics"),
    color = "navy",
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
        shiny::uiOutput("arimaDatesInput"),
        shiny::uiOutput("arimaLagsInput")
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
        
        plotly::plotlyOutput("arima_coolplot1") %>% shinycssloaders::withSpinner(color =
                                                                                  "#dd4b39"),
        width = 12
      )
    )
  ),
  
  
  fluidRow(
    column(
      3,
      shinydashboard::box(
        color = "red",
        status = "danger",
        title = "Autocorrelations",
        plotly::plotlyOutput("arima_coolplot2") %>% shinycssloaders::withSpinner(color =
                                                                                   "#dd4b39"),
        width = 12
      )
    ),
    column(
      3,
      shinydashboard::box(
        color = "red",
        status = "danger",
        title = "Partial autocorrelations",
        plotly::plotlyOutput("arima_coolplot3") %>% shinycssloaders::withSpinner(color =
                                                                                    "#dd4b39"),
        width = 12
      )
    ),
    column(
      6,
      shinydashboard::box(
        color = "red",
        status = "danger",
        title = "Arima Check residuals",
        shiny::plotOutput("arima_coolplot4") %>% shinycssloaders::withSpinner(color =
                                                                                "#dd4b39"),
        width = 12
      )
    )
  ),
  
  
  fluidRow(
    column(
      6,
      shinydashboard::box(
        color = "red",
        status = "danger",
        title = "Arima Data Output",
        shiny::verbatimTextOutput("arima_shell_output") %>% shinycssloaders::withSpinner(color =
                                                                                           "#dd4b39"),
        width = 12
      )
    )
  )
)