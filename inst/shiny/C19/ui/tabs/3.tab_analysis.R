tabItem(tabName = "tab_3",
        shiny::navbarPage(title = "Area", inverse = TRUE, fluid = FALSE,
                          shiny::tabPanel(title = "Country", shiny::uiOutput("countryPanel")),
                          shiny::tabPanel(title = "Region", 
                             shinydashboard::dashboardBody(
                               waiter::use_waiter(),
                               shiny::fluidRow(
                                 shiny::column(width = 4,
                                               shinydashboard::box(title = "Input", 
                                                                   shiny::uiOutput("regionInput"),
                                                                   width = NULL),
                                               shinydashboard::box("Qualcosa", "Qua metteremo qualcosa", width = NULL)
                                 ),
                                 shiny::column(width = 8,
                                               shinydashboard::box(title = "Charts", plotly::plotlyOutput("coolplot_region"),
                                                                   width = NULL),
                                               shinydashboard::tabBox(
                                                 title = "Technical data", id = "tech_tab", width = NULL,
                                                 shiny::tabPanel("Fitting output", shiny::verbatimTextOutput("fit_smry")),
                                                 shiny::tabPanel("Chi-squared test", shiny::verbatimTextOutput("chisq_smry"))
                                               )
                                 )
                               )
                               
                             )),
             shiny::tabPanel(title = "Province", shiny::uiOutput("provPanel"))
        )
)