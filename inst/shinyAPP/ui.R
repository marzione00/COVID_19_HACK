ui<-fluidPage(titlePanel("COVID19 Cases"),#preparation of the panels for shiny app

              sidebarLayout(
                sidebarPanel(selectInput("Plot_input", "Fit Curve",choices = c("Total_cases", "Total_cases_Lombardy", "Total_cases_Milan","Total_cases_Lodi","Total_cases_Bergamo")),
                             selectInput("Distribution", "Data set",choices = c("cases", "cases_Lombardy", "cases_Milan","cases_Lodi","cases_Bergamo")),#                             selectInput("Fit_output", "Fit output",choices = c("Total", "Lombardy", "Milan","Lodi","Bergamo")),
                             uiOutput("filter_degree")

                ),





                mainPanel(plotOutput("coolplot"), #plots that has to be loaded on the app
                          verbatimTextOutput("coolplot5"),
                ))

)
