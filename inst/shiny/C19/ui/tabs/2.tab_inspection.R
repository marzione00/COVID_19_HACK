tabItem(tabName = "tab_2",
        
        
        fluidRow(
          column(6,
                 
                 valueBox("Data Inspection", "Descriptive analysis",
                          icon=icon("search"),
                          color = "red", width = NULL),
                 
                 
                 
                 
                 
                 
                 
                 box(title="Intensive care", solidHeader = T,
                     width = NULL,
                     color = "red",
                     
                     tags$head(tags$style(HTML('
                         /* tabBox background */

                         .nav-tabs-custom > .nav-tabs > li.active {
                             border-top-color: red !important;
                         }
                         
                         .btn-default {
                            background-color: #dd4b39 !important;
                            color: white !important;
                            border-color: #dd4b39 !important;
                         }
                                               '))),
                     
                     
                     tabBox(width = 12,
                            title = NULL,
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset2",
                            tabPanel("% occupation/capacity",
                                     plotlyOutput("intensivecare_cap_perc")%>% shinycssloaders::withSpinner( color="#dd4b39")
                            ),
                            tabPanel("capacity vs. occupation ",
                                     plotlyOutput("intensivecare_cap")%>% shinycssloaders::withSpinner( color="#dd4b39")
                            )
                     ),
                     
                     
                     height=NULL, status="danger")
                 
                 
                 
                 
                 
                 
                 
                 
          ),
          column(6,
                 "output"
                 
                 
                 
          )
        )
)