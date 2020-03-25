shinydashboard::tabItem(tabName = "tab_2",
                        
                        shinydashboard::valueBox("Data Inspection", "Descriptive analysis",
                                                 icon=icon("search"),
                                                 color = "red", width = NULL),
                        
                        
                        
                        shinydashboard::valueBox("Introduction", "Total cases, total deaths, total recovered, total hospitalised",
                                                 icon=icon("search"),
                                                 color = "red", width = NULL),
                        fluidRow(
                                # column(4,
                                #        
                                #        box(title="Inputs", solidHeader = T,
                                #            width = NULL,
                                #            status = "danger"
                                #            
                                #        )),
                                column(12,
                                       
                                       shinydashboard::box(title="General info", solidHeader = T,
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
                                                           
                                                           shinydashboard::tabBox(width = 12,
                                                                                  title = NULL,
                                                                                  tabPanel("Plot",
                                                                                           highcharter::highchartOutput("general_infos_plot")%>%shinycssloaders::withSpinner( color="#dd4b39")
                                                                                  ),
                                                                                  
                                                                                  tabPanel("Raw data",
                                                                                           shiny::uiOutput("rawData_input"),
                                                                                           DT::dataTableOutput("rawData_table")
                                                                                  )
                                                           )
                                       )
                                       
                                       
                                )
                        ),
                        
                        fluidRow(
                                column(6,
                                       
                                       
                                       
                                       shinydashboard::box(title="Intensive care", solidHeader = T,
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
                                                           
                                                           
                                                           shinydashboard::tabBox(width = 12,
                                                                                  title = NULL,
                                                                                  # The id lets us use input$tabset1 on the server to find the current tab
                                                                                  id = "tabset2",
                                                                                  tabPanel("% occupation/capacity",
                                                                                           plotly::plotlyOutput("intensivecare_cap_perc",
                                                                                                                width = "100%",
                                                                                                                height = "415px"
                                                                                           ) %>% 
                                                                                                   shinycssloaders::withSpinner( color="#dd4b39")
                                                                                  ),
                                                                                  tabPanel("capacity vs. occupation ",
                                                                                           plotly::plotlyOutput("intensivecare_cap",
                                                                                                                width = "100%",
                                                                                                                height = "415px"
                                                                                           ) %>%
                                                                                                   shinycssloaders::withSpinner( color="#dd4b39")
                                                                                  )
                                                           ),
                                                           
                                                           
                                                           height=NULL, status="danger")
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                ),
                                column(6,
                                       
                                       
                                       
                                       shinydashboard::box(title="Growth Monitoring", solidHeader = T,
                                                           width = NULL,
                                                           color = "red",
                                                           
                                                           
                                                           shinydashboard::box(    
                                                                   title=NULL,
                                                                   solidHeader = T,
                                                                   width = NULL,
                                                                   color = "red",
                                                                   
                                                                   
                                                                   footer = fluidRow(
                                                                           column(
                                                                                   width = 6,
                                                                                   shinydashboardPlus::descriptionBlock(
                                                                                           number = paste0(tail(country_growth$growth,1),"%"),
                                                                                           number_color = ifelse(tail(country_growth$growth,1)>0,"red","green"), 
                                                                                           number_icon = ifelse(tail(country_growth$growth,1)>0,"fa fa-caret-up","fa fa-caret-down"),
                                                                                           header = "CASES GROWTH", 
                                                                                           text = NULL, 
                                                                                           right_border = TRUE,
                                                                                           margin_bottom = FALSE
                                                                                   )
                                                                           ),
                                                                           column(
                                                                                   width = 6,
                                                                                   shinydashboardPlus::descriptionBlock(
                                                                                           number = paste0(tail(country_growth$growth_change,1),"%"),
                                                                                           number_color = ifelse(tail(country_growth$growth_change,1)>0,"red","green"), 
                                                                                           number_icon = ifelse(tail(country_growth$growth_change,1)>0,"fa fa-caret-up","fa fa-caret-down"),
                                                                                           header = HTML("CASES GROWTH &Delta;"), 
                                                                                           text = NULL, 
                                                                                           right_border = FALSE,
                                                                                           margin_bottom = FALSE
                                                                                   )
                                                                           )
                                                                   )
                                                           ),
                                                           
                                                           
                                                           highcharter::highchartOutput("plot_test", width = "100%",
                                                                                        height = "400px"),
                                                           height=NULL, status="danger")
                                       
                                       
                                       
                                ),
                                
                                column(12,
                                       shinydashboard::box(title="Tests Tracking", status="danger", 
                                           solidHeader = TRUE, 
                                           highcharter::highchartOutput("tamp_plot")
                                       )
                                )
                                
                        )
)