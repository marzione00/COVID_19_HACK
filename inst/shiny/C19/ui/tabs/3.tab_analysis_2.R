shinydashboard::tabItem(
  tabName = "tab_3_2",
  
  shinydashboard::valueBox(
    "Entropy",
    "Analysis",
    icon = icon("chart-bar"),
    color = "red",
    width = NULL
  ),
  
  shinydashboard::box(title = "Entropy", status = "danger", solidHeader = TRUE,
                      width = NULL,
                      highcharter::highchartOutput("entropy_plot"))%>%shinycssloaders::withSpinner( color="#dd4b39"),
  
)
