# Inital and final dates of samples
init_date <- min(countryTS$data)
fin_date <- max(countryTS$data)

# Total population sizes in 2020 winter
country_tot_pop <- 6.048e+07
region_tot_pop <- NULL


regNames <- names(regionTS)
provNames <- names(provTS)

# Time horizon of all graphs
days <- (1:50)

output$regionPanel <- shiny::renderUI({
  shinydashboard::dashboardBody(
    box(title = "Input", 
        shiny::selectInput(inputId = "region", label = "Choose one region",
                           choices = regNames),
        shiny::sliderInput(inputId = "fitInterval", label = "Choose fitting interval",
                           min = init_date, max = fin_date, timeFormat = "%d %b",
                           step = 1, value = c(init_date, fin_date)),
        shiny::checkboxGroupInput(inputId = "plot_type", label = "Plot type",
                                  choices = list("Cumulative cases" = 1, "New cases" = 2),
                                  selected = 1)),
    box(title = "Charts", plotly::plotlyOutput("coolplot_region"))
  )
})