server <- function(input, output, session) {

  ## DATI ##
  # NB!! Da inserire dentro un eventReactive di un BUTTON "refresh" #
  countryTS <- get_countryTS()
  regionTS <- get_regionTS()
  provTS <- get_provTS

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



  # conf<-nlstools::confint2(level = 0.95,model)

  #Conf_UP<-conf[1,1]/(conf[2,1] * exp(conf[3,1] * days_dat) +1 )
  #Conf_DOWN<-conf[1,2]/(conf[2,2] * exp(conf[3,2] * days_dat) +1 )

  #data_u<-data.frame(days_dat,Conf_UP)
  #data_u<-data.frame(days_dat,Conf_DOWN)

  ### ------ COUNTRY ----- ###
  ## COUNTRY UI ##
  output$countryPanel <- shiny::renderUI({
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "Plot_input_country", label = "Fit Curve",
                           choices = c("Italy")),
        shiny::uiOutput("filter_degree_country")
      ),
      shiny::mainPanel(
        shiny::plotOutput("coolplot_country"), #plots that has to be loaded on the app
        shiny::verbatimTextOutput("coolplot5_country"),
        shiny::verbatimTextOutput("coolplot6_country")
      )
    )
  })

  ## COUNTRY plot ##
  output$coolplot_country <- shiny::renderPlot({

     # CODICE DA SCRIVERE - FABIO #

  })



  ### ------ REGION ----- ###
  ## REGION UI ##
  output$regionPanel <- shiny::renderUI({
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "region", label = "Choose one region",
                           choices = regNames),
        shiny::sliderInput(inputId = "fitInterval", label = "Choose fitting interval",
                           min = init_date, max = fin_date, timeFormat = "%d %b",
                           step = 1, value = c(init_date, fin_date)),
        shiny::uiOutput("filter_degree_region")
      ),
      shiny::mainPanel(
        shiny::plotOutput("coolplot_region"), #plots that has to be loaded on the app
        shiny::verbatimTextOutput("coolplot5_region"),
        shiny::verbatimTextOutput("coolplot6_region")
      )
    )
  })

  ## REGION reactive values ##
  reac_region <- shiny::reactiveValues()


  ## REGION plot (currently date against total cases) ##
  output$coolplot_region <- shiny::renderPlot({

    # Data trim and curve fitting
    sample_date <- regionTS[[input$region]]$data_seriale
    sample_cases <- regionTS[[input$region]]$totale_casi
    logic_interval <- regionTS[[input$region]]$data >= input$fitInterval[1] &
                      regionTS[[input$region]]$data <= input$fitInterval[2]
    sample_date_trim <- sample_date[logic_interval]
    sample_cases_trim <- sample_cases[logic_interval]
    sample_date_rem <- sample_date[!logic_interval]
    sample_cases_rem <- sample_cases[!logic_interval]

    fit_data <- exe_fit(sample_cases = sample_cases_trim,
                        sample_date = sample_date_trim,
                        days = days)

    reac_region$model <- fit_data$out_fit$model
    reac_region$chisq <- fit_data$out_chisq$p.value
    reac_region$vals <- fit_data$out_fit$vals

    # Conversion to real date and creation of fitted points
    points_trim <- data.frame("sample_date_trim" = regionTS[[input$region]]$data[logic_interval],
                              sample_cases_trim)
    points_rem <- data.frame("sample_date_rem" = regionTS[[input$region]]$data[!logic_interval],
                             sample_cases_rem)
    fittedPoints <- fit_data$fittedPoints
    seq_dates <- seq(from = init_date, by = 1, length.out = length(days))
    fittedPoints$days <- seq_dates

    # Plot of fitted curve and points
    ggplot2::ggplot(fittedPoints, ggplot2::aes_string("days","yFitted")) +
    ggplot2::geom_line(color = "darkblue", size = 1)+
    ggplot2::geom_point(data = points_rem, ggplot2::aes_string("sample_date_rem","sample_cases_rem"), size = 1, color="red")+
    ggplot2::geom_point(data = points_trim, ggplot2::aes_string("sample_date_trim","sample_cases_trim"), size = 1, color="green")+
    #geom_line(data=data_u,aes_string("days_dat","Conf_UP"),linetype = "dashed",size=1.05) +
    #geom_line(data=data_u,aes_string("days_dat","Conf_DOWN"),linetype = "dashed",size=1.05) +
    ggplot2::theme_dark() +
    ggplot2::theme(legend.position="top", panel.grid.minor = ggplot2::element_line(),text = ggplot2::element_text(size=14)) +
    #ggplot2::scale_y_continuous(breaks=seq(0, 11/10*fit_data$out_fit$vals$k , 1/10*fit_data$out_fit$vals$k))+
    ggplot2::labs(title="COVID19")
  })

  output$coolplot5_region <- shiny::renderPrint(
    summary(reac_region$model)
    )

  output$coolplot6_region <- shiny::renderPrint(
    cat("Pvalue (chi-square) =",reac_region$chisq)    
  )

  # output$coolplot_region <- renderText({
  #   str(input$region)
  # })




  ### ----- PROVINCE ----- ###
  ## PROVINCE UI ##
  output$provincePanel <- shiny::renderUI({
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(inputId = "Plot_input_province", label = "Fit Curve",
                           choices = provNames),
        shiny::selectInput(inputId = "Distribution_province", label = "Data set",
                           choices = provNames),
        shiny::uiOutput("filter_degree_province")
      ),
      shiny::mainPanel(
        shiny::plotOutput("coolplot_province"), #plots that has to be loaded on the app
        shiny::verbatimTextOutput("coolplot5_province")
      )
    )
  })

  ## PROVINCE plot ##
  output$coolplot_province <- shiny::renderPlot({

    # CODICE DA SCRIVERE - FABIO #

  })
}
