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
        shiny::checkboxGroupInput(inputId = "plot_type", label = "Plot type",
                                  choices = list("Cumulative cases" = 1, "New cases" = 2),
                                  selected = 1)
      ),
      shiny::mainPanel(
        plotly::plotlyOutput("coolplot_region"), #plots that has to be loaded on the app

          # plot summary in html form - TODO
        #htmlOutput("fit_smryhtml"),

        shiny::verbatimTextOutput("fit_smry"),
        shiny::verbatimTextOutput("chisq_smry")
      )
    )
  })

  ## REGION reactive values ##
  reac_region <- shiny::reactiveValues()


  ## REGION plot (currently date against total cases) ##
  output$coolplot_region <- plotly::renderPlotly({

    # Data trim and curve fitting
    logic_interval <- regionTS[[input$region]]$data >= input$fitInterval[1] &
      regionTS[[input$region]]$data <= input$fitInterval[2]

    sample_date <- regionTS[[input$region]]$data_seriale
    sample_cases <- regionTS[[input$region]]$totale_casi
    sample_diff <-  c(NA,diff(sample_cases))

    sample_date_trim <- sample_date[logic_interval]
    sample_cases_trim <- sample_cases[logic_interval]
    sample_diff_trim <- sample_diff[logic_interval]

    sample_date_rem <- sample_date[!logic_interval]
    sample_cases_rem <- sample_cases[!logic_interval]
    sample_diff_rem <- sample_diff[!logic_interval]

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
    points_diff_trim <- data.frame("sample_date_trim" = regionTS[[input$region]]$data[logic_interval],
                                   sample_diff_trim)
    points_diff_rem <- data.frame("sample_date_rem" = regionTS[[input$region]]$data[!logic_interval],
                                  sample_diff_rem)

    fittedPoints <- fit_data$fittedPoints
    fittedPoints_der <- fit_data$fittedPoints_der
    seq_dates <- seq(from = init_date, by = 1, length.out = length(days))
    fittedPoints$days <- seq_dates
    fittedPoints_der$days <- seq_dates


    fig = plotly::plot_ly( name = "Cases", type= "scatter")

    # funtions for the two different plots
    plot1  = function(fig)
    {
      fig <- fig %>% plotly::add_trace(data = fittedPoints, x = ~days, y = ~yFitted, line = list(color ='rgb(0,0,139)',width=4), mode='lines', name = "Fitted logistic curve”" )

      fig <- fig %>% plotly::add_trace(data =  points_rem, x =~sample_date_rem, y =~sample_cases_rem ,marker = list(color = "red"), mode = 'markers', name = "Total cases (fitting)")
      fig <- fig %>% plotly::add_trace(data = points_trim, x =~sample_date_trim, y =~sample_cases_trim ,marker = list(color = "green"), mode = 'markers', name = "Total cases (excluded)")
      return(fig)
    }

    plot2 = function (fig)
    {
      fig <- fig %>% plotly::add_trace(data = fittedPoints_der, x = ~days, y = ~yFitted_der, line = list(color ='rgb(255,117,20)',width=4), mode='lines', name= "Fitted logistic distribution")

      fig <- fig %>% plotly::add_bars(data =  points_diff_rem, x =~sample_date_rem, y =~sample_diff_rem, marker = list(color = "red"), name = "New cases (fitting)")
      fig <- fig %>% plotly::add_bars(data =  points_diff_trim, x =~sample_date_trim, y =~sample_diff_trim, marker = list(color = "green"), name = "New cases (excluded)")

      return(fig)
    }


    #Plot based on the checkbox

    if( 1 %in% input$plot_type && !(2 %in% input$plot_type ) )
    {
      fig = plot1(fig)
    }

    else if( 2 %in% input$plot_type && !(1 %in% input$plot_type ) )
    {
      fig = plot2(fig)
    }
    else if( 1 %in% input$plot_type && (2 %in% input$plot_type ) )
    {
      fig = plot1(fig)
      fig = plot2(fig)

    }

    #plot
    fig


  })

  # OUTPUT SUMMARY HTML --- TODO
 # output$fit_smryhtml <- shiny::renderText({
#
 #   #text = HTML(paste0("<b>","Summary","</b><br>",  summary(reac_region$model)))
 #   text = HTML(paste0(as.character(summary(reac_region$model))))
 #   text
#
 # })

  output$fit_smry <- shiny::renderPrint(
    summary(reac_region$model)

  )

  output$chisq_smry <- shiny::renderPrint(
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
