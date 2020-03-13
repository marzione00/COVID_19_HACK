server <- function(input, output, session) {

  ## DATI ##
  # NB!! Da inserire dentro un eventReactive di un BUTTON "refresh" #
  countryTS <- get_countryTS()
  regionTS <- get_regionTS()
  provTS <- get_provTS

  regNames <- names(regionTS)
  provNames <- names(provTS)

  # Time horizon of all graphs
  Days <- (1:50)



  # conf<-nlstools::confint2(level = 0.95,model)

  #Conf_UP<-conf[1,1]/(conf[2,1] * exp(conf[3,1] * Days_dat) +1 )
  #Conf_DOWN<-conf[1,2]/(conf[2,2] * exp(conf[3,2] * Days_dat) +1 )

  #data_u<-data.frame(Days_dat,Conf_UP)
  #data_u<-data.frame(Days_dat,Conf_DOWN)

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
        shiny::verbatimTextOutput("coolplot5_country")
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
        shiny::uiOutput("filter_degree_region")
      ),
      shiny::mainPanel(
        shiny::plotOutput("coolplot_region"), #plots that has to be loaded on the app
        shiny::verbatimTextOutput("coolplot5_region")
      )
    )
  })

  ## REGION reactive values ##
  reac_region <- shiny::reactiveValues()

  ## REGION plot (currently date against total cases) ##
  output$coolplot_region <- shiny::renderPlot({

    # Data retrieval and fitting
    date <- regionTS[[input$region]]$data_seriale
    cases <- regionTS[[input$region]]$totale_casi
    points <- data.frame(date,cases)
    reac_region$model <- stats::nls(cases ~ (alpha/(beta * exp(gamma * date) +1 )), start = list(alpha=9000,beta=1700,gamma=-0.36))
    coeff <- coef(reac_region$model)

    # Creation of fitted points
    yFitted <- coeff[1]/(coeff[2] * exp(coeff[3] * Days) + 1)
    fittedPoints <- data.frame(Days,yFitted)

    # Plot of fitted curve and points
    ggplot2::ggplot(fittedPoints,ggplot2::aes_string("Days","yFitted")) +
      ggplot2::geom_line(color="darkblue",size=1)+
    ggplot2::geom_point(data=points,ggplot2::aes_string("date","cases"),size=1,color="red")+
      #geom_line(data=data_u,aes_string("Days_dat","Conf_UP"),linetype = "dashed",size=1.05) +
      #geom_line(data=data_u,aes_string("Days_dat","Conf_DOWN"),linetype = "dashed",size=1.05) +
      ggplot2::theme_dark() +
      ggplot2::theme(legend.position="top",panel.grid.minor = ggplot2::element_line(),text = ggplot2::element_text(size=14)) +
      ggplot2::scale_x_continuous(breaks=seq(0, 50, 2))+
      ggplot2::scale_y_continuous(breaks=seq(0, 44000, 2000))+
      ggplot2::labs(title="COVID19")
  })

  output$coolplot5_region <- shiny::renderPrint(
    summary(reac_region$model)
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
