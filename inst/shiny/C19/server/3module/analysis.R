


## CHECKS FOR ERROR PREVENTING ##
is_ready <- function(x) {
  if(( is.null(x) || length(x) == 0 ))
    return(FALSE)
  
  return(TRUE)
}

# Use this function waitLoading() to validate the content of input$region and prevent errors
# (just use it at the beginning of each chunk of code)
waitLoading <- shiny::reactive({
  shiny::validate(
    shiny::need(is_ready(t$r), "Wait...")
  )
  return(t$r)
})

waitInput <- shiny::reactive({
  shiny::validate(
    shiny::need(is_ready(input$region), "Wait...")
  )
  return(input$region)
})


output$terrInput <- shiny::renderUI({
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::selectizeInput(
                      inputId = "country", label = "Country",
                      choices = countryNames, selected = "Italy")
      ),
      shiny::column(4,
                    shiny::selectizeInput(
                      inputId = "region", label = "Region",
                      choices = c("--- ALL ---" = "default", regNames), selected = NULL)
      ),
      shiny::column(4,
                    shiny::selectizeInput(
                      inputId = "province", label = "Province",
                      choices = c("--- ALL ---" = "default", provNames), selected = NULL)
      )
    ),
    shiny::fluidRow(
      column(12, align = "center",
             shiny::actionButton(inputId = "terr_go", label = "Show!")
      )
    )
  )
  
})

# Province input updater
shiny::observe({
  wait <- waitInput()
  if(input$region != "default") {
    ch <- c("--- ALL ---" = "default", regAndProv[regAndProv$region == input$region, "province"])
  } else {
    ch <- c("--- ALL ---" = "default", provNames)
  }
  shiny::updateSelectizeInput(session, inputId = "province", choices = ch, selected = NULL)
})


# THIS IS THE REACTIVE CONTAINER OF TERRITORY SELECTION VARIABLES.
t <- shiny::reactiveValues()

# Assignment of territory variables when pressing action button
shiny::observeEvent(input$terr_go, {
  if(input$region == "default" && input$province == "default") {
    t$name <- input$country
    t$data <- expression(countryTS)
    t$c <- TRUE
    t$r <- FALSE
    t$p <- FALSE
  } else if(input$province != "default") {
    t$name <- input$province
    t$data <- expression(provTS)
    t$c <- FALSE
    t$r <- FALSE
    t$p <- TRUE
  } else {
    t$name <- input$region
    t$data <- expression(regionTS)
    t$c <- FALSE
    t$r <- TRUE
    t$p <- FALSE
  }
})




output$fitInput <- shiny::renderUI({
  fluidRow(
    column(12,
           
           shiny::sliderInput(inputId = "fitInterval", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = c(init_date, fin_date)),
           shiny::checkboxInput(inputId = "swab_std", label = "Standardise positive cases by total swabs"),
           shiny::checkboxGroupInput(inputId = "plot_type", label = "Plot type",
                                     choices = list("Cumulative cases" = 1, "New cases" = 2),
                                     selected = 1),
           hr(),
           h3("Residuals"),
           shiny::selectInput(inputId = "plot_res_type", "Residuals plot type",choices =  c("Residuals","Residuals_standardized","Autocorrelation","Sqrt of abs of res vs fitted"),selected = "Residuals")
           
    )
  )
})

suggest_date <- function()
{
  wait <- waitLoading()
  
  new = const_trim(eval(t$data)[[t$name]]$totale_casi,1)
  index = which(eval(t$data)[[t$name]]$totale_casi %in% new)
  newdates = eval(t$data)[[t$name]]$data[index]
  
  return(c(newdates[1], newdates[length(newdates)]))
}

output$arimaInput <- shiny::renderUI({
  fluidRow(
    column(12,
           
           shiny::sliderInput(inputId = "arima_interval", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = suggest_date()),
           
           shiny::sliderInput(inputId = "forecast", label = "Choose forecast lags",
                              min = 1,  max = 40, value = 10),
           
           hr(),
           
           shiny::sliderInput(inputId = "ARIMA_q", label = "Choose p",
                              min = 0, max = 10,step = 1,value=0),
           shiny::sliderInput(inputId = "ARIMA_I", label = "Choose i",
                              min = 0, max = 3,step = 1,value=1),
           shiny::sliderInput(inputId = "ARIMA_p", label = "Choose q",
                              min = 0, max = 10,step = 1,value=1)
           
    )
  )
})



## Reactive values for global storage ##
reac_general <- shiny::reactiveValues()
reac_general_TS <- shiny::reactiveValues()

## REGION plot (currently date against total cases) ##

output$coolplot1 <- plotly::renderPlotly({
  
  wait <- waitLoading()
  #waiter::waiter_show(id = "coolplot1", html = waiter::spin_loaders(id = 1, color = "#ff471a"), color = "white")
  # Data trim and curve fitting #
  n <- nrow(eval(t$data)[[t$name]])
  logic_interval <- eval(t$data)[[t$name]]$data >= input$fitInterval[1] &
    eval(t$data)[[t$name]]$data <= input$fitInterval[2]
  
  sample_date <- eval(t$data)[[t$name]]$data_seriale
  
  sample_cases <- eval(t$data)[[t$name]]$totale_casi
  sample_diff <-  c(NA,diff(sample_cases))
  
  if( !t$p && input$swab_std ) {
    swabs <- eval(t$data)[[t$name]]$tamponi
    sample_cases <- sample_cases / swabs
    sample_diff <-  c(NA,sample_diff[-1] / diff(swabs))
  }
  
  
  sample_date_trim <- sample_date[logic_interval]
  sample_cases_trim <- sample_cases[logic_interval]
  sample_diff_trim <- sample_diff[logic_interval]
  
  sample_date_rem <- sample_date[!logic_interval]
  sample_cases_rem <- sample_cases[!logic_interval]
  sample_diff_rem <- sample_diff[!logic_interval]
  
  fit_data <- exe_fit(sample_cases = sample_cases_trim,
                      sample_date = sample_date_trim,
                      days = days)
  
  reac_general$model <- fit_data$out_fit$model
  reac_general$resid <- fit_data$out_resid
  reac_general$vals <- fit_data$out_fit$vals
  
  conf <- nlstools::confint2(level = 0.95, object = reac_general$model)
  
  yConf_up <- (conf["n0",2]*conf["k",2])/(conf["n0",2] + (conf["k",2]-conf["n0",2]) * exp(-conf["r",2]*sample_date_trim))
  yConf_down <- (conf["n0",1]*conf["k",1])/(conf["n0",1] + (conf["k",1]-conf["n0",1]) * exp(-conf["r",1]*sample_date_trim))
  
  
  
  # Conversion to real date and creation of fitted points #
  points_trim <- data.frame("sample_date_trim" = eval(t$data)[[t$name]]$data[logic_interval],
                            sample_cases_trim)
  points_rem <- data.frame("sample_date_rem" = eval(t$data)[[t$name]]$data[!logic_interval],
                           sample_cases_rem)
  points_diff_trim <- data.frame("sample_date_trim" = eval(t$data)[[t$name]]$data[logic_interval],
                                 sample_diff_trim)
  points_diff_rem <- data.frame("sample_date_rem" = eval(t$data)[[t$name]]$data[!logic_interval],
                                sample_diff_rem)
  
  fittedPoints <- fit_data$fittedPoints
  fittedPoints_der <- fit_data$fittedPoints_der
  seq_dates <- seq(from = init_date, by = 1, length.out = length(days))
  fittedPoints$days <- seq_dates
  fittedPoints_der$days <- seq_dates
  
  confPoints_up <- data.frame("sample_date_trim" = eval(t$data)[[t$name]]$data[logic_interval],
                              yConf_up)
  confPoints_down <- data.frame("sample_date_trim" = eval(t$data)[[t$name]]$data[logic_interval],
                                yConf_down)
  
  # PLOT with plotly #
  fig = plotly::plot_ly( name = "Cases", type= "scatter")
  
  # funtions for the two different plots
  plot1  = function(fig)
  {
    
    fig <- fig %>% plotly::add_trace(data = confPoints_down, x = ~sample_date_trim, y = ~yConf_down, mode='none',hoverinfo='skip',fill = 'tozeroy', name="IGNORED_LEGEND", fillcolor="rgba(0,0,0,0)",showlegend = FALSE)
    
    fig <- fig %>% plotly::add_trace(data = confPoints_up, x = ~sample_date_trim, y = ~yConf_up, mode='none', fill = 'tonexty' ,name="Confidence interval 95%", fillcolor="rgb(255,250,205)")
    
    hovlabels <- c("")
    for(i in c(1:n)) {
      hovlabels[i] <- paste(format(eval(t$data)[[t$name]]$data[i], "%d %b"),
                            ", Tot cases = ", eval(t$data)[[t$name]]$totale_casi[i], sep ="")
      if( !t$p && input$swab_std )
        hovlabels[i] <- paste(hovlabels[i], ", Tot swabs = ", swabs[i], sep = "")
    }
    
    fig <- fig %>% plotly::add_trace(data =  points_rem, x =~sample_date_rem, y =~sample_cases_rem ,marker = list(color = "red"), mode = 'markers', name = "Total cases (excluded)",
                                     text = hovlabels[!logic_interval], hoverinfo = 'text')
    fig <- fig %>% plotly::add_trace(data = points_trim, x =~sample_date_trim, y =~sample_cases_trim ,marker = list(color = "green"), mode = 'markers', name = "Total cases (fitting)",
                                     text = hovlabels[logic_interval], hoverinfo = 'text')
    fig <- fig %>% plotly::add_trace(data = fittedPoints, x = ~days, y = ~yFitted, line = list(color ='rgb(0,0,139)',width=2.5), mode='lines', name = "Fitted logistic curve" )
    
    return(fig)
  }
  
  plot2 = function (fig)
  {
    hovlabels <- c("")
    for(i in c(2:n)) {
      hovlabels[i] <- paste(format(eval(t$data)[[t$name]]$data[i], "%d %b"),
                            ", Cases = ", 
                            eval(t$data)[[t$name]]$totale_casi[i] - eval(t$data)[[t$name]]$totale_casi[i-1], 
                            sep ="")
      if( !t$p && input$swab_std )
        hovlabels[i] <- paste(hovlabels[i], ", Swabs = ", swabs[i]-swabs[i-1], sep = "")
    }
    
    fig <- fig %>% plotly::add_bars(data =  points_diff_rem, x =~sample_date_rem, y =~sample_diff_rem, marker = list(color = "red"), name = "New cases (excluded)",
                                    text = hovlabels[!logic_interval], hoverinfo = 'text')
    fig <- fig %>% plotly::add_bars(data =  points_diff_trim, x =~sample_date_trim, y =~sample_diff_trim, marker = list(color = "green"), name = "New cases (fitting)",
                                    text = hovlabels[logic_interval], hoverinfo = 'text')
    fig <- fig %>% plotly::add_trace(data = fittedPoints_der, x = ~days, y = ~yFitted_der, line = list(color ='rgb(255,117,20)',width=2.5), mode='lines', name= "Fitted logistic distribution")
    
    
    return(fig)
  }
  
  #Plot based on the checkbox
  if( 1 %in% input$plot_type && !(2 %in% input$plot_type ) )
  {
    fig = plot1(fig)
  } else if( 2 %in% input$plot_type && !(1 %in% input$plot_type ) )
  {
    fig = plot2(fig)
  } else if( 1 %in% input$plot_type && (2 %in% input$plot_type ) )
  {
    fig = plot1(fig)
    fig = plot2(fig)
  }
  
  # labels and plot
  fig <- fig %>%plotly::layout(xaxis = list(title = "day"), yaxis = list(title = "Infected"))
  if( reac_general$vals$k > 1e7 )
    fig <- fig %>%plotly::layout(title = "Warning: unrealistic model estimated", font = list(color = 'red'),dtick= 5 )
  fig
  
})

#-- Summary of regions ---
output$fit_smry <- shiny::renderPrint({
  wait <- waitLoading()
  summary(reac_general$model)
})


output$resid_smry <- shiny::renderPrint({
  wait <- waitLoading()
  nlstools::test.nlsResiduals(reac_general$resid)
})


#-- Residuals ---

output$plot_residual <- plotly::renderPlotly({
  wait <- waitLoading()
  
  Res_DF_1<-as.data.frame(reac_general$resid$resi1)
  Res_DF_2<-as.data.frame(reac_general$resid$resi2)
  Res_DF_3<-as.data.frame(reac_general$resid$resi4)
  Res_DF_4<-as.data.frame(reac_general$resid$resi3)
  
  p = plotly::plot_ly(type = 'scatter')
  
  p <- p %>% plotly::layout(
    xaxis = list(
      showline = FALSE,
      dtick = 2000,
      zeroline = FALSE
      
    ),
    
    yaxis = list(
      showline = FALSE,
      zeroline = FALSE
      
    )
  )
  
  if( is_ready(input$plot_res_type) ) {
    if(input$plot_res_type=="Residuals"){
      colnames(Res_DF_1)=c("fitted1","res")
      
      p <- p %>% plotly::layout(
        title ="Residuals",
        xaxis = list(title="Fitted values",zeroline = FALSE),
        yaxis = list(title="Residuals")
      )
      p= p %>%plotly::add_trace(name = "residual",data=Res_DF_1,x=~Res_DF_1$fitted1,y=~Res_DF_1$res,marker = list(size = 15,
                                                                                                                  color = 'rgba(255, 182, 193, .9)',
                                                                                                                  line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                              width = 2)))
      p <- p %>%plotly::add_trace(data=Res_DF_2,x=~Res_DF_1$fitted1,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode = 'lines')
      
      p
      #grafico1
      
    }
    
    else if(input$plot_res_type=="Residuals_standardized"){
      colnames(Res_DF_2)=c("fitted2","res_stand")
      p <- p %>% plotly::layout(
        title ="Residuals standardized",
        xaxis = list(title="Fitted values"),
        yaxis = list(title="Standardized residuals")
      )
      p <- p %>%plotly::add_trace(name="Residuals standardized",data=Res_DF_2,x=~Res_DF_2$fitted2,y=~Res_DF_2$res_stand,marker = list(size = 15,
                                                                                                                                      color = 'rgba(255, 182, 193, .9)',
                                                                                                                                      line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                                  width = 2)))
      p <- p %>%plotly::add_trace(data=Res_DF_2,x=~Res_DF_2$fitted2,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode = 'lines')
      p <- p %>%plotly::add_trace(data=Res_DF_2,x=~Res_DF_2$fitted2,y = 2, name = 'liney2', line = list(color = 'rgb(0,0,0)', width = 1),showlegend = FALSE ,mode = 'lines')
      
      
      p
    }
    
    else if(input$plot_res_type=="Autocorrelation"){
      colnames(Res_DF_3)=c("fitted3","resiplus1")
      p <- p %>% plotly::layout(
        xaxis = list(title="Residuals i"),
        yaxis = list(title="Residuals i+1"),
        title ="Autocorrelation")
      p = p %>%plotly::add_trace(name = "Autocorrelation", data=Res_DF_3,x=~Res_DF_3$fitted3,y=~Res_DF_3$resiplus1,marker = list(size = 15,
                                                                                                                                 color = 'rgba(255, 182, 193, .9)',
                                                                                                                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                             width = 2)))
      p <- p %>%plotly::add_trace(data=Res_DF_3,x=~Res_DF_3$fitted3,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode="lines")
      
      p
      #grafico1
      
    }
    
    else if(input$plot_res_type=="Sqrt of abs of res vs fitted"){
      p <- p %>% plotly::layout(
        title ="Sqrt of abs of residual",
        xaxis = list(title="Fitted"),
        yaxis = list(title="Residuals")
      )
      colnames(Res_DF_4)=c("fitted4","qq")
      p = p %>%plotly::add_trace(name="Sqrt of abs of res vs fitted",data=Res_DF_4,x=~Res_DF_4$fitted4,y=~Res_DF_4$qq,marker = list(size = 15,
                                                                                                                                    color = 'rgba(255, 182, 193, .9)',
                                                                                                                                    line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                                width = 2)))
      p
      #grafico1
    }
  }
})



##===================================== ARIMA SECTION =============================================##
reac_ARIMA <- shiny::reactiveValues()

#   shiny::sliderInput(inputId = "arima_interval", label = "Choose fitting interval",
# min = init_date, max = fin_date, timeFormat = "%d %b",
# step = 1, value = c(init_date, fin_date)),


## HERE GOES ALL ARIMA IMPLEMENTATION COMMON TO ALL GRAPHS
shiny::observe({
  wait <- waitLoading()
  
  
  reac_ARIMA$logic_interval <- eval(t$data)[[t$name]]$data >=  input$arima_interval[1]  &
    eval(t$data)[[t$name]]$data <= input$arima_interval[2] 
  
  
  

  reac_ARIMA$sample_date <- eval(t$data)[[t$name]]$data_seriale
  reac_ARIMA$sample_cases <- eval(t$data)[[t$name]]$totale_casi
  reac_ARIMA$sample_diff <-  c(NA,diff(reac_ARIMA$sample_cases))
  
  
  reac_ARIMA$sample_date_trim <- reac_ARIMA$sample_date[reac_ARIMA$logic_interval]
  reac_ARIMA$sample_cases_trim <- reac_ARIMA$sample_cases[reac_ARIMA$logic_interval]+1
  reac_ARIMA$sample_diff_trim <- reac_ARIMA$sample_diff[reac_ARIMA$logic_interval]
  
  reac_ARIMA$sample_date_rem <- reac_ARIMA$sample_date[!reac_ARIMA$logic_interval]
  reac_ARIMA$sample_cases_rem <- reac_ARIMA$sample_cases[!reac_ARIMA$logic_interval]
  reac_ARIMA$sample_diff_rem <- reac_ARIMA$sample_diff[!reac_ARIMA$logic_interval]
  
  reac_ARIMA$points_trim <- data.frame("sample_date_trim" = eval(t$data)[[t$name]]$data[reac_ARIMA$logic_interval],
                                       "sample_cases_trim" = reac_ARIMA$sample_cases_trim)
  
  
  
  reac_ARIMA$arima <- checkExp(  stats::arima(log(reac_ARIMA$sample_cases_trim),order=c(input$ARIMA_p,input$ARIMA_I,input$ARIMA_q)) , "There is not a suitable ARIMA model")
  
  #  reac_ARIMA$arima <- stats::arima(log(reac_ARIMA$sample_cases_trim),order=c(input$ARIMA_p,input$ARIMA_I,input$ARIMA_q))
})


output$log <- renderText({
  wait <- waitInput()
  # paste("input$country = ", input$country, ",  input$region = ", input$region, 
  #       "   input$province = ", input$province, "   t$c = ", t$c, "   t$r = ", t$r,
  #       "   t$p = ", t$p)
  paste(reac_ARIMA$sample_cases_trim, collapse = "; ")
})




## Plot of autocorrelation function
output$arima_coolplot2 <- plotly::renderPlotly({
  
  wait <- waitLoading()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    
    p = checkExp(  forecast::autoplot(acf(log(reac_ARIMA$sample_cases_trim))) , "There is not a forecast for the ARIMA model")
    
    plotly::ggplotly(p)
    
    
  }
  
})

## Plot of partial autocorrelation function
output$arima_coolplot3 <- plotly::renderPlotly({
  
  wait <- waitLoading()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    
    
    p =     checkExp( forecast::autoplot(pacf(log(reac_ARIMA$sample_cases_trim)) ) , "There is not a forecast for the ARIMA model")
    plotly::ggplotly(p)
    
    
  }
  
})

## Plot of ARIMA Time Series and FORECAST
output$arima_coolplot1 <- plotly::renderPlotly({
  
  wait <- waitLoading()
  #acf(log(sample_cases_trim))
  #pacf(log(sample_cases_trim))
  
  #print(reac_general_TS )
  if(is_ready(reac_ARIMA$points_trim)) {
    
    
    fore = checkExp(forecast::forecast(reac_ARIMA$arima,input$forecast) , "There is not a forecast for the ARIMA model")
    
    sdt <- reac_ARIMA$points_trim$sample_date_trim
    
    fore.dates <- seq(from = sdt[length(sdt)], by = 1, len = input$forecast)
    
    p <-plotly::plot_ly() %>%
      plotly::add_ribbons(x = fore.dates,
                          ymin = fore$mean,
                          ymax = fore$upper[, 2],
                          color = I("#17becf"),
                          name = "95% confidence") %>%
      plotly::add_ribbons(p,
                          x = fore.dates,
                          ymin = fore$mean,
                          ymax = fore$upper[, 1],
                          color = I("#ed9dac"), name = "80% confidence")%>%
      plotly::add_lines(x = sdt, y = log(reac_ARIMA$sample_cases_trim),
                        color = I("#037d50"),
                        name = "observed",
                        mode="lines")%>%
      plotly::add_lines(x = fore.dates, y = fore$mean, color = I("#ee1147"), name = "prediction")
    
    p <- p %>% plotly::layout(
      title = paste0("ARIMA Forecast (",input$ARIMA_p,",",input$ARIMA_I,",",input$ARIMA_q,")"),
      xaxis = list(title="Days"),
      yaxis = list(title="log cases")
    )
    
    p
    
  }
})

## Plot of ARIMA residuals
output$arima_coolplot4 <- shiny::renderPlot({
  
  wait <- waitLoading()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    
    
    result = checkExp(forecast::checkresiduals(reac_ARIMA$arima) , "There is not a suitable residuals for the ARIMA model")
    result
    #forecast::checkresiduals(reac_ARIMA$arima)
  }
  
})


# --- Summary ARIMA

## Print of suggested parameters
output$parameters_sugg <- shiny::renderUI({
  
  wait <- waitLoading()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    
    
    auto_arima = checkExp(forecast::auto.arima(log(reac_ARIMA$sample_cases_trim)),  "There is not a suitable ARIMA model")
    
    h3(paste("Suggested Parameters: ",toString(auto_arima)))
    
    
  }
  
})

# toString(reac_ARIMA$arima$coef)
# suggested fit toString(forecast::auto.arima(log(sample_cases_trim)))
#reac_ARIMA$arima$coef
# reac_ARIMA$arima$sigma2

output$arima_shell_output <- shiny::renderPrint({
  
  wait <- waitLoading()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    
    result = checkExp(arima(log(reac_ARIMA$sample_cases_trim),order=c(input$ARIMA_p,input$ARIMA_I,input$ARIMA_q)),"There is not a suitable ARIMA model")
    result
  }
  
})


