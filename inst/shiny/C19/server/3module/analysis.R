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


## CHECKS FOR ERROR PREVENTING ##
is_ready <- function(x) {
  if(( is.null(x) || length(x) == 0 ))
    return(FALSE)
  
  return(TRUE)
}

# Use this function region() to validate the content of input$region and prevent errors
# (just use it at the beginning of each chunk of code)
region <- shiny::reactive({
  shiny::validate(
    shiny::need(is_ready(input$region), "Wait...")
  )
  return(input$region)
})





output$regionInput <- shiny::renderUI({
  fluidRow(
    column(12,
           
           h3("Confirmed cases of infection"),
           shiny::selectInput(inputId = "region", label = "Choose one region",
                              choices = regNames, selected = "Lombardia"),
           
           shiny::sliderInput(inputId = "fitInterval", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = c(init_date, fin_date)),
           shiny::checkboxInput(inputId = "swab_std", label = "Standardise positive cases by total swabs"),
           shiny::checkboxGroupInput(inputId = "plot_type_region", label = "Plot type",
                                     choices = list("Cumulative cases" = 1, "New cases" = 2),
                                     selected = 1),
           hr(),
           h3("Residuals"),
           shiny::selectInput(inputId = "plot_res_type_region", "Plot type",choices =  c("Residual","Residual_standardized","Autocorrelation","Sqrt of abs of res vs fitted"),selected = "Residual")
           
    )
  )
})


output$regionInput_TS <- shiny::renderUI({
  fluidRow(
    column(12,
           
           h3("ARIMA inputs"),
           
           shiny::sliderInput(inputId = "arima_interval", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = c(init_date, fin_date)),
           
           shiny::sliderInput(inputId = "forecast", label = "Choose forecast lags",
                              min = 1,  max = 40, value = 10),
           
           hr(),
           
           shiny::sliderInput(inputId = "ARIMA_p", label = "Choose a q value",
                              min = 0, max = 10,step = 1,value=1),
           shiny::sliderInput(inputId = "ARIMA_q", label = "Choose a p value",
                              min = 0, max = 10,step = 1,value=0),
           shiny::sliderInput(inputId = "ARIMA_I", label = "Choose a I value",
                              min = 0, max = 3,step = 1,value=1)
           
    )
  )
})



output$countryInput <- shiny::renderUI({
  fluidRow(
    column(12,
           h3("Italy"),
           hr(),
           shiny::sliderInput(inputId = "fi2", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = c(init_date, fin_date)),
           shiny::checkboxGroupInput(inputId = "plot_type_country", label = "Plot type",
                                     choices = list("Cumulative cases" = 1, "New cases" = 2),
                                     selected = 1)
    )
  )
})



## REGION reactive values ##
reac_region <- shiny::reactiveValues()
reac_region_TS <- shiny::reactiveValues()


## REGION plot (currently date against total cases) ##
output$coolplot_region <- plotly::renderPlotly({
  wait <- region()
  #waiter::waiter_show(id = "coolplot_region", html = waiter::spin_loaders(id = 1, color = "#ff471a"), color = "white")
    # Data trim and curve fitting #
    n <- nrow(regionTS[[input$region]])
    logic_interval <- regionTS[[input$region]]$data >= input$fitInterval[1] &
      regionTS[[input$region]]$data <= input$fitInterval[2]
    
    sample_date <- regionTS[[input$region]]$data_seriale
    
    sample_cases <- regionTS[[input$region]]$totale_casi
    sample_diff <-  c(NA,diff(sample_cases))
    
    if( input$swab_std ) {
      swabs <- regionTS[[input$region]]$tamponi
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
    
    reac_region$model <- fit_data$out_fit$model
    reac_region$resid <- fit_data$out_resid
    reac_region$vals <- fit_data$out_fit$vals
    
    conf <- nlstools::confint2(level = 0.95, object = reac_region$model)
    
    yConf_up <- (conf["n0",2]*conf["k",2])/(conf["n0",2] + (conf["k",2]-conf["n0",2]) * exp(-conf["r",2]*sample_date_trim))
    yConf_down <- (conf["n0",1]*conf["k",1])/(conf["n0",1] + (conf["k",1]-conf["n0",1]) * exp(-conf["r",1]*sample_date_trim))
    
    
    
    # Conversion to real date and creation of fitted points #
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
    
    confPoints_up <- data.frame("sample_date_trim" = regionTS[[input$region]]$data[logic_interval],
                                yConf_up)
    confPoints_down <- data.frame("sample_date_trim" = regionTS[[input$region]]$data[logic_interval],
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
        hovlabels[i] <- paste(format(regionTS[[input$region]]$data[i], "%d %b"),
                              ", Tot cases = ", regionTS[[input$region]]$totale_casi[i], sep ="")
        if( input$swab_std )
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
        hovlabels[i] <- paste(format(regionTS[[input$region]]$data[i], "%d %b"),
                              ", Cases = ", 
                              regionTS[[input$region]]$totale_casi[i] - regionTS[[input$region]]$totale_casi[i-1], 
                              sep ="")
        if( input$swab_std )
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
    if( 1 %in% input$plot_type_region && !(2 %in% input$plot_type_region ) )
    {
      fig = plot1(fig)
    } else if( 2 %in% input$plot_type_region && !(1 %in% input$plot_type_region ) )
    {
      fig = plot2(fig)
    } else if( 1 %in% input$plot_type_region && (2 %in% input$plot_type_region ) )
    {
      fig = plot1(fig)
      fig = plot2(fig)
    }
    
    # labels and plot
    fig <- fig %>% layout(xaxis = list(title = "day"), yaxis = list(title = "Infected"))
    if( reac_region$vals$k > 1e7 )
      fig <- fig %>% layout(title = "Warning: unrealistic model estimated", font = list(color = 'red'),dtick= 5 )
    fig
    
})

#-- Summary of regions ---
output$fit_smry_region <- shiny::renderPrint({
  wait <- region()
  summary(reac_region$model)
})


output$resid_smry_region <- shiny::renderPrint({
  wait <- region()
  nlstools::test.nlsResiduals(reac_region$resid)
})


#-- Residuals ---

output$Plot_residual <- plotly::renderPlotly({
  wait <- region()
  pippo<-reac_region$resid
  
  Res_DF_1<-as.data.frame(pippo$resi1)
  Res_DF_2<-as.data.frame(pippo$resi2)
  Res_DF_3<-as.data.frame(pippo$resi4)
  Res_DF_4<-as.data.frame(pippo$resi3)
  
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
  
  if( is_ready(input$plot_res_type_region) ) {
    if(input$plot_res_type_region=="Residual"){
      colnames(Res_DF_1)=c("fitted1","res")
      
      p <- p %>% plotly::layout(
        title ="Residual",
        xaxis = list(title="Fitted values",zeroline = FALSE),
        yaxis = list(title="Residuals")
      )
      p= p %>% add_trace(name = "residual",data=Res_DF_1,x=~Res_DF_1$fitted1,y=~Res_DF_1$res,marker = list(size = 15,
                                                                                                           color = 'rgba(255, 182, 193, .9)',
                                                                                                           line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                       width = 2)))
      p <- p %>% add_trace(data=Res_DF_2,x=~Res_DF_1$fitted1,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode = 'lines') 
      
      p
      #grafico1
      
    }
    
    else if(input$plot_res_type_region=="Residual_standardized"){
      colnames(Res_DF_2)=c("fitted2","res_stand")
      p <- p %>% plotly::layout(
        title ="Residual standardized",
        xaxis = list(title="Fitted values"),
        yaxis = list(title="Standardized residuals")
      )
      p <- p %>% add_trace(name="Residual standardized",data=Res_DF_2,x=~Res_DF_2$fitted2,y=~Res_DF_2$res_stand,marker = list(size = 15,
                                                                                                                              color = 'rgba(255, 182, 193, .9)',
                                                                                                                              line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                          width = 2)))
      p <- p %>% add_trace(data=Res_DF_2,x=~Res_DF_2$fitted2,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode = 'lines') 
      p <- p %>% add_trace(data=Res_DF_2,x=~Res_DF_2$fitted2,y = 2, name = 'liney2', line = list(color = 'rgb(0,0,0)', width = 1),showlegend = FALSE ,mode = 'lines') 
      
      
      p    
    }
    
    else if(input$plot_res_type_region=="Autocorrelation"){
      colnames(Res_DF_3)=c("fitted3","resiplus1")
      p <- p %>% plotly::layout(
        xaxis = list(title="Residuals i"),
        yaxis = list(title="Residuals i+1"),
        title ="Autocorrelation")
      p = p %>% add_trace(name = "Autocorrelation", data=Res_DF_3,x=~Res_DF_3$fitted3,y=~Res_DF_3$resiplus1,marker = list(size = 15,
                                                                                                                          color = 'rgba(255, 182, 193, .9)',
                                                                                                                          line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                                                      width = 2)))
      p <- p %>% add_trace(data=Res_DF_3,x=~Res_DF_3$fitted3,y = 0, name = "liney0", line = list(color = 'rgb(0,0,0)', width = 1, dash = 'dot'),showlegend = FALSE, mode="lines") 
      
      p
      #grafico1
      
    }
    
    else if(input$plot_res_type_region=="Sqrt of abs of res vs fitted"){
      p <- p %>% plotly::layout(
        title ="Sqrt of abs of residual",
        xaxis = list(title="Fitted"),
        yaxis = list(title="Residuals")
      )
      colnames(Res_DF_4)=c("fitted4","qq")
      p = p %>% add_trace(name="Sqrt of abs of res vs fitted",data=Res_DF_4,x=~Res_DF_4$fitted4,y=~Res_DF_4$qq,marker = list(size = 15,
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

## HERE GOES ALL ARIMA IMPLEMENTATION COMMON TO ALL GRAPHS
shiny::observe({
  wait <- region()
  reac_ARIMA$logic_interval <- regionTS[[input$region]]$data >= input$arima_interval[1] &
                               regionTS[[input$region]]$data <= input$arima_interval[2]
  
  reac_ARIMA$sample_date <- regionTS[[input$region]]$data_seriale
  reac_ARIMA$sample_cases <- regionTS[[input$region]]$totale_casi
  reac_ARIMA$sample_diff <-  c(NA,diff(reac_ARIMA$sample_cases))
  
  reac_ARIMA$sample_date_trim <- reac_ARIMA$sample_date[reac_ARIMA$logic_interval]
  reac_ARIMA$sample_cases_trim <- reac_ARIMA$sample_cases[reac_ARIMA$logic_interval]
  reac_ARIMA$sample_diff_trim <- reac_ARIMA$sample_diff[reac_ARIMA$logic_interval]
  
  reac_ARIMA$sample_date_rem <- reac_ARIMA$sample_date[!reac_ARIMA$logic_interval]
  reac_ARIMA$sample_cases_rem <- reac_ARIMA$sample_cases[!reac_ARIMA$logic_interval]
  reac_ARIMA$sample_diff_rem <- reac_ARIMA$sample_diff[!reac_ARIMA$logic_interval]
  
  reac_ARIMA$points_trim <- data.frame("sample_date_trim" = regionTS[[input$region]]$data[reac_ARIMA$logic_interval],
                                        "sample_cases_trim" = reac_ARIMA$sample_cases_trim)
  
  reac_ARIMA$arima <- arima(log(reac_ARIMA$sample_cases_trim),order=c(input$ARIMA_p,input$ARIMA_I,input$ARIMA_q))
})

## Plot of autocorrelation function
output$Arima_coolplot0 <- plotly::renderPlotly({
  
  wait <- region()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    p = autoplot(acf(log(reac_ARIMA$sample_cases_trim)))
    ggplotly(p)
  }
  
})

## Plot of partial autocorrelation function
output$Arima_coolplot00 <- plotly::renderPlotly({
  
  wait <- region()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    p = ggplot2::autoplot(pacf(log(reac_ARIMA$sample_cases_trim)) )
    ggplotly(p)
  }
 
})

## Plot of ARIMA Time Series and FORECAST
output$Arima_coolplot <- plotly::renderPlotly({
  
  wait <- region()
  #acf(log(sample_cases_trim))  
  #pacf(log(sample_cases_trim))
  
  #print(reac_region_TS )
  if(is_ready(reac_ARIMA$points_trim)) {
    fore <- forecast::forecast(reac_ARIMA$arima,input$forecast)
  
    sdt <- reac_ARIMA$points_trim$sample_date_trim
    
    fore.dates <- seq(from = sdt[length(sdt)], by = 1, len = input$forecast)
    
    p <- plot_ly() %>%
      add_ribbons(x = fore.dates, 
                  ymin = fore$lower[, 2], 
                  ymax = fore$upper[, 2],
                  color = I("#17becf"), 
                  name = "95% confidence") %>%
      add_ribbons(p, 
                  x = fore.dates, 
                  ymin = fore$lower[, 1], 
                  ymax = fore$upper[, 1],
                  color = I("#ed9dac"), name = "80% confidence")%>% 
      add_lines(x = sdt, y = log(reac_ARIMA$sample_cases_trim),
              color = I("#037d50"), 
              name = "observed", 
              mode="lines")%>% 
      add_lines(x = fore.dates, y = fore$mean, color = I("#ee1147"), name = "prediction")
    
    p <- p %>% plotly::layout(
      title = paste0("ARIMA Forecast ( ",input$ARIMA_p,", ",input$ARIMA_I,", ",input$ARIMA_q," )"),
      xaxis = list(title="Days"),
      yaxis = list(title="log cases")
    )
    p
    
    #p = TSplotly::TSplot(length(reac_ARIMA$sample_cases_trim),forecast::forecast(reac_ARIMA$arima,input$forecast),  Ylab = "Value", Xlab = "Time (Day) ",NEWtitle=paste0("ARIMA Forecast ( ",input$ARIMA_p,", ",input$ARIMA_I,", ",input$ARIMA_q," )"),title_size =15, ts_original = "Original time series", ts_forecast= "Predicted time series")
    
    
    #autoplot(forecast::forecast(reac_ARIMA$arima ))
  }
})

## Plot of ARIMA residuals
output$Arima_coolplot2 <- shiny::renderPlot({
  
  wait <- region()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    forecast::checkresiduals(reac_ARIMA$arima)
  }
  
})


# --- Summary ARIMA

## Print of suggested parameters
output$parameters_sugg <- shiny::renderUI({
 
  wait <- region()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    auto_arima <- forecast::auto.arima(log(reac_ARIMA$sample_cases_trim))
    h3(paste("Suggested Parameters: ",toString(auto_arima)))
  }
  
})


# toString(reac_ARIMA$arima$coef)
# suggested fit toString(forecast::auto.arima(log(sample_cases_trim)))
#reac_ARIMA$arima$coef
# reac_ARIMA$arima$sigma2

output$Arima_shell_output <- shiny::renderPrint({
  
  wait <- region()
  if(is_ready(reac_ARIMA$sample_cases_trim)) {
    arima(log(reac_ARIMA$sample_cases_trim),order=c(input$ARIMA_p,input$ARIMA_I,input$ARIMA_q))
  }
  
})

#======================================= COUNTRY SECTION ============================================
## COUNTRY reactive values##

# reac_country = shiny::reactiveValues()
# 
# output$coolplot_country <- plotly::renderPlotly({
#   
#   waiter::waiter_show(id = "coolplot_country", html = waiter::spin_loaders(id = 1, color = "#ff471a"), color = "white")
#   
#   logic_interval <- countryTS$data >= input$fi2[1] & countryTS$data <= input$fi2[2]
#   sample_date = countryTS$data_seriale
#   sample_cases <- countryTS$totale_casi
#   sample_diff <-  c(NA,diff(sample_cases))
#   
#   sample_date_trim <- sample_date[logic_interval]
#   sample_cases_trim <- sample_cases[logic_interval]
#   sample_diff_trim <- sample_diff[logic_interval]
#   
#   sample_date_rem <- sample_date[!logic_interval]
#   sample_cases_rem <- sample_cases[!logic_interval]
#   sample_diff_rem <- sample_diff[!logic_interval]
#   
#   fit_data <- exe_fit(sample_cases = sample_cases_trim,
#                       sample_date = sample_date_trim,
#                       days = days)
#   
#   
#   reac_country$model <- fit_data$out_fit$model
#   reac_country$resid <- fit_data$out_resid
#   reac_country$vals <- fit_data$out_fit$vals
#   
#   conf <- nlstools::confint2(level = 0.95, object = reac_country$model)
#   
#   yConf_up <- (conf["n0",2]*conf["k",2])/(conf["n0",2] + (conf["k",2]-conf["n0",2]) * exp(-conf["r",2]*sample_date_trim))
#   yConf_down <- (conf["n0",1]*conf["k",1])/(conf["n0",1] + (conf["k",1]-conf["n0",1]) * exp(-conf["r",1]*sample_date_trim))
#   
#   
#   
#   # Conversion to real date and creation of fitted points #
#   points_trim <- data.frame("sample_date_trim" = countryTS$data[logic_interval],
#                             sample_cases_trim)
#   points_rem <- data.frame("sample_date_rem" = countryTS$data[!logic_interval],
#                            sample_cases_rem)
#   points_diff_trim <- data.frame("sample_date_trim" = countryTS$data[logic_interval],
#                                  sample_diff_trim)
#   points_diff_rem <- data.frame("sample_date_rem" = countryTS$data[!logic_interval],
#                                 sample_diff_rem)
#   
#   fittedPoints <- fit_data$fittedPoints
#   fittedPoints_der <- fit_data$fittedPoints_der
#   seq_dates <- seq(from = init_date, by = 1, length.out = length(days))
#   fittedPoints$days <- seq_dates
#   fittedPoints_der$days <- seq_dates
#   
#   confPoints_up <- data.frame("sample_date_trim" = countryTS$data[logic_interval],
#                               yConf_up)
#   confPoints_down <- data.frame("sample_date_trim" =  countryTS$data[logic_interval],
#                                 yConf_down)
#   
#   
#   
#   ###---- PLOT
#   # PLOT with plotly #
#   fig = plotly::plot_ly( name = "Cases", type= "scatter")
#   
#   # funtions for the two different plots
#   plot1  = function(fig)
#   {
#     fig <- fig %>% plotly::add_trace(data = confPoints_up, x = ~sample_date_trim, y = ~yConf_up, mode='none', fill = 'tonexty' ,name="Confidence interval 95%", fillcolor="rgb(255,250,205)")
#     fig <- fig %>% plotly::add_trace(data = confPoints_down, x = ~sample_date_trim, y = ~yConf_down, mode='none', fill = 'tozeroy', name="IGNORED_LEGEND", fillcolor="rgba(0,0,0,0)",showlegend = FALSE)
#     
#     fig <- fig %>% plotly::add_trace(data =  points_rem, x =~sample_date_rem, y =~sample_cases_rem ,marker = list(color = "red"), mode = 'markers', name = "Total cases (excluded)")
#     fig <- fig %>% plotly::add_trace(data = points_trim, x =~sample_date_trim, y =~sample_cases_trim ,marker = list(color = "green"), mode = 'markers', name = "Total cases (fitting)")
#     fig <- fig %>% plotly::add_trace(data = fittedPoints, x = ~days, y = ~yFitted, line = list(color ='rgb(0,0,139)',width=2.5), mode='lines', name = "Fitted logistic curve" )
#     
#     return(fig)
#   }
#   
#   plot2 = function (fig)
#   {
#     
#     fig <- fig %>% plotly::add_bars(data =  points_diff_rem, x =~sample_date_rem, y =~sample_diff_rem, marker = list(color = "red"), name = "New cases (excluded)")
#     fig <- fig %>% plotly::add_bars(data =  points_diff_trim, x =~sample_date_trim, y =~sample_diff_trim, marker = list(color = "green"), name = "New cases (fitting)")
#     fig <- fig %>% plotly::add_trace(data = fittedPoints_der, x = ~days, y = ~yFitted_der, line = list(color ='rgb(255,117,20)',width=2.5), mode='lines', name= "Fitted logistic distribution")
#     
#     
#     return(fig)
#   }
#   
#   #Plot based on the checkbox
#   if( 1 %in% input$plot_type_country && !(2 %in% input$plot_type_country ) )
#   {
#     fig = plot1(fig)
#   } else if( 2 %in% input$plot_type_country && !(1 %in% input$plot_type_country ) )
#   {
#     fig = plot2(fig)
#   } else if( 1 %in% input$plot_type_country && (2 %in% input$plot_type_country ) )
#   {
#     fig = plot1(fig)
#     fig = plot2(fig)
#   }
#   
#   # labels and plot
#   fig <- fig %>% layout(xaxis = list(title = "day"), yaxis = list(title = "Infected"))
#   if( reac_country$vals$k > 1e8 )
#     fig <- fig %>% layout(title = "Warning: unrealistc model estimated", font = list(color = 'red'))
#   fig
#   
#   
# })
# 
# output$fit_smry_country <- shiny::renderPrint(
#   summary(reac_country$model)
# )
# 
# 
# output$resid_smry_country <- shiny::renderText({
#   nlstools::test.nlsResiduals(reac_country$resid)
# })