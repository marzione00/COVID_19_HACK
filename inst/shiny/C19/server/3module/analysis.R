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

output$regionInput <- shiny::renderUI({
  fluidRow(
    column(12,
           
           shiny::selectInput(inputId = "region", label = "Choose one region",
                              choices = regNames, selected = "Lombardia"),
           
           shiny::sliderInput(inputId = "fitInterval", label = "Choose fitting interval",
                              min = init_date, max = fin_date, timeFormat = "%d %b",
                              step = 1, value = c(init_date, fin_date)),
           shiny::checkboxGroupInput(inputId = "plot_type_region", label = "Plot type",
                                     choices = list("Cumulative cases" = 1, "New cases" = 2),
                                     selected = 1),
           shiny::selectInput(inputId = "Typeplot", "Type of plot residuals",choices =  c("Residual","Residual_standardized","Autocorrelation","Sqrt of abs of res vs fitted"),selected = "Residual")
           
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

## REGION plot (currently date against total cases) ##
output$coolplot_region <- plotly::renderPlotly({
  
  waiter::waiter_show(id = "coolplot_region", html = waiter::spin_loaders(id = 1, color = "#ff471a"), color = "white")
  if(!( is.null(input$region) || length(input$region) == 0 ) ) {
    # Data trim and curve fitting #
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
      
      fig <- fig %>% plotly::add_trace(data = confPoints_down, x = ~sample_date_trim, y = ~yConf_down, mode='none', fill = 'tozeroy', name="IGNORED_LEGEND", fillcolor="rgba(0,0,0,0)",showlegend = FALSE)
      fig <- fig %>% plotly::add_trace(data = confPoints_up, x = ~sample_date_trim, y = ~yConf_up, mode='none', fill = 'tonexty' ,name="Confidence interval 95%", fillcolor="rgb(255,250,205)")
      
      fig <- fig %>% plotly::add_trace(data =  points_rem, x =~sample_date_rem, y =~sample_cases_rem ,marker = list(color = "red"), mode = 'markers', name = "Total cases (excluded)")
      fig <- fig %>% plotly::add_trace(data = points_trim, x =~sample_date_trim, y =~sample_cases_trim ,marker = list(color = "green"), mode = 'markers', name = "Total cases (fitting)")
      fig <- fig %>% plotly::add_trace(data = fittedPoints, x = ~days, y = ~yFitted, line = list(color ='rgb(0,0,139)',width=2.5), mode='lines', name = "Fitted logistic curve" )
      
      return(fig)
    }
    
    plot2 = function (fig)
    {
      
      fig <- fig %>% plotly::add_bars(data =  points_diff_rem, x =~sample_date_rem, y =~sample_diff_rem, marker = list(color = "red"), name = "New cases (excluded)")
      fig <- fig %>% plotly::add_bars(data =  points_diff_trim, x =~sample_date_trim, y =~sample_diff_trim, marker = list(color = "green"), name = "New cases (fitting)")
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
      fig <- fig %>% layout(title = "Warning: unrealistic model estimated", font = list(color = 'red'))
    fig
  }
})

output$fit_smry_region <- shiny::renderPrint(
  summary(reac_region$model)
)


output$resid_smry_region <- shiny::renderPrint({
  nlstools::test.nlsResiduals(reac_region$resid)
})

output$Plot_residual <- plotly::renderPlotly({
  pippo<-nlstools::nlsResiduals(reac_region$model)

  Res_DF_1<-as.data.frame(pippo$resi1)
  Res_DF_2<-as.data.frame(pippo$resi2)
  Res_DF_3<-as.data.frame(pippo$resi4)
  Res_DF_4<-as.data.frame(pippo$resi3)
  
  
  if(input$Typeplot=="Residual"){
    colnames(Res_DF_1)=c("fitted1","res")
    p = plotly::plot_ly(data=Res_DF_1,x=~Res_DF_1$fitted1,y=~Res_DF_1$res,marker = list(size = 10,
                                                                                    color = 'rgba(255, 182, 193, .9)',
                                                                                    line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                width = 2)))
    p <- p %>% add_trace(y = 0, name = 'High 2000', line = list(color = 'orange', width = 4, dash = 'dot')) 
    
    p
    #grafico1
    
  }
  
  else if(input$Typeplot=="Residual_standardized"){
    colnames(Res_DF_2)=c("fitted2","res_stand")
    print(rownames(Res_DF_2))
    p = plotly::plot_ly(data=Res_DF_2,x=~Res_DF_2$fitted2,y=~Res_DF_2$res_stand,marker = list(size = 10,
                                                                                          color = 'rgba(255, 182, 193, .9)',
                                                                                          line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                      width = 2)))
    p <- p %>% add_trace(y = 0, name = 'High 2000', line = list(color = 'orange', width = 4, dash = 'dot')) 
    p <- p %>% add_trace(y = 2, name = 'High 2000', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    
    p
    
    p    
  }
  
  else if(input$Typeplot=="Autocorrelation"){
    colnames(Res_DF_3)=c("fitted3","resiplus1")
    print(rownames(Res_DF_3))
    p = plotly::plot_ly(data=Res_DF_3,x=~Res_DF_3$fitted3,y=~Res_DF_3$resiplus1,marker = list(size = 10,
                                                                                          color = 'rgba(255, 182, 193, .9)',
                                                                                          line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                      width = 2)))
    p <- p %>% add_trace(y = 0, name = 'High 2000', line = list(color = 'orange', width = 4, dash = 'dot')) 
    
    p
    #grafico1
    
  }
  
  else if(input$Typeplot=="Sqrt of abs of res vs fitted"){
    colnames(Res_DF_4)=c("fitted4","qq")
    print(rownames(Res_DF_4))
    plotly::plot_ly(data=Res_DF_4,x=~Res_DF_4$fitted4,y=~Res_DF_4$qq,marker = list(size = 10,
                                                                                   color = 'rgba(255, 182, 193, .9)',
                                                                                   line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                               width = 2)))
    #grafico1
    
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
#     
#     fig <- fig %>% plotly::add_trace(data = confPoints_down, x = ~sample_date_trim, y = ~yConf_down, mode='none', fill = 'tozeroy', name="IGNORED_LEGEND", fillcolor="rgba(0,0,0,0)",showlegend = FALSE)
#     fig <- fig %>% plotly::add_trace(data = confPoints_up, x = ~sample_date_trim, y = ~yConf_up, mode='none', fill = 'tonexty' ,name="Confidence interval 95%", fillcolor="rgb(255,250,205)")
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