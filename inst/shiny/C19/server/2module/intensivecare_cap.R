# General info plot


output$general_infos_plot <- highcharter::renderHighchart(
  
  
  highcharter::hchart(countryTS,"spline",title= "General info",highcharter::hcaes(x=data,y = totale_casi),  name="Total cases", color="#383838", yAxis = 1)
  %>% 
    
    highcharter::hc_chart(zoomType = "xy") %>%
    
    highcharter::hc_yAxis_multiples(
      list(lineWidth = 3, title = list(text  =  '')),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text  =  ''))
    ) %>%
    
    highcharter::hc_add_series(data = countryTS, type = "spline", 
                               yAxis = 1, highcharter::hcaes(x = data, y = terapia_intensiva),
                               name="Total Intesive care", color="red")
  %>%
    highcharter::hc_add_series(data = countryTS, type = "spline", 
                               yAxis = 1, highcharter::hcaes(x = data, y = ricoverati_con_sintomi),
                               name="Total Hospitalized", color="orange")
  %>%
    highcharter::hc_add_series(data = countryTS, type = "spline", 
                               yAxis = 1, highcharter::hcaes(x = data, y = deceduti),
                               name="Total Deaths", color="black")
  %>%
    highcharter::hc_add_series(data = countryTS, type = "spline", 
                               yAxis = 1, highcharter::hcaes(x = data, y = dimessi_guariti),
                               name="Total recovered", color="green")
  %>%
    highcharter::hc_legend(align = "top", verticalAlign = "top",
              layout = "vertical", x = 0, y = 100) 
)


# tabbox selection
output$tabset2Selected <- renderText({
  input$tabset2
})

# plots
output$intensivecare_cap_perc <- plotly::renderPlotly({
  
  
  fig <- plot_ly(type = 'bar', marker = list(color = intensivecare_capacity$perc, width=3,line = list(color = 'rgb(8,48,107)', width = 1.5)))
  fig <- fig %>% add_bars(data = intensivecare_capacity, x =~region , y=~perc, name="percentage",
                          text = ~perc, textposition = 'auto' )
  fig <- fig %>% layout(
    xaxis = list(title = "Region"),
    yaxis = list(title = "Percentage occupation/capacity"),
    legend = list(x = 0.1, y = 0.9))
  
  fig
})



output$intensivecare_cap <- plotly::renderPlotly({
  
  df = intensivecare_capacity[order(intensivecare_capacity$occupation,decreasing = TRUE),]
  
  fig  = plot_ly(type="bar")
  fig <- fig %>% add_trace(data = df, name = "capacity",x = ~region, y = ~capacity, type = 'bar',
                           text = ~capacity, textposition = 'auto',
                           marker = list(color = 'rgb(255,228,181)',
                                         line = list(color = 'rgb(8,48,107)', width = 1.5)))
  
  fig <- fig %>% add_trace(data = df,name = "occupation",x = ~region, y=~occupation, type = 'bar',
                           text = ~occupation, textposition = 'auto',
                           marker = list(color = 'rgb(220,20,60)',
                                         line = list(color = 'rgb(8,48,107)', width = 1.5)))
  
  
  fig <- fig %>% layout(title = "Occupation vs Capacity",
                        barmode = 'group',
                        xaxis = list(title = "Region"),
                        yaxis = list(title = "Occupation vs Capacity"),
                        legend = list(x = 0.1, y = 0.9))
  
  fig
  
  
  
})


# plot growth monitoring --------------------------------------------------------------------


output$plot_test <- highcharter::renderHighchart(
  hc
)

# tamponi graph -----------------------------------------------------------


output$tamp_plot <- highcharter::renderHighchart(
  highcharter::hchart(tamp_data_1, "column", highcharter::hcaes(x = date, y = value, group = key), color=c("red","#888888")) %>% 
    highcharter::hc_chart(zoomType = "xy") %>%
    
    highcharter::hc_yAxis_multiples(
      list(lineWidth = 3, title = list(text  =  '')),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text  =  ''))
    ) %>%
    highcharter::hc_add_series(data = tamp_data, type = "spline", 
                               yAxis = 1, highcharter::hcaes(x = date, y = share_infected_discovered),
                               name="share_infected_discovered", color="#383838")
)
