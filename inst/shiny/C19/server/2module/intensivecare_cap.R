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


# plot --------------------------------------------------------------------

growth <- data.frame(date=get_countryTS()$data,
           growth=get_country_growth()$growth)

growth_xts <- xts(growth[,-1], order.by=growth[,1])

growth_change <- data.frame(date=get_countryTS()$data,
                     growth=get_country_growth()$growth_change)

growth_change_xts <- xts(growth_change[,-1], order.by=growth_change[,1])

hc <- highcharter::highchart(type = "stock") %>% 
  highcharter::hc_title(text = "% growth and growth change of total cases") %>%
  highcharter::hc_add_series(growth_xts, name="growth", color = "red") %>% 
  highcharter::hc_add_series(growth_change_xts, name="growth_change", color = "orange")

output$plot_test <- highcharter::renderHighchart(
  
  hc

)




