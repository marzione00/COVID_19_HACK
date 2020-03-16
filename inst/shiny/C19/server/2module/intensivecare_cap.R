output$intensivecare_cap <- plotly::renderPlotly({
  
  
    #intensivecare capacity
  #colnames(df) = c("region","intensivecare_cap","intensive_care_occupation")
    
  print(intensivecare_capacity)
  
  print("DOPO")
    fig <- plot_ly(data = intensivecare_capacity, x =~region , y=intensivecare_capacity$perc, type = 'bar',
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(
                          xaxis = list(title = "Region"),
                          yaxis = list(title = "Intensive care capacity"))
    
    fig
})