output$intensivecare_cap <- plotly::renderPlotly({
  
    print(intensivecare_capacity)

    fig <- plot_ly(data = intensivecare_capacity, x =~region , y=~perc, type = 'bar', 
                   marker = list(color = 'rgb(255,250,205)',
                                 line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(
                          xaxis = list(title = "Region"),
                          yaxis = list(title = "Percentage occupation/capacity"))
    
    fig
})