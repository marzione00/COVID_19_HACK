output$intensivecare_cap <- plotly::renderPlotly({
  
  
    intensivecare_capacity
    
    x <- c('Product A', 'Product B', 'Product C')
    y <- c(20, 14, 23)
    text <- c('27% market share', '24% market share', '19% market share')
    data <- data.frame(x, y, text)
    
    fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                   text = y, textposition = 'auto',
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)', width = 1.5)))
    fig <- fig %>% layout(title = "January 2013 Sales Report",
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    fig
})