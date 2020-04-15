# map ---------------------------------------------------------------------

custom_map <- reactive({
  if(input$map_value=="absolute"){
    list("absolute (total cases)",
         c("#FFE4B5","#FFA500","#FF4500","#cc0000")
    )
  } else if(input$map_value=="percentage") {
    list("percentage (cases/pop * 100)",
         c("#D4E6F1", "#7FB3D5", "#2980B9", "#1A5276")
    )
  } else if(input$map_value=="density") {
    list("density (cases/km^2 * 1000)",
         c("#d8ebb5","#639a67","#2b580c","#003000")
    )
  } else {
    list("growth",
         c("#E6E6FA","#D8BFD8","#BA55D3","#800080"))
  }
})

 

output$map_region <- highcharter::renderHighchart(
  
        highcharter::highchart(type = "map") %>% 
          highcharter::hc_chart(zoomType = "xy") %>%
          highcharter::hc_add_series_map(map = map, df = dfita1,
                                         joinBy = "id", value = input$map_value, name=custom_map()[[1]]) %>%
        highcharter::hc_colorAxis(
          stops = highcharter::color_stops(4,custom_map()[[2]]))
  
        )

  

# map by province ---------------------------------------------------------


output$map_province <- highcharter::renderHighchart(
  
    highcharter::highchart(type = "map") %>% 
      highcharter::hc_chart(zoomType = "xy") %>%
      highcharter::hc_add_series_map(map = ita, df = dfita2, 
                                     joinBy = "hasc", value = input$map_value, name=custom_map()[[1]] ) %>%
      highcharter::hc_colorAxis(
        stops = highcharter::color_stops(4,custom_map()[[2]]))
  
)


# tabbox ------------------------------------------------------------------

  output$tabset1Selected <- renderText({
    input$tabset1
  })


# modal dialog ------------------------------------------------------------

observeEvent(input$show, {
  showModal(modalDialog(
    title = NULL, easyClose = TRUE, size = "l",
    
        tags$head(tags$style(HTML('
 /* tabBox background */

 .nav-tabs-custom > .nav-tabs > li.active {
     border-top-color: red !important;
 }

 .btn-default {
    background-color: #dd4b39 !important;
    color: white !important;
    border-color: #dd4b39 !important;
}

                                  '))),

    shinydashboard::tabBox(
          width = 12,
          # height = "250px",
          title = NULL,
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1",
          tabPanel("By Province",
                   shinycssloaders::withSpinner(
                   highcharter::highchartOutput('map_province_modal',
                                                width = "100%",
                                                height = "800px"
                   ),
                   color="#dd4b39"
                   )
          ),
          tabPanel("By Region",
                   shinycssloaders::withSpinner(
                   highcharter::highchartOutput('map_region_modal',
                                                width = "100%",
                                                height = "800px"
                   ),
                   color="#dd4b39"
                   )
          )
        )
    
  ))
})
  