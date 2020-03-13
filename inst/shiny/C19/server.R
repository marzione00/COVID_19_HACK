## server.R ##

server <- function(input, output, session) {
  
  shinyjs::runjs('
        var el2 = document.querySelector(".skin-red");
        el2.className = "skin-red sidebar-mini";
        ')
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  # section 1 ---------------------------------------------------------------
  

  # section 2 ---------------------------------------------------------------

  waiter::waiter_hide()
  
  output$menu <- shinydashboard::renderMenu({
    
    shinydashboard::sidebarMenu(id="tabs",
                                shinydashboard::menuItem("Home", tabName = "tab_1", icon=icon("home")),
                                shinydashboard::menuItem("Inspection", tabName = "tab_2", icon=icon("search")),
                                shinydashboard::menuItem("Analysis", tabName = "tab_3", icon=icon("chart-line")),
                                shinydashboard::menuItem("Conclusion", tabName = "tab_4", icon=icon("calendar-check"))
    )
  })
  
  
  # animations
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_1', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_2', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_3', 'fadeIn')
  })
  observeEvent(input$tabs,{
    shinyanimate::startAnim(session, 'effect_4', 'fadeIn')
  })
  
}
