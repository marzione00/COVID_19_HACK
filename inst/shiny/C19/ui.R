## ui.R ##

if(interactive()){
  
  
  ui <- shinydashboard::dashboardPage(skin = "red", title = "Covid-19",
    
    shinydashboard::dashboardHeader(title = img(src = "coronavirus_white.png")),
    
    shinydashboard::dashboardSidebar(collapsed = T,
      
      shinydashboard::sidebarMenuOutput("menu")
      
    ),
    
    shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      
      # css
      tags$head(
        
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        
        tags$link(rel = "shortcut icon",
                  href = "coronavirus_black.png")
      ),
      
      waiter::use_waiter(),
      waiter::waiter_show_on_load(html = waiter::spin_flower()), # will show on load
      
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "tab_1",
                shinyanimate::withAnim(),
                tags$div(id = 'effect_1',
                         
                         HTML("
                   <h1>Coronavirus Outbreak Analysis<br>")
                         
                         )
        ),
        
        
        shinydashboard::tabItem(tabName = "tab_2",
                shinyanimate::withAnim(),
                tags$div(id = 'effect_2',
                         HTML("<h2>Data Inspection</h2>")
                )
        ),
        
        shinydashboard::tabItem(tabName = "tab_3",
                shinyanimate::withAnim(),
                tags$div(id = 'effect_3',
                         HTML("
                   <h2>Test</h2>
                   ")
                )
        ),
        shinydashboard::tabItem(tabName = "tab_4",
                shinyanimate::withAnim(),
                tags$div(id = 'effect_4',
                         
                         HTML("
                    <h2>Conclusions</h2>
                   ")
                )
                
        )
      )
    )
  )
  
}
