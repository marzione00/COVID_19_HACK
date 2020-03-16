tabItem(tabName = "tab_3",
        shiny::navbarPage(title = "",
             shiny::tabPanel(title = "Country", shiny::uiOutput("countryPanel")),
             shiny::tabPanel(title = "Region", shiny::uiOutput("regionPanel"))
        )
)