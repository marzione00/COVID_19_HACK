ui <- shiny::fluidPage(
  shiny::titlePanel("COVID19 Cases"), #preparation of the panels for shiny app
  shiny::tabsetPanel(
    shiny::tabPanel(title = "Country", shiny::uiOutput("countryPanel")),
    shiny::tabPanel(title = "Region", shiny::uiOutput("regionPanel")),
    shiny::tabPanel(title = "Province", shiny::uiOutput("provincePanel"))
    )
)
