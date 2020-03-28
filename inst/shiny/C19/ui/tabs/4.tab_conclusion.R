shinydashboard::tabItem(tabName = "tab_4",
        
 
        shinydashboard::valueBox(
          "Documentation",
          "Authors, data source and documentation",
          icon = icon("chart-bar"),
          color = "red",
          width = NULL
        ),
        
        
        shinydashboard::valueBox(
          "Authors",
          "Information",
          icon = icon("chart-bar"),
          color = "navy",
          width = NULL
        ),
        
        
        shiny::fluidRow(
          shiny::column(6,
        shinydashboard::box(width = 12,
                            status= "danger",
                            solidHeader = FALSE,
                            title = "Authors",
                           
                            shinydashboardPlus::userList(
                              shinydashboardPlus::userListItem(
                              src="https://avatars1.githubusercontent.com/u/36515398?s=460&u=152624d5b1720f1e66ffd3fbd43c6e13704c3e42&v=4",
                              user_name = "Andrea Ierardi",
                              description ="MSc. Data Science and Economics -Universita' degli studi di Milano        Graduated in Computer Science Universita' del Piemonte orientale(UPO)"
                              ),
                              shinydashboardPlus::userListItem(
                                src="https://avatars3.githubusercontent.com/u/59971032?s=400&u=38395af942e20f0c9a395b2b30b12763a39ca8ac&v=4",
                                user_name = "Gregorio Saporito",
                                description ="Description ...."
                              ),
                                shinydashboardPlus::userListItem(
                                  src="https://avatars3.githubusercontent.com/u/60003802?s=460&u=6463f4db28ebb1eaf463f117093a803af1b9cabc&v=4",
                                  user_name = "Marzio De Corato",
                                  description =paste0("M.Sc. Data Science and Economics student (UNIMI)       Ph.D in Physics and Nanoscience (UNIMO), M.Sc. in Physics (UNIMIB) (theoretical-computational solid state physics)")
                                )),
                            br(),
                          
                                          shinydashboardPlus::userList(
                                  shinydashboardPlus::userListItem(
                                    src="https://media-exp1.licdn.com/dms/image/C4D03AQFNxDX9vdxEmQ/profile-displayphoto-shrink_800_800/0?e=1590019200&v=beta&t=ftXpX3Wg-dB2t8sBoSYspTKC3z5cK2ehYhWk5obIBh4",
                                    user_name = "Fabio Caironi",
                                    description ="BC. in Mathematics. MSc. student of Data Science and Economics Piano student at Milan's Conservatorio G. Verdi"
                                    ),
                               
                              
                              
                              shinydashboardPlus::userListItem(
                                src="https://avatars0.githubusercontent.com/u/59998291?s=460&v=4",
                                user_name = "Federico Matteucci",
                                description ="Description ...."
                              ),
                                          )
                              )
                            ),
        shiny::column(6,
        shinydashboard::box(width = 12,
                            status= "danger",
                            solidHeader = FALSE,
                            title = "Data source",
        )
                            
        )
        
        ),
        
        br(),
        
        shinydashboard::valueBox(
          "Authors",
          "Information",
          icon = icon("chart-bar"),
          color = "navy",
          width = NULL
        ),
        
                            
  
)