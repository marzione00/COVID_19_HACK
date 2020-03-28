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
          shiny::column(8,
        shinydashboard::box(width = 12,
                            status= "danger",
                            solidHeader = FALSE,
                            title = "Authors",
                           
                            shinydashboardPlus::userList(
                              shinydashboardPlus::userListItem(
                              src="https://avatars1.githubusercontent.com/u/36515398?s=460&u=152624d5b1720f1e66ffd3fbd43c6e13704c3e42&v=4",
                              user_name = "Andrea Ierardi",
                              description =HTML('<br> MSc. Data Science and Economics -Universita degli studi di Milano <br>  BC. in Computer Science Universita del Piemonte orientale(UPO) <br><br> 
                                                       <a href="https://www.linkedin.com/in/andreaierardi/" rel="nofollow noreferrer">
                                                       <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn</a> &nbsp; 
                                                       <a href="https://github.com/Andreaierardi" rel="nofollow noreferrer">
                                                      <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github</a> <br> 
                                                      <a href="https://ierardiandrea.com/" rel="nofollow noreferrer">
                                                      <i class="fas fa-cloud"></i> Website  </a> ')
                              ),
                              
                              shinydashboardPlus::userListItem(
                                src="https://avatars3.githubusercontent.com/u/59971032?s=400&u=38395af942e20f0c9a395b2b30b12763a39ca8ac&v=4",
                                user_name = "Gregorio Saporito",
                                description =HTML('<br><a href="https://www.linkedin.com/in/greg-saporito/" rel="nofollow noreferrer">
                                                       <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn</a>  &nbsp;
                                                       <a href="https://github.com/gregorio-saporito" rel="nofollow noreferrer"> 
                                                       <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; '
                              )),
                                shinydashboardPlus::userListItem(
                                  src="https://avatars3.githubusercontent.com/u/60003802?s=460&u=6463f4db28ebb1eaf463f117093a803af1b9cabc&v=4",
                                  user_name = "Marzio De Corato",
                                  description =HTML('<br>M.Sc. Data Science and Economics student (UNIMI) <br> Ph.D in Physics and Nanoscience (UNIMORE), M.Sc. in Physics (UNIMIB) (theoretical-computational solid state physics)<br><br>
                                                     <a href="https://www.linkedin.com/in/marzio-de-corato-2351a44b/" rel="nofollow noreferrer">
                                  <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn </a>  &nbsp;
                                  <a href="https://github.com/marzione00" rel="nofollow noreferrer"> 
                                  <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
                                                    
                                                    ')
                                  )
                                ),
                            br(),
                          
                                          shinydashboardPlus::userList(
                                  shinydashboardPlus::userListItem(
                                    src="https://media-exp1.licdn.com/dms/image/C4D03AQFNxDX9vdxEmQ/profile-displayphoto-shrink_800_800/0?e=1590019200&v=beta&t=ftXpX3Wg-dB2t8sBoSYspTKC3z5cK2ehYhWk5obIBh4",
                                    user_name = "Fabio Caironi",
                                    description = HTML('<br>
                                    BC. in Mathematics. MSc. student of Data Science and Economics Piano student at Milans Conservatorio G. Verdi
                                                       <br><br>
                                                      <a href="https://www.linkedin.com/in/fabio-caironi-8361091a2/" rel="nofollow noreferrer">
                                                      <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn</a>  &nbsp;
                                                      <a href="https://github.com/fabio130497" rel="nofollow noreferrer"> 
                                                      <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp; 
                                                       '
                                                       
                                    )),
                               
                              
                              
                              shinydashboardPlus::userListItem(
                                src="https://avatars0.githubusercontent.com/u/59998291?s=460&v=4",
                                user_name = "Federico Matteucci",
                                description = HTML (' <br><a href="https://github.com/De-Rham-Cohomology" rel="nofollow noreferrer"> 
                                                       <img src="https://i.stack.imgur.com/tskMh.png" alt="github"> Github </a> &nbsp;')
                              ),
                                          )
                              )
                            ),
        shiny::column(4,
        shinydashboard::box(width = 12,
                            status= "danger",
                            solidHeader = FALSE,
                            title = "Data source",
        )
                            
        )
        
        ),
        
        br(),
        
        shinydashboard::valueBox(
          "Documentation",
          "A navigator within the pandemia",
          icon = icon("chart-bar"),
          color = "navy",
          width = NULL
        ),
        
        shiny::fluidRow(
          shiny::column(8,
        shinydashboard::box(width = 12,
                            status= "danger",
                            solidHeader = FALSE,
                            title = "Documentation",
                            br(),
                            h3("To be completed.."),
                            hr(),
                            
        tags$iframe(style="height:600px; width:100%", src="C:/Users/AndreDany/Desktop/GitHub/COVID_19_HACK/inst/shiny/C19/documentation/documentation_29032020.pdf")
))
        )
  
)