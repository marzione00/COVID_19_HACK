server <- function(input, output, session) {
  matrix  <- read.csv(system.file("extdata", "COVID_dat.csv", package="CovidPack"))
  colnames(matrix) <- c("Date","Cases","Cases_Lombardy","Cases_Milan","Cases_Lodi","Cases_Bergamo")

  date <- as.numeric(matrix[,"Date"])
  cases <- as.numeric(matrix[,"Cases"])
  cases_Lombardy <- as.numeric(matrix[,"Cases_Lombardy"])
  cases_Milan  <- as.numeric(matrix[,"Cases_Milan"])
  cases_Lodi  <- as.numeric(matrix[,"Cases_Lodi"])
  cases_Bergamo  <- as.numeric(matrix[,"Cases_Bergamo"])

  Distribution_cases<-data.frame(date,cases)
  Distribution_cases_Lombardy<-data.frame(date,cases_Lombardy)
  Distribution_cases_Milan<-data.frame(date,cases_Milan)
  Distribution_cases_Lodi<-data.frame(date,cases_Lodi)
  Distribution_cases_Bergamo<-data.frame(date,cases_Bergamo)

  Days <- (1:50)
  #Days_dat <- (1:24)

  Total <- nls(cases ~ (alpha/(beta * exp(gamma * date) +1 )),start = list(alpha=9000,beta=1700,gamma=-0.36))
  Lombardy <- nls(cases_Lombardy ~(alpha1/(beta1 * exp(gamma1 * date) +1 )),start = list(alpha1=9000,beta1=1700,gamma1=-0.36))
  Milan <- nls(cases_Milan ~ (alpha2/(beta2 * exp(gamma2 * date) +1 )),start = list(alpha2=9000,beta2=1700,gamma2=-0.36))
  Lodi <- nls(cases_Lodi ~ (alpha3/(beta3 * exp(gamma3 * date) +1 )),start = list(alpha3=9000,beta3=1700,gamma3=-0.36))
  Bergamo <- nls(cases_Bergamo ~ (alpha4/(beta4 * exp(gamma4 * date) +1 )),start = list(alpha4=9000,beta4=1700,gamma4=-0.36))


  conf<-nlstools::confint2(level = 0.95,Total)

  coeff<-coef(Total)
  coeff1<-coef(Lombardy)
  coeff2<-coef(Milan)
  coeff3<-coef(Lodi)
  coeff4<-coef(Bergamo)

  Total_cases<-coeff[1]/(coeff[2] * exp(coeff[3] * Days) +1 )
  Total_cases_Lombardy<-coeff1[1]/(coeff1[2] * exp(coeff1[3] * Days) +1 )
  Total_cases_Milan<-coeff2[1]/(coeff2[2] * exp(coeff2[3] * Days) +1 )
  Total_cases_Lodi<-coeff3[1]/(coeff3[2] * exp(coeff3[3] * Days) +1 )
  Total_cases_Bergamo<-coeff4[1]/(coeff4[2] * exp(coeff4[3] * Days) +1 )


  #Conf_UP<-conf[1,1]/(conf[2,1] * exp(conf[3,1] * Days_dat) +1 )
  #Conf_DOWN<-conf[1,2]/(conf[2,2] * exp(conf[3,2] * Days_dat) +1 )

  data_p <- data.frame(Days,Total_cases)
  data_p_lombardy <- data.frame(Days,Total_cases_Lombardy)
  data_p_Milan <-data.frame(Days,Total_cases_Milan)
  data_p_Lodi <-data.frame(Days,Total_cases_Lodi)
  data_p_Bergamo <-data.frame(Days,Total_cases_Bergamo)

  #data_u<-data.frame(Days_dat,Conf_UP)
  #data_u<-data.frame(Days_dat,Conf_DOWN)



text_out<-observe({
  input$Fit_output
})


  output$coolplot <- renderPlot( #plot of the data choosen by the user on the panels

    #    plot(matrix[,"BP"],matrix[,input$"Plot_input"],col = cor(),type = "l",xlab="Years BP",ylab=input$"Plot_input",cex.lab=1.3,lwd=size()),
    ggplot2::ggplot(data_p,ggplot2::aes_string("Days",input$"Plot_input")) +
      ggplot2::geom_line(color="darkblue",size=1)+
    ggplot2::geom_point(data=Distribution_cases,ggplot2::aes_string("date",input$Distribution),size=1,color="red")+
      #geom_line(data=data_u,aes_string("Days_dat","Conf_UP"),linetype = "dashed",size=1.05) +
      #geom_line(data=data_u,aes_string("Days_dat","Conf_DOWN"),linetype = "dashed",size=1.05) +



      ggplot2::theme_dark() +
      ggplot2::theme(legend.position="top",panel.grid.minor = ggplot2::element_line(),text = ggplot2::element_text(size=14)) +
      ggplot2::scale_x_continuous(breaks=seq(0, 50, 2))+
      ggplot2::scale_y_continuous(breaks=seq(0, 44000, 2000))+
      ggplot2::labs(title="COVID19")


  )
  output$coolplot5<- renderPrint(

    summary(Total)


  )
}
