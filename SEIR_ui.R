library(shiny)
library(covid19)

ui <- fluidPage(
	
	fluidRow(
	
		column(
			4,
			shinydashboard::box(
				width = 12,
				status = "danger",
				solidHeader = TRUE,
				title = "Input",
				shiny::sliderInput(inputId = "rate_IT", label = "mean incubation time",
					min = 0, max = 10, step = 0.1, value = 5.2),
				shiny::sliderInput(inputId = "rate_SRT", label = "mean time from symptoms to recovery",
					min = 0, max = 10, step = 0.1, value=round(log(2)/5) ,1),
				shiny::actionButton(inputId = "est_R0", "If epidemic continued as in initial stages"), 
				shiny::actionButton(inputId = "est_Rt", "all times Rt. Refer to previous tab for Rt parameters choice"),	
				shiny::sliderInput(inputId = "R0_exp_est_end", label = "If estimating initial R0, number of days to estimate",
					min = 1, max = 20, step = 1, value = 5),
				shiny::sliderInput(inputId = "future", label = "How many days in the future do you wanna go?",
					min = 0, max = 100, step = 1, value = 0),
				shiny::checkboxInput(inputId = "plot_data", label = "Are true data points to be shown on plot?"),
				
				if(!is.null(output$SEIR_R0))
					shiny::verbatimTextOutput(output$SEIR_R0)
				
			),

		),
		
		column(
			8,
			shinydashboard::box(
				color = "red",
				status = "danger",
				title = "SEIR",
				shiny::plotOutput("SEIR_plot") %>% shinycssloaders::withSpinner(color = "#dd4b39"),
				width = 12
			)
		)
	
	), 
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
)

