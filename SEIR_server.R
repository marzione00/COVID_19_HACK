server <- function(input, output){

	#---- data retrieval, common to both
	# X <- infected data
	# popX <- population of area
	# R <- hospitalised + dead 
	
	distr_IT <- 'exp'
	distr_SRT <- 'exp'
	distr_IBST <- 'none'
	
	par_IT <- list('rate'=1/input$rate_IT)
	par_SRT <- list('rate'=1/input$rate_SRT)
	par_IBST <- NULL
	
	IT <- generate(distr_IT, par_IT)
	SRT <- generate(distr_SRT, par_SRT)
	IBST <- generate(distr_IBST, par_IBST)

	m <- max(length(IT), length(SRT), length(IBST))
	end <- length(X) #Rt goes from day 1 to day n_obs-m BUT also the SEIR data series will go from day 1 to that day after refinement
	
	#---- R0 part
	
	observeEvent(input$est_R0, {
		
		out <- SEIR_factotum(X, R, N, 
					end=end, future=input$future,
					distr_IT=distr_IT, distr_SRT=distr_SRT, distr_IBST=distr_IBST,
					par_IT=par_IT, par_SRT=par_SRT, par_IBST=par_IBST, 
					R0_exp_est_end=input$R0_exp_est_end,
					plot_data=input$plot_data)
		
		R0 <- out$R0
	
		S_<-out$S_; E_<-out$E_; I_<-out$I_; R_<-out$R_
		S<-out$S; E<-out$E; I<-out$I; R<-out$R
		
		U <- list('S'=S, 'E'=E, 'I'=I, 'R'=R)
		U_ <- list('S_'=S_, 'E_'=E_, 'I_'=I_, 'R_'=R_)
		
		# if(input$plot_data <- TRUE)
		# 	output$SEIR_plot <- plotly::plot(U_ & U on time)
		# else
		# 	output$SEIR_plot <- plotly::plot(U_ on time)
		
		output$SEIR_R0 <- R0
		
	})
	
	
	observeEvent(input$est_Rt, {
		
		n_obs <- end
		len_data_series <- n_obs-m
		
		# REFERS TO INPUT IN PRECEDENT TAB
		
		GT<-R0::generation.time("gamma", input$"Gamma_1", input$"Gamma_2")
		R0_out <- R0::est.R0.SB(P,GT,begin=1, end=len_data_series)
		Rt <- R0_out$R
		R0_stages <- (1:(length(Rt)-1))
			
		out <- SEIR_factotum(X, R, N, 
							end=end, future=input$future,
							distr_IT=distr_IT, distr_SRT=distr_SRT, distr_IBST=distr_IBST,
							par_IT=par_IT, par_SRT=par_SRT, par_IBST=par_IBST, 
							R0_msp=Rt, R0_stages=R0_stages, 
							plot_data=input$plot_data)

		S_<-out$S_; E_<-out$E_; I_<-out$I_; R_<-out$R_
		S<-out$S; E<-out$E; I<-out$I; R<-out$R
		
		U <- list('S'=S, 'E'=E, 'I'=I, 'R'=R)
		U_ <- list('S_'=S_, 'E_'=E_, 'I_'=I_, 'R_'=R_)
		
		# if(input$plot_data <- TRUE)
		# 	output$SEIR_plot <- plotly::plot(U_ & U on time)
		# else
		# 	output$SEIR_plot <- plotly::plot(U_ on time)
		
		
	})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}