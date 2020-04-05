# default value of std_IT is arbitrary
# consdering the infectious period to be from symptoms to test, ASSUMING: test and hospitalisation occur i
# on the same day & mean of infectious period is the same as its median: gamma is 1/5. also, gamma is 1/mean(IBST+SRT)
# ie the length of the span of time in which someone is infective
# https://www.epicentro.iss.it/coronavirus/sars-cov-2-decessi-italia

# SRT symptoms to removal (hospitalisation) time

SEIR_factotum <- function(P, R, N, time_step=1, normalise=TRUE, 
					distr_IT='norm', distr_SRT='norm', distr_IBST='norm',
					par_IT=list('mean'=6, 'std'=1), par_SRT=list('mean'=5, 'std'=1), par_IBST=list('mean'=1, 'std'=1), 
					R0_time_start, R0_time_end) {
	#### Functions ####
	# Generate distribution without outliers (Q3+1.5(IQR))
	generate <- function(distr, parameters){
		if(distr=='norm'){
			mean <- parameters$mean
			std <- parameters$std
			sup <- floor(mean+3.4*std)
			out <- dnorm(seq(0,sup), mean, std)
		}
		else if(distr=='exp'){
			rate <- parameters$rate
			sup <- floor(log(4)/rate + 1.5*log(3)/rate)
			out <- dexp(seq(0, sup), rate)
		}
		else{
			out <- NULL
		}
		
		return(out)
	}
	# Extract S, E, I, R from P, R. population is normalised to N=1
	refine_data <- function(P, R, IT, SRT, IBST, normalise=TRUE) {
	
		n_obs = length(P)
		m = max(length(IT), length(SRT), length(IBST))
		ITl = length(IT)
		SRTl = length(SRT)
		IBSTl = length(IBST)
		
		S = integer(n_obs-m)
		E = integer(n_obs-m)
		I = integer(n_obs-m)
		R = R[1:(n_obs-m)]
		
		for(t in (1:(n_obs-m))){
			IT_temp = c(IT, integer(n_obs-ITl-t+1))
			SRT_temp = c(SRT, integer(n_obs-SRTl-t+1))
			IBST_temp = c(IBST, integer(n_obs-IBSTl-t+1))
			
			E[t] <- ((IT_temp+SRT_temp)/2) %*% P[t:n_obs]
			I[t] <- ((IBST_temp+SRT_temp)/2) %*% P[t:n_obs]
		}
		
		S = N-E-I-R
		
		if(normalise){
			S = S/N
			E = E/N
			I = I/N
			R = R/N
		}
		
		return(data.frame(S, E, I, R))
	
	}
	# Calculate sigma and gamma
	calculate_sigma_gamma <- function(IT, SRT, IBST){
		#### Functions ####
		expected_value <- function(X){
			out <- (1:length(X)-1) %*% X
	 		return(out)
		}
		
		#### Operations ####
		sigma <- 1/expected_value(IT)
		gamma <- 1/(expected_value(SRT)+expected_value(IBST))
		
		out <- list('sigma'=sigma, 'gamma'=gamma)
	
		return(out)	
	}
	# Estimate R0 using I and its exponential properties
	estimate_R0 <- function(sigma, gamma, I, time_start, time_end){
		#### Functions ####
		
		# Find the period of exponential growth for X
		exp_period <- function(X){
			start <- 1
			for(t in (3:length(X)-1)){
				end <- t
			}
			period <- c(start, end)
			return(period)
		}
		# Define the exponential approximation of I 
		I_approx <- function(R0, sigma, gamma, I0, t) {
	
			out <- I0*exp(1/2*(sqrt(4*(R0-1)*sigma*gamma+(sigma+gamma)^2)-(sigma+gamma))*t)
	
			return(out) 
	
		}
		# Define the target function, ie a least square estimation for log(I)
		sqdist_log <- function(R0, sigma, gamma, I){
	
			out = 0
			for(t in (1:length(I)))
				out = out + (log(I_approx(R0, sigma, gamma, I[1],t-1))-log(I[t]))^2
			
			return(out)
		}
		
		#### Operations ####
		if(missing(time_start) & missing(time_end)) {
			period<-exp_period(I); start<-period[1]; end<-period[2]
		}	
		else if(missing(time_start)) {
			start <- 1
			end <- time_end
		}	
		else if(missing(time_end)) {
			start <- time_start
			end <- length(I)
		}	
		else {
			start<-time_start; end<-time_end
		}

		t <- (start:end)
		
		I <- I[t]
		R0 = optimise(sqdist_log, sigma=sigma, gamma=gamma, I=I, interval=c(0,20))$minimum
		
		out <- list('start'=start, 'end'=end, 'R0'=R0)

		return(out)
	}
	# SEIR differential equations
	SEIR_model <- function(t, state, parameters){
		with(as.list(c(state, parameters)), {
			dS <- beta*S*I
			dE <- beta*S*I-sigma*E
			dI <- sigma*E-gamma*I
			dR <- gamma*I
		
			list(c(dS, dE, dI, dR))
		})
	}
	# Solve SEIR
	solve_SEIR <- function(U0, eval_time, parameters){
		state <- c(S=U0[1], E=U0[2], I=U0[3], R=U0[4])
		
		out <- deSolve::ode(y=state, times=eval_time, func=SEIR_model, parms=parameters)
		out <- data.frame(out)
		colnames(out) <- c('time', 'S_', 'E_', 'I_', 'R_')
		
		return(out)
	}
	
	####Operations ####
	# Generate distribution without outliers (Q3+1.5(IQR))
	IT <- generate(distr_IT, par_IT)
	SRT <- generate(distr_SRT, par_SRT)
	IBST <- generate(distr_IBST, par_IBST)
	
	# Calculate sigma = 1/E[IT]. Calculate gamma = 1/E[IBST+SRT]
	sg_out<-calculate_sigma_gamma(IT, SRT, IBST); sigma<-sg_out$sigma; gamma<-sg_out$gamma
	# Extract S, E, I, R from P, R
	U<-refine_data(P, R, IT=IT, SRT=SRT, IBST=IBST, normalise=normalise); S<-U$S; E<-U$E; I<-U$I; R<-U$R
	# Estimate R0 using I and its exponential properties
	R0_out <- estimate_R0(sigma, gamma, I, R0_time_start, R0_time_end); R0<-R0_out$R0
	# Solve the SEIR with parameters sigma gamma beta=R0*gamma
	U0 <- as.numeric(U[1,])
	beta <- R0*gamma
	eval_time <- seq(0, length(U$I)-1, by = time_step)
	parameters <- c(beta=beta, sigma=sigma, gamma=gamma)

	sol<-solve_SEIR(U0, eval_time, parameters)
	
	# Returns SEIR, S_E_I_R_, time, R0, start (R0_time_start), end (R0_time_end)
	out <- c(U, sol, R0_out)
	return(out)
}








