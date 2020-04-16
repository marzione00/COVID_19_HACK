# Solves a normalised (population=1) simplified SEIR system (no immunity loss, no mortality): 
# 
# 1_ extract the S, E, I, R, information from the input data P (positive) and R using the distributions IT, SRT, IBST. #
#	The default distribution for IT is an exponential with rate 1/5.2 (5.2 being the mean Incubation Time)
#		https://www.nejm.org/doi/full/10.1056/NEJMoa2001316
#	The default distribution for SRT is an exponential with rate log(2)/5 (5 being the median time from symptomaticity to removal - hospitalisation)
#		
#	The default distribution for IBST is an none as no data is available
#		
# 2_ parameters sigma and gamma are estimated as 1/mean(IT) and 1/mean(SRT) resp

# 3_a if the user inputs a valid Rt (consisting of R0_msp and R0_stagesof length RN and RN-1 resp), SEIR_factotum will solve the multi-stage SEIR with 
#	parameters (at n-th stage) sigma, gamma, beta[n]=Rt[n]*gamma. It will return U and sol, U being the refined data and sol being 
#	the solution of the SEIR equation. 

# 3_b if no valid Rt is passed, SEIR_factotum will estimate an initial R0 using asymptotic properties of the series I in the early stages
#	(from R0_exp_est_start to R0_exp_est_end, meaning R0 exponential estimate start/end), 
#	then it will solve a single stage SEIR system with parameters sigma, gamma, beta=R0*gamma. 
#	This can be used to show how the growth would have been like if no measures had taken place.
#	It will return U and sol as above, plus result of the estimation of R0

####

# Example1: solve the SEIR for 10 days in the future, use normal distribution for IBST, 
# 			estimate the initial R0 up to day 4. 
# 			Retrieve the real data U, the solution sol, the estimated R0, info about R0. 
# 	
# 	out <- SEIR_factotum(P,R,N, future=10, distr_IBST='norm', par_IBST=list('mean'=2, 'std'=1), 
# 				R0_exp_est_end=4)
# 
# 	S<-out$S; E<-out$E; I<-out$I; R<-out$R
# 	S_<-out$S_; E_<-out$E_; I_<-out$I_; R_<-out$R_
# 	U <- list('S'=S, 'E'=E, 'I'=I, 'R'=R)
# 	sol <- list('S_'=S_, 'E_'=E_, 'I_'=I_, 'R_'=R_)
# 	
# 	R0<-out$R0; R0_time_start<-out$start; R0_time_end<-out$end; R0_fitting<-out$R0_fitting
# 	
# Example2: solve the SEIR up to data availability (that is, the length of the refined (S,E,I,R) 
# 		time series, in general less than the length of input time series P and R), 
# 		with one time fixed parameter R0_input (please note that R0_stages is required).
# 		Retrieve U and sol as above. 
# 
# 	out <- SEIR_factotum(P,R,N, future=0, R0_msp=R0_input, R0_stages=c())
# 	
# 	S<-out$S; E<-out$E; I<-out$I; R<-out$R
# 	S_<-out$S_; E_<-out$E_; I_<-out$I_; R_<-out$R_
# 	U <- list('S'=S, 'E'=E, 'I'=I, 'R'=R)
# 	sol <- list('S_'=S_, 'E_'=E_, 'I_'=I_, 'R_'=R_)
# 	
# Example3: solve the SEIR up to day 45 (20 S,E,I,R observations + 25 = future), 
# 		with a time varying parameter Rt, 
# 		Rt = 	8 from day 0 to day 5, 
# 				2 from day 5 to day 10, 
# 				4 from day 10 to day 20 (or in general up to the last observation)
# 				4 from day 20 to day 45 (the last value is used from day end to day end+future)
# 		Retrieve U and sol. 					
# 	
# 	Rt = c(8,2,4)
# 	R0_stages = c(5,10)
# 	
# 	out <- SEIR_factotum(P,R,N, future=25, end=20, R0_msp=Rt, R0_stages=c())
# 	
# 	S<-out$S; E<-out$E; I<-out$I; R<-out$R
# 	S_<-out$S_; E_<-out$E_; I_<-out$I_; R_<-out$R_
# 	U <- list('S'=S, 'E'=E, 'I'=I, 'R'=R)
# 	sol <- list('S_'=S_, 'E_'=E_, 'I_'=I_, 'R_'=R_)
# 	
	
	

SEIR_factotum <- function(P, R, N, normalise=TRUE, 
					time_step=1, end, future=0,
					distr_IT='exp', distr_SRT='exp', distr_IBST='none',
					par_IT=list('rate'=1/5.2), par_SRT=list('rate'=log(2)/5), par_IBST=list(), 
					R0_exp_est_start=1, R0_exp_est_end=5, R0_msp, R0_stages) {
	#### Functions ####
	# Generate distribution without outliers (Q3+1.5(IQR))
	generate <- function(distr, parameters){
		if(distr=='norm' & !is.null(parameters$mean) & !is.null(parameters$std)){
			mean <- parameters$mean
			std <- parameters$std
			sup <- floor(mean+3.4*std)
			out <- dnorm(seq(0,sup), mean, std)
		}
		else if(distr=='exp' & !is.null(parameters$rate)){
			rate <- parameters$rate
			sup <- floor(log(4)/rate + 1.5*log(3)/rate)
			out <- dexp(seq(0, sup), rate)
		}
		else if(distr=='none'){
			out <- c(1)
		}
		else {
			out <- NULL
		}
		return(out)
	}
	# Extract S, E, I, R from P, R. population is normalised to N=1
	refine_data <- function(P, R, IT, SRT, IBST, normalise=TRUE, end) {
	
		if(!missing(end))
			if(end<length(P))
				n_obs <- end
			else
				n_obs <- end
		else
			n_obs <- length(P)
		
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
			out <- c(1:(length(X))) %*% X
			out <- as.numeric(out)
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
			for(t in (1:(length(X)-1))){
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
		opt <- optimise(sqdist_log, sigma=sigma, gamma=gamma, I=I, interval=c(0,20))
		R0 <- opt$minimum
		mf <- opt$objective/(end-start+1)
		
		out <- list('start'=start, 'end'=end, 'R0'=R0, 'R0_fitting'=mf)

		return(out)
	}
	# SEIR differential equations
	SEIR_model <- function(t, state, parameters){
		with(as.list(c(state, parameters)), {
			dS <- -beta*S*I
			dE <- beta*S*I-sigma*E
			dI <- sigma*E-gamma*I
			dR <- gamma*I
		
			list(c(dS, dE, dI, dR))
		})
	}
	# solve ODE with variable parameters
	multi_stage_ODE <- function(U0, func, var_names, par_fixed, msp, msp_name, L, time_step){
		N <- length(L) # number of stages
		parms <- data.frame(par_fixed, msp) # parms[n, ] are the parameter to be used at stage n
		names(parms)[length(names(parms))] <- msp_name # msp will be referred with the passed name
		total <- data.frame() # solution from 0 to L[n]
		
		# at step n: 
		# find the interval of interest for deSolve::ode (eval_time) 
		# polish U0 in order to use it in further calculations
		# solve (again) the ode with new_parms as parameters of SEIR_model, polish solution
		# define initial data of stage n+1
		# drop the last element of solution n
		# save solution as stage n in total
		
		for(n in (1:N)){
			if(n==1)
				eval_time <- seq(0, L[n], by=time_step)
			else
				eval_time <- seq(0, L[n]-L[n-1], by=time_step) 
			
			U0 <- as.numeric(U0)
			names(U0) <- var_names # so that deSolve::ode can use appropriately the parameters
			
			sol <- deSolve::ode(y=U0, times=eval_time, func=func, parms=parms[n,])
			sol <- data.frame(sol)	
			sol <- sol[!(names(sol)=='time')] # drop the time column
			
			# initial data of next stage
			if(n<N)
				if(n==1)
					U0 <- sol[L[n]+1,] 
				else	
					U0 <- sol[L[n]-L[n-1]+1,]
			
			# drop the last value in sol, as it is U0 for next stage. Avoid repetitions
			# if n==N, the last values should be dropped as well to have length(S)==length(S_)
			#
			if(n==1)
				total <- sol[-length(sol[[1]]),]
			else {
				sol <- sol[-length(sol[[1]]),] 
				total <- rbind(total, sol) # append the new solution
			}	
		}
	
		rownames(total) <- 1:nrow(total)
		time <- seq(0, L[N]-1, by=time_step) # the evaluation times for total
		out <- list('sol'=total, 'time'=time)
		
		return(out)
	}
	# Solve SEIR
	solve_SEIR <- function(U0, eval_time, parameters){
		state <- c('S'=U0[1], 'E'=U0[2], 'I'=U0[3], 'R'=U0[4])
		
		out <- deSolve::ode(y=state, times=eval_time, func=SEIR_model, parms=parameters)
		out <- data.frame(out)
		colnames(out) <- c('time', 'S_', 'E_', 'I_', 'R_')
		
		return(out)
	}
	# Solve multi stage SEIR
	solve_ms_SEIR <- function(U0, gamma, sigma, R0_msp, R0_stages, time_step){
		var_names <- list('S', 'E', 'I', 'R')
		par_fixed <- list('sigma'=sigma, 'gamma'=gamma)
		msp <- R0_msp*gamma
		L <- R0_stages
		
		ode <- multi_stage_ODE(U0, SEIR_model, var_names, par_fixed, msp, 'beta', L, time_step)
		sol <- ode$sol; time <- ode$time
		S_<-sol$S; E_<-sol$E; I_<-sol$I; R_<-sol$R

		out <- data.frame(time, S_, E_, I_, R_)
		colnames(out) <- c('time', 'S_', 'E_', 'I_', 'R_')
		
		return(out)
	}
	
	####Operations ####
	
	# Generate distribution without outliers (Q3+1.5(IQR))
	if(is.null(IT <- generate(distr_IT, par_IT)))	return('ERROR in IT generation')
	if(is.null(SRT <- generate(distr_SRT, par_SRT))) return('ERROR in SRT generation')
	if(is.null(IBST <- generate(distr_IBST, par_IBST))) return('ERROR in IBST generation')
	
	# Calculate sigma = 1/E[IT]. Calculate gamma = 1/E[IBST+SRT]
	sg_out<-calculate_sigma_gamma(IT, SRT, IBST); sigma<-sg_out$sigma; gamma<-sg_out$gamma
	
	# Extract S, E, I, R from P, R. Update R0_stages with length(S) if necessary
	U<-refine_data(P, R, IT=IT, SRT=SRT, IBST=IBST, end=end, normalise=normalise); S<-U$S; E<-U$E; I<-U$I; R<-U$R
	
	if(!missing(R0_msp) & !missing(R0_stages))
		R0_stages <- c(R0_stages, length(S))
		
	# Initial conditions 
	U0 <- as.numeric(U[1,])
	
	# Solve SEIR with parameters input by user (R0_msp is not missing)
	# please note that now R0_msp and R0_stages (should) have the same length
	
	if(!missing(R0_msp) & !missing(R0_stages)){
		if(length(R0_msp)==length(R0_stages))
			N <- length(R0_msp) 
		else 
			return(NULL)
		
		# add future stage (from Length(S) to future+Length(S))
		if(future>0){
			# add one more stage to Rt,  from R0_stages[N] to R0_stages[N]+future
			R0_msp[N+1] <- R0_msp[N]
			used_time <- c(R0_stages, (future+R0_stages[N]))
		}
		else
			used_time <- R0_stages
			
		sol <- solve_ms_SEIR(U0, gamma, sigma, R0_msp, used_time, time_step)
		
		out <- c(U, sol)
	}
	# Solve the SEIR with parameters sigma gamma beta=R0*gamma
	else {
		
		R0_out <- estimate_R0(sigma, gamma, I, R0_exp_est_start, R0_exp_est_end); R0<-R0_out$R0
		
		beta <- R0*gamma
		eval_time <- seq(0, (future+length(U$I)-1), by = time_step)
		parameters <- c(beta=beta, sigma=sigma, gamma=gamma)

		sol <- solve_SEIR(U0, eval_time, parameters)
		
		# Returns SEIR, S_E_I_R_, time, R0, start (R0_time_start), end (R0_time_end), mean sq dist (mean_fitting)
		out <- c(U, sol, R0_out)
		
	}
	
	return(out)
}








