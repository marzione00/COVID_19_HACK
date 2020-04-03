

SEIR_factotum <- function(P, R, N, gamma=0.1, time_step=1, normalise=TRUE, IT=c(0,0,0,0,0,1), TT=c(1), IBST=c(1)) {
	#### Functions ####
	# Extract S, E, I, R from P, R. population is normalised to N=1
	refine_data <- function(P, R, normalise=TRUE) {
	
		n_obs = length(P)
		m = max(length(IT), length(TT), length(IBST))
		ITl = length(IT)
		TTl = length(TT)
		IBSTl = length(IBST)
		
		S = integer(n_obs-m)
		E = integer(n_obs-m)
		I = integer(n_obs-m)
		R = R[1:(n_obs-m)]
		
		for(t in (1:(n_obs-m))){
			IT_temp = c(IT, integer(n_obs-ITl-t+1))
			TT_temp = c(TT, integer(n_obs-TTl-t+1))
			IBST_temp = c(IBST, integer(n_obs-IBSTl-t+1))
			
			E[t] <- ((IT_temp+TT_temp)/2) %*% P[t:n_obs]
			I[t] <- ((IBST_temp+TT_temp)/2) %*% P[t:n_obs]
		}
		
		if(normalise){
			S = (N-E-I-R)/N
			E = E/N
			I = I/N
			R = R/N
		}
		
		return(data.frame(S, E, I, R))
	
	}
	# Estimate R0 using I and its exponential properties
	estimate_R0 <- function(sigma, gamma, I){
		# Find the period of exponential growth for X
		exp_period <- function(X){
			start <- 1; end <- length(X)
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
		
		# Operate
		period<-exp_period(I); start<-period[1]; end<-period[2]
		
		t <- (start:end)
		I <- I[t]
		R0 = optimise(sqdist_log, sigma=sigma, gamma=gamma, I=I, interval=c(0,20))$minimum
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
	
	# Extract S, E, I, R from P, R
	U<-refine_data(P, R, normalise=normalise); S<-U$S; E<-U$E; I<-U$I; R<-U$R
	
	
	# Estimate R0 using I and its exponential properties
	sigma <- dot((0:length(IT)-1), IT)
	
	R0 <- estimate_R0(sigma, gamma, I)
	
	# Solve the SEIR with parameters sigma gamma beta=R0*gamma
	U0 <- as.numeric(U[1,])
	beta <- R0*gamma
	eval_time <- seq(0, length(U$I)-1, by = time_step)
	parameters <- c(beta, sigma, gamma)
	sol<-solve_SEIR(U0, eval_time, parameters)
	
	# Return sol = SEIR, S_E_I_R_, time, R0
	out <- c(U, sol, R0)
	names(out)[10] <- 'R0'
	
	return(out)
	
}




