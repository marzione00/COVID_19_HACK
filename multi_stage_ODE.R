# solve an ODE with time-varying parameter msp (multi stage parameter) and time fixed parameters 
# parameters. 


# Input : 
# 	U0 : named list 
# 		initial conditions at time 0
# 		
# 	func : function
# 		given U array of variables, func(U;parameters) = dU/dt
# 	
# 	var_names : list of char
# 		the names of the variable as in func
# 	
# 	par_fixed : named list
# 		format is ('name'=value)
# 	
# 	msp : array like 
# 		contains all the values assumed by the time variable parameters
# 		
# 	msp_name : char
# 		name of the msp as appearing in func 
# 		
# 	L : array like
# 		contains the extreme of use of the components of msp
# 		msp[n] is the msp in use from L[n-1] to L[n]
#		the last component of L is the final point of integration 	
# 	
# 	time_step : float
# 		step of integration
# 		
# Output : 
# 	$sol : dataframe
# 		solution to ODE evaluated at $times
# 		
# 	$time : array like
# 		points of evaluation of solution
		
				
multi_stage_ODE <- function(U0, func, var_names, par_fixed, msp, msp_name, L, time_step){
	
	N <- length(L) # number of stages
	parms <- data.frame(par_fixed, msp) # parms[n, ] are the parameter to be used at stage n
	names(parms)[length(names(parms))] <- msp_name # msp will be referreed with the passed name
	total <- data.frame() # solution from 0 to L[n]
	
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
		if(n==1)
			total <- sol[-length(sol[[1]]),]
		else {
			sol <- sol[-length(sol[[1]]),] 
			total <- rbind(total, sol) # append the new solution
		}	
	}

	rownames(total) <- 1:nrow(total)
	time <- seq(0, L[N], by=time_step) # the evaluation times for total
	out <- list('sol'=total, 'time'=time)
	
	return(out)
}








