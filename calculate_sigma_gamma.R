# Calculate sigma and gamma using distributions IT, SRT, IBST

# Inputs : 
#	IT, SRT, IBST : array-like
	
# Outputs : 	
#	out$sigma, out$gamma : float 
#		value of sigma and gamma
		
calculate_sigma_gamma <- function(IT, SRT, IBST){
	expected_value <- function(X){
		out <- (0:length(X)) %*% X
 		return(out)
	}
	
	sigma <- 1/expected_value(IT)
	gamma <- 1/(expected_value(SRT)+expected_value(IBST))
	
	out <- c('sigma'=sigma, 'gamma'=gamma)
	
	return(out)	
	
}
















