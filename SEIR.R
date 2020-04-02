#R SEIR
library(deSolve)
library(covid19)
library(ggplot2)

#Data loading & global variables
lomb <- get_regionTS()$Lombardia
poplomb <- italy_pop$region[italy_pop$region$territorio == "Lombardia", "valore"]
P <- lomb$totale_casi
R <- lomb$totale_casi - lomb$totale_positivi

IT <- c(0,0,0,0,0,1)
TT <- c(1)
IBST <- c(1)
m = max(length(IT), length(TT), length(IBST))
ITl = length(IT)
TTl = length(TT)
IBSTl = length(IBST)
N = 10500000
latent_time = IT%*%(1:length(IT)-1)



#%% Functions
refine_data <- function(P, R) {
	
	n_obs = length(P)
	
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
	
	S = N-E-I-R
	
	return(data.frame(S, E, I, R))
	
}

SEIR_model <- function(t, state, parameters){
	with(as.list(c(state, parameters)), {
		dS <- alpha*R-beta*S*I
		dE <- beta*S*I-sigma*E
		dI <- sigma*E-gamma*I
		dR <- gamma*I-alpha*R
	
		list(c(dS, dE, dI, dR))
	})
}

solve_SEIR <- function(U0, eval_time=eval_time, parameters=parameters){
	state <- c( S = U0[1], E=U0[2], I=U0[3], R=U0[4])
	
	
	out <- ode(y=state, times=eval_time, func=SEIR_model, parms=parameters)
	out <- data.frame(out)
	return(out)
}

I_approx <- function(R0, sigma, gamma,t) {
	
	out <- exp(1/2*(sqrt(4*(R0-1)*sigma*gamma+(sigma+gamma)^2)-(sigma+gamma))*t)
	
	return(out) 
	
}

sqdist <- function(R0, sigma, gamma, I) {
	
	out = 0
	
	for(t in (1:length(I)))
		out = out + (I_approx(R0, sigma, gamma, t)+I[t])^2
	
	return(out)
}


#%% testing
I = c(1, 2, 4, 8, 16, 32, 62, 120, 230, 500, 960, 2050)
t <- (1:length(I))
sigma = 0.2
gamma = 0.1
R0 = 30

I_est = I_approx(R0, sigma, gamma, t)
sqd = sqdist(R0, sigma, gamma, I)
df <- data.frame(I_est, I, t)

ggplot(df, aes(t)) + geom_line(aes(y=I_est)) + geom_point(aes(y=I))

R0_opt = optimise(sqdist, sigma=sigma, gamma=gamma, I=I, interval=c(15,45))


#%% Use
U = refine_data(P, R)
parameters <- c(alpha = 0.1,
				beta = 0.5/N,
				gamma = 0.2,
				sigma = 0.4)

U0 <- as.numeric(U[1,])

eval_time <- seq(0, length(U$S)-1, by = 1)		


sol <- solve_SEIR(U0, eval_time=eval_time, parameters=parameters)

S <- U[[1]]
E <- U[[2]]
I <- U[[3]]
Rem <- U[[4]]

S_ <- sol$S
E_ <- sol$E
I_ <- sol$I
R_ <- sol$R

t = (1:length(U[[1]]))

Udf <- data.frame(U, sol, t)


#ggplot(Udf, aes(t)) +
#	geom_point(aes(y = E), colour = "red") +
	#geom_point(aes(y = I), colour = "blue") +
	#geom_point(aes(y = R), colour = "green")+
	#geom_line(aes(y = E_), colour = "red") +
	#geom_line(aes(y = I_), colour = "blue") +
	#geom_line(aes(y = R_), colour = "green")




















