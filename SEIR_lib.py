"""
Estimating a SEIR model on Italian data:

S_der = -beta*S*I + alpha*R
E_der = beta*S*I - epsilon*E
I_der = epsilon*E - gamma*I
R_der = gamma*I - alpha*R

Incubation Time (IT = 5.5) data from : 
https://www.jwatch.org/na51083/2020/03/13/covid-19-incubation-period-update

Data pulled from civil protection:
https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv

MODELLING ASSUMPTIONS per function:
    import_italy :  Incubation Time is 6 days (from mean of 5.5)
                    Italian population is 60500000
                    Data is IT days old
                    
    der :           X_der[T] = dX/dt (T) is approximable with (X[T+1]-X[T-1])/2 
    
PROBLEMS WITH CURRENT MODEL:
    The parameters (alpha, beta, gamma, epsilon), though estimated in a sensible manner,
        produce a model that does not fit the data at all

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from scipy.integrate import odeint


"""Import data from civil protection, returns data series S, E, I, R"""

def import_italy():   
    # E and R are observed, I is estimated from E[+IT], S = N-E-I-R
    # Each seris is rescaled to match with I
    df = pd.read_csv('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv')
    N = 60500000
    IT = 6
    
    E = df.totale_casi.values
    I = [E[i+IT] for i in range(len(E)-IT)]
    R = df.dimessi_guariti.values + df.deceduti.values
    
    E = E[:-IT]
    R = R[:-IT]
    
    S = N - E - I - R
    
    return [S, E, I, R]


"""Approximate derivative for data points"""

def der(X):
    return np.array([X[1]-X[0] if t == 0
            else X[-1]-X[-2] if t == len(X)-1
            else (X[t+1]-X[t-1])/2 
            for t in range(0, len(X))])


"""Estimate beta and epsilon with E_der = beta*S*I + epsilon*E, 
return parameters and R^2""" 
  
def estimate_beta_epsilon(S, I, E, E_der):
    X = np.array(list(zip(S*I,E))) 
    y = E_der
    
    reg = LinearRegression(fit_intercept=False).fit(X, y)
    beta, epsilon = reg.coef_
    R_sq = reg.score(X, y)
    
    return beta, epsilon, R_sq


"""Estimate alpha and gamma with S_der + E_der + I_der = -gamma*I + alpha*R, 
return parameters and R^2"""

def estimate_alpha_gamma(I, R, S_der, E_der, I_der):
    X = np.array(list(zip(I, R)))
    y = S_der + E_der + I_der
    
    reg = LinearRegression(fit_intercept=False).fit(X, y)
    gamma, alpha = reg.coef_
    gamma = -gamma
    R_sq = reg.score(X, y)
    
    return alpha, gamma, R_sq


"""Model of SEIR, maps Y in Y_der"""

def SEIR_model(Y, t, alpha, beta, gamma, epsilon):
    S, E, I, R = Y
    return np.array([
        -beta*S*I + alpha*R,    #susceptible
        beta*S*I - epsilon*E,   #exposed
        epsilon*E - gamma*I,    #infected
        gamma*I - alpha*R])     #removed


"""Solve SEIR using odeint. Plot two graphs: one for forecasting the other for 
comparison of model prediction with actual data"""

def plot_SEIR(Y0, parameters, time_steps, time_data, Y):
    
    sol = odeint(SEIR_model, Y0, time_steps, args=parameters)
    
    S_ = sol[:,0]
    E_ = sol[:,1]
    I_ = sol[:,2]
    R_ = sol[:,3]
    
    fig = plt.figure(1, figsize =[10,15])
    
    forecast = fig.add_subplot(211)  
    forecast.plot(time_steps, S_, 'b', label='Susceptible')
    forecast.plot(time_steps, E_, 'r', label='Exposed')
    forecast.plot(time_steps, I_, 'g', label='Infected')
    forecast.plot(time_steps, R_, 'k', label='Removed')
    forecast.plot(time_steps, S_+E_+I_+R_, '--', label='Population')
    forecast.legend()
    
    # ridimension solutions
    
    E_ = E_[:len(Y)]
    I_ = I_[:len(Y)]
    R_ = R_[:len(Y)]
    
    show_data = fig.add_subplot(212)  
    show_data.plot(time_data, E_, 'r', label='Exposed')
    show_data.plot(time_data, I_, 'g', label='Infected')
    show_data.plot(time_data, R_, 'k', label='Removed')
    show_data.plot(time_data, Y, 'go', label='Data Infected')
    show_data.legend()    
    
    plt.show()
   
        





    
