"""
Estimating a SEIR model on Italian data:

S_der = -beta*S*I + alpha*R
E_der = beta*S*I - epsilon*E
I_der = epsilon*E - gamma*I
R_der = gamma*I - alpha*R

Restricted SEIR: 
    alpha = immunity_loss_time = 0
    beta = R_0*gamma
    gamma = 1/T_inf = 1/2.5   
    epsilon = 1/T_inc = 1/5.5
    


Incubation Time (T_inc = 5.5) data from : 
https://www.jwatch.org/na51083/2020/03/13/covid-19-incubation-period-update

Infectious Period (T_inf ~ 2.5) data from : 
http://gabgoh.github.io/COVID/index.html    

Data pulled from civil protection:
https://raw.githubusercontent.com/pcm-dpc/COVID-19

MODELLING ASSUMPTIONS per function:
    import_[...] :  Incubation Time is 6 days (from mean of 5.5)
                    Italian population is 60500000
                    Data is T_inc days old
                    
    der :           X_der[T] = dX/dt (T) is approximable with (X[T+1]-X[T-1])/2 in inner points
    
PROBLEMS WITH CURRENT MODEL:
    The parameters (alpha, beta, gamma, epsilon), though estimated in a sensible manner,
        produce a model that does not fit the data at all
    Dataset is from 24/02 only 
    
    
    
    
    
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from scipy.integrate import odeint


# Global Parameters

T_inc = 5.5 # Incubation time
T_inc_int = int(np.ceil(T_inc)) # Approx to int
T_inf = 2.5 # Infectiveness time
First_measures = -3 # First quarantine measures, 21/02/2020
Local_measures = 1 # Strong quarantine in Lombardia & some other provinces, 25/02/2020
National_measures = 13 # Strong quarantine in all of Italy, 8/03/2020


"""Import data from civil protection, returns data series S, E, I, R"""

def import_italy():   
    # E and R are observed, I is estimated from E[+T_inc], S = N-E-I-R
    # Each seris is rescaled to match with I
    url = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv'
    df = pd.read_csv(url)
    N = 60500000
    
    E = df.totale_casi.values
    I = [E[i-T_inc_int] for i in range(T_inc_int, len(E))]
    R = df.dimessi_guariti.values + df.deceduti.values
    
    E = E[T_inc_int:]
    R = R[T_inc_int:]    
    S = N - E - I - R
    total = [S, E, I, R]
    
    S_national = pd.Series()
    S_national.pre = S[:National_measures] 
    S_national.post = S[National_measures:]
    I_national = pd.Series()
    I_national.pre = I[:National_measures] 
    I_national.post = I[National_measures:]  
    E_national = pd.Series()
    E_national.pre = E[:National_measures] 
    E_national.post = E[National_measures:]    
    R_national = pd.Series()
    R_national.pre = R[:National_measures] 
    R_national.post = R[National_measures:]
    national = [S_national, I_national, E_national, R_national]
    
    return total, national


"""Import data from civil protection, returns data series S, E, I, R=0 due to no data"""

def import_province(province_code = 'LO'):
    
    url = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv'
    df = pd.read_csv(url, usecols=['data', 'denominazione_regione', 'sigla_provincia', 'totale_casi'])
    df.dropna(axis=0, inplace=True)
    
    df = df.loc[df.sigla_provincia == province_code]
      
    N = 60500000
    T_inc = 6
    
    E = df.totale_casi.values
    I = [E[i-T_inc] for i in range(T_inc, len(E))]
    R = np.empty(len(I), dtype=int)
    
    E = E[T_inc:]
    
    S = N - E - I 
    
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


""" """

def estimate_restricted_parameters(S, E, I, E_der):
    alpha = 0
    gamma = 1/T_inf
    epsilon = 1/T_inc
    
    X = S*I
    y = E_der + epsilon*E
    
    X = X.reshape(-1, 1)
    y = y.reshape(-1, 1)    
    
    reg = LinearRegression(fit_intercept=False).fit(X, y)
    beta = reg.coef_[0][0]
    R_sq = reg.score(X, y) 
    
    parameters = [alpha, beta, gamma, epsilon]
    
    return parameters, R_sq

""" """
def estimate_R_0(N, beta, gamma):
    return beta/gamma*N


"""Model of SEIR, maps Y in Y_der"""

def SEIR_model(Y, t, alpha, beta, gamma, epsilon):
    S, E, I, R = Y
    return np.array([
        -beta*S*I + alpha*R,    #susceptible
        beta*S*I - epsilon*E,   #exposed
        epsilon*E - gamma*I,    #infected
        gamma*I - alpha*R])     #removed



def restricted_SEIR_model(Y, t, alpha, beta, gamma, epsilon):
    S, E, I, R = Y
    return np.array([
        -beta*S*I,   #susceptible
        beta*S*I - epsilon*E,   #exposed
        epsilon*E - gamma*I,    #infected
        gamma*I])     #removed

"""Solve SEIR using odeint. Plot two graphs: one for forecasting the other for 
comparison of model prediction with actual data"""

def plot_SEIR(Y0, parameters, time_steps, time_data, Y):
    
    sol = odeint(SEIR_model, Y0, time_steps, args=parameters)
    
    S_ = sol[:,0]
    E_ = sol[:,1]
    I_ = sol[:,2]
    R_ = sol[:,3]
    
    
    
    fig = plt.figure(1, figsize =[20,15])
    
    forecast = fig.add_subplot(211)  
    forecast.plot(time_steps, S_, 'b', label='Susceptible')
    forecast.plot(time_steps, E_, 'r', label='Exposed')
    forecast.plot(time_steps, I_, 'g', label='Infected')
    forecast.plot(time_steps, R_, 'k', label='Removed')
    forecast.plot(time_steps, S_+E_+I_+R_, '--', label='Population')
    forecast.legend()
    forecast.set_title('Forecasting with unrestricted parameters from 24/02/2020')
    
    # reshape solutions
    E_ = E_[:len(Y)]
    I_ = I_[:len(Y)]
    R_ = R_[:len(Y)]

    show_data = fig.add_subplot(212)  
    show_data.plot(time_data, E_, 'r', label='Exposed')
    show_data.plot(time_data, I_, 'g', label='Infected')
    show_data.plot(time_data, R_, 'k', label='Removed')
    show_data.plot(time_data, Y, 'go', label='Data Infected')
    show_data.legend()   
    show_data.set_title('Comparison with data with unrestricted parameters from 24/02/2020')

    plt.show()

def plot_restricted_SEIR(Y0, parameters, time_steps, time_data, Y):   
     
    sol = odeint(restricted_SEIR_model, Y0, time_steps, args=parameters)
    
    S_ = sol[:,0]
    E_ = sol[:,1]
    I_ = sol[:,2]
    R_ = sol[:,3]
    
    fig = plt.figure(2, figsize =[20,15])
    
    forecast = fig.add_subplot(211)  
    forecast.plot(time_steps, S_, 'b', label='Susceptible')
    forecast.plot(time_steps, E_, 'r', label='Exposed')
    forecast.plot(time_steps, I_, 'g', label='Infected')
    forecast.plot(time_steps, R_, 'k', label='Removed')
    forecast.plot(time_steps, S_+E_+I_+R_, '--', label='Population')
    forecast.legend()
    forecast.set_title('Forecasting with restricted parameters from 24/02/2020')
    forecast.grid()
    
    # reshape solutions  
    E_ = E_[:len(Y)]
    I_ = I_[:len(Y)]
    R_ = R_[:len(Y)]

    show_data = fig.add_subplot(212)  
    show_data.plot(time_data, E_, 'r', label='Exposed')
    show_data.plot(time_data, I_, 'g', label='Infected')
    show_data.plot(time_data, R_, 'k', label='Removed')
    show_data.plot(time_data, Y, 'go', label='Data Infected')
    show_data.legend()   
    show_data.set_title('Comparison with data with restricted parameters from 24/02/2020')
        
    plt.show()




    
