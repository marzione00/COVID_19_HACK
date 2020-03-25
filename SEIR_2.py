# -*- coding: utf-8 -*-
"""
Created on Wed Mar 25 16:07:41 2020

@author: Io
"""

import numpy as np
import pandas as pd

from sklearn.linear_model import LinearRegression
from scipy.integrate import odeint

IT = np.array([0, 0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.1, 0.1]) # Incubation time 
TT = np.array([0.2, 0.5, 0.3]) # Test time
IBST = np.flip(np.array([0.7, 0.2, 0.1])) # IBST(tau) = P(infective at day of symptoms - tau)


m = max(len(IT), len(TT), len(IBST))
N = 60500000

def import_italy():
    
    url = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv'
    df = pd.read_csv(url)
    
    P = df.totale_casi.values   # tested positive cases
    E = (P-P)[:-m]
    I = (P-P)[:-m]
    
    for t in range(len(P)-m):
        IT.resize(len(P)-t, refcheck=False)
        TT.resize(len(P)-t, refcheck=False)
        IBST.resize(len(P)-t, refcheck=False)
        
        E[t] = ((IT+TT)/2).dot(P[t:])
        I[t] = ((IBST+TT)/2).dot(P[t:])
    
    R = df.dimessi_guariti.values + df.deceduti.values
    R = R[:-m]
    S = N-E-I-R
    
    refined_data = [S, E, I, R]
    
    return refined_data, P[:-m]
    
def SEIR_model(U, t, param):
        S, E, I, R = U
        alpha, beta, gamma, epsilon = param
        return np.array([
                -beta*S*I + alpha*R,    #susceptible
                beta*S*I - epsilon*E,   #exposed
                epsilon*E - gamma*I,    #infected
                gamma*I - alpha*R])     #removed
    
def der(U):
    dU = np.empty_like(U)
    i = 0
    
    for X in U:
        dU[i] = np.array([X[1]-X[0] if t == 0
            else X[-1]-X[-2] if t == len(X)-1
            else (X[t+1]-X[t-1])/2 
            for t in range(len(X))])
        i += 1
        
    return dU          
    
def estimate_parameters(U, dU, **kwargs):
    S, E, I, R = U
    dS, dE, dI, dR = dU
    
    alpha = kwargs.get('alpha', None)
    beta = kwargs.get('beta', None)
    gamma = kwargs.get('gamma', None)
    epsilon = kwargs.get('epsilon', None)
    
    if alpha==None and beta==None and gamma==None and epsilon==None:       #estimate all parameters
        x = np.array(list(zip(I, R)))
        y = dS + dE + dI
        reg = LinearRegression(fit_intercept=False).fit(x, y)
        gamma, alpha = [-reg.coef_[0], reg.coef_[1]]
        Rsq_ga = reg.score(x, y)
        
        x = np.array(list(zip(S*I,E)))
        y = dE
        reg = LinearRegression(fit_intercept=False).fit(x, y)
        beta, epsilon = [-reg.coef_[0], reg.coef_[1]]
        Rsq_be = reg.score(x, y)
        
        Rsq = Rsq_ga, Rsq_be
        
    elif alpha!=None and gamma!=None and epsilon!=None:      # estimate beta only
        x = S*I.reshape(-1, 1)
        y = dE + epsilon*E       
        
        reg = LinearRegression(fit_intercept=False).fit(x, y)
        beta = reg.coef_[0][0]
        Rsq = reg.score(x, y)
    
    else:
        print('Specify either alpha gamma and epsilon or no paramters at all\n')
    
    parameters = [alpha, beta, gamma, epsilon]

    return parameters, Rsq

def solve_SEIR(U, k=4):
    
    dU = der(U)
    
    param, Rsq = estimate_parameters(U, dU)
    eval_time = range(k*len(U[0]))
    U0 = [U[i][0] for i in range(len(U))]    
    
    sol = odeint(SEIR_model, U0, eval_time, args=(param,))    
    
    S_ = sol[:,0]
    E_ = sol[:,1]
    I_ = sol[:,2]
    R_ = sol[:,3]
    
    sol = [S_, E_, I_, R_]
    
    return sol, param, Rsq
    



