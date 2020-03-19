# -*- coding: utf-8 -*-
"""
Created on Thu Mar 19 12:39:41 2020

@author: Io
"""

import SEIR_lib
import pandas as pd 
import matplotlib.pyplot as plt

#%% Import

total, national = SEIR_lib.import_italy()
S, E, I, R = total
S_n, I_n, E_n, R_n = national   

N = (S + E + I + R)[0]
S_der = SEIR_lib.der(S)
E_der = SEIR_lib.der(E)
I_der = SEIR_lib.der(I)
R_der = SEIR_lib.der(R)

S_n_der = pd.Series(float)
S_n_der.pre = SEIR_lib.der(S_n.pre)
S_n_der.post = SEIR_lib.der(S_n.post)
I_n_der = pd.Series(float)
I_n_der.pre = SEIR_lib.der(I_n.pre)
I_n_der.post = SEIR_lib.der(I_n.post)
E_n_der = pd.Series(float)
E_n_der.pre = SEIR_lib.der(E_n.pre)
E_n_der.post = SEIR_lib.der(E_n.post)
R_n_der = pd.Series(float)
R_n_der.pre = SEIR_lib.der(R_n.pre)
R_n_der.post = SEIR_lib.der(R_n.post)

#%% Fitting with restricted parameters

parameters, R_sq = SEIR_lib.estimate_restricted_parameters(S, E, I, E_der)
alpha, beta, gamma, epsilon = parameters
R_0 = SEIR_lib.estimate_R_0(N, beta, gamma)

parameters_pre, R_sq_pre = SEIR_lib.estimate_restricted_parameters(S_n.pre, E_n.pre, I_n.pre, E_n_der.pre)
alpha_pre, beta_pre, gamma_pre, epsilon_pre = parameters_pre
R_0_pre = SEIR_lib.estimate_R_0(N, beta_pre, gamma_pre)

parameters_post, R_sq_post = SEIR_lib.estimate_restricted_parameters(S_n.post, E_n.post, I_n.post, E_n_der.post)
alpha_post, beta_post, gamma_post, epsilon_post = parameters_post
R_0_post = SEIR_lib.estimate_R_0(N, beta_post, gamma_post)

print('R_0 Total           : ', R_0.round(2), 'R_sq : ', R_sq.round(2), sep='\t')
print('R_0 Pre quarantine  : ', R_0_pre.round(2), 'R_sq : ', R_sq_pre.round(2), sep='\t')
print('R_0 Post quarantine : ', R_0_post.round(2), 'R_sq : ', R_sq_post.round(2), sep='\t')

#%% solving SEIR eq

Y0 = [S[0], E[0], I[0], R[0]] # initial conditions
Y = I    # actual data points of infected
parameters_r = (alpha, beta, gamma, epsilon)
time_steps = range(50*len(S))   # in order to forecast in long distance 
time_data = range(len(S))   # the actual range of time for which data is available

SEIR_lib.plot_restricted_SEIR(Y0, parameters_r, time_steps, time_data, Y)



