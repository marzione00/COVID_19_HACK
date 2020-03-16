"""
Showcase for the SEIR_lib module

"""

import SEIR_lib 

#%% Import

S, E, I, R = SEIR_lib.import_italy()

S_der = SEIR_lib.der(S)
E_der = SEIR_lib.der(E)
I_der = SEIR_lib.der(I)
R_der = SEIR_lib.der(R)

#%% Estimate parameters

beta, epsilon, R_sq_be = SEIR_lib.estimate_beta_epsilon(S, I, E, E_der)
alpha, gamma, R_sq_ag = SEIR_lib.estimate_alpha_gamma(I, R, S_der, E_der, I_der)

print('alpha : ', alpha)
print('beta : ', beta)
print('gamma : ', gamma)
print('epsilon : ', epsilon)
print('R_sq_be : ', R_sq_be)
print('R_sq_ag : ', R_sq_ag)

#%% Plot SEIR model

Y0 = [S[0], E[0], I[0], R[0]] # initial conditions
Y = I    # actual data points of infected
parameters = (alpha, beta, gamma, epsilon)
time_steps = range(50*len(S))   # in order to forecast in long distance 
time_data = range(len(S))   # the actual range of time for which data is available

SEIR_lib.plot_SEIR(Y0, parameters, time_steps, time_data, Y)







