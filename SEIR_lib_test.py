"""
Showcase for the SEIR_lib module

"""

import SEIR_lib 
import matplotlib.pyplot as plt

#%% Import

total,_ = SEIR_lib.import_italy()
S, E, I, R = total

N = (S + E + I + R)[0]
S_der = SEIR_lib.der(S)
E_der = SEIR_lib.der(E)
I_der = SEIR_lib.der(I)
R_der = SEIR_lib.der(R)

#%% Fitting with unrestricted parameters

beta, epsilon, R_sq_be = SEIR_lib.estimate_beta_epsilon(S, I, E, E_der)
alpha, gamma, R_sq_ag = SEIR_lib.estimate_alpha_gamma(I, R, S_der, E_der, I_der)
R_0 = SEIR_lib.estimate_R_0(N, beta, gamma)

print('alpha : ', alpha)
print('beta : ', beta)
print('gamma : ', gamma)
print('epsilon : ', epsilon)
print('R_sq_be : ', R_sq_be)
print('R_sq_ag : ', R_sq_ag)
print('R_0 : ', R_0, '\n')

Y0 = [S[0], E[0], I[0], R[0]] # initial conditions
Y = I    # actual data points of infected
parameters = (alpha, beta, gamma, epsilon)
time_steps = range(50*len(S))   # in order to forecast in long distance 
time_data = range(len(S))   # the actual range of time for which data is available

SEIR_lib.plot_SEIR(Y0, parameters, time_steps, time_data, Y)

#%% Fitting with restricted parameters

parameters_r, R_sq_r = SEIR_lib.estimate_restricted_parameters(S, E, I, E_der)
alpha_r, beta_r, gamma_r, epsilon_r = parameters_r
R_0_r = SEIR_lib.estimate_R_0(N, beta_r, gamma_r)

print('beta_r : ', beta_r)
print('gamma_r : ', gamma_r)
print('epsilon_r : ', epsilon_r)
print('R_sq_r : ', R_sq_r)
print('R_0_r : ', R_0_r)

Y0 = [S[0], E[0], I[0], R[0]] # initial conditions
Y = I    # actual data points of infected
parameters_r = tuple(parameters_r)
time_steps = range(50*len(S))   # in order to forecast in long distance 
time_data = range(len(S))   # the actual range of time for which data is available

SEIR_lib.plot_restricted_SEIR(Y0, parameters_r, time_data, time_data, Y)


#%% Plotting data
"""
plt.plot(time_data, S, 'o', label='Susceptible')
plt.plot(time_data, E, 'o', label='Exposed')
plt.plot(time_data, I, 'o', label='Infecting')
plt.plot(time_data, R, 'o', label='Removed')
plt.title('Data')
plt.legend()
plt.show()
"""
#%% lodi 
"""
S_lodi, E_lodi, I_lodi, R_lodi = SEIR_lib.import_province()
t = range(len(S_lodi))
#plt.plot(t, S_lodi, 'o', label='Susceptible')
plt.plot(t, E_lodi, 'o', label='Exposed')
plt.plot(t, I_lodi, 'o', label='Infecting')
plt.legend()
plt.show()
"""
