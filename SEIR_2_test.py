# -*- coding: utf-8 -*-
"""
Created on Wed Mar 25 16:23:10 2020

@author: Io
"""

from SEIR_2 import *
import matplotlib.pyplot as plt
from scipy.interpolate import *

U, P = import_italy()

k=1

t = range(len(P))
t_k = range(k*len(P))

S, E, I, R = U

# R0 = 2.2
# alpha = 0
# beta = R0/N
# gamma = 0.05
# epsilon = 1
alpha, beta, gamma, epsilon = None, None, None, None


sol, param, Rsq = solve_SEIR(U, k=k, alpha=alpha, beta=beta, gamma=gamma, epsilon=epsilon)

S_, E_, I_, R_ = sol

plt.plot(t_k, E_, 'r', label='SEIR Exposed')
plt.plot(t_k, I_, 'g', label='SEIR Infected')
plt.plot(t_k, R_, 'y', label='SEIR Removed')

plt.plot(t, P, 'bo', label='Positive')    
plt.plot(t, E, 'ro', label='Exposed')
plt.plot(t, I, 'go', label='Infected')
plt.plot(t, R, 'yo', label='Removed')
plt.legend()

# plt.show()

dict = {0:'alpha', 1:'beta', 2:'gamma', 3:'epsilon', 4:'Rsq'}
for i in range(len(param)+1):
    if i<len(param):
        print(dict.get(i), param[i], sep = '\t:\t')
    else: 
        print(dict.get(i), Rsq, sep = '\t:\t')
        
#%%    LOGISTIC, SPLINE: data interpolation. more resolution
        
# param_l, cov_l = logistic_fit(U)
# sup, g_rate, cen = param_l

# # dict_l = {0:'Superior', 1:'Growth Rate', 2:'Flex Point'}
# # for i in range(len(param_l)):
# #     print(dict_l.get(i), param_l[i], sep = '\t:\t')

# z = [logistic(x, sup, g_rate, cen) for x in t_triple]

# plt.plot(t_triple, z)
# plt.plot(t, I, 'o')


# #%% SEIR WITH DATA FROM LOGISTIC and interpolation

# t_ = np.linspace(0, len(P), 100*len(P))

# Uspl = [splrep(t, X, k=1) for X in U]

# U_ = [splev(t_, Xspl) for Xspl in Uspl]

# Sspl, Espl, Ispl, Rspl = Uspl
# S_, E_, I_, R_ = U_

# # for X_ in U_:
# #     if X_ is not S_:
# #         plt.plot(t_, X_)

# sol_, param_, Rsq_ = solve_SEIR(U_, k=2, alpha=alpha, beta=beta, gamma=gamma, epsilon=epsilon)

# dict_ = {0:'alpha_', 1:'beta_', 2:'gamma_', 3:'epsilon_', 4:'Rsq_'}
# for i in range(len(param_)+1):
#     if i<len(param_):
#         print(dict_.get(i), param_[i], sep = '\t:\t')
#     else: 
#         print(dict_.get(i), Rsq_, sep = '\t:\t')

# S_, E_, I_, R_ = sol_

# plt.plot(t_, E_, 'r', label='SPLINE Exposed')
# plt.plot(t_, I_, 'g', label='SEIR Infected')
# plt.plot(t_, R_, 'y', label='SEIR Removed')

# plt.show()


