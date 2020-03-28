# -*- coding: utf-8 -*-
"""
Created on Fri Mar 27 12:01:10 2020

@author: Io
"""

"""
TO DO: 
relation between length of interval, resolution and fixing number (res/lgt)
fix splines
normalize the interval on 0,1 to achieve higher resolution m
normalize population to 1


"""





from SEIR_2 import *
import matplotlib.pyplot as plt

def test_model(U0, eval_time, param, resolution=1):
    
    sol = odeint(SEIR_model, U0, eval_time, args=(param,))
    
    
    S = sol[:,0]
    E = sol[:,1]
    I = sol[:,2]
    R = sol[:,3]
    
    
    
    
    
    U = [S, E, I, R]
    dU = der(U, resolution)
    
    new_param, Rsq = estimate_parameters(U, dU)
    new_sol = odeint(SEIR_model, U0, eval_time, args=(new_param,))
    
    return sol, new_sol, new_param, Rsq

N = 10e8    
U0 = [N, 100000, 100, 1]
res = 20
lgt = 20

eval_time = np.linspace(0, lgt, res)
param = [0, 2/N, 0.1, 0.5]

sol, new_sol, new_param, Rsq = test_model(U0, eval_time, param, resolution=lgt/res)

dict = {0:'alpha', 1:'beta', 2:'gamma', 3:'epsilon'}
dict_est = {0:'alpha est', 1:'beta est', 2:'gamma est', 3:'epsilon est'}
for i in range(len(param)):
    print(dict.get(i), param[i], sep = '\t:\t')
for i in range(len(new_param)):
    print(dict_est.get(i), new_param[i], sep = '\t:\t')    
print('Rsq\t:\t', Rsq)

S = sol[:,0]
E = sol[:,1]
I = sol[:,2]
R = sol[:,3]

Sn = new_sol[:,0]
En = new_sol[:,1]
In = new_sol[:,2]
Rn = new_sol[:,3]

t = np.linspace(0, lgt, res)


fig = plt.figure()
ax = plt.subplot(111)
ax.plot(t, S,'b', label='SEIR Susceptible')
ax.plot(t, E,'r', label='SEIR Exposed')
ax.plot(t, I,'g', label='SEIR Infected')
ax.plot(t, R,'y', label='SEIR Removed')

ax.plot(t, Sn,'--b', label='EST Susceptible')
ax.plot(t, En,'r--', label='EST Exposed')
ax.plot(t, In,'g--', label='EST Infected')
ax.plot(t, Rn,'y--', label='EST Removed')

ax.legend(loc='best', shadow=True, ncol=2)

plt.show()





