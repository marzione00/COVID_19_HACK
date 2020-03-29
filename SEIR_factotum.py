# -*- coding: utf-8 -*-
"""
Created on Sun Mar 29 12:34:27 2020

@author: Federico Matteucci - De Rham Cohomology
"""
"""
SEIR_factotum documentation

Input Parameters:
    P : list of int
        time series of all positive tested cases. time range=(0, lenP).
        
    R : list of int
        time series of all removed cases (either dead or immune).
        
    N : int (default 0)
        total population.    
            
    interest : string (default 'all')
        specifies which SEIR parameters are to be estimated, determines inputs -> outputs
        'all' : alpha, beta, gamma, epsilon 
                P, R -> U, sol, param, Rsq 
        'none': none 
                P, R, alpha, beta, gamma, epsilon -> U, sol
        'R0'  : beta (R0 = beta/gamma*N) 
                P, R, alpha, gamma, epsilon -> U, sol, R0, Rsq
        
    future : int (default 1) 
        rescaling factor of the time range of data to evaluate the SEIR solutions.
        
    time_step : int (default 1)
        time difference (in days) between observations.
            

Other input parameters:
    IT : list of float
        discrete probability distribution of random variable Incubation Time. 
        IT[tau] = Pr(Incubation time == tau)
        
    TT : list of float
        discrete probability distribution of random variable Test Time. 
        TT[tau] = Pr(Time span from symptomaticity to tested positive == tau)    
        
    IBST : list of float
        discrete probability distribution of random variable Infective Before Symptoms Time.
        IBST[tau] = Pr(Time span from infectivity to simptomaticity == tau)
        
    alpha, beta, gamma, epsilon : float
        SEIR parameter alpha, beta, gamma, epsilon resp.
        used ONLY if fit_data==False
        
        
Output paramteters:
    U : list of np.array    
        U=[S, E, I, R] datapoints corresponding to range(0, lenP-m, time_step).
        
    sol : list of np.array
        sol=[S_, E_, I_, R_] evaluation of SEIR in range(0, future*(lenP-m), time_step).
        
    param : list of float
        param=[alpha, beta, gamma, epsilon] the estimated SEIR parameters.
        
    R0 : float
        R0 = beta/gamma*N. gamma and N are supposed known, beta is estimated    
    
    Rsq : tuple of floats
        R square of linear regressions performed in parameter estimation. 
        Naif rule of thumb: if Rsq<0.999 parameters do not model the data properly. 
"""

def SEIR_factotum(P, R, N=0, interest='all', future=1, time_step=1, **kwargs):
#%% ADDITIONAL MODULES   
    
    import numpy as np 
    from sklearn.linear_model import LinearRegression
    from scipy.integrate import odeint
    
#%% INPUT TREATMENT & VARIABLES 
    
    # IT[tau] = Pr(Incubation time == tau)
    IT = np.array(kwargs.get('IT', [0, 0, 0, 0, 0, 1]))
    # TT[tau] = Pr(Time span from symptomaticity to tested positive == tau)
    TT = np.array(kwargs.get('TT', [1]))
    # IBST[tau] = Pr(Time span from infectivity to simptomaticity == tau)
    IBST = np.array(kwargs.get('IBST', [1]))
    # SEIR parameters
    alpha = kwargs.get('alpha', None)
    beta = kwargs.get('beta', None)
    gamma = kwargs.get('gamma', None)
    epsilon = kwargs.get('epsilon', None)

    # m = number of final observation to be dropped
    m = max(len(IT), len(TT), len(IBST))    
    
#%% FUNCTIONS
    
    def refine_data(P, R):     # Get S, E, I, R of proper length 
        
        P = np.array(P)
        R = np.array(R)
        
        E = (P-P)[:-m]
        I = (P-P)[:-m]
        R = R[:-m]
        
        for t in range(len(P)-m):
            IT.resize(len(P)-t, refcheck=False)
            TT.resize(len(P)-t, refcheck=False)
            IBST.resize(len(P)-t, refcheck=False)
            
            E[t] = ((IT+TT)/2).dot(P[t:])
            I[t] = ((IBST+TT)/2).dot(P[t:])
        
        S = N-E-I-R   
        
        refined_data = [S, E, I, R]
    
        return refined_data, P[:-m]

    def SEIR_model(U, t, param): # SEIR ODE system
        S, E, I, R = U
        alpha, beta, gamma, epsilon = param
        return np.array([
                -beta*S*I + alpha*R,    # dS
                beta*S*I - epsilon*E,   # dE
                epsilon*E - gamma*I,    # dI
                gamma*I - alpha*R])     # dR
    
    def der(U, time_step=1):   # Derivative approximation for discrete U 
        dU = np.empty_like(U)
        i = 0
        
        for X in U:
            dU[i] = np.array([X[1]-X[0] if t == 0
                else X[-1]-X[-2] if t == len(X)-1
                else (X[t+1]-X[t-1])/2 
                for t in range(len(X))])
            dU[i] = dU[i]/time_step
            i += 1
    
        return dU          
    
    def estimate_parameters(U, dU, **kwargs): # Estimation of alpha, beta, gamma, epsilon
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
            beta, epsilon = [reg.coef_[0], -reg.coef_[1]]
            Rsq_be = reg.score(x, y)
            
            Rsq = Rsq_ga, Rsq_be
            
        elif alpha!=None and gamma!=None and epsilon!=None:      # estimate beta only
            if beta==None:
                x = S*I.reshape(-1, 1)
                y = dE + epsilon*E       
                reg = LinearRegression(fit_intercept=False).fit(x, y)
                beta = reg.coef_[0]
                Rsq = reg.score(x, y)
            else:
                Rsq = 1
     
        else:
            print('Enter all parameters, no parameters or alpha, gamma, epsilon\n')
            param = None
            Rsq = None 
            return 
        
        parameters = [alpha, beta, gamma, epsilon]
        
        return parameters, Rsq
    
    def solve_SEIR(U, future=1, time_step=1, **kwargs): # Returns SEIR solution for U[0], parameters
    
        alpha = kwargs.get('alpha', None)
        beta = kwargs.get('beta', None)
        gamma = kwargs.get('gamma', None)
        epsilon = kwargs.get('epsilon', None)
        
        dU = der(U, time_step)
        
        param, Rsq = estimate_parameters(U, dU, alpha=alpha, beta=beta, gamma=gamma, epsilon=epsilon)  
            
        eval_time = range(0, future*len(U[0]), time_step)
        U0 = [U[i][0] for i in range(len(U))]    
        
        sol = odeint(SEIR_model, U0, eval_time, args=(param,))    
        
        S_ = sol[:,0]
        E_ = sol[:,1]
        I_ = sol[:,2]
        R_ = sol[:,3]
        
        sol = [S_, E_, I_, R_]
        
        return sol, param, Rsq
    
#%% PUT IT ALL TOGETHER    
    
    U, P = refine_data(P, R)
    S, E, I, R = U
    
    if interest == 'none': 
        if alpha==None or beta==None or gamma==None or epsilon==None:
            print('ERROR: interest = \'none\' requires all parameters as input')
            return None
        
        sol, _, _ = solve_SEIR(U, future=future, time_step=time_step, alpha=alpha, beta=beta, gamma=gamma, epsilon=epsilon)
        return U, sol
    
    elif interest == 'all':    
        sol, param, Rsq = solve_SEIR(U, future=future, time_step=time_step)
        return U, sol, param, Rsq
    
    elif interest == 'R0':
        if alpha==None or gamma==None or epsilon==None:
            print('ERROR: interest = \'R0\' requires alpha, gamma, epsilon as input')
            return None
        
        sol, param, Rsq = solve_SEIR(U, future=future, time_step=time_step, alpha=alpha, gamma=gamma, epsilon=epsilon)
        R0 = param[1]/param[2]*N
        # Rsq is returned as tuple
        return U, sol, R0, (Rsq,)
        
    else: 
        print('ERROR: interest has to be set to either \'all\', \'none\' or \'R0\'')
        return None
    
    

    
    
    
    
    
    
    
    
    
    
