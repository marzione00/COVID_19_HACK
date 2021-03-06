% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_stage_ODE.R
\name{multi_stage_ODE}
\alias{multi_stage_ODE}
\title{Multi-stage ODE}
\usage{
multi_stage_ODE(U0, func, var_names, par_fixed, msp, msp_name, L, time_step)
}
\arguments{
\item{U0}{named list. Initial conditions at time 0}

\item{func}{function. Given U array of variables, func(U;parameters) = dU/dt}

\item{var_names}{list of char. The names of the variable as in func}

\item{par_fixed}{named list. The format is ('name'=value)}

\item{msp}{array like. Contains all the values assumed by the time variable parameters}

\item{msp_name}{char. Name of the msp as appearing in func}

\item{L}{array like. Contains the extreme of use of the components of msp.    msp[n] is the msp in use from L[n-1] to L[n]. The last component of L is the final point of integration}

\item{time_step}{float. Step of integration}
}
\value{
a list:
\describe{
\item{sol}{dataframe. Solution to ODE evaluated at $times}
\item{time}{array like. Points of evaluation of solution}
}
}
\description{
Solve an ODE with time-varying parameter msp (multi stage parameter) and time fixed parameters 
parameters.
}
