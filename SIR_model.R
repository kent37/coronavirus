# Estimate a simple SIR model for US cases
# Based on https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov

library(tidyverse)
library(lubridate)
library(deSolve)

# Data
consolidate_us = function(df, values_to) {
  df %>% 
  filter(`Country/Region`=='US') %>% 
  select(-`Country/Region`, -`Province/State`, -Lat, -Long) %>% 
  summarize_all(sum) %>% 
  pivot_longer(everything(), names_to='Date', values_to=values_to)
}
conf = read_csv(here::here('data/time_series_19-covid-Confirmed.csv'))
conf_us = consolidate_us(conf, 'Cases')

deaths_us = read_csv(here::here('data/time_series_19-covid-Deaths.csv')) %>% 
  consolidate_us('Deaths')

recovered_us  = read_csv(here::here('data/time_series_19-covid-Recovered.csv')) %>% 
  consolidate_us('Recovered')

all_us = reduce(list(conf_us, deaths_us, recovered_us), 
                left_join, by='Date') %>% 
  mutate(Removed=Deaths+Recovered, 
         Infected=Cases-Removed)

# Model
N = 329403072 # US population from https://www.census.gov/popclock/
Infected = all_us$Infected
Removed = all_us$Removed
Day = seq_along(Infected)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
    })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  I_hat <- out[ , 3]
  R_hat <- out[ , 4]
  sum((Infected - I_hat)^2)
}
 
Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
 
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6746089 0.3253912
 
t <- 1:200 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour
 
matplot(fit$time, fit[ , 3:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col[2:3])
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot
 
points(Day, Infected)
points(Day, Removed)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model 2019-nCoV China", outer = TRUE, line = -2)

# SEIR model without vitals
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SEIR_model
SEIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dE <- beta/N * I * S - alpha * E
    dI <- alpha * E - gamma * I
    dR <- gamma * I
    list(c(dS, dE, dI, dR))
    })
}

init <- c(S = N-Infected[1], E=0, I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("alpha", "beta", "gamma")
  out <- ode(y = init, times = Day, func = SEIR, parms = parameters)
  I_hat <- out[ , 'I']
  R_hat <- out[ , 'R']
  sum((Infected - I_hat)^2 + (Removed-R_hat)^2)
}
 
Opt <- optim(c(0.5, 0.5, 0.5), RSS, method = "L-BFGS-B", 
             lower = c(0, 0), upper = c(1, 1),
             control=list(trace=2, maxit=200)
             ) # optimize with some sensible conditions

Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
 
(Opt_par <- setNames(Opt$par, c("alpha", "beta", "gamma")))

(R_0 = Opt_par['beta'] / Opt_par['gamma'])

t <- 1:300 # time in days
fit <- data.frame(ode(y = init, times = t, func = SEIR, parms = Opt_par))
col <- 1:4 # colour
 
matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:5], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")

points(Day, Infected, col=3)
points(Day, Removed, col=4)
legend("bottomright", c("Susceptibles","Exposed", "Infecteds", "Removed"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SEIR model 2019-nCoV US", outer = TRUE, line = -2)
