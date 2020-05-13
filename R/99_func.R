# Define project functions
# ------------------------------------------------------------------------------
# Linear model for the time series
mdl <- function(df) {
  lm(count ~ date_observation, data = df)
}

# SIR model computation
SIR <- function(date,data,pars) {
  dS = -beta*I*S
  dI = beta*I*S - gamma*I
  dR = gamma*I
  dD = mu*I
  return(c(dS,dI,dR,dD))
}
