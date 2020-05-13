# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("purr")
library("deSolve")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_func.R")


# Load data
# ------------------------------------------------------------------------------
df_ts <- read_csv(file = "data/_augmented/final_ts_world_df_augm.csv")
df_SIR <- read_csv(file = "data/_augmented/SIR_df.csv")

# Plot Scandinavia time series
# ------------------------------------------------------------------------------
ggplot(data=df_ts %>%
         filter(province == 'Denmark' | province == 'Norway' | province == 'Sweden'),
       mapping = aes(x = days_since_first, y = total_confirmed,
                     group = province, color = province)) +
  xlab("Days since first infection") +
  ylab("Total infections (cumulative)") +
  geom_point() +
  xlim(c(0,100))

# SIR modelling
# ------------------------------------------------------------------------------
# ODEs describing susceptible, infected and recovered
SIR <- function(time,state,parameters) {
  with(as.list(c(state,parameters)), {
  dS = -beta*I*S / N
  dI = beta*I*S / N - gamma*I
  dR = gamma*I
  #dD = mu*I
  return(list(c(dS,dI,dR)))
  })
}

# vector of time points from point 0 to latest
max_days <- df_SIR %>%
  filter(province == 'Denmark') %>%
  select(days_since_first) %>%
  max()

times <- seq(0,max_days)

# population size
N <- df_SIR %>%
  filter(province == 'Denmark') %>%
  select(N) %>%
  min()

initial_values <- c(
  S = N - 1,
  I = 1,
  R = 0
)

# function to be minimized (residual square error)
RSS <- function(parameters) {
  #predicted infections
  fit = ode(y=initial_values,
            times = times,
            func = SIR,
            parms = parameters)[, 3]
  #observed infections
  obs = df_SIR %>%
    filter(province == "Denmark" & days_since_first >= 0) %>%
    select(I)
  return(sum((fit - obs/N)^2))
}

opt <- optim(c(beta=0.5, gamma=0.5),
             RSS,
             method = "L-BFGS-B",
             lower = c(1/N, 1/N),
             upper = c(1, 1)
)

opt$par

df_fitted <- ode(y=initial_values, times = times, func = SIR, parms = opt$par) %>% as_tibble()

ggplot(df_fitted, mapping = aes(x = time, y = I)) +
  geom_point() +
  xlim(c(0,100))

df_SIR_long <- df_SIR %>%
  select(province, date_observation, days_since_first, I, R) %>%
  filter(days_since_first>=0) %>%
  pivot_longer(cols = c(-province,-date_observation,-days_since_first),
               names_to = "variable",
               values_to = "value")

ggplot(data=df_SIR_long %>% filter(province == "Denmark"),
       mapping = aes(x = date_observation, y = value, group = variable, color = variable)) +
  geom_point()




