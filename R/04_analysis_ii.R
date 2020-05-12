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
df_patient <- read_csv(file = "data/_augmented/final_patient_data_df_augm.csv")
df_ts <- read_csv(file = "data/_augmented/final_ts_world_df_augm.csv")


df_ts <- df_ts %>%
  group_by(province) %>%
  mutate(tmp_date = case_when(total_confirmed > 0 ~ date_observation)) %>%
  mutate(days_since_first = date_observation - min(tmp_date, na.rm = TRUE)) %>%
  ungroup %>%
  select(-tmp_date) %>%
  mutate(days_since_first = as.numeric(days_since_first,units="days"))

df_SIR = df_ts %>%
  rename(N = total_population) %>%
  mutate(I = total_confirmed - total_recovered - total_deaths) %>%
  mutate(R = total_recovered + total_deaths) %>%
  mutate(S = N - I - R) %>%
  select(province, N, date_observation, days_since_first, S, I, R)


# Plot time series
# ------------------------------------------------------------------------------
ggplot(data=df_ts %>%
         filter(province == 'Denmark' | province == 'Norway' | province == 'Sweden'),
       mapping = aes(x = days_since_first, y = total_confirmed,
                     group = province, color = province)) +
  geom_point() +
  xlim(c(0,100))

# SIR modelling
# ------------------------------------------------------------------------------
<<<<<<< HEAD
SIR <- function(time,state,parameters) {
  with(as.list(c(state,parameters)), {
  dS = -beta*I*S / N
  dI = beta*I*S / N - gamma*I
  dR = gamma*I
  #dD = mu*I
  return(list(c(dS,dI,dR)))
  })
}
=======

>>>>>>> c4f779187185e96f87d576131c3af2526d8eb671

parameters <- c(
  beta = 0.000000001,
  gamma = 0.000005
)

times = seq(1,80)

ode(y=initial_values, times = times, func = SIR, parms = parameters)

N <- df_ts %>% filter(province == 'Denmark') %>% select(total_population) %>% min()

initial_values <- c(
  S = N - 1,
  I = 1,
  R = 0
)

RSS <- function(parameters) {
  fit = ode(y=initial_values, times = times, func = SIR, parms = parameters)[, 3]
  return(sum((fit - df_SIR %>% filter(province == "Denmark") %>% select(I))^2))
}

opt <- optim(c(beta=0.5, gamma=0.5),
             RSS,
             method = "L-BFGS-B",
             lower = c(0, 0),
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


