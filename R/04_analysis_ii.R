# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("purr")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_func.R")


# Load data
# ------------------------------------------------------------------------------
df_patient <- read_csv(file = "data/_augmented/final_patient_data_df_augm.csv")
df_ts <- read_csv(file = "data/_augmented/final_ts_world_df_augm.csv")


df_ts <- df_ts %>%
  group_by(country) %>%
  mutate(tmp_date = case_when(total_confirmed > 0 ~ date_observation)) %>%
  mutate(days_since_first = date_observation - min(tmp_date, na.rm = TRUE)) %>%
  ungroup %>%
  select(-tmp_date) %>%
  mutate(days_since_first = as.numeric(days_since_first,units="days"))



# Plot time series
# ------------------------------------------------------------------------------
ggplot(data=df_ts %>%
         filter(country == 'Denmark' | country == 'Sweden' | country == 'Norway'),
       mapping = aes(x = days_since_first, y = total_confirmed,
                     group = country, color = country)) +
  geom_point() +
  xlim(c(0,100))

# SIR modelling
# ------------------------------------------------------------------------------


df_SIR = df_ts %>%
  rename(N = total_population) %>%
  mutate(I = total_confirmed - total_recovered - total_deaths) %>%
  mutate(R = total_recovered) %>%
  mutate(S = N - I - R) %>%
  select(country, days_since_first, I, R)

df_SIR_long <- df_SIR %>%
  filter(days_since_first>=0) %>%
  pivot_longer(cols = c(-country,-days_since_first),
               names_to = "variable",
               values_to = "value")

ggplot(data=df_SIR_long %>% filter(country == "Denmark"),
       mapping = aes(x = days_since_first, y = value, group = variable, color = variable)) +
  geom_point()




