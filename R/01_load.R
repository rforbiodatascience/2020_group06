# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")


# Define functions
# ------------------------------------------------------------------------------
#source(file = "R/99_func.R")


# Load data
# ------------------------------------------------------------------------------
df1 <- read_csv('.//data//raw//covid_19_data.csv')
df2 <- read_csv('.//data//raw//COVID19_line_list_data.csv')
df3 <- read_csv('.//data//raw//COVID19_open_line_list.csv')
df4 <- read_csv('.//data//raw//time_series_covid_19_confirmed.csv')
df5 <- read_csv('.//data//raw//time_series_covid_19_confirmed_US.csv')
df6 <- read_csv('.//data//raw//time_series_covid_19_deaths.csv')
df7 <- read_csv('.//data//raw//time_series_covid_19_deaths_US.csv')
df8 <- read_csv('.//data//raw//time_series_covid_19_recovered.csv')

# # Wrangle data
# # ------------------------------------------------------------------------------
# bl62 <- bl62 %>%
#   select(X1:V) %>%
#   slice(1:20)
#
#
# # Write data
# # ------------------------------------------------------------------------------
# write_tsv(x = bl62,
#           path = "data/01_my_data.tsv")
