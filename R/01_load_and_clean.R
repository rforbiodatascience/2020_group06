
################################################################################
########################### - Clear workspace - ################################
################################################################################

rm(list = ls())

################################################################################
############################ - Load libraries - ################################
################################################################################

library('tidyverse')
library('lubridate')

################################################################################
########################### - Define functions - ###############################
################################################################################

#source(file = "R/99_func.R")

################################################################################
############################## - Load data - ###################################
################################################################################

df1 <- read_csv('.//data//_raw//covid_19_data.csv')
df2 <- read_csv('.//data//_raw//COVID19_line_list_data.csv')
df3 <- read_csv('.//data//_raw//COVID19_open_line_list.csv')
df4 <- read_csv('.//data//_raw//time_series_covid_19_confirmed.csv')
df5 <- read_csv('.//data//_raw//time_series_covid_19_confirmed_US.csv')
df6 <- read_csv('.//data//_raw//time_series_covid_19_deaths.csv')
df7 <- read_csv('.//data//_raw//time_series_covid_19_deaths_US.csv')
df8 <- read_csv('.//data//_raw//time_series_covid_19_recovered.csv')


###################################################################################################
####################### - Wrangle data from 'df1': covid_19_data.csv - ############################
###################################################################################################

print('The structure of df1 is: ')
str(df1)

################## - RENAME COLUMNS - ####################
daily_covid_trends_df <-
  df1 %>%
  rename("IDKey" = "SNo") %>%
  mutate(ObservationDate = mdy(ObservationDate)) %>%
  rename("Province" = `Province/State`) %>%
  rename("Country" = `Country/Region`) %>%
  select(-`Last Update`)

################## - Standarize PROVINCE data (removing data inconstencies) - ####################
daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert patterns of the type 'Pierce County, WA' to 'Pierce County'
  mutate(Province = str_replace_all(Province, pattern = '(.*), \\w\\w(.*)', replacement = '\\1\\2')) %>%

  # Convert patterns of the type 'Pierce County' to 'Pierce'
  mutate(Province = str_replace_all(Province, pattern = '(.*) County(.*)', replacement = '\\1\\2')) %>%

  # Convert patterns of the type 'Travis (From Diamond Princess)' to 'Diamond Princess'
  mutate(Province = str_replace_all(Province, pattern = '.*\\(From Diamond Princess\\)', replacement = 'Diamond Princess')) %>%

  # Convert ('Diamond Princess cruise ship' or 'From Diamond  Princess' or 'Cruise Ship') to 'Diamond Princess'
  mutate(Province = str_replace_all(Province, pattern = '(Diamond Princess cruise ship)|(From Diamond Princess)|(Cruise Ship)', replacement = 'Diamond Princess')) %>%

  # Convert 'Grand Princess Cruise Ship' to 'Grand Princess'
  mutate(Province = str_replace_all(Province, pattern = 'Grand Princess Cruise Ship', replacement = 'Grand Princess')) %>%

  # Replace NA values with 'Unassigned Location'
  mutate(Province = replace_na(Province, 'Unassigned Location'))


################## - Standarize COUNTRY data (removing data inconstencies) - ####################
daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert 'Mainland China' to 'China'
  mutate(Country = str_replace_all(Country, pattern = 'Mainland China', replacement = 'China')) %>%

  # Convert "('St. Martin',)" to 'St. Martin'
  mutate(Country = str_replace_all(Country, pattern = "\\(\\'St. Martin\\',\\)", replacement = 'St. Martin')) %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(Country = str_replace_all(Country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))', replacement = 'Congo')) %>%

  # Convert 'Bahamas, The' AND 'The Bahamas' to 'Bahamas'
  mutate(Country = str_replace_all(Country, pattern = '.*Bahamas.*', replacement = 'Bahamas')) %>%

  # Convert 'Gambia, The' AND 'The Gambia' to 'Gambia'
  mutate(Country = str_replace_all(Country, pattern = '.*Gambia*', replacement = 'Gambia')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(Country = str_replace_all(Country, pattern = 'Holy See', replacement = 'Vatican City')) %>%

  # Convert 'Diamond Princess' and 'MS Zaandam' Country to 'Others'
  mutate(Country = str_replace_all(Country, pattern = '(Diamond Princess)|(MS Zaandam)', replacement = 'Others'))

# Write data
# ------------------------------------------------------------------------------
# write_tsv(x = my_data_clean,
#           path = "data/02_my_data_clean.tsv")



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
