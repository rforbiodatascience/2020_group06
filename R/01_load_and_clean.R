
################################################################################
########################### - Clear workspace - ################################
################################################################################

rm(list = ls())

################################################################################
############################ - Load libraries - ################################
################################################################################

library(tidyverse)
library(lubridate)
library(testthat)

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

################################# - RENAME & MUTATE COLUMNS - #################################

daily_covid_trends_df <-
  df1 %>%
  rename("id_key" = "SNo") %>%
  mutate(ObservationDate = mdy(ObservationDate)) %>%
  rename("date_observation" = "ObservationDate") %>%
  rename("province" = `Province/State`) %>%
  rename("country" = `Country/Region`) %>%
  select(-`Last Update`) %>%
  rename("total_confirmed" = 'Confirmed') %>%
  rename("total_deaths" = 'Deaths') %>%
  rename("total_recovered" = 'Recovered')

################################# - Standarize PROVINCE data (removing data inconstencies) - ###################################

daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert patterns of the type 'Pierce County, WA' to 'Pierce County'
  mutate(province = str_replace_all(province, pattern = '(.*), \\w\\.?\\w\\.?(.*)', replacement = '\\1\\2')) %>%

  # Convert patterns of the type 'Pierce County' to 'Pierce'
  mutate(province = str_replace_all(province, pattern = '(.*) County(.*)', replacement = '\\1\\2')) %>%

  # Fix problems associated with the Shanxi province
  mutate(province = str_replace_all(province, pattern = 'Shaanxi', replacement = 'Shanxi')) %>%

  # Fix problems associated with UK
  mutate(province = str_replace_all(province, pattern = 'United Kingdom', replacement = 'UK')) %>%

  # Convert patterns of the type 'Travis (From Diamond Princess)' to 'Diamond Princess'
  mutate(province = str_replace_all(province, pattern = '.*\\(From Diamond Princess\\)', replacement = 'Diamond Princess')) %>%

  # Convert ('Diamond Princess cruise ship' or 'From Diamond  Princess' or 'Cruise Ship') to 'Diamond Princess'
  mutate(province = str_replace_all(province, pattern = '(Diamond Princess cruise ship)|(From Diamond Princess)|(Cruise Ship)', replacement = 'Diamond Princess')) %>%

  # Convert 'Grand Princess Cruise Ship' to 'Grand Princess'
  mutate(province = str_replace_all(province, pattern = 'Grand Princess Cruise Ship', replacement = 'Grand Princess')) %>%

  # Convert 'Grand Princess Diamond Princess' to 'Unassigned Location'
  mutate(province = str_replace_all(province, pattern = 'Grand Princess Diamond Princess', replacement = 'Unassigned Location')) %>%

  # Convert 'Unknown Location' to 'Unassigned Location'
  mutate(province = str_replace_all(province, pattern = 'Unknown Location', replacement = 'Unassigned Location')) %>%

  # Replace NA values with 'Unassigned Location'
  mutate(province = replace_na(province, 'Unassigned Location'))


################################# - Standarize COUNTRY data (removing data inconstencies) - ###################################

daily_covid_trends_df <-
  daily_covid_trends_df %>%

  # Convert 'Mainland China' to 'China', for consistency with the other datasets
  mutate(country = str_replace_all(country, pattern = 'Mainland China', replacement = 'China')) %>%

  # Convert "('St. Martin',)" to 'St. Martin'
  mutate(country = str_replace_all(country, pattern = "\\(\\'St. Martin\\',\\)", replacement = 'St. Martin')) %>%

  # Convert Congo (Brazzaville) AND Congo (Kinshasa) to 'Congo'
  mutate(country = str_replace_all(country, pattern = '(Congo \\(Brazzaville\\))|(Congo \\(Kinshasa\\))', replacement = 'Congo')) %>%

  # Convert 'Bahamas, The' AND 'The Bahamas' to 'Bahamas'
  mutate(country = str_replace_all(country, pattern = '.*Bahamas.*', replacement = 'Bahamas')) %>%

  # Convert 'Gambia, The' AND 'The Gambia' to 'Gambia'
  mutate(country = str_replace_all(country, pattern = '.*Gambia.*', replacement = 'Gambia')) %>%

  # Convert 'Holy See' to 'Vatican City'
  mutate(country = str_replace_all(country, pattern = 'Holy See', replacement = 'Vatican City')) %>%

  # Convert 'United Arab Emirates' to 'UAE', for consistency with the other datasets
  mutate(country = str_replace_all(country, pattern = 'United Arab Emirates', replacement = 'UAE')) %>%

  # Convert 'Diamond Princess' and 'MS Zaandam' Country to 'Others'
  mutate(country = str_replace_all(country, pattern = '(Diamond Princess)|(MS Zaandam)', replacement = 'Others'))


################################# - Fix MISSING VALUES in PROVINCE column - ###################################

# Here, if the province is unknown, we just assign it to the name of the country
daily_covid_trends_df <-
  daily_covid_trends_df %>%
  mutate(province = if_else(province == 'Unassigned Location',
                            true = country,
                            false = province))

################################# - Use TESTS for Primary Key uniqueness - ###################################

test_that("IDKey is unique for each row in the dataset", {
  expect_true(
    daily_covid_trends_df %>%
      group_by(id_key) %>%
      count(id_key) %>%
      filter(n > 1) %>%
      nrow() == 0,

    info = 'The Primary ID Key of daily_covid_trends_df is not always unique!'
  )
})

################################# - Converting FACTORS - ###################################

daily_covid_trends_df <-
  daily_covid_trends_df %>%
  mutate(province = factor(province)) %>%
  mutate(country = factor(country))


###################################################################################################
################### - Wrangle data from 'df2': COVID19_line_list_data.csv - #######################
###################################################################################################

print('The structure of df2 is: ')
str(df2)

################################ - REMOVING UNNECESSARY COLUMNS - #############################
patient_data_first_df <-
  df2 %>%

  # Remove empty columns
  select(-c(X4, X22, X23, X24, X25, X26, X27)) %>%

  # Remove 'summary' column, since its data is already summarized through the other columns
  select(-summary) %>%

  # Remove non-relevant columns for our analysis
  select(-c(source, link, If_onset_approximated, case_in_country))


################################# - RENAME & MUTATE COLUMNS - #################################

patient_data_first_df <-
  patient_data_first_df %>%

  # Rename the ID key
  rename("id_key" = "id") %>%

  # Rename the province
  rename("province" = "location") %>%

  # Rename the date of reported infection and convert to date
  mutate(`reporting date` = mdy(`reporting date`)) %>%
  rename("date_reported" = `reporting date`) %>%

  # Rename the date of onset and convert to date
  rename("date_onset" = "symptom_onset") %>%
  mutate(date_onset = mdy(date_onset)) %>%

  # Rename the the date of hospital admission and convert to 'date'
  rename('date_admission_hospital' = 'hosp_visit_date') %>%
  mutate(date_admission_hospital = mdy(date_admission_hospital)) %>%

  # Rename the dates for exposure start and end, and convert them to 'date'
  mutate(exposure_start = mdy(exposure_start)) %>%
  mutate(exposure_end = mdy(exposure_end)) %>%
  rename('date_exposure_start' = 'exposure_start') %>%
  rename('date_exposure_end' = 'exposure_end') %>%

  # Rename and combine the variables related to Wuhan contact
  rename('visited_Wuhan' = `visiting Wuhan`) %>%
  rename('lives_in_Wuhan' = `from Wuhan`) %>%
  mutate(lives_in_Wuhan = replace_na(lives_in_Wuhan, 0)) %>%
  mutate(contact_with_Wuhan = if_else( visited_Wuhan + lives_in_Wuhan > 0, true = 1, false = 0)) %>%

  # Remove the old Wuhan-related columns and reorder the columns in the dataframe
  select(-c(visited_Wuhan, lives_in_Wuhan)) %>%
  select(1:10, 14, 11:13) %>%

  # Rename and fix some data errors for the death and recovered logical variables
  rename("is_dead" = "death") %>%
  rename("is_recovered" = "recovered") %>%
  mutate(is_recovered = if_else(is_recovered != 0, true = 1, false = 0)) %>%
  mutate(is_dead = if_else(is_dead != 0, true = 1, false = 0)) %>%

  # Renaming the symptoms set
  rename("symptoms_set" = "symptom")


################################# - Standarize PROVINCE data (removing data inconstencies) - ###################################

patient_data_first_df <-
  patient_data_first_df %>%

  # Convert patterns of the type: 'Hechi, Guangxi' to 'Guangxi'
  mutate(province = str_replace_all(province, pattern = '(.*), (.*)', replacement = '\\2')) %>%

  # Convert patterns of the type: 'Fukuoka Prefecture' or 'Fukuoka City' to 'Fukuoka'
  mutate(province = str_replace_all(province, pattern = '(.*) ((Prefecture)|(City))', replacement = '\\1')) %>%

  # Fix problems associated with the Shanxi province
  mutate(province = str_replace_all(province, pattern = '(.*) \\(.*\\)', replacement = '\\1')) %>%
  mutate(province = str_replace_all(province, pattern = 'Shaanxi', replacement = 'Shanxi'))


################################# - Standarize COUNTRY data (removing data inconstencies) - ###################################

patient_data_first_df <-
  patient_data_first_df %>%

  # Convert 'US' to 'USA', for consistency with the other datasets
  mutate(country = str_replace_all(country, pattern = 'USA', replacement = 'US'))


################################# - Fix small values of AGE data - ###################################

patient_data_first_df <-
  patient_data_first_df %>%

  # Fix any age values smaller than one to be 1 years old
  mutate(age = if_else(age < 1, true = 1, false = age))


################################# - Wrangle the SYMPTOMS_SET string data - ###################################

patient_data_first_df <-
  patient_data_first_df %>%

  # Fix patterns of the type 'X with Y' to 'X, Y'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(.*) with (.*)', replacement = '\\1, \\2')) %>%

  # Fix problems associated with 'chest pain'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(chest) discomfort', replacement = '\\1 pain')) %>%

  # Fix problems associated with 'dyspnea' (i.e. shortness of breath)
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(shortness of breath)|(difficulty breathing)|(difficult in breathing)|(breathlessness)|(respiratory distress)', replacement = 'dyspnea')) %>%

  # Fix problems associated with 'chills'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(chill,)|(cold,)', replacement = 'chills,')) %>%

  # Fix problems associated with 'sore throat'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(itchy throat)|(throat discomfort)', replacement = 'sore throat')) %>%

  # Fix problems associated with 'fever'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(feaver)|(feve\\\\)|(high fever)|(mild fever)', replacement = 'fever')) %>%

  # Fix problems associated with 'muscle pain'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(muscle ((cramps)|(aches)))|(myalgia(s)?)|(aching muscles)|(sore body)|(physical discomfort)', replacement = 'muscle pain')) %>%

  # Fix problems associated with 'heavy head'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = 'heavy head', replacement = 'headache, nausea')) %>%

  # Fix problems associated with 'runny nose' (i.e. nasal discharge or snot = mucus from the nose) [vs (sputum = mucus from the airways)]
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(runny nose)|(sneeze)', replacement = 'nasal discharge')) %>%

  # Fix problems associated with 'flu symptoms'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(flu)\\W$', replacement = '\\1 symptoms')) %>%

  # Fix problems associated with 'cough'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(mild )?cough(ing)?', replacement = 'cough')) %>%

  # Fix problems associated with 'fatigue'
  mutate(symptoms_set = str_replace_all(symptoms_set, pattern = '(tired)', replacement = 'fatigue'))


################################# - Use TESTS for Primary Key uniqueness - ###################################

test_that("IDKey is unique for each row in the dataset", {
  expect_true(
    patient_data_first_df %>%
      group_by(id_key) %>%
      count(id_key) %>%
      filter(n > 1) %>%
      nrow() == 0,

    info = 'The Primary ID Key of patient_data_first_df is not always unique!'
  )
})

################################# - Converting FACTORS - ###################################

patient_data_first_df <-
  patient_data_first_df %>%
  mutate(province = factor(province)) %>%
  mutate(country = factor(country))


###################################################################################################
################### - Wrangle data from 'df3': .......................... - #######################
###################################################################################################

# TODO: MORE WRANGLING

# patient_data_first_df %>% select(province) %>% anti_join(y = daily_covid_trends_df, by = c('province' = 'province')) %>% group_by(province) %>% count() %>% print(n = Inf)
# daily_covid_trends_df %>% select(province) %>% anti_join(y = patient_data_first_df, by = c('province' = 'province')) %>% group_by(province) %>% count() %>% print(n = Inf)
#
# patient_data_first_df %>% select(province) %>% group_by(province) %>% count() %>% print(n = Inf)
# daily_covid_trends_df %>% select(province) %>% group_by(province) %>% count() %>% print(n = Inf)
#
# patient_data_first_df %>% select(country) %>% group_by(country) %>% count() %>% print(n = Inf)
# daily_covid_trends_df %>% select(country) %>% group_by(country) %>% count() %>% print(n = Inf)
#
# patient_data_first_df %>%
#   select(symptoms_set) %>%
#   separate_rows(symptoms_set, sep = ', ') %>%
#   group_by(symptoms_set) %>%
#   count() %>%
#   print(n = Inf)

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
