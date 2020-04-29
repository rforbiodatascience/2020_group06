# Define project functions
# ------------------------------------------------------------------------------


# Linear model for the time series
mdl <- function(df) {
  lm(count ~ date_observation, data = df)
}






# Sorry that I dumped here some debug code used for the wrangling
# Please put the functions above these lines


# patient_data_first_df %>% select(province) %>% anti_join(y = daily_covid_trends_df, by = c('province' = 'province')) %>% group_by(province) %>% count() %>% print(n = Inf)
# ts_confirmed_world_df %>% select(country) %>% anti_join(y = daily_covid_trends_df, by = c('country' = 'country')) %>% group_by(country) %>% count() %>% print(n = Inf)
#
# patient_data_first_df %>% select(province) %>% group_by(province) %>% count() %>% print(n = Inf)
# daily_covid_trends_df %>% select(province) %>% group_by(province) %>% count() %>% print(n = Inf)
#
# patient_data_first_df %>% select(country) %>% group_by(country) %>% count() %>% print(n = Inf)
# daily_covid_trends_df %>% select(country) %>% group_by(country) %>% count() %>% print(n = Inf)

# patient_data_first_df %>%
#   select(symptoms_set) %>%
#   separate_rows(symptoms_set, sep = ', ') %>%
#   group_by(symptoms_set) %>%
#   count() %>%
#   print(n = Inf)

# na_if(df3, 'N/A')

# patient_data_first_df %>%
#   filter_all(any_vars(str_detect(., 'N/A')))


# patient_data_second_df %>% select(country, province) %>% anti_join(y = daily_covid_trends_df) %>% group_by(country, province) %>% count() %>% print(n = Inf)
#
# ts_confirmed_world_df %>% select(province) %>% group_by(province) %>% count() %>% print(n = Inf)
# patient_data_second_df %>% select(province) %>% anti_join(y = patient_data_first_df, by = c('province' = 'province')) %>% group_by(province) %>% count() %>% print(n = Inf)
#
# ts_confirmed_world_df %>% select(country) %>% group_by(country) %>% count() %>% print(n = Inf)



# unique_symptoms <-
#   final_patient_data_df %>%
#   select(symptoms_set) %>%
#   separate_rows(symptoms_set, sep = ', ') %>%
#   group_by(symptoms_set) %>%
#   distinct() %>%
#   arrange(symptoms_set) %>%
#   filter(is.na(symptoms_set) == FALSE)
#
# final_patient_data_df %>%
#   separate_rows(symptoms_set, sep = ', ') %>%
#   mutate(dummy = 1) %>%
#   pivot_wider(names_from = symptoms_set, values_from = dummy, values_fn = list(dummy = list)) %>% View()
#   # spread(key = symptoms_set, value = dummy, fill = 0)
#
# final_patient_data_df %>%
#   select(symptoms_set) %>%
#   separate(symptoms_set, into = c('s1', 's2', 's3', 's4', 's5', 's6'), sep = ', ') %>%
#   mutate(dummy = 1) %>%
#   pivot_wider(names_from = symptoms_set, values_from = c(s1:s6), values_fill = list(0)) %>% View()
