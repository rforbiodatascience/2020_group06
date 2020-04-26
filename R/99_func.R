# Define project functions
# ------------------------------------------------------------------------------

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
