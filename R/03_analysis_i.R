# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("leaflet")
library("leaflet.extras")
library("ggpubr")
library("broom")
library("purrr")
library("rpart")
library("rpart.plot")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_func.R")


# Load data
# ------------------------------------------------------------------------------
df_patient <- read_csv(file = "data/_augmented/final_patient_data_df_augm.csv")
df_ts <- read_csv(file = "data/_augmented/final_ts_world_df_augm.csv")


# Basic descriptive and visualization: Patient data
# ------------------------------------------------------------------------------


### Distribution of age group by gender

df_patient %>%
  group_by(age_group,gender) %>%
  tally() %>%
  collect() %>%
  drop_na(gender,age_group) %>%
  arrange(desc(age_group)) %>%
  ggplot() +
  geom_col(aes(x = age_group, y = n, fill = gender)) +
  labs(title = "Distribution of age group by gender",
       subtitle= "COVID-19 affected", x = "Age group", y = "Count")


### Smoothing of time 2 admin as a function of age, grouped by gender and dead

df_patient %>%
  mutate(time2admis = as.integer(date_admission_hospital - date_onset)) %>%
  select(gender,age,time2admis,is_dead,contact_with_Wuhan) %>%
  drop_na(gender,age,time2admis,is_dead, contact_with_Wuhan) %>%
  ggplot() +
  geom_point(aes(age,time2admis, color=gender)) +
  geom_smooth(aes(age,time2admis)) +
  facet_grid(contact_with_Wuhan~.,
             labeller = label_both, scales = "free") +
  ylim(0,30) +
  labs(title = "From onset to hospital admission",
       subtitle= "COVID-19 affected", x = "Age",
       y = "Day(s)")



### Boxplot of age range, by is_dead, contact with wuhan and had a fever

df_patient %>%
  select(gender,age,dyspnea,is_dead,contact_with_Wuhan) %>%
  drop_na(gender,age,dyspnea,is_dead, contact_with_Wuhan) %>%
  ggplot() +
  geom_boxplot(aes(as.factor(dyspnea),age, fill=gender)) +
  facet_grid(contact_with_Wuhan~is_dead,
             labeller = label_both, scales = "free") +
  labs(title = "Age distribution by symptoms, death and contact with wuhan",
       subtitle= "COVID-19 affected", x = "Dyspnea", y = "Age")



### Barplot in polar coordinates of incidents pr. country above 100

df_patient %>%
  group_by(country) %>%
  tally() %>%
  filter(country != "China",n > 100) %>%
  collect() %>%
  ggplot() +
  geom_bar(aes(country, n,fill = country), stat = "identity") +
  coord_polar(start = 300) +
  labs(title = "Numbers of cases (above 100) between Jan-feb 2020",
       subtitle= "COVID-19 affected", x = "", y = "Count")


### Barplot of the symptoms (only when counts > 10 for visualization purposes)

df_patient %>%
  select(chills:thirst) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE) %>%
  gather(symptoms,counts,chills:thirst) %>%
  filter(counts > 10) %>%
  ggplot(aes(reorder(symptoms,counts),counts,fill = symptoms)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + ylim(0,650) +
  labs(title = "Prevalence of symptoms",
       subtitle= "Observed in more than 10 cases",
       x = "Symptoms", y = "Count")


### Heatmap of cases (without china)

df_patient %>%
  #filter(country != "China") %>%
  drop_na(long,lat) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addHeatmap(lng = ~long, lat = ~lat,
             blur = 9, max = 0.05, radius = 6) %>%
  addMarkers(clusterOptions =
               markerClusterOptions())

# labels not possible!
# labs(title = "Confirmed cases",
#     subtitle= "COVID-19 affected",
#     x = "", y = "")



# Visualization: Time Series
# ------------------------------------------------------------------------------


########## Comparison of Denmark and Sweden ##########


##### Comparing total confirmed
p1<- df_ts %>%
  filter(country %in% c("Denmark","Sweden")) %>%
  ggplot() +
  geom_line(aes(date_observation,total_confirmed,color = country)) +
  theme(legend.position = "none")


##### Comparing total confirmed per mill. population
p2<- df_ts %>%
  filter(country %in% c("Denmark","Sweden")) %>%
  ggplot() +
  geom_line(aes(date_observation,total_confirmed_per_mil_pop ,color = country))


##### Comparing total deaths
p3<- df_ts %>%
  filter(country %in% c("Denmark","Sweden")) %>%
  ggplot() +
  geom_line(aes(date_observation,total_deaths,color = country)) +
  theme(legend.position = "none")


##### Comparing total deaths per mill. population
p4<- df_ts %>%
  filter(country %in% c("Denmark","Sweden")) %>%
  ggplot() +
  geom_line(aes(date_observation,total_deaths_per_mil_pop,color = country))


##### Plotting both of the total death
ggarrange(p3, p4, ncol=1, nrow=2, align = "v")


##### Plotting all 4 plots together; confirmed, death + per mill. pop.
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, align = "h", common.legend = TRUE
)



###############################################################################
## FROM HERE - ONLY FROM 2020-03-11 ARE SHOWN DUE TO NUMBERS OF OBSERVATIONS ##
###############################################################################


########## Comparison of Denmark, Sweden, Romania, Turkey and Philippines##########


### Comparing Denmark, Sweden, Romania, Turkey and Philippines per mill. population
df_ts %>%
  filter(country %in% c("Denmark","Sweden", "Romania","Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>% # Starting here due to observation
  ggplot() +
  geom_line(aes(date_observation, total_confirmed_per_mil_pop, color = country)) +
  labs(title = "Confirmed cases per million population",
       subtitle= "COVID-19 affected",
       x = "Date", y = "Count per million population")

### Total deaths per mil pop
df_ts %>%
  filter(country %in% c("Denmark","Sweden", "Romania","Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>% # Starting here due to observation
  ggplot() +
  geom_line(aes(date_observation, total_deaths_per_mil_pop, color = country)) +
  labs(title = "Death(s) per million population",
       subtitle= "COVID-19 affected",
       x = "Date", y = "Count per million population")


# Model data: Time Series
# ------------------------------------------------------------------------------

#Selecting few countries and nesting per country and time series type
df_ts_selected<- df_ts %>%
  filter(country %in% c("Denmark","Sweden", "Romania","Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-11") %>%
  gather(ts, count, total_confirmed:total_deaths_per_mil_pop) %>%
  group_by(country,ts) %>%
  nest()


# Modelling with linear model
df_ts_models<- df_ts_selected %>%
  mutate(ts_country = str_c(ts, country, sep="_"),
         mdls = map(data, mdl),
         glance = map(mdls,glance),
         tidy = map(mdls,tidy),
         conf = map(mdls,confint_tidy),
         aug = map(mdls,augment))


########## Estimate pr. day of confirmed and death per mil pop ##########


##### Showing model estimate (coefficient) confirmed per mil pop
df_ts_models %>%
  unnest(tidy,conf) %>%
  filter(ts == "total_confirmed_per_mil_pop",term == "date_observation") %>%
  select(ts_country,estimate,conf.low,conf.high) %>%
  ggplot(aes(estimate,ts_country,color = ts_country),show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin= conf.low, xmax = conf.high)) +
  labs(title = "Model evaluation of confirmed cases",
       subtitle= "COVID-19 affected",
       x = "Estimated coefficient", y = "Country")


##### Showing model estimate (coefficient) deaths per mil pop
df_ts_models %>%
  unnest(tidy,conf) %>%
  filter(ts == "total_deaths_per_mil_pop",term == "date_observation") %>%
  select(ts_country,estimate,conf.low,conf.high) %>%
  ggplot(aes(estimate,ts_country,color = ts_country),show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin= conf.low, xmax = conf.high)) +
  labs(title = "Model evaluation of death cases",
       subtitle= "COVID-19 affected",
       x = "Estimated coefficient", y = "Country")


##### Evaluation of the models based on the residuals per country
df_ts_models %>%
  unnest(aug) %>%
  select(count,.resid) %>%
  filter(ts %in% c("total_confirmed_per_mil_pop", "total_deaths_per_mil_pop")) %>%
  ggplot() +
  geom_boxplot(aes(country,.resid, fill = country)) +
  facet_grid(.~ts) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Model evaluation (residuals)",
       subtitle= "COVID-19",
       x = "Country", y = "Residuals")


# Model data: Patient data
# ------------------------------------------------------------------------------

# subsetting data frame for the pca
df_patient_sub<- df_patient %>%
  select(gender,age,contact_with_Wuhan:is_recovered,chills:thirst) %>%
  na.omit() %>%
  mutate(gender = case_when(gender == 'female' ~ 1,
                            gender == 'male' ~ 0))


# Making PCA of the subset and removing columns that only contains zeros - showing variance
df_patient_sub %>%
  select_if(~length(unique(.)) > 1) %>%
  prcomp(center = TRUE, scale. = TRUE) %>%
  tidy("pcs") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col()


# Showing decicion tree ????????
df_patient %>%
  select(country,gender,age,contact_with_Wuhan:is_recovered,
         chills:thirst) %>%
  select_if(~length(unique(.)) > 1) %>% # removing zeros
  mutate(result = case_when(is_dead == 1 ~ 'dead',
                            is_dead == 0 ~ 'still_sick',
                            is_recovered == 1 ~ 'recovered',
                            is_recovered == 0 ~ 'still_sick')) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_if(is.numeric,as.factor) %>%
  mutate(age = as.integer(age)) %>%
  select(-is_dead,-is_recovered) %>%
  rpart(result ~ .,.,method = 'class', model = TRUE,
        minsplit = 2, minbucket = 1, cp = 0.01) %>%
  rpart.plot(extra = 101)


# Performing logistic regression

df_patient_glm<- df_patient %>%
  select(country:age,contact_with_Wuhan:is_recovered,
         chills:thirst) %>%
  na.omit() %>%
  select_if(~length(unique(.)) > 1) %>% # removing zeros
  mutate_if(is.character,as.factor) %>%
  mutate_if(is.numeric,as.factor) %>%
  mutate(age = as.integer(age))


# making model prediction for being dead
df_patient_glm %>%
  glm(is_dead ~ ., ., family = binomial()) %>%
  summary()


# selecting significant variables
df_patient_glm_is_dead<- df_patient_glm %>%
  select(is_dead, gender, age, contact_with_Wuhan, fever)


# showing the summary (estimate + std.error)
df_patient_glm_is_dead %>%
  glm(is_dead ~ ., ., family = binomial()) %>%
  tidy() %>%
  mutate(low = estimate - std.error,
         high = estimate + std.error) %>%
  ggplot(aes(estimate, term, xmin = low, xmax = high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() +
  labs(title = "Model evaluation of logsitic regression",
       subtitle= "COVID-19 affected",
       x = "Estimated coefficient", y = "Parameters")


##### Using augment
df_patient_glm_is_dead %>%
  glm(is_dead ~ ., ., family = binomial) %>%
  augment(type.predict = "response") %>%
  mutate(logit = log(.fitted/(1-.fitted)),
         pred_class = ifelse(.fitted > 0.5, "is_dead","not_dead")) %>%
  ggplot(aes(age,.fitted)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Age") +
  ylab("Probability")



### Plotting hat vs. resid
df_patient_glm_is_dead %>%
  glm(is_dead ~ ., ., family = binomial()) %>%
  augment() %>%
  ggplot(aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + geom_smooth(se = FALSE)


### Plotting .hat vs. cook's distance
df_patient_glm_is_dead %>%
  glm(is_dead ~ ., ., family = binomial()) %>%
  augment() %>%
  ggplot(aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
  geom_smooth(se = FALSE) +
  geom_point() +
  labs(title = "Model evaluation logsitic regression",
       subtitle= "COVID-19 affected",
       x = "Estimated values", y = "Coock's distance")



df_patient_glm_is_dead %>%
  glm(is_dead ~ ., ., family = binomial()) %>%
  glance()



# Write data
# ------------------------------------------------------------------------------

#write_tsv(...)
# ggsave(path = "./results",
#        filename = "04_plot.png",
#        plot = bl62_pca_aug_plt,
#        width = 10,
#        height = 6)

