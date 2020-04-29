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
  labs(title = "Distribution of age group by gender", x = "Age group", y = "Count")


### Smoothing of time 2 admin as a function of age, grouped by gender and dead

df_patient %>%
  mutate(time2admin = as.integer(date_admission_hospital - date_onset)) %>%
  select(gender,age,time2admin,is_dead,contact_with_Wuhan) %>%
  drop_na(gender,age,time2admin,is_dead, contact_with_Wuhan) %>%
  ggplot() +
  geom_point(aes(age,time2admin, color=gender)) +
  geom_smooth(aes(age,time2admin)) +
  facet_grid(contact_with_Wuhan~.,
             labeller = label_both, scales = "free")


### Boxplot of age range, by is_dead, contact with wuhan and had a fever

df_patient %>%
  select(gender,age,fever,is_dead,contact_with_Wuhan) %>%
  drop_na(gender,age,fever,is_dead, contact_with_Wuhan) %>%
  ggplot() +
  geom_boxplot(aes(as.factor(fever),age, fill=gender)) +
  facet_grid(contact_with_Wuhan~is_dead,
             labeller = label_both, scales = "free") +
  xlab("fever")



### Barplot in polar coordinates of incidents pr. country above 100

df_patient %>%
  group_by(country) %>%
  tally() %>%
  filter(country != "China",n > 100) %>%
  collect() %>%
  ggplot() +
  geom_bar(aes(country, n,fill = country), stat = "identity") +
  coord_polar(start = 300) +
  labs(title = "Counts of incidents above 100", x = "", y = "Count",
       subtitle = "Jan-Feb 2020")


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
  labs(title = "Incidents (n > 10) of symptoms",y = "Count", x = "")


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
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, align = "h")



####################################################################################
########## Comparison of Denmark, Sweden, Romania, Turkey and Philippines ##########


### Comparing Denmark, Sweden, Romania, Turkey and Philippines per mill. population
df_ts %>%
  filter(country %in% c("Denmark","Sweden", "Romania","Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-01") %>% # Starting here due to observation
  ggplot() +
  geom_line(aes(date_observation, total_confirmed_per_mil_pop, color = country))



# Model data: Time Series
# ------------------------------------------------------------------------------

#Selecting few countries and nesting per country and time series type
df_ts_selected<- df_ts %>%
  filter(country %in% c("Denmark","Sweden", "Romania","Turkey","Philippines")) %>%
  filter(date_observation >= "2020-03-01") %>%
  gather(ts, count, total_confirmed:total_deaths_per_mil_pop) %>%
  group_by(country,ts) %>%
  nest()


# Modelling with linear model
df_ts_models<- df_ts_selected %>%
  mutate(ts_country = str_c(ts, country, sep="_"),
         mdls = map(data, mdl),
         glance = map(mdls,glance),
         tidy = map(mdls,tidy),
         conf = map(mdls,confint_tidy))


#########################################################################
########## Estimate pr. day of confirmed and death per mil pop ##########


##### Showing model estimate per confirmed per mil pop
df_ts_models %>%
  unnest(tidy,conf) %>%
  filter(ts == "total_confirmed_per_mil_pop",term == "date_observation") %>%
  select(ts_country,estimate,conf.low,conf.high) %>%
  ggplot(aes(estimate,ts_country,color = ts_country),show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin= conf.low, xmax = conf.high))


##### Showing model estimate per confirmed per mil pop
df_ts_models %>%
  unnest(tidy,conf) %>%
  filter(ts == "total_deaths_per_mil_pop",term == "date_observation") %>%
  select(ts_country,estimate,conf.low,conf.high) %>%
  ggplot(aes(estimate,ts_country,color = ts_country),show.legend = FALSE) +
  geom_point() +
  geom_errorbarh(aes(xmin= conf.low, xmax = conf.high))




# Write data
# ------------------------------------------------------------------------------
#write_tsv(...)
ggsave(path = "./results",
       filename = "04_plot.png",
       plot = bl62_pca_aug_plt,
       width = 10,
       height = 6)
