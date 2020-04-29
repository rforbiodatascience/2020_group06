# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("leaflet")
library("leaflet.extras")

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



# Basic descriptive and visualization: Time Series
# ------------------------------------------------------------------------------


df_ts %>%
  filter(country == "Denmark") %>%
  ggplot() +
  geom_line(aes(date_observation,total_confirmed_per_mil_pop))





# Wrangle data
# ------------------------------------------------------------------------------

#
df<- df %>%
  mutate(time2admin = date_admission_hospital - date_onset)




bl62_pca <- my_data_clean_aug %>%
  select(-X1) %>%
  prcomp(center = TRUE, scale = TRUE)

bl62_pca %>%
  broom::tidy("pcs") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

bl62_pca_aug <- bl62_pca %>%
  broom::augment(my_data_clean_aug)

bl62_pca_aug <- bl62_pca_aug %>%
  mutate(chem_class = get_chem_class(X1))

bl62_pca_aug %>% select(X1, chem_class)


# Model data
# ------------------------------------------------------------------------------
my_data_clean_aug # %>% ...


# Visualise data
# ------------------------------------------------------------------------------

bl62_pca_aug_plt <- bl62_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, label = X1, colour = chem_class)) +
  geom_text() +
  theme(legend.position = "bottom")


# Write data
# ------------------------------------------------------------------------------
#write_tsv(...)
ggsave(path = "./results",
       filename = "04_plot.png",
       plot = bl62_pca_aug_plt,
       width = 10,
       height = 6)
