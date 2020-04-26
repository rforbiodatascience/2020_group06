# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())


# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_func.R")


# Load data
# ------------------------------------------------------------------------------
df <- read_csv(file = "data/_augmented/final_patient_data_df_augm.csv")


# Basic descriptive and visualization
# ------------------------------------------------------------------------------


### Distribution of age group by gender

df %>%
  group_by(age_group,gender) %>%
  tally() %>%
  collect() %>%
  drop_na(gender,age_group) %>%
  arrange(desc(age_group)) %>%
  ggplot() +
  geom_col(aes(x = age_group, y = n, fill = gender)) +
  labs(title = "Distribution of age group by gender", x = "Age group", y = "Count")


### Boxplot of Time from onset to admission between "dead" and "not dead" patients

df %>%
  mutate(time2admin = as.integer(date_admission_hospital - date_onset)) %>%
  drop_na(time2admin) %>%
  group_by(time2admin,is_dead) %>%
  tally() %>%
  collect() %>%
  ggplot() +
  geom_boxplot(aes(as_factor(is_dead),time2admin, fill = as_factor(is_dead))) +
  theme(legend.position="none") +
  labs(title = "Time from onset to admission", x = "Alive: 0, Dead: 1", y = "Days")


### Barplot in polar coordinates of incidents pr. country above 100

df %>%
  group_by(country) %>%
  tally() %>%
  filter(country != "China",n > 100) %>%
  collect() %>%
  ggplot() +
  geom_bar(aes(country, n,fill = country), stat = "identity") +
  coord_polar(start = 300) +
  labs(title = "Counts of incidents above 100", x = "", y = "Count")


### Barplot of the symptoms (only when counts > 10 for visualization purposes)

df %>%
  select(chills:thirst) %>%
  summarise_if(is.numeric,sum,na.rm=TRUE) %>%
  gather(symptoms,counts,chills:thirst) %>%
  filter(counts > 10) %>%
  ggplot(aes(reorder(symptoms,counts),counts,fill = symptoms)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position = "none") + ylim(0,650) +
  labs(title = "Incidents (n > 10) of symptoms",y = "Count", x = "")



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
