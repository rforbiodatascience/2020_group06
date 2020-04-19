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
my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")


# Wrangle data
# ------------------------------------------------------------------------------
my_data_clean_aug <- my_data_clean # %>% ...


########################### - Perform BINNING on AGE data (for consistency with our other datasets) - ###########################

# Within this dataset, we

patient_data_first_df %>%
  group_by(age) %>%
  count() %>%
  print(n = Inf)

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")
