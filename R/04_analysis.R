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
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data
# ------------------------------------------------------------------------------

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
