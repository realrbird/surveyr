# This script is used to create and save the internal data objects for the package.

library(tibble)
library(dplyr)
library(rlang) # Used for rlang::set_names in the target list generation

# --- 1. Generate the Sample Survey Data (survey_df) ---
# Total sample size
N_sample <- 2000

# Define categories
age_categories <- c('18-30', '31-54', '55+')
sex_categories <- c('male', 'female')
region_categories <- c('Northeast', 'Midwest', 'South', 'West')

# Create a data frame with roughly the target distributions
set.seed(42) # For reproducibility

survey_df <- tibble(
  # Sample individuals according to target proportions (approx 10:20:70 for age)
  age_group = factor(sample(age_categories, size = N_sample, replace = TRUE, prob = c(0.1, 0.2, 0.7)),
                     levels = age_categories),
  # Sample sex (approx 50/50)
  sex_group = factor(sample(sex_categories, size = N_sample, replace = TRUE, prob = c(0.5, 0.5)),
                     levels = sex_categories),
  # Sample region (approx 18:22:38:22)
  region = factor(sample(region_categories, size = N_sample, replace = TRUE, prob = c(0.18, 0.22, 0.38, 0.22)),
                  levels = region_categories),
  # Generate a dummy base weight (total population size is 1000)
  WEIGHT = round((100000 / N_sample) + rnorm(N_sample, mean = 0, sd = 5), 2)
)

# --- 2. Generate the Population Target List (target_list) ---
# Define population counts for 100,000 people
N_pop <- 100000

# Targets (Population size: 100,000)
age_target_data <- tibble(
  age_group = factor(age_categories, levels = age_categories),
  COUNT = c(10000, 20000, 70000) # 10%, 20%, 70%
)

sex_target_data <- tibble(
  sex_group = factor(sex_categories, levels = sex_categories),
  COUNT = c(49000, 51000) # 49%, 51%
)

region_target_data <- tibble(
  region = factor(region_categories, levels = region_categories),
  COUNT = c(18000, 22000, 38000, 22000) # 18%, 22%, 38%, 22%
)

# Combine into the required target_list structure, calculating percentages (COUNT/N_pop * 100)
target_list <- list(
  age_group = age_target_data %>% mutate(Freq = (COUNT / N_pop) * 100),
  sex_group = sex_target_data %>% mutate(Freq = (COUNT / N_pop) * 100),
  region = region_target_data %>% mutate(Freq = (COUNT / N_pop) * 100)
)

# --- 3. Save the Data Objects ---
# Use the function usethis::use_data() to save the objects to the 'data/' directory.
# This makes them available for use in examples, tests, and for the user to load.

usethis::use_data(survey_df, overwrite = TRUE)
usethis::use_data(target_list, overwrite = TRUE)

# The objects will be saved to data/survey_df.rda and data/target_list.rda
# and automatically loaded when the user attaches the 'surveyr' package.



