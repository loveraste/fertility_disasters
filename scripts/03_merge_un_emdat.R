# -----------------------------------------------------------------------------#
# 03_merge_un_emdat.R
# Purpose: Merge UN fertility with disasters and create standardized variables
# Inputs : data/processed/un_wpp2024_country_year.rds
#          data/processed/annual_disasters_wide.rds
# Output : data/fertility_disaster.rds
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(tidyverse, here)

# ---- paths ----
paths <- list(
  un_proc  = here("data", "processed", "un_wpp2024.rds"),
  dis_proc = here("data", "processed", "disasters.rds"),
  out_rds  = here("data", "processed", "fertility_disaster.rds")
)

#---------------------------------#
# Load processed datasets ----
#---------------------------------#

un_data <- readRDS(paths$un_proc)
disasters <- readRDS(paths$dis_proc)

#------------------------------------------#
# Merge fertility and disaster data ----
#------------------------------------------#

merged_data <- un_data %>%
  select(-population2010) %>% 
  left_join(disasters, by = c("iso3", "year")) %>%
  filter(!is.na(fertility_rate))

# Standardize log-transformed disaster rates 
merged_data <- merged_data %>% 
  mutate(
    across(
      matches("ln_(affected_rate|death_rate)"),
      ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
      .names = "{.col}_std"
    )
  )

saveRDS(merged_data, paths$out_rds)
message("Merged data saved to: ", paths$out_rds)
