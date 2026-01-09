# -----------------------------------------------------------------------------#
# 01_clean_un_data.R
# Purpose: Clean and harmonize UN WPP 2024 (Demographic Indicators - Compact)
# Input : data/raw/un/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx
# Output: data/processed/un_wpp2024_country_year.rds
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(readxl, tidyverse, here, janitor)

# ---- paths ----
paths <- list(
  un_raw = here("data", "raw", "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"),
  out_rds = here("data", "processed", "un_wpp2024.rds")
)

#---------------------------------#
# Data reading and cleaning ----
#---------------------------------#

# Read raw UN data
un_raw <- readxl::read_excel(path = paths$un_raw, range = "A17:BM22000", col_types = "text") %>% 
  clean_names()

# Clean and select relevant variables
un_general_data <- un_raw %>%
  filter(type == "Country/Area") %>%
  transmute(
    country = region_subregion_country_or_area,
    iso3 = iso3_alpha_code,
    year = parse_integer(year),
    population = parse_number(total_population_as_of_1_january_thousands),
    fertility_rate = parse_number(total_fertility_rate_live_births_per_woman)
  ) %>%
  arrange(country, year)

# ---- Auxiliary variable: population in 2010 ----
population2010 <- un_general_data %>%
  filter(year == 2010) %>%
  select(iso3, population2010 = population)

un_data <- un_general_data %>%
  left_join(population2010, by = "iso3")

# ---------------------------------#
# Save outputs ----
# ---------------------------------#
saveRDS(un_data, paths$out_rds)
message("UN data cleaned and saved to: ", paths$out_rds)
