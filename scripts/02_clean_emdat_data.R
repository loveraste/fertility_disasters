# -----------------------------------------------------------------------------#
# 02_clean_emdat_data.R
# Purpose: Clean EM-DAT and build annual disaster measures
# Inputs : data/emdat/<your_emdat_file>.xlsx
#          data/processed/un_wpp2024_country_year.rds (for population2010)
# Outputs: data/processed/disasters.rds
#          data/processed/disasters_climatic.rds
#          data/processed/disasters_category.rds
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(tidyverse, readxl, here, janitor)

# ---- paths ----
paths <- list(
  emdat_raw = here("data", "raw", "emdat_natural_1950_2023.xlsx"), # <- adjust name if necessary
  un_proc   = here("data", "processed", "un_wpp2024.rds"),
  out_dis  = here("data",  "processed", "disasters.rds"),
  out_clim  = here("data",  "processed", "disasters_climatic.rds"),
  out_cat  = here("data",  "processed", "disasters_category.rds")
)

#---------------------------------#
# Data reading and cleaning ----
#---------------------------------#

# read EM-DAT
emdat <- readxl::read_excel(paths$emdat_raw) %>%
  janitor::clean_names() %>%
  transmute(
    iso3 = iso,
    location,
    year  = start_year,
    month = start_month,
    subgroup = disaster_subgroup,
    type     = disaster_type,
    subtype  = disaster_subtype,
    deaths = total_deaths,
    num_affected = total_affected
  ) %>%
  filter(!(type %in% c("Glacial lake outburst flood", "Impact", "Fog"))) %>%
  mutate(
    affected = rowSums(across(c(num_affected, deaths)), na.rm = TRUE),
    category = case_when(
      subgroup == "Biological" ~ "biological",
      subgroup == "Geophysical" ~ "geophysical",
      subgroup == "Hydrological" ~ "hydrological",
      type == "Wildfire" ~ "wildfire",
      type == "Drought" ~ "drought",
      subtype %in% c("Cold wave", "Severe winter conditions") ~ "cold_wave",
      subtype == "Heat wave" ~ "heat_wave",
      type == "Storm" ~ "storm",
      TRUE ~ NA_character_
    )
  ) %>%
  select(iso3, year, month, category, deaths, affected) %>%
  filter(year >= 1950 & year <= 2023, !is.na(iso3), !is.na(year), !is.na(category))

# ---- population2010 from UN processed ----
un_data <- readRDS(paths$un_proc)
pop2010 <- un_data %>%
  filter(year == 2010) %>%
  distinct(iso3, population2010 = population)

#---------------------------------#
# Data grouping ----
#---------------------------------#

# ---- annual totals by country-year ----
data <- emdat %>%
  mutate(total = 1) %>%
  group_by(iso3, year) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    affected = sum(affected, na.rm = TRUE),
    .groups = "drop"
  )

disasters <- expand_grid(
  iso3 = unique(pop2010$iso3),
  year = 1950:2023
) %>%
  left_join(data, by = c("iso3", "year")) %>%
  left_join(pop2010, by = "iso3") %>%
  replace_na(list(total = 0, deaths = 0, affected = 0)) %>%
  mutate(
    death_rate = (deaths / population2010) * 100,
    affected_rate = (affected / population2010) * 100,
    ln_death_rate = log(death_rate + 1),
    ln_affected_rate = log(affected_rate + 1)
  )

# ---- climatic vs non-climatic ----
data_climatic <- emdat %>%
  mutate(
    climatic = ifelse(category %in% c("biological", "geophysical"), "no_climate", "climate"),
    total = 1
  ) %>%
  group_by(iso3, year, climatic) %>%
  summarise(
    total = sum(total),
    deaths = sum(deaths, na.rm = TRUE),
    affected = sum(affected, na.rm = TRUE),
    .groups = "drop"
  )

disasters_climatic <- expand_grid(
  iso3 = unique(pop2010$iso3),
  climatic = c("climate", "no_climate"),
  year = 1950:2023
) %>%
  left_join(data_climatic, by = c("iso3", "year", "climatic")) %>%
  left_join(pop2010, by = "iso3") %>%
  replace_na(list(total = 0, deaths = 0, affected = 0)) %>%
  mutate(
    death_rate = (deaths / population2010) * 100,
    affected_rate = (affected / population2010) * 100,
    ln_death_rate = log(death_rate + 1),
    ln_affected_rate = log(affected_rate + 1)
  ) %>%
  select(-population2010) 

#climatic wide (used only for merging into all_data)
disasters_climatic_wide <- disasters_climatic %>%  select(-total) %>% 
  pivot_wider(names_from = climatic, values_from = c(deaths, death_rate, affected, affected_rate, ln_affected_rate, ln_death_rate), values_fill = 0)

# ---- by category ----
data_category <- emdat %>%
  mutate(total = 1) %>%
  group_by(iso3, year, category) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    affected = sum(affected, na.rm = TRUE),
    .groups = "drop"
  )

disasters_category <- expand_grid(
  iso3 = unique(pop2010$iso3),
  category = unique(emdat$category),
  year = 1950:2023
) %>%
  left_join(data_category, by = c("iso3", "year", "category")) %>%
  left_join(pop2010, by = "iso3") %>%
  replace_na(list(total = 0, deaths = 0, affected = 0)) %>%
  mutate(
    death_rate = (deaths / population2010) * 100,
    affected_rate = (affected / population2010) * 100,
    ln_death_rate = log(death_rate + 1),
    ln_affected_rate = log(affected_rate + 1)
  ) %>%
  select(-population2010) 

# category wide (used only for merging into all_data)
disasters_category_wide <- disasters_category %>%  
  pivot_wider(names_from = category, values_from = c(deaths, death_rate, affected, affected_rate, ln_affected_rate, ln_death_rate), values_fill = 0)

#--------------------------------------------#
# Merge all wide data and save ----
#--------------------------------------------#

all_data <- disasters %>% 
  left_join(disasters_category_wide, join_by(iso3, year)) %>% 
  left_join(disasters_climatic_wide,join_by(iso3, year)) 

saveRDS(all_data, paths$out_dis)
saveRDS(disasters_climatic, paths$out_clim)
saveRDS(disasters_category, paths$out_cat)

message("EMDAT data cleaned and saved to: ", paths$out_dis)
message("EMDAT data cleaned and saved to: ", paths$out_clim)
message("EMDAT data cleaned and saved to: ", paths$out_cat)
