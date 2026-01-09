# -----------------------------------------------------------------------------#
# 05_estimations.R
# Purpose: Run DID estimations and save standardized outputs
# Inputs : data/processed/fertility_disaster.rds
#          data/raw/CLASS.xlsx  (income groups)
# Outputs: outputs/estimates/all_estimations.rds
#          outputs/estimates/*.csv (optional tidy exports)
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(
  tidyverse, purrr, readr, here,
  DIDmultiplegtDYN, readxl
)

# ---- paths ----
paths <- list(
  in_data       = here("data", "processed", "fertility_disaster.rds"),
  in_income_xls = here("data", "raw", "CLASS.xlsx"),          # adjust if needed
  out_dir       = here("outputs", "estimates"),
  out_rds       = here("outputs", "estimates", "all_estimations.rds"),
  out_csv   = here("outputs", "estimates")
)

#---------------------------------#
# Data reading ----
#---------------------------------#
data <- readRDS(paths$in_data)

#---------------------------------#
# Functions ----
#---------------------------------#

# 1) Central DID estimator (fixed signature)
estimate_did_model <- function(df, treatment, normalized = TRUE,
                               effects = 15, placebo = 5) {
  did_multiplegt_dyn(
    df = df,
    outcome = "fertility_rate",
    group = "iso3",
    time = "year",
    treatment = treatment,
    effects = effects,
    placebo = placebo,
    normalized = normalized,
    graph_off = TRUE
  )
}

# 2) Extract event-study (effects + placebos + t=0 row) into a tidy tibble
tidy_event_study <- function(model, treatment, spec_id = NA_character_,
                             window_start = NA_integer_, window_end = NA_integer_,
                             subgroup_var = NA_character_, subgroup = NA_character_) {
  
  eff <- model$results$Effects %>%
    as_tibble() %>%
    mutate(event_time = 1:model$results$N_Effects)
  
  pla <- model$results$Placebos %>%
    as_tibble() %>%
    mutate(event_time = -1 * (1:model$results$N_Placebos))
  
  bind_rows(
    eff,
    pla,
    tibble(
      Estimate = 0, SE = 0, `LB CI` = NA_real_, `UB CI` = NA_real_,
      N = 0, Switchers = 0, N.w = 0, Switchers.w = 0,
      event_time = 0
    )
  ) %>%
    arrange(event_time) %>%
    transmute(
      spec        = spec_id,
      treatment   = treatment,
      event_time  = event_time,
      estimate    = Estimate,
      se          = SE,
      ci_low      = `LB CI`,
      ci_high     = `UB CI`,
      n           = N,
      switchers   = Switchers,
      n_w         = N.w,
      switchers_w = Switchers.w,
      window_start = window_start,
      window_end   = window_end,
      subgroup_var = subgroup_var,
      subgroup     = subgroup
    )
}

# 3) Runner for one treatment/spec
run_one <- function(df, treatment, spec_id,
                    window_start = NA_integer_, window_end = NA_integer_,
                    subgroup_var = NA_character_, subgroup = NA_character_,
                    normalized = TRUE, effects = 15, placebo = 5) {
  
  model <- estimate_did_model(
    df = df,
    treatment = treatment,
    normalized = normalized,
    effects = effects,
    placebo = placebo
  )
  
  tidy_event_study(
    model = model,
    treatment = treatment,
    spec_id = spec_id,
    window_start = window_start,
    window_end = window_end,
    subgroup_var = subgroup_var,
    subgroup = subgroup
  )
}

# 4) Rolling windows creator
build_windows <- function(start = 1950, end = 2023, width = 20, step = 5) {
  tibble(
    window_start = seq(start, end - width + 1, by = step),
    window_end   = window_start + width 
  )
}

# 5) Utility: identify treatment vectors safely
get_treatments_std <- function(df) {
  affected_vars_std <- names(df)[str_detect(names(df), "^ln_affected_rate.*_std$")] %>%
    setdiff(c("ln_affected_rate_std", "ln_affected_rate_climate_std", "ln_affected_rate_no_climate_std"))
  
  death_vars_std <- names(df)[str_detect(names(df), "^ln_death_rate.*_std$")] %>%
    setdiff(c("ln_death_rate_std", "ln_death_rate_climate_std", "ln_death_rate_no_climate_std"))
  
  list(
    affected_vars_std = affected_vars_std,
    death_vars_std    = death_vars_std,
    all_std           = c(affected_vars_std, death_vars_std)
  )
}

#---------------------------------#
# Estimations ----
#---------------------------------#

treats <- get_treatments_std(data)

# (A) Main: climatic vs non-climatic (affected & deaths)
main_treatments <- c(
  "ln_affected_rate_climate_std",
  "ln_affected_rate_no_climate_std",
  "ln_death_rate_climate_std",
  "ln_death_rate_no_climate_std"
) %>% intersect(names(data)) # only keep existing columns

res_main <- map_dfr(main_treatments, \(tr) {
  run_one(
    df = data,
    treatment = tr,
    spec_id = paste0("main_", tr),
    normalized = TRUE
  )
})

# (B) By disaster category (all disaggregated std treatments)
# NOTE: this will run 2*(8 categories) models. Can take time.
res_by_category <- map_dfr(treats$all_std, \(tr) {
  run_one(
    df = data,
    treatment = tr,
    spec_id = paste0("by_category_", tr),
    normalized = TRUE
  )
})

# (C) Rolling windows (20y from 1950–2023 every 5y) for main treatments only
windows <- build_windows(start = 1950, end = 2023, width = 20, step = 5) %>% 
  add_row(
    window_start = 2005,
    window_end   = 2023
  )

res_windows <- pmap_dfr(windows, \(window_start, window_end) {
  df_w <- data %>% filter(year >= window_start, year <= window_end)
  
  map_dfr(main_treatments, \(tr) {
    run_one(
      df = df_w,
      treatment = tr,
      spec_id = paste0("window_", window_start, "_", window_end, "_", tr),
      window_start = window_start,
      window_end = window_end,
      normalized = TRUE
    )
  })
})

# (D) Income group split (optional): Rich vs Poor
# This block will:
# 1) try to read income file
# 2) join to data
# 3) run main treatments by subgroup
res_income <- tibble()

income_map <- readxl::read_excel(paths$in_income_xls) %>%
  rename(iso3 = Code, income_group = `Income group`) %>%
  select(iso3, income_group) %>%
  mutate(group_income = if_else(
    income_group %in% c("High income", "Upper middle income"),
    "Rich", "Poor"
  )) %>%
  select(iso3, group_income)

data_inc <- data %>%
  left_join(income_map, by = "iso3") 

groups <- data_inc %>%
  distinct(group_income) %>%
  filter(!is.na(group_income)) %>%
  pull(group_income)

res_income <- map_dfr(groups, \(g) {
  df_g <- data_inc %>% filter(group_income == g)
  
  map_dfr(main_treatments, \(tr) {
    run_one(
      df = df_g,
      treatment = tr,
      spec_id = paste0("income_", g, "_", tr),
      subgroup_var = "group_income",
      subgroup = g,
      normalized = TRUE
    )
  })
})


#---------------------------------#
# Save outputs ----
#---------------------------------#

results <- list(
  meta = list(
    created_at = as.character(Sys.time()),
    model = "did_multiplegt_dyn (DIDmultiplegtDYN)",
    outcome = "fertility_rate",
    effects = 15,
    placebo = 5,
    normalized = TRUE,
    main_treatments = main_treatments,
    n_obs = nrow(data),
    n_countries = n_distinct(data$iso3),
    years = range(data$year, na.rm = TRUE)
  ),
  main_climate_vs_nonclimate = res_main,
  by_disaster_category       = res_by_category,
  rolling_windows_20y        = res_windows,
  by_income_group            = res_income
)

saveRDS(results, paths$out_rds)

# Optional: export tidy CSVs for quick inspection
write_csv(res_main,        file.path(paths$out_csv, "main_climate_vs_nonclimate.csv"))
write_csv(res_by_category, file.path(paths$out_csv, "by_disaster_category.csv"))
write_csv(res_windows,     file.path(paths$out_csv, "rolling_windows_20y.csv"))
write_csv(res_income,    file.path(paths$out_csv, "by_income_group.csv"))

message("Saved all estimation outputs to: ", paths$out_rds)
message("CSV exports in: ", paths$out_csv)
