# -----------------------------------------------------------------------------#
# 04_descriptives.R
# Purpose: Descriptive statistics and time series plots (fertility + disasters)
# Inputs : data/processed/un_wpp2024.rds
#          data/processed/disasters_climatic.rds
#          data/processed/disasters_category.rds
# Outputs: outputs/figures/*.png (and/or .pdf)
#          outputs/tables/unified_disaster_stats.tex
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(tidyverse, here, kableExtra, stringr)

# ---- paths ----
paths <- list(
  un_proc   = here("data", "processed", "un_wpp2024.rds"),
  dis_clim  = here("data", "processed", "disasters_climatic.rds"),
  dis_cat   = here("data", "processed", "disasters_category.rds"),
  out_fig   = here("outputs", "figures"),
  out_tab   = here("outputs", "tables", "unified_disaster_stats.tex")
)

# ---- theme ----
theme_ts <- theme(
  axis.line = element_line(color = "black"),
  panel.background = element_blank(),
  axis.text = element_text(size = 14, color = "black"),
  axis.title.x = element_text(size = 14, color = "gray18", margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.y = element_text(size = 14, color = "gray18", margin = margin(t = 0, r = 10, b = 0, l = 0))
)

# ---- helpers ----
mean_without_zeros <- function(x) {
  x <- x[x != 0]
  if (length(x) == 0) return(NA_real_)
  mean(x, na.rm = TRUE)
}

sd_without_zeros <- function(x) {
  x <- x[x != 0]
  if (length(x) == 0) return(NA_real_)
  sd(x, na.rm = TRUE)
}


# Generic time-series plot helper
plot_country_ts <- function(df_country, df_world, yvar, ylab, out_file,
                            y_limits = NULL) {
  p <- ggplot() +
    geom_line(
      data = df_country,
      aes(x = year, y = .data[[yvar]], group = iso3),
      color = "lightgrey", alpha = 0.5
    ) +
    geom_line(
      data = df_world,
      aes(x = year, y = .data[[yvar]]),
      color = "red2", linewidth = 0.6
    ) +
    labs(x = "Year", y = ylab) +
    theme_ts +
    scale_x_continuous(expand = c(0, 0), limits = c(1950, 2023))
  
  if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
  
  ggsave(filename = out_file, plot = p, width = 12, height = 8)
}

#---------------------------------#
# Load processed datasets ----
#---------------------------------#

un_data <- readRDS(paths$un_proc)
disasters_climatic <- readRDS(paths$dis_clim)
disasters_category <- readRDS(paths$dis_cat)

fertility <- un_data %>%
  filter(!is.na(fertility_rate)) %>%
  select(iso3, year, fertility_rate, population2010)

# --------------------------#
# Fertility plots ----
# --------------------------#

world_rates <- fertility %>%
  group_by(year) %>%
  summarise(
    fertility_rate_world = weighted.mean(
      x = fertility_rate,
      w = population2010,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

p_fert <- ggplot() +
  geom_line(
    data = fertility,
    aes(x = year, y = fertility_rate, group = iso3),
    color = "lightgrey", alpha = 0.5
  ) +
  geom_line(
    data = world_rates,
    aes(x = year, y = fertility_rate_world),
    color = "red2", linewidth = 0.6
  ) +
  labs(x = "Year", y = "Fertility rate") +
  theme_ts +
  scale_x_continuous(expand = c(0, 0), limits = c(1950, 2023))

ggsave(filename = here(paths$out_fig, "fertility_rate.png"), plot = p_fert, width = 12, height = 8)

#---------------------------------------#
# Disasters: climate vs no_climate ----
#---------------------------------------#

# Climate related disasters
disasters_country_climate <- disasters_climatic %>%
  filter(climatic == "climate") %>%
  group_by(iso3, climatic, year) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    affected_rate = sum(affected_rate, na.rm = TRUE),
    death_rate = sum(death_rate, na.rm = TRUE),
    .groups = "drop"
  )

world_disaster_climate <- disasters_country_climate %>%
  group_by(year) %>%
  summarise(
    total = mean(total, na.rm = TRUE),
    affected_rate = mean(affected_rate, na.rm = TRUE),
    death_rate = mean(death_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

disasters_country_no_climate <- disasters_climatic %>%
  filter(climatic == "no_climate") %>%
  group_by(iso3, climatic, year) %>%
  summarise(
    total = sum(total, na.rm = TRUE),
    affected_rate = sum(affected_rate, na.rm = TRUE),
    death_rate = sum(death_rate, na.rm = TRUE),
    .groups = "drop"
  )

world_disaster_no_climate <- disasters_country_no_climate %>%
  group_by(year) %>%
  summarise(
    total = mean(total, na.rm = TRUE),
    affected_rate = mean(affected_rate, na.rm = TRUE),
    death_rate = mean(death_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# --- Climate plots ---
plot_country_ts(
  df_country = disasters_country_climate,
  df_world   = world_disaster_climate,
  yvar       = "affected_rate",
  ylab       = "Affected rate",
  out_file   = here(paths$out_fig, "affected_rate_country_ts_climate.png"),
  y_limits   = c(0, 5000)
)

plot_country_ts(
  df_country = disasters_country_climate,
  df_world   = world_disaster_climate,
  yvar       = "death_rate",
  ylab       = "Death rate",
  out_file   = here(paths$out_fig, "death_rate_country_ts_climate.png"),
  y_limits   = c(0, 20)
)

plot_country_ts(
  df_country = disasters_country_climate,
  df_world   = world_disaster_climate,
  yvar       = "total",
  ylab       = "Number of disasters",
  out_file   = here(paths$out_fig, "total_disasters_country_ts_climate.png"),
  y_limits   = c(0, 5)
)

# --- No-climate plots ---
plot_country_ts(
  df_country = disasters_country_no_climate,
  df_world   = world_disaster_no_climate,
  yvar       = "affected_rate",
  ylab       = "Affected rate",
  out_file   = here(paths$out_fig, "affected_rate_country_ts_no_climate.png"),
  y_limits   = c(0, 1000)
)

plot_country_ts(
  df_country = disasters_country_no_climate,
  df_world   = world_disaster_no_climate,
  yvar       = "death_rate",
  ylab       = "Death rate",
  out_file   = here(paths$out_fig, "death_rate_country_ts_no_climate.png"),
  y_limits   = c(0, 20)
)

plot_country_ts(
  df_country = disasters_country_no_climate,
  df_world   = world_disaster_no_climate,
  yvar       = "total",
  ylab       = "Number of disasters",
  out_file   = here(paths$out_fig, "total_disasters_country_ts_no_climate.png"),
  y_limits   = c(0, 5)
)

# ---------------------------------------------------------------------------#
# Appendix table: stats by category (events + intensity) ----
# ---------------------------------------------------------------------------#

summary_category <- disasters_category %>%
  mutate(event = ifelse(affected_rate > 0, 1, 0)) %>%
  group_by(category) %>%
  summarise(
    events = sum(event, na.rm = TRUE),
    affected_mean = mean_without_zeros(affected_rate),
    affected_sd   = sd_without_zeros(affected_rate),
    death_mean    = mean_without_zeros(death_rate),
    death_sd      = sd_without_zeros(death_rate),
    .groups = "drop"
  )

climatic_groups <- list(
  climatic     = c("cold_wave", "drought", "heat_wave", "hydrological", "storm", "wildfire"),
  non_climatic = c("biological", "geophysical")
)

summary_category <- summary_category %>%
  mutate(group = case_when(
    category %in% climatic_groups$climatic     ~ "Climatic",
    category %in% climatic_groups$non_climatic ~ "Non-climatic",
    TRUE ~ "Other"
  ))

summary_group <- summary_category %>%
  group_by(group) %>%
  summarise(
    events = sum(events, na.rm = TRUE),
    affected_mean = mean(affected_mean, na.rm = TRUE),
    affected_sd   = mean(affected_sd, na.rm = TRUE),
    death_mean    = mean(death_mean, na.rm = TRUE),
    death_sd      = mean(death_sd, na.rm = TRUE),
    .groups = "drop"
  )

summary_all <- summary_category %>%
  summarise(
    group = "All",
    events = sum(events, na.rm = TRUE),
    affected_mean = mean(affected_mean, na.rm = TRUE),
    affected_sd   = mean(affected_sd, na.rm = TRUE),
    death_mean    = mean(death_mean, na.rm = TRUE),
    death_sd      = mean(death_sd, na.rm = TRUE)
  )

total_events <- sum(summary_category$events, na.rm = TRUE)

summary_category <- summary_category %>%
  mutate(perc_total = round(events / total_events * 100, 2))

summary_group <- summary_group %>%
  mutate(perc_total = round(events / total_events * 100, 2))

summary_all <- summary_all %>%
  mutate(perc_total = 100)

final_table <- bind_rows(summary_all, summary_group, summary_category) %>%
  mutate(
    category = ifelse(is.na(category), "", category),
    category = str_to_title(str_replace_all(category, "_", " ")),
    affected = sprintf("%.2f (%.2f)", affected_mean, affected_sd),
    death    = sprintf("%.2f (%.2f)", death_mean, death_sd)
  )

ord_type <- c("All", "Climatic", "Non-climatic", "Other")
final_table <- final_table %>%
  mutate(
    order_group = factor(group, levels = ord_type),
    order_sub   = ifelse(category == "", 0, 1)
  ) %>%
  arrange(order_group, order_sub, category)

k <- final_table %>%
  select(group, category, events, perc_total, affected, death) %>%
  kable(
    "latex", booktabs = TRUE,
    align = "llcccc",
    col.names = c("Type", "Subcategory", "Events", "\\# % Total",
                  "Affected rate (Mean, SD)", "Death rate (Mean, SD)")
  ) %>%
  kable_styling(latex_options = "HOLD_position")

idx_bold   <- which(final_table$category == "")
idx_indent <- which(final_table$category != "" & final_table$group != "All")

k <- k %>%
  row_spec(idx_bold, bold = TRUE) %>%
  add_indent(idx_indent)

save_kable(k, paths$out_tab)
