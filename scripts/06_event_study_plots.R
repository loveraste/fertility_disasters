# -----------------------------------------------------------------------------#
# 06_event_study_plots.R
# Purpose: Create event-study plots from saved estimation outputs (05_estimations)
# Inputs : outputs/estimates/all_estimations.rds
# Outputs: outputs/figures/... (png)
# Notes  : Automatically harmonizes y-axes for comparability
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(tidyverse, here)

# ---- paths ----
paths <- list(
  in_rds  = here("outputs", "estimates", "all_estimations.rds"),
  out_fig = here("outputs", "figures")
)

# ---- theme ----
my_theme <- theme(
  strip.placement = "outside",
  axis.line = element_line(color = "black"),
  axis.text = element_text(size = 14, color = "black"),
  axis.title = element_text(size = 14, color = "gray18"),
  axis.ticks.y = element_blank(),
  legend.key = element_blank(),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 15),
  panel.spacing  = unit(1, "lines"),
  strip.background = element_blank(),
  panel.background = element_blank(), 
  axis.title.x = element_text(size = 15, color = "gray18", margin = margin(t = 5)),
  strip.text = element_text(size = 15))

# ---- helpers ----

round_up_step <- function(x, step = 0.005) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  ceiling(max(x) / step) * step
}

round_down_step <- function(x, step = 0.005) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  floor(min(x) / step) * step
}

compute_y_limits_from_df <- function(df, treatments, step = 0.005) {
  df_use <- df %>%
    filter(treatment %in% treatments)
  
  vals <- df_use %>%
    transmute(
      est = estimate,
      lo  = ci_low,
      hi  = ci_high
    ) %>%
    pivot_longer(everything(), values_to = "v") %>%
    pull(v)
  
  ymin <- round_down_step(vals, step = step)
  ymax <- round_up_step(vals, step = step)
  
  c(ymin, ymax)
}

prep_dual_df <- function(df, treatment1, treatment2,
                         label1 = "Climate", label2 = "Non-climate") {
  df %>%
    filter(treatment %in% c(treatment1, treatment2)) %>%
    mutate(
      group = case_when(
        treatment == treatment1 ~ label1,
        treatment == treatment2 ~ label2,
        TRUE ~ NA_character_
      )
    ) %>%
    select(event_time, estimate, ci_low, ci_high, group)
}

graph_main <- function(df, treatment1, treatment2,
                                          color1 = "black", color2 = "#b2182b",
                                          label1 = "Climate", label2 = "Non-climate",
                                          y_limits = NULL, save_path = NULL) {
  
  df_plot <- prep_dual_df(
    df = df,
    treatment1 = treatment1,
    treatment2 = treatment2,
    label1 = label1,
    label2 = label2
  )
  
  g <- ggplot(df_plot, aes(x = event_time, y = estimate, color = group)) +
    geom_point(size = 2) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) +
    labs(x = "Time to treatment", y = "Estimated effect on fertility", color = "") +
    scale_color_manual(values = setNames(c(color1, color2), c(label1, label2))) +
    theme_classic() +
    my_theme +
    theme(panel.grid.major.y = element_line(color = "gray85"))
  
  if (!is.null(y_limits)) {
    g <- g + coord_cartesian(ylim = y_limits)
  }
  
  if (!is.null(save_path)) {
    ggsave(save_path, g, width = 10, height = 6)
  }
  
  g
}

graph_windows <- function(df_win,
                          prefix1, prefix2,
                          label1 = "Climate", label2 = "Non-climate",
                          event_t = 5,
                          y_limits = NULL,
                          save_path) {
  
  df_all <- df_win %>%
    filter(treatment %in% c(prefix1, prefix2),
           event_time == event_t) %>%
    mutate(
      window = paste0(window_start, "-", window_end),
      indicator = case_when(
        treatment == prefix1 ~ label1,
        treatment == prefix2 ~ label2,
        TRUE ~ NA_character_
      ),
      point_estimate = estimate,
      lb_CI_95 = ci_low,
      up_CI_95 = ci_high
    ) %>%
    arrange(window_start) %>%
    mutate(
      window = factor(window, levels = unique(window)),
      indicator = factor(indicator, levels = c(label1, label2))
    )
  
  if (nrow(df_all) > 0) {
    g <- ggplot(df_all, aes(x = window, y = point_estimate, color = indicator)) +
      geom_point(position = position_dodge(width = 0.5), size = 2) +
      geom_errorbar(aes(ymin = lb_CI_95, ymax = up_CI_95),
                    width = 0.3, position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      labs(x = "Time window", y = paste("Effect at t =", event_t), color = NULL) +
      { if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) } +
      theme_classic() +
      my_theme +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(panel.grid.major.y = element_line(color = "gray85")) +
      scale_color_manual(values = c("black", "#b2182b"))
    
    ggsave(save_path, g, width = 10, height = 6)
    return(g)
  }
  
  return(NULL)
}

graph_income <- function(df_income,
                         climatic_var, non_climatic_var,
                         measure_name,
                         event_t = 5,
                         y_limits = NULL,
                         save_path) {
  
  df_all <- df_income %>%
    filter(treatment %in% c(climatic_var, non_climatic_var),
           event_time == event_t) %>%
    mutate(
      disaster_type = case_when(
        treatment == climatic_var ~ "Climate",
        treatment == non_climatic_var ~ "Non-climate",
        TRUE ~ NA_character_
      ),
      point_estimate = estimate,
      lb_CI_95 = ci_low,
      up_CI_95 = ci_high,
      group = case_when(
        subgroup == "Poor" ~ "Low income",
        subgroup == "Rich" ~ "High income",
        TRUE ~ subgroup
      )
    ) %>%
    mutate(
      disaster_type = factor(disaster_type, levels = c("Climate", "Non-climate")),
      group = factor(group, levels = c("Low income", "High income"))
    )
  
  g_combined <- ggplot(df_all, aes(x = fct_rev(factor(group)),
                                   y = point_estimate,
                                   color = disaster_type)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = lb_CI_95, ymax = up_CI_95),
                  width = 0.4, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    { if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) } +
    coord_flip() +
    labs(x = "",
         y = "Estimated effect on fertility",
         color = "") +
    theme_classic() +
    my_theme +
    theme(panel.grid.major.x = element_line(color = "gray85")) +
    scale_color_manual(values = c("Climate" = "black", "Non-climate" = "#b2182b"))
  
  ggsave(filename = save_path, plot = g_combined, width = 8, height = 6)
  g_combined
}

#---------------------------------#
# Read estimates ----
#---------------------------------#

results <- readRDS(paths$in_rds)

df_main   <- results$main_climate_vs_nonclimate
df_win    <- results$rolling_windows_20y
df_hazard <- results$by_disaster_category
df_income <- results$by_income_group

#---------------------------------------#
# MAIN event studies ----
# --------------------------------------#

main_treatments <- c(
  "ln_affected_rate_climate_std",
  "ln_affected_rate_no_climate_std",
  "ln_death_rate_climate_std",
  "ln_death_rate_no_climate_std"
) %>% intersect(unique(df_main$treatment))

ylims_main <- compute_y_limits_from_df(df_main, main_treatments, step = 0.005)

g_main_affected <- graph_main(
  df = df_main,
  treatment1 = "ln_affected_rate_climate_std",
  treatment2 = "ln_affected_rate_no_climate_std",
  color1 = "black",
  color2 = "#b2182b",
  label1 = "Climate",
  label2 = "Non-climate",
  y_limits = ylims_main,
  save_path = here(paths$out_fig, "didn_ln_affected_rate_std_comparison.png")
)

g_main_death <- graph_main(
  df = df_main,
  treatment1 = "ln_death_rate_climate_std",
  treatment2 = "ln_death_rate_no_climate_std",
  color1 = "black",
  color2 = "#b2182b",
  label1 = "Climate",
  label2 = "Non-climate",
  y_limits = ylims_main,
  save_path = here(paths$out_fig, "didn_ln_death_rate_std_comparison.png")
)

g_main_affected
g_main_death

#---------------------------------#
# Hazard-type plots (t = 5 only) ----
#---------------------------------#

df_h5 <- df_hazard %>%
  filter(event_time == 5) %>%
  mutate(
    measure = if_else(str_detect(treatment, "affected"), "Affected rate", "Death rate"),
    disaster = treatment %>%
      str_remove("^ln_affected_rate_") %>%
      str_remove("^ln_death_rate_") %>%
      str_remove("_std$") %>%
      str_replace_all("_", " ") %>%
      str_to_title(),
    climatic = if_else(
      disaster %in% c("Cold Wave", "Heat Wave", "Drought", "Hydrological", "Storm", "Wildfire"),
      "Climate",
      "Non-climate"
    ),
    climatic = factor(climatic, levels = c("Climate", "Non-climate")),
    measure  = factor(measure, levels = c("Affected rate", "Death rate")),
    disaster = factor(disaster, levels = sort(unique(disaster), decreasing = TRUE))
  )

g_hazard_t5 <- ggplot(df_h5, aes(x = disaster, y = estimate, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.4,
    position = position_dodge(width = 0.6)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(climatic ~ ., scales = "free_y", space = "free_y", switch = "y") +
  coord_flip() +
  labs(x = "", y = "Estimated effect on fertility", color = "") +
  scale_color_manual(values = c("Affected rate" = "black", "Death rate" = "#b2182b")) +
  theme_classic() +
  my_theme +
  theme(panel.grid.major.x = element_line(color = "gray85"))

ggsave(filename = here(paths$out_fig, "effects_by_hazard_t5.png"), plot = g_hazard_t5, width = 10, height = 8)

g_hazard_t5

#---------------------------------#
# Windows plots (t = 5) ----
#---------------------------------#

win_treatments_t5 <- c(
  "ln_affected_rate_climate_std",
  "ln_affected_rate_no_climate_std",
  "ln_death_rate_climate_std",
  "ln_death_rate_no_climate_std"
) %>% intersect(unique(df_win$treatment))

ylims_win_t5 <- compute_y_limits_from_df(
  df = df_win %>% filter(event_time == 5),
  treatments = win_treatments_t5,
  step = 0.005
)

g_win_affected_t5 <- graph_windows(
  df_win    = df_win,
  prefix1   = "ln_affected_rate_climate_std",
  prefix2   = "ln_affected_rate_no_climate_std",
  label1    = "Climate",
  label2    = "Non-climate",
  event_t   = 5,
  y_limits  = ylims_win_t5,
  save_path = here(paths$out_fig, "window_ln_affected_rate_std_effect_comparison_5.png")
)

g_win_death_t5 <- graph_windows(
  df_win    = df_win,
  prefix1   = "ln_death_rate_climate_std",
  prefix2   = "ln_death_rate_no_climate_std",
  label1    = "Climate",
  label2    = "Non-climate",
  event_t   = 5,
  y_limits  = ylims_win_t5,
  save_path = here(paths$out_fig, "window_ln_death_rate_std_effect_comparison_5.png")
)

g_win_affected_t5
g_win_death_t5


#---------------------------------#
# Income-group plots (t = 5) ----
#---------------------------------#

# ---- 1) Calcular límites UNA vez para t = 5 con los 4 tratamientos ----
income_treatments_t5 <- c(
  "ln_affected_rate_climate_std",
  "ln_affected_rate_no_climate_std",
  "ln_death_rate_climate_std",
  "ln_death_rate_no_climate_std"
) %>% intersect(unique(df_income$treatment))

ylims_income_t5 <- compute_y_limits_from_df(
  df = df_income %>% filter(event_time == 5),
  treatments = income_treatments_t5,
  step = 0.005
)

# ---- 2) Affected (t = 5) ----
g_income_affected_t5 <- graph_income(
  df_income       = df_income,
  climatic_var    = "ln_affected_rate_climate_std",
  non_climatic_var= "ln_affected_rate_no_climate_std",
  measure_name    = "affected_rate",
  event_t         = 5,
  y_limits        = ylims_income_t5,
  save_path       = here(paths$out_fig, "comparison_affected_rate_group_income_t5.png")
)

# ---- 3) Death (t = 5) ----
g_income_death_t5 <- graph_income(
  df_income       = df_income,
  climatic_var    = "ln_death_rate_climate_std",
  non_climatic_var= "ln_death_rate_no_climate_std",
  measure_name    = "death_rate",
  event_t         = 5,
  y_limits        = ylims_income_t5,
  save_path       = here(paths$out_fig, "comparison_death_rate_group_income_t5.png")
)

g_income_affected_t5
g_income_death_t5

message("Event-study figures saved in: ", paths$in_rds)
