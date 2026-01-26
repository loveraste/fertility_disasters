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
  out_fig = here("outputs", "figures"),
  out_root = here("outputs", "figures", "robustness")
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
                       color1 = "black", color2 = "gray70",
                       label1 = "Climate", label2 = "Non-climate",
                       y_limits = NULL, save_path = NULL) {
  
  df_plot <- prep_dual_df(
    df = df,
    treatment1 = treatment1,
    treatment2 = treatment2,
    label1 = label1,
    label2 = label2
  ) %>% 
    arrange(desc(group)) 
  
  g <- ggplot(df_plot, aes(x = event_time, y = estimate, color = group)) +
    geom_point(size = 2) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) +
    labs(x = "Time to treatment", y = "Estimated effect on fertility", color = "") +
    scale_color_manual(values = setNames(c(color1, color2), c(label1, label2)),
                       breaks = c(label1, label2)) +
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
                                 trt,
                                 label = "Climate",
                                 event_t = 5,
                                 color = "black",
                                 y_limits = NULL,
                                 save_path) {
  
  df_all <- df_win %>%
    filter(treatment == trt, event_time == event_t) %>%
    mutate(
      window = paste0(window_start, "-", window_end),
      point_estimate = estimate,
      lb_CI_95 = ci_low,
      up_CI_95 = ci_high
    ) %>%
    arrange(window_start) %>%
    mutate(window = factor(window, levels = unique(window)))
  
  g <- ggplot(df_all, aes(x = window, y = point_estimate)) +
    geom_point(size = 2, color = color) +
    geom_errorbar(aes(ymin = lb_CI_95, ymax = up_CI_95),
                  width = 0.3, color = color) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    { if (!is.null(y_limits)) coord_cartesian(ylim = y_limits) } +
    labs(x = "Time window", y = paste("Effect at t =", event_t)) +
    theme_classic() +
    my_theme +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.y = element_line(color = "gray85")
    )
  
  ggsave(save_path, g, width = 10, height = 6)
  return(g)
}

graph_income <- function(df_income,
                          trt,
                          label = "Climate",
                          event_t = 5,
                          color = "black",
                          y_limits = NULL,
                          save_path) {
  
  df_all <- df_income %>%
    filter(treatment == trt, event_time == event_t) %>%
    mutate(
      group = case_when(
        subgroup == "Poor" ~ "Low income",
        subgroup == "Rich" ~ "High income",
        TRUE ~ subgroup
      ),
      group = factor(group, levels = c("Low income", "High income")),
      point_estimate = estimate,
      lb_CI_95 = ci_low,
      up_CI_95 = ci_high
    )
  
  g <- ggplot(df_all, aes(x = fct_rev(group), y = point_estimate)) +
    geom_point(size = 2, color = color) +
    geom_errorbar(aes(ymin = lb_CI_95, ymax = up_CI_95),
                  width = 0.4, color = color) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    coord_flip(ylim = y_limits) +
    labs(x = "",
         y = "Estimated effect on fertility") +
    theme_classic() +
    my_theme +
    theme(panel.grid.major.x = element_line(color = "gray85"))
  
  ggsave(filename = save_path, plot = g, width = 8, height = 4)
  g
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
  color2 = "gray",
  label1 = "Climate",
  label2 = "Non-climate",
  y_limits = ylims_main,
  save_path = here(paths$out_fig, "didn_ln_affected_rate_std_comparison.png")
)

g_main_death <- graph_main(
  df = df_main,
  treatment1 = "ln_death_rate_climate_std",
  treatment2 = "ln_death_rate_no_climate_std",
  color1 = "darkblue",
  color2 = "#7FBADC",
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

disaster_order <- c("Geophysical", "Biological", "Wildfire", "Cold Wave", "Heat Wave", "Storm", "Hydrological", "Drought")

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
    disaster = factor(disaster, levels = disaster_order),
    color_key = case_when(
      climatic == "Climate"     & measure == "Affected rate" ~ "clim_aff",
      climatic == "Climate"     & measure == "Death rate"    ~ "clim_death",
      climatic == "Non-climate" & measure == "Affected rate" ~ "nonclim_aff",
      climatic == "Non-climate" & measure == "Death rate"    ~ "nonclim_death",
      TRUE ~ NA_character_
    ))

g_hazard_t5 <- ggplot(df_h5, aes(x = disaster, y = estimate, color = color_key)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.4,
    position = position_dodge(width = 0.6)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ climatic, ncol = 2) +
  coord_flip() +
  labs(x = "", y = "Estimated effect on fertility", color = "") +
  scale_color_manual(
    values = c(
      clim_aff     = "black",
      clim_death   = "darkblue",  
      nonclim_aff  = "grey",
      nonclim_death= "#7FBADC"   
    ),
    breaks = c("clim_aff","nonclim_aff", "clim_death", "nonclim_death"),
    labels = c(
      "",
      "Affected rate    ",
      "",
      "Death rate"
    ))+
  theme_classic() +
  my_theme +
  theme(
    panel.grid.major.x = element_line(color = "gray85"),
    strip.text = element_text(size = 15)
  )

ggsave(filename = here(paths$out_fig, "effects_by_hazard_t5.png"), plot = g_hazard_t5, width = 10, height = 8)

g_hazard_t5

 #---------------------------------#
# Windows plots Climate (t = 5) ----
#---------------------------------#

win_treatments <- c(
  "ln_affected_rate_climate_std",
  "ln_death_rate_climate_std"
) %>% intersect(unique(df_win$treatment))

ylims_win_t5 <- compute_y_limits_from_df(
  df = df_win %>% filter(event_time == 5),
  treatments = win_treatments,
  step = 0.005
)

g_win_affected_t5_climate <- graph_windows(
  df_win    = df_win,
  trt       = "ln_affected_rate_climate_std",
  label     = "Climate",
  event_t   = 5,
  color     = "black",
  y_limits  = ylims_win_t5,
  save_path = here(paths$out_fig, "window_affected_std_climate_t5.png")
)

g_win_death_t5_climate <- graph_windows(
  df_win    = df_win,
  trt       = "ln_death_rate_climate_std",
  label     = "Climate",
  event_t   = 5,
  color     = "darkblue", 
  y_limits  = ylims_win_t5,
  save_path = here(paths$out_fig, "window_death_std_climate_t5.png")
)

g_win_affected_t5_climate
g_win_death_t5_climate

#---------------------------------#
# Income-group plots (t = 5) ----
#---------------------------------#

income_treatments <- c(
  "ln_affected_rate_climate_std",
  "ln_death_rate_climate_std"
) %>% intersect(unique(df_income$treatment))

ylims_income_t5 <- compute_y_limits_from_df(
  df = df_income %>% filter(event_time == 5),
  treatments = income_treatments,
  step = 0.005
)

# Affected (t = 5)
g_income_affected_t5_climate <- graph_income(
  df_income  = df_income,
  trt        = "ln_affected_rate_climate_std",
  label      = "Climate",
  event_t    = 5,
  color      = "black",
  y_limits   = ylims_income_t5,
  save_path  = here(paths$out_fig, "income_affected_climate_t5.png")
)

#Death (t = 5)
g_income_death_t5_climate <- graph_income(
  df_income  = df_income,
  trt        = "ln_death_rate_climate_std",
  label      = "Climate",
  event_t    = 5,
  color      = "darkblue",   # dark red
  y_limits   = ylims_income_t5,
  save_path  = here(paths$out_fig, "income_death_climate_t5.png")
)


g_income_affected_t5_climate
g_income_death_t5_climate

message("Event-study figures saved in: ", paths$in_rds)
