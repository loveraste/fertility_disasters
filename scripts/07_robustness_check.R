# -----------------------------------------------------------------------------#
# 07_robustness_check.R
# Purpose: Robustness event-study plots
# Inputs : outputs/estimates/all_estimations.rds
# Outputs: outputs/figures/robustness/*.png
# -----------------------------------------------------------------------------#

# ---- packages ----
pacman::p_load(tidyverse, here)

# ---- paths ----
paths <- list(
  in_rds   = here("outputs", "estimates", "all_estimations.rds"),
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


#---------------------------------#
# Helpers: y-limits (same as MAIN) ----
#---------------------------------#

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
  df_use <- df %>% filter(treatment %in% treatments)
  
  vals <- df_use %>%
    transmute(est = estimate, lo = ci_low, hi = ci_high) %>%
    pivot_longer(everything(), values_to = "v") %>%
    pull(v)
  
  c(
    round_down_step(vals, step = step),
    round_up_step(vals, step = step)
  )
}

#---------------------------------#
# Plot builders ----
#---------------------------------#

# (A) Dual line: climate vs non-climate (like MAIN)
plot_event_study_dual_from_df <- function(df, treatment1, treatment2,
                                          label1 = "Climate", label2 = "Non-climate",
                                          color1 = "black", color2 = "#b2182b",
                                          y_limits = NULL, title = NULL, save_path = NULL) {
  
  df_plot <- df %>%
    filter(treatment %in% c(treatment1, treatment2)) %>%
    mutate(
      group = case_when(
        treatment == treatment1 ~ label1,
        treatment == treatment2 ~ label2,
        TRUE ~ NA_character_
      )
    )
  
  g <- ggplot(df_plot, aes(x = event_time, y = estimate, color = group)) +
    geom_point(size = 2) +
    geom_line() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) +
    labs(
      x = "Time to treatment",
      y = "Estimated effect on fertility",
      color = "",
      title = title
    ) +
    scale_color_manual(values = setNames(c(color1, color2), c(label1, label2))) +
    theme_classic() +
    my_theme +
    theme(panel.grid.major.y = element_line(color = "gray85"))
  
  if (!is.null(y_limits)) g <- g + coord_cartesian(ylim = y_limits)
  
  if (!is.null(save_path)) ggsave(save_path, g, width = 10, height = 6)
  
  g
}

# (B) Single line: one treatment (used for hazard-specific ES)
plot_event_study_single_from_df <- function(df, trt,
                                            color = "black",
                                            y_limits = NULL, title = NULL, save_path = NULL) {
  
  df_plot <- df %>%
    filter(.data$treatment == trt) %>%  
    arrange(event_time)
  
  g <- ggplot(df_plot, aes(x = event_time, y = estimate)) +
    geom_point(size = 2, color = color) +
    geom_line(color = color) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4, color = color) +
    labs(
      x = "Time to treatment",
      y = "Estimated effect on fertility",
      title = title
    ) +
    theme_classic() +
    my_theme +
    theme(panel.grid.major.y = element_line(color = "gray85"))
  
  if (!is.null(y_limits)) g <- g + coord_cartesian(ylim = y_limits)
  
  if (!is.null(save_path)) ggsave(save_path, g, width = 10, height = 6)
  
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

#---------------------------------#
# Read estimates ----
#---------------------------------#

results <- readRDS(paths$in_rds)

df_win    <- results$rolling_windows_20y
df_hazard <- results$by_disaster_category
df_income <- results$by_income_group

#------------------------------------#
#  Hazard type (t=10) ----
# -----------------------------------#

df_h10 <- df_hazard %>%
  filter(event_time == 10) %>%
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

g_hazard_t10 <- ggplot(df_h10, aes(x = disaster, y = estimate, color = measure)) +
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

ggsave(filename = here(paths$out_root, "effects_by_hazard_t10.png"), plot = g_hazard_t5, width = 10, height = 8)

g_hazard_t10

#---------------------------------#
# Windows plots (t = 10) ----
#---------------------------------#

win_treatments_t10 <- c(
  "ln_affected_rate_climate_std",
  "ln_affected_rate_no_climate_std",
  "ln_death_rate_climate_std",
  "ln_death_rate_no_climate_std"
) %>% intersect(unique(df_win$treatment))

ylims_win_t10 <- compute_y_limits_from_df(
  df = df_win %>% filter(event_time == 10),
  treatments = win_treatments_t10,
  step = 0.005
)

g_win_affected_t10 <- graph_windows(
  df_win    = df_win,
  prefix1   = "ln_affected_rate_climate_std",
  prefix2   = "ln_affected_rate_no_climate_std",
  label1    = "Climate",
  label2    = "Non-climate",
  event_t   = 10,
  y_limits  = ylims_win_t10,
  save_path = here(paths$out_root, "window_ln_affected_rate_std_effect_comparison_10.png")
)

g_win_death_t10 <- graph_windows(
  df_win    = df_win,
  prefix1   = "ln_death_rate_climate_std",
  prefix2   = "ln_death_rate_no_climate_std",
  label1    = "Climate",
  label2    = "Non-climate",
  event_t   = 10,
  y_limits  = ylims_win_t10,
  save_path = here(paths$out_root, "window_ln_death_rate_std_effect_comparison_10.png")
)

g_win_affected_t10
g_win_death_t10

#------------------------------------------------------------------------------#
#  Rolling windows: for each window -> 2 plots (affected & death) ----
# -----------------------------------------------------------------------------#

windows_tbl <- df_win %>%
  distinct(window_start, window_end) %>%
  arrange(window_start, window_end)

for (i in seq_len(nrow(windows_tbl))) {
  
  ws <- windows_tbl$window_start[i]
  we <- windows_tbl$window_end[i]
  
  df_w <- df_win %>%
    filter(window_start == ws, window_end == we)
  
  # treatments present in this window
  tset <- c(
    "ln_affected_rate_climate_std",
    "ln_affected_rate_no_climate_std",
    "ln_death_rate_climate_std",
    "ln_death_rate_no_climate_std"
  ) %>% intersect(unique(df_w$treatment))
  
  # compute shared limits for the pair (affected plot + death plot) within this window
  ylims_w <- compute_y_limits_from_df(df_w, treatments = tset, step = 0.005)
  
  tag_window <- paste0(ws, "_", we)
  
  # affected
  plot_event_study_dual_from_df(
    df = df_w,
    treatment1 = "ln_affected_rate_climate_std",
    treatment2 = "ln_affected_rate_no_climate_std",
    label1 = "Climate", label2 = "Non-climate",
    color1 = "black", color2 = "#b2182b",
    y_limits = ylims_w,
    save_path = here(paths$out_root, paste0("win_", tag_window, "_affected.png"))
  )
  
  # death
  plot_event_study_dual_from_df(
    df = df_w,
    treatment1 = "ln_death_rate_climate_std",
    treatment2 = "ln_death_rate_no_climate_std",
    label1 = "Climate", label2 = "Non-climate",
    color1 = "black", color2 = "#b2182b",
    y_limits = ylims_w,
    save_path = here(paths$out_root, paste0("win_", tag_window, "_death.png"))
  )
}

# -----------------------------------------------------------------------------#
# Hazard type: for each hazard -> 2 plots (affected & death) ----
# -----------------------------------------------------------------------------#

# hazard name parsed exactly like your old code
df_hazard_tagged <- df_hazard %>%
  mutate(
    measure = if_else(str_detect(treatment, "affected"), "affected", "death"),
    hazard = treatment %>%
      str_remove("^ln_affected_rate_") %>%
      str_remove("^ln_death_rate_") %>%
      str_remove("_std$") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

hazard_tbl <- df_hazard_tagged %>%
  distinct(hazard) %>%
  arrange(hazard)

for (i in seq_len(nrow(hazard_tbl))) {
  
  hz <- hazard_tbl$hazard[i]
  
  df_hz <- df_hazard_tagged %>% filter(hazard == hz)
  
  # define the two treatments for this hazard
  tr_aff <- df_hz %>% filter(measure == "affected") %>% distinct(treatment) %>% pull(treatment)
  tr_dth <- df_hz %>% filter(measure == "death") %>% distinct(treatment) %>% pull(treatment)
  
  # skip if something is missing
  if (length(tr_aff) != 1 || length(tr_dth) != 1) next
  
  # shared limits within hazard (affected vs death)
  ylims_hz <- compute_y_limits_from_df(df_hz, treatments = c(tr_aff, tr_dth), step = 0.005)
  
  hz_tag <- hz %>% str_replace_all(" ", "_") %>% str_to_lower()
  
  plot_event_study_single_from_df(
    df = df_hz,
    trt = tr_aff,
    color = "black",
    y_limits = ylims_hz,
    save_path = here(paths$out_root, paste0("hazard_", hz_tag, "_affected.png"))
  )
  
  plot_event_study_single_from_df(
    df = df_hz,
    trt = tr_dth,
    color = "#b2182b",
    y_limits = ylims_hz,
    save_path = here(paths$out_root, paste0("hazard_", hz_tag, "_death.png"))
  )
}

# -----------------------------------------------------------------------------#
# Income subgroup: for each subgroup -> 2 plots (affected & death) ----
# -----------------------------------------------------------------------------#

income_tbl <- df_income %>%
  distinct(subgroup) %>%
  arrange(subgroup)

for (i in seq_len(nrow(income_tbl))) {
  
  sg <- income_tbl$subgroup[i]
  
  df_sg <- df_income %>%
    filter(subgroup == sg) %>%
    mutate(
      subgroup_label = case_when(
        subgroup == "Poor" ~ "Low income",
        subgroup == "Rich" ~ "High income",
        TRUE ~ subgroup
      )
    )
  
  tset <- c(
    "ln_affected_rate_climate_std",
    "ln_affected_rate_no_climate_std",
    "ln_death_rate_climate_std",
    "ln_death_rate_no_climate_std"
  ) %>% intersect(unique(df_sg$treatment))
  
  ylims_sg <- compute_y_limits_from_df(df_sg, treatments = tset, step = 0.005)
  
  sg_tag <- df_sg$subgroup_label[1] %>%
    str_replace_all(" ", "_") %>%
    str_to_lower()
  
  plot_event_study_dual_from_df(
    df = df_sg,
    treatment1 = "ln_affected_rate_climate_std",
    treatment2 = "ln_affected_rate_no_climate_std",
    label1 = "Climate", label2 = "Non-climate",
    color1 = "black", color2 = "#b2182b",
    y_limits = ylims_sg,
    save_path = here(paths$out_root, paste0("income_", sg_tag, "_affected.png"))
  )
  
  plot_event_study_dual_from_df(
    df = df_sg,
    treatment1 = "ln_death_rate_climate_std",
    treatment2 = "ln_death_rate_no_climate_std",
    label1 = "Climate", label2 = "Non-climate",
    color1 = "black", color2 = "#b2182b",
    y_limits = ylims_sg,
    save_path = here(paths$out_root, paste0("income_", sg_tag, "_death.png"))
  )
}

message("Robustness figures saved in: ", paths$out_root)
