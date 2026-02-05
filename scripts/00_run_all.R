# -----------------------------------------------------------------------------#
# 00_run_all.R
# Purpose: Run the full replication pipeline end-to-end
# Inputs : (see each script)
# Outputs: data/processed/*.rds, outputs/figures/*, outputs/estimates/*, etc.
# -----------------------------------------------------------------------------#

# ---- renv + packages ----
if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  # Safe to run; renv will skip what is already installed
  renv::restore(prompt = FALSE)
}

pacman::p_load(here)

# OS / headless graphics guard (macOS OpenGL/rgl)
Sys.setenv(RGL_USE_NULL = "TRUE")

# ---- helper: run scripts with logging ----
run_script <- function(path) {
  stopifnot(file.exists(path))
  message("\n----------------------------")
  message("Running: ", path)
  message("------------------------------\n")
  source(path, local = FALSE, echo = FALSE)
}

# ---- pipeline order ----
# 1) Clean UN WPP data
run_script(here("scripts", "01_clean_un_data.R"))

# 2) Clean EM-DAT and create annual disaster measures
run_script(here("scripts", "02_clean_emdat_data.R"))

# 3) Merge UN + EM-DAT and standardize disaster variables
run_script(here("scripts", "03_merge_un_emdat.R"))

# 4) Descriptives and time series plots + appendix table
run_script(here("scripts", "04_descriptives.R"))

# 5) Estimations (DID) and save all outputs
run_script(here("scripts", "05_estimations.R"))

# 6) Main event-study plots (and windows/hazard/income plots)
run_script(here("scripts", "06_event_study_plots.R"))

# 7) Robustness plots
run_script(here("scripts", "07_robustness_check.R"))

message("\n Full replication pipeline finished successfully.")
