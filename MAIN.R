# ============================================================
# MAIN CONTROL FILE
# ============================================================

suppressPackageStartupMessages({
  library(here)
})

# ============================================================
# CHOOSE WHAT TO RUN
# ============================================================

run_scraping                  <- TRUE # Necessary for all canadian codes
run_heatmaps_blags            <- FALSE
run_canada_targets            <- FALSE
run_canada_recession          <- FALSE
run_canada_variable_selection <- FALSE
run_eu                        <- FALSE

# ============================================================
# GLOBAL PARAMETERS
# ============================================================

project_name <- "P8_forecasting"

horizons <- c(1, 3, 6, 12)

oos_start_canada <- as.Date("1982-01-01")
oos_end_canada   <- as.Date("2021-12-01")

oos_start_eu <- as.Date("2005-01-01")
oos_end_eu   <- as.Date("2025-10-01")

training_months <- 10 * 12
retune_every_months <- 12

ocmt_p_val <- 0.05
bmt_p_val  <- 0.05

farmselect_cv <- TRUE
farmselect_nfolds <- 10

tcsr_cutoff <- 1.645
tcsr_L <- 10

n_workers <- 15

# ============================================================
# OUTPUT FOLDERS
# ============================================================

output_dir <- here("outputs")

scraping_dir <- here("outputs", "scraping")
heatmap_dir  <- here("outputs", "heatmaps")
canada_targets_dir <- here("outputs", "canada_targets")
canada_recession_dir <- here("outputs", "canada_recession")
canada_selection_dir <- here("outputs", "canada_variable_selection")
eu_dir <- here("outputs", "eu")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(scraping_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(heatmap_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(canada_targets_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(canada_recession_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(canada_selection_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# LOAD PACKAGES AND FUNCTIONS
# ============================================================

source(here("R", "00_packages.R"))
source(here("R", "01_helpers.R"))
source(here("R", "02_output_helpers.R"))
source(here("R", "03_model_functions.R"))

# ============================================================
# RUN SELECTED PARTS
# ============================================================

if (run_scraping) {
  source(here("scripts", "scraping.R"))
}

if (run_heatmaps_blags) {
  source(here("scripts", "heatmaps_blags.R"))
}

if (run_canada_targets) {
  source(here("scripts", "canada_targets.R"))
}

if (run_canada_recession) {
  source(here("scripts", "canada_recession.R"))
}

if (run_canada_variable_selection) {
  source(here("scripts", "canada_variable_selection.R"))
}

if (run_eu) {
  source(here("scripts", "eu.R"))
}