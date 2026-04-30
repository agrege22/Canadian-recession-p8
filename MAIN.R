# ============================================================
# MAIN CONTROL FILE
# ============================================================

suppressPackageStartupMessages({
  library(here)
})

here::i_am("MAIN.R")

cat("\nProject root detected as:\n")
print(here::here())

# ============================================================
# CHOOSE WHAT TO RUN
# ============================================================

run_scraping                  <- FALSE # Gather new Canada data

# Cleaning options:
# "auto"  = run only if cleaned files are missing and needed
# "force" = always rebuild cleaned files
# FALSE   = never run cleaning, might not work
run_canada_cleaning           <- "auto"

run_heatmaps_blags            <- FALSE
run_canada_targets            <- FALSE
run_canada_recession          <- FALSE
run_canada_variable_selection <- FALSE

run_eu                        <- TRUE

# ============================================================
# PACKAGE SETTINGS
# ============================================================

install_local_packages <- FALSE

# ============================================================
# THREAD SETTINGS
# ============================================================

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

# ============================================================
# GLOBAL PARAMETERS
# ============================================================

project_name <- "P8_forecasting"

horizons <- c(1, 3, 6, 12)

n_workers <- 15L

available_cores <- tryCatch(
  as.integer(parallel::detectCores(logical = TRUE)),
  error = function(e) 1L
)

if (!is.finite(available_cores) || available_cores < 1L) {
  available_cores <- 1L
}

safe_n_workers <- max(
  1L,
  min(as.integer(n_workers), max(1L, available_cores - 1L))
)

cat("\nWorker settings:\n")
cat("Requested workers:", n_workers, "\n")
cat("Available cores:  ", available_cores, "\n")
cat("Safe workers:     ", safe_n_workers, "\n")

# ============================================================
# SCRAPING / DATA PARAMETERS
# ============================================================

dslcmd_dest_dir <- here("data", "dslcmd")

scraping_h <- 1
scraping_make_lags <- TRUE
scraping_add_yahoo_returns <- TRUE
scraping_yahoo_lag_months <- 1
scraping_overwrite <- FALSE

# ============================================================
# CANADA CLEANING PARAMETERS
# Shared by Canada targets, recession, and variable selection
# ============================================================

canada_cleaning_start_date <- as.Date("1981-01-01")
canada_cleaning_predictor_start_col <- 3
canada_cleaning_min_predictors_available <- 10

# ============================================================
# HEATMAP PARAMETERS
# ============================================================

heatmap_panels <- c("panel_nonprov", "panel_blag")

heatmap_K <- 20
heatmap_months_pad <- 24

heatmap_width <- 12
heatmap_height <- 6
heatmap_dpi_png <- 300

heatmap_save_pdf <- TRUE
heatmap_save_png <- TRUE
heatmap_print_plots <- FALSE

# ============================================================
# CANADA CONTINUOUS-TARGET PARAMETERS
# ============================================================

canada_targets_oos_start <- as.Date("2005-01-01")
canada_targets_oos_end   <- as.Date("2021-12-01")

canada_targets_horizons <- c(1, 3, 6, 12)

canada_py <- 5
canada_px <- 5
canada_K_fixed <- 6

canada_training_months <- 10 * 12
canada_retune_every_months <- 12

canada_glmnet_nlambda <- 240

canada_elastic_alpha_grid <- c(
  0.02, 0.08, 0.15, 0.25, 0.35,
  0.5, 0.65, 0.75, 0.85, 0.92, 0.98
)

canada_ocmt_pvals <- c("5" = 0.05)
canada_bmt_pvals  <- c("5" = 0.05)

canada_ocmt_max_iter <- 300
canada_bmt_maxit <- 300

canada_farm_cv <- TRUE
canada_farm_nfolds <- 10
canada_farm_max_iter <- 90000

canada_rf_num_trees <- 1800
canada_rf_min_node_size <- 10

canada_tcsr_tc <- 1.645
canada_tcsr_L  <- 10
canada_tcsr_M  <- 10000

canada_n_workers <- safe_n_workers

# ============================================================
# CANADA RECESSION PARAMETERS
# ============================================================

canada_recession_oos_start <- as.Date("1982-01-01")
canada_recession_oos_end   <- as.Date("2021-12-01")

canada_recession_horizons <- c(1, 3, 6, 12)

canada_recession_py <- 5L
canada_recession_px <- 5L
canada_recession_K_fixed <- 6L

canada_recession_training_months <- 10L * 12L
canada_recession_retune_every_months <- 12L

canada_recession_n_workers <- safe_n_workers

canada_recession_glmnet_nlambda <- 240L

canada_recession_elastic_alpha_grid <- c(
  0.02, 0.08, 0.15, 0.25, 0.35,
  0.5, 0.65, 0.75, 0.85, 0.92, 0.98
)

canada_recession_fallback_lambda <- 1e-4
canada_recession_adalasso_eps <- 1e-6

canada_recession_ocmt_pvals <- c("5" = 0.05)
canada_recession_bmt_pvals  <- c("5" = 0.05)

canada_recession_ocmt_max_iter <- 300L
canada_recession_bmt_maxit <- 300L

canada_recession_farm_cv <- TRUE
canada_recession_farm_nfolds <- 10L
canada_recession_farm_max_iter <- 90000L

canada_recession_rf_num_trees <- 1800L
canada_recession_rf_min_node_size <- 10L

canada_recession_tcsr_tc <- 1.645
canada_recession_tcsr_L  <- 10L
canada_recession_tcsr_M  <- 10000L

canada_recession_factor_probit_k <- 3L

# ============================================================
# CANADA VARIABLE-SELECTION PARAMETERS
# ============================================================

canada_selection_initial_train <- 84L
canada_selection_min_class_count <- 3L

canada_selection_max_iter_farm <- 1200L
canada_selection_max_iter_ocmt <- 300L
canada_selection_maxit_bmt <- 300L
canada_selection_maxit_post_logit <- 300L

canada_selection_threshold_class <- 0.5

canada_selection_fixed_hyper <- list(
  "OCMT" = 0.05,
  "BMT" = 0.05,
  "FarmSelect" = 2
)

canada_selection_n_snapshots <- 10L
canada_selection_heatmap_top_n <- 40L

canada_selection_use_parallel <- TRUE
canada_selection_n_workers <- safe_n_workers
canada_selection_show_progress <- TRUE

# ============================================================
# EU DATA / TRANSFORMATION PARAMETERS
# ============================================================

eu_excel_path  <- here("data", "eu", "EUdata.xlsx")
eu_excel_sheet <- 2

# Transformation codes are read from the "Tcode" row in EUdata.xlsx, sheet 2.
# Change the transformation codes directly in the Excel file, not here.
# T-code 1 = level
# T-code 2 = seasonal difference: x_t - x_{t-4}
# T-code 3 = annual growth: 100 * (x_t / x_{t-4} - 1)
eu_transform_lag_season <- 4L

# Sample construction
eu_remove_vars_with_na_pre1980 <- TRUE
eu_pre1980_cutoff_date <- as.Date("1980-01-01")

eu_dataset_label <- "EU_data_dropvars_pre1980"

# EU targets
eu_real_activity_targets <- c("YER", "PCR")
eu_labour_income_targets <- c("WRN", "LNN")
eu_prices_monetary_housing_targets <- c("HICP", "RPP", "M2")

eu_targets_all <- list(
  table_real_activity = eu_real_activity_targets,
  table_labour_income = eu_labour_income_targets,
  table_prices_monetary_housing = eu_prices_monetary_housing_targets
)

eu_target_order <- c(
  eu_real_activity_targets,
  eu_labour_income_targets,
  eu_prices_monetary_housing_targets
)

eu_target_labels_all <- c(
  YER  = "YER - Real GDP",
  PCR  = "PCR - Real household consumption",
  HICP = "HICP - HICP overall index",
  RPP  = "RPP - Residential property prices",
  WRN  = "WRN - Average wage",
  LNN  = "LNN - Total employment (thousand persons)",
  M2   = "M2 - Monetary aggregate M2"
)

# EU forecasting setup
eu_oos_start <- as.Date("2005-01-01")
eu_oos_end   <- as.Date("2025-12-31")

eu_horizons <- c(1, 2, 3, 4)

eu_py <- 4L
eu_px <- 4L
eu_K_fixed <- 6L

eu_training_quarters <- 10L * 4L
eu_retune_every_quarters <- 4L

eu_glmnet_nlambda <- 240L

eu_elastic_alpha_grid <- c(
  0.02, 0.08, 0.15, 0.25, 0.35,
  0.5, 0.65, 0.75, 0.85, 0.92, 0.98
)

eu_ocmt_pvals <- c("5" = 0.05)
eu_bmt_pvals  <- c("5" = 0.05)

eu_ocmt_max_iter <- 300L
eu_bmt_maxit <- 300L

eu_farm_cv <- TRUE
eu_farm_nfolds <- 10L
eu_farm_max_iter <- 90000L

eu_rf_num_trees <- 1800L
eu_rf_min_node_size <- 10L

eu_tcsr_tc <- 1.645
eu_tcsr_L  <- 10L
eu_tcsr_M  <- 10000L

eu_n_workers <- safe_n_workers

# EU plots
eu_save_raw_series_plot <- TRUE
eu_save_transformed_series_plot <- TRUE
eu_save_individual_target_plots <- TRUE

# "all" plots all transformed series; "targets" plots only eu_target_order.
eu_plot_series_scope <- "all"

eu_plot_width <- 12
eu_plot_panel_height <- 3.6
eu_plot_add_zero_line_transformed <- TRUE

# ============================================================
# CHRONOS PARAMETERS
# ============================================================

chronos2_enabled <- TRUE
chronos2_uni_enabled <- TRUE
chronos2_multi_enabled <- TRUE
chronos2_covariate_enabled <- TRUE

chronos2_envname <- "chronos2-r"
chronos2_device <- "cpu"
chronos2_model_id <- "autogluon/chronos-2-small"
chronos2_quantile_levels <- c(0.5)

chronos2_batch_size <- 8L
chronos2_cross_learning <- FALSE
chronos2_validate_inputs <- FALSE
chronos2_context_max <- 256L

chronos2_torch_threads <- 8L
chronos2_torch_interop_threads <- 1L
chronos2_workers <- 3L

chronos_python <- normalizePath(
  file.path(
    Sys.getenv("LOCALAPPDATA"),
    "r-miniconda",
    "envs",
    chronos2_envname,
    "python.exe"
  ),
  winslash = "/",
  mustWork = FALSE
)

cat("\nChronos Python path:\n")
cat(chronos_python, "\n")

# ============================================================
# TARGET GROUPS: CANADA CONTINUOUS TARGETS
# ============================================================

real_activity_targets <- c(
  "IP_new",
  "EMP_CAN",
  "UNEMP_CAN"
)

inflation_targets <- c(
  "CPI_ALL_CAN",
  "CPI_MINUS_FOO_CAN"
)

credit_targets <- c(
  "CRED_T_cb",
  "CRED_BUS",
  "CRED_HOUS"
)

housing_targets <- c(
  "hstart_CAN_new",
  "build_Total_CAN_new"
)

targets_all <- list(
  table3_real    = real_activity_targets,
  table4_infl    = inflation_targets,
  table5_credit  = credit_targets,
  table6_housing = housing_targets
)

target_order <- c(
  real_activity_targets,
  inflation_targets,
  credit_targets,
  housing_targets
)

# ============================================================
# OUTPUT FOLDERS
# ============================================================

output_dir <- here("outputs")

scraping_dir <- here("outputs", "scraping")
heatmap_dir  <- here("outputs", "heatmaps")

canada_cleaned_dir <- here("outputs", "canada_cleaned")
canada_targets_dir <- here("outputs", "canada_targets")
canada_recession_dir <- here("outputs", "canada_recession")
canada_selection_dir <- here("outputs", "canada_variable_selection")

eu_dir <- here("outputs", "eu")
eu_dataset_dir <- file.path(eu_dir, eu_dataset_label)
eu_phase_dir <- file.path(eu_dataset_dir, "phase_results")
eu_plot_dir <- file.path(eu_dataset_dir, "plots")
eu_table_dir <- file.path(eu_dataset_dir, "tables")
eu_target_plot_dir <- file.path(eu_dataset_dir, "target_plots")
eu_data_prep_dir <- file.path(eu_dataset_dir, "data_prep")

dirs_to_create <- c(
  output_dir,
  scraping_dir,
  heatmap_dir,
  canada_cleaned_dir,
  canada_targets_dir,
  canada_recession_dir,
  canada_selection_dir,
  eu_dir,
  eu_dataset_dir,
  eu_phase_dir,
  eu_plot_dir,
  eu_table_dir,
  eu_target_plot_dir,
  eu_data_prep_dir,
  dslcmd_dest_dir,
  dirname(eu_excel_path)
)

for (d in dirs_to_create) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# CLEANING PATHS AND AUTO-CLEANING LOGIC
# ============================================================

raw_canada_path <- file.path(scraping_dir, "panel_nonprov.rds")

clean_no_recession_path <- file.path(
  canada_cleaned_dir,
  "panel_nonprov_clean_no_recession.rds"
)

clean_with_recession_path <- file.path(
  canada_cleaned_dir,
  "panel_nonprov_clean_with_recession.rds"
)

canada_scripts_need_cleaning <- isTRUE(run_canada_targets) ||
  isTRUE(run_canada_recession) ||
  isTRUE(run_canada_variable_selection)

cleaned_files_exist <- file.exists(clean_no_recession_path) &&
  file.exists(clean_with_recession_path)

if (identical(run_canada_cleaning, "auto")) {
  run_canada_cleaning <- canada_scripts_need_cleaning && !cleaned_files_exist
}

if (identical(run_canada_cleaning, "force")) {
  run_canada_cleaning <- TRUE
}

run_canada_cleaning <- isTRUE(run_canada_cleaning)

cat("\nCanada cleaning decision:\n")
cat("Canada scripts need cleaning:", canada_scripts_need_cleaning, "\n")
cat("Cleaned files exist:", cleaned_files_exist, "\n")
cat("Run Canada cleaning:", run_canada_cleaning, "\n")

# ============================================================
# LOAD PACKAGES AND FUNCTIONS
# ============================================================

source(here("R", "00_packages.R"))
source(here("R", "dslcmd_data_helpers.R"))

if (file.exists(here("R", "canada_cleaning_helpers.R"))) {
  source(here("R", "canada_cleaning_helpers.R"))
}

if (file.exists(here("R", "canada_targets_helpers.R"))) {
  source(here("R", "canada_targets_helpers.R"))
}

if (file.exists(here("R", "canada_recession_helpers.R"))) {
  source(here("R", "canada_recession_helpers.R"))
}

if (file.exists(here("R", "canada_variable_selection_helpers.R"))) {
  source(here("R", "canada_variable_selection_helpers.R"))
}

if (file.exists(here("R", "eu_helpers.R"))) {
  source(here("R", "eu_helpers.R"))
}

# ============================================================
# BASIC CHECKS
# ============================================================

if (!dir.exists(here("scripts"))) {
  stop("The folder 'scripts' does not exist. Rename 'Scripts' to 'scripts' or change the source paths.")
}

require_script <- function(file_name, run_flag) {
  if (isTRUE(run_flag) && !file.exists(here("scripts", file_name))) {
    stop("Missing script: scripts/", file_name)
  }
}

require_script("scraping.R", run_scraping)
require_script("canada_cleaning.R", run_canada_cleaning)
require_script("heatmaps_blags.R", run_heatmaps_blags)
require_script("canada_targets.R", run_canada_targets)
require_script("canada_recession.R", run_canada_recession)
require_script("canada_variable_selection.R", run_canada_variable_selection)
require_script("eu.R", run_eu)

if (run_canada_cleaning && !file.exists(here("R", "canada_cleaning_helpers.R"))) {
  stop(
    "run_canada_cleaning is TRUE, but R/canada_cleaning_helpers.R does not exist.\n",
    "Put the no-EM-PCA cleaning helper function there."
  )
}

if (run_canada_targets && !file.exists(here("R", "canada_targets_helpers.R"))) {
  stop(
    "run_canada_targets is TRUE, but R/canada_targets_helpers.R does not exist.\n",
    "Put the long Canada model helper functions there."
  )
}

if (run_canada_recession && !file.exists(here("R", "canada_recession_helpers.R"))) {
  stop(
    "run_canada_recession is TRUE, but R/canada_recession_helpers.R does not exist.\n",
    "Put the Canada recession helper functions there."
  )
}

if (run_canada_variable_selection && !file.exists(here("R", "canada_variable_selection_helpers.R"))) {
  stop(
    "run_canada_variable_selection is TRUE, but R/canada_variable_selection_helpers.R does not exist.\n",
    "Put the Canada variable-selection helper functions there."
  )
}

if (run_eu && !file.exists(here("R", "eu_helpers.R"))) {
  stop(
    "run_eu is TRUE, but R/eu_helpers.R does not exist.\n",
    "Put the EU data-prep, plotting, and model helper functions there."
  )
}

if ((run_canada_cleaning || run_heatmaps_blags) &&
    !run_scraping &&
    !file.exists(raw_canada_path)) {
  stop(
    "Raw Canadian data not found:\n",
    raw_canada_path,
    "\nRun with run_scraping <- TRUE first."
  )
}

if (canada_scripts_need_cleaning &&
    !run_canada_cleaning &&
    !cleaned_files_exist) {
  stop(
    "Canada scripts require cleaned data, but cleaned files do not exist.\n",
    "Either set run_canada_cleaning <- 'auto' or run_canada_cleaning <- 'force'."
  )
}

if (run_eu && !file.exists(eu_excel_path)) {
  stop(
    "EU Excel file not found:\n",
    eu_excel_path,
    "\nPut EUdata.xlsx in data/eu/ or change eu_excel_path in MAIN.R."
  )
}

if (!(eu_plot_series_scope %in% c("all", "targets"))) {
  stop("eu_plot_series_scope must be either 'all' or 'targets'.")
}

# ============================================================
# RUN SELECTED PARTS
# ============================================================

if (run_scraping) {
  cat("\n==============================\n")
  cat("Running scraping\n")
  cat("==============================\n")
  source(here("scripts", "scraping.R"))
}

if (run_canada_cleaning) {
  cat("\n==============================\n")
  cat("Running Canada sample cleaning\n")
  cat("==============================\n")
  source(here("scripts", "canada_cleaning.R"))
}

if (run_heatmaps_blags) {
  cat("\n==============================\n")
  cat("Running heatmaps with best-lag panel\n")
  cat("==============================\n")
  source(here("scripts", "heatmaps_blags.R"))
}

if (run_canada_targets) {
  cat("\n==============================\n")
  cat("Running Canada continuous targets\n")
  cat("==============================\n")
  source(here("scripts", "canada_targets.R"))
}

if (run_canada_recession) {
  cat("\n==============================\n")
  cat("Running Canada recession application\n")
  cat("==============================\n")
  source(here("scripts", "canada_recession.R"))
}

if (run_canada_variable_selection) {
  cat("\n==============================\n")
  cat("Running Canada variable-selection comparison\n")
  cat("==============================\n")
  source(here("scripts", "canada_variable_selection.R"))
}

if (run_eu) {
  cat("\n==============================\n")
  cat("Running EU application\n")
  cat("==============================\n")
  source(here("scripts", "eu.R"))
}

cat("\nMAIN.R finished.\n")