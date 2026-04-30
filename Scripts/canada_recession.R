# ============================================================
# CANADA RECESSION FORECASTING
# ============================================================

cat("\nRunning Canada recession script...\n")

# ============================================================
# CHECK REQUIRED FUNCTIONS
# ============================================================

required_functions <- c(
  "rec_run_all_targets_parallel",
  "rec_collect_summary",
  "rec_collect_tcsr_fallback_summary",
  "rec_make_metric_table"
)

missing_functions <- required_functions[
  !vapply(required_functions, exists, logical(1), mode = "function")
]

if (length(missing_functions) > 0) {
  stop(
    "Missing recession helper functions. Put them in R/canada_recession_helpers.R.\nMissing: ",
    paste(missing_functions, collapse = ", ")
  )
}

# ============================================================
# LOAD CLEANED CANADA DATA WITH RECESSION TARGET
# ============================================================

cleaned_rec_panel_path <- file.path(
  canada_cleaned_dir,
  "panel_nonprov_clean_with_recession.rds"
)

if (!file.exists(cleaned_rec_panel_path)) {
  stop(
    "Cleaned Canada recession panel not found:\n",
    cleaned_rec_panel_path,
    "\nRun MAIN.R with run_canada_cleaning <- 'auto' or run_canada_cleaning <- 'force' first."
  )
}

panel_rec <- readRDS(cleaned_rec_panel_path)

names(panel_rec)[1] <- "Date"
panel_rec$Date <- as.Date(panel_rec$Date)

if (!"recession_tplus1" %in% names(panel_rec)) {
  stop(
    "The cleaned recession panel does not contain recession_tplus1.\n",
    "Check that the scraping and cleaning scripts save panel_nonprov_clean_with_recession.rds with recession_tplus1."
  )
}

panel_rec <- panel_rec |>
  dplyr::arrange(.data$Date) |>
  dplyr::mutate(
    recession = dplyr::lag(as.numeric(.data$recession_tplus1), 1)
  )

rec_target <- "recession"
rec_targets <- c(rec_target)

panel_rec_model <- panel_rec |>
  dplyr::select(
    Date,
    dplyr::everything(),
    -dplyr::any_of(
      setdiff(grep("^recession", names(panel_rec), value = TRUE), rec_target)
    )
  ) |>
  dplyr::arrange(.data$Date)

num_cols <- setdiff(names(panel_rec_model), "Date")

panel_rec_model[num_cols] <- lapply(
  panel_rec_model[num_cols],
  function(x) suppressWarnings(as.numeric(x))
)

cat("\nLoaded cleaned Canada recession panel:\n")
cat("Rows:", nrow(panel_rec_model), "\n")
cat("Columns:", ncol(panel_rec_model), "\n")
cat("Start:", as.character(min(panel_rec_model$Date, na.rm = TRUE)), "\n")
cat("End:", as.character(max(panel_rec_model$Date, na.rm = TRUE)), "\n")
cat("Target:", rec_target, "\n")

# ============================================================
# MODEL DISPLAY NAMES
# ============================================================

rec_model_display_names <- c(
  "Static-Probit-Factors" = "Static-Probit-Factors",
  "AR,BIC"                = "AR,BIC",
  "ARDI,BIC"              = "ARDI,BIC",
  "Ridge-X"               = "Ridge-X",
  "Lasso-X"               = "Lasso-X",
  "Elastic-Net-X"         = "Elastic-Net-X",
  "Adaptive-Lasso-X"      = "Adaptive-Lasso-X",
  "OCMT_5"                = "OCMT",
  "BMT_5"                 = "BMT",
  "FarmSelect"            = "FarmSelect",
  "RF-X"                  = "RF-X",
  "ARDI,Ridge"            = "ARDI,Ridge",
  "ARDI,Lasso"            = "ARDI,Lasso",
  "ARDI,Elastic-Net"      = "ARDI,Elastic-Net",
  "ARDI,Adaptive-Lasso"   = "ARDI,Adaptive-Lasso",
  "RFARDI"                = "RFARDI",
  "T-CSR"                 = "T-CSR",
  "AVG"                   = "AVG"
)

# ============================================================
# CONFIGURATION FROM MAIN.R
# ============================================================

rec_ocmt_pval_grid <- canada_recession_ocmt_pvals
rec_bmt_pval_grid  <- canada_recession_bmt_pvals

factor_probit_k <- canada_recession_factor_probit_k

rec_cfg <- list(
  n_workers = canada_recession_n_workers,
  
  oos_start = canada_recession_oos_start,
  oos_end = min(
    canada_recession_oos_end,
    max(panel_rec_model$Date, na.rm = TRUE)
  ),
  
  horizons = canada_recession_horizons,
  
  py = canada_recession_py,
  px = canada_recession_px,
  K_fixed = canada_recession_K_fixed,
  
  retune_every_months = canada_recession_retune_every_months,
  training_months = canada_recession_training_months,
  
  glmnet_nlambda = canada_recession_glmnet_nlambda,
  elastic_alpha_grid = canada_recession_elastic_alpha_grid,
  fallback_lambda = canada_recession_fallback_lambda,
  adalasso_eps = canada_recession_adalasso_eps,
  
  ocmt = list(
    pvals = rec_ocmt_pval_grid,
    delta1 = 1,
    delta2 = 1,
    multi_stage = TRUE,
    max_iter = canada_recession_ocmt_max_iter,
    kappa_const = 0.5
  ),
  
  bmt = list(
    pvals = rec_bmt_pval_grid,
    delta1 = 1,
    delta2 = 1,
    maxit = canada_recession_bmt_maxit
  ),
  
  farm = list(
    loss = "lasso",
    robust = FALSE,
    cv = canada_recession_farm_cv,
    tau = 2,
    K.factors = NULL,
    max.iter = canada_recession_farm_max_iter,
    nfolds = canada_recession_farm_nfolds,
    eps = 1e-4
  ),
  
  rf = list(
    num_trees = canada_recession_rf_num_trees,
    min_node_size = canada_recession_rf_min_node_size
  ),
  
  tcsr = list(
    tc = canada_recession_tcsr_tc,
    L  = canada_recession_tcsr_L,
    M  = canada_recession_tcsr_M
  ),
  
  tick_every = 2L
)

cat("\nCanada recession configuration:\n")
cat("OOS start:", as.character(rec_cfg$oos_start), "\n")
cat("OOS end:", as.character(rec_cfg$oos_end), "\n")
cat("Horizons:", paste(rec_cfg$horizons, collapse = ", "), "\n")
cat("Workers:", rec_cfg$n_workers, "\n")
cat("OCMT p-values:", paste(names(rec_cfg$ocmt$pvals), rec_cfg$ocmt$pvals, sep = "=", collapse = ", "), "\n")
cat("BMT p-values:", paste(names(rec_cfg$bmt$pvals), rec_cfg$bmt$pvals, sep = "=", collapse = ", "), "\n")
cat("FarmSelect CV:", rec_cfg$farm$cv, "with", rec_cfg$farm$nfolds, "folds\n")
cat("T-CSR cutoff:", rec_cfg$tcsr$tc, "\n")
cat("T-CSR L:", rec_cfg$tcsr$L, "\n")
cat("T-CSR M:", rec_cfg$tcsr$M, "\n")

# ============================================================
# RUN RECESSION MODELS
# ============================================================

cat("\nStarting Canada recession run...\n")

rec_all_results <- rec_run_all_targets_parallel(
  panel_rec_model = panel_rec_model,
  rec_targets = rec_targets,
  n_workers = rec_cfg$n_workers,
  oos_start = rec_cfg$oos_start,
  oos_end   = rec_cfg$oos_end,
  horizons = rec_cfg$horizons,
  py = rec_cfg$py,
  px = rec_cfg$px,
  K_fixed = rec_cfg$K_fixed,
  retune_every_months = rec_cfg$retune_every_months,
  training_months = rec_cfg$training_months,
  tcsr_M = rec_cfg$tcsr$M,
  rf_num_trees = rec_cfg$rf$num_trees,
  ocmt_pvals = rec_cfg$ocmt$pvals,
  bmt_pvals = rec_cfg$bmt$pvals
)

cat("\nCanada recession run finished.\n")

# ============================================================
# SAVE OUTPUTS
# ============================================================

dir.create(canada_recession_dir, recursive = TRUE, showWarnings = FALSE)

results_rds <- file.path(canada_recession_dir, "canada_recession_results.rds")

saveRDS(
  list(
    rec_all_results = rec_all_results,
    rec_cfg = rec_cfg,
    rec_targets = rec_targets,
    rec_target = rec_target,
    rec_model_display_names = rec_model_display_names,
    panel_rec_model = panel_rec_model
  ),
  results_rds
)

cat("\nSaved recession RDS to:\n")
cat(results_rds, "\n")

rec_summary <- rec_collect_summary(
  rec_all_results = rec_all_results,
  rec_targets = rec_targets,
  horizons = rec_cfg$horizons
)

rec_tcsr_fallback_summary <- rec_collect_tcsr_fallback_summary(
  rec_all_results = rec_all_results,
  rec_targets = rec_targets,
  horizons = rec_cfg$horizons
)

summary_csv <- file.path(canada_recession_dir, "canada_recession_summary.csv")
tcsr_csv <- file.path(canada_recession_dir, "canada_recession_tcsr_fallback_summary.csv")

utils::write.csv(
  rec_summary,
  summary_csv,
  row.names = FALSE
)

utils::write.csv(
  rec_tcsr_fallback_summary,
  tcsr_csv,
  row.names = FALSE
)

cat("\nSaved summary CSV to:\n")
cat(summary_csv, "\n")

cat("\nSaved T-CSR fallback CSV to:\n")
cat(tcsr_csv, "\n")

# ============================================================
# SAVE METRIC TABLES
# ============================================================

metrics_to_save <- c(
  "qps",
  "lps",
  "auc",
  "mcfadden_r2",
  "estrella_r2"
)

for (metric in metrics_to_save) {
  metric_tbl <- rec_make_metric_table(
    rec_all_results = rec_all_results,
    rec_targets = rec_targets,
    metric = metric,
    horizons = rec_cfg$horizons,
    digits = 3
  )
  
  metric_file <- file.path(
    canada_recession_dir,
    paste0("canada_recession_table_", metric, ".csv")
  )
  
  utils::write.csv(
    metric_tbl,
    metric_file,
    row.names = FALSE
  )
  
  cat("\nSaved", metric, "table to:\n")
  cat(metric_file, "\n")
}

cat("\nCanada recession script finished.\n")
cat("Outputs saved to:\n")
cat(canada_recession_dir, "\n")