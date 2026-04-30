# ============================================================
# EU APPLICATION
# ============================================================

cat("\nRunning EU application...\n")

# ============================================================
# CHECK REQUIRED HELPERS
# ============================================================

required_eu_functions <- c(
  "eu_prepare_data",
  "eu_get_targets",
  "eu_make_model_spec",
  "eu_make_fortin_controls",
  "eu_make_task_list",
  "eu_run_task_grid_nonchronos",
  "eu_run_chronos_external",
  "eu_build_merged_results",
  "eu_summarise_fallback_counts",
  "eu_plot_target_series",
  "eu_plot_series_combined",
  "eu_save_fortin_table_grouped_tex"
)

missing_eu_functions <- required_eu_functions[
  !vapply(required_eu_functions, exists, logical(1), mode = "function")
]

if (length(missing_eu_functions) > 0) {
  stop(
    "Missing EU helper functions. Put the EU helper code in R/eu_helpers.R and source it from MAIN.R.\nMissing: ",
    paste(missing_eu_functions, collapse = ", ")
  )
}

# ============================================================
# SMALL DEFAULT HELPER
# ============================================================

get_param <- function(name, default) {
  if (exists(name, inherits = TRUE)) {
    get(name, inherits = TRUE)
  } else {
    default
  }
}

# ============================================================
# EU DEFAULTS
# These can all be overwritten from MAIN.R.
# Transformation codes are read from the Tcode row in the Excel file.
# To change transformations, edit the Tcode row in the EU Excel file.
# ============================================================

eu_excel_path <- get_param(
  "eu_excel_path",
  here::here("data", "eu", "EUdata.xlsx")
)

eu_excel_sheet <- get_param("eu_excel_sheet", 2)

eu_lag_season <- get_param("eu_lag_season", 4L)

eu_remove_vars_with_na_pre1980 <- get_param(
  "eu_remove_vars_with_na_pre1980",
  TRUE
)

eu_pre1980_cutoff_date <- get_param(
  "eu_pre1980_cutoff_date",
  as.Date("1980-01-01")
)

eu_dataset_label <- get_param(
  "eu_dataset_label",
  if (isTRUE(eu_remove_vars_with_na_pre1980)) {
    "EU_data_dropvars_pre1980"
  } else {
    "EU_data_cutoff"
  }
)

eu_oos_start <- get_param("eu_oos_start", as.Date("2005-01-01"))
eu_oos_end   <- get_param("eu_oos_end",   as.Date("2025-10-01"))

eu_horizons <- get_param("eu_horizons", c(1, 2, 3, 4))

eu_py <- get_param("eu_py", 4L)
eu_px <- get_param("eu_px", 4L)
eu_K_fixed <- get_param("eu_K_fixed", 6L)

eu_training_obs <- get_param("eu_training_obs", 10L * 4L)
eu_retune_every_quarters <- get_param("eu_retune_every_quarters", 4L)

eu_n_workers <- get_param("eu_n_workers", get_param("n_workers", 14L))

eu_glmnet_nlambda <- get_param("eu_glmnet_nlambda", 240L)

eu_elastic_alpha_grid <- get_param(
  "eu_elastic_alpha_grid",
  c(
    0.02, 0.08, 0.15, 0.25, 0.35,
    0.5, 0.65, 0.75, 0.85, 0.92, 0.98
  )
)

eu_fallback_lambda <- get_param("eu_fallback_lambda", 1e-4)
eu_adalasso_eps <- get_param("eu_adalasso_eps", 1e-6)

eu_ocmt_pvals <- get_param("eu_ocmt_pvals", c("5" = 0.05))
eu_bmt_pvals  <- get_param("eu_bmt_pvals",  c("5" = 0.05))

eu_ocmt_max_iter <- get_param("eu_ocmt_max_iter", 300L)
eu_bmt_maxit <- get_param("eu_bmt_maxit", 300L)

eu_farm_cv <- get_param("eu_farm_cv", TRUE)
eu_farm_nfolds <- get_param("eu_farm_nfolds", 10L)
eu_farm_max_iter <- get_param("eu_farm_max_iter", 90000L)

eu_rf_num_trees <- get_param("eu_rf_num_trees", 1800L)
eu_rf_min_node_size <- get_param("eu_rf_min_node_size", 10L)

eu_tcsr_tc <- get_param("eu_tcsr_tc", 1.645)
eu_tcsr_L  <- get_param("eu_tcsr_L", 10L)
eu_tcsr_M  <- get_param("eu_tcsr_M", 10000L)

eu_chronos2_enabled <- get_param(
  "eu_chronos2_enabled",
  get_param("chronos2_enabled", TRUE)
)

eu_chronos2_uni_enabled <- get_param(
  "eu_chronos2_uni_enabled",
  get_param("chronos2_uni_enabled", TRUE)
)

eu_chronos2_multi_enabled <- get_param(
  "eu_chronos2_multi_enabled",
  get_param("chronos2_multi_enabled", TRUE)
)

eu_chronos2_covariate_enabled <- get_param(
  "eu_chronos2_covariate_enabled",
  get_param("chronos2_covariate_enabled", TRUE)
)

eu_chronos2_envname <- get_param(
  "eu_chronos2_envname",
  get_param("chronos2_envname", "chronos2-r")
)

eu_chronos2_device <- get_param(
  "eu_chronos2_device",
  get_param("chronos2_device", "cpu")
)

eu_chronos2_model_id <- get_param(
  "eu_chronos2_model_id",
  get_param("chronos2_model_id", "autogluon/chronos-2-small")
)

eu_chronos2_batch_size <- get_param(
  "eu_chronos2_batch_size",
  get_param("chronos2_batch_size", 8L)
)

eu_chronos2_cross_learning <- get_param(
  "eu_chronos2_cross_learning",
  get_param("chronos2_cross_learning", FALSE)
)

eu_chronos2_validate_inputs <- get_param(
  "eu_chronos2_validate_inputs",
  get_param("chronos2_validate_inputs", FALSE)
)

eu_chronos2_context_max <- get_param(
  "eu_chronos2_context_max",
  get_param("chronos2_context_max", 256L)
)

eu_chronos2_torch_threads <- get_param(
  "eu_chronos2_torch_threads",
  get_param("chronos2_torch_threads", 8L)
)

eu_chronos2_torch_interop_threads <- get_param(
  "eu_chronos2_torch_interop_threads",
  get_param("chronos2_torch_interop_threads", 1L)
)

eu_chronos_python <- get_param(
  "chronos_python",
  normalizePath(
    file.path(
      Sys.getenv("LOCALAPPDATA"),
      "r-miniconda",
      "envs",
      eu_chronos2_envname,
      "python.exe"
    ),
    winslash = "/",
    mustWork = FALSE
  )
)

# ============================================================
# PATHS
# ============================================================

if (!file.exists(eu_excel_path)) {
  stop(
    "EU Excel file not found:\n",
    eu_excel_path,
    "\nPut EUdata.xlsx there or set eu_excel_path in MAIN.R."
  )
}

eu_data_prep_dir <- file.path(eu_dir, "data_prep")
eu_dataset_dir <- file.path(eu_dir, eu_dataset_label)
eu_phase_dir <- file.path(eu_dataset_dir, "phase_results")
eu_tables_dir <- file.path(eu_dataset_dir, "tables")
eu_plots_dir <- file.path(eu_dataset_dir, "plots")
eu_target_plot_dir <- file.path(eu_plots_dir, "target_plots")

dir.create(eu_data_prep_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_phase_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_target_plot_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nEU Excel path:\n")
cat(eu_excel_path, "\n")

cat("\nEU output folder:\n")
cat(eu_dataset_dir, "\n")

cat("\nEU note:\n")
cat("Transformation codes are read from the Tcode row in the Excel file.\n")
cat("To change EU transformations, edit the Tcode row in that file.\n")

# ============================================================
# PREPARE DATA
# ============================================================

eu_prep <- eu_prepare_data(
  excel_path = eu_excel_path,
  sheet = eu_excel_sheet,
  lag_season = eu_lag_season,
  remove_vars_with_na_pre1980 = eu_remove_vars_with_na_pre1980,
  pre1980_cutoff_date = eu_pre1980_cutoff_date,
  dataset_label = eu_dataset_label,
  data_prep_dir = eu_data_prep_dir
)

panel_eu <- eu_prep$panel_can
eu_raw_panel <- eu_prep$eu_raw_panel
eu_transformed_full <- eu_prep$eu_transformed_full
eu_tcodes <- eu_prep$tcodes
eu_tcode_tbl <- eu_prep$tcode_tbl
eu_dataset_label <- eu_prep$dataset_label
eu_dataset_label_tex <- eu_prep$dataset_label_tex

# Recreate paths if helper sanitized the label
eu_dataset_dir <- file.path(eu_dir, eu_dataset_label)
eu_phase_dir <- file.path(eu_dataset_dir, "phase_results")
eu_tables_dir <- file.path(eu_dataset_dir, "tables")
eu_plots_dir <- file.path(eu_dataset_dir, "plots")
eu_target_plot_dir <- file.path(eu_plots_dir, "target_plots")

dir.create(eu_dataset_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_phase_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(eu_target_plot_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# TARGETS
# ============================================================

eu_real_activity_targets <- get_param(
  "eu_real_activity_targets",
  c("YER", "PCR")
)

eu_labour_income_targets <- get_param(
  "eu_labour_income_targets",
  c("WRN", "LNN")
)

eu_prices_monetary_housing_targets <- get_param(
  "eu_prices_monetary_housing_targets",
  c("HICP", "RPP", "M2")
)

eu_targets_all_raw <- list(
  table_real_activity = eu_real_activity_targets,
  table_labour_income = eu_labour_income_targets,
  table_prices_monetary_housing = eu_prices_monetary_housing_targets
)

eu_target_order_raw <- c(
  eu_real_activity_targets,
  eu_labour_income_targets,
  eu_prices_monetary_housing_targets
)

eu_target_labels_all <- get_param(
  "eu_target_labels_all",
  c(
    YER  = "Real GDP",
    PCR  = "Real household consumption",
    WRN  = "Average wage",
    LNN  = "Total employment",
    HICP = "HICP overall index",
    RPP  = "Residential property prices",
    M2   = "Monetary aggregate M2"
  )
)

eu_targets_obj <- eu_get_targets(
  panel_can = panel_eu,
  targets_all = eu_targets_all_raw,
  target_order = eu_target_order_raw,
  target_labels_all = eu_target_labels_all
)

eu_targets_all <- eu_targets_obj$targets_all
eu_target_order <- eu_targets_obj$target_order
eu_target_labels <- eu_targets_obj$target_labels

eu_real_activity_targets <- eu_targets_all$table_real_activity
eu_labour_income_targets <- eu_targets_all$table_labour_income
eu_prices_monetary_housing_targets <- eu_targets_all$table_prices_monetary_housing

cat("\nEU target order:\n")
print(eu_target_order)

utils::write.csv(
  data.frame(
    target = eu_target_order,
    label = unname(eu_target_labels[eu_target_order]),
    row.names = NULL
  ),
  file = file.path(eu_tables_dir, "eu_target_order.csv"),
  row.names = FALSE
)

# ============================================================
# MODEL SPECIFICATION
# ============================================================

eu_spec <- eu_make_model_spec(
  ocmt_pvals = eu_ocmt_pvals,
  bmt_pvals = eu_bmt_pvals
)

# ============================================================
# CONTROL OBJECT
# ============================================================

eu_ctrl <- eu_make_fortin_controls(
  n_workers = eu_n_workers,
  
  oos_start = eu_oos_start,
  oos_end   = eu_oos_end,
  horizons = eu_horizons,
  
  py = eu_py,
  px = eu_px,
  K_fixed = eu_K_fixed,
  
  retune_every_quarters = eu_retune_every_quarters,
  training_obs = eu_training_obs,
  
  glmnet_nlambda = eu_glmnet_nlambda,
  elastic_alpha_grid = eu_elastic_alpha_grid,
  fallback_lambda = eu_fallback_lambda,
  adalasso_eps = eu_adalasso_eps,
  
  ocmt_pvals = eu_ocmt_pvals,
  ocmt_max_iter = eu_ocmt_max_iter,
  
  bmt_pvals = eu_bmt_pvals,
  bmt_maxit = eu_bmt_maxit,
  
  farm_cv = eu_farm_cv,
  farm_nfolds = eu_farm_nfolds,
  farm_max_iter = eu_farm_max_iter,
  
  rf_num_trees = eu_rf_num_trees,
  rf_min_node_size = eu_rf_min_node_size,
  
  tcsr_tc = eu_tcsr_tc,
  tcsr_L = eu_tcsr_L,
  tcsr_M = eu_tcsr_M,
  
  chronos2_enabled = eu_chronos2_enabled,
  chronos2_uni_enabled = eu_chronos2_uni_enabled,
  chronos2_multi_enabled = eu_chronos2_multi_enabled,
  chronos2_covariate_enabled = eu_chronos2_covariate_enabled,
  chronos2_multi_target_names = eu_target_order,
  chronos2_envname = eu_chronos2_envname,
  chronos2_device = eu_chronos2_device,
  chronos2_model_id = eu_chronos2_model_id,
  chronos2_batch_size = eu_chronos2_batch_size,
  chronos2_cross_learning = eu_chronos2_cross_learning,
  chronos2_validate_inputs = eu_chronos2_validate_inputs,
  chronos2_context_max = eu_chronos2_context_max,
  chronos2_torch_threads = eu_chronos2_torch_threads,
  chronos2_torch_interop_threads = eu_chronos2_torch_interop_threads,
  
  fortin_dir = eu_dataset_dir
)

eu_ctrl$rolling$oos_end <- min(
  as.Date(eu_ctrl$rolling$oos_end),
  max(panel_eu$Date, na.rm = TRUE)
)

cat("\nEU control object:\n")
cat("OOS start: ", as.character(eu_ctrl$rolling$oos_start), "\n")
cat("OOS end:   ", as.character(eu_ctrl$rolling$oos_end), "\n")
cat("Horizons:  ", paste(eu_ctrl$rolling$horizons, collapse = ", "), "\n")
cat("Workers:   ", eu_ctrl$parallel$requested_workers, "\n")

# ============================================================
# TASK LIST
# ============================================================

eu_task_list <- eu_make_task_list(
  targets_all = eu_targets_all,
  ctrl = eu_ctrl
)

cat("\nEU task list:\n")
print(eu_task_list)

utils::write.csv(
  eu_task_list,
  file.path(eu_tables_dir, "eu_task_list.csv"),
  row.names = FALSE
)

# ============================================================
# RUN NON-CHRONOS PHASE
# ============================================================

eu_nonchronos_rds <- file.path(eu_phase_dir, "eu_nonchronos_results.rds")
eu_merged_rds <- file.path(eu_phase_dir, "eu_merged_results.rds")

cat("\nStarting EU non-Chronos phase...\n")

eu_nonchronos_results <- eu_run_task_grid_nonchronos(
  panel_can = panel_eu,
  task_list = eu_task_list,
  ctrl = eu_ctrl,
  spec = eu_spec,
  models_to_run = eu_spec$non_chronos_model_names
)

cat("\nEU non-Chronos phase finished.\n")

saveRDS(
  list(
    nonchronos_results = eu_nonchronos_results,
    task_list = eu_task_list,
    ctrl = eu_ctrl,
    spec = eu_spec,
    target_order = eu_target_order,
    targets_all = eu_targets_all,
    target_labels = eu_target_labels,
    dataset_label = eu_dataset_label,
    data_prep = eu_prep
  ),
  eu_nonchronos_rds
)

cat("\nSaved EU non-Chronos results to:\n")
cat(eu_nonchronos_rds, "\n")

future::plan(future::sequential)
gc()

# ============================================================
# RUN CHRONOS PHASE, IF ENABLED
# ============================================================

eu_chronos_results <- NULL
eu_chronos_obj <- NULL

if (isTRUE(eu_ctrl$model$chronos2$enabled)) {
  cat("\nStarting EU Chronos phase...\n")
  
  eu_chronos_obj <- eu_run_chronos_external(
    panel_can = panel_eu,
    nonchronos_results = eu_nonchronos_results,
    task_list = eu_task_list,
    ctrl = eu_ctrl,
    target_order = eu_target_order,
    spec = eu_spec,
    phase_dir = eu_phase_dir,
    chronos_python = eu_chronos_python,
    prefix = "eu"
  )
  
  eu_chronos_results <- eu_chronos_obj$chronos_results
  
  cat("\nEU Chronos phase finished.\n")
} else {
  cat("\nChronos disabled for EU. Merging non-Chronos results only.\n")
}

# ============================================================
# MERGE RESULTS
# ============================================================

eu_all_results <- eu_build_merged_results(
  nonchronos_results = eu_nonchronos_results,
  chronos_results = eu_chronos_results,
  task_list = eu_task_list,
  target_order = eu_target_order,
  spec = eu_spec
)

eu_all_results <- eu_all_results[eu_target_order]
names(eu_all_results) <- eu_target_order

eu_fallback_summary <- eu_summarise_fallback_counts(
  all_results = eu_all_results,
  spec = eu_spec
)

cat("\nEU fallback summary:\n")
print(eu_fallback_summary$as_table)
print(eu_fallback_summary$tcsr_as_table)

saveRDS(
  list(
    all_results = eu_all_results,
    fallback_summary = eu_fallback_summary,
    nonchronos_results = eu_nonchronos_results,
    chronos_results = eu_chronos_results,
    task_list = eu_task_list,
    ctrl = eu_ctrl,
    spec = eu_spec,
    chronos_pred_long = if (!is.null(eu_chronos_obj)) eu_chronos_obj$chronos_pred_long else NULL,
    target_order = eu_target_order,
    targets_all = eu_targets_all,
    target_labels = eu_target_labels,
    dataset_label = eu_dataset_label,
    data_prep = eu_prep
  ),
  eu_merged_rds
)

cat("\nSaved EU merged results to:\n")
cat(eu_merged_rds, "\n")

utils::write.csv(
  eu_fallback_summary$as_table,
  file = file.path(eu_tables_dir, "eu_fallback_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  eu_fallback_summary$tcsr_as_table,
  file = file.path(eu_tables_dir, "eu_tcsr_fallback_summary.csv"),
  row.names = FALSE
)

# ============================================================
# PLOTS
# ============================================================

eu_raw_plot_labels <- c(
  YER  = "YER - Real GDP",
  PCR  = "PCR - Real household consumption",
  HICP = "HICP - HICP overall index",
  RPP  = "RPP - Residential property prices",
  WRN  = "WRN - Average wage",
  LNN  = "LNN - Total employment",
  M2   = "M2 - Monetary aggregate M2"
)

eu_plot_target_series(
  panel_data = panel_eu,
  targets = eu_target_order,
  target_labels = eu_target_labels,
  out_dir = eu_target_plot_dir
)

eu_raw_plot_file <- file.path(eu_plots_dir, paste0("all_raw_series_", eu_dataset_label, ".pdf"))
eu_transformed_plot_file <- file.path(eu_plots_dir, paste0("all_transformed_series_", eu_dataset_label, ".pdf"))

eu_plot_targets_use <- intersect(
  c("YER", "PCR", "HICP", "RPP", "WRN", "LNN", "M2"),
  names(eu_raw_panel)
)

eu_plot_series_combined(
  panel_data = eu_raw_panel,
  targets = eu_plot_targets_use,
  target_labels = eu_raw_plot_labels,
  tcodes = eu_tcodes,
  out_file = eu_raw_plot_file,
  title_suffix = "Raw EU series",
  add_zero_line = FALSE
)

eu_plot_series_combined(
  panel_data = panel_eu,
  targets = eu_plot_targets_use,
  target_labels = eu_raw_plot_labels,
  tcodes = eu_tcodes,
  out_file = eu_transformed_plot_file,
  title_suffix = paste("Transformed EU series -", eu_dataset_label),
  add_zero_line = TRUE
)

# ============================================================
# LATEX TABLES
# ============================================================

save_eu_group_table_if_any <- function(targets, target_labels, file, caption, label) {
  targets <- intersect(targets, names(eu_all_results))
  
  if (length(targets) == 0) {
    cat("\nSkipping empty EU table:", file, "\n")
    return(invisible(NULL))
  }
  
  eu_save_fortin_table_grouped_tex(
    all_results = eu_all_results,
    targets = targets,
    spec = eu_spec,
    target_labels = unname(target_labels[targets]),
    horizons = eu_ctrl$rolling$horizons,
    file = file,
    caption = caption,
    label = label
  )
  
  cat("\nSaved EU table:\n")
  cat(file, "\n")
  
  invisible(file)
}

eu_tab_real_activity_file <- file.path(
  eu_tables_dir,
  paste0("tab_real_activity_", eu_dataset_label, ".tex")
)

eu_tab_labour_income_file <- file.path(
  eu_tables_dir,
  paste0("tab_labour_income_", eu_dataset_label, ".tex")
)

eu_tab_prices_monetary_housing_file <- file.path(
  eu_tables_dir,
  paste0("tab_prices_monetary_housing_", eu_dataset_label, ".tex")
)

save_eu_group_table_if_any(
  targets = eu_real_activity_targets,
  target_labels = eu_target_labels,
  file = eu_tab_real_activity_file,
  caption = paste0("Forecasting real activity variables (", eu_dataset_label_tex, ")"),
  label = paste0("tab:eu_real_activity_", eu_dataset_label)
)

save_eu_group_table_if_any(
  targets = eu_labour_income_targets,
  target_labels = eu_target_labels,
  file = eu_tab_labour_income_file,
  caption = paste0("Forecasting labour and income variables (", eu_dataset_label_tex, ")"),
  label = paste0("tab:eu_labour_income_", eu_dataset_label)
)

save_eu_group_table_if_any(
  targets = eu_prices_monetary_housing_targets,
  target_labels = eu_target_labels,
  file = eu_tab_prices_monetary_housing_file,
  caption = paste0("Forecasting prices, monetary, and housing variables (", eu_dataset_label_tex, ")"),
  label = paste0("tab:eu_prices_monetary_housing_", eu_dataset_label)
)

# ============================================================
# SUMMARY FILE
# ============================================================

eu_summary_file <- file.path(eu_dataset_dir, "eu_export_summary.txt")

cat(
  "EU export completed\n",
  "===================\n\n",
  "Excel file:\n",
  eu_excel_path, "\n\n",
  "Important transformation note:\n",
  "Transformation codes are read from the Tcode row in the Excel file.\n",
  "To change transformations, edit the Tcode row in the EU Excel file.\n\n",
  "Dataset label:\n",
  eu_dataset_label, "\n\n",
  "Output folder:\n",
  eu_dataset_dir, "\n\n",
  "Phase folder:\n",
  eu_phase_dir, "\n\n",
  "Tables folder:\n",
  eu_tables_dir, "\n\n",
  "Plots folder:\n",
  eu_plots_dir, "\n\n",
  "Evaluation sample:\n",
  as.character(eu_ctrl$rolling$oos_start), " to ",
  as.character(eu_ctrl$rolling$oos_end), "\n\n",
  "Horizons:\n",
  paste(eu_ctrl$rolling$horizons, collapse = ", "), "\n\n",
  "Targets:\n",
  paste(eu_target_order, collapse = ", "), "\n\n",
  "OCMT p-values:\n",
  paste(names(eu_ctrl$selection$ocmt$pvals), eu_ctrl$selection$ocmt$pvals, collapse = ", "), "\n\n",
  "BMT p-values:\n",
  paste(names(eu_ctrl$selection$bmt$pvals), eu_ctrl$selection$bmt$pvals, collapse = ", "), "\n\n",
  "FarmSelect:\n",
  "cv = ", eu_ctrl$selection$farm$cv,
  ", nfolds = ", eu_ctrl$selection$farm$nfolds,
  ", max.iter = ", eu_ctrl$selection$farm$max.iter, "\n\n",
  "Chronos enabled:\n",
  eu_ctrl$model$chronos2$enabled, "\n\n",
  "Saved files:\n",
  " - eu_nonchronos_results.rds\n",
  " - eu_merged_results.rds\n",
  " - eu_fallback_summary.csv\n",
  " - eu_tcsr_fallback_summary.csv\n",
  " - EU LaTeX tables in tables/\n",
  " - raw/transformed series plots in plots/\n",
  file = eu_summary_file
)

cat("\nSaved EU summary file:\n")
cat(eu_summary_file, "\n")

cat("\nEU application finished.\n")
cat("Outputs saved to:\n")
cat(eu_dataset_dir, "\n")