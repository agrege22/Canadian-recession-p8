# ============================================================
# CANADA VARIABLE-SELECTION COMPARISON
# ============================================================

cat("\nRunning Canada variable-selection comparison script...\n")

# ============================================================
# CHECK REQUIRED FUNCTIONS
# ============================================================

required_functions <- c(
  "compare_methods_expanding_fixed",
  "count_selected_variables",
  "build_farmselect_coef_long",
  "build_selection_snapshot_tables",
  "make_top_selection_heatmap_data",
  "plot_selected_counts_separate",
  "plot_selected_counts_together",
  "plot_top_selected_variables",
  "plot_top_selection_heatmaps",
  "plot_probs_separate",
  "plot_probs_together",
  "plot_probs_faceted",
  "save_plot_pdf",
  "clean_method_name"
)

missing_functions <- required_functions[
  !vapply(required_functions, exists, logical(1), mode = "function")
]

if (length(missing_functions) > 0) {
  stop(
    "Missing variable-selection helper functions. Put them in R/canada_variable_selection_helpers.R.\nMissing: ",
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
    "Check that panel_nonprov_clean_with_recession.rds was created correctly."
  )
}

panel_rec <- panel_rec |>
  dplyr::arrange(.data$Date) |>
  dplyr::mutate(
    recession = dplyr::lag(as.numeric(.data$recession_tplus1), 1)
  )

rec_target <- "recession"

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

panel_to_use <- panel_rec_model |>
  dplyr::select(Date, dplyr::all_of(rec_target), dplyr::everything())

panel_name <- canada_selection_panel_name

cat("\nLoaded panel for variable-selection comparison:\n")
cat("Rows:", nrow(panel_to_use), "\n")
cat("Columns:", ncol(panel_to_use), "\n")
cat("Start:", as.character(min(panel_to_use$Date, na.rm = TRUE)), "\n")
cat("End:", as.character(max(panel_to_use$Date, na.rm = TRUE)), "\n")
cat("Target:", rec_target, "\n")

# ============================================================
# SETTINGS FROM MAIN.R
# ============================================================

initial_train     <- canada_selection_initial_train
min_class_count   <- canada_selection_min_class_count

max.iter_farm     <- canada_selection_max_iter_farm
max_iter_ocmt     <- canada_selection_max_iter_ocmt
maxit_bmt         <- canada_selection_maxit_bmt
maxit_post_logit  <- canada_selection_maxit_post_logit

threshold_class   <- canada_selection_threshold_class

fixed_hyper <- list(
  "OCMT"       = canada_selection_ocmt_pval,
  "BMT"        = canada_selection_bmt_pval,
  "FarmSelect" = canada_selection_farm_tau
)

n_selection_snapshots <- canada_selection_n_snapshots
heatmap_top_n         <- canada_selection_heatmap_top_n

use_parallel  <- canada_selection_use_parallel
n_workers     <- canada_selection_n_workers
show_progress <- canada_selection_show_progress

cat("\nVariable-selection configuration:\n")
cat("Initial train:", initial_train, "\n")
cat("Min class count:", min_class_count, "\n")
cat("OCMT p-value:", fixed_hyper$OCMT, "\n")
cat("BMT p-value:", fixed_hyper$BMT, "\n")
cat("FarmSelect tau:", fixed_hyper$FarmSelect, "\n")
cat("Workers:", n_workers, "\n")

# ============================================================
# RUN
# ============================================================

tm <- system.time({
  oos_res <- compare_methods_expanding_fixed(
    panel_df = panel_to_use,
    panel_label = panel_name,
    initial_train = initial_train,
    min_class_count = min_class_count,
    max.iter_farm = max.iter_farm,
    max_iter_ocmt = max_iter_ocmt,
    maxit_bmt = maxit_bmt,
    maxit_post_logit = maxit_post_logit,
    threshold_class = threshold_class,
    fixed_hyper = fixed_hyper,
    use_parallel = use_parallel,
    n_workers = n_workers,
    show_progress = show_progress
  )
})

cat("\nRuntime:\n")
print(tm)

cat("\nMain evaluation metrics:\n")
print(oos_res$metrics)

cat("\nFixed hyperparameter table:\n")
print(oos_res$tuning_tbl)

# ============================================================
# DIAGNOSTIC TABLES
# ============================================================

diag_tbl <- oos_res$pred_df |>
  dplyr::group_by(.data$method) |>
  dplyr::summarise(
    n_oos = dplyr::n(),
    avg_n_selected = mean(.data$n_selected),
    pct_zero_selected = mean(.data$n_selected == 0),
    pct_mean_fallback = mean(grepl("mean", .data$fit_type, fixed = TRUE)),
    n_unique_probs = dplyr::n_distinct(round(.data$p, 10)),
    min_p = min(.data$p),
    max_p = max(.data$p),
    .groups = "drop"
  )

farm_diag_tbl <- oos_res$pred_df |>
  dplyr::filter(.data$method == "FarmSelect") |>
  dplyr::summarise(
    n_oos = dplyr::n(),
    n_lambda_available = sum(is.finite(.data$lambda_min)),
    pct_lambda_available = mean(is.finite(.data$lambda_min)),
    mean_lambda_min = mean(.data$lambda_min, na.rm = TRUE),
    median_lambda_min = median(.data$lambda_min, na.rm = TRUE),
    pct_nonempty_coef_string = mean(nzchar(.data$coef_full_string)),
    .groups = "drop"
  )

qps_lps_tbl <- oos_res$metrics |>
  dplyr::select(.data$panel, .data$target, .data$h, .data$method, .data$n_oos, .data$qps, .data$lps) |>
  dplyr::arrange(.data$qps, .data$lps)

farm_pred_detail_tbl <- oos_res$pred_df |>
  dplyr::filter(.data$method == "FarmSelect") |>
  dplyr::select(
    Date, y, p, method, hyper_used, fit_type,
    n_selected, selected_vars,
    model_form, nfolds_used,
    n_factor_nonzero, n_resid_nonzero,
    intercept_hat, lambda_min,
    coef_full_string, factor_coef_string,
    resid_coef_string, selected_coef_string
  )

farm_coef_long_tbl <- build_farmselect_coef_long(oos_res$pred_df)

# ============================================================
# SELECTION TABLES
# ============================================================

sel_count_tbl <- count_selected_variables(oos_res$pred_df)

top20_tbl <- sel_count_tbl |>
  dplyr::group_by(.data$methods) |>
  dplyr::slice_max(order_by = .data$times_selected, n = 20, with_ties = FALSE) |>
  dplyr::ungroup()

selection_snapshots <- build_selection_snapshot_tables(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col,
  n_snapshots = n_selection_snapshots
)

heatmap_top40 <- make_top_selection_heatmap_data(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col,
  top_n = heatmap_top_n,
  exclude_always_selected_for_ocmt = TRUE
)

top40_tbl <- heatmap_top40$top_tbl
heatmap40_tbl <- heatmap_top40$heat_tbl
ocmt_always_selected_tbl <- heatmap_top40$ocmt_always_tbl

cat("\nTop selected variables:\n")
print(top20_tbl, n = min(60, nrow(top20_tbl)), width = Inf)

cat("\nOCMT always-selected variables:\n")
print(ocmt_always_selected_tbl, n = nrow(ocmt_always_selected_tbl), width = Inf)

# ============================================================
# PLOTS
# ============================================================

sel_plots <- plot_selected_counts_separate(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

sel_plot_all <- plot_selected_counts_together(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

top_plots <- plot_top_selected_variables(
  sel_count_tbl = sel_count_tbl,
  top_n = 30
)

selection_heatmap_plots <- plot_top_selection_heatmaps(
  heat_tbl = heatmap40_tbl,
  top_tbl = top40_tbl,
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

prob_plots_sep <- plot_probs_separate(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

prob_plot_all <- plot_probs_together(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

prob_plot_facet <- plot_probs_faceted(
  pred_df = oos_res$pred_df,
  target_col = oos_res$target_col
)

# ============================================================
# EXPORT FOLDERS
# ============================================================

out_dir <- file.path(
  canada_selection_dir,
  paste0("QPS_LPS_SELECTION_", panel_name, "_noretune")
)

plots_dir  <- file.path(out_dir, "plots")
tables_dir <- file.path(out_dir, "tables")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nSaving variable-selection outputs to:\n")
cat(out_dir, "\n")

# ============================================================
# SAVE RDS
# ============================================================

saveRDS(
  list(
    oos_res = oos_res,
    diag_tbl = diag_tbl,
    farm_diag_tbl = farm_diag_tbl,
    qps_lps_tbl = qps_lps_tbl,
    farm_pred_detail_tbl = farm_pred_detail_tbl,
    farm_coef_long_tbl = farm_coef_long_tbl,
    sel_count_tbl = sel_count_tbl,
    top20_tbl = top20_tbl,
    selection_snapshots = selection_snapshots,
    heatmap_top40 = heatmap_top40,
    top40_tbl = top40_tbl,
    heatmap40_tbl = heatmap40_tbl,
    ocmt_always_selected_tbl = ocmt_always_selected_tbl,
    panel_name = panel_name,
    rec_target = rec_target,
    fixed_hyper = fixed_hyper
  ),
  file.path(out_dir, "canada_variable_selection_results.rds")
)

# ============================================================
# SAVE TABLES
# ============================================================

write.csv(qps_lps_tbl, file.path(tables_dir, "qps_lps_table.csv"), row.names = FALSE, na = "")
write.csv(oos_res$tuning_tbl, file.path(tables_dir, "fixed_hyper_table.csv"), row.names = FALSE, na = "")
write.csv(oos_res$metrics, file.path(tables_dir, "metrics_full.csv"), row.names = FALSE, na = "")
write.csv(diag_tbl, file.path(tables_dir, "diagnostic_table.csv"), row.names = FALSE, na = "")
write.csv(farm_diag_tbl, file.path(tables_dir, "farmselect_diagnostic_table.csv"), row.names = FALSE, na = "")
write.csv(oos_res$pred_df, file.path(tables_dir, "oos_predictions_full.csv"), row.names = FALSE, na = "")

write.csv(farm_pred_detail_tbl, file.path(tables_dir, "farmselect_prediction_details.csv"), row.names = FALSE, na = "")
write.csv(farm_coef_long_tbl, file.path(tables_dir, "farmselect_coefficients_long.csv"), row.names = FALSE, na = "")

write.csv(sel_count_tbl, file.path(tables_dir, "selected_variable_counts.csv"), row.names = FALSE, na = "")
write.csv(selection_snapshots$summary_tbl, file.path(tables_dir, "selected_variable_snapshots.csv"), row.names = FALSE, na = "")
write.csv(selection_snapshots$long_tbl, file.path(tables_dir, "selected_variable_snapshots_long.csv"), row.names = FALSE, na = "")
write.csv(top40_tbl, file.path(tables_dir, "top40_most_selected.csv"), row.names = FALSE, na = "")
write.csv(ocmt_always_selected_tbl, file.path(tables_dir, "ocmt_always_selected.csv"), row.names = FALSE, na = "")
write.csv(heatmap40_tbl, file.path(tables_dir, "heatmap_top40_long.csv"), row.names = FALSE, na = "")

# ============================================================
# SAVE PLOTS
# ============================================================

sel_named <- purrr::imap(
  sel_plots,
  ~ stats::setNames(list(.x), paste0("selected_", clean_method_name(.y)))
) |>
  purrr::flatten()

top_named <- purrr::imap(
  top_plots,
  ~ stats::setNames(list(.x), paste0("top_", clean_method_name(.y)))
) |>
  purrr::flatten()

heatmap_named <- purrr::imap(
  selection_heatmap_plots,
  ~ stats::setNames(list(.x), paste0("heatmap_top40_", clean_method_name(.y)))
) |>
  purrr::flatten()

prob_sep_named <- purrr::imap(
  prob_plots_sep,
  ~ stats::setNames(list(.x), paste0("probs_", clean_method_name(.y)))
) |>
  purrr::flatten()

plots_to_save <- c(
  sel_named,
  list(selected_all_methods = sel_plot_all),
  top_named,
  heatmap_named,
  list(
    probs_all_methods = prob_plot_all,
    probs_faceted = prob_plot_facet
  ),
  prob_sep_named
)

for (nm in names(plots_to_save)) {
  if (nm %in% c("probs_all_methods", "probs_faceted", "selected_all_methods")) {
    save_plot_pdf(
      plot_obj = plots_to_save[[nm]],
      filename = file.path(plots_dir, paste0(nm, ".pdf")),
      width = 11,
      height = 7.5
    )
  } else if (grepl("^heatmap_top40_", nm)) {
    save_plot_pdf(
      plot_obj = plots_to_save[[nm]],
      filename = file.path(plots_dir, paste0(nm, ".pdf")),
      width = 12,
      height = 9.5
    )
  } else {
    save_plot_pdf(
      plot_obj = plots_to_save[[nm]],
      filename = file.path(plots_dir, paste0(nm, ".pdf")),
      width = 12,
      height = 8
    )
  }
}

# ============================================================
# SUMMARY FILE
# ============================================================

summary_file <- file.path(out_dir, "export_summary.txt")

cat(
  "Export completed\n",
  "================\n\n",
  "Main folder:\n",
  out_dir, "\n\n",
  "Plots folder:\n",
  plots_dir, "\n\n",
  "Tables folder:\n",
  tables_dir, "\n\n",
  "Fixed hyperparameters:\n",
  " - OCMT pval = ", fixed_hyper$OCMT, "\n",
  " - BMT pval = ", fixed_hyper$BMT, "\n",
  " - FarmSelect tau = ", fixed_hyper$FarmSelect, "\n\n",
  "Selector iteration settings:\n",
  " - OCMT max_iter = ", max_iter_ocmt, "\n",
  " - BMT maxit = ", maxit_bmt, "\n",
  " - FarmSelect max.iter = ", max.iter_farm, "\n",
  " - post-selection logit maxit = ", maxit_post_logit, "\n\n",
  "Downstream model after selection:\n",
  " - OCMT and BMT use ordinary logistic regression via glm(family = binomial)\n",
  " - FarmSelect selection is followed by the same ordinary post-selection logit if at least one variable is selected\n",
  " - if no variables are selected, the training-sample recession frequency is used\n\n",
  "OCMT top-40 rule:\n",
  " - variables selected at every OOS date are excluded from the OCMT top-40 heatmap/table\n",
  " - these are instead saved in ocmt_always_selected.csv\n\n",
  "Saved tables are in the tables folder.\n",
  "Saved plots are in the plots folder.\n",
  file = summary_file
)

cat("\nCanada variable-selection comparison finished.\n")
cat("Outputs saved to:\n")
cat(out_dir, "\n")