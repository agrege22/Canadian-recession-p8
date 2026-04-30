# ============================================================
# CANADA SAMPLE CLEANING
# Shared cleaned panels for targets, recession, and variable selection.
# ============================================================

cat("\nRunning Canada sample cleaning...\n")

# ============================================================
# LOAD SCRAPED DATA
# ============================================================

panel_nonprov_raw <- readRDS(file.path(scraping_dir, "panel_nonprov.rds"))

names(panel_nonprov_raw)[1] <- "Date"
panel_nonprov_raw$Date <- as.Date(panel_nonprov_raw$Date)

# Make all non-date columns numeric
num_cols <- setdiff(names(panel_nonprov_raw), "Date")

panel_nonprov_raw[num_cols] <- lapply(
  panel_nonprov_raw[num_cols],
  function(x) suppressWarnings(as.numeric(x))
)

# ============================================================
# APPLY NO-EM-PCA SAMPLE CUT
# ============================================================

canada_cleaning_info <- choose_window_and_clean_panel(
  df = panel_nonprov_raw,
  start_date = canada_cleaning_start_date,
  predictor_start_col = canada_cleaning_predictor_start_col,
  k = canada_cleaning_min_predictors_available,
  date_col = "Date",
  panel_name = "panel_nonprov"
)

panel_nonprov_clean_with_recession <- canada_cleaning_info$panel_clean

# Version for continuous-target forecasting:
# remove recession target columns after the sample window has been chosen.
panel_nonprov_clean_no_recession <- panel_nonprov_clean_with_recession |>
  dplyr::select(-dplyr::matches("^recession"))

# ============================================================
# SAVE OUTPUTS
# ============================================================

dir.create(canada_cleaned_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  canada_cleaning_info,
  file.path(canada_cleaned_dir, "canada_cleaning_info.rds")
)

saveRDS(
  panel_nonprov_clean_with_recession,
  file.path(canada_cleaned_dir, "panel_nonprov_clean_with_recession.rds")
)

saveRDS(
  panel_nonprov_clean_no_recession,
  file.path(canada_cleaned_dir, "panel_nonprov_clean_no_recession.rds")
)

utils::write.csv(
  canada_cleaning_info$first_available_table,
  file.path(canada_cleaned_dir, "first_available_table.csv"),
  row.names = FALSE
)

utils::write.csv(
  canada_cleaning_info$availability_count_by_date,
  file.path(canada_cleaned_dir, "availability_count_by_date.csv"),
  row.names = FALSE
)

utils::write.csv(
  data.frame(
    start_date = canada_cleaning_info$start_date,
    end_date = canada_cleaning_info$end_date,
    first_date_with_k_predictors = canada_cleaning_info$first_date_with_k_predictors,
    n_kept_predictors = length(canada_cleaning_info$kept_predictors),
    n_dropped_at_start = length(canada_cleaning_info$dropped_predictors_at_start),
    n_dropped_inside_window = length(canada_cleaning_info$dropped_predictors_inside_window)
  ),
  file.path(canada_cleaned_dir, "cleaning_summary.csv"),
  row.names = FALSE
)

cat("\nSaved cleaned Canada panels to:\n")
cat(canada_cleaned_dir, "\n")