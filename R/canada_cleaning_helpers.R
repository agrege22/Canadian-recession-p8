# ============================================================
# CANADA SAMPLE CLEANING HELPERS
# No EM-PCA: choose start date, keep predictors available at start,
# and cut sample before later missingness appears.
# ============================================================

choose_window_and_clean_panel <- function(df,
                                          start_date,
                                          predictor_start_col = 3,
                                          k = 10,
                                          date_col = NULL,
                                          panel_name = "panel") {
  if (is.null(date_col)) {
    date_col <- names(df)[1]
  }
  
  if (!date_col %in% names(df)) {
    stop("date_col not found in df.")
  }
  
  names(df)[names(df) == date_col] <- "Date"
  
  if (!"Date" %in% names(df)) {
    stop("Data must contain a Date column.")
  }
  
  if (predictor_start_col > ncol(df)) {
    stop("predictor_start_col is larger than the number of columns.")
  }
  
  start_date <- as.Date(start_date)
  
  df <- df |>
    dplyr::arrange(.data$Date) |>
    dplyr::mutate(Date = as.Date(.data$Date)) |>
    dplyr::filter(.data$Date >= start_date)
  
  if (nrow(df) == 0) {
    stop("No observations on or after the chosen start date.")
  }
  
  fixed_cols <- names(df)[seq_len(predictor_start_col - 1)]
  pred_names <- names(df)[predictor_start_col:ncol(df)]
  
  df[pred_names] <- lapply(
    df[pred_names],
    function(x) suppressWarnings(as.numeric(x))
  )
  
  first_tbl <- lapply(pred_names, function(v) {
    idx_nonmiss <- which(!is.na(df[[v]]))
    idx_miss <- which(is.na(df[[v]]))
    
    data.frame(
      variable = v,
      available_at_start = !is.na(df[[v]][1]),
      first_available_from_start = if (length(idx_nonmiss) == 0) {
        as.Date(NA)
      } else {
        as.Date(df$Date[min(idx_nonmiss)])
      },
      first_missing_from_start = if (length(idx_miss) == 0) {
        as.Date(NA)
      } else {
        as.Date(df$Date[min(idx_miss)])
      },
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$first_available_from_start, .data$variable)
  
  avail_count <- rowSums(!is.na(df[, pred_names, drop = FALSE]))
  
  first_k_idx <- which(avail_count >= k)[1]
  
  first_k_date <- if (is.na(first_k_idx)) {
    as.Date(NA)
  } else {
    as.Date(df$Date[first_k_idx])
  }
  
  keep_from_start <- pred_names[!is.na(df[1, pred_names, drop = TRUE])]
  dropped_at_start <- setdiff(pred_names, keep_from_start)
  
  if (length(keep_from_start) == 0) {
    cat("\n==============================\n")
    cat("Panel:", panel_name, "\n")
    cat("Chosen start date:", as.character(start_date), "\n")
    cat("No predictors are available at the chosen start date.\n")
    cat("==============================\n\n")
    
    return(list(
      panel_name = panel_name,
      start_date = start_date,
      end_date = as.Date(NA),
      first_available_table = first_tbl,
      availability_count_by_date = data.frame(
        Date = as.Date(df$Date),
        n_available_predictors = avail_count
      ),
      first_date_with_k_predictors = first_k_date,
      kept_predictors = character(0),
      dropped_predictors_at_start = dropped_at_start,
      dropped_predictors_inside_window = character(0),
      panel_clean = df[0, fixed_cols, drop = FALSE]
    ))
  }
  
  first_missing_dates <- sapply(keep_from_start, function(v) {
    idx <- which(is.na(df[[v]]))
    if (length(idx) == 0) as.Date(NA) else as.Date(df$Date[min(idx)])
  })
  
  if (all(is.na(first_missing_dates))) {
    end_date <- max(df$Date)
  } else {
    first_missing_any <- min(first_missing_dates, na.rm = TRUE)
    valid_dates <- df$Date[df$Date < first_missing_any]
    end_date <- if (length(valid_dates) == 0) as.Date(NA) else max(valid_dates)
  }
  
  if (is.na(end_date) || end_date < start_date) {
    df_window <- df[0, , drop = FALSE]
  } else {
    df_window <- df |>
      dplyr::filter(.data$Date >= start_date, .data$Date <= end_date)
  }
  
  if (nrow(df_window) > 0) {
    keep_final <- keep_from_start[
      colSums(is.na(df_window[, keep_from_start, drop = FALSE])) == 0
    ]
  } else {
    keep_final <- character(0)
  }
  
  dropped_inside_window <- setdiff(keep_from_start, keep_final)
  
  df_clean <- if (nrow(df_window) > 0) {
    df_window |>
      dplyr::select(dplyr::all_of(fixed_cols), dplyr::all_of(keep_final))
  } else {
    df_window[, fixed_cols, drop = FALSE]
  }
  
  cat("\n==============================\n")
  cat("Panel:", panel_name, "\n")
  cat("Chosen start date:", as.character(start_date), "\n")
  cat("Automatic end date:", as.character(end_date), "\n")
  cat("First date with at least", k, "predictors available:", as.character(first_k_date), "\n")
  cat("Predictors in original panel:", length(pred_names), "\n")
  cat("Predictors available at chosen start:", length(keep_from_start), "\n")
  cat("Predictors dropped because missing at start:", length(dropped_at_start), "\n")
  cat("Predictors kept in final window:", length(keep_final), "\n")
  cat("Predictors dropped inside final window:", length(dropped_inside_window), "\n")
  cat("Final sample rows:", nrow(df_clean), "\n")
  cat("==============================\n\n")
  
  list(
    panel_name = panel_name,
    start_date = start_date,
    end_date = end_date,
    first_available_table = first_tbl,
    availability_count_by_date = data.frame(
      Date = as.Date(df$Date),
      n_available_predictors = avail_count
    ),
    first_date_with_k_predictors = first_k_date,
    kept_predictors = keep_final,
    dropped_predictors_at_start = dropped_at_start,
    dropped_predictors_inside_window = dropped_inside_window,
    panel_clean = df_clean
  )
}