# ============================================================
# CANADA CONTINUOUS TARGETS
# ============================================================

cat("\nRunning Canada continuous-target script...\n")

# ============================================================
# CHECK REQUIRED FUNCTIONS
# ============================================================

required_functions <- c(
  "make_fortin_controls",
  "run_task_grid_nonchronos",
  "merge_task_details",
  "summarise_fallback_counts",
  "save_fortin_table_grouped_tex"
)

missing_functions <- required_functions[
  !vapply(required_functions, exists, logical(1), mode = "function")
]

if (length(missing_functions) > 0) {
  stop(
    "Missing helper functions. Put the long Canada target helper code in R/canada_targets_helpers.R and source it from MAIN.R.\nMissing: ",
    paste(missing_functions, collapse = ", ")
  )
}

# ============================================================
# LOAD CLEANED CANADA DATA
# ============================================================

cleaned_panel_path <- file.path(
  canada_cleaned_dir,
  "panel_nonprov_clean_no_recession.rds"
)

if (!file.exists(cleaned_panel_path)) {
  stop(
    "Cleaned Canada panel not found:\n",
    cleaned_panel_path,
    "\nRun MAIN.R with run_canada_cleaning <- 'auto' or run_canada_cleaning <- 'force' first."
  )
}

panel_can <- readRDS(cleaned_panel_path)

names(panel_can)[1] <- "Date"
panel_can$Date <- as.Date(panel_can$Date)

panel_can <- panel_can |>
  dplyr::arrange(.data$Date)

num_cols <- setdiff(names(panel_can), "Date")

panel_can[num_cols] <- lapply(
  panel_can[num_cols],
  function(x) suppressWarnings(as.numeric(x))
)

cat("\nLoaded cleaned Canada continuous-target panel:\n")
cat("Rows:", nrow(panel_can), "\n")
cat("Columns:", ncol(panel_can), "\n")
cat("Start:", as.character(min(panel_can$Date, na.rm = TRUE)), "\n")
cat("End:", as.character(max(panel_can$Date, na.rm = TRUE)), "\n")

# ============================================================
# CONTROL OBJECT
# ============================================================

ctrl <- make_fortin_controls(
  n_workers = canada_n_workers,
  
  oos_start = canada_targets_oos_start,
  oos_end = canada_targets_oos_end,
  horizons = canada_targets_horizons,
  
  py = canada_py,
  px = canada_px,
  K_fixed = canada_K_fixed,
  
  retune_every_months = canada_retune_every_months,
  træningsår = canada_training_months,
  
  glmnet_nlambda = canada_glmnet_nlambda,
  elastic_alpha_grid = canada_elastic_alpha_grid,
  
  ocmt_pvals = canada_ocmt_pvals,
  ocmt_max_iter = canada_ocmt_max_iter,
  
  bmt_pvals = canada_bmt_pvals,
  bmt_maxit = canada_bmt_maxit,
  
  farm_cv = canada_farm_cv,
  farm_nfolds = canada_farm_nfolds,
  farm_max_iter = canada_farm_max_iter,
  
  rf_num_trees = canada_rf_num_trees,
  rf_min_node_size = canada_rf_min_node_size,
  
  tcsr_tc = canada_tcsr_tc,
  tcsr_M = canada_tcsr_M,
  
  chronos2_enabled = chronos2_enabled,
  chronos2_uni_enabled = chronos2_uni_enabled,
  chronos2_multi_enabled = chronos2_multi_enabled,
  chronos2_covariate_enabled = chronos2_covariate_enabled,
  
  chronos2_envname = chronos2_envname,
  chronos2_device = chronos2_device,
  chronos2_model_id = chronos2_model_id,
  chronos2_quantile_levels = chronos2_quantile_levels,
  chronos2_batch_size = chronos2_batch_size,
  chronos2_cross_learning = chronos2_cross_learning,
  chronos2_validate_inputs = chronos2_validate_inputs,
  chronos2_context_max = chronos2_context_max,
  chronos2_torch_threads = chronos2_torch_threads,
  chronos2_torch_interop_threads = chronos2_torch_interop_threads,
  chronos2_workers = chronos2_workers,
  
  fortin_dir = canada_targets_dir
)

ctrl$rolling$oos_end <- min(
  as.Date(ctrl$rolling$oos_end),
  max(panel_can$Date, na.rm = TRUE)
)

# ============================================================
# TASK LIST
# ============================================================

task_list <- purrr::imap(targets_all, function(ys, g) {
  expand.grid(
    group = g,
    yname = ys,
    h = ctrl$rolling$horizons,
    stringsAsFactors = FALSE
  )
}) |>
  dplyr::bind_rows() |>
  dplyr::arrange(.data$group, .data$yname, .data$h) |>
  dplyr::mutate(task_id = dplyr::row_number()) |>
  dplyr::select(.data$task_id, dplyr::everything())

cat("\nTask list:\n")
print(task_list)

utils::write.csv(
  task_list,
  file.path(canada_targets_dir, "canada_targets_task_list.csv"),
  row.names = FALSE
)

# ============================================================
# PHASE OUTPUT FOLDER
# ============================================================

phase_dir <- file.path(canada_targets_dir, "phase_results")
dir.create(phase_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# RUN NON-CHRONOS PHASE
# ============================================================

cat("\nStarting Canada non-Chronos phase...\n")

nonchronos_results <- run_task_grid_nonchronos(
  panel_can = panel_can,
  task_list = task_list,
  ctrl = ctrl,
  models_to_run = non_chronos_model_names
)

cat("\nCanada non-Chronos phase finished.\n")

nonchronos_rds <- file.path(phase_dir, "can_nonchronos_results.rds")

saveRDS(
  list(
    nonchronos_results = nonchronos_results,
    task_list = task_list,
    ctrl = ctrl,
    target_order = target_order,
    targets_all = targets_all,
    model_order_internal = model_order_internal,
    model_display_names = model_display_names,
    benchmark_model = benchmark_model
  ),
  nonchronos_rds
)

cat("\nSaved non-Chronos results to:\n")
cat(nonchronos_rds, "\n")

gc()

# ============================================================
# RUN CHRONOS PHASE, IF ENABLED
# ============================================================

merged_rds <- file.path(phase_dir, "can_merged_results.rds")

if (isTRUE(chronos2_enabled)) {
  if (!file.exists(chronos_python)) {
    stop("Chronos Python was not found at:\n", chronos_python)
  }
  
  panel_csv   <- file.path(phase_dir, "can_panel_for_chronos.csv")
  request_csv <- file.path(phase_dir, "can_chronos_request.csv")
  config_json <- file.path(phase_dir, "can_chronos_config.json")
  py_file     <- file.path(phase_dir, "run_can_chronos_external.py")
  out_csv     <- file.path(phase_dir, "can_chronos_predictions.csv")
  
  chronos_request <- purrr::map_dfr(seq_along(nonchronos_results), function(i) {
    d <- nonchronos_results[[i]]
    
    data.frame(
      task_id = i,
      group = task_list$group[i],
      yname = task_list$yname[i],
      h = task_list$h[i],
      row_i = seq_along(d$dates),
      Date = as.Date(d$dates),
      y_true = as.numeric(d$y_true),
      stringsAsFactors = FALSE
    )
  })
  
  utils::write.csv(panel_can, panel_csv, row.names = FALSE)
  utils::write.csv(chronos_request, request_csv, row.names = FALSE)
  
  chronos_cfg <- list(
    target_order = as.character(target_order),
    oos_start = as.character(ctrl$rolling$oos_start),
    oos_end = as.character(max(panel_can$Date, na.rm = TRUE)),
    training_obs = as.integer(ctrl$rolling$træningsår),
    enabled = isTRUE(ctrl$model$chronos2$enabled),
    uni_enabled = isTRUE(ctrl$model$chronos2$uni_enabled),
    multi_enabled = isTRUE(ctrl$model$chronos2$multi_enabled),
    covariate_enabled = isTRUE(ctrl$model$chronos2$covariate_enabled),
    model_id = ctrl$model$chronos2$model_id,
    device = ctrl$model$chronos2$device,
    batch_size = as.integer(ctrl$model$chronos2$batch_size),
    cross_learning = isTRUE(ctrl$model$chronos2$cross_learning),
    validate_inputs = isTRUE(ctrl$model$chronos2$validate_inputs),
    context_max = as.integer(ctrl$model$chronos2$context_max),
    torch_threads = as.integer(ctrl$model$chronos2$torch_threads),
    torch_interop_threads = as.integer(ctrl$model$chronos2$torch_interop_threads)
  )
  
  writeLines(
    jsonlite::toJSON(chronos_cfg, auto_unbox = TRUE, pretty = TRUE),
    con = config_json
  )
  
  py_code <- r"---(
import argparse
import json
import traceback

import numpy as np
import pandas as pd
import torch
from chronos import Chronos2Pipeline


def mark(msg):
    print(msg, flush=True)


def add_month_covariates(df):
    ts = pd.to_datetime(df["timestamp"])
    mm = ts.dt.month
    df["month_sin"] = np.sin(2 * np.pi * mm / 12)
    df["month_cos"] = np.cos(2 * np.pi * mm / 12)
    return df


def safe_float(x):
    try:
        x = float(x)
        if np.isfinite(x):
            return x
        return np.nan
    except Exception:
        return np.nan


def extract_point_forecast(pred, target_name=None, h=1):
    if pred is None or len(pred) == 0:
        return np.nan

    d = pred.copy()

    if "timestamp" in d.columns:
        d["timestamp"] = pd.to_datetime(d["timestamp"], errors="coerce")
        d = d.dropna(subset=["timestamp"])
        d = d.sort_values("timestamp")

    if target_name is not None and "target_name" in d.columns:
        dd = d[d["target_name"].astype(str) == str(target_name)].copy()
        if len(dd) > 0:
            d = dd

    if len(d) == 0:
        return np.nan

    pick_col = None

    if "predictions" in d.columns:
        pick_col = "predictions"
    elif "mean" in d.columns:
        pick_col = "mean"
    elif "0.5" in d.columns:
        pick_col = "0.5"
    else:
        qcols = [c for c in d.columns if str(c).startswith("0.")]
        if len(qcols) > 0:
            pick_col = qcols[0]

    if pick_col is None:
        return np.nan

    row = int(h) - 1
    if row < 0 or row >= len(d):
        return np.nan

    return safe_float(d.iloc[row][pick_col])


def predict_df_safe(pipe, context_df, prediction_length, target, future_df, cfg):
    kwargs = dict(
        prediction_length=int(prediction_length),
        id_column="id",
        timestamp_column="timestamp",
        target=target,
        batch_size=int(cfg.get("batch_size", 8)),
        cross_learning=bool(cfg.get("cross_learning", False)),
        validate_inputs=bool(cfg.get("validate_inputs", False)),
    )

    if future_df is None:
        try:
            return pipe.predict_df(context_df, **kwargs)
        except TypeError:
            kwargs.pop("validate_inputs", None)
            return pipe.predict_df(context_df, **kwargs)
    else:
        try:
            return pipe.predict_df(context_df, future_df=future_df, **kwargs)
        except TypeError:
            kwargs.pop("validate_inputs", None)
            return pipe.predict_df(context_df, future_df=future_df, **kwargs)


def make_uni_context(hist, yname, context_max):
    d = hist[["Date", yname]].copy()
    d = d.replace([np.inf, -np.inf], np.nan).dropna()

    if len(d) < 24:
        return None

    out = pd.DataFrame({
        "id": yname,
        "timestamp": pd.to_datetime(d["Date"]),
        "target": pd.to_numeric(d[yname], errors="coerce")
    }).dropna()

    if context_max is not None and len(out) > context_max:
        out = out.tail(context_max)

    return out


def make_multi_context(hist, yname, target_names, context_max):
    names = [x for x in target_names if x in hist.columns]

    if yname not in names:
        names = [yname] + names

    names = list(dict.fromkeys(names))

    if len(names) < 2:
        return None, names

    d = hist[["Date"] + names].copy()
    d = d.replace([np.inf, -np.inf], np.nan).dropna()

    if len(d) < 24:
        return None, names

    out = d.copy()
    out["id"] = "multi_" + yname
    out["timestamp"] = pd.to_datetime(out["Date"])
    out = out[["id", "timestamp"] + names]

    if context_max is not None and len(out) > context_max:
        out = out.tail(context_max)

    return out, names


def make_cov_context(hist, yname, context_max):
    d = hist[["Date", yname]].copy()
    d = d.replace([np.inf, -np.inf], np.nan).dropna()

    if len(d) < 24:
        return None

    out = pd.DataFrame({
        "id": yname,
        "timestamp": pd.to_datetime(d["Date"]),
        "target": pd.to_numeric(d[yname], errors="coerce")
    }).dropna()

    out = add_month_covariates(out)

    if context_max is not None and len(out) > context_max:
        out = out.tail(context_max)

    return out


def make_future_cov(last_date, h, yname):
    last_date = pd.to_datetime(last_date)

    future_dates = pd.date_range(
        start=last_date + pd.DateOffset(months=1),
        periods=int(h),
        freq="MS"
    )

    out = pd.DataFrame({
        "id": yname,
        "timestamp": future_dates
    })

    out = add_month_covariates(out)
    return out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--panel", required=True)
    parser.add_argument("--request", required=True)
    parser.add_argument("--config", required=True)
    parser.add_argument("--out", required=True)
    args = parser.parse_args()

    with open(args.config, "r", encoding="utf-8") as f:
        cfg = json.load(f)

    torch.set_num_threads(int(cfg.get("torch_threads", 8)))

    try:
        torch.set_num_interop_threads(int(cfg.get("torch_interop_threads", 1)))
    except Exception:
        pass

    panel = pd.read_csv(args.panel)
    request = pd.read_csv(args.request)

    panel["Date"] = pd.to_datetime(panel["Date"])
    request["Date"] = pd.to_datetime(request["Date"])

    for c in panel.columns:
        if c != "Date":
            panel[c] = pd.to_numeric(panel[c], errors="coerce")

    target_order = cfg.get("target_order", [])
    oos_start = pd.to_datetime(cfg.get("oos_start"))
    oos_end = pd.to_datetime(cfg.get("oos_end"))
    training_obs = int(cfg.get("training_obs", 120))
    context_max = int(cfg.get("context_max", 256))

    model_id = cfg.get("model_id", "autogluon/chronos-2-small")
    device = cfg.get("device", "cpu")

    mark("Loading Chronos pipeline: " + model_id)
    pipe = Chronos2Pipeline.from_pretrained(model_id, device_map=device)
    mark("Chronos pipeline loaded.")

    results = []
    task_ids = sorted(request["task_id"].unique())

    for task_id in task_ids:
        req_task = request[request["task_id"] == task_id].copy()
        req_task = req_task.sort_values("row_i")

        yname = str(req_task["yname"].iloc[0])
        h = int(req_task["h"].iloc[0])

        mark(f"Task {task_id} | {yname} | h={h} | rows={len(req_task)}")

        task_dates = pd.to_datetime(req_task["Date"])

        for _, row in req_task.iterrows():
            t0 = pd.to_datetime(row["Date"])
            row_i = int(row["row_i"])

            out_row = {
                "task_id": int(task_id),
                "row_i": row_i,
                "Date": t0.strftime("%Y-%m-%d"),
                "yname": yname,
                "h": h,
                "Chronos-2-uni": np.nan,
                "Chronos-2-multi": np.nan,
                "Chronos-2-covariate-informed": np.nan,
            }

            if t0 < oos_start or t0 > oos_end:
                results.append(out_row)
                continue

            cutoff = t0 - pd.DateOffset(months=int(h))
            train_count = int((task_dates <= cutoff).sum())

            if train_count < training_obs:
                results.append(out_row)
                continue

            hist = panel[panel["Date"] <= t0].copy()

            try:
                if bool(cfg.get("enabled", True)) and bool(cfg.get("uni_enabled", True)):
                    context_uni = make_uni_context(hist, yname, context_max)

                    if context_uni is not None:
                        pred_uni = predict_df_safe(pipe, context_uni, h, "target", None, cfg)
                        out_row["Chronos-2-uni"] = extract_point_forecast(pred_uni, "target", h)

                if bool(cfg.get("enabled", True)) and bool(cfg.get("multi_enabled", True)):
                    context_multi, names_multi = make_multi_context(hist, yname, target_order, context_max)

                    if context_multi is not None and len(names_multi) >= 2:
                        pred_multi = predict_df_safe(pipe, context_multi, h, names_multi, None, cfg)
                        out_row["Chronos-2-multi"] = extract_point_forecast(pred_multi, yname, h)

                if bool(cfg.get("enabled", True)) and bool(cfg.get("covariate_enabled", True)):
                    context_cov = make_cov_context(hist, yname, context_max)

                    if context_cov is not None:
                        future_cov = make_future_cov(hist["Date"].max(), h, yname)
                        pred_cov = predict_df_safe(pipe, context_cov, h, "target", future_cov, cfg)
                        out_row["Chronos-2-covariate-informed"] = extract_point_forecast(pred_cov, "target", h)

            except Exception:
                mark("ERROR during prediction:")
                mark(traceback.format_exc())

            results.append(out_row)

        pd.DataFrame(results).to_csv(args.out, index=False)

    pd.DataFrame(results).to_csv(args.out, index=False)
    mark("Saved Chronos predictions to: " + args.out)


if __name__ == "__main__":
    main()
)---"
  
  writeLines(py_code, con = py_file)
  
  if (file.exists(out_csv)) {
    file.remove(out_csv)
  }
  
  cat("\nRunning external Chronos Python...\n")
  cat("Python:", chronos_python, "\n")
  cat("Script:", py_file, "\n")
  cat("Output:", out_csv, "\n\n")
  
  status <- system2(
    command = chronos_python,
    args = c(
      shQuote(py_file),
      "--panel", shQuote(panel_csv),
      "--request", shQuote(request_csv),
      "--config", shQuote(config_json),
      "--out", shQuote(out_csv)
    ),
    stdout = "",
    stderr = ""
  )
  
  cat("\nPython exit status:", status, "\n")
  
  if (status != 0 || !file.exists(out_csv)) {
    stop("External Chronos run failed.")
  }
  
  chronos_pred_long <- read.csv(
    out_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  chronos_pred_long$task_id <- as.integer(chronos_pred_long$task_id)
  chronos_pred_long$row_i <- as.integer(chronos_pred_long$row_i)
  
  chronos_model_cols <- c(
    "Chronos-2-uni",
    "Chronos-2-multi",
    "Chronos-2-covariate-informed"
  )
  
  missing_cols <- setdiff(chronos_model_cols, names(chronos_pred_long))
  
  if (length(missing_cols) > 0) {
    stop("Missing Chronos columns: ", paste(missing_cols, collapse = ", "))
  }
  
  chronos_results <- vector("list", length(nonchronos_results))
  
  for (i in seq_along(nonchronos_results)) {
    base <- nonchronos_results[[i]]
    
    pred_tbl <- as.data.frame(
      matrix(NA_real_, nrow = length(base$dates), ncol = length(chronos_model_cols)),
      stringsAsFactors = FALSE
    )
    
    names(pred_tbl) <- chronos_model_cols
    
    rows_i <- chronos_pred_long[chronos_pred_long$task_id == i, , drop = FALSE]
    
    if (nrow(rows_i) > 0) {
      for (m in chronos_model_cols) {
        pred_tbl[rows_i$row_i, m] <- suppressWarnings(as.numeric(rows_i[[m]]))
      }
    }
    
    chronos_results[[i]] <- list(
      h = base$h,
      dates = base$dates,
      y_true = base$y_true,
      pred_tbl = pred_tbl,
      meta = list(
        fallback_tunes = empty_fallback_counter(),
        fallback_forecasts = empty_fallback_counter(),
        tcsr_benchmark_fallbacks = 0L,
        tcsr_total_forecasts = 0L
      )
    )
  }
  
} else {
  cat("\nChronos disabled. Merging non-Chronos results only.\n")
  chronos_results <- vector("list", length(nonchronos_results))
}

# ============================================================
# MERGE RESULTS
# ============================================================

all_results <- setNames(vector("list", length(target_order)), target_order)

for (tg in target_order) {
  all_results[[tg]] <- list(
    meta = list(
      fallback_tunes = empty_fallback_counter(),
      fallback_forecasts = empty_fallback_counter(),
      tcsr_benchmark_fallbacks = 0L,
      tcsr_total_forecasts = 0L
    )
  )
}

for (i in seq_len(nrow(task_list))) {
  tg <- task_list$yname[i]
  h <- task_list$h[i]
  
  res_merged <- merge_task_details(
    base_detail = nonchronos_results[[i]],
    add_detail = if (isTRUE(chronos2_enabled)) chronos_results[[i]] else NULL,
    final_models = model_order_internal
  )
  
  all_results[[tg]][[paste0("h", h)]] <- res_merged[c(
    "h", "rmspe_bmk", "ratio", "pvals", "dates"
  )]
  
  all_results[[tg]]$meta$fallback_tunes <-
    all_results[[tg]]$meta$fallback_tunes + res_merged$meta$fallback_tunes
  
  all_results[[tg]]$meta$fallback_forecasts <-
    all_results[[tg]]$meta$fallback_forecasts + res_merged$meta$fallback_forecasts
  
  all_results[[tg]]$meta$tcsr_benchmark_fallbacks <-
    all_results[[tg]]$meta$tcsr_benchmark_fallbacks + res_merged$meta$tcsr_benchmark_fallbacks
  
  all_results[[tg]]$meta$tcsr_total_forecasts <-
    all_results[[tg]]$meta$tcsr_total_forecasts + res_merged$meta$tcsr_total_forecasts
}

all_results <- all_results[target_order]
names(all_results) <- target_order

fallback_summary <- summarise_fallback_counts(all_results)

farmselect_fallback_diag <- fallback_summary$as_table |>
  dplyr::filter(.data$Model == "FarmSelect")

saveRDS(
  list(
    all_results = all_results,
    fallback_summary = fallback_summary,
    nonchronos_results = nonchronos_results,
    chronos_results = if (exists("chronos_results")) chronos_results else NULL,
    task_list = task_list,
    ctrl = ctrl,
    farmselect_fallback_diag = farmselect_fallback_diag
  ),
  merged_rds
)

cat("\nSaved merged results to:\n")
cat(merged_rds, "\n")

utils::write.csv(
  fallback_summary$as_table,
  file = file.path(canada_targets_dir, "fallback_summary.csv"),
  row.names = FALSE
)

utils::write.csv(
  fallback_summary$tcsr_as_table,
  file = file.path(canada_targets_dir, "tcsr_fallback_summary.csv"),
  row.names = FALSE
)

# ============================================================
# SAVE LATEX TABLES
# ============================================================

save_fortin_table_grouped_tex(
  all_results = all_results,
  targets = real_activity_targets,
  target_labels = c("Industrial Production", "Employment", "Unemployment"),
  file = file.path(canada_targets_dir, "tab3.tex"),
  caption = "Forecasting real activity",
  label = "tab:real_activity"
)

save_fortin_table_grouped_tex(
  all_results = all_results,
  targets = inflation_targets,
  target_labels = c("CPI All", "CPI Minus Food"),
  file = file.path(canada_targets_dir, "tab4.tex"),
  caption = "Forecasting inflation",
  label = "tab:inflation"
)

save_fortin_table_grouped_tex(
  all_results = all_results,
  targets = credit_targets,
  target_labels = c("Credit T", "Business Credit", "Housing Credit"),
  file = file.path(canada_targets_dir, "tab5.tex"),
  caption = "Forecasting credit",
  label = "tab:credit"
)

save_fortin_table_grouped_tex(
  all_results = all_results,
  targets = housing_targets,
  target_labels = c("Housing Starts", "Building Permits"),
  file = file.path(canada_targets_dir, "tab6.tex"),
  caption = "Forecasting housing",
  label = "tab:housing"
)

cat("\nCanada continuous-target script finished.\n")
cat("Outputs saved to:\n")
cat(canada_targets_dir, "\n")