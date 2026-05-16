# ============================================================
# EU HELPERS
# Data prep, model helpers, Chronos wrapper, tables, and plots
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(forecast)
  library(glmnet)
  library(ranger)
  library(future)
  library(furrr)
  library(progressr)
  library(jsonlite)
  library(OCMT)
  library(FarmSelect)
  library(MultipleTestingBoosting)
})

options(progressr.enable = TRUE)
progressr::handlers("txtprogressbar")

# ============================================================
# GENERAL HELPERS
# ============================================================

eu_sanitize_filename <- function(x) {
  gsub("[^A-Za-z0-9_\\-]+", "_", x)
}

eu_latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&~_^{}])", "\\\\\\1", x, perl = TRUE)
  x
}

eu_quarter_to_date <- function(q) {
  q <- as.character(q)
  yr  <- as.integer(substr(q, 1, 4))
  qtr <- as.integer(sub(".*Q", "", q))
  mm  <- c(1, 4, 7, 10)[qtr]
  as.Date(sprintf("%04d-%02d-01", yr, mm))
}

eu_normalize_pval_grid <- function(pvals) {
  pvals <- as.numeric(pvals)
  labs <- names(pvals)
  
  if (is.null(labs) || any(!nzchar(labs))) {
    labs <- as.character(round(100 * pvals))
  }
  
  stats::setNames(pvals, labs)
}

eu_p_to_stars <- function(p) {
  if (length(p) == 0 || is.null(p)) return("")
  p <- suppressWarnings(as.numeric(p)[1])
  if (!is.finite(p)) return("")
  if (p < 0.10) "*" else ""
}

eu_quarters_between <- function(a, b) {
  qa <- 4L * lubridate::year(a) + ((lubridate::month(a) - 1L) %/% 3L)
  qb <- 4L * lubridate::year(b) + ((lubridate::month(b) - 1L) %/% 3L)
  qb - qa
}

eu_transform_series <- function(x, tcode, lag_season = 4L) {
  x <- suppressWarnings(as.numeric(x))
  tcode <- suppressWarnings(as.integer(tcode))
  lag_season <- as.integer(lag_season)
  
  if (tcode == 1L) {
    return(x)
  }
  
  if (length(x) <= lag_season) {
    return(rep(NA_real_, length(x)))
  }
  
  if (tcode == 2L) {
    return(c(
      rep(NA_real_, lag_season),
      x[(lag_season + 1L):length(x)] - x[1L:(length(x) - lag_season)]
    ))
  }
  
  if (tcode == 3L) {
    return(c(
      rep(NA_real_, lag_season),
      100 * (x[(lag_season + 1L):length(x)] / x[1L:(length(x) - lag_season)] - 1)
    ))
  }
  
  stop("Unknown T-code: ", tcode)
}

# ============================================================
# EU DATA PREPARATION
# ============================================================

eu_prepare_data <- function(excel_path,
                            sheet = 2,
                            lag_season = 4L,
                            remove_vars_with_na_pre1980 = TRUE,
                            pre1980_cutoff_date = as.Date("1980-01-01"),
                            dataset_label = "EU_data_dropvars_pre1980",
                            data_prep_dir = NULL) {
  
  eu_data <- readxl::read_excel(excel_path, sheet = sheet)
  
  if (!("Vars" %in% names(eu_data))) {
    stop("The EU Excel sheet must contain a column named 'Vars'.")
  }
  
  if (!("Tcode" %in% eu_data$Vars)) {
    stop("The EU Excel sheet must contain a row where Vars == 'Tcode'.")
  }
  
  tcodes <- as.numeric(unlist(eu_data[eu_data$Vars == "Tcode", -1]))
  
  eu_raw <- eu_data[!(eu_data$Vars %in% c("Tcode", "Start")), ]
  dates <- eu_raw$Vars
  
  X <- as.data.frame(lapply(eu_raw[, -1], function(z) suppressWarnings(as.numeric(z))))
  colnames(X) <- colnames(eu_raw)[-1]
  
  if (length(tcodes) != ncol(X)) {
    stop("Number of T-codes does not match number of series.")
  }
  
  names(tcodes) <- colnames(X)
  
  eu_raw_panel <- data.frame(
    Quarter = dates,
    X,
    check.names = FALSE
  ) |>
    dplyr::mutate(Date = eu_quarter_to_date(.data$Quarter)) |>
    dplyr::arrange(.data$Date) |>
    dplyr::select(.data$Date, dplyr::everything(), -dplyr::all_of("Quarter"))
  
  X_transformed <- as.data.frame(
    Map(
      f = function(x, tc) eu_transform_series(x, tc, lag_season = lag_season),
      X,
      tcodes
    ),
    check.names = FALSE
  )
  
  colnames(X_transformed) <- colnames(X)
  
  eu_transformed_full <- data.frame(
    Quarter = dates,
    X_transformed,
    check.names = FALSE
  ) |>
    dplyr::mutate(Date = eu_quarter_to_date(.data$Quarter)) |>
    dplyr::arrange(.data$Date) |>
    dplyr::select(.data$Date, dplyr::everything(), -dplyr::all_of("Quarter"))
  
  raw_panel <- eu_raw_panel |>
    dplyr::arrange(.data$Date)
  
  if (isTRUE(remove_vars_with_na_pre1980)) {
    vars_with_na_pre1980 <- raw_panel |>
      dplyr::filter(.data$Date < pre1980_cutoff_date) |>
      dplyr::select(-dplyr::all_of("Date")) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ any(is.na(.)))) |>
      unlist()
    
    keep_vars <- names(vars_with_na_pre1980)[!vars_with_na_pre1980]
  } else {
    keep_vars <- setdiff(names(eu_transformed_full), "Date")
  }
  
  dataset_label <- eu_sanitize_filename(dataset_label)
  dataset_label_tex <- eu_latex_escape(dataset_label)
  
  panel_can <- eu_transformed_full |>
    dplyr::select(.data$Date, dplyr::all_of(keep_vars)) |>
    dplyr::arrange(.data$Date)
  
  complete_rows <- which(complete.cases(panel_can |> dplyr::select(-dplyr::all_of("Date"))))
  
  if (length(complete_rows) == 0) {
    stop("No complete rows remain after EU filtering.")
  }
  
  first_complete_row <- min(complete_rows)
  
  panel_can <- panel_can |>
    dplyr::slice(first_complete_row:dplyr::n()) |>
    dplyr::arrange(.data$Date)
  
  tcode_tbl <- tibble::tibble(
    variable = names(tcodes),
    tcode = as.integer(tcodes),
    kept = names(tcodes) %in% keep_vars
  )
  
  info <- list(
    eu_data = eu_data,
    eu_raw_panel = eu_raw_panel,
    eu_transformed_full = eu_transformed_full,
    panel_can = panel_can,
    tcodes = tcodes,
    tcode_tbl = tcode_tbl,
    keep_vars = keep_vars,
    dataset_label = dataset_label,
    dataset_label_tex = dataset_label_tex,
    first_date = min(panel_can$Date, na.rm = TRUE),
    last_date = max(panel_can$Date, na.rm = TRUE),
    n_rows = nrow(panel_can),
    n_variables = ncol(panel_can) - 1L
  )
  
  cat("\nEU data prepared\n")
  cat("Dataset label:           ", dataset_label, "\n")
  cat("First complete date:     ", as.character(info$first_date), "\n")
  cat("Last date:               ", as.character(info$last_date), "\n")
  cat("Rows:                    ", info$n_rows, "\n")
  cat("Variables:               ", info$n_variables, "\n")
  
  if (!is.null(data_prep_dir)) {
    dir.create(data_prep_dir, recursive = TRUE, showWarnings = FALSE)
    
    saveRDS(info, file.path(data_prep_dir, "eu_data_prep_info.rds"))
    saveRDS(panel_can, file.path(data_prep_dir, "eu_panel_transformed_clean.rds"))
    saveRDS(eu_raw_panel, file.path(data_prep_dir, "eu_raw_panel.rds"))
    
    utils::write.csv(
      tcode_tbl,
      file.path(data_prep_dir, "eu_tcodes.csv"),
      row.names = FALSE
    )
    
    utils::write.csv(
      panel_can,
      file.path(data_prep_dir, "eu_panel_transformed_clean.csv"),
      row.names = FALSE
    )
  }
  
  info
}

eu_get_targets <- function(panel_can,
                           targets_all,
                           target_order,
                           target_labels_all) {
  
  targets_all_use <- lapply(targets_all, function(x) intersect(x, names(panel_can)))
  target_order_use <- intersect(target_order, names(panel_can))
  
  if (length(target_order_use) == 0) {
    stop("No EU target variables found in the prepared panel.")
  }
  
  target_labels <- target_labels_all[target_order_use]
  
  list(
    targets_all = targets_all_use,
    target_order = target_order_use,
    target_labels = target_labels
  )
}

# ============================================================
# MODEL SPECIFICATION
# ============================================================

eu_make_model_spec <- function(ocmt_pvals = c("5" = 0.05),
                               bmt_pvals = c("5" = 0.05)) {
  
  benchmark_model <- "AR,BIC"
  
  ocmt_pval_grid <- eu_normalize_pval_grid(ocmt_pvals)
  bmt_pval_grid  <- eu_normalize_pval_grid(bmt_pvals)
  
  ocmt_model_names <- paste0("OCMT_", names(ocmt_pval_grid))
  bmt_model_names  <- paste0("BMT_", names(bmt_pval_grid))
  
  model_order_internal <- c(
    "AR,BIC",
    "ARDI,BIC",
    "Ridge-X",
    "Lasso-X",
    "Elastic-Net-X",
    "Adaptive-Lasso-X",
    ocmt_model_names,
    "FarmSelect",
    bmt_model_names,
    "RF-X",
    "ARDI,Ridge",
    "ARDI,Lasso",
    "ARDI,Elastic-Net",
    "ARDI,Adaptive-Lasso",
    "RFARDI",
    "T-CSR",
    "Chronos-2-uni",
    "Chronos-2-multi",
    "Chronos-2-covariate-informed",
    "AVG"
  )
  
  model_display_names <- c(
    "AR,BIC"                        = "AR,BIC",
    "ARDI,BIC"                      = "ARDI,BIC",
    "Ridge-X"                       = "Ridge-X",
    "Lasso-X"                       = "Lasso-X",
    "Elastic-Net-X"                 = "Elastic-Net-X",
    "Adaptive-Lasso-X"              = "Adaptive-Lasso-X",
    stats::setNames("OCMT", ocmt_model_names),
    "FarmSelect"                    = "FarmSelect",
    stats::setNames("BMT", bmt_model_names),
    "RF-X"                          = "RF-X",
    "ARDI,Ridge"                    = "ARDI,Ridge",
    "ARDI,Lasso"                    = "ARDI,Lasso",
    "ARDI,Elastic-Net"              = "ARDI,Elastic-Net",
    "ARDI,Adaptive-Lasso"           = "ARDI,Adaptive-Lasso",
    "RFARDI"                        = "RFARDI",
    "T-CSR"                         = "T-CSR",
    "Chronos-2-uni"                 = "Chronos-2-uni",
    "Chronos-2-multi"               = "Chronos-2-multi",
    "Chronos-2-covariate-informed"  = "Chronos-2-covariate-informed",
    "AVG"                           = "AVG"
  )
  
  non_chronos_model_names <- setdiff(
    model_order_internal,
    c("Chronos-2-uni", "Chronos-2-multi", "Chronos-2-covariate-informed", "AVG")
  )
  
  chronos_only_model_names <- c(
    "Chronos-2-uni",
    "Chronos-2-multi",
    "Chronos-2-covariate-informed"
  )
  
  combo_candidate_models <- setdiff(model_order_internal, "AVG")
  
  fallback_model_names <- c(
    "Ridge-X",
    "Lasso-X",
    "Elastic-Net-X",
    "Adaptive-Lasso-X",
    "FarmSelect",
    "ARDI,Ridge",
    "ARDI,Lasso",
    "ARDI,Elastic-Net",
    "ARDI,Adaptive-Lasso"
  )
  
  list(
    benchmark_model = benchmark_model,
    ocmt_pval_grid = ocmt_pval_grid,
    bmt_pval_grid = bmt_pval_grid,
    ocmt_model_names = ocmt_model_names,
    bmt_model_names = bmt_model_names,
    model_order_internal = model_order_internal,
    model_display_names = model_display_names,
    non_chronos_model_names = non_chronos_model_names,
    chronos_only_model_names = chronos_only_model_names,
    combo_candidate_models = combo_candidate_models,
    fallback_model_names = fallback_model_names
  )
}

eu_empty_fallback_counter <- function(spec) {
  stats::setNames(integer(length(spec$fallback_model_names)), spec$fallback_model_names)
}

eu_empty_tcsr_counter <- function() {
  c(
    tcsr_benchmark_fallbacks = 0L,
    tcsr_total_forecasts = 0L
  )
}

# ============================================================
# CONTROL OBJECT
# ============================================================

eu_make_fortin_controls <- function(
    n_workers = 15L,
    oos_start = as.Date("2005-01-01"),
    oos_end   = as.Date("2025-12-31"),
    horizons = c(1, 2, 3, 4),
    py = 4L,
    px = 4L,
    K_fixed = 6L,
    retune_every_quarters = 4L,
    training_obs = 10L * 4L,
    glmnet_nlambda = 240L,
    elastic_alpha_grid = c(0.02, 0.08, 0.15, 0.25, 0.35, 0.5, 0.65, 0.75, 0.85, 0.92, 0.98),
    elastic_fallback_alpha = 0.5,
    fallback_lambda = 1e-4,
    adalasso_eps = 1e-6,
    ocmt_pvals = c("5" = 0.05),
    ocmt_delta1 = 1,
    ocmt_delta2 = 1,
    ocmt_multi_stage = TRUE,
    ocmt_max_iter = 300L,
    ocmt_kappa_const = 0.5,
    bmt_pvals = c("5" = 0.05),
    bmt_delta1 = 1,
    bmt_delta2 = 1,
    bmt_maxit = 300L,
    farm_loss = "lasso",
    farm_robust = FALSE,
    farm_cv = TRUE,
    farm_tau = 2,
    farm_K_factors = NULL,
    farm_max_iter = 90000L,
    farm_nfolds = 10L,
    farm_eps = 1e-4,
    rf_num_trees = 1800L,
    rf_min_node_size = 10L,
    tcsr_tc = 1.645,
    tcsr_L = 10L,
    tcsr_M = 10000L,
    chronos2_enabled = TRUE,
    chronos2_uni_enabled = TRUE,
    chronos2_multi_enabled = TRUE,
    chronos2_covariate_enabled = TRUE,
    chronos2_multi_target_names = NULL,
    chronos2_envname = "chronos2-r",
    chronos2_device = "cpu",
    chronos2_model_id = "autogluon/chronos-2-small",
    chronos2_batch_size = 8L,
    chronos2_cross_learning = FALSE,
    chronos2_validate_inputs = FALSE,
    chronos2_context_max = 256L,
    chronos2_torch_threads = 8L,
    chronos2_torch_interop_threads = 1L,
    fortin_dir
) {
  
  avail <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
  avail <- avail[1]
  if (!is.finite(avail) || avail < 1L) avail <- 1L
  
  n_workers <- max(1L, min(as.integer(n_workers), avail))
  
  list(
    parallel = list(
      requested_workers = as.integer(n_workers)
    ),
    rolling = list(
      oos_start = as.Date(oos_start),
      oos_end = as.Date(oos_end),
      horizons = as.integer(horizons),
      py = as.integer(py),
      px = as.integer(px),
      K_fixed = as.integer(K_fixed),
      retune_every_quarters = as.integer(retune_every_quarters),
      training_obs = as.integer(training_obs)
    ),
    glmnet = list(
      nlambda = as.integer(glmnet_nlambda),
      elastic_alpha_grid = elastic_alpha_grid,
      elastic_fallback_alpha = elastic_fallback_alpha,
      fallback_lambda = fallback_lambda,
      adalasso_eps = adalasso_eps
    ),
    selection = list(
      ocmt = list(
        pvals = eu_normalize_pval_grid(ocmt_pvals),
        delta1 = ocmt_delta1,
        delta2 = ocmt_delta2,
        multi_stage = ocmt_multi_stage,
        max_iter = as.integer(ocmt_max_iter),
        kappa_const = ocmt_kappa_const
      ),
      bmt = list(
        pvals = eu_normalize_pval_grid(bmt_pvals),
        delta1 = bmt_delta1,
        delta2 = bmt_delta2,
        maxit = as.integer(bmt_maxit)
      ),
      farm = list(
        loss = farm_loss,
        robust = farm_robust,
        cv = farm_cv,
        tau = farm_tau,
        K.factors = farm_K_factors,
        max.iter = as.integer(farm_max_iter),
        nfolds = as.integer(farm_nfolds),
        eps = farm_eps
      )
    ),
    model = list(
      rf = list(
        num_trees = as.integer(rf_num_trees),
        min_node_size = as.integer(rf_min_node_size)
      ),
      tcsr = list(
        tc = tcsr_tc,
        L = as.integer(tcsr_L),
        M = as.integer(tcsr_M)
      ),
      chronos2 = list(
        enabled = isTRUE(chronos2_enabled),
        uni_enabled = isTRUE(chronos2_enabled) && isTRUE(chronos2_uni_enabled),
        multi_enabled = isTRUE(chronos2_enabled) && isTRUE(chronos2_multi_enabled),
        covariate_enabled = isTRUE(chronos2_enabled) && isTRUE(chronos2_covariate_enabled),
        multi_target_names = chronos2_multi_target_names,
        envname = chronos2_envname,
        device = chronos2_device,
        model_id = chronos2_model_id,
        batch_size = as.integer(chronos2_batch_size),
        cross_learning = isTRUE(chronos2_cross_learning),
        validate_inputs = isTRUE(chronos2_validate_inputs),
        context_max = as.integer(chronos2_context_max),
        torch_threads = as.integer(chronos2_torch_threads),
        torch_interop_threads = as.integer(chronos2_torch_interop_threads)
      )
    ),
    output = list(
      fortin_dir = fortin_dir
    )
  )
}

# ============================================================
# BASIC MODEL HELPERS
# ============================================================

eu_safe_scale_newx <- function(X_new, center, scale) {
  if (is.null(X_new)) return(matrix(numeric(0), 1, length(center)))
  if (!is.matrix(X_new)) X_new <- matrix(as.numeric(X_new), nrow = 1)
  if (ncol(X_new) != length(center)) stop("X_new has incompatible number of columns")
  if (length(center) == 0) return(matrix(numeric(0), nrow(X_new), 0))
  sweep(sweep(X_new, 2, center, FUN = "-"), 2, scale, FUN = "/")
}

eu_prepare_penalized_data <- function(X, y) {
  y <- as.numeric(y)
  
  if (is.null(X)) X <- matrix(numeric(0), length(y), 0)
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) != length(y)) stop("nrow(X) must match length(y)")
  
  ok_x <- if (ncol(X) == 0) rep(TRUE, nrow(X)) else apply(X, 1, function(r) all(is.finite(r)))
  ok <- is.finite(y) & ok_x
  
  y2 <- y[ok]
  X2 <- X[ok, , drop = FALSE]
  
  if (length(y2) == 0) {
    return(list(
      X = X2,
      Xs = X2,
      y = y2,
      yc = y2,
      center = numeric(ncol(X2)),
      scale = rep(1, ncol(X2)),
      y_mean = NA_real_,
      ok = ok
    ))
  }
  
  if (ncol(X2) == 0) {
    center <- numeric(0)
    scale  <- numeric(0)
    Xs <- matrix(numeric(0), nrow(X2), 0)
  } else {
    center <- colMeans(X2)
    scale  <- apply(X2, 2, sd)
    scale[!is.finite(scale) | scale == 0] <- 1
    Xs <- sweep(sweep(X2, 2, center, FUN = "-"), 2, scale, FUN = "/")
  }
  
  y_mean <- mean(y2)
  
  list(
    X = X2,
    Xs = Xs,
    y = y2,
    yc = y2 - y_mean,
    center = center,
    scale = scale,
    y_mean = y_mean,
    ok = ok
  )
}

eu_prepare_linear_data <- function(y_train, X_train, X_test = NULL) {
  y_train <- as.numeric(y_train)
  
  if (is.null(X_train)) X_train <- matrix(numeric(0), length(y_train), 0)
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (nrow(X_train) != length(y_train)) return(NULL)
  
  ok_x <- if (ncol(X_train) == 0) rep(TRUE, nrow(X_train)) else apply(X_train, 1, function(r) all(is.finite(r)))
  ok <- is.finite(y_train) & ok_x
  
  y2 <- y_train[ok]
  X2 <- X_train[ok, , drop = FALSE]
  
  X_test2 <- NULL
  
  if (!is.null(X_test)) {
    if (!is.matrix(X_test)) X_test <- matrix(as.numeric(X_test), nrow = 1)
    if (nrow(X_test) != 1) return(NULL)
    if (ncol(X_test) != ncol(X_train)) return(NULL)
    if (any(!is.finite(X_test))) return(NULL)
    X_test2 <- X_test
  }
  
  list(y = y2, X = X2, X_test = X_test2, ok = ok)
}

eu_safe_lm_predict <- function(y_train, X_train, X_test) {
  prep <- eu_prepare_linear_data(y_train, X_train, X_test = X_test)
  if (is.null(prep)) return(NA_real_)
  if (length(prep$y) <= (ncol(prep$X) + 1)) return(NA_real_)
  
  X1 <- cbind(1, prep$X)
  beta <- qr.coef(qr(X1), prep$y)
  beta[!is.finite(beta)] <- 0
  
  x_test1 <- c(1, as.numeric(prep$X_test))
  if (any(!is.finite(x_test1))) return(NA_real_)
  
  as.numeric(drop(x_test1 %*% beta))
}

eu_get_selected_idx <- function(selected, p) {
  idx <- integer(0)
  
  if (!is.null(selected)) {
    if (is.logical(selected)) {
      idx <- which(selected)
    } else {
      idx <- unique(as.integer(selected))
    }
    idx <- idx[is.finite(idx) & idx >= 1 & idx <= p]
  }
  
  idx
}

eu_build_post_selection_train_test <- function(Xcand_train,
                                               Xcand_test,
                                               Xcond_train = NULL,
                                               Xcond_test = NULL,
                                               selected = NULL) {
  
  if (!is.matrix(Xcand_train)) Xcand_train <- as.matrix(Xcand_train)
  if (!is.matrix(Xcand_test))  Xcand_test  <- as.matrix(Xcand_test)
  
  idx <- eu_get_selected_idx(selected, ncol(Xcand_train))
  
  Xsel_train <- if (length(idx) > 0) Xcand_train[, idx, drop = FALSE] else matrix(numeric(0), nrow(Xcand_train), 0)
  Xsel_test  <- if (length(idx) > 0) Xcand_test[, idx, drop = FALSE]  else matrix(numeric(0), nrow(Xcand_test), 0)
  
  if (is.null(Xcond_train)) Xcond_train <- matrix(numeric(0), nrow(Xcand_train), 0)
  if (is.null(Xcond_test))  Xcond_test  <- matrix(numeric(0), nrow(Xcand_test), 0)
  
  if (!is.matrix(Xcond_train)) Xcond_train <- as.matrix(Xcond_train)
  if (!is.matrix(Xcond_test))  Xcond_test  <- as.matrix(Xcond_test)
  
  list(
    X_train = cbind(Xcond_train, Xsel_train),
    X_test = cbind(Xcond_test, Xsel_test),
    idx = idx
  )
}

eu_predict_from_selection_ols <- function(y_train,
                                          Xcand_train,
                                          Xcand_test,
                                          Xcond_train = NULL,
                                          Xcond_test = NULL,
                                          selected = NULL) {
  
  mats <- eu_build_post_selection_train_test(
    Xcand_train = Xcand_train,
    Xcand_test = Xcand_test,
    Xcond_train = Xcond_train,
    Xcond_test = Xcond_test,
    selected = selected
  )
  
  eu_safe_lm_predict(y_train, mats$X_train, mats$X_test)
}

# ============================================================
# GLMNET BIC
# ============================================================

eu_ridge_df_path <- function(Xs, lambda) {
  n <- nrow(Xs)
  if (ncol(Xs) == 0) return(rep(1, length(lambda)))
  
  d <- svd(Xs, nu = 0, nv = 0)$d
  
  1 + vapply(
    lambda,
    function(lam) sum(ifelse(d > 0, d^2 / (d^2 + n * lam), 0)),
    numeric(1)
  )
}

eu_select_bic_glmnet <- function(Z,
                                 y,
                                 alpha,
                                 penalty.factor = NULL,
                                 nlambda = 240,
                                 lambda = NULL) {
  
  prep <- tryCatch(eu_prepare_penalized_data(Z, y), error = function(e) NULL)
  if (is.null(prep)) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  n <- length(prep$y)
  p <- ncol(prep$Xs)
  
  if (n == 0) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  if (p == 0) return(list(lambda = 0, bic = -Inf, df = 1))
  
  if (!is.null(penalty.factor) && length(penalty.factor) != p) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  glmnet_args <- list(
    x = prep$Xs,
    y = prep$yc,
    alpha = alpha,
    nlambda = nlambda,
    standardize = FALSE,
    intercept = FALSE
  )
  
  if (!is.null(lambda)) glmnet_args$lambda <- lambda
  if (!is.null(penalty.factor)) glmnet_args$penalty.factor <- penalty.factor
  
  fit <- tryCatch(do.call(glmnet::glmnet, glmnet_args), error = function(e) NULL)
  
  if (is.null(fit) || length(fit$lambda) == 0) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  pred_mat <- tryCatch(stats::predict(fit, newx = prep$Xs), error = function(e) NULL)
  if (is.null(pred_mat)) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  pred_mat <- as.matrix(pred_mat)
  
  rss <- colSums((matrix(prep$yc, nrow = n, ncol = ncol(pred_mat)) - pred_mat)^2)
  rss[!is.finite(rss) | rss <= 0] <- .Machine$double.xmin
  
  k <- if (alpha == 0) eu_ridge_df_path(prep$Xs, fit$lambda) else fit$df + 1
  
  bic <- n * log(rss / n) + k * log(n)
  
  if (all(!is.finite(bic))) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  j <- which.min(bic)
  
  list(
    lambda = fit$lambda[j],
    bic = bic[j],
    df = k[j]
  )
}

eu_tune_ridge_or_lasso <- function(Z,
                                   y,
                                   alpha,
                                   nlambda = 240,
                                   fallback_lambda = 1e-4) {
  
  out <- eu_select_bic_glmnet(
    Z = Z,
    y = y,
    alpha = alpha,
    nlambda = nlambda
  )
  
  lam <- out$lambda
  used_fallback <- FALSE
  
  if (!is.finite(lam) || lam < 0) {
    lam <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(
    alpha = alpha,
    lambda = lam,
    used_fallback = used_fallback
  )
}

eu_tune_elasticnet <- function(Z,
                               y,
                               alpha_grid,
                               nlambda = 240,
                               fallback_alpha = 0.5,
                               fallback_lambda = 1e-4) {
  
  best <- list(bic = Inf, alpha = NA_real_, lambda = NA_real_)
  
  for (a in alpha_grid) {
    cur <- eu_select_bic_glmnet(
      Z = Z,
      y = y,
      alpha = a,
      nlambda = nlambda
    )
    
    if (is.finite(cur$bic) && cur$bic < best$bic) {
      best$bic <- cur$bic
      best$alpha <- a
      best$lambda <- cur$lambda
    }
  }
  
  used_fallback <- FALSE
  
  if (!is.finite(best$alpha)) {
    best$alpha <- fallback_alpha
  }
  
  if (!is.finite(best$lambda) || best$lambda < 0) {
    best$lambda <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(
    alpha = best$alpha,
    lambda = best$lambda,
    used_fallback = used_fallback
  )
}

eu_tune_adaptive_lasso <- function(Z,
                                   y,
                                   nlambda = 240,
                                   eps = 1e-6,
                                   fallback_lambda = 1e-4) {
  
  prep <- tryCatch(eu_prepare_penalized_data(Z, y), error = function(e) NULL)
  
  if (is.null(prep) || length(prep$y) == 0 || ncol(prep$Xs) == 0) {
    p0 <- if (is.null(Z)) 0 else ncol(as.matrix(Z))
    return(list(
      lambda = fallback_lambda,
      weights = rep(1, p0),
      used_fallback = TRUE
    ))
  }
  
  ridge_bic <- eu_select_bic_glmnet(
    Z = Z,
    y = y,
    alpha = 0,
    nlambda = nlambda
  )
  
  lambda0 <- ridge_bic$lambda
  if (!is.finite(lambda0) || lambda0 < 0) lambda0 <- fallback_lambda
  
  fit0 <- tryCatch(
    glmnet::glmnet(
      prep$Xs,
      prep$yc,
      alpha = 0,
      lambda = lambda0,
      standardize = FALSE,
      intercept = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit0)) {
    w <- rep(1, ncol(prep$Xs))
  } else {
    b0 <- as.numeric(stats::coef(fit0))[-1]
    w <- 1 / (abs(b0) + eps)
    w[!is.finite(w)] <- 1 / eps
  }
  
  ada_bic <- eu_select_bic_glmnet(
    Z = Z,
    y = y,
    alpha = 1,
    penalty.factor = w,
    nlambda = nlambda
  )
  
  lam <- ada_bic$lambda
  used_fallback <- FALSE
  
  if (!is.finite(lam) || lam < 0) {
    lam <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(
    lambda = lam,
    weights = w,
    used_fallback = used_fallback
  )
}

eu_fit_predict_glmnet <- function(Z_train,
                                  y_train,
                                  Z_test,
                                  spec,
                                  fallback_lambda = 1e-4) {
  
  if (is.null(spec) || is.null(spec$type)) return(NA_real_)
  
  lambda_use <- spec$lambda
  
  if (is.null(lambda_use) || !is.finite(lambda_use) || lambda_use < 0) {
    lambda_use <- fallback_lambda
  }
  
  prep <- tryCatch(eu_prepare_penalized_data(Z_train, y_train), error = function(e) NULL)
  if (is.null(prep) || length(prep$y) == 0) return(NA_real_)
  
  if (is.null(Z_test)) Z_test <- matrix(numeric(0), 1, 0)
  if (!is.matrix(Z_test)) Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  
  if (nrow(Z_test) != 1 || any(!is.finite(Z_test)) || ncol(Z_test) != ncol(prep$X)) {
    return(NA_real_)
  }
  
  if (ncol(prep$Xs) == 0) return(prep$y_mean)
  
  Zs_test <- tryCatch(
    eu_safe_scale_newx(Z_test, prep$center, prep$scale),
    error = function(e) NULL
  )
  
  if (is.null(Zs_test)) return(NA_real_)
  
  fit <- tryCatch(
    switch(
      spec$type,
      ridge = glmnet::glmnet(
        prep$Xs,
        prep$yc,
        alpha = 0,
        lambda = lambda_use,
        standardize = FALSE,
        intercept = FALSE
      ),
      lasso = glmnet::glmnet(
        prep$Xs,
        prep$yc,
        alpha = 1,
        lambda = lambda_use,
        standardize = FALSE,
        intercept = FALSE
      ),
      elastic = glmnet::glmnet(
        prep$Xs,
        prep$yc,
        alpha = ifelse(is.finite(spec$alpha), spec$alpha, 0.5),
        lambda = lambda_use,
        standardize = FALSE,
        intercept = FALSE
      ),
      adalasso = glmnet::glmnet(
        prep$Xs,
        prep$yc,
        alpha = 1,
        lambda = lambda_use,
        standardize = FALSE,
        intercept = FALSE,
        penalty.factor = spec$weights
      ),
      stop("Unknown glmnet spec$type")
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NA_real_)
  
  pred_c <- tryCatch(
    as.numeric(stats::predict(fit, newx = Zs_test, s = lambda_use)),
    error = function(e) NA_real_
  )
  
  as.numeric(pred_c + prep$y_mean)
}

# ============================================================
# VARIABLE-SELECTION HELPERS
# ============================================================

eu_select_ocmt <- function(y_train,
                           Xcand_train,
                           Xcond_train,
                           pval = 0.05,
                           delta1 = 1,
                           delta2 = 1,
                           multi_stage = TRUE,
                           max_iter = 300,
                           kappa_const = 0.5) {
  
  if (!is.matrix(Xcand_train)) Xcand_train <- as.matrix(Xcand_train)
  if (!is.matrix(Xcond_train)) Xcond_train <- as.matrix(Xcond_train)
  
  p <- ncol(Xcand_train)
  
  out <- tryCatch(
    ocmt_sel(
      Y = y_train,
      X = Xcand_train,
      Xcond = Xcond_train,
      pval = pval,
      delta1 = delta1,
      delta2 = delta2,
      multi_stage = multi_stage,
      max_iter = max_iter,
      kappa_const = kappa_const,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(out) || is.null(out$ind)) return(rep(FALSE, p))
  
  as.logical(out$ind)
}

eu_select_bmt <- function(y_train,
                          Xcand_train,
                          Xcond_train,
                          pval = 0.05,
                          delta1 = 1,
                          delta2 = 1,
                          maxit = 300) {
  
  if (!is.matrix(Xcand_train)) Xcand_train <- as.matrix(Xcand_train)
  if (!is.matrix(Xcond_train)) Xcond_train <- as.matrix(Xcond_train)
  
  p <- ncol(Xcand_train)
  
  out <- tryCatch(
    boosting_glm(
      y = y_train,
      X = Xcand_train,
      Xcond = Xcond_train,
      pval = pval,
      delta1 = delta1,
      delta2 = delta2,
      link = "mean",
      maxit = maxit
    ),
    error = function(e) NULL
  )
  
  if (is.null(out)) return(rep(FALSE, p))
  
  as.logical(out)
}

eu_fit_farm_native <- function(y_train,
                               Z_train,
                               loss = "lasso",
                               robust = FALSE,
                               cv = TRUE,
                               tau = 2,
                               K.factors = NULL,
                               max.iter = 90000,
                               nfolds = 10,
                               eps = 1e-4) {
  
  if (!is.matrix(Z_train)) Z_train <- as.matrix(Z_train)
  
  ok_x <- if (ncol(Z_train) == 0) {
    rep(TRUE, nrow(Z_train))
  } else {
    apply(Z_train, 1, function(r) all(is.finite(r)))
  }
  
  ok_y <- is.finite(y_train)
  ok <- ok_x & ok_y
  
  y2 <- as.numeric(y_train[ok])
  Z2 <- Z_train[ok, , drop = FALSE]
  
  if (length(y2) == 0) {
    return(list(
      use_mean = TRUE,
      y_mean = NA_real_,
      fit = NULL,
      selected_idx = integer(0)
    ))
  }
  
  y_mean <- mean(y2)
  
  if (ncol(Z2) == 0 || min(nrow(Z2), ncol(Z2)) <= 4) {
    return(list(
      use_mean = TRUE,
      y_mean = y_mean,
      fit = NULL,
      selected_idx = integer(0)
    ))
  }
  
  fit <- tryCatch(
    farm.select(
      X = Z2,
      Y = y2,
      loss = loss,
      robust = robust,
      cv = cv,
      tau = tau,
      lin.reg = TRUE,
      K.factors = K.factors,
      max.iter = max.iter,
      nfolds = nfolds,
      eps = eps,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(list(
      use_mean = TRUE,
      y_mean = y_mean,
      fit = NULL,
      selected_idx = integer(0)
    ))
  }
  
  idx <- integer(0)
  
  if (!is.null(fit$beta.chosen)) {
    idx <- unique(as.integer(fit$beta.chosen))
    idx <- idx[is.finite(idx) & idx >= 1 & idx <= ncol(Z2)]
  }
  
  list(
    use_mean = FALSE,
    y_mean = y_mean,
    fit = fit,
    selected_idx = idx
  )
}

eu_predict_farm_native <- function(farm_obj, Z_test, return_info = FALSE) {
  out <- function(pred, used_fallback, reason) {
    if (isTRUE(return_info)) {
      return(list(
        pred = as.numeric(pred),
        used_fallback = isTRUE(used_fallback),
        reason = reason
      ))
    }
    
    as.numeric(pred)
  }
  
  if (is.null(farm_obj)) {
    return(out(NA_real_, TRUE, "missing_farm_object"))
  }
  
  if (!is.matrix(Z_test)) {
    Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  }
  
  if (nrow(Z_test) != 1) {
    return(out(NA_real_, TRUE, "bad_Z_test_rows"))
  }
  
  if (!is.finite(farm_obj$y_mean)) {
    return(out(NA_real_, TRUE, "bad_y_mean"))
  }
  
  if (isTRUE(farm_obj$use_mean) || is.null(farm_obj$fit)) {
    return(out(farm_obj$y_mean, TRUE, "farm_fit_fallback"))
  }
  
  pred <- tryCatch(
    as.numeric(predict(farm_obj$fit, newx = Z_test, type = "response")),
    error = function(e) NA_real_
  )
  
  if (length(pred) == 0 || !is.finite(pred[1])) {
    return(out(farm_obj$y_mean, TRUE, "prediction_failed"))
  }
  
  out(pred[1], FALSE, NA_character_)
}

# ============================================================
# TIME SERIES / PCA / RF / T-CSR HELPERS
# ============================================================

eu_as_numeric_df <- function(df, date_col = "Date") {
  out <- df
  out[[date_col]] <- as.Date(out[[date_col]])
  num_cols <- setdiff(names(out), date_col)
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))
  out
}

eu_make_yh_direct <- function(y, h) {
  y <- as.numeric(y)
  n <- length(y)
  out <- rep(NA_real_, n)
  
  if (h <= 0) stop("h must be >= 1")
  if (n <= h) return(out)
  
  for (t in 1:(n - h)) {
    out[t] <- y[t + h]
  }
  
  out
}

eu_add_lags <- function(df, var, p, prefix = NULL) {
  stopifnot(p >= 1)
  
  x <- df[[var]]
  base <- if (is.null(prefix)) var else prefix
  
  for (L in 0:(p - 1)) {
    df[[paste0(base, "_L", L)]] <- dplyr::lag(x, L)
  }
  
  df
}

eu_add_lags_many <- function(df, vars, p) {
  for (v in vars) {
    df <- eu_add_lags(df, v, p, prefix = v)
  }
  df
}

eu_bic_lm <- function(y, X) {
  n <- length(y)
  X1 <- cbind(1, X)
  fit <- lm.fit(X1, y)
  rss <- sum(fit$residuals^2)
  k <- ncol(X1)
  
  list(
    bic = n * log(rss / n) + k * log(n),
    coef = fit$coefficients
  )
}

eu_dm_pval <- function(e_bench, e_model, h) {
  e_bench <- as.numeric(e_bench)
  e_model <- as.numeric(e_model)
  
  ok <- is.finite(e_bench) & is.finite(e_model)
  
  e_bench <- e_bench[ok]
  e_model <- e_model[ok]
  
  n <- length(e_bench)
  
  if (n < max(h + 1, 10)) return(NA_real_)
  if (stats::var(e_model - e_bench) == 0) return(NA_real_)
  
  tryCatch(
    forecast::dm.test(
      e_model,
      e_bench,
      h = h,
      power = 2,
      alternative = "two.sided"
    )$p.value,
    error = function(e) NA_real_
  )
}

eu_compute_RMSE_info <- function(y_true, y_hat) {
  ok <- is.finite(y_true) & is.finite(y_hat)
  
  if (!any(ok)) {
    return(list(RMSE = NA_real_, ok = ok))
  }
  
  e <- y_true[ok] - y_hat[ok]
  
  list(
    RMSE = sqrt(mean(e^2)),
    ok = ok
  )
}

eu_compute_simple_avg_forecast <- function(current_preds, candidate_models) {
  vals <- as.numeric(current_preds[candidate_models])
  vals <- vals[is.finite(vals)]
  
  if (length(vals) == 0) return(NA_real_)
  
  mean(vals)
}

eu_pca_fit <- function(X, Kmax = 10) {
  if (is.null(X)) return(NULL)
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) < 2 || ncol(X) < 1) return(NULL)
  
  mu <- apply(X, 2, function(z) median(z[is.finite(z)], na.rm = TRUE))
  mu[!is.finite(mu)] <- 0
  
  for (j in seq_len(ncol(X))) {
    bad <- !is.finite(X[, j])
    if (any(bad)) X[bad, j] <- mu[j]
  }
  
  sdv <- apply(X, 2, sd)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  
  Xs <- scale(X, center = mu, scale = sdv)
  
  pc <- tryCatch(
    prcomp(Xs, center = FALSE, scale. = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(pc) || is.null(pc$x) || ncol(pc$x) == 0) return(NULL)
  
  Kuse <- min(Kmax, ncol(pc$x))
  
  list(
    loadings = pc$rotation[, 1:Kuse, drop = FALSE],
    scores = pc$x[, 1:Kuse, drop = FALSE],
    mu = mu,
    sd = sdv
  )
}

eu_pca_project <- function(X_new, pca_obj) {
  if (is.null(pca_obj)) return(NULL)
  if (!is.matrix(X_new)) X_new <- as.matrix(X_new)
  if (ncol(X_new) != length(pca_obj$mu)) return(NULL)
  
  for (j in seq_len(ncol(X_new))) {
    bad <- !is.finite(X_new[, j])
    if (any(bad)) X_new[bad, j] <- pca_obj$mu[j]
  }
  
  Xs_new <- sweep(sweep(X_new, 2, pca_obj$mu, FUN = "-"), 2, pca_obj$sd, FUN = "/")
  
  as.matrix(Xs_new %*% pca_obj$loadings)
}

eu_build_factor_history <- function(panel_df,
                                    y_name,
                                    x_names,
                                    cutoff,
                                    t0,
                                    Kmax = 6) {
  
  base_df <- panel_df |>
    dplyr::arrange(.data$Date) |>
    dplyr::filter(.data$Date <= t0) |>
    dplyr::select(.data$Date, dplyr::all_of(c(y_name, x_names)))
  
  if (nrow(base_df) == 0) return(NULL)
  
  names(base_df)[names(base_df) == y_name] <- "y"
  
  pca_train_df <- base_df |>
    dplyr::filter(.data$Date <= cutoff)
  
  if (nrow(pca_train_df) < 2 || length(x_names) == 0) return(NULL)
  
  X_pca_train <- as.matrix(pca_train_df[, x_names, drop = FALSE])
  X_full_hist <- as.matrix(base_df[, x_names, drop = FALSE])
  
  pca_obj <- eu_pca_fit(X_pca_train, Kmax = Kmax)
  if (is.null(pca_obj)) return(NULL)
  
  F_full <- eu_pca_project(X_full_hist, pca_obj)
  if (is.null(F_full) || ncol(F_full) == 0) return(NULL)
  
  colnames(F_full) <- paste0("F", seq_len(ncol(F_full)))
  
  dplyr::bind_cols(
    base_df |> dplyr::select(.data$Date, .data$y),
    as.data.frame(F_full)
  )
}

eu_fit_predict_AR_BIC <- function(train_df, test_row, py_max = 4) {
  bics <- rep(Inf, py_max)
  coefs <- vector("list", py_max)
  
  for (p in 1:py_max) {
    cols <- paste0("y_L", 0:(p - 1))
    d <- train_df[, c("y_h", cols), drop = FALSE]
    d <- d[complete.cases(d), , drop = FALSE]
    
    if (nrow(d) < (p + 25)) next
    
    tmp <- eu_bic_lm(d$y_h, as.matrix(d[, cols, drop = FALSE]))
    bics[p] <- tmp$bic
    coefs[[p]] <- tmp$coef
  }
  
  p_best <- which.min(bics)
  
  if (!length(p_best) || !is.finite(bics[p_best])) return(NA_real_)
  
  cols <- paste0("y_L", 0:(p_best - 1))
  x_test <- as.numeric(test_row[, cols, drop = FALSE])
  beta <- coefs[[p_best]]
  
  sum(c(1, x_test) * beta)
}

eu_fit_predict_ARDI_BIC <- function(factor_hist,
                                    train_dates,
                                    test_date,
                                    h,
                                    py_max = 4,
                                    pf_max = 4) {
  
  if (is.null(factor_hist)) return(NA_real_)
  
  factor_cols <- grep("^F\\d+$", names(factor_hist), value = TRUE)
  K_avail <- length(factor_cols)
  
  if (K_avail == 0) return(NA_real_)
  
  hf <- factor_hist |>
    dplyr::arrange(.data$Date)
  
  hf$y_h <- eu_make_yh_direct(hf$y, h = h)
  hf <- eu_add_lags(hf, "y", p = py_max, prefix = "y")
  
  for (fc in factor_cols) {
    for (L in 0:(pf_max - 1)) {
      hf[[paste0(fc, "_L", L)]] <- dplyr::lag(hf[[fc]], L)
    }
  }
  
  hf_train <- hf |>
    dplyr::filter(.data$Date %in% train_dates)
  
  best <- list(bic = Inf, beta = NULL, cols = NULL)
  
  for (py in 1:py_max) {
    ycols <- paste0("y_L", 0:(py - 1))
    
    for (pf in 1:pf_max) {
      for (K in 1:K_avail) {
        fcols <- unlist(lapply(seq_len(K), function(k) paste0("F", k, "_L", 0:(pf - 1))))
        cols <- c(ycols, fcols)
        
        d <- hf_train[, c("y_h", cols), drop = FALSE]
        d <- d[complete.cases(d), , drop = FALSE]
        
        if (nrow(d) < (length(cols) + 25)) next
        
        tmp <- eu_bic_lm(d$y_h, as.matrix(d[, cols, drop = FALSE]))
        
        if (is.finite(tmp$bic) && tmp$bic < best$bic) {
          best$bic <- tmp$bic
          best$beta <- tmp$coef
          best$cols <- cols
        }
      }
    }
  }
  
  if (!is.finite(best$bic)) return(NA_real_)
  
  hf_test <- hf |>
    dplyr::filter(.data$Date == test_date)
  
  if (nrow(hf_test) != 1) return(NA_real_)
  
  x_test <- as.numeric(hf_test[, best$cols, drop = FALSE])
  if (any(!is.finite(x_test))) return(NA_real_)
  
  as.numeric(sum(c(1, x_test) * best$beta))
}

eu_build_ardi_fixed_design <- function(factor_hist,
                                       train_dates,
                                       test_date,
                                       h,
                                       p_factor_lags = 4) {
  
  if (is.null(factor_hist)) return(NULL)
  
  factor_cols <- grep("^F\\d+$", names(factor_hist), value = TRUE)
  if (length(factor_cols) == 0) return(NULL)
  
  hf <- factor_hist |>
    dplyr::arrange(.data$Date)
  
  hf$y_h <- eu_make_yh_direct(hf$y, h = h)
  hf <- eu_add_lags(hf, "y", p = p_factor_lags, prefix = "y")
  
  for (fc in factor_cols) {
    for (L in 0:(p_factor_lags - 1)) {
      hf[[paste0(fc, "_L", L)]] <- dplyr::lag(hf[[fc]], L)
    }
  }
  
  fac_lag_cols <- c(
    paste0("y_L", 0:(p_factor_lags - 1)),
    unlist(lapply(factor_cols, function(fc) paste0(fc, "_L", 0:(p_factor_lags - 1))))
  )
  
  tr2 <- hf |>
    dplyr::filter(.data$Date %in% train_dates)
  
  te2 <- hf |>
    dplyr::filter(.data$Date == test_date)
  
  tr2 <- tr2[complete.cases(tr2[, c("y_h", fac_lag_cols), drop = FALSE]), , drop = FALSE]
  
  if (nrow(te2) != 1) return(NULL)
  
  Zf_test <- as.matrix(te2[, fac_lag_cols, drop = FALSE])
  
  if (any(!is.finite(Zf_test))) return(NULL)
  
  list(
    Zf_train = as.matrix(tr2[, fac_lag_cols, drop = FALSE]),
    yf_train = tr2$y_h,
    Zf_test = Zf_test
  )
}

eu_fit_predict_rf <- function(Z_train,
                              y_train,
                              Z_test,
                              num_trees = 1800,
                              min_node_size = 10) {
  
  if (!is.matrix(Z_train)) Z_train <- as.matrix(Z_train)
  if (!is.matrix(Z_test))  Z_test  <- as.matrix(Z_test)
  
  ok_x <- if (ncol(Z_train) == 0) {
    rep(TRUE, nrow(Z_train))
  } else {
    apply(Z_train, 1, function(r) all(is.finite(r)))
  }
  
  ok <- is.finite(y_train) & ok_x
  
  Z2 <- Z_train[ok, , drop = FALSE]
  y2 <- y_train[ok]
  
  if (length(y2) == 0) return(NA_real_)
  if (ncol(Z2) == 0) return(mean(y2))
  if (any(!is.finite(Z_test))) return(NA_real_)
  
  df_tr <- data.frame(y = y2, Z2)
  mtry <- max(1, floor(ncol(Z2) / 3))
  
  fit <- tryCatch(
    ranger::ranger(
      y ~ .,
      data = df_tr,
      num.trees = num_trees,
      mtry = mtry,
      min.node.size = min_node_size,
      verbose = FALSE,
      num.threads = 1
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NA_real_)
  
  as.numeric(stats::predict(fit, data = data.frame(Z_test))$predictions)
}

eu_tcsr_screen <- function(train_df, x_names, tc = 1.645) {
  keep <- c()
  base_cols <- paste0("y_L", 0:3)
  base_cols <- intersect(base_cols, names(train_df))
  
  for (x in x_names) {
    cols <- c("y_h", base_cols, x)
    d <- train_df[, cols, drop = FALSE]
    d <- d[complete.cases(d), , drop = FALSE]
    
    if (nrow(d) < 50) next
    
    fit <- tryCatch(
      stats::lm(d$y_h ~ ., data = d[, setdiff(cols, "y_h"), drop = FALSE]),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    sm <- summary(fit)
    
    if (x %in% rownames(coef(sm))) {
      tval <- coef(sm)[x, "t value"]
      if (is.finite(tval) && abs(tval) > tc) keep <- c(keep, x)
    }
  }
  
  unique(keep)
}

eu_tcsr_predict <- function(train_df,
                            test_row,
                            xstar,
                            L = 10,
                            M = 10000,
                            seed = 1) {
  
  set.seed(seed)
  
  y_tr <- train_df$y_h
  y0 <- train_df$y_L0
  
  if (length(xstar) == 0) {
    d_ar <- data.frame(y_h = y_tr, y_L0 = y0)
    d_ar <- d_ar[complete.cases(d_ar), , drop = FALSE]
    
    if (nrow(d_ar) < 10) return(NA_real_)
    
    fit_ar <- tryCatch(
      stats::lm.fit(x = cbind(1, d_ar$y_L0), y = d_ar$y_h),
      error = function(e) NULL
    )
    
    if (is.null(fit_ar) || is.null(fit_ar$coefficients)) return(NA_real_)
    
    beta <- fit_ar$coefficients
    beta[!is.finite(beta)] <- 0
    
    x_new <- c(1, as.numeric(test_row$y_L0))
    
    return(as.numeric(sum(x_new * beta)))
  }
  
  X_tr <- as.matrix(train_df[, xstar, drop = FALSE])
  X_te <- as.numeric(test_row[, xstar, drop = FALSE])
  
  preds <- rep(NA_real_, M)
  
  for (m in seq_len(M)) {
    idx <- if (length(xstar) >= L) {
      sample(seq_along(xstar), size = L, replace = FALSE)
    } else {
      sample(seq_along(xstar), size = L, replace = TRUE)
    }
    
    fit <- tryCatch(
      stats::lm.fit(cbind(1, y0, X_tr[, idx, drop = FALSE]), y_tr),
      error = function(e) NULL
    )
    
    if (is.null(fit) || is.null(fit$coefficients)) next
    
    beta <- fit$coefficients
    beta[!is.finite(beta)] <- 0
    
    preds[m] <- sum(c(1, test_row$y_L0, X_te[idx]) * beta)
  }
  
  if (!any(is.finite(preds))) return(NA_real_)
  
  mean(preds, na.rm = TRUE)
}

# ============================================================
# DETAIL INPUTS AND NON-CHRONOS RUNNER
# ============================================================

eu_prepare_fortin_detail_inputs <- function(panel_can,
                                            y_name,
                                            h,
                                            ctrl) {
  
  stopifnot("Date" %in% names(panel_can))
  stopifnot(y_name %in% names(panel_can))
  
  py <- ctrl$rolling$py
  px <- ctrl$rolling$px
  K_fixed <- ctrl$rolling$K_fixed
  oos_start <- ctrl$rolling$oos_start
  oos_end <- ctrl$rolling$oos_end
  retune_every_quarters <- ctrl$rolling$retune_every_quarters
  training_obs <- ctrl$rolling$training_obs
  
  df_base <- eu_as_numeric_df(panel_can, "Date") |>
    dplyr::arrange(.data$Date)
  
  x_names <- setdiff(setdiff(names(df_base), "Date"), y_name)
  
  df <- df_base
  df$y <- df[[y_name]]
  df$y_h <- eu_make_yh_direct(df$y, h = h)
  
  df_l <- df |>
    dplyr::select(.data$Date, .data$y, .data$y_h, dplyr::all_of(x_names)) |>
    eu_add_lags("y", p = py, prefix = "y")
  
  df_x <- df_l |>
    eu_add_lags_many(x_names, p = px) |>
    dplyr::filter(!is.na(.data$y_h), .data$Date <= oos_end)
  
  ycols <- paste0("y_L", 0:(py - 1))
  xlagcols <- unlist(lapply(x_names, function(v) paste0(v, "_L", 0:(px - 1))))
  req_cols <- c("y_h", ycols, xlagcols)
  
  df_x <- df_x[complete.cases(df_x[, c("Date", req_cols), drop = FALSE]), , drop = FALSE]
  
  eval_idx <- which(df_x$Date >= oos_start & df_x$Date <= oos_end)
  
  list(
    py = py,
    px = px,
    K_fixed = K_fixed,
    oos_start = oos_start,
    oos_end = oos_end,
    retune_every_quarters = retune_every_quarters,
    training_obs = training_obs,
    df_base = df_base,
    df_x = df_x,
    eval_idx = eval_idx,
    x_names = x_names,
    ycols = ycols,
    xlagcols = xlagcols
  )
}

eu_run_one_target_one_h_detail_nonchronos <- function(
    panel_can,
    y_name,
    h,
    ctrl,
    spec,
    models_to_run = spec$non_chronos_model_names,
    run_id = y_name,
    p = NULL
) {
  
  prep <- eu_prepare_fortin_detail_inputs(panel_can, y_name, h, ctrl)
  
  models_to_run <- unique(c(
    intersect(models_to_run, spec$non_chronos_model_names),
    spec$benchmark_model
  ))
  
  py <- prep$py
  px <- prep$px
  K_fixed <- prep$K_fixed
  retune_every_quarters <- prep$retune_every_quarters
  training_obs <- prep$training_obs
  
  df_base <- prep$df_base
  df_x <- prep$df_x
  eval_idx <- prep$eval_idx
  x_names <- prep$x_names
  ycols <- prep$ycols
  xlagcols <- prep$xlagcols
  
  glmnet_nlambda <- ctrl$glmnet$nlambda
  elastic_alpha_grid <- ctrl$glmnet$elastic_alpha_grid
  elastic_fallback_alpha <- ctrl$glmnet$elastic_fallback_alpha
  fallback_lambda <- ctrl$glmnet$fallback_lambda
  adalasso_eps <- ctrl$glmnet$adalasso_eps
  
  ocmt_ctrl <- ctrl$selection$ocmt
  bmt_ctrl <- ctrl$selection$bmt
  farm_ctrl <- ctrl$selection$farm
  
  ocmt_pvals <- eu_normalize_pval_grid(ocmt_ctrl$pvals)
  bmt_pvals <- eu_normalize_pval_grid(bmt_ctrl$pvals)
  
  rf_ctrl <- ctrl$model$rf
  tcsr_ctrl <- ctrl$model$tcsr
  
  ocmt_base_names <- paste0("OCMT_", names(ocmt_pvals))
  bmt_base_names <- paste0("BMT_", names(bmt_pvals))
  
  run_ar <- "AR,BIC" %in% models_to_run
  run_ardi_bic <- "ARDI,BIC" %in% models_to_run
  
  run_x_block <- any(c(
    "Ridge-X", "Lasso-X", "Elastic-Net-X", "Adaptive-Lasso-X",
    ocmt_base_names, "FarmSelect", bmt_base_names, "RF-X"
  ) %in% models_to_run)
  
  run_factor_block <- any(c(
    "ARDI,Ridge", "ARDI,Lasso", "ARDI,Elastic-Net", "ARDI,Adaptive-Lasso", "RFARDI"
  ) %in% models_to_run)
  
  run_tcsr <- "T-CSR" %in% models_to_run
  
  need_retune <- function(last, now) {
    is.na(last) || eu_quarters_between(last, now) >= retune_every_quarters
  }
  
  fallback_tunes <- eu_empty_fallback_counter(spec)
  fallback_forecasts <- eu_empty_fallback_counter(spec)
  tcsr_counter <- eu_empty_tcsr_counter()
  
  pred_tbl <- as.data.frame(
    matrix(NA_real_, nrow = nrow(df_x), ncol = length(models_to_run)),
    stringsAsFactors = FALSE
  )
  
  names(pred_tbl) <- models_to_run
  
  if (nrow(df_x) == 0 || length(eval_idx) == 0) {
    return(list(
      h = h,
      dates = df_x$Date,
      y_true = df_x$y_h,
      pred_tbl = pred_tbl,
      meta = list(
        fallback_tunes = fallback_tunes,
        fallback_forecasts = fallback_forecasts,
        tcsr_benchmark_fallbacks = 0L,
        tcsr_total_forecasts = 0L
      )
    ))
  }
  
  last_tune <- list(
    ridge = as.Date(NA),
    lasso = as.Date(NA),
    elastic = as.Date(NA),
    adalasso = as.Date(NA),
    ocmt = stats::setNames(rep(as.Date(NA), length(ocmt_base_names)), ocmt_base_names),
    bmt = stats::setNames(rep(as.Date(NA), length(bmt_base_names)), bmt_base_names),
    farm = as.Date(NA),
    ardi_ridge = as.Date(NA),
    ardi_lasso = as.Date(NA),
    ardi_elastic = as.Date(NA),
    ardi_adalasso = as.Date(NA),
    tcsr = as.Date(NA)
  )
  
  model_specs <- list(
    ocmt_sel = list(),
    bmt_sel = list()
  )
  
  for (i in eval_idx) {
    t0 <- df_x$Date[i]
    cutoff <- t0 %m-% lubridate::period(months = 3 * h)
    
    train_idx <- which(df_x$Date <= cutoff)
    if (length(train_idx) < training_obs) next
    
    train_df <- df_x[train_idx, , drop = FALSE]
    test_row <- df_x[i, , drop = FALSE]
    
    if (run_ar) {
      pred_tbl[i, "AR,BIC"] <- eu_fit_predict_AR_BIC(train_df, test_row, py_max = py)
    }
    
    factor_hist <- NULL
    
    if (run_ardi_bic || run_factor_block) {
      factor_hist <- eu_build_factor_history(
        panel_df = df_base,
        y_name = y_name,
        x_names = x_names,
        cutoff = cutoff,
        t0 = t0,
        Kmax = K_fixed
      )
    }
    
    if (run_ardi_bic) {
      pred_tbl[i, "ARDI,BIC"] <- eu_fit_predict_ARDI_BIC(
        factor_hist = factor_hist,
        train_dates = train_df$Date,
        test_date = t0,
        h = h,
        py_max = py,
        pf_max = px
      )
    }
    
    if (run_x_block) {
      Zx_cols <- c(ycols, xlagcols)
      Zx_train <- as.matrix(train_df[, Zx_cols, drop = FALSE])
      Zy_train <- train_df$y_h
      Zx_test <- as.matrix(test_row[, Zx_cols, drop = FALSE])
      
      Xcond_train <- as.matrix(train_df[, ycols, drop = FALSE])
      Xcond_test <- as.matrix(test_row[, ycols, drop = FALSE])
      
      Xcand_train <- as.matrix(train_df[, xlagcols, drop = FALSE])
      Xcand_test <- as.matrix(test_row[, xlagcols, drop = FALSE])
      
      if ("Ridge-X" %in% models_to_run) {
        if (need_retune(last_tune$ridge, t0)) {
          model_specs$ridge <- c(
            list(type = "ridge"),
            eu_tune_ridge_or_lasso(
              Zx_train,
              Zy_train,
              alpha = 0,
              nlambda = glmnet_nlambda,
              fallback_lambda = fallback_lambda
            )
          )
          
          if (isTRUE(model_specs$ridge$used_fallback)) {
            fallback_tunes["Ridge-X"] <- fallback_tunes["Ridge-X"] + 1L
          }
          
          last_tune$ridge <- t0
        }
        
        if (isTRUE(model_specs$ridge$used_fallback)) {
          fallback_forecasts["Ridge-X"] <- fallback_forecasts["Ridge-X"] + 1L
        }
        
        pred_tbl[i, "Ridge-X"] <- eu_fit_predict_glmnet(
          Zx_train,
          Zy_train,
          Zx_test,
          model_specs$ridge,
          fallback_lambda
        )
      }
      
      if ("Lasso-X" %in% models_to_run) {
        if (need_retune(last_tune$lasso, t0)) {
          model_specs$lasso <- c(
            list(type = "lasso"),
            eu_tune_ridge_or_lasso(
              Zx_train,
              Zy_train,
              alpha = 1,
              nlambda = glmnet_nlambda,
              fallback_lambda = fallback_lambda
            )
          )
          
          if (isTRUE(model_specs$lasso$used_fallback)) {
            fallback_tunes["Lasso-X"] <- fallback_tunes["Lasso-X"] + 1L
          }
          
          last_tune$lasso <- t0
        }
        
        if (isTRUE(model_specs$lasso$used_fallback)) {
          fallback_forecasts["Lasso-X"] <- fallback_forecasts["Lasso-X"] + 1L
        }
        
        pred_tbl[i, "Lasso-X"] <- eu_fit_predict_glmnet(
          Zx_train,
          Zy_train,
          Zx_test,
          model_specs$lasso,
          fallback_lambda
        )
      }
      
      if ("Elastic-Net-X" %in% models_to_run) {
        if (need_retune(last_tune$elastic, t0)) {
          model_specs$elastic <- c(
            list(type = "elastic"),
            eu_tune_elasticnet(
              Zx_train,
              Zy_train,
              alpha_grid = elastic_alpha_grid,
              nlambda = glmnet_nlambda,
              fallback_alpha = elastic_fallback_alpha,
              fallback_lambda = fallback_lambda
            )
          )
          
          if (isTRUE(model_specs$elastic$used_fallback)) {
            fallback_tunes["Elastic-Net-X"] <- fallback_tunes["Elastic-Net-X"] + 1L
          }
          
          last_tune$elastic <- t0
        }
        
        if (isTRUE(model_specs$elastic$used_fallback)) {
          fallback_forecasts["Elastic-Net-X"] <- fallback_forecasts["Elastic-Net-X"] + 1L
        }
        
        pred_tbl[i, "Elastic-Net-X"] <- eu_fit_predict_glmnet(
          Zx_train,
          Zy_train,
          Zx_test,
          model_specs$elastic,
          fallback_lambda
        )
      }
      
      if ("Adaptive-Lasso-X" %in% models_to_run) {
        if (need_retune(last_tune$adalasso, t0)) {
          ta <- eu_tune_adaptive_lasso(
            Zx_train,
            Zy_train,
            nlambda = glmnet_nlambda,
            eps = adalasso_eps,
            fallback_lambda = fallback_lambda
          )
          
          model_specs$adalasso <- list(
            type = "adalasso",
            lambda = ta$lambda,
            weights = ta$weights,
            used_fallback = ta$used_fallback
          )
          
          if (isTRUE(model_specs$adalasso$used_fallback)) {
            fallback_tunes["Adaptive-Lasso-X"] <- fallback_tunes["Adaptive-Lasso-X"] + 1L
          }
          
          last_tune$adalasso <- t0
        } else {
          ta <- eu_tune_adaptive_lasso(
            Zx_train,
            Zy_train,
            nlambda = glmnet_nlambda,
            eps = adalasso_eps,
            fallback_lambda = fallback_lambda
          )
          
          model_specs$adalasso$weights <- ta$weights
        }
        
        if (isTRUE(model_specs$adalasso$used_fallback)) {
          fallback_forecasts["Adaptive-Lasso-X"] <- fallback_forecasts["Adaptive-Lasso-X"] + 1L
        }
        
        pred_tbl[i, "Adaptive-Lasso-X"] <- eu_fit_predict_glmnet(
          Zx_train,
          Zy_train,
          Zx_test,
          model_specs$adalasso,
          fallback_lambda
        )
      }
      
      for (lev in names(ocmt_pvals)) {
        base_name <- paste0("OCMT_", lev)
        if (!(base_name %in% models_to_run)) next
        
        if (need_retune(last_tune$ocmt[base_name], t0)) {
          model_specs$ocmt_sel[[base_name]] <- eu_select_ocmt(
            y_train = Zy_train,
            Xcand_train = Xcand_train,
            Xcond_train = Xcond_train,
            pval = ocmt_pvals[[lev]],
            delta1 = ocmt_ctrl$delta1,
            delta2 = ocmt_ctrl$delta2,
            multi_stage = ocmt_ctrl$multi_stage,
            max_iter = ocmt_ctrl$max_iter,
            kappa_const = ocmt_ctrl$kappa_const
          )
          
          last_tune$ocmt[base_name] <- t0
        }
        
        pred_tbl[i, base_name] <- eu_predict_from_selection_ols(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcand_test = Xcand_test,
          Xcond_train = Xcond_train,
          Xcond_test = Xcond_test,
          selected = model_specs$ocmt_sel[[base_name]]
        )
      }
      
      if ("FarmSelect" %in% models_to_run) {
        model_specs$farm_fit <- eu_fit_farm_native(
          y_train = Zy_train,
          Z_train = Xcand_train,
          loss = farm_ctrl$loss,
          robust = farm_ctrl$robust,
          cv = farm_ctrl$cv,
          tau = farm_ctrl$tau,
          K.factors = farm_ctrl$K.factors,
          max.iter = farm_ctrl$max.iter,
          nfolds = farm_ctrl$nfolds,
          eps = farm_ctrl$eps
        )
        
        if (isTRUE(model_specs$farm_fit$use_mean) || is.null(model_specs$farm_fit$fit)) {
          fallback_tunes["FarmSelect"] <- fallback_tunes["FarmSelect"] + 1L
        }
        
        farm_pred <- eu_predict_farm_native(
          farm_obj = model_specs$farm_fit,
          Z_test = Xcand_test,
          return_info = TRUE
        )
        
        if (isTRUE(farm_pred$used_fallback)) {
          fallback_forecasts["FarmSelect"] <- fallback_forecasts["FarmSelect"] + 1L
        }
        
        pred_tbl[i, "FarmSelect"] <- farm_pred$pred
      }
      
      for (lev in names(bmt_pvals)) {
        base_name <- paste0("BMT_", lev)
        if (!(base_name %in% models_to_run)) next
        
        if (need_retune(last_tune$bmt[base_name], t0)) {
          model_specs$bmt_sel[[base_name]] <- eu_select_bmt(
            y_train = Zy_train,
            Xcand_train = Xcand_train,
            Xcond_train = Xcond_train,
            pval = bmt_pvals[[lev]],
            delta1 = bmt_ctrl$delta1,
            delta2 = bmt_ctrl$delta2,
            maxit = bmt_ctrl$maxit
          )
          
          last_tune$bmt[base_name] <- t0
        }
        
        pred_tbl[i, base_name] <- eu_predict_from_selection_ols(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcand_test = Xcand_test,
          Xcond_train = Xcond_train,
          Xcond_test = Xcond_test,
          selected = model_specs$bmt_sel[[base_name]]
        )
      }
      
      if ("RF-X" %in% models_to_run) {
        pred_tbl[i, "RF-X"] <- eu_fit_predict_rf(
          Zx_train,
          Zy_train,
          Zx_test,
          num_trees = rf_ctrl$num_trees,
          min_node_size = rf_ctrl$min_node_size
        )
      }
    }
    
    if (run_factor_block) {
      factor_design <- eu_build_ardi_fixed_design(
        factor_hist = factor_hist,
        train_dates = train_df$Date,
        test_date = t0,
        h = h,
        p_factor_lags = py
      )
      
      if (!is.null(factor_design) && nrow(factor_design$Zf_train) >= 50) {
        Zf_train <- factor_design$Zf_train
        yf_train <- factor_design$yf_train
        Zf_test <- factor_design$Zf_test
        
        if ("ARDI,Ridge" %in% models_to_run) {
          if (need_retune(last_tune$ardi_ridge, t0)) {
            model_specs$ardi_ridge <- c(
              list(type = "ridge"),
              eu_tune_ridge_or_lasso(
                Zf_train,
                yf_train,
                alpha = 0,
                nlambda = glmnet_nlambda,
                fallback_lambda = fallback_lambda
              )
            )
            
            if (isTRUE(model_specs$ardi_ridge$used_fallback)) {
              fallback_tunes["ARDI,Ridge"] <- fallback_tunes["ARDI,Ridge"] + 1L
            }
            
            last_tune$ardi_ridge <- t0
          }
          
          if (isTRUE(model_specs$ardi_ridge$used_fallback)) {
            fallback_forecasts["ARDI,Ridge"] <- fallback_forecasts["ARDI,Ridge"] + 1L
          }
          
          pred_tbl[i, "ARDI,Ridge"] <- eu_fit_predict_glmnet(
            Zf_train,
            yf_train,
            Zf_test,
            model_specs$ardi_ridge,
            fallback_lambda
          )
        }
        
        if ("ARDI,Lasso" %in% models_to_run) {
          if (need_retune(last_tune$ardi_lasso, t0)) {
            model_specs$ardi_lasso <- c(
              list(type = "lasso"),
              eu_tune_ridge_or_lasso(
                Zf_train,
                yf_train,
                alpha = 1,
                nlambda = glmnet_nlambda,
                fallback_lambda = fallback_lambda
              )
            )
            
            if (isTRUE(model_specs$ardi_lasso$used_fallback)) {
              fallback_tunes["ARDI,Lasso"] <- fallback_tunes["ARDI,Lasso"] + 1L
            }
            
            last_tune$ardi_lasso <- t0
          }
          
          if (isTRUE(model_specs$ardi_lasso$used_fallback)) {
            fallback_forecasts["ARDI,Lasso"] <- fallback_forecasts["ARDI,Lasso"] + 1L
          }
          
          pred_tbl[i, "ARDI,Lasso"] <- eu_fit_predict_glmnet(
            Zf_train,
            yf_train,
            Zf_test,
            model_specs$ardi_lasso,
            fallback_lambda
          )
        }
        
        if ("ARDI,Elastic-Net" %in% models_to_run) {
          if (need_retune(last_tune$ardi_elastic, t0)) {
            model_specs$ardi_elastic <- c(
              list(type = "elastic"),
              eu_tune_elasticnet(
                Zf_train,
                yf_train,
                alpha_grid = elastic_alpha_grid,
                nlambda = glmnet_nlambda,
                fallback_alpha = elastic_fallback_alpha,
                fallback_lambda = fallback_lambda
              )
            )
            
            if (isTRUE(model_specs$ardi_elastic$used_fallback)) {
              fallback_tunes["ARDI,Elastic-Net"] <- fallback_tunes["ARDI,Elastic-Net"] + 1L
            }
            
            last_tune$ardi_elastic <- t0
          }
          
          if (isTRUE(model_specs$ardi_elastic$used_fallback)) {
            fallback_forecasts["ARDI,Elastic-Net"] <- fallback_forecasts["ARDI,Elastic-Net"] + 1L
          }
          
          pred_tbl[i, "ARDI,Elastic-Net"] <- eu_fit_predict_glmnet(
            Zf_train,
            yf_train,
            Zf_test,
            model_specs$ardi_elastic,
            fallback_lambda
          )
        }
        
        if ("ARDI,Adaptive-Lasso" %in% models_to_run) {
          if (need_retune(last_tune$ardi_adalasso, t0)) {
            taf <- eu_tune_adaptive_lasso(
              Zf_train,
              yf_train,
              nlambda = glmnet_nlambda,
              eps = adalasso_eps,
              fallback_lambda = fallback_lambda
            )
            
            model_specs$ardi_adalasso <- list(
              type = "adalasso",
              lambda = taf$lambda,
              weights = taf$weights,
              used_fallback = taf$used_fallback
            )
            
            if (isTRUE(model_specs$ardi_adalasso$used_fallback)) {
              fallback_tunes["ARDI,Adaptive-Lasso"] <- fallback_tunes["ARDI,Adaptive-Lasso"] + 1L
            }
            
            last_tune$ardi_adalasso <- t0
          } else {
            taf <- eu_tune_adaptive_lasso(
              Zf_train,
              yf_train,
              nlambda = glmnet_nlambda,
              eps = adalasso_eps,
              fallback_lambda = fallback_lambda
            )
            
            model_specs$ardi_adalasso$weights <- taf$weights
          }
          
          if (isTRUE(model_specs$ardi_adalasso$used_fallback)) {
            fallback_forecasts["ARDI,Adaptive-Lasso"] <- fallback_forecasts["ARDI,Adaptive-Lasso"] + 1L
          }
          
          pred_tbl[i, "ARDI,Adaptive-Lasso"] <- eu_fit_predict_glmnet(
            Zf_train,
            yf_train,
            Zf_test,
            model_specs$ardi_adalasso,
            fallback_lambda
          )
        }
        
        if ("RFARDI" %in% models_to_run) {
          pred_tbl[i, "RFARDI"] <- eu_fit_predict_rf(
            Zf_train,
            yf_train,
            Zf_test,
            num_trees = rf_ctrl$num_trees,
            min_node_size = rf_ctrl$min_node_size
          )
        }
      }
    }
    
    if (run_tcsr) {
      if (need_retune(last_tune$tcsr, t0)) {
        model_specs$tcsr <- list(
          xstar = eu_tcsr_screen(train_df, x_names, tc = tcsr_ctrl$tc)
        )
        
        last_tune$tcsr <- t0
      }
      
      tcsr_counter["tcsr_total_forecasts"] <- tcsr_counter["tcsr_total_forecasts"] + 1L
      
      tcsr_raw <- eu_tcsr_predict(
        train_df,
        test_row,
        xstar = model_specs$tcsr$xstar,
        L = tcsr_ctrl$L,
        M = tcsr_ctrl$M,
        seed = 1
      )
      
      if (!is.finite(tcsr_raw) && is.finite(pred_tbl[i, spec$benchmark_model])) {
        pred_tbl[i, "T-CSR"] <- pred_tbl[i, spec$benchmark_model]
        tcsr_counter["tcsr_benchmark_fallbacks"] <- tcsr_counter["tcsr_benchmark_fallbacks"] + 1L
      } else {
        pred_tbl[i, "T-CSR"] <- tcsr_raw
      }
    }
  }
  
  if (!is.null(p)) {
    p(message = sprintf("%s | h=%s finished", run_id, h))
  }
  
  list(
    h = h,
    dates = df_x$Date,
    y_true = df_x$y_h,
    pred_tbl = pred_tbl,
    meta = list(
      fallback_tunes = fallback_tunes,
      fallback_forecasts = fallback_forecasts,
      tcsr_benchmark_fallbacks = as.integer(tcsr_counter["tcsr_benchmark_fallbacks"]),
      tcsr_total_forecasts = as.integer(tcsr_counter["tcsr_total_forecasts"])
    )
  )
}

# ============================================================
# TASK GRID / MERGING
# ============================================================

eu_make_task_list <- function(targets_all, ctrl) {
  purrr::imap(targets_all, function(ys, g) {
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
}

eu_run_task_grid_nonchronos <- function(panel_can,
                                        task_list,
                                        ctrl,
                                        spec,
                                        models_to_run = spec$non_chronos_model_names) {
  
  if (nrow(task_list) == 0) return(list())
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  available_workers <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
  available_workers <- available_workers[1]
  if (!is.finite(available_workers) || available_workers < 1) available_workers <- 1L
  
  requested_workers <- ctrl$parallel$requested_workers
  n_workers_use <- max(1L, min(requested_workers, available_workers, nrow(task_list)))
  
  message(sprintf(
    "EU non-Chronos phase | Requested workers: %d | Available cores: %d | Parallel tasks: %d | Using workers: %d",
    requested_workers,
    available_workers,
    nrow(task_list),
    n_workers_use
  ))
  
  future::plan(future::multisession, workers = n_workers_use)
  
  total_steps <- nrow(task_list)
  
  progressr::with_progress({
    p <- progressr::progressor(steps = total_steps)
    
    furrr::future_map(
      seq_len(total_steps),
      function(i) {
        eu_run_one_target_one_h_detail_nonchronos(
          panel_can = panel_can,
          y_name = task_list$yname[i],
          h = task_list$h[i],
          ctrl = ctrl,
          spec = spec,
          models_to_run = models_to_run,
          run_id = sprintf("%s / %s", task_list$group[i], task_list$yname[i]),
          p = p
        )
      },
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c(
          "dplyr",
          "purrr",
          "forecast",
          "glmnet",
          "ranger",
          "lubridate",
          "progressr",
          "OCMT",
          "FarmSelect",
          "MultipleTestingBoosting"
        )
      )
    )
  })
}

eu_summarise_task_detail <- function(detail_res, spec) {
  y_true <- detail_res$y_true
  pred_tbl <- as.data.frame(detail_res$pred_tbl, stringsAsFactors = FALSE)
  h <- detail_res$h
  models_final <- spec$model_order_internal
  
  res_h <- lapply(models_final, function(m) {
    if (!(m %in% names(pred_tbl))) {
      return(list(RMSE = NA_real_, ok = rep(FALSE, length(y_true))))
    }
    eu_compute_RMSE_info(y_true, pred_tbl[[m]])
  })
  
  names(res_h) <- models_final
  
  bmk <- res_h[[spec$benchmark_model]]
  
  ratio <- sapply(models_final, function(m) {
    if (!is.finite(res_h[[m]]$RMSE) || !is.finite(bmk$RMSE) || bmk$RMSE <= 0) {
      return(NA_real_)
    }
    res_h[[m]]$RMSE / bmk$RMSE
  })
  
  ratio[spec$benchmark_model] <- NA_real_
  
  pvals <- rep(NA_real_, length(models_final))
  names(pvals) <- models_final
  
  for (m in setdiff(models_final, spec$benchmark_model)) {
    if (!(m %in% names(pred_tbl))) next
    
    ok <- res_h[[m]]$ok & bmk$ok
    if (!any(ok)) next
    
    e_b <- y_true[ok] - pred_tbl[ok, spec$benchmark_model]
    e_m <- y_true[ok] - pred_tbl[ok, m]
    
    pvals[m] <- eu_dm_pval(
      e_bench = e_b,
      e_model = e_m,
      h = h
    )
  }
  
  list(
    h = h,
    RMSE_bmk = bmk$RMSE,
    ratio = ratio,
    pvals = pvals,
    dates = detail_res$dates,
    meta = detail_res$meta
  )
}

eu_merge_task_details <- function(base_detail,
                                  add_detail = NULL,
                                  spec) {
  
  detail_use <- if (!is.null(base_detail)) base_detail else add_detail
  
  dates_use <- detail_use$dates
  y_true_use <- detail_use$y_true
  h_use <- detail_use$h
  
  final_models <- spec$model_order_internal
  
  pred_all <- as.data.frame(
    matrix(NA_real_, nrow = length(y_true_use), ncol = length(final_models)),
    stringsAsFactors = FALSE
  )
  
  names(pred_all) <- final_models
  
  meta_use <- list(
    fallback_tunes = eu_empty_fallback_counter(spec),
    fallback_forecasts = eu_empty_fallback_counter(spec),
    tcsr_benchmark_fallbacks = 0L,
    tcsr_total_forecasts = 0L
  )
  
  add_meta <- function(meta_obj) {
    if (is.null(meta_obj)) return(invisible(NULL))
    
    if (!is.null(meta_obj$fallback_tunes)) {
      common <- intersect(names(meta_use$fallback_tunes), names(meta_obj$fallback_tunes))
      meta_use$fallback_tunes[common] <<- meta_use$fallback_tunes[common] + meta_obj$fallback_tunes[common]
    }
    
    if (!is.null(meta_obj$fallback_forecasts)) {
      common <- intersect(names(meta_use$fallback_forecasts), names(meta_obj$fallback_forecasts))
      meta_use$fallback_forecasts[common] <<- meta_use$fallback_forecasts[common] + meta_obj$fallback_forecasts[common]
    }
    
    if (!is.null(meta_obj$tcsr_benchmark_fallbacks)) {
      meta_use$tcsr_benchmark_fallbacks <<- meta_use$tcsr_benchmark_fallbacks + meta_obj$tcsr_benchmark_fallbacks
    }
    
    if (!is.null(meta_obj$tcsr_total_forecasts)) {
      meta_use$tcsr_total_forecasts <<- meta_use$tcsr_total_forecasts + meta_obj$tcsr_total_forecasts
    }
  }
  
  if (!is.null(base_detail)) {
    for (nm in intersect(names(base_detail$pred_tbl), final_models)) {
      pred_all[[nm]] <- base_detail$pred_tbl[[nm]]
    }
    add_meta(base_detail$meta)
  }
  
  if (!is.null(add_detail)) {
    if (!isTRUE(all.equal(as.Date(dates_use), as.Date(add_detail$dates)))) {
      stop("Date mismatch when merging non-Chronos and Chronos results.")
    }
    
    if (!isTRUE(all.equal(y_true_use, add_detail$y_true, check.attributes = FALSE))) {
      stop("y_true mismatch when merging non-Chronos and Chronos results.")
    }
    
    for (nm in intersect(names(add_detail$pred_tbl), final_models)) {
      pred_all[[nm]] <- add_detail$pred_tbl[[nm]]
    }
    
    add_meta(add_detail$meta)
  }
  
  pred_all[["AVG"]] <- vapply(seq_len(nrow(pred_all)), function(i) {
    current_preds <- as.numeric(pred_all[i, spec$combo_candidate_models, drop = TRUE])
    names(current_preds) <- spec$combo_candidate_models
    
    eu_compute_simple_avg_forecast(
      current_preds,
      spec$combo_candidate_models
    )
  }, numeric(1))
  
  merged_detail <- list(
    h = h_use,
    dates = dates_use,
    y_true = y_true_use,
    pred_tbl = pred_all,
    meta = meta_use
  )
  
  eu_summarise_task_detail(merged_detail, spec = spec)
}

eu_build_merged_results <- function(nonchronos_results,
                                    chronos_results = NULL,
                                    task_list,
                                    target_order,
                                    spec) {
  
  out <- stats::setNames(vector("list", length(target_order)), target_order)
  
  for (tg in target_order) {
    out[[tg]] <- list(
      meta = list(
        fallback_tunes = eu_empty_fallback_counter(spec),
        fallback_forecasts = eu_empty_fallback_counter(spec),
        tcsr_benchmark_fallbacks = 0L,
        tcsr_total_forecasts = 0L
      )
    )
  }
  
  for (i in seq_len(nrow(task_list))) {
    tg <- task_list$yname[i]
    h <- task_list$h[i]
    
    add_detail <- if (is.null(chronos_results)) NULL else chronos_results[[i]]
    
    res_merged <- eu_merge_task_details(
      base_detail = nonchronos_results[[i]],
      add_detail = add_detail,
      spec = spec
    )
    
    out[[tg]][[paste0("h", h)]] <- res_merged[c(
      "h",
      "RMSE_bmk",
      "ratio",
      "pvals",
      "dates"
    )]
    
    out[[tg]]$meta$fallback_tunes <-
      out[[tg]]$meta$fallback_tunes + res_merged$meta$fallback_tunes
    
    out[[tg]]$meta$fallback_forecasts <-
      out[[tg]]$meta$fallback_forecasts + res_merged$meta$fallback_forecasts
    
    out[[tg]]$meta$tcsr_benchmark_fallbacks <-
      out[[tg]]$meta$tcsr_benchmark_fallbacks + res_merged$meta$tcsr_benchmark_fallbacks
    
    out[[tg]]$meta$tcsr_total_forecasts <-
      out[[tg]]$meta$tcsr_total_forecasts + res_merged$meta$tcsr_total_forecasts
  }
  
  out[target_order]
}

eu_summarise_fallback_counts <- function(all_results, spec) {
  tunes <- eu_empty_fallback_counter(spec)
  forecasts <- eu_empty_fallback_counter(spec)
  
  tcsr_benchmark_fallbacks <- 0L
  tcsr_total_forecasts <- 0L
  
  for (nm in names(all_results)) {
    meta <- all_results[[nm]]$meta
    if (is.null(meta)) next
    
    if (!is.null(meta$fallback_tunes)) {
      common <- intersect(names(tunes), names(meta$fallback_tunes))
      tunes[common] <- tunes[common] + meta$fallback_tunes[common]
    }
    
    if (!is.null(meta$fallback_forecasts)) {
      common <- intersect(names(forecasts), names(meta$fallback_forecasts))
      forecasts[common] <- forecasts[common] + meta$fallback_forecasts[common]
    }
    
    if (!is.null(meta$tcsr_benchmark_fallbacks)) {
      tcsr_benchmark_fallbacks <- tcsr_benchmark_fallbacks + meta$tcsr_benchmark_fallbacks
    }
    
    if (!is.null(meta$tcsr_total_forecasts)) {
      tcsr_total_forecasts <- tcsr_total_forecasts + meta$tcsr_total_forecasts
    }
  }
  
  list(
    fallback_tunes = tunes,
    fallback_forecasts = forecasts,
    as_table = data.frame(
      Model = names(tunes),
      fallback_tunes = as.integer(tunes),
      fallback_forecasts = as.integer(forecasts),
      row.names = NULL
    ),
    tcsr_as_table = data.frame(
      Model = "T-CSR",
      benchmark_fallbacks = as.integer(tcsr_benchmark_fallbacks),
      total_forecasts = as.integer(tcsr_total_forecasts),
      fallback_share = if (tcsr_total_forecasts > 0) {
        tcsr_benchmark_fallbacks / tcsr_total_forecasts
      } else {
        NA_real_
      },
      row.names = NULL
    )
  )
}

# ============================================================
# CHRONOS EXTERNAL SCRIPT
# ============================================================

eu_write_chronos_python <- function(py_file) {
  
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


def add_quarter_covariates(df):
    ts = pd.to_datetime(df["timestamp"])
    qtr = ((ts.dt.month - 1) // 3) + 1
    df["qtr_sin"] = np.sin(2 * np.pi * qtr / 4)
    df["qtr_cos"] = np.cos(2 * np.pi * qtr / 4)
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

    if len(d) < 12:
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

    if len(d) < 12:
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

    if len(d) < 12:
        return None

    out = pd.DataFrame({
        "id": yname,
        "timestamp": pd.to_datetime(d["Date"]),
        "target": pd.to_numeric(d[yname], errors="coerce")
    }).dropna()

    out = add_quarter_covariates(out)

    if context_max is not None and len(out) > context_max:
        out = out.tail(context_max)

    return out


def make_future_cov(last_date, h, yname):
    last_date = pd.to_datetime(last_date)

    future_dates = pd.date_range(
        start=last_date + pd.DateOffset(months=3),
        periods=int(h),
        freq="QS"
    )

    out = pd.DataFrame({
        "id": yname,
        "timestamp": future_dates
    })

    out = add_quarter_covariates(out)

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
    training_obs = int(cfg.get("training_obs", 40))
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

            cutoff = t0 - pd.DateOffset(months=3 * h)
            train_count = int((task_dates <= cutoff).sum())

            if train_count < training_obs:
                results.append(out_row)
                continue

            hist = panel[panel["Date"] <= t0].copy()

            try:
                if bool(cfg.get("enabled", True)) and bool(cfg.get("uni_enabled", True)):
                    context_uni = make_uni_context(hist, yname, context_max)

                    if context_uni is not None:
                        pred_uni = predict_df_safe(
                            pipe=pipe,
                            context_df=context_uni,
                            prediction_length=h,
                            target="target",
                            future_df=None,
                            cfg=cfg
                        )

                        out_row["Chronos-2-uni"] = extract_point_forecast(
                            pred_uni,
                            target_name="target",
                            h=h
                        )

                if bool(cfg.get("enabled", True)) and bool(cfg.get("multi_enabled", True)):
                    context_multi, names_multi = make_multi_context(
                        hist,
                        yname,
                        target_order,
                        context_max
                    )

                    if context_multi is not None and len(names_multi) >= 2:
                        pred_multi = predict_df_safe(
                            pipe=pipe,
                            context_df=context_multi,
                            prediction_length=h,
                            target=names_multi,
                            future_df=None,
                            cfg=cfg
                        )

                        out_row["Chronos-2-multi"] = extract_point_forecast(
                            pred_multi,
                            target_name=yname,
                            h=h
                        )

                if bool(cfg.get("enabled", True)) and bool(cfg.get("covariate_enabled", True)):
                    context_cov = make_cov_context(hist, yname, context_max)

                    if context_cov is not None:
                        future_cov = make_future_cov(hist["Date"].max(), h, yname)

                        pred_cov = predict_df_safe(
                            pipe=pipe,
                            context_df=context_cov,
                            prediction_length=h,
                            target="target",
                            future_df=future_cov,
                            cfg=cfg
                        )

                        out_row["Chronos-2-covariate-informed"] = extract_point_forecast(
                            pred_cov,
                            target_name="target",
                            h=h
                        )

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
  
  dir.create(dirname(py_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(py_code, con = py_file)
  
  invisible(py_file)
}

eu_run_chronos_external <- function(panel_can,
                                    nonchronos_results,
                                    task_list,
                                    ctrl,
                                    target_order,
                                    spec,
                                    phase_dir,
                                    chronos_python,
                                    prefix = "eu") {
  
  if (!isTRUE(ctrl$model$chronos2$enabled)) {
    return(NULL)
  }
  
  if (!file.exists(chronos_python)) {
    stop("Chronos Python was not found at:\n", chronos_python)
  }
  
  dir.create(phase_dir, recursive = TRUE, showWarnings = FALSE)
  
  panel_csv <- file.path(phase_dir, paste0(prefix, "_panel_for_chronos.csv"))
  request_csv <- file.path(phase_dir, paste0(prefix, "_chronos_request.csv"))
  config_json <- file.path(phase_dir, paste0(prefix, "_chronos_config.json"))
  py_file <- file.path(phase_dir, paste0("run_", prefix, "_chronos_external.py"))
  out_csv <- file.path(phase_dir, paste0(prefix, "_chronos_predictions.csv"))
  log_file <- file.path(phase_dir, paste0(prefix, "_chronos_external_log.txt"))
  
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
    oos_end = as.character(ctrl$rolling$oos_end),
    training_obs = as.integer(ctrl$rolling$training_obs),
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
  
  eu_write_chronos_python(py_file)
  
  if (file.exists(out_csv)) file.remove(out_csv)
  if (file.exists(log_file)) file.remove(log_file)
  
  cat("\nRunning external Chronos Python...\n")
  cat("Python:", chronos_python, "\n")
  cat("Script:", py_file, "\n")
  cat("Output:", out_csv, "\n")
  cat("Log:   ", log_file, "\n\n")
  
  status <- system2(
    command = chronos_python,
    args = c(
      shQuote(py_file),
      "--panel", shQuote(panel_csv),
      "--request", shQuote(request_csv),
      "--config", shQuote(config_json),
      "--out", shQuote(out_csv)
    ),
    stdout = log_file,
    stderr = log_file
  )
  
  cat("\nPython exit status:", status, "\n")
  
  if (status != 0 || !file.exists(out_csv)) {
    cat("\nChronos failed. Last log lines:\n")
    log_lines <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
    cat(tail(log_lines, 200), sep = "\n")
    stop("External Chronos run failed.")
  }
  
  chronos_pred_long <- read.csv(
    out_csv,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  chronos_pred_long$task_id <- as.integer(chronos_pred_long$task_id)
  chronos_pred_long$row_i <- as.integer(chronos_pred_long$row_i)
  
  chronos_model_cols <- spec$chronos_only_model_names
  
  missing_cols <- setdiff(chronos_model_cols, names(chronos_pred_long))
  
  if (length(missing_cols) > 0) {
    stop(
      "These Chronos columns are missing:\n",
      paste(missing_cols, collapse = ", ")
    )
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
        fallback_tunes = eu_empty_fallback_counter(spec),
        fallback_forecasts = eu_empty_fallback_counter(spec),
        tcsr_benchmark_fallbacks = 0L,
        tcsr_total_forecasts = 0L
      )
    )
  }
  
  list(
    chronos_results = chronos_results,
    chronos_pred_long = chronos_pred_long,
    panel_csv = panel_csv,
    request_csv = request_csv,
    config_json = config_json,
    py_file = py_file,
    out_csv = out_csv,
    log_file = log_file
  )
}

# ============================================================
# PLOTS
# ============================================================

eu_plot_target_series <- function(panel_data,
                                  targets,
                                  target_labels = NULL,
                                  out_dir) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (is.null(target_labels)) {
    target_labels <- stats::setNames(targets, targets)
  }
  
  targets <- intersect(targets, names(panel_data))
  
  for (tg in targets) {
    d <- panel_data[, c("Date", tg), drop = FALSE]
    d <- d[is.finite(d[[tg]]) & !is.na(d$Date), , drop = FALSE]
    
    if (nrow(d) == 0) next
    
    ylab_use <- if (tg %in% names(target_labels)) target_labels[[tg]] else tg
    out_file <- file.path(out_dir, paste0("series_", eu_sanitize_filename(tg), ".pdf"))
    
    grDevices::pdf(out_file, width = 10, height = 6)
    
    plot(
      d$Date,
      d[[tg]],
      type = "l",
      xlab = "Date",
      ylab = ylab_use,
      main = ylab_use,
      lwd = 2
    )
    
    grDevices::dev.off()
  }
  
  invisible(out_dir)
}

eu_plot_series_combined <- function(panel_data,
                                    targets,
                                    target_labels = NULL,
                                    tcodes = NULL,
                                    out_file,
                                    title_suffix = "",
                                    add_zero_line = FALSE,
                                    width = 12,
                                    panel_height = 3.6) {
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  if (is.null(target_labels)) {
    target_labels <- stats::setNames(targets, targets)
  }
  
  targets <- intersect(targets, names(panel_data))
  
  if (length(targets) == 0) {
    stop("None of the requested targets were found in panel_data.")
  }
  
  n_panels <- length(targets)
  ncol_plot <- 2
  nrow_plot <- ceiling(n_panels / ncol_plot)
  
  grDevices::pdf(out_file, width = width, height = panel_height * nrow_plot)
  
  old_par <- par(no.readonly = TRUE)
  
  on.exit({
    par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  
  par(
    mfrow = c(nrow_plot, ncol_plot),
    mar = c(3.2, 3.5, 2.8, 1),
    oma = c(0, 0, 2, 0)
  )
  
  for (tg in targets) {
    d <- panel_data[, c("Date", tg), drop = FALSE]
    d <- d[is.finite(d[[tg]]) & !is.na(d$Date), , drop = FALSE]
    
    base_label <- if (tg %in% names(target_labels)) target_labels[[tg]] else tg
    
    if (!is.null(tcodes) && tg %in% names(tcodes)) {
      main_use <- paste0(base_label, " (T-code ", tcodes[[tg]], ")")
    } else {
      main_use <- base_label
    }
    
    if (nrow(d) == 0) {
      plot.new()
      title(main = paste0(main_use, "\n(no data)"), cex.main = 0.95)
      next
    }
    
    plot(
      d$Date,
      d[[tg]],
      type = "l",
      xlab = "Date",
      ylab = "",
      main = main_use,
      lwd = 1.5
    )
    
    if (isTRUE(add_zero_line)) {
      abline(h = 0, lty = 2, col = "grey60")
    }
  }
  
  if (n_panels %% 2 == 1) {
    plot.new()
  }
  
  mtext(title_suffix, outer = TRUE, cex = 1.1, line = 0.5)
  
  invisible(out_file)
}

# ============================================================
# LATEX TABLE
# ============================================================

eu_save_fortin_table_grouped_tex <- function(all_results,
                                             targets,
                                             spec,
                                             target_labels = NULL,
                                             horizons = c(1, 2, 3, 4),
                                             file,
                                             caption = NULL,
                                             label = NULL,
                                             digits_ratio = 2,
                                             digits_rmse = 2,
                                             note = "Note: This table reports the ratio of the root mean squared predictive error (RMSE) relative to the benchmark AR,BIC model. A star (*) indicates significance at the 10\\% level in the Diebold--Mariano test. Green cells indicate the lowest RMSE ratio within each target-horizon column, including ties.",
                                             highlight_color = "green!15",
                                             placement = "H",
                                             font_size = "\\scriptsize",
                                             tabcolsep = "3pt",
                                             arraystretch = "0.95") {
  
  stopifnot(is.list(all_results))
  stopifnot(all(targets %in% names(all_results)))
  
  if (is.null(target_labels)) {
    target_labels <- targets
  }
  
  stopifnot(length(target_labels) == length(targets))
  
  model_internal <- spec$model_order_internal
  model_rows <- unname(spec$model_display_names[model_internal])
  model_rows[model_internal == spec$benchmark_model] <- "AR,BIC (RMSE)"
  
  n_targets <- length(targets)
  n_h <- length(horizons)
  n_cols <- n_targets * n_h
  
  fmt_num <- function(x, d) {
    if (length(x) == 0 || is.null(x)) return("")
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) return("")
    formatC(x, format = "f", digits = d)
  }
  
  body <- matrix("", nrow = length(model_internal), ncol = n_cols)
  rownames(body) <- model_rows
  
  ratio_vals <- matrix(NA_real_, nrow = length(model_internal), ncol = n_cols)
  rownames(ratio_vals) <- model_rows
  
  col_index <- 1
  
  for (tg in targets) {
    for (h in horizons) {
      hh <- paste0("h", h)
      
      body[model_rows[model_internal == spec$benchmark_model], col_index] <-
        fmt_num(all_results[[tg]][[hh]]$RMSE_bmk, digits_rmse)
      
      for (m in setdiff(model_internal, spec$benchmark_model)) {
        r <- all_results[[tg]][[hh]]$ratio[m]
        p <- all_results[[tg]][[hh]]$pvals[m]
        
        row_name <- spec$model_display_names[m]
        r1 <- suppressWarnings(as.numeric(r)[1])
        
        ratio_vals[row_name, col_index] <- if (length(r) == 0 || !is.finite(r1)) {
          NA_real_
        } else {
          r1
        }
        
        body[row_name, col_index] <- if (length(r) == 0 || !is.finite(r1)) {
          ""
        } else {
          paste0(fmt_num(r1, digits_ratio), eu_p_to_stars(p))
        }
      }
      
      col_index <- col_index + 1
    }
  }
  
  benchmark_row <- "AR,BIC (RMSE)"
  non_benchmark_rows <- setdiff(rownames(body), benchmark_row)
  
  for (j in seq_len(n_cols)) {
    x <- ratio_vals[non_benchmark_rows, j]
    
    if (all(is.na(x))) {
      if (nzchar(body[benchmark_row, j])) {
        body[benchmark_row, j] <- paste0("\\cellcolor{", highlight_color, "}", body[benchmark_row, j])
      }
      next
    }
    
    best_val <- min(x, na.rm = TRUE)
    
    if (is.finite(best_val) && best_val < 1) {
      best_rows <- non_benchmark_rows[!is.na(x) & x == best_val]
      
      for (rw in best_rows) {
        if (nzchar(body[rw, j])) {
          body[rw, j] <- paste0("\\cellcolor{", highlight_color, "}", body[rw, j])
        }
      }
    } else {
      if (nzchar(body[benchmark_row, j])) {
        body[benchmark_row, j] <- paste0("\\cellcolor{", highlight_color, "}", body[benchmark_row, j])
      }
    }
  }
  
  align <- paste0("p{3.7cm}", paste(rep("c", n_cols), collapse = ""))
  
  header1_parts <- c(" ")
  
  for (lab in target_labels) {
    header1_parts <- c(header1_parts, paste0("\\multicolumn{", n_h, "}{c}{", lab, "}"))
  }
  
  header1 <- paste(header1_parts, collapse = " & ")
  
  header2_parts <- c("Methods")
  
  for (i in seq_len(n_targets)) {
    header2_parts <- c(header2_parts, paste0("h=", horizons))
  }
  
  header2 <- paste(header2_parts, collapse = " & ")
  
  cmid <- c()
  start <- 2
  
  for (i in seq_len(n_targets)) {
    end <- start + n_h - 1
    cmid <- c(cmid, paste0("\\cmidrule(lr){", start, "-", end, "}"))
    start <- end + 1
  }
  
  cmid_line <- paste(cmid, collapse = "")
  
  lines <- c(
    paste0("\\begin{table}[", placement, "]"),
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", caption, "}") else NULL,
    if (!is.null(label)) paste0("\\label{", label, "}") else NULL,
    font_size,
    paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"),
    paste0("\\renewcommand{\\arraystretch}{", arraystretch, "}"),
    "\\begin{adjustbox}{max width=\\textwidth}",
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header1, " \\\\"),
    cmid_line,
    paste0(header2, " \\\\"),
    "\\midrule"
  )
  
  for (i in seq_len(nrow(body))) {
    row_line <- paste(c(rownames(body)[i], body[i, ]), collapse = " & ")
    lines <- c(lines, paste0(row_line, " \\\\"))
  }
  
  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{adjustbox}",
    if (!is.null(note)) paste0("\\vspace{0.4em}\n\\parbox{0.98\\textwidth}{\\footnotesize ", note, "}") else NULL,
    "\\end{table}"
  )
  
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, con = file)
  
  invisible(file)
}
