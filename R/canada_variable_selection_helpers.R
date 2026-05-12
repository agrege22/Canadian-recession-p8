# ============================================================
# CANADA VARIABLE-SELECTION HELPERS
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(pROC)
  library(ggplot2)
  library(tidyr)
  library(tibble)
  library(future)
  library(furrr)
  library(progressr)
  library(lubridate)
  library(stringr)
  
  library(OCMT)
  library(FarmSelect)
  library(MultipleTestingBoosting)
})

# ============================================================
# BASIC HELPERS
# ============================================================

safe_median <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  m <- suppressWarnings(median(x, na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  m
}

safe_auc <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  
  if (length(y) == 0 || length(unique(y)) < 2) return(NA_real_)
  
  tryCatch(
    as.numeric(pROC::roc(response = y, predictor = p, quiet = TRUE)$auc),
    error = function(e) NA_real_
  )
}

qps_score <- function(y, p) {
  y <- as.numeric(y)
  p <- pmin(pmax(as.numeric(p), 1e-8), 1 - 1e-8)
  2 * mean((p - y)^2)
}

lps_score <- function(y, p) {
  y <- as.numeric(y)
  p <- pmin(pmax(as.numeric(p), 1e-8), 1 - 1e-8)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

brier_score <- function(y, p) {
  y <- as.numeric(y)
  p <- pmin(pmax(as.numeric(p), 1e-8), 1 - 1e-8)
  mean((p - y)^2)
}

accuracy_score <- function(y, p, thr = 0.5) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  
  if (length(y) == 0) return(NA_real_)
  
  mean((p >= thr) == y)
}

parse_h_from_target <- function(target_col) {
  h <- suppressWarnings(as.integer(stringr::str_extract(target_col, "\\d+$")))
  if (is.na(h)) 1L else h
}

median_impute_train_test <- function(X_train, X_test) {
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test)) X_test <- as.matrix(X_test)
  
  if (ncol(X_train) == 0) {
    return(list(X_train = X_train, X_test = X_test))
  }
  
  meds <- apply(X_train, 2, safe_median)
  
  for (j in seq_len(ncol(X_train))) {
    X_train[is.na(X_train[, j]), j] <- meds[j]
    X_test[is.na(X_test[, j]), j] <- meds[j]
  }
  
  list(X_train = X_train, X_test = X_test)
}

drop_zero_var_from_train <- function(X_train, X_test) {
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test)) X_test <- as.matrix(X_test)
  
  if (ncol(X_train) == 0) {
    return(list(X_train = X_train, X_test = X_test))
  }
  
  sds <- apply(X_train, 2, sd, na.rm = TRUE)
  keep <- is.finite(sds) & sds > 0
  
  if (!any(keep)) {
    return(list(
      X_train = matrix(numeric(0), nrow = nrow(X_train), ncol = 0),
      X_test = matrix(numeric(0), nrow = nrow(X_test), ncol = 0)
    ))
  }
  
  list(
    X_train = X_train[, keep, drop = FALSE],
    X_test = X_test[, keep, drop = FALSE]
  )
}

clean_method_name <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

blank_method_plot <- function(title, label = "No data available") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 4) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::labs(title = title)
}

serialize_named_numeric <- function(x, digits = 10) {
  x <- as.numeric(x)
  
  if (length(x) == 0) return("")
  
  nm <- names(x)
  
  if (is.null(nm) || length(nm) != length(x)) {
    nm <- paste0("b", seq_along(x))
  }
  
  vals <- ifelse(
    is.finite(x),
    formatC(x, digits = digits, format = "fg", flag = "#"),
    NA_character_
  )
  
  paste0(nm, "=", vals, collapse = " | ")
}

equally_spaced_indices <- function(n, k) {
  if (n <= 0 || k <= 0) return(integer(0))
  if (n <= k) return(seq_len(n))
  sort(unique(round(seq(1, n, length.out = k))))
}

# ============================================================
# RECESSION SPAN HELPERS
# ============================================================

make_recession_spans <- function(dates, y) {
  dates <- as.Date(dates)
  y <- suppressWarnings(as.integer(y))
  
  ok <- !is.na(dates) & !is.na(y)
  dates <- dates[ok]
  y <- y[ok]
  
  if (length(dates) == 0 || !any(y == 1L)) {
    return(tibble::tibble(
      xmin = as.Date(character()),
      xmax = as.Date(character())
    ))
  }
  
  ord <- order(dates)
  dates <- dates[ord]
  y <- y[ord]
  
  step_days <- suppressWarnings(median(diff(as.numeric(dates)), na.rm = TRUE))
  if (!is.finite(step_days) || step_days <= 0) step_days <- 30
  
  run_id <- cumsum(c(1L, diff(y) != 0L))
  
  tibble::tibble(Date = dates, y = y, run_id = run_id) |>
    dplyr::group_by(.data$run_id, .data$y) |>
    dplyr::summarise(
      xmin = min(.data$Date),
      xmax = max(.data$Date) + lubridate::days(step_days),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$y == 1L) |>
    dplyr::select("xmin", "xmax")
}

make_recession_rects_from_dates <- function(date_vec, rec_spans) {
  date_tbl <- tibble::tibble(Date_target = as.Date(date_vec)) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$Date_target)
  
  if (nrow(date_tbl) == 0 || nrow(rec_spans) == 0) {
    return(tibble::tibble(xmin = numeric(0), xmax = numeric(0)))
  }
  
  in_rec <- vapply(
    date_tbl$Date_target,
    function(d) any(d >= rec_spans$xmin & d < rec_spans$xmax),
    logical(1)
  )
  
  date_tbl <- date_tbl |>
    dplyr::mutate(
      x_id = dplyr::row_number(),
      in_rec = in_rec
    )
  
  if (!any(date_tbl$in_rec)) {
    return(tibble::tibble(xmin = numeric(0), xmax = numeric(0)))
  }
  
  date_tbl |>
    dplyr::mutate(run_id = cumsum(c(1L, diff(.data$in_rec) != 0L))) |>
    dplyr::filter(.data$in_rec) |>
    dplyr::group_by(.data$run_id) |>
    dplyr::summarise(
      xmin = min(.data$x_id) - 0.5,
      xmax = max(.data$x_id) + 0.5,
      .groups = "drop"
    )
}

# ============================================================
# SELECTOR OUTPUT READERS
# ============================================================

get_sel_ocmt <- function(res, X) {
  if (is.null(res)) return(character(0))
  
  if (is.numeric(res)) {
    idx <- as.integer(res)
    idx <- idx[idx >= 1 & idx <= ncol(X)]
    return(colnames(X)[idx])
  }
  
  if (is.logical(res) && length(res) == ncol(X)) {
    return(colnames(X)[which(res)])
  }
  
  if (!is.null(res$selected)) {
    s <- res$selected
    
    if (is.character(s) && all(grepl("^\\d+$", s))) {
      s <- as.integer(s)
    }
    
    if (is.numeric(s)) {
      s <- s[s >= 1 & s <= ncol(X)]
      return(colnames(X)[s])
    }
    
    if (is.logical(s) && length(s) == ncol(X)) {
      return(colnames(X)[which(s)])
    }
    
    if (is.character(s)) {
      return(intersect(s, colnames(X)))
    }
  }
  
  if (!is.null(res$selected_names)) {
    return(intersect(as.character(res$selected_names), colnames(X)))
  }
  
  if (!is.null(res$ind) && is.logical(res$ind) && length(res$ind) == ncol(X)) {
    return(colnames(X)[which(res$ind)])
  }
  
  character(0)
}

get_sel_boost <- function(res, X) {
  if (is.null(res)) return(character(0))
  
  if (is.logical(res) && length(res) == ncol(X)) {
    return(colnames(X)[which(res)])
  }
  
  if (is.character(res)) {
    return(intersect(res, colnames(X)))
  }
  
  if (!is.null(res$selected)) {
    s <- res$selected
    
    if (is.logical(s) && length(s) == ncol(X)) {
      return(colnames(X)[which(s)])
    }
    
    if (is.character(s)) {
      return(intersect(s, colnames(X)))
    }
  }
  
  character(0)
}

# ============================================================
# SELECTION MODELS
# ============================================================

select_vars_one_method <- function(
    method,
    y_train,
    X_train,
    hyper,
    max.iter_farm = 1200,
    max_iter_ocmt = 300,
    maxit_bmt = 300
) {
  sel <- character(0)
  
  if (method == "OCMT") {
    res <- tryCatch(
      ocmt_sel(
        Y = y_train,
        X = X_train,
        pval = hyper,
        multi_stage = TRUE,
        max_iter = max_iter_ocmt,
        link = "logit",
        verbose = FALSE
      ),
      error = function(e) NULL
    )
    
    sel <- get_sel_ocmt(res, X_train)
  }
  
  if (method == "BMT") {
    res <- tryCatch(
      boosting_glm(
        y = y_train,
        X = X_train,
        pval = hyper,
        link = "logit",
        maxit = maxit_bmt
      ),
      error = function(e) NULL
    )
    
    sel <- get_sel_boost(res, X_train)
  }
  
  if (method == "FarmSelect") {
    stop("FarmSelect is handled separately through select_vars_farmselect().")
  }
  
  sel <- unique(intersect(sel, colnames(X_train)))
  
  list(
    selected = sel,
    n_selected = length(sel)
  )
}

select_vars_farmselect <- function(
    y_train,
    X_train,
    tau = 2,
    max.iter_farm = 1200,
    nfolds = 10L,
    eps = 1e-3,
    loss = "lasso",
    robust = FALSE,
    cv = TRUE,
    K.factors = NULL
) {
  y_train <- as.numeric(y_train)
  
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  
  ok_x <- if (ncol(X_train) == 0) {
    rep(TRUE, nrow(X_train))
  } else {
    apply(X_train, 1, function(r) all(is.finite(r)))
  }
  
  ok_y <- is.finite(y_train) & y_train %in% c(0, 1)
  ok <- ok_x & ok_y
  
  y2 <- y_train[ok]
  X2 <- X_train[ok, , drop = FALSE]
  
  if (length(y2) == 0 ||
      length(unique(y2)) < 2 ||
      ncol(X2) == 0 ||
      nrow(X2) <= 4 ||
      ncol(X2) <= 4) {
    return(list(
      selected = character(0),
      n_selected = 0L,
      fit_type = "mean_no_selection",
      lambda_min = NA_real_,
      nfolds_used = NA_integer_
    ))
  }
  
  n0 <- sum(y2 == 0)
  n1 <- sum(y2 == 1)
  nfolds_use <- min(as.integer(nfolds), n0, n1)
  
  if (!is.finite(nfolds_use) || nfolds_use < 2L) {
    return(list(
      selected = character(0),
      n_selected = 0L,
      fit_type = "mean_too_few_class_for_cv",
      lambda_min = NA_real_,
      nfolds_used = as.integer(nfolds_use)
    ))
  }
  
  fit <- tryCatch(
    farm.select(
      X = X2,
      Y = y2,
      loss = loss,
      robust = robust,
      cv = cv,
      tau = tau,
      lin.reg = FALSE,
      K.factors = K.factors,
      max.iter = max.iter_farm,
      nfolds = nfolds_use,
      eps = eps,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(list(
      selected = character(0),
      n_selected = 0L,
      fit_type = "mean_farmselect_fail",
      lambda_min = NA_real_,
      nfolds_used = as.integer(nfolds_use)
    ))
  }
  
  idx <- integer(0)
  
  if (!is.null(fit$beta.chosen)) {
    idx <- unique(as.integer(fit$beta.chosen))
    idx <- idx[is.finite(idx) & idx >= 1 & idx <= ncol(X2)]
  }
  
  selected_vars <- if (length(idx) > 0) colnames(X2)[idx] else character(0)
  
  lambda_min <- if (!is.null(fit$lambda.min) && length(fit$lambda.min) >= 1) {
    suppressWarnings(as.numeric(fit$lambda.min)[1])
  } else {
    NA_real_
  }
  
  list(
    selected = unique(selected_vars),
    n_selected = length(unique(selected_vars)),
    fit_type = if (length(selected_vars) == 0) {
      "mean_no_selection"
    } else {
      "logit_after_farmselect_selection"
    },
    lambda_min = lambda_min,
    nfolds_used = as.integer(nfolds_use)
  )
}

fit_predict_farmselect_binary <- function(
    y_train,
    X_train,
    X_test,
    tau = 2,
    max.iter_farm = 1200,
    nfolds = 10L,
    eps = 1e-3,
    loss = "lasso",
    robust = FALSE,
    cv = TRUE,
    K.factors = NULL
) {
  y_train <- as.numeric(y_train)
  
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test)) X_test <- as.matrix(X_test)
  
  p_bar <- mean(y_train, na.rm = TRUE)
  p_bar <- pmin(pmax(p_bar, 1e-8), 1 - 1e-8)
  
  fallback <- function(fit_type, nfolds_used = NA_integer_) {
    list(
      p = rep(p_bar, nrow(X_test)),
      selected = character(0),
      n_selected = 0L,
      fit_type = fit_type,
      lambda_min = NA_real_,
      nfolds_used = nfolds_used,
      n_factor_nonzero = NA_integer_,
      n_resid_nonzero = NA_integer_,
      intercept_hat = NA_real_,
      coef_full_string = "",
      factor_coef_string = "",
      resid_coef_string = "",
      selected_coef_string = "",
      model_form = NA_character_
    )
  }
  
  ok_x <- if (ncol(X_train) == 0) {
    rep(TRUE, nrow(X_train))
  } else {
    apply(X_train, 1, function(r) all(is.finite(r)))
  }
  
  ok_y <- is.finite(y_train) & y_train %in% c(0, 1)
  ok <- ok_x & ok_y
  
  y2 <- y_train[ok]
  X2 <- X_train[ok, , drop = FALSE]
  
  if (length(y2) == 0 ||
      length(unique(y2)) < 2 ||
      ncol(X2) == 0 ||
      nrow(X2) <= 4 ||
      ncol(X2) <= 4) {
    return(fallback("mean_no_selection"))
  }
  
  n0 <- sum(y2 == 0)
  n1 <- sum(y2 == 1)
  nfolds_use <- min(as.integer(nfolds), n0, n1)
  
  if (!is.finite(nfolds_use) || nfolds_use < 2L) {
    return(fallback("mean_too_few_class_for_cv", as.integer(nfolds_use)))
  }
  
  fit <- tryCatch(
    farm.select(
      X = X2,
      Y = y2,
      loss = loss,
      robust = robust,
      cv = cv,
      tau = tau,
      lin.reg = FALSE,
      K.factors = K.factors,
      max.iter = max.iter_farm,
      nfolds = nfolds_use,
      eps = eps,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(fallback("mean_farmselect_fail", as.integer(nfolds_use)))
  }
  
  p_hat <- tryCatch(
    as.numeric(predict(fit, newx = X_test, type = "response")),
    error = function(e) rep(NA_real_, nrow(X_test))
  )
  
  if (length(p_hat) != nrow(X_test) || any(!is.finite(p_hat))) {
    return(fallback("mean_farmselect_predict_fail", as.integer(nfolds_use)))
  }
  
  p_hat <- pmin(pmax(p_hat, 1e-8), 1 - 1e-8)
  
  idx <- integer(0)
  
  if (!is.null(fit$beta.chosen)) {
    idx <- unique(as.integer(fit$beta.chosen))
    idx <- idx[is.finite(idx) & idx >= 1 & idx <= ncol(X2)]
  }
  
  selected_vars <- if (length(idx) > 0) {
    colnames(X2)[idx]
  } else {
    character(0)
  }
  
  selected_vars <- unique(selected_vars)
  
  lambda_min <- if (!is.null(fit$lambda.min) && length(fit$lambda.min) >= 1) {
    suppressWarnings(as.numeric(fit$lambda.min)[1])
  } else {
    NA_real_
  }
  
  factor_coef <- fit$factor.coef
  if (is.null(factor_coef)) factor_coef <- numeric(0)
  factor_coef <- as.numeric(factor_coef)
  names(factor_coef) <- paste0("Factor", seq_along(factor_coef))
  
  selected_coef <- fit$coef.chosen
  if (is.null(selected_coef)) selected_coef <- numeric(0)
  selected_coef <- as.numeric(selected_coef)
  
  if (length(selected_coef) == length(selected_vars)) {
    names(selected_coef) <- selected_vars
  }
  
  n_factor_nonzero <- sum(is.finite(factor_coef) & abs(factor_coef) > 0)
  n_resid_nonzero <- length(selected_vars)
  
  coef_full_sparse <- c(
    "(Intercept)" = fit$intercept,
    factor_coef,
    selected_coef
  )
  
  fit_type <- if (n_resid_nonzero > 0) {
    "farmselect_logit_factors_plus_selected_residuals"
  } else if (n_factor_nonzero > 0) {
    "farmselect_logit_factor_only"
  } else {
    "farmselect_logit_intercept_only"
  }
  
  list(
    p = p_hat,
    selected = selected_vars,
    n_selected = length(selected_vars),
    fit_type = fit_type,
    lambda_min = lambda_min,
    nfolds_used = as.integer(nfolds_use),
    n_factor_nonzero = as.integer(n_factor_nonzero),
    n_resid_nonzero = as.integer(n_resid_nonzero),
    intercept_hat = as.numeric(fit$intercept),
    coef_full_string = serialize_named_numeric(coef_full_sparse),
    factor_coef_string = serialize_named_numeric(factor_coef),
    resid_coef_string = serialize_named_numeric(selected_coef),
    selected_coef_string = serialize_named_numeric(selected_coef),
    model_form = "FarmSelect logit: factors included unpenalized; idiosyncratic predictors penalized"
  )
}

# ============================================================
# POST-SELECTION LOGIT
# ============================================================

fit_predict_logit <- function(y_train, X_train, X_test, maxit_post_logit = 300) {
  y_train <- as.numeric(y_train)
  
  p_bar <- mean(y_train, na.rm = TRUE)
  p_bar <- pmin(pmax(p_bar, 1e-8), 1 - 1e-8)
  
  if (length(unique(y_train[is.finite(y_train)])) < 2) {
    return(rep(p_bar, nrow(X_test)))
  }
  
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test)) X_test <- as.matrix(X_test)
  
  if (ncol(X_train) == 0) {
    return(rep(p_bar, nrow(X_test)))
  }
  
  if (nrow(X_train) <= ncol(X_train)) {
    return(rep(p_bar, nrow(X_test)))
  }
  
  train_df <- as.data.frame(X_train, check.names = FALSE)
  test_df <- as.data.frame(X_test, check.names = FALSE)
  train_df$y <- y_train
  
  fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        y ~ .,
        data = train_df,
        family = stats::binomial(link = "logit"),
        control = stats::glm.control(maxit = maxit_post_logit)
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(rep(p_bar, nrow(X_test)))
  }
  
  p_hat <- tryCatch(
    suppressWarnings(stats::predict(fit, newdata = test_df, type = "response")),
    error = function(e) rep(NA_real_, nrow(X_test))
  )
  
  p_hat <- as.numeric(p_hat)
  
  if (length(p_hat) != nrow(X_test) || any(!is.finite(p_hat))) {
    return(rep(p_bar, nrow(X_test)))
  }
  
  pmin(pmax(p_hat, 1e-8), 1 - 1e-8)
}

# ============================================================
# PANEL PREPARATION
# ============================================================

prepare_binary_panel <- function(panel_df, panel_label = "panel") {
  if (ncol(panel_df) < 3) {
    stop(panel_label, ": panel must have at least Date, target, predictors.")
  }
  
  date_col <- names(panel_df)[1]
  y_col <- names(panel_df)[2]
  rec_cols <- grep("^recession", names(panel_df), value = TRUE)
  
  x_cols <- setdiff(
    names(panel_df),
    c(date_col, y_col, setdiff(rec_cols, y_col))
  )
  
  if (length(x_cols) == 0) {
    stop(panel_label, ": no predictors left.")
  }
  
  df <- panel_df |>
    dplyr::transmute(
      Date = as.Date(.data[[date_col]]),
      y = suppressWarnings(as.numeric(.data[[y_col]])),
      dplyr::across(dplyr::all_of(x_cols), ~ suppressWarnings(as.numeric(.x)))
    ) |>
    dplyr::arrange(.data$Date) |>
    dplyr::filter(!is.na(.data$y))
  
  list(
    df = df,
    x_cols = x_cols,
    y_col = y_col
  )
}

# ============================================================
# ONE FORECAST ORIGIN
# ============================================================

forecast_one_origin <- function(
    i,
    df,
    x_cols,
    methods,
    hyper_lookup,
    min_class_count,
    max.iter_farm,
    max_iter_ocmt,
    maxit_bmt,
    maxit_post_logit
) {
  train_df <- df[1:i, , drop = FALSE]
  test_df <- df[(i + 1), , drop = FALSE]
  
  y_train <- train_df$y
  class_tab <- table(base::factor(y_train, levels = c(0, 1)))
  
  if (min(class_tab) < min_class_count) return(NULL)
  
  X_train <- as.matrix(train_df[, x_cols, drop = FALSE])
  X_test <- as.matrix(test_df[, x_cols, drop = FALSE])
  
  imp <- median_impute_train_test(X_train, X_test)
  X_train <- imp$X_train
  X_test <- imp$X_test
  
  dz <- drop_zero_var_from_train(X_train, X_test)
  X_train <- dz$X_train
  X_test <- dz$X_test
  
  if (ncol(X_train) == 0) {
    p_null <- mean(y_train, na.rm = TRUE)
    
    return(
      purrr::map_dfr(methods, function(m) {
        tibble::tibble(
          Date = test_df$Date,
          y = test_df$y,
          p = p_null,
          method = m,
          hyper_used = hyper_lookup[[m]][as.character(i)],
          n_selected = 0L,
          fit_type = "mean_no_columns",
          selected_vars = "",
          model_form = NA_character_,
          nfolds_used = NA_integer_,
          n_factor_nonzero = NA_integer_,
          n_resid_nonzero = NA_integer_,
          intercept_hat = NA_real_,
          lambda_min = NA_real_,
          coef_full_string = "",
          factor_coef_string = "",
          resid_coef_string = "",
          selected_coef_string = ""
        )
      })
    )
  }
  
  purrr::map_dfr(methods, function(m) {
    hyper_now <- hyper_lookup[[m]][as.character(i)]
    
    if (m == "FarmSelect") {
      fs_obj <- fit_predict_farmselect_binary(
        y_train = y_train,
        X_train = X_train,
        X_test = X_test,
        tau = hyper_now,
        max.iter_farm = max.iter_farm,
        nfolds = 10L,
        eps = 1e-3,
        loss = "lasso",
        robust = FALSE,
        cv = TRUE,
        K.factors = NULL
      )
      
      return(
        tibble::tibble(
          Date = test_df$Date,
          y = test_df$y,
          p = as.numeric(fs_obj$p[1]),
          method = m,
          hyper_used = hyper_now,
          n_selected = as.integer(fs_obj$n_selected),
          fit_type = fs_obj$fit_type,
          selected_vars = paste(fs_obj$selected, collapse = " | "),
          model_form = fs_obj$model_form,
          nfolds_used = fs_obj$nfolds_used,
          n_factor_nonzero = fs_obj$n_factor_nonzero,
          n_resid_nonzero = fs_obj$n_resid_nonzero,
          intercept_hat = fs_obj$intercept_hat,
          lambda_min = fs_obj$lambda_min,
          coef_full_string = fs_obj$coef_full_string,
          factor_coef_string = fs_obj$factor_coef_string,
          resid_coef_string = fs_obj$resid_coef_string,
          selected_coef_string = fs_obj$selected_coef_string
        )
      )
    }
    
    sel_obj <- select_vars_one_method(
      method = m,
      y_train = y_train,
      X_train = X_train,
      hyper = hyper_now,
      max.iter_farm = max.iter_farm,
      max_iter_ocmt = max_iter_ocmt,
      maxit_bmt = maxit_bmt
    )
    
    if (length(sel_obj$selected) == 0) {
      p_hat <- mean(y_train, na.rm = TRUE)
      fit_type <- "mean_no_selection"
    } else {
      p_hat <- fit_predict_logit(
        y_train = y_train,
        X_train = X_train[, sel_obj$selected, drop = FALSE],
        X_test = X_test[, sel_obj$selected, drop = FALSE],
        maxit_post_logit = maxit_post_logit
      )
      
      fit_type <- "logit_after_selection"
    }
    
    tibble::tibble(
      Date = test_df$Date,
      y = test_df$y,
      p = as.numeric(p_hat[1]),
      method = m,
      hyper_used = hyper_now,
      n_selected = as.integer(sel_obj$n_selected),
      fit_type = fit_type,
      selected_vars = paste(sel_obj$selected, collapse = " | "),
      model_form = NA_character_,
      nfolds_used = NA_integer_,
      n_factor_nonzero = NA_integer_,
      n_resid_nonzero = NA_integer_,
      intercept_hat = NA_real_,
      lambda_min = NA_real_,
      coef_full_string = "",
      factor_coef_string = "",
      resid_coef_string = "",
      selected_coef_string = ""
    )
  })
}

# ============================================================
# MAIN VARIABLE-SELECTION COMPARISON FUNCTION
# ============================================================

compare_methods_expanding_fixed <- function(
    panel_df,
    panel_label = "panel",
    initial_train = 84,
    min_class_count = 3,
    max.iter_farm = 1200,
    max_iter_ocmt = 300,
    maxit_bmt = 300,
    maxit_post_logit = 300,
    threshold_class = 0.5,
    fixed_hyper,
    use_parallel = TRUE,
    n_workers = 10,
    show_progress = TRUE
) {
  prep <- prepare_binary_panel(panel_df, panel_label = panel_label)
  
  df <- prep$df
  x_cols <- prep$x_cols
  y_col <- prep$y_col
  h <- parse_h_from_target(y_col)
  
  methods <- c("OCMT", "BMT", "FarmSelect")
  origins <- seq.int(initial_train, nrow(df) - 1)
  
  if (length(origins) == 0) {
    stop("Not enough rows for chosen initial_train.")
  }
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  workers_use <- min(
    n_workers,
    max(1L, future::availableCores() - 1L),
    length(origins)
  )
  
  if (isTRUE(use_parallel)) {
    future::plan(future::multisession, workers = workers_use)
    message("Running with ", workers_use, " worker(s).")
  } else {
    future::plan(future::sequential)
    message("Running sequentially.")
  }
  
  hyper_lookup <- list()
  
  for (m in methods) {
    hyp <- rep(unname(fixed_hyper[[m]]), length(origins))
    names(hyp) <- as.character(origins)
    hyper_lookup[[m]] <- hyp
  }
  
  fixed_tbl <- tibble::tibble(
    retune_origin = NA_integer_,
    retune_date = as.Date(NA),
    method = methods,
    best_hyper = unlist(fixed_hyper[methods]),
    tune_qps = NA_real_,
    tune_lps = NA_real_,
    tune_n_selected = NA_real_,
    status = "fixed_no_retune"
  )
  
  if (isTRUE(show_progress)) {
    pred_df <- progressr::with_progress({
      p <- progressr::progressor(steps = length(origins))
      
      if (isTRUE(use_parallel)) {
        furrr::future_map_dfr(
          origins,
          function(i) {
            out <- forecast_one_origin(
              i = i,
              df = df,
              x_cols = x_cols,
              methods = methods,
              hyper_lookup = hyper_lookup,
              min_class_count = min_class_count,
              max.iter_farm = max.iter_farm,
              max_iter_ocmt = max_iter_ocmt,
              maxit_bmt = maxit_bmt,
              maxit_post_logit = maxit_post_logit
            )
            
            p()
            out
          },
          .options = furrr::furrr_options(
            seed = TRUE,
            packages = c(
              "dplyr",
              "purrr",
              "pROC",
              "tibble",
              "OCMT",
              "FarmSelect",
              "MultipleTestingBoosting"
            )
          )
        )
      } else {
        purrr::map_dfr(
          origins,
          function(i) {
            out <- forecast_one_origin(
              i = i,
              df = df,
              x_cols = x_cols,
              methods = methods,
              hyper_lookup = hyper_lookup,
              min_class_count = min_class_count,
              max.iter_farm = max.iter_farm,
              max_iter_ocmt = max_iter_ocmt,
              maxit_bmt = maxit_bmt,
              maxit_post_logit = maxit_post_logit
            )
            
            p()
            out
          }
        )
      }
    })
  } else {
    if (isTRUE(use_parallel)) {
      pred_df <- furrr::future_map_dfr(
        origins,
        function(i) {
          forecast_one_origin(
            i = i,
            df = df,
            x_cols = x_cols,
            methods = methods,
            hyper_lookup = hyper_lookup,
            min_class_count = min_class_count,
            max.iter_farm = max.iter_farm,
            max_iter_ocmt = max_iter_ocmt,
            maxit_bmt = maxit_bmt,
            maxit_post_logit = maxit_post_logit
          )
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          packages = c(
            "dplyr",
            "purrr",
            "pROC",
            "tibble",
            "OCMT",
            "FarmSelect",
            "MultipleTestingBoosting"
          )
        )
      )
    } else {
      pred_df <- purrr::map_dfr(
        origins,
        function(i) {
          forecast_one_origin(
            i = i,
            df = df,
            x_cols = x_cols,
            methods = methods,
            hyper_lookup = hyper_lookup,
            min_class_count = min_class_count,
            max.iter_farm = max.iter_farm,
            max_iter_ocmt = max_iter_ocmt,
            maxit_bmt = maxit_bmt,
            maxit_post_logit = maxit_post_logit
          )
        }
      )
    }
  }
  
  if (is.null(pred_df) || nrow(pred_df) == 0) {
    stop("No out-of-sample predictions were produced.")
  }
  
  metrics <- pred_df |>
    dplyr::group_by(.data$method) |>
    dplyr::summarise(
      panel = panel_label,
      target = y_col,
      h = h,
      n_oos = dplyr::n(),
      qps = qps_score(.data$y, .data$p),
      lps = lps_score(.data$y, .data$p),
      avg_n_selected = mean(.data$n_selected),
      pct_zero_selected = mean(.data$n_selected == 0),
      pct_mean_fallback = mean(grepl("mean", .data$fit_type, fixed = TRUE)),
      auc = safe_auc(.data$y, .data$p),
      brier = brier_score(.data$y, .data$p),
      accuracy_05 = accuracy_score(.data$y, .data$p, thr = threshold_class),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$qps, .data$lps)
  
  list(
    pred_df = pred_df,
    metrics = metrics,
    tuning_tbl = fixed_tbl,
    target_col = y_col,
    h = h,
    retune_origins = NA_integer_,
    train_end_date = df$Date[initial_train],
    first_oos_date = df$Date[initial_train + 1],
    train_end_target_date = df$Date[initial_train] %m+% lubridate::months(h),
    first_oos_target_date = df$Date[initial_train + 1] %m+% lubridate::months(h)
  )
}

# ============================================================
# TABLE HELPERS
# ============================================================

count_selected_variables <- function(pred_df) {
  pred_df_methods <- pred_df |>
    dplyr::rename(methods = "method")
  
  denom_tbl <- pred_df_methods |>
    dplyr::distinct(.data$methods, .data$Date) |>
    dplyr::count(.data$methods, name = "n_oos_method")
  
  pred_df_methods |>
    dplyr::mutate(sel_string = .data$selected_vars) |>
    dplyr::filter(!is.na(.data$sel_string), nzchar(.data$sel_string)) |>
    tidyr::separate_rows(.data$sel_string, sep = "\\s*\\|\\s*") |>
    dplyr::filter(nzchar(.data$sel_string)) |>
    dplyr::group_by(.data$methods, variable = .data$sel_string) |>
    dplyr::summarise(times_selected = dplyr::n(), .groups = "drop") |>
    dplyr::left_join(denom_tbl, by = "methods") |>
    dplyr::mutate(pct_oos = .data$times_selected / .data$n_oos_method) |>
    dplyr::arrange(.data$methods, dplyr::desc(.data$times_selected), .data$variable)
}

build_farmselect_coef_long <- function(pred_df) {
  out <- pred_df |>
    dplyr::filter(.data$method == "FarmSelect") |>
    dplyr::transmute(
      Date = as.Date(.data$Date),
      y = .data$y,
      p = .data$p,
      fit_type = .data$fit_type,
      lambda_min = .data$lambda_min,
      coef_string = .data$coef_full_string
    ) |>
    dplyr::filter(!is.na(.data$lambda_min) | nzchar(.data$coef_string))
  
  if (nrow(out) == 0) {
    return(tibble::tibble(
      Date = as.Date(character()),
      y = numeric(0),
      p = numeric(0),
      fit_type = character(0),
      lambda_min = numeric(0),
      coefficient = character(0),
      estimate = numeric(0)
    ))
  }
  
  out |>
    tidyr::separate_rows(.data$coef_string, sep = "\\s*\\|\\s*") |>
    dplyr::filter(nzchar(.data$coef_string)) |>
    tidyr::separate(
      .data$coef_string,
      into = c("coefficient", "estimate"),
      sep = "=",
      extra = "merge",
      fill = "right"
    ) |>
    dplyr::mutate(estimate = suppressWarnings(as.numeric(.data$estimate)))
}

build_selection_long <- function(pred_df, target_col) {
  h <- parse_h_from_target(target_col)
  
  pred_df |>
    dplyr::transmute(
      Date = as.Date(.data$Date),
      Date_target = .data$Date %m+% lubridate::months(h),
      methods = .data$method,
      sel_string = .data$selected_vars
    ) |>
    dplyr::filter(!is.na(.data$sel_string), nzchar(.data$sel_string)) |>
    tidyr::separate_rows(.data$sel_string, sep = "\\s*\\|\\s*") |>
    dplyr::transmute(
      Date = .data$Date,
      Date_target = .data$Date_target,
      methods = .data$methods,
      variable = .data$sel_string
    ) |>
    dplyr::filter(nzchar(.data$variable)) |>
    dplyr::distinct(.data$methods, .data$Date, .data$Date_target, .data$variable)
}

build_selection_snapshot_tables <- function(pred_df, target_col, n_snapshots = 10) {
  h <- parse_h_from_target(target_col)
  
  all_dates <- pred_df |>
    dplyr::distinct(.data$Date) |>
    dplyr::arrange(.data$Date) |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  idx <- equally_spaced_indices(nrow(all_dates), n_snapshots)
  
  snap_dates <- all_dates[idx, , drop = FALSE] |>
    dplyr::mutate(
      snapshot_no = seq_len(dplyr::n()),
      snapshot_label = paste0(
        "S",
        .data$snapshot_no,
        " (",
        format(.data$Date_target, "%Y-%m-%d"),
        ")"
      )
    )
  
  summary_tbl <- pred_df |>
    dplyr::select("Date", "method", "n_selected", "selected_vars") |>
    dplyr::rename(methods = "method") |>
    dplyr::inner_join(snap_dates, by = "Date") |>
    dplyr::arrange(.data$methods, .data$snapshot_no) |>
    dplyr::select(
      "snapshot_no",
      "snapshot_label",
      "Date",
      "Date_target",
      "methods",
      "n_selected",
      "selected_vars"
    )
  
  long_tbl <- summary_tbl |>
    dplyr::transmute(
      snapshot_no = .data$snapshot_no,
      snapshot_label = .data$snapshot_label,
      Date = .data$Date,
      Date_target = .data$Date_target,
      methods = .data$methods,
      sel_string = .data$selected_vars
    ) |>
    dplyr::filter(!is.na(.data$sel_string), nzchar(.data$sel_string)) |>
    tidyr::separate_rows(.data$sel_string, sep = "\\s*\\|\\s*") |>
    dplyr::transmute(
      snapshot_no = .data$snapshot_no,
      snapshot_label = .data$snapshot_label,
      Date = .data$Date,
      Date_target = .data$Date_target,
      methods = .data$methods,
      variable = .data$sel_string
    ) |>
    dplyr::filter(nzchar(.data$variable)) |>
    dplyr::distinct()
  
  list(
    summary_tbl = summary_tbl,
    long_tbl = long_tbl,
    snapshot_dates = snap_dates
  )
}

make_top_selection_heatmap_data <- function(
    pred_df,
    target_col,
    top_n = 30,
    exclude_always_selected_for_ocmt = TRUE
) {
  h <- parse_h_from_target(target_col)
  
  pred_df_methods <- pred_df |>
    dplyr::rename(methods = "method")
  
  sel_long <- build_selection_long(
    pred_df = pred_df,
    target_col = target_col
  )
  
  denom_tbl <- pred_df_methods |>
    dplyr::distinct(.data$methods, .data$Date) |>
    dplyr::count(.data$methods, name = "n_oos_method")
  
  count_tbl <- sel_long |>
    dplyr::count(.data$methods, .data$variable, name = "times_selected") |>
    dplyr::left_join(denom_tbl, by = "methods") |>
    dplyr::mutate(pct_oos = .data$times_selected / .data$n_oos_method)
  
  ocmt_always_tbl <- count_tbl |>
    dplyr::filter(.data$methods == "OCMT", .data$times_selected == .data$n_oos_method) |>
    dplyr::arrange(dplyr::desc(.data$times_selected), .data$variable)
  
  count_tbl_for_top <- count_tbl
  
  if (isTRUE(exclude_always_selected_for_ocmt)) {
    count_tbl_for_top <- count_tbl_for_top |>
      dplyr::filter(!(.data$methods == "OCMT" & .data$times_selected == .data$n_oos_method))
  }
  
  top_tbl <- count_tbl_for_top |>
    dplyr::group_by(.data$methods) |>
    dplyr::arrange(dplyr::desc(.data$times_selected), .data$variable, .by_group = TRUE) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::ungroup()
  
  all_dates <- pred_df_methods |>
    dplyr::distinct(.data$Date) |>
    dplyr::arrange(.data$Date) |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  heat_tbl <- tidyr::crossing(
    top_tbl |>
      dplyr::select("methods", "variable", "times_selected", "n_oos_method", "pct_oos"),
    all_dates |>
      dplyr::select("Date", "Date_target")
  ) |>
    dplyr::left_join(
      sel_long |>
        dplyr::mutate(selected = 1L),
      by = c("methods", "variable", "Date", "Date_target")
    ) |>
    dplyr::mutate(selected = dplyr::if_else(is.na(.data$selected), 0L, .data$selected)) |>
    dplyr::arrange(.data$methods, dplyr::desc(.data$times_selected), .data$variable, .data$Date_target)
  
  list(
    top_tbl = top_tbl,
    heat_tbl = heat_tbl,
    sel_long = sel_long,
    count_tbl = count_tbl,
    ocmt_always_tbl = ocmt_always_tbl
  )
}

# ============================================================
# PLOTS: SELECTION COUNTS
# ============================================================

plot_selected_counts_separate <- function(
    pred_df,
    target_col,
    method_levels = c("OCMT", "BMT", "FarmSelect")
) {
  h <- parse_h_from_target(target_col)
  
  df2 <- pred_df |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  truth_df <- df2 |>
    dplyr::distinct(.data$Date_target, .data$y) |>
    dplyr::arrange(.data$Date_target)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  out <- vector("list", length(method_levels))
  names(out) <- method_levels
  
  for (m in method_levels) {
    d <- df2 |>
      dplyr::filter(.data$method == m)
    
    if (nrow(d) == 0) {
      out[[m]] <- blank_method_plot(
        title = paste0(m, ": # selected variables through time"),
        label = "No out-of-sample predictions"
      )
    } else {
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$Date_target, .data$n_selected))
      
      if (nrow(rec_spans) > 0) {
        p <- p +
          ggplot2::geom_rect(
            data = rec_spans,
            ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey70",
            alpha = 0.30,
            color = NA
          )
      }
      
      out[[m]] <- p +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 1.2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste0(m, ": # selected variables through time"),
          x = NULL,
          y = "# selected"
        )
    }
  }
  
  out
}

plot_selected_counts_together <- function(pred_df, target_col) {
  h <- parse_h_from_target(target_col)
  
  df2 <- pred_df |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  truth_df <- df2 |>
    dplyr::distinct(.data$Date_target, .data$y) |>
    dplyr::arrange(.data$Date_target)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  p <- ggplot2::ggplot(
    df2,
    ggplot2::aes(.data$Date_target, .data$n_selected, color = .data$method, linetype = .data$method)
  )
  
  if (nrow(rec_spans) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = rec_spans,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "grey70",
        alpha = 0.30,
        color = NA
      )
  }
  
  p +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
    ggplot2::geom_point(size = 1.0, alpha = 0.9) +
    ggplot2::scale_linetype_manual(
      values = c(
        "OCMT" = "dotted",
        "BMT" = "solid",
        "FarmSelect" = "longdash"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "# selected variables through time: all methods",
      x = NULL,
      y = "# selected",
      color = NULL,
      linetype = NULL
    )
}

# ============================================================
# PLOTS: TOP VARIABLES
# ============================================================

plot_top_selected_variables <- function(
    sel_count_tbl,
    top_n = 20,
    method_levels = c("OCMT", "BMT", "FarmSelect")
) {
  out <- vector("list", length(method_levels))
  names(out) <- method_levels
  
  for (m in method_levels) {
    d <- sel_count_tbl |>
      dplyr::filter(.data$methods == m) |>
      dplyr::arrange(dplyr::desc(.data$times_selected), .data$variable) |>
      dplyr::slice_head(n = top_n)
    
    if (nrow(d) == 0) {
      out[[m]] <- blank_method_plot(
        title = paste0(m, ": top selected variables"),
        label = "No variables selected"
      )
    } else {
      d <- d |>
        dplyr::mutate(variable = reorder(.data$variable, .data$times_selected))
      
      out[[m]] <- ggplot2::ggplot(d, ggplot2::aes(.data$times_selected, .data$variable)) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste0(m, ": top selected variables"),
          x = "Times selected",
          y = NULL
        )
    }
  }
  
  out
}

plot_top_selection_heatmaps <- function(
    heat_tbl,
    top_tbl,
    pred_df,
    target_col,
    method_levels = c("OCMT", "BMT", "FarmSelect")
) {
  h <- parse_h_from_target(target_col)
  
  truth_df <- pred_df |>
    dplyr::distinct(.data$Date, .data$y) |>
    dplyr::arrange(.data$Date) |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h)) |>
    dplyr::distinct(.data$Date_target, .data$y)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  out <- vector("list", length(method_levels))
  names(out) <- method_levels
  
  for (m in method_levels) {
    d <- heat_tbl |>
      dplyr::filter(.data$methods == m)
    
    top_m <- top_tbl |>
      dplyr::filter(.data$methods == m) |>
      dplyr::arrange(dplyr::desc(.data$times_selected), .data$variable)
    
    if (nrow(d) == 0 || nrow(top_m) == 0) {
      out[[m]] <- blank_method_plot(
        title = paste0(m, ": top 40 selected variables heatmap"),
        label = "No variables selected"
      )
    } else {
      var_levels <- rev(top_m$variable)
      
      date_map <- d |>
        dplyr::distinct(.data$Date_target) |>
        dplyr::arrange(.data$Date_target) |>
        dplyr::mutate(x_id = dplyr::row_number())
      
      lab_idx <- unique(round(seq(1, nrow(date_map), length.out = min(8, nrow(date_map)))))
      
      rec_rects <- make_recession_rects_from_dates(
        date_vec = date_map$Date_target,
        rec_spans = rec_spans
      )
      
      d2 <- d |>
        dplyr::left_join(date_map, by = "Date_target") |>
        dplyr::mutate(
          variable = base::factor(.data$variable, levels = var_levels),
          selected_lab = base::factor(
            dplyr::if_else(.data$selected == 1L, "Yes", "No"),
            levels = c("No", "Yes")
          )
        )
      
      p <- ggplot2::ggplot(d2, ggplot2::aes(x = .data$x_id, y = .data$variable))
      
      if (nrow(rec_rects) > 0) {
        p <- p +
          ggplot2::geom_rect(
            data = rec_rects,
            ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey30",
            alpha = 0.20,
            color = NA
          )
      }
      
      out[[m]] <- p +
        ggplot2::geom_tile(
          ggplot2::aes(fill = .data$selected_lab),
          width = 1,
          height = 0.95
        ) +
        ggplot2::scale_fill_manual(
          values = c("No" = "white", "Yes" = "steelblue"),
          breaks = c("Yes"),
          labels = c("Variable selected"),
          name = NULL,
          drop = FALSE
        ) +
        ggplot2::scale_x_continuous(
          breaks = date_map$x_id[lab_idx],
          labels = format(date_map$Date_target[lab_idx], "%Y"),
          expand = c(0, 0)
        ) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 8),
          legend.position = "bottom",
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          plot.margin = ggplot2::margin(10, 15, 10, 10)
        ) +
        ggplot2::labs(
          title = paste0(m, ": top 40 selected variables heatmap"),
          x = NULL,
          y = NULL
        )
    }
  }
  
  out
}

# ============================================================
# PLOTS: PROBABILITIES
# ============================================================

plot_probs_separate <- function(
    pred_df,
    target_col,
    method_levels = c("OCMT", "BMT", "FarmSelect")
) {
  h <- parse_h_from_target(target_col)
  
  df2 <- pred_df |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  truth_df <- df2 |>
    dplyr::distinct(.data$Date_target, .data$y) |>
    dplyr::arrange(.data$Date_target)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  out <- vector("list", length(method_levels))
  names(out) <- method_levels
  
  for (m in method_levels) {
    d <- df2 |>
      dplyr::filter(.data$method == m)
    
    if (nrow(d) == 0) {
      out[[m]] <- blank_method_plot(
        title = paste0(m, ": OOS recession probabilities"),
        label = "No out-of-sample predictions"
      )
    } else {
      p <- ggplot2::ggplot()
      
      if (nrow(rec_spans) > 0) {
        p <- p +
          ggplot2::geom_rect(
            data = rec_spans,
            ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey70",
            alpha = 0.30,
            color = NA
          )
      }
      
      out[[m]] <- p +
        ggplot2::geom_step(
          data = truth_df,
          ggplot2::aes(.data$Date_target, .data$y),
          color = "black",
          linewidth = 1.0,
          alpha = 0.95
        ) +
        ggplot2::geom_line(
          data = d,
          ggplot2::aes(.data$Date_target, .data$p),
          color = "darkblue",
          linewidth = 0.9,
          alpha = 0.9
        ) +
        ggplot2::coord_cartesian(ylim = c(0, 1)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste0(m, ": OOS recession probabilities"),
          x = NULL,
          y = "Predicted probability"
        )
    }
  }
  
  out
}

plot_probs_together <- function(pred_df, target_col) {
  h <- parse_h_from_target(target_col)
  
  df2 <- pred_df |>
    dplyr::mutate(Date_target = .data$Date %m+% lubridate::months(h))
  
  truth_df <- df2 |>
    dplyr::distinct(.data$Date_target, .data$y) |>
    dplyr::arrange(.data$Date_target)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  p <- ggplot2::ggplot()
  
  if (nrow(rec_spans) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = rec_spans,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "grey70",
        alpha = 0.30,
        color = NA
      )
  }
  
  p +
    ggplot2::geom_step(
      data = truth_df,
      ggplot2::aes(.data$Date_target, .data$y),
      color = "black",
      linewidth = 1.05,
      alpha = 0.95
    ) +
    ggplot2::geom_line(
      data = df2,
      ggplot2::aes(.data$Date_target, .data$p, color = .data$method, linetype = .data$method),
      linewidth = 0.75,
      alpha = 0.9
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "OCMT" = "dotted",
        "BMT" = "solid",
        "FarmSelect" = "longdash"
      )
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "OOS recession probabilities: all methods",
      x = NULL,
      y = "Predicted probability",
      color = NULL,
      linetype = NULL
    )
}

plot_probs_faceted <- function(
    pred_df,
    target_col,
    method_levels = c("OCMT", "BMT", "FarmSelect")
) {
  h <- parse_h_from_target(target_col)
  
  df2 <- pred_df |>
    dplyr::mutate(
      Date_target = .data$Date %m+% lubridate::months(h),
      method = base::factor(.data$method, levels = method_levels)
    )
  
  truth_df <- df2 |>
    dplyr::distinct(.data$Date_target, .data$y) |>
    dplyr::arrange(.data$Date_target)
  
  rec_spans <- make_recession_spans(truth_df$Date_target, truth_df$y)
  
  p <- ggplot2::ggplot(df2, ggplot2::aes(.data$Date_target, .data$p))
  
  if (nrow(rec_spans) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = rec_spans,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = "grey60",
        alpha = 0.30,
        color = NA
      )
  }
  
  p +
    ggplot2::geom_line(color = "darkblue", linewidth = 0.85, alpha = 0.95) +
    ggplot2::facet_grid(method ~ .) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      strip.text.y = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "OOS recession probabilities by method",
      x = NULL,
      y = "Predicted probability"
    )
}

# ============================================================
# EXPORT HELPERS
# ============================================================

get_desktop_path <- function() {
  p1 <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
  p2 <- path.expand("~/Desktop")
  
  if (dir.exists(p1)) return(p1)
  if (dir.exists(p2)) return(p2)
  
  stop("Could not find Desktop path. Please set it manually.")
}

save_plot_pdf <- function(plot_obj, filename, width = 12, height = 8, units = "in") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  
  dev <- if (capabilities("cairo")) grDevices::cairo_pdf else "pdf"
  
  ggplot2::ggsave(
    filename = filename,
    plot = plot_obj,
    device = dev,
    width = width,
    height = height,
    units = units
  )
  
  message("Saved: ", normalizePath(filename, winslash = "/", mustWork = FALSE))
}