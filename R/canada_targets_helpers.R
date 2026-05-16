# ============================================================
# CANADA CONTINUOUS-TARGET HELPER FUNCTIONS
# ============================================================

# ============================================================
# MODEL ORDER / LABELS
# ============================================================

benchmark_model <- "AR,BIC"
fallback_lambda_default <- 1e-4

normalize_pval_grid <- function(pvals) {
  pvals <- as.numeric(pvals)
  labs <- names(pvals)
  
  if (is.null(labs) || any(!nzchar(labs))) {
    labs <- as.character(round(100 * pvals))
  }
  
  stats::setNames(pvals, labs)
}

ocmt_pval_grid <- normalize_pval_grid(c("5" = 0.05))
bmt_pval_grid  <- normalize_pval_grid(c("5" = 0.05))

ocmt_model_names <- paste0("OCMT_", names(ocmt_pval_grid))
bmt_model_names  <- paste0("BMT_", names(bmt_pval_grid))

combination_model_names <- "AVG"

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
  combination_model_names
)

model_display_names <- c(
  "AR,BIC"                       = "AR,BIC",
  "ARDI,BIC"                     = "ARDI,BIC",
  "Ridge-X"                      = "Ridge-X",
  "Lasso-X"                      = "Lasso-X",
  "Elastic-Net-X"                = "Elastic-Net-X",
  "Adaptive-Lasso-X"             = "Adaptive-Lasso-X",
  "OCMT_5"                       = "OCMT",
  "FarmSelect"                   = "FarmSelect",
  "BMT_5"                        = "BMT",
  "RF-X"                         = "RF-X",
  "ARDI,Ridge"                   = "ARDI,Ridge",
  "ARDI,Lasso"                   = "ARDI,Lasso",
  "ARDI,Elastic-Net"             = "ARDI,Elastic-Net",
  "ARDI,Adaptive-Lasso"          = "ARDI,Adaptive-Lasso",
  "RFARDI"                       = "RFARDI",
  "T-CSR"                        = "T-CSR",
  "Chronos-2-uni"                = "Chronos-2-uni",
  "Chronos-2-multi"              = "Chronos-2-multi",
  "Chronos-2-covariate-informed" = "Chronos-2-covariate-informed",
  "AVG"                          = "AVG"
)

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

combo_candidate_models <- setdiff(model_order_internal, combination_model_names)

empty_fallback_counter <- function() {
  stats::setNames(integer(length(fallback_model_names)), fallback_model_names)
}

empty_tcsr_counter <- function() {
  c(
    tcsr_benchmark_fallbacks = 0L,
    tcsr_total_forecasts = 0L
  )
}

# ============================================================
# CONTROL OBJECTS
# ============================================================

make_fortin_controls <- function(
    n_workers = 15L,
    oos_start = as.Date("2005-01-01"),
    oos_end = as.Date("2021-12-01"),
    horizons = c(1, 3, 6, 12),
    py = 5,
    px = 5,
    K_fixed = 6,
    retune_every_months = 12,
    træningsår = 10 * 12,
    glmnet_nlambda = 240,
    elastic_alpha_grid = c(0.02, 0.08, 0.15, 0.25, 0.35, 0.5, 0.65, 0.75, 0.85, 0.92, 0.98),
    elastic_fallback_alpha = 0.5,
    fallback_lambda = fallback_lambda_default,
    adalasso_eps = 1e-6,
    ocmt_pvals = ocmt_pval_grid,
    ocmt_delta1 = 1,
    ocmt_delta2 = 1,
    ocmt_multi_stage = TRUE,
    ocmt_max_iter = 300,
    ocmt_kappa_const = 0.5,
    bmt_pvals = bmt_pval_grid,
    bmt_delta1 = 1,
    bmt_delta2 = 1,
    bmt_maxit = 300,
    farm_loss = "lasso",
    farm_robust = FALSE,
    farm_cv = TRUE,
    farm_tau = 2,
    farm_K_factors = NULL,
    farm_max_iter = 90000,
    farm_nfolds = 10,
    farm_eps = 1e-4,
    rf_num_trees = 1800,
    rf_min_node_size = 10,
    tcsr_tc = 1.645,
    tcsr_L = 10,
    tcsr_M = 10000,
    chronos2_enabled = TRUE,
    chronos2_uni_enabled = TRUE,
    chronos2_multi_enabled = TRUE,
    chronos2_covariate_enabled = TRUE,
    chronos2_multi_target_names = NULL,
    chronos2_envname = "chronos2-r",
    chronos2_device = "cpu",
    chronos2_model_id = "autogluon/chronos-2-small",
    chronos2_quantile_levels = c(0.5),
    chronos2_batch_size = 8L,
    chronos2_cross_learning = FALSE,
    chronos2_validate_inputs = FALSE,
    chronos2_context_max = 256L,
    chronos2_torch_threads = 8L,
    chronos2_torch_interop_threads = 1L,
    chronos2_workers = 3L,
    fortin_dir = file.path(Sys.getenv("USERPROFILE"), "Desktop", "fortin_tables")
) {
  avail <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
  avail <- avail[1]
  if (!is.finite(avail) || avail < 1) avail <- 1L
  if (is.null(n_workers)) n_workers <- max(1L, avail - 1L)
  
  ocmt_pvals <- normalize_pval_grid(ocmt_pvals)
  bmt_pvals  <- normalize_pval_grid(bmt_pvals)
  
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
      retune_every_months = as.integer(retune_every_months),
      træningsår = as.integer(træningsår)
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
        pvals = ocmt_pvals,
        delta1 = ocmt_delta1,
        delta2 = ocmt_delta2,
        multi_stage = ocmt_multi_stage,
        max_iter = as.integer(ocmt_max_iter),
        kappa_const = ocmt_kappa_const
      ),
      bmt = list(
        pvals = bmt_pvals,
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
        quantile_levels = as.numeric(chronos2_quantile_levels),
        batch_size = as.integer(chronos2_batch_size),
        cross_learning = isTRUE(chronos2_cross_learning),
        validate_inputs = isTRUE(chronos2_validate_inputs),
        context_max = as.integer(chronos2_context_max),
        torch_threads = as.integer(chronos2_torch_threads),
        torch_interop_threads = as.integer(chronos2_torch_interop_threads),
        workers = as.integer(chronos2_workers)
      )
    ),
    output = list(
      fortin_dir = fortin_dir
    )
  )
}

# ============================================================
# BASIC HELPERS
# ============================================================

safe_scale_newx <- function(X_new, center, scale) {
  if (is.null(X_new)) return(matrix(numeric(0), 1, length(center)))
  if (!is.matrix(X_new)) X_new <- matrix(as.numeric(X_new), nrow = 1)
  if (ncol(X_new) != length(center)) stop("X_new has incompatible number of columns")
  if (length(center) == 0) return(matrix(numeric(0), nrow(X_new), 0))
  sweep(sweep(X_new, 2, center, FUN = "-"), 2, scale, FUN = "/")
}

prepare_penalized_data <- function(X, y) {
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
      X = X2, Xs = X2, y = y2, yc = y2,
      center = numeric(ncol(X2)), scale = rep(1, ncol(X2)),
      y_mean = NA_real_, ok = ok
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

prepare_linear_data <- function(y_train, X_train, X_test = NULL) {
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

safe_lm_predict <- function(y_train, X_train, X_test) {
  prep <- prepare_linear_data(y_train, X_train, X_test = X_test)
  if (is.null(prep)) return(NA_real_)
  if (length(prep$y) <= (ncol(prep$X) + 1)) return(NA_real_)
  
  X1 <- cbind(1, prep$X)
  beta <- qr.coef(qr(X1), prep$y)
  beta[!is.finite(beta)] <- 0
  
  x_test1 <- c(1, as.numeric(prep$X_test))
  if (any(!is.finite(x_test1))) return(NA_real_)
  
  as.numeric(drop(x_test1 %*% beta))
}

get_selected_idx <- function(selected, p) {
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

build_post_selection_train_test <- function(Xcand_train,
                                            Xcand_test,
                                            Xcond_train = NULL,
                                            Xcond_test = NULL,
                                            selected = NULL) {
  if (!is.matrix(Xcand_train)) Xcand_train <- as.matrix(Xcand_train)
  if (!is.matrix(Xcand_test))  Xcand_test  <- as.matrix(Xcand_test)
  
  idx <- get_selected_idx(selected, ncol(Xcand_train))
  
  Xsel_train <- if (length(idx) > 0) {
    Xcand_train[, idx, drop = FALSE]
  } else {
    matrix(numeric(0), nrow(Xcand_train), 0)
  }
  
  Xsel_test <- if (length(idx) > 0) {
    Xcand_test[, idx, drop = FALSE]
  } else {
    matrix(numeric(0), nrow(Xcand_test), 0)
  }
  
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

predict_from_selection_ols <- function(y_train,
                                       Xcand_train,
                                       Xcand_test,
                                       Xcond_train = NULL,
                                       Xcond_test = NULL,
                                       selected = NULL) {
  mats <- build_post_selection_train_test(
    Xcand_train = Xcand_train,
    Xcand_test = Xcand_test,
    Xcond_train = Xcond_train,
    Xcond_test = Xcond_test,
    selected = selected
  )
  
  safe_lm_predict(y_train, mats$X_train, mats$X_test)
}

# ============================================================
# BIC TUNING FOR GLMNET
# ============================================================

ridge_df_path <- function(Xs, lambda) {
  n <- nrow(Xs)
  if (ncol(Xs) == 0) return(rep(1, length(lambda)))
  d <- svd(Xs, nu = 0, nv = 0)$d
  1 + vapply(lambda, function(lam) sum(ifelse(d > 0, d^2 / (d^2 + n * lam), 0)), numeric(1))
}

select_bic_glmnet <- function(Z, y, alpha, penalty.factor = NULL, nlambda = 240, lambda = NULL) {
  prep <- tryCatch(prepare_penalized_data(Z, y), error = function(e) NULL)
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
  if (is.null(fit) || length(fit$lambda) == 0) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  pred_mat <- tryCatch(stats::predict(fit, newx = prep$Xs), error = function(e) NULL)
  if (is.null(pred_mat)) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  pred_mat <- as.matrix(pred_mat)
  rss <- colSums((matrix(prep$yc, nrow = n, ncol = ncol(pred_mat)) - pred_mat)^2)
  rss[!is.finite(rss) | rss <= 0] <- .Machine$double.xmin
  
  k <- if (alpha == 0) ridge_df_path(prep$Xs, fit$lambda) else fit$df + 1
  bic <- n * log(rss / n) + k * log(n)
  
  if (all(!is.finite(bic))) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  j <- which.min(bic)
  
  list(lambda = fit$lambda[j], bic = bic[j], df = k[j])
}

tune_ridge_or_lasso <- function(Z, y, alpha, nlambda = 240, fallback_lambda = fallback_lambda_default) {
  out <- select_bic_glmnet(Z = Z, y = y, alpha = alpha, nlambda = nlambda)
  lam <- out$lambda
  used_fallback <- FALSE
  
  if (!is.finite(lam) || lam < 0) {
    lam <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(alpha = alpha, lambda = lam, used_fallback = used_fallback)
}

tune_elasticnet <- function(Z,
                            y,
                            alpha_grid = c(0.02, 0.08, 0.15, 0.25, 0.35, 0.5, 0.65, 0.75, 0.85, 0.92, 0.98),
                            nlambda = 240,
                            fallback_alpha = 0.5,
                            fallback_lambda = fallback_lambda_default) {
  best <- list(bic = Inf, alpha = NA_real_, lambda = NA_real_)
  
  for (a in alpha_grid) {
    cur <- select_bic_glmnet(Z = Z, y = y, alpha = a, nlambda = nlambda)
    
    if (is.finite(cur$bic) && cur$bic < best$bic) {
      best$bic <- cur$bic
      best$alpha <- a
      best$lambda <- cur$lambda
    }
  }
  
  used_fallback <- FALSE
  if (!is.finite(best$alpha)) best$alpha <- fallback_alpha
  
  if (!is.finite(best$lambda) || best$lambda < 0) {
    best$lambda <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(alpha = best$alpha, lambda = best$lambda, used_fallback = used_fallback)
}

tune_adaptive_lasso <- function(Z, y, nlambda = 240, eps = 1e-6, fallback_lambda = fallback_lambda_default) {
  prep <- tryCatch(prepare_penalized_data(Z, y), error = function(e) NULL)
  
  if (is.null(prep) || length(prep$y) == 0 || ncol(prep$Xs) == 0) {
    p0 <- if (is.null(Z)) 0 else ncol(as.matrix(Z))
    return(list(lambda = fallback_lambda, weights = rep(1, p0), used_fallback = TRUE))
  }
  
  ridge_bic <- select_bic_glmnet(Z = Z, y = y, alpha = 0, nlambda = nlambda)
  lambda0 <- ridge_bic$lambda
  if (!is.finite(lambda0) || lambda0 < 0) lambda0 <- fallback_lambda
  
  fit0 <- tryCatch(
    glmnet::glmnet(prep$Xs, prep$yc, alpha = 0, lambda = lambda0, standardize = FALSE, intercept = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(fit0)) {
    w <- rep(1, ncol(prep$Xs))
  } else {
    b0 <- as.numeric(stats::coef(fit0))[-1]
    w <- 1 / (abs(b0) + eps)
    w[!is.finite(w)] <- 1 / eps
  }
  
  ada_bic <- select_bic_glmnet(Z = Z, y = y, alpha = 1, penalty.factor = w, nlambda = nlambda)
  
  lam <- ada_bic$lambda
  used_fallback <- FALSE
  
  if (!is.finite(lam) || lam < 0) {
    lam <- fallback_lambda
    used_fallback <- TRUE
  }
  
  list(lambda = lam, weights = w, used_fallback = used_fallback)
}

fit_predict_glmnet <- function(Z_train, y_train, Z_test, spec, fallback_lambda = fallback_lambda_default) {
  if (is.null(spec) || is.null(spec$type)) return(NA_real_)
  
  lambda_use <- spec$lambda
  if (is.null(lambda_use) || !is.finite(lambda_use) || lambda_use < 0) lambda_use <- fallback_lambda
  
  prep <- tryCatch(prepare_penalized_data(Z_train, y_train), error = function(e) NULL)
  if (is.null(prep) || length(prep$y) == 0) return(NA_real_)
  
  if (is.null(Z_test)) Z_test <- matrix(numeric(0), 1, 0)
  if (!is.matrix(Z_test)) Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  if (nrow(Z_test) != 1 || any(!is.finite(Z_test)) || ncol(Z_test) != ncol(prep$X)) return(NA_real_)
  
  if (ncol(prep$Xs) == 0) return(prep$y_mean)
  
  Zs_test <- tryCatch(safe_scale_newx(Z_test, prep$center, prep$scale), error = function(e) NULL)
  if (is.null(Zs_test)) return(NA_real_)
  
  fit <- tryCatch(
    switch(
      spec$type,
      ridge = glmnet::glmnet(prep$Xs, prep$yc, alpha = 0, lambda = lambda_use, standardize = FALSE, intercept = FALSE),
      lasso = glmnet::glmnet(prep$Xs, prep$yc, alpha = 1, lambda = lambda_use, standardize = FALSE, intercept = FALSE),
      elastic = glmnet::glmnet(prep$Xs, prep$yc, alpha = ifelse(is.finite(spec$alpha), spec$alpha, 0.5), lambda = lambda_use, standardize = FALSE, intercept = FALSE),
      adalasso = glmnet::glmnet(prep$Xs, prep$yc, alpha = 1, lambda = lambda_use, standardize = FALSE, intercept = FALSE, penalty.factor = spec$weights),
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
# SELECTION HELPERS
# ============================================================

select_ocmt <- function(y_train,
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
    OCMT::ocmt_sel(
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

select_bmt <- function(y_train,
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
    MultipleTestingBoosting::boosting_glm(
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

# ============================================================
# FARMSELECT NATIVE FIT/PREDICT
# ============================================================

fit_farm_native <- function(y_train,
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
      selected_idx = integer(0),
      fit_fallback_reason = "no_valid_training_rows"
    ))
  }
  
  y_mean <- mean(y2)
  
  if (ncol(Z2) == 0 || min(nrow(Z2), ncol(Z2)) <= 4) {
    return(list(
      use_mean = TRUE,
      y_mean = y_mean,
      fit = NULL,
      selected_idx = integer(0),
      fit_fallback_reason = "too_few_rows_or_columns"
    ))
  }
  
  fit <- tryCatch(
    FarmSelect::farm.select(
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
      selected_idx = integer(0),
      fit_fallback_reason = "farm_select_failed"
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
    selected_idx = idx,
    fit_fallback_reason = NA_character_
  )
}

predict_farm_native <- function(farm_obj, Z_test, return_info = FALSE) {
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
  
  if (is.null(farm_obj)) return(out(NA_real_, TRUE, "missing_farm_object"))
  if (!is.matrix(Z_test)) Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  if (nrow(Z_test) != 1) return(out(NA_real_, TRUE, "bad_Z_test_rows"))
  if (!is.finite(farm_obj$y_mean)) return(out(NA_real_, TRUE, "bad_y_mean"))
  
  if (isTRUE(farm_obj$use_mean) || is.null(farm_obj$fit)) {
    return(out(farm_obj$y_mean, TRUE, farm_obj$fit_fallback_reason))
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
# MISC HELPERS
# ============================================================

as_numeric_df <- function(df, date_col = "Date") {
  out <- df
  out[[date_col]] <- as.Date(out[[date_col]])
  num_cols <- setdiff(names(out), date_col)
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))
  out
}

make_yh_ahead <- function(y, h, scale = 1) {
  y <- as.numeric(y)
  n <- length(y)
  out <- rep(NA_real_, n)
  
  if (h <= 0) stop("h must be >= 1")
  if (n <= h) return(out)
  
  for (t in 1:(n - h)) {
    out[t] <- scale * y[t + h]
  }
  
  out
}

add_lags <- function(df, var, p, prefix = NULL) {
  stopifnot(p >= 1)
  
  x <- df[[var]]
  base <- if (is.null(prefix)) var else prefix
  
  for (L in 0:(p - 1)) {
    df[[paste0(base, "_L", L)]] <- dplyr::lag(x, L)
  }
  
  df
}

add_lags_many <- function(df, vars, p) {
  for (v in vars) {
    df <- add_lags(df, v, p, prefix = v)
  }
  
  df
}

bic_lm <- function(y, X) {
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

dm_pval <- function(e_bench, e_model, h) {
  e_bench <- as.numeric(e_bench)
  e_model <- as.numeric(e_model)
  
  ok <- is.finite(e_bench) & is.finite(e_model)
  e_bench <- e_bench[ok]
  e_model <- e_model[ok]
  
  n <- length(e_bench)
  if (n < max(h + 1, 10)) return(NA_real_)
  if (stats::var(e_model - e_bench) == 0) return(NA_real_)
  
  tryCatch(
    forecast::dm.test(e_model, e_bench, h = h, power = 2, alternative = "two.sided")$p.value,
    error = function(e) NA_real_
  )
}

compute_RMSE_info <- function(y_true, y_hat) {
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

compute_simple_average_forecast <- function(current_preds, candidate_models) {
  vals <- as.numeric(current_preds[candidate_models])
  vals <- vals[is.finite(vals)]
  
  if (length(vals) == 0) return(NA_real_)
  
  mean(vals)
}

# ============================================================
# PCA / ARDI
# ============================================================

pca_fit <- function(X, Kmax = 10) {
  X <- as.matrix(X)
  
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
  pc <- tryCatch(prcomp(Xs, center = FALSE, scale. = FALSE), error = function(e) NULL)
  if (is.null(pc) || is.null(pc$x) || ncol(pc$x) == 0) return(NULL)
  
  Kuse <- min(Kmax, ncol(pc$x))
  
  list(
    loadings = pc$rotation[, 1:Kuse, drop = FALSE],
    scores = pc$x[, 1:Kuse, drop = FALSE],
    mu = mu,
    sd = sdv
  )
}

fit_predict_AR_BIC <- function(train_df, test_row, py_max = 6) {
  bics <- rep(Inf, py_max)
  coefs <- vector("list", py_max)
  
  for (p in 1:py_max) {
    cols <- paste0("y_L", 0:(p - 1))
    d <- train_df[, c("y_h", cols), drop = FALSE]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    
    if (nrow(d) < (p + 25)) next
    
    tmp <- bic_lm(d$y_h, as.matrix(d[, cols, drop = FALSE]))
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

fit_predict_ARDI_BIC <- function(hist_df, train_df, test_row, x_names, py_max = 6, pf_max = 6, Kmax = 10) {
  X_hist <- as.matrix(hist_df[, x_names, drop = FALSE])
  pca <- pca_fit(X_hist, Kmax = Kmax)
  
  if (is.null(pca)) return(NA_real_)
  
  F_hist <- pca$scores
  K_avail <- ncol(F_hist)
  
  if (K_avail == 0) return(NA_real_)
  
  colnames(F_hist) <- paste0("F", seq_len(K_avail))
  hf <- dplyr::bind_cols(hist_df, as.data.frame(F_hist))
  
  for (k in seq_len(K_avail)) {
    for (L in 0:(pf_max - 1)) {
      hf[[paste0("F", k, "_L", L)]] <- dplyr::lag(hf[[paste0("F", k)]], L)
    }
  }
  
  hf_train <- hf |>
    dplyr::filter(.data$Date %in% train_df$Date)
  
  best <- list(bic = Inf, beta = NULL, cols = NULL)
  
  for (py in 1:py_max) {
    ycols <- paste0("y_L", 0:(py - 1))
    
    for (pf in 1:pf_max) {
      for (K in 1:K_avail) {
        fcols <- unlist(lapply(1:K, function(k) paste0("F", k, "_L", 0:(pf - 1))))
        cols <- c(ycols, fcols)
        d <- hf_train[, c("y_h", cols), drop = FALSE]
        d <- d[stats::complete.cases(d), , drop = FALSE]
        
        if (nrow(d) < (length(cols) + 25)) next
        
        tmp <- bic_lm(d$y_h, as.matrix(d[, cols, drop = FALSE]))
        
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
    dplyr::filter(.data$Date == test_row$Date)
  
  if (nrow(hf_test) != 1) return(NA_real_)
  
  x_test <- as.numeric(hf_test[, best$cols, drop = FALSE])
  
  sum(c(1, x_test) * best$beta)
}

# ============================================================
# RF / T-CSR
# ============================================================

fit_predict_rf <- function(Z_train, y_train, Z_test, num_trees = 1800, min_node_size = 10) {
  if (!is.matrix(Z_train)) Z_train <- as.matrix(Z_train)
  if (!is.matrix(Z_test))  Z_test  <- as.matrix(Z_test)
  
  ok_x <- if (ncol(Z_train) == 0) rep(TRUE, nrow(Z_train)) else apply(Z_train, 1, function(r) all(is.finite(r)))
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
      num.threads = 1
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NA_real_)
  
  as.numeric(stats::predict(fit, data = data.frame(Z_test))$predictions)
}

tcsr_screen <- function(train_df, x_names, tc = 1.645) {
  keep <- c()
  base_cols <- paste0("y_L", 0:3)
  
  for (x in x_names) {
    cols <- c("y_h", base_cols, x)
    d <- train_df[, cols, drop = FALSE]
    d <- d[stats::complete.cases(d), , drop = FALSE]
    
    if (nrow(d) < 50) next
    
    fit <- stats::lm(d$y_h ~ ., data = d[, setdiff(cols, "y_h"), drop = FALSE])
    sm <- summary(fit)
    
    if (x %in% rownames(coef(sm))) {
      tval <- coef(sm)[x, "t value"]
      if (is.finite(tval) && abs(tval) > tc) keep <- c(keep, x)
    }
  }
  
  unique(keep)
}

tcsr_predict <- function(train_df, test_row, xstar, L = 10, M = 10000, seed = 1) {
  set.seed(seed)
  
  y_tr <- train_df$y_h
  y0 <- train_df$y_L0
  
  if (length(xstar) == 0) {
    d_ar <- data.frame(y_h = y_tr, y_L0 = y0)
    d_ar <- d_ar[stats::complete.cases(d_ar), , drop = FALSE]
    
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
  
  if (!any(is.finite(preds))) {
    d_ar <- data.frame(y_h = y_tr, y_L0 = y0)
    d_ar <- d_ar[stats::complete.cases(d_ar), , drop = FALSE]
    
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
  
  mean(preds, na.rm = TRUE)
}

# ============================================================
# MODEL-GROUP SPLITS
# ============================================================

non_chronos_model_names <- setdiff(
  model_order_internal,
  c("Chronos-2-uni", "Chronos-2-multi", "Chronos-2-covariate-informed", "AVG")
)

chronos_only_model_names <- intersect(
  c("Chronos-2-uni", "Chronos-2-multi", "Chronos-2-covariate-informed"),
  model_order_internal
)

# ============================================================
# SHARED DETAIL PREP
# ============================================================

prepare_fortin_detail_inputs <- function(panel_can, y_name, h, ctrl) {
  stopifnot("Date" %in% names(panel_can))
  stopifnot(y_name %in% names(panel_can))
  
  py <- ctrl$rolling$py
  px <- ctrl$rolling$px
  K_fixed <- ctrl$rolling$K_fixed
  oos_start <- ctrl$rolling$oos_start
  oos_end <- max(panel_can$Date, na.rm = TRUE)
  retune_every_months <- ctrl$rolling$retune_every_months
  træningsår <- ctrl$rolling$træningsår
  
  df_base <- as_numeric_df(panel_can, "Date") |>
    dplyr::arrange(.data$Date)
  
  x_names <- setdiff(setdiff(names(df_base), "Date"), y_name)
  
  df <- df_base
  df$y <- df[[y_name]]
  df$y_h <- make_yh_ahead(df$y, h = h)
  
  df_l <- df |>
    dplyr::select(.data$Date, .data$y, .data$y_h, dplyr::all_of(x_names)) |>
    add_lags("y", p = py, prefix = "y")
  
  df_x <- df_l |>
    add_lags_many(x_names, p = px) |>
    dplyr::filter(!is.na(.data$y_h), .data$Date <= oos_end)
  
  ycols <- paste0("y_L", 0:(py - 1))
  xlagcols <- unlist(lapply(x_names, function(v) paste0(v, "_L", 0:(px - 1))))
  req_cols <- c("y_h", ycols, xlagcols)
  
  df_x <- df_x[stats::complete.cases(df_x[, c("Date", req_cols), drop = FALSE]), , drop = FALSE]
  
  eval_idx <- which(df_x$Date >= oos_start & df_x$Date <= oos_end)
  
  list(
    py = py,
    px = px,
    K_fixed = K_fixed,
    oos_start = oos_start,
    oos_end = oos_end,
    retune_every_months = retune_every_months,
    træningsår = træningsår,
    df_base = df_base,
    df_x = df_x,
    eval_idx = eval_idx,
    x_names = x_names,
    ycols = ycols,
    xlagcols = xlagcols
  )
}

# ============================================================
# DETAIL-LEVEL ROLLING FUNCTION: NON-CHRONOS
# ============================================================

run_fortin_one_target_one_h_detail_nonchronos <- function(panel_can,
                                                          y_name,
                                                          h,
                                                          ctrl,
                                                          models_to_run = non_chronos_model_names,
                                                          run_id = y_name,
                                                          p = NULL) {
  prep <- prepare_fortin_detail_inputs(panel_can, y_name, h, ctrl)
  
  models_to_run <- unique(c(intersect(models_to_run, non_chronos_model_names), benchmark_model))
  
  py <- prep$py
  px <- prep$px
  K_fixed <- prep$K_fixed
  retune_every_months <- prep$retune_every_months
  træningsår <- prep$træningsår
  
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
  bmt_ctrl  <- ctrl$selection$bmt
  farm_ctrl <- ctrl$selection$farm
  
  ocmt_pvals <- normalize_pval_grid(ocmt_ctrl$pvals)
  bmt_pvals  <- normalize_pval_grid(bmt_ctrl$pvals)
  
  rf_ctrl   <- ctrl$model$rf
  tcsr_ctrl <- ctrl$model$tcsr
  
  ocmt_base_names <- paste0("OCMT_", names(ocmt_pvals))
  bmt_base_names  <- paste0("BMT_", names(bmt_pvals))
  
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
  
  months_between <- function(a, b) {
    12L * (lubridate::year(b) - lubridate::year(a)) +
      (lubridate::month(b) - lubridate::month(a))
  }
  
  need_retune <- function(last, now) is.na(last) || months_between(last, now) >= retune_every_months
  
  fallback_tunes <- empty_fallback_counter()
  fallback_forecasts <- empty_fallback_counter()
  tcsr_counter <- empty_tcsr_counter()
  
  pred_tbl <- as.data.frame(
    matrix(NA_real_, nrow = nrow(df_x), ncol = length(models_to_run)),
    stringsAsFactors = FALSE
  )
  names(pred_tbl) <- models_to_run
  
  if (nrow(df_x) == 0 || length(eval_idx) == 0) {
    return(list(
      h = h,
      dates = if (nrow(df_x) == 0) as.Date(character(0)) else df_x$Date,
      y_true = if (nrow(df_x) == 0) numeric(0) else df_x$y_h,
      pred_tbl = pred_tbl,
      meta = list(
        fallback_tunes = fallback_tunes,
        fallback_forecasts = fallback_forecasts,
        tcsr_benchmark_fallbacks = as.integer(tcsr_counter["tcsr_benchmark_fallbacks"]),
        tcsr_total_forecasts = as.integer(tcsr_counter["tcsr_total_forecasts"])
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
  
  spec <- list(
    ocmt_sel = list(),
    bmt_sel = list()
  )
  
  for (i in eval_idx) {
    t0 <- df_x$Date[i]
    cutoff <- t0 %m-% lubridate::period(months = h)
    
    train_idx <- which(df_x$Date <= cutoff)
    if (length(train_idx) < træningsår) next
    
    train_df <- df_x[train_idx, , drop = FALSE]
    test_row <- df_x[i, , drop = FALSE]
    
    if (run_ar) {
      pred_tbl[i, "AR,BIC"] <- fit_predict_AR_BIC(train_df, test_row, py_max = py)
    }
    
    hist_df_t0 <- NULL
    
    if (run_ardi_bic || run_factor_block) {
      hist_df_t0 <- df_base |>
        dplyr::arrange(.data$Date) |>
        dplyr::filter(.data$Date <= t0)
      
      hist_df_t0$y <- hist_df_t0[[y_name]]
      hist_df_t0$y_h <- make_yh_ahead(hist_df_t0$y, h = h)
      hist_df_t0 <- hist_df_t0 |> add_lags("y", p = py, prefix = "y")
    }
    
    if (run_ardi_bic) {
      pred_tbl[i, "ARDI,BIC"] <- fit_predict_ARDI_BIC(
        hist_df = hist_df_t0 |>
          dplyr::select(.data$Date, .data$y_h, dplyr::starts_with("y_L"), dplyr::all_of(x_names)),
        train_df = train_df |>
          dplyr::select(.data$Date),
        test_row = test_row |>
          dplyr::select(.data$Date, dplyr::all_of(x_names), dplyr::starts_with("y_L"), .data$y_h),
        x_names = x_names,
        py_max = py,
        pf_max = px,
        Kmax = K_fixed
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
          spec$ridge <- c(
            list(type = "ridge"),
            tune_ridge_or_lasso(Zx_train, Zy_train, alpha = 0, nlambda = glmnet_nlambda, fallback_lambda = fallback_lambda)
          )
          if (isTRUE(spec$ridge$used_fallback)) fallback_tunes["Ridge-X"] <- fallback_tunes["Ridge-X"] + 1L
          last_tune$ridge <- t0
        }
        if (isTRUE(spec$ridge$used_fallback)) fallback_forecasts["Ridge-X"] <- fallback_forecasts["Ridge-X"] + 1L
        pred_tbl[i, "Ridge-X"] <- fit_predict_glmnet(Zx_train, Zy_train, Zx_test, spec$ridge, fallback_lambda)
      }
      
      if ("Lasso-X" %in% models_to_run) {
        if (need_retune(last_tune$lasso, t0)) {
          spec$lasso <- c(
            list(type = "lasso"),
            tune_ridge_or_lasso(Zx_train, Zy_train, alpha = 1, nlambda = glmnet_nlambda, fallback_lambda = fallback_lambda)
          )
          if (isTRUE(spec$lasso$used_fallback)) fallback_tunes["Lasso-X"] <- fallback_tunes["Lasso-X"] + 1L
          last_tune$lasso <- t0
        }
        if (isTRUE(spec$lasso$used_fallback)) fallback_forecasts["Lasso-X"] <- fallback_forecasts["Lasso-X"] + 1L
        pred_tbl[i, "Lasso-X"] <- fit_predict_glmnet(Zx_train, Zy_train, Zx_test, spec$lasso, fallback_lambda)
      }
      
      if ("Elastic-Net-X" %in% models_to_run) {
        if (need_retune(last_tune$elastic, t0)) {
          spec$elastic <- c(
            list(type = "elastic"),
            tune_elasticnet(
              Zx_train,
              Zy_train,
              alpha_grid = elastic_alpha_grid,
              nlambda = glmnet_nlambda,
              fallback_alpha = elastic_fallback_alpha,
              fallback_lambda = fallback_lambda
            )
          )
          if (isTRUE(spec$elastic$used_fallback)) fallback_tunes["Elastic-Net-X"] <- fallback_tunes["Elastic-Net-X"] + 1L
          last_tune$elastic <- t0
        }
        if (isTRUE(spec$elastic$used_fallback)) fallback_forecasts["Elastic-Net-X"] <- fallback_forecasts["Elastic-Net-X"] + 1L
        pred_tbl[i, "Elastic-Net-X"] <- fit_predict_glmnet(Zx_train, Zy_train, Zx_test, spec$elastic, fallback_lambda)
      }
      
      if ("Adaptive-Lasso-X" %in% models_to_run) {
        if (need_retune(last_tune$adalasso, t0)) {
          ta <- tune_adaptive_lasso(Zx_train, Zy_train, nlambda = glmnet_nlambda, eps = adalasso_eps, fallback_lambda = fallback_lambda)
          spec$adalasso <- list(type = "adalasso", lambda = ta$lambda, weights = ta$weights, used_fallback = ta$used_fallback)
          if (isTRUE(spec$adalasso$used_fallback)) fallback_tunes["Adaptive-Lasso-X"] <- fallback_tunes["Adaptive-Lasso-X"] + 1L
          last_tune$adalasso <- t0
        } else {
          ta <- tune_adaptive_lasso(Zx_train, Zy_train, nlambda = glmnet_nlambda, eps = adalasso_eps, fallback_lambda = fallback_lambda)
          spec$adalasso$weights <- ta$weights
        }
        
        if (isTRUE(spec$adalasso$used_fallback)) fallback_forecasts["Adaptive-Lasso-X"] <- fallback_forecasts["Adaptive-Lasso-X"] + 1L
        pred_tbl[i, "Adaptive-Lasso-X"] <- fit_predict_glmnet(Zx_train, Zy_train, Zx_test, spec$adalasso, fallback_lambda)
      }
      
      for (lev in names(ocmt_pvals)) {
        base_name <- paste0("OCMT_", lev)
        if (!(base_name %in% models_to_run)) next
        
        if (need_retune(last_tune$ocmt[base_name], t0)) {
          spec$ocmt_sel[[base_name]] <- select_ocmt(
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
        
        pred_tbl[i, base_name] <- predict_from_selection_ols(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcand_test = Xcand_test,
          Xcond_train = Xcond_train,
          Xcond_test = Xcond_test,
          selected = spec$ocmt_sel[[base_name]]
        )
      }
      
      if ("FarmSelect" %in% models_to_run) {
        if (need_retune(last_tune$farm, t0)) {
          spec$farm_fit <- fit_farm_native(
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
          
          if (isTRUE(spec$farm_fit$use_mean) || is.null(spec$farm_fit$fit)) {
            fallback_tunes["FarmSelect"] <- fallback_tunes["FarmSelect"] + 1L
          }
          
          last_tune$farm <- t0
        }
        
        farm_pred <- predict_farm_native(
          farm_obj = spec$farm_fit,
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
          spec$bmt_sel[[base_name]] <- select_bmt(
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
        
        pred_tbl[i, base_name] <- predict_from_selection_ols(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcand_test = Xcand_test,
          Xcond_train = Xcond_train,
          Xcond_test = Xcond_test,
          selected = spec$bmt_sel[[base_name]]
        )
      }
      
      if ("RF-X" %in% models_to_run) {
        pred_tbl[i, "RF-X"] <- fit_predict_rf(
          Zx_train,
          Zy_train,
          Zx_test,
          num_trees = rf_ctrl$num_trees,
          min_node_size = rf_ctrl$min_node_size
        )
      }
    }
    
    if (run_factor_block && !is.null(hist_df_t0)) {
      X_hist_all <- as.matrix(hist_df_t0[, x_names, drop = FALSE])
      pcaK <- pca_fit(X_hist_all, Kmax = K_fixed)
      
      if (!is.null(pcaK)) {
        F_all <- pcaK$scores
        K_avail <- ncol(F_all)
        
        if (K_avail > 0) {
          colnames(F_all) <- paste0("F", seq_len(K_avail))
          
          hf2 <- dplyr::bind_cols(
            hist_df_t0 |>
              dplyr::select(.data$Date, .data$y_h, dplyr::starts_with("y_L")),
            as.data.frame(F_all)
          )
          
          for (k in seq_len(K_avail)) {
            for (L in 0:(py - 1)) {
              hf2[[paste0("F", k, "_L", L)]] <- dplyr::lag(hf2[[paste0("F", k)]], L)
            }
          }
          
          hf2_oos <- hf2 |>
            dplyr::filter(.data$Date %in% df_x$Date)
          
          idx_train2 <- which(hf2_oos$Date <= cutoff)
          idx_test2  <- which(hf2_oos$Date == t0)
          
          if (length(idx_test2) == 1) {
            tr2 <- hf2_oos[idx_train2, , drop = FALSE]
            te2 <- hf2_oos[idx_test2, , drop = FALSE]
            
            fac_lag_cols <- c(
              paste0("y_L", 0:(py - 1)),
              unlist(lapply(seq_len(K_avail), function(k) paste0("F", k, "_L", 0:(py - 1))))
            )
            
            cc_f <- stats::complete.cases(tr2[, c("y_h", fac_lag_cols), drop = FALSE])
            tr2 <- tr2[cc_f, , drop = FALSE]
            
            if (nrow(tr2) >= 50) {
              Zf_train <- as.matrix(tr2[, fac_lag_cols, drop = FALSE])
              yf_train <- tr2$y_h
              Zf_test <- as.matrix(te2[, fac_lag_cols, drop = FALSE])
              
              if ("ARDI,Ridge" %in% models_to_run) {
                if (need_retune(last_tune$ardi_ridge, t0)) {
                  spec$ardi_ridge <- c(
                    list(type = "ridge"),
                    tune_ridge_or_lasso(Zf_train, yf_train, alpha = 0, nlambda = glmnet_nlambda, fallback_lambda = fallback_lambda)
                  )
                  if (isTRUE(spec$ardi_ridge$used_fallback)) fallback_tunes["ARDI,Ridge"] <- fallback_tunes["ARDI,Ridge"] + 1L
                  last_tune$ardi_ridge <- t0
                }
                if (isTRUE(spec$ardi_ridge$used_fallback)) fallback_forecasts["ARDI,Ridge"] <- fallback_forecasts["ARDI,Ridge"] + 1L
                pred_tbl[i, "ARDI,Ridge"] <- fit_predict_glmnet(Zf_train, yf_train, Zf_test, spec$ardi_ridge, fallback_lambda)
              }
              
              if ("ARDI,Lasso" %in% models_to_run) {
                if (need_retune(last_tune$ardi_lasso, t0)) {
                  spec$ardi_lasso <- c(
                    list(type = "lasso"),
                    tune_ridge_or_lasso(Zf_train, yf_train, alpha = 1, nlambda = glmnet_nlambda, fallback_lambda = fallback_lambda)
                  )
                  if (isTRUE(spec$ardi_lasso$used_fallback)) fallback_tunes["ARDI,Lasso"] <- fallback_tunes["ARDI,Lasso"] + 1L
                  last_tune$ardi_lasso <- t0
                }
                if (isTRUE(spec$ardi_lasso$used_fallback)) fallback_forecasts["ARDI,Lasso"] <- fallback_forecasts["ARDI,Lasso"] + 1L
                pred_tbl[i, "ARDI,Lasso"] <- fit_predict_glmnet(Zf_train, yf_train, Zf_test, spec$ardi_lasso, fallback_lambda)
              }
              
              if ("ARDI,Elastic-Net" %in% models_to_run) {
                if (need_retune(last_tune$ardi_elastic, t0)) {
                  spec$ardi_elastic <- c(
                    list(type = "elastic"),
                    tune_elasticnet(
                      Zf_train,
                      yf_train,
                      alpha_grid = elastic_alpha_grid,
                      nlambda = glmnet_nlambda,
                      fallback_alpha = elastic_fallback_alpha,
                      fallback_lambda = fallback_lambda
                    )
                  )
                  if (isTRUE(spec$ardi_elastic$used_fallback)) fallback_tunes["ARDI,Elastic-Net"] <- fallback_tunes["ARDI,Elastic-Net"] + 1L
                  last_tune$ardi_elastic <- t0
                }
                if (isTRUE(spec$ardi_elastic$used_fallback)) fallback_forecasts["ARDI,Elastic-Net"] <- fallback_forecasts["ARDI,Elastic-Net"] + 1L
                pred_tbl[i, "ARDI,Elastic-Net"] <- fit_predict_glmnet(Zf_train, yf_train, Zf_test, spec$ardi_elastic, fallback_lambda)
              }
              
              if ("ARDI,Adaptive-Lasso" %in% models_to_run) {
                if (need_retune(last_tune$ardi_adalasso, t0)) {
                  taf <- tune_adaptive_lasso(Zf_train, yf_train, nlambda = glmnet_nlambda, eps = adalasso_eps, fallback_lambda = fallback_lambda)
                  spec$ardi_adalasso <- list(type = "adalasso", lambda = taf$lambda, weights = taf$weights, used_fallback = taf$used_fallback)
                  if (isTRUE(spec$ardi_adalasso$used_fallback)) fallback_tunes["ARDI,Adaptive-Lasso"] <- fallback_tunes["ARDI,Adaptive-Lasso"] + 1L
                  last_tune$ardi_adalasso <- t0
                } else {
                  taf <- tune_adaptive_lasso(Zf_train, yf_train, nlambda = glmnet_nlambda, eps = adalasso_eps, fallback_lambda = fallback_lambda)
                  spec$ardi_adalasso$weights <- taf$weights
                }
                
                if (isTRUE(spec$ardi_adalasso$used_fallback)) fallback_forecasts["ARDI,Adaptive-Lasso"] <- fallback_forecasts["ARDI,Adaptive-Lasso"] + 1L
                pred_tbl[i, "ARDI,Adaptive-Lasso"] <- fit_predict_glmnet(Zf_train, yf_train, Zf_test, spec$ardi_adalasso, fallback_lambda)
              }
              
              if ("RFARDI" %in% models_to_run) {
                pred_tbl[i, "RFARDI"] <- fit_predict_rf(
                  Zf_train,
                  yf_train,
                  Zf_test,
                  num_trees = rf_ctrl$num_trees,
                  min_node_size = rf_ctrl$min_node_size
                )
              }
            }
          }
        }
      }
    }
    
    if (run_tcsr) {
      if (need_retune(last_tune$tcsr, t0)) {
        spec$tcsr <- list(xstar = tcsr_screen(train_df, x_names, tc = tcsr_ctrl$tc))
        last_tune$tcsr <- t0
      }
      
      xstar <- spec$tcsr$xstar
      tcsr_counter["tcsr_total_forecasts"] <- tcsr_counter["tcsr_total_forecasts"] + 1L
      
      tcsr_raw <- tcsr_predict(
        train_df,
        test_row,
        xstar = xstar,
        L = tcsr_ctrl$L,
        M = tcsr_ctrl$M,
        seed = 1
      )
      
      if (!is.finite(tcsr_raw) && is.finite(pred_tbl[i, benchmark_model])) {
        pred_tbl[i, "T-CSR"] <- pred_tbl[i, benchmark_model]
        tcsr_counter["tcsr_benchmark_fallbacks"] <- tcsr_counter["tcsr_benchmark_fallbacks"] + 1L
      } else {
        pred_tbl[i, "T-CSR"] <- tcsr_raw
      }
    }
  }
  
  if (!is.null(p)) p(message = sprintf("%s | h=%s finished", run_id, h))
  
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
# SUMMARY / MERGE
# ============================================================

summarise_task_detail <- function(detail_res, models_final = model_order_internal) {
  y_true <- detail_res$y_true
  pred_tbl <- as.data.frame(detail_res$pred_tbl, stringsAsFactors = FALSE)
  h <- detail_res$h
  
  res_h <- lapply(models_final, function(m) {
    if (!(m %in% names(pred_tbl))) {
      return(list(RMSE = NA_real_, ok = rep(FALSE, length(y_true))))
    }
    
    compute_RMSE_info(y_true, pred_tbl[[m]])
  })
  
  names(res_h) <- models_final
  
  bmk <- res_h[[benchmark_model]]
  
  ratio <- sapply(models_final, function(m) {
    if (!is.finite(res_h[[m]]$RMSE) || !is.finite(bmk$RMSE) || bmk$RMSE <= 0) return(NA_real_)
    res_h[[m]]$RMSE / bmk$RMSE
  })
  
  ratio[benchmark_model] <- NA_real_
  
  pvals <- rep(NA_real_, length(models_final))
  names(pvals) <- models_final
  
  for (m in setdiff(models_final, benchmark_model)) {
    if (!(m %in% names(pred_tbl))) next
    
    ok <- res_h[[m]]$ok & bmk$ok
    if (!any(ok)) next
    
    e_b <- y_true[ok] - pred_tbl[ok, benchmark_model]
    e_m <- y_true[ok] - pred_tbl[ok, m]
    
    pvals[m] <- dm_pval(e_bench = e_b, e_model = e_m, h = h)
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

merge_task_details <- function(base_detail, add_detail = NULL, final_models = model_order_internal) {
  detail_use <- if (!is.null(base_detail)) base_detail else add_detail
  dates_use <- detail_use$dates
  y_true_use <- detail_use$y_true
  h_use <- detail_use$h
  
  pred_all <- as.data.frame(
    matrix(NA_real_, nrow = length(y_true_use), ncol = length(final_models)),
    stringsAsFactors = FALSE
  )
  names(pred_all) <- final_models
  
  meta_use <- list(
    fallback_tunes = empty_fallback_counter(),
    fallback_forecasts = empty_fallback_counter(),
    tcsr_benchmark_fallbacks = 0L,
    tcsr_total_forecasts = 0L
  )
  
  add_meta <- function(meta_obj) {
    if (is.null(meta_obj)) return(invisible(NULL))
    
    if (!is.null(meta_obj$fallback_tunes)) {
      meta_use$fallback_tunes[names(meta_obj$fallback_tunes)] <<-
        meta_use$fallback_tunes[names(meta_obj$fallback_tunes)] + meta_obj$fallback_tunes
    }
    
    if (!is.null(meta_obj$fallback_forecasts)) {
      meta_use$fallback_forecasts[names(meta_obj$fallback_forecasts)] <<-
        meta_use$fallback_forecasts[names(meta_obj$fallback_forecasts)] + meta_obj$fallback_forecasts
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
    current_preds <- as.numeric(pred_all[i, combo_candidate_models, drop = TRUE])
    names(current_preds) <- combo_candidate_models
    
    compute_simple_average_forecast(
      current_preds = current_preds,
      candidate_models = combo_candidate_models
    )
  }, numeric(1))
  
  merged_detail <- list(
    h = h_use,
    dates = dates_use,
    y_true = y_true_use,
    pred_tbl = pred_all,
    meta = meta_use
  )
  
  summarise_task_detail(merged_detail, models_final = final_models)
}

# ============================================================
# TASK RUNNERS
# ============================================================

run_task_grid_nonchronos <- function(panel_can, task_list, ctrl, models_to_run) {
  if (nrow(task_list) == 0) return(list())
  if (length(models_to_run) == 0) return(vector("list", nrow(task_list)))
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  available_workers <- tryCatch(as.integer(future::availableCores()), error = function(e) 1L)
  available_workers <- available_workers[1]
  if (!is.finite(available_workers) || available_workers < 1) available_workers <- 1L
  
  requested_workers <- ctrl$parallel$requested_workers
  n_workers_use <- max(1L, min(requested_workers, available_workers, nrow(task_list)))
  
  message(sprintf(
    "Non-Chronos phase | Requested workers: %d | Available cores: %d | Parallel tasks: %d | Using workers: %d",
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
        run_fortin_one_target_one_h_detail_nonchronos(
          panel_can = panel_can,
          y_name = task_list$yname[i],
          h = task_list$h[i],
          ctrl = ctrl,
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

# ============================================================
# FALLBACK SUMMARY
# ============================================================

summarise_fallback_counts <- function(all_results) {
  tunes <- empty_fallback_counter()
  forecasts <- empty_fallback_counter()
  
  tcsr_benchmark_fallbacks <- 0L
  tcsr_total_forecasts <- 0L
  
  for (nm in names(all_results)) {
    meta <- all_results[[nm]]$meta
    if (is.null(meta)) next
    
    if (!is.null(meta$fallback_tunes)) {
      tunes[names(meta$fallback_tunes)] <- tunes[names(meta$fallback_tunes)] + meta$fallback_tunes
    }
    
    if (!is.null(meta$fallback_forecasts)) {
      forecasts[names(meta$fallback_forecasts)] <- forecasts[names(meta$fallback_forecasts)] + meta$fallback_forecasts
    }
    
    if (!is.null(meta$tcsr_benchmark_fallbacks)) {
      tcsr_benchmark_fallbacks <- tcsr_benchmark_fallbacks + meta$tcsr_benchmark_fallbacks
    }
    
    if (!is.null(meta$tcsr_total_forecasts)) {
      tcsr_total_forecasts <- tcsr_total_forecasts + meta$tcsr_total_forecasts
    }
  }
  
  tcsr_as_table <- data.frame(
    Model = "T-CSR",
    benchmark_fallbacks = as.integer(tcsr_benchmark_fallbacks),
    total_forecasts = as.integer(tcsr_total_forecasts),
    fallback_share = if (tcsr_total_forecasts > 0) tcsr_benchmark_fallbacks / tcsr_total_forecasts else NA_real_,
    row.names = NULL
  )
  
  list(
    fallback_tunes = tunes,
    fallback_forecasts = forecasts,
    as_table = data.frame(
      Model = names(tunes),
      fallback_tunes = as.integer(tunes),
      fallback_forecasts = as.integer(forecasts),
      row.names = NULL
    ),
    tcsr_as_table = tcsr_as_table
  )
}

# ============================================================
# OUTPUT TABLE HELPERS
# ============================================================

p_to_stars <- function(p) {
  ifelse(is.finite(p) & p < 0.10, "*", "")
}

make_fortin_table <- function(all_results,
                              targets,
                              horizons = c(1, 3, 6, 12),
                              digits = 2,
                              include_benchmark = TRUE) {
  stopifnot(is.list(all_results))
  stopifnot(all(targets %in% names(all_results)))
  
  models <- model_order_internal
  
  if (!include_benchmark) {
    models <- setdiff(models, benchmark_model)
  }
  
  out <- data.frame(
    Model = unname(model_display_names[models]),
    stringsAsFactors = FALSE
  )
  
  for (tg in targets) {
    for (h in horizons) {
      hh <- paste0("h", h)
      
      ratio_vec <- all_results[[tg]][[hh]]$ratio
      pval_vec  <- all_results[[tg]][[hh]]$pvals
      
      vals <- sapply(models, function(m) {
        r <- ratio_vec[m]
        p <- pval_vec[m]
        
        if (is.na(r)) return("")
        paste0(formatC(r, format = "f", digits = digits), p_to_stars(p))
      })
      
      out[[paste0(tg, "_h", h)]] <- unname(vals)
    }
  }
  
  out
}

save_fortin_table_grouped_tex <- function(all_results,
                                          targets,
                                          target_labels = NULL,
                                          horizons = c(1, 3, 6, 12),
                                          file,
                                          caption = NULL,
                                          label = NULL,
                                          digits_ratio = 2,
                                          digits_rmse = 2,
                                          note = "Note: This table reports the ratio of the root mean squared predictive error (RMSE) relative to the benchmark AR,BIC model. A single * indicates Diebold-Mariano significance at the 10\\% level.",
                                          highlight_color = "green!15",
                                          placement = "H",
                                          font_size = "\\tiny",
                                          tabcolsep = "1.5pt",
                                          arraystretch = "0.9") {
  stopifnot(is.list(all_results))
  stopifnot(all(targets %in% names(all_results)))
  
  if (is.null(target_labels)) {
    target_labels <- targets
  }
  
  stopifnot(length(target_labels) == length(targets))
  
  model_internal <- model_order_internal
  model_rows <- unname(model_display_names[model_internal])
  model_rows[model_internal == "AR,BIC"] <- "AR,BIC (RMSE)"
  
  n_targets <- length(targets)
  n_h <- length(horizons)
  n_cols <- n_targets * n_h
  
  fmt_num <- function(x, d) {
    if (is.na(x) || !is.finite(x)) return("")
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
      
      body[model_rows[model_internal == "AR,BIC"], col_index] <-
        fmt_num(all_results[[tg]][[hh]]$RMSE_bmk, digits_rmse)
      
      for (m in setdiff(model_internal, "AR,BIC")) {
        r <- all_results[[tg]][[hh]]$ratio[m]
        p <- all_results[[tg]][[hh]]$pvals[m]
        
        row_name <- model_display_names[m]
        
        ratio_vals[row_name, col_index] <- if (is.na(r) || !is.finite(r)) NA_real_ else r
        
        body[row_name, col_index] <- if (is.na(r) || !is.finite(r)) {
          ""
        } else {
          paste0(fmt_num(r, digits_ratio), p_to_stars(p))
        }
      }
      
      col_index <- col_index + 1
    }
  }
  
  benchmark_row <- "AR,BIC (RMSE)"
  non_benchmark_rows <- setdiff(rownames(body), benchmark_row)
  
  for (j in seq_len(n_cols)) {
    x <- ratio_vals[non_benchmark_rows, j]
    if (all(is.na(x))) next
    
    best_val <- min(x, na.rm = TRUE)
    best_rows <- non_benchmark_rows[!is.na(x) & x == best_val]
    
    for (rw in best_rows) {
      if (nzchar(body[rw, j])) {
        body[rw, j] <- paste0("\\cellcolor{", highlight_color, "}", body[rw, j])
      }
    }
  }
  
  align <- paste0("l", paste(rep("c", n_cols), collapse = ""))
  
  header1_parts <- c(" ")
  for (lab in target_labels) {
    header1_parts <- c(
      header1_parts,
      paste0("\\multicolumn{", n_h, "}{c}{", lab, "}")
    )
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
  
  full_caption <- caption
  
  if (!is.null(caption)) {
    full_caption <- paste0(
      caption,
      ". Table includes RMSE of AR,BIC, and the RMSE ratio of all other methods compared to the AR,BIC. Green cells indicate the lowest RMSE ratio within each target-horizon column, including ties."
    )
  }
  
  lines <- c(
    paste0("\\begin{table}[", placement, "]"),
    "\\centering",
    if (!is.null(full_caption)) paste0("\\caption{", full_caption, "}") else NULL,
    if (!is.null(label)) paste0("\\label{", label, "}") else NULL,
    font_size,
    paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"),
    paste0("\\renewcommand{\\arraystretch}{", arraystretch, "}"),
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
    if (!is.null(note)) {
      paste0(
        "\\vspace{0.25em}\n\n",
        "\\parbox{0.95\\textwidth}{\\footnotesize ",
        note,
        "}"
      )
    } else {
      NULL
    },
    "\\end{table}"
  )
  
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, con = file)
}
