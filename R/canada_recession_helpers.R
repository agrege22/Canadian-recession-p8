# ============================================================
# CANADA RECESSION HELPER FUNCTIONS
# ============================================================

# ============================================================
# HELPERS
# ============================================================

rec_normalize_pval_grid <- function(pvals) {
  pvals <- as.numeric(pvals)
  labs <- names(pvals)
  if (is.null(labs) || any(!nzchar(labs))) {
    labs <- as.character(round(100 * pvals))
  }
  stats::setNames(pvals, labs)
}

rec_clip_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  p[p < eps] <- eps
  p[p > 1 - eps] <- 1 - eps
  p
}

rec_safe_scale_newx <- function(X_new, center, scale) {
  if (is.null(X_new)) return(matrix(numeric(0), 1, length(center)))
  if (!is.matrix(X_new)) X_new <- matrix(as.numeric(X_new), nrow = 1)
  if (ncol(X_new) != length(center)) stop("X_new has incompatible number of columns")
  if (length(center) == 0) return(matrix(numeric(0), nrow(X_new), 0))
  sweep(sweep(X_new, 2, center, FUN = "-"), 2, scale, FUN = "/")
}

rec_prepare_penalized_data <- function(X, y) {
  y <- as.numeric(y)
  
  if (is.null(X)) X <- matrix(numeric(0), length(y), 0)
  if (!is.matrix(X)) X <- as.matrix(X)
  if (nrow(X) != length(y)) stop("nrow(X) must match length(y)")
  
  ok_x <- if (ncol(X) == 0) rep(TRUE, nrow(X)) else apply(X, 1, function(r) all(is.finite(r)))
  ok_y <- is.finite(y) & y %in% c(0, 1)
  ok <- ok_y & ok_x
  
  y2 <- y[ok]
  X2 <- X[ok, , drop = FALSE]
  
  if (length(y2) == 0) {
    return(list(
      X = X2,
      Xs = X2,
      y = y2,
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
  
  list(
    X = X2,
    Xs = Xs,
    y = y2,
    center = center,
    scale = scale,
    y_mean = mean(y2),
    ok = ok
  )
}

rec_ridge_df_path <- function(Xs, lambda) {
  if (ncol(Xs) == 0) return(rep(1, length(lambda)))
  d <- svd(Xs, nu = 0, nv = 0)$d
  1 + vapply(lambda, function(lam) {
    sum(d^2 / (d^2 + nrow(Xs) * lam))
  }, numeric(1))
}

rec_select_bic_glmnet_binom <- function(Z, y,
                                        alpha,
                                        penalty.factor = NULL,
                                        nlambda = rec_cfg$glmnet_nlambda,
                                        lambda = NULL) {
  prep <- tryCatch(rec_prepare_penalized_data(Z, y), error = function(e) NULL)
  if (is.null(prep)) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  n <- length(prep$y)
  p <- ncol(prep$Xs)
  
  if (n == 0) return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  
  if (length(unique(prep$y)) < 2) {
    return(list(lambda = rec_cfg$fallback_lambda, bic = Inf, df = 1))
  }
  
  if (p == 0) {
    p0 <- rec_clip_prob(rep(mean(prep$y), n))
    ll0 <- sum(prep$y * log(p0) + (1 - prep$y) * log(1 - p0))
    bic0 <- -2 * ll0 + log(n)
    return(list(lambda = 0, bic = bic0, df = 1))
  }
  
  if (!is.null(penalty.factor) && length(penalty.factor) != p) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  fit <- tryCatch(
    glmnet::glmnet(
      x = prep$Xs,
      y = prep$y,
      family = "binomial",
      alpha = alpha,
      lambda = lambda,
      nlambda = nlambda,
      standardize = FALSE,
      intercept = TRUE,
      penalty.factor = penalty.factor
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit) || length(fit$lambda) == 0) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  p_mat <- tryCatch(
    as.matrix(stats::predict(fit, newx = prep$Xs, type = "response")),
    error = function(e) NULL
  )
  
  if (is.null(p_mat)) {
    return(list(lambda = NA_real_, bic = Inf, df = NA_real_))
  }
  
  p_mat <- apply(p_mat, 2, rec_clip_prob)
  if (!is.matrix(p_mat)) p_mat <- matrix(p_mat, ncol = 1)
  
  ll <- colSums(
    matrix(prep$y, nrow = n, ncol = ncol(p_mat)) * log(p_mat) +
      matrix(1 - prep$y, nrow = n, ncol = ncol(p_mat)) * log(1 - p_mat)
  )
  
  if (alpha == 0) {
    k <- rec_ridge_df_path(prep$Xs, fit$lambda)
  } else {
    k <- fit$df + 1
  }
  
  bic <- -2 * ll + k * log(n)
  j <- which.min(bic)
  
  list(
    lambda = fit$lambda[j],
    bic = bic[j],
    df = k[j]
  )
}

rec_make_adalasso_weights_binom <- function(Z, y,
                                            nlambda = rec_cfg$glmnet_nlambda,
                                            eps = rec_cfg$adalasso_eps) {
  prep <- tryCatch(rec_prepare_penalized_data(Z, y), error = function(e) NULL)
  if (is.null(prep) || ncol(prep$Xs) == 0 || length(prep$y) == 0) {
    p0 <- if (is.null(Z)) 0 else ncol(as.matrix(Z))
    return(rep(1, p0))
  }
  
  ridge_bic <- rec_select_bic_glmnet_binom(
    Z = Z,
    y = y,
    alpha = 0,
    nlambda = nlambda
  )
  
  lambda0 <- ridge_bic$lambda
  if (!is.finite(lambda0) || lambda0 < 0) lambda0 <- rec_cfg$fallback_lambda
  
  fit0 <- tryCatch(
    glmnet::glmnet(
      x = prep$Xs,
      y = prep$y,
      family = "binomial",
      alpha = 0,
      lambda = lambda0,
      standardize = FALSE,
      intercept = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit0)) return(rep(1, ncol(prep$Xs)))
  
  b0 <- tryCatch(as.numeric(stats::coef(fit0))[-1], error = function(e) rep(0, ncol(prep$Xs)))
  w <- 1 / (abs(b0) + eps)
  w[!is.finite(w)] <- 1 / eps
  w
}

rec_tune_ridge_or_lasso_binom <- function(Z, y, alpha,
                                          nlambda = rec_cfg$glmnet_nlambda,
                                          fallback_lambda = rec_cfg$fallback_lambda) {
  out <- rec_select_bic_glmnet_binom(
    Z = Z,
    y = y,
    alpha = alpha,
    nlambda = nlambda
  )
  
  lam <- out$lambda
  if (!is.finite(lam) || lam < 0) lam <- fallback_lambda
  
  list(alpha = alpha, lambda = lam)
}

rec_tune_elasticnet_binom <- function(Z, y,
                                      alpha_grid = rec_cfg$elastic_alpha_grid,
                                      nlambda = rec_cfg$glmnet_nlambda,
                                      fallback_lambda = rec_cfg$fallback_lambda) {
  best <- list(bic = Inf, alpha = NA_real_, lambda = fallback_lambda)
  
  for (a in alpha_grid) {
    cur <- rec_select_bic_glmnet_binom(
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
  
  if (!is.finite(best$alpha)) best$alpha <- alpha_grid[1]
  if (!is.finite(best$lambda) || best$lambda < 0) best$lambda <- fallback_lambda
  
  best
}

rec_tune_adaptive_lasso_binom <- function(Z, y,
                                          nlambda = rec_cfg$glmnet_nlambda,
                                          eps = rec_cfg$adalasso_eps,
                                          fallback_lambda = rec_cfg$fallback_lambda) {
  p <- if (is.null(Z)) 0 else ncol(as.matrix(Z))
  w <- rec_make_adalasso_weights_binom(
    Z = Z,
    y = y,
    nlambda = nlambda,
    eps = eps
  )
  
  if (length(w) != p) w <- rep(1, p)
  
  ada_bic <- rec_select_bic_glmnet_binom(
    Z = Z,
    y = y,
    alpha = 1,
    penalty.factor = w,
    nlambda = nlambda
  )
  
  lam <- ada_bic$lambda
  if (!is.finite(lam) || lam < 0) lam <- fallback_lambda
  
  list(lambda = lam, weights = w)
}

rec_safe_probit_predict <- function(y_train, X_train, X_test) {
  y_train <- as.numeric(y_train)
  
  if (is.null(X_train)) X_train <- matrix(numeric(0), length(y_train), 0)
  if (is.null(X_test))  X_test  <- matrix(numeric(0), 1, 0)
  
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test))  X_test  <- matrix(as.numeric(X_test), nrow = 1)
  
  if (nrow(X_train) != length(y_train)) return(NA_real_)
  if (nrow(X_test) != 1) return(NA_real_)
  if (any(!is.finite(X_test))) return(NA_real_)
  
  ok_x <- if (ncol(X_train) == 0) rep(TRUE, nrow(X_train)) else apply(X_train, 1, function(r) all(is.finite(r)))
  ok <- (y_train %in% c(0, 1)) & is.finite(y_train) & ok_x
  
  y2 <- y_train[ok]
  X2 <- X_train[ok, , drop = FALSE]
  
  if (length(y2) == 0) return(NA_real_)
  
  pbar <- rec_clip_prob(mean(y2))
  
  if (length(unique(y2)) < 2) return(pbar)
  if (ncol(X_test) != ncol(X2)) return(NA_real_)
  
  fit <- tryCatch(
    suppressWarnings(
      glm(
        y2 ~ .,
        data = data.frame(y2 = y2, X2),
        family = binomial(link = "probit")
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(pbar)
  
  newdata <- data.frame(X_test)
  names(newdata) <- names(data.frame(X2))
  
  pred <- tryCatch(
    as.numeric(stats::predict(fit, newdata = newdata, type = "response")),
    error = function(e) NA_real_
  )
  
  rec_clip_prob(pred)
}

rec_safe_logit_predict <- function(y_train, X_train, X_test) {
  y_train <- as.numeric(y_train)
  
  if (is.null(X_train)) X_train <- matrix(numeric(0), length(y_train), 0)
  if (is.null(X_test))  X_test  <- matrix(numeric(0), 1, 0)
  
  if (!is.matrix(X_train)) X_train <- as.matrix(X_train)
  if (!is.matrix(X_test))  X_test  <- matrix(as.numeric(X_test), nrow = 1)
  
  if (nrow(X_train) != length(y_train)) return(NA_real_)
  if (nrow(X_test) != 1) return(NA_real_)
  if (any(!is.finite(X_test))) return(NA_real_)
  
  ok_x <- if (ncol(X_train) == 0) rep(TRUE, nrow(X_train)) else apply(X_train, 1, function(r) all(is.finite(r)))
  ok <- (y_train %in% c(0, 1)) & is.finite(y_train) & ok_x
  
  y2 <- y_train[ok]
  X2 <- X_train[ok, , drop = FALSE]
  
  if (length(y2) == 0) return(NA_real_)
  
  pbar <- rec_clip_prob(mean(y2))
  
  if (length(unique(y2)) < 2) return(pbar)
  if (ncol(X2) == 0) return(pbar)
  if (ncol(X_test) != ncol(X2)) return(NA_real_)
  
  fit <- tryCatch(
    suppressWarnings(
      glm.fit(
        x = cbind(1, X2),
        y = y2,
        family = binomial()
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit) || is.null(fit$coefficients)) return(pbar)
  
  beta <- fit$coefficients
  beta[!is.finite(beta)] <- 0
  
  eta <- tryCatch(
    sum(c(1, as.numeric(X_test)) * beta),
    error = function(e) NA_real_
  )
  
  if (!is.finite(eta)) return(pbar)
  
  rec_clip_prob(plogis(eta))
}

rec_fit_predict_glmnet_binom <- function(Z_train, y_train, Z_test, spec) {
  if (is.null(spec) || is.null(spec$type)) return(NA_real_)
  if (is.null(spec$lambda) || !is.finite(spec$lambda) || spec$lambda < 0) return(NA_real_)
  
  prep <- tryCatch(rec_prepare_penalized_data(Z_train, y_train), error = function(e) NULL)
  if (is.null(prep)) return(NA_real_)
  if (length(prep$y) == 0) return(NA_real_)
  
  pbar <- rec_clip_prob(mean(prep$y))
  if (length(unique(prep$y)) < 2) return(pbar)
  
  if (is.null(Z_test)) Z_test <- matrix(numeric(0), 1, 0)
  if (!is.matrix(Z_test)) Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  if (nrow(Z_test) != 1) return(NA_real_)
  if (any(!is.finite(Z_test))) return(NA_real_)
  if (ncol(Z_test) != ncol(prep$X)) return(NA_real_)
  
  if (ncol(prep$Xs) == 0) return(pbar)
  
  Zs_test <- tryCatch(
    rec_safe_scale_newx(Z_test, prep$center, prep$scale),
    error = function(e) NULL
  )
  if (is.null(Zs_test)) return(NA_real_)
  
  fit <- tryCatch(
    switch(
      spec$type,
      ridge = glmnet::glmnet(
        prep$Xs, prep$y,
        family = "binomial",
        alpha = 0,
        lambda = spec$lambda,
        standardize = FALSE,
        intercept = TRUE
      ),
      lasso = glmnet::glmnet(
        prep$Xs, prep$y,
        family = "binomial",
        alpha = 1,
        lambda = spec$lambda,
        standardize = FALSE,
        intercept = TRUE
      ),
      elastic = {
        if (is.null(spec$alpha) || !is.finite(spec$alpha)) stop("Missing elastic alpha")
        glmnet::glmnet(
          prep$Xs, prep$y,
          family = "binomial",
          alpha = spec$alpha,
          lambda = spec$lambda,
          standardize = FALSE,
          intercept = TRUE
        )
      },
      adalasso = {
        if (is.null(spec$weights) || length(spec$weights) != ncol(prep$Xs)) {
          stop("Invalid adaptive-lasso weights")
        }
        glmnet::glmnet(
          prep$Xs, prep$y,
          family = "binomial",
          alpha = 1,
          lambda = spec$lambda,
          standardize = FALSE,
          intercept = TRUE,
          penalty.factor = spec$weights
        )
      },
      stop("Unknown glmnet spec$type: ", spec$type)
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(pbar)
  
  pred <- tryCatch(
    as.numeric(stats::predict(fit, newx = Zs_test, s = spec$lambda, type = "response")),
    error = function(e) NA_real_
  )
  
  rec_clip_prob(pred)
}

rec_get_selected_idx <- function(selected, p) {
  idx <- integer(0)
  if (!is.null(selected)) {
    if (is.logical(selected)) idx <- which(selected) else idx <- unique(as.integer(selected))
    idx <- idx[is.finite(idx) & idx >= 1 & idx <= p]
  }
  idx
}

rec_build_post_selection_train_test <- function(Xcand_train, Xcand_test,
                                                Xcond_train = NULL, Xcond_test = NULL,
                                                selected = NULL) {
  if (!is.matrix(Xcand_train)) Xcand_train <- as.matrix(Xcand_train)
  if (!is.matrix(Xcand_test))  Xcand_test  <- as.matrix(Xcand_test)
  
  p <- ncol(Xcand_train)
  idx <- rec_get_selected_idx(selected, p)
  
  Xsel_train <- if (length(idx) > 0) Xcand_train[, idx, drop = FALSE] else matrix(numeric(0), nrow(Xcand_train), 0)
  Xsel_test  <- if (length(idx) > 0) Xcand_test[, idx, drop = FALSE]  else matrix(numeric(0), nrow(Xcand_test), 0)
  
  if (is.null(Xcond_train)) Xcond_train <- matrix(numeric(0), nrow(Xcand_train), 0)
  if (is.null(Xcond_test))  Xcond_test  <- matrix(numeric(0), nrow(Xcand_test), 0)
  
  if (!is.matrix(Xcond_train)) Xcond_train <- as.matrix(Xcond_train)
  if (!is.matrix(Xcond_test))  Xcond_test  <- as.matrix(Xcond_test)
  
  list(
    X_train = cbind(Xcond_train, Xsel_train),
    X_test  = cbind(Xcond_test,  Xsel_test),
    idx = idx
  )
}

rec_predict_from_selection <- function(y_train,
                                       Xcand_train, Xcand_test,
                                       Xcond_train = NULL, Xcond_test = NULL,
                                       selected = NULL) {
  mats <- rec_build_post_selection_train_test(
    Xcand_train = Xcand_train,
    Xcand_test = Xcand_test,
    Xcond_train = Xcond_train,
    Xcond_test = Xcond_test,
    selected = selected
  )
  
  rec_safe_logit_predict(y_train, mats$X_train, mats$X_test)
}

rec_predict_from_farm_selection <- function(y_train,
                                            Xcand_train,
                                            Xcand_test,
                                            Xcond_train,
                                            Xcond_test,
                                            farm_obj) {
  selected <- integer(0)
  
  if (!is.null(farm_obj) && !is.null(farm_obj$selected_idx)) {
    selected <- farm_obj$selected_idx
  }
  
  rec_predict_from_selection(
    y_train = y_train,
    Xcand_train = Xcand_train,
    Xcand_test = Xcand_test,
    Xcond_train = Xcond_train,
    Xcond_test = Xcond_test,
    selected = selected
  )
}

# ============================================================
# VARIABLE SELECTION MODELS
# ============================================================

rec_select_ocmt <- function(y_train, Xcand_train, Xcond_train,
                            pval = 0.05,
                            delta1 = rec_cfg$ocmt$delta1,
                            delta2 = rec_cfg$ocmt$delta2,
                            multi_stage = rec_cfg$ocmt$multi_stage,
                            max_iter = rec_cfg$ocmt$max_iter,
                            kappa_const = rec_cfg$ocmt$kappa_const) {
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
      link = "logit",
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(out) || is.null(out$ind)) return(rep(FALSE, p))
  as.logical(out$ind)
}

rec_select_bmt <- function(y_train, Xcand_train, Xcond_train,
                           pval = 0.05,
                           delta1 = rec_cfg$bmt$delta1,
                           delta2 = rec_cfg$bmt$delta2,
                           maxit = rec_cfg$bmt$maxit) {
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
      link = "logit",
      maxit = maxit
    ),
    error = function(e) NULL
  )
  
  if (is.null(out)) return(rep(FALSE, p))
  as.logical(out)
}

# ============================================================
# FARMSELECT NATIVE LOGIT
# ============================================================

rec_fit_farm_native_logit <- function(y_train, Z_train,
                                      loss = rec_cfg$farm$loss,
                                      robust = rec_cfg$farm$robust,
                                      cv = rec_cfg$farm$cv,
                                      tau = rec_cfg$farm$tau,
                                      K.factors = rec_cfg$farm$K.factors,
                                      max.iter = rec_cfg$farm$max.iter,
                                      nfolds = rec_cfg$farm$nfolds,
                                      eps = rec_cfg$farm$eps) {
  if (!is.matrix(Z_train)) Z_train <- as.matrix(Z_train)
  
  ok_x <- if (ncol(Z_train) == 0) {
    rep(TRUE, nrow(Z_train))
  } else {
    apply(Z_train, 1, function(r) all(is.finite(r)))
  }
  
  ok_y <- is.finite(y_train) & y_train %in% c(0, 1)
  ok <- ok_x & ok_y
  
  y2 <- as.numeric(y_train[ok])
  Z2 <- Z_train[ok, , drop = FALSE]
  
  pbar <- if (length(y2) == 0) NA_real_ else rec_clip_prob(mean(y2))
  
  if (length(y2) == 0 || ncol(Z2) == 0 || min(nrow(Z2), ncol(Z2)) <= 4 || length(unique(y2)) < 2) {
    return(list(
      use_mean = TRUE,
      p_mean = pbar,
      fit = NULL,
      selected_idx = integer(0)
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
      lin.reg = FALSE,
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
      p_mean = pbar,
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
    p_mean = pbar,
    fit = fit,
    selected_idx = idx
  )
}

rec_predict_farm_native_logit <- function(farm_obj, Z_test) {
  if (is.null(farm_obj)) return(NA_real_)
  if (!is.matrix(Z_test)) Z_test <- matrix(as.numeric(Z_test), nrow = 1)
  
  if (nrow(Z_test) != 1) return(NA_real_)
  if (!is.finite(farm_obj$p_mean)) return(NA_real_)
  
  if (isTRUE(farm_obj$use_mean) || is.null(farm_obj$fit)) {
    return(as.numeric(farm_obj$p_mean))
  }
  
  pred <- tryCatch(
    as.numeric(predict(farm_obj$fit, Z_test, type = "response")),
    error = function(e) NA_real_
  )
  
  if (length(pred) == 0 || !is.finite(pred[1])) {
    return(as.numeric(farm_obj$p_mean))
  }
  
  rec_clip_prob(pred[1])
}

# ============================================================
# DATA / BIC / PCA
# ============================================================

rec_as_numeric_df <- function(df, date_col = "Date") {
  out <- df
  out[[date_col]] <- as.Date(out[[date_col]])
  num_cols <- setdiff(names(out), date_col)
  out[num_cols] <- lapply(out[num_cols], function(x) suppressWarnings(as.numeric(x)))
  out
}

rec_make_yh_from_binary <- function(y, h) {
  y <- as.numeric(y)
  n <- length(y)
  out <- rep(NA_real_, n)
  if (h <= 0) stop("h must be >= 1")
  if (n <= h) return(out)
  for (t in 1:(n - h)) out[t] <- y[t + h]
  out
}

rec_add_lags <- function(df, var, p, prefix = NULL) {
  stopifnot(p >= 1)
  x <- df[[var]]
  base <- if (is.null(prefix)) var else prefix
  for (L in 0:(p - 1)) df[[paste0(base, "_L", L)]] <- dplyr::lag(x, L)
  df
}

rec_add_lags_many <- function(df, vars, p) {
  for (v in vars) df <- rec_add_lags(df, v, p, prefix = v)
  df
}

rec_bic_glm_binom <- function(y, X) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  
  ok_x <- if (ncol(X) == 0) rep(TRUE, nrow(X)) else apply(X, 1, function(r) all(is.finite(r)))
  ok <- (y %in% c(0, 1)) & is.finite(y) & ok_x
  
  y2 <- y[ok]
  X2 <- X[ok, , drop = FALSE]
  
  if (length(y2) == 0 || length(unique(y2)) < 2) return(list(bic = Inf, coef = NULL))
  
  fit <- tryCatch(
    suppressWarnings(
      glm.fit(
        x = cbind(1, X2),
        y = y2,
        family = binomial()
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit) || is.null(fit$coefficients)) return(list(bic = Inf, coef = NULL))
  
  beta <- fit$coefficients
  beta[!is.finite(beta)] <- 0
  
  p_hat <- rec_clip_prob(plogis(drop(cbind(1, X2) %*% beta)))
  ll <- sum(y2 * log(p_hat) + (1 - y2) * log(1 - p_hat))
  
  k <- sum(is.finite(fit$coefficients))
  bic <- -2 * ll + k * log(length(y2))
  
  list(bic = bic, coef = beta)
}

rec_pca_fit <- function(X, Kmax = 10) {
  X <- as.matrix(X)
  
  if (nrow(X) < 2 || ncol(X) < 1) return(NULL)
  
  mu <- colMeans(X, na.rm = TRUE)
  mu[!is.finite(mu)] <- 0
  
  sdv <- apply(X, 2, sd, na.rm = TRUE)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  
  X_imp <- X
  for (j in seq_len(ncol(X_imp))) {
    bad <- !is.finite(X_imp[, j])
    if (any(bad)) X_imp[bad, j] <- mu[j]
  }
  
  Xs <- scale(X_imp, center = mu, scale = sdv)
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

rec_fit_predict_AR_BIC_logit <- function(train_df, test_row, py_max = rec_cfg$py) {
  pbar <- rec_clip_prob(mean(train_df$y_h, na.rm = TRUE))
  
  bics <- rep(Inf, py_max)
  coefs <- vector("list", py_max)
  
  for (p in 1:py_max) {
    cols <- paste0("y_L", 0:(p - 1))
    d <- train_df[, c("y_h", cols), drop = FALSE]
    d <- d[complete.cases(d), , drop = FALSE]
    if (nrow(d) < (p + 25)) next
    if (length(unique(d$y_h)) < 2) next
    
    X <- as.matrix(d[, cols, drop = FALSE])
    y <- d$y_h
    tmp <- rec_bic_glm_binom(y, X)
    bics[p] <- tmp$bic
    coefs[[p]] <- tmp$coef
  }
  
  p_best <- which.min(bics)
  if (!is.finite(bics[p_best])) return(pbar)
  
  cols <- paste0("y_L", 0:(p_best - 1))
  x_test <- as.numeric(test_row[, cols, drop = FALSE])
  beta <- coefs[[p_best]]
  if (is.null(beta)) return(pbar)
  
  rec_clip_prob(plogis(sum(c(1, x_test) * beta)))
}

rec_fit_predict_ARDI_BIC_logit <- function(hist_df, train_df, test_row,
                                           x_names, py_max = rec_cfg$py, pf_max = rec_cfg$px, Kmax = rec_cfg$K_fixed) {
  pbar <- rec_clip_prob(mean(hist_df$y_h, na.rm = TRUE))
  
  X_hist <- as.matrix(hist_df[, x_names, drop = FALSE])
  pca <- rec_pca_fit(X_hist, Kmax = Kmax)
  if (is.null(pca)) return(pbar)
  
  F_hist <- pca$scores
  K_avail <- ncol(F_hist)
  if (K_avail == 0) return(pbar)
  
  colnames(F_hist) <- paste0("F", seq_len(K_avail))
  
  hf <- dplyr::bind_cols(hist_df, as.data.frame(F_hist))
  for (k in seq_len(K_avail)) {
    for (L in 0:(pf_max - 1)) {
      hf[[paste0("F", k, "_L", L)]] <- dplyr::lag(hf[[paste0("F", k)]], L)
    }
  }
  
  train_dates <- train_df$Date
  hf_train <- hf %>% dplyr::filter(Date %in% train_dates)
  
  best <- list(bic = Inf, beta = NULL, cols = NULL)
  
  for (py in 1:py_max) {
    ycols <- paste0("y_L", 0:(py - 1))
    for (pf in 1:pf_max) {
      for (K in 1:K_avail) {
        fcols <- c()
        for (k in 1:K) fcols <- c(fcols, paste0("F", k, "_L", 0:(pf - 1)))
        cols <- c(ycols, fcols)
        d <- hf_train[, c("y_h", cols), drop = FALSE]
        d <- d[complete.cases(d), , drop = FALSE]
        if (nrow(d) < (length(cols) + 25)) next
        if (length(unique(d$y_h)) < 2) next
        
        X <- as.matrix(d[, cols, drop = FALSE])
        y <- d$y_h
        tmp <- rec_bic_glm_binom(y, X)
        
        if (is.finite(tmp$bic) && tmp$bic < best$bic) {
          best$bic <- tmp$bic
          best$beta <- tmp$coef
          best$cols <- cols
        }
      }
    }
  }
  
  if (!is.finite(best$bic)) return(pbar)
  
  hf_test <- hf %>% dplyr::filter(Date == test_row$Date)
  if (nrow(hf_test) != 1) return(pbar)
  
  x_test <- as.numeric(hf_test[, best$cols, drop = FALSE])
  rec_clip_prob(plogis(sum(c(1, x_test) * best$beta)))
}

# ============================================================
# RANDOM FOREST / T-CSR
# ============================================================

rec_fit_predict_rf <- function(Z_train, y_train, Z_test,
                               num_trees = rec_cfg$rf$num_trees,
                               min_node_size = rec_cfg$rf$min_node_size) {
  if (!is.matrix(Z_train)) Z_train <- as.matrix(Z_train)
  if (!is.matrix(Z_test))  Z_test  <- as.matrix(Z_test)
  
  y_train <- as.numeric(y_train)
  
  ok_x <- if (ncol(Z_train) == 0) rep(TRUE, nrow(Z_train)) else apply(Z_train, 1, function(r) all(is.finite(r)))
  ok <- is.finite(y_train) & y_train %in% c(0, 1) & ok_x
  
  Z2 <- Z_train[ok, , drop = FALSE]
  y2 <- y_train[ok]
  
  if (length(y2) == 0) return(NA_real_)
  pbar <- rec_clip_prob(mean(y2))
  if (length(unique(y2)) < 2) return(pbar)
  if (any(!is.finite(Z_test))) return(NA_real_)
  
  df_tr <- data.frame(y = base::factor(y2, levels = c(0, 1)), Z2)
  mtry <- max(1, floor(max(1, ncol(Z2)) / 3))
  
  fit <- tryCatch(
    ranger::ranger(
      y ~ .,
      data = df_tr,
      probability = TRUE,
      num.trees = num_trees,
      mtry = mtry,
      min.node.size = min_node_size,
      verbose = FALSE,
      num.threads = 1
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(pbar)
  
  pred <- tryCatch(
    stats::predict(fit, data = data.frame(Z_test))$predictions[, "1"],
    error = function(e) NA_real_
  )
  
  rec_clip_prob(pred)
}

rec_tcsr_screen <- function(train_df, x_names, zc = rec_cfg$tcsr$tc) {
  keep <- c()
  base_cols <- intersect(paste0("y_L", 0:3), names(train_df))
  
  for (x in x_names) {
    cols <- c("y_h", base_cols, x)
    d <- train_df[, cols, drop = FALSE]
    d <- d[complete.cases(d), , drop = FALSE]
    if (nrow(d) < 50) next
    if (length(unique(d$y_h)) < 2) next
    
    fit <- tryCatch(
      suppressWarnings(
        glm(
          y_h ~ .,
          data = d,
          family = binomial()
        )
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    sm <- summary(fit)
    if (x %in% rownames(coef(sm))) {
      zval <- coef(sm)[x, "z value"]
      if (is.finite(zval) && abs(zval) > zc) keep <- c(keep, x)
    }
  }
  
  unique(keep)
}

rec_tcsr_predict <- function(train_df, test_row, xstar,
                             L = rec_cfg$tcsr$L, M = rec_cfg$tcsr$M, seed = 1) {
  set.seed(seed)
  if (length(xstar) == 0) return(NA_real_)
  
  y_tr <- train_df$y_h
  y0   <- train_df$y_L0
  
  X_tr <- as.matrix(train_df[, xstar, drop = FALSE])
  X_te <- as.numeric(test_row[, xstar, drop = FALSE])
  
  ok_x <- apply(X_tr, 1, function(r) all(is.finite(r)))
  ok <- is.finite(y_tr) & y_tr %in% c(0, 1) & is.finite(y0) & ok_x
  
  y_tr <- y_tr[ok]
  y0   <- y0[ok]
  X_tr <- X_tr[ok, , drop = FALSE]
  
  if (length(y_tr) == 0) return(NA_real_)
  pbar <- rec_clip_prob(mean(y_tr))
  if (length(unique(y_tr)) < 2) return(pbar)
  
  preds <- numeric(M)
  
  for (m in 1:M) {
    if (length(xstar) >= L) {
      idx <- sample(seq_along(xstar), size = L, replace = FALSE)
    } else {
      idx <- sample(seq_along(xstar), size = L, replace = TRUE)
    }
    
    Zm_tr <- cbind(1, y0, X_tr[, idx, drop = FALSE])
    
    fit <- tryCatch(
      suppressWarnings(glm.fit(Zm_tr, y_tr, family = binomial())),
      error = function(e) NULL
    )
    
    if (is.null(fit) || is.null(fit$coefficients)) {
      preds[m] <- NA_real_
      next
    }
    
    beta <- fit$coefficients
    if (any(!is.finite(beta))) {
      preds[m] <- NA_real_
      next
    }
    
    Zm_te <- c(1, test_row$y_L0, X_te[idx])
    eta <- sum(Zm_te * beta)
    preds[m] <- rec_clip_prob(plogis(eta))
  }
  
  preds <- preds[is.finite(preds)]
  if (length(preds) == 0) return(NA_real_)
  mean(preds)
}

# ============================================================
# LOSSES / AUC / PSEUDO-R2 / DM
# ============================================================

rec_qps_vec <- function(y, p) {
  2 * (p - y)^2
}

rec_lps_vec <- function(y, p) {
  p <- rec_clip_prob(p)
  -(y * log(p) + (1 - y) * log(1 - p))
}

rec_loglik_sum <- function(y, p) {
  p <- rec_clip_prob(p)
  sum(y * log(p) + (1 - y) * log(1 - p))
}

rec_auc_safe <- function(y, p) {
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

rec_mcfadden_r2 <- function(y, p_model, p_null) {
  y <- as.numeric(y)
  p_model <- as.numeric(p_model)
  p_null  <- as.numeric(p_null)
  
  ok <- is.finite(y) & is.finite(p_model) & is.finite(p_null)
  y <- y[ok]
  p_model <- p_model[ok]
  p_null <- p_null[ok]
  
  if (length(y) == 0) return(NA_real_)
  
  ll_m <- rec_loglik_sum(y, p_model)
  ll_0 <- rec_loglik_sum(y, p_null)
  
  if (!is.finite(ll_m) || !is.finite(ll_0) || ll_0 == 0) return(NA_real_)
  
  1 - (ll_m / ll_0)
}

rec_estrella_r2 <- function(y, p_model, p_null) {
  y <- as.numeric(y)
  p_model <- as.numeric(p_model)
  p_null  <- as.numeric(p_null)
  
  ok <- is.finite(y) & is.finite(p_model) & is.finite(p_null)
  y <- y[ok]
  p_model <- p_model[ok]
  p_null <- p_null[ok]
  
  n <- length(y)
  if (n == 0) return(NA_real_)
  
  ll_m <- rec_loglik_sum(y, p_model)
  ll_0 <- rec_loglik_sum(y, p_null)
  
  if (!is.finite(ll_m) || !is.finite(ll_0) || ll_0 >= 0) return(NA_real_)
  
  ratio <- ll_m / ll_0
  expo  <- -2 * ll_0 / n
  
  if (!is.finite(ratio) || !is.finite(expo) || ratio < 0) return(NA_real_)
  
  1 - ratio^expo
}

rec_dm_test_loss <- function(loss_model, loss_bench, h) {
  loss_model <- as.numeric(loss_model)
  loss_bench <- as.numeric(loss_bench)
  
  ok <- is.finite(loss_model) & is.finite(loss_bench)
  d <- loss_model[ok] - loss_bench[ok]
  
  n <- length(d)
  if (n < max(10, h + 1)) return(NA_real_)
  
  dbar <- mean(d)
  if (!is.finite(dbar)) return(NA_real_)
  
  max_lag <- min(h - 1, n - 1)
  gamma0 <- mean((d - dbar)^2)
  
  if (!is.finite(gamma0)) return(NA_real_)
  
  lrv <- gamma0
  
  if (max_lag >= 1) {
    for (lag in 1:max_lag) {
      w <- 1 - lag / (max_lag + 1)
      g <- mean((d[(lag + 1):n] - dbar) * (d[1:(n - lag)] - dbar))
      if (is.finite(g)) lrv <- lrv + 2 * w * g
    }
  }
  
  if (!is.finite(lrv) || lrv <= 0) return(NA_real_)
  
  stat <- dbar / sqrt(lrv / n)
  2 * stats::pnorm(abs(stat), lower.tail = FALSE)
}

rec_star_p10 <- function(p) {
  ifelse(is.finite(p) & p < 0.10, "*", "")
}

# ============================================================
# SIMPLE AVERAGE
# ============================================================

rec_simple_average <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

# ============================================================
# BLOCK / TASK HELPERS
# ============================================================

rec_months_between <- function(a, b) {
  12L * (lubridate::year(b) - lubridate::year(a)) +
    (lubridate::month(b) - lubridate::month(a))
}

rec_model_names_full <- function(ocmt_pvals = rec_cfg$ocmt$pvals,
                                 bmt_pvals  = rec_cfg$bmt$pvals) {
  ocmt_pvals <- rec_normalize_pval_grid(ocmt_pvals)
  bmt_pvals  <- rec_normalize_pval_grid(bmt_pvals)
  
  c(
    "Static-Probit-Factors",
    "AR,BIC", "ARDI,BIC",
    "Ridge-X", "Lasso-X", "Elastic-Net-X", "Adaptive-Lasso-X",
    paste0("OCMT_", names(ocmt_pvals)),
    paste0("BMT_", names(bmt_pvals)),
    "FarmSelect",
    "RF-X",
    "ARDI,Ridge", "ARDI,Lasso", "ARDI,Elastic-Net", "ARDI,Adaptive-Lasso",
    "RFARDI",
    "T-CSR",
    "AVG"
  )
}

# ============================================================
# PREPARE RECESSION TASK FRAME
# ============================================================

rec_prepare_task_frame <- function(panel_rec_model, rec_target, h,
                                   oos_start = rec_cfg$oos_start,
                                   oos_end   = rec_cfg$oos_end,
                                   py = rec_cfg$py,
                                   px = rec_cfg$px,
                                   training_months = rec_cfg$training_months,
                                   retune_every_months = rec_cfg$retune_every_months,
                                   ocmt_pvals = rec_cfg$ocmt$pvals,
                                   bmt_pvals  = rec_cfg$bmt$pvals) {
  stopifnot("Date" %in% names(panel_rec_model))
  stopifnot(rec_target %in% names(panel_rec_model))
  
  df <- rec_as_numeric_df(panel_rec_model, "Date") %>%
    dplyr::arrange(Date)
  
  x_names <- setdiff(setdiff(names(df), "Date"), rec_target)
  
  df$y <- df[[rec_target]]
  df$y_h <- rec_make_yh_from_binary(df$y, h = h)
  
  df_l <- df %>%
    dplyr::select(Date, y, y_h, dplyr::all_of(x_names)) %>%
    rec_add_lags("y", p = py, prefix = "y")
  
  df_x <- df_l %>%
    rec_add_lags_many(x_names, p = px) %>%
    dplyr::filter(!is.na(y_h), Date <= oos_end)
  
  ycols <- paste0("y_L", 0:(py - 1))
  xlagcols <- unlist(lapply(x_names, function(v) paste0(v, "_L", 0:(px - 1))))
  req_cols <- c("y_h", ycols, xlagcols)
  
  df_x <- df_x[complete.cases(df_x[, req_cols, drop = FALSE]), , drop = FALSE]
  
  eval_idx <- which(df_x$Date >= oos_start & df_x$Date <= oos_end)
  
  eligible_idx <- integer(0)
  blocks <- list()
  
  if (nrow(df_x) > 0 && length(eval_idx) > 0) {
    cutoff_dates <- lubridate::`%m-%`(
      df_x$Date[eval_idx],
      lubridate::period(months = h)
    )
    
    n_train_each <- findInterval(cutoff_dates, df_x$Date)
    eligible_idx <- eval_idx[n_train_each >= training_months]
    
    if (length(eligible_idx) > 0) {
      block_list <- list()
      cur_block <- eligible_idx[1]
      block_start_date <- df_x$Date[eligible_idx[1]]
      
      if (length(eligible_idx) >= 2) {
        for (idx in eligible_idx[-1]) {
          if (rec_months_between(block_start_date, df_x$Date[idx]) >= retune_every_months) {
            block_list[[length(block_list) + 1L]] <- cur_block
            cur_block <- idx
            block_start_date <- df_x$Date[idx]
          } else {
            cur_block <- c(cur_block, idx)
          }
        }
      }
      
      block_list[[length(block_list) + 1L]] <- cur_block
      blocks <- block_list
    }
  }
  
  cat("\n==============================\n")
  cat("Recession task frame check\n")
  cat("Target:                  ", rec_target, "\n")
  cat("h:                       ", h, "\n")
  cat("First row in df_x:        ", if (nrow(df_x) > 0) as.character(min(df_x$Date)) else "none", "\n")
  cat("OOS start:                ", as.character(oos_start), "\n")
  cat("OOS end:                  ", as.character(oos_end), "\n")
  cat("Rows in df_x:             ", nrow(df_x), "\n")
  cat("Evaluation rows:          ", length(eval_idx), "\n")
  cat("Eligible forecast rows:   ", length(eligible_idx), "\n")
  cat("First eligible forecast:  ", if (length(eligible_idx) > 0) as.character(df_x$Date[min(eligible_idx)]) else "none", "\n")
  cat("First training date used: ", if (nrow(df_x) > 0) as.character(min(df_x$Date)) else "none", "\n")
  cat("==============================\n\n")
  
  list(
    df_full = df,
    hist_base = df_l,
    df_x = df_x,
    x_names = x_names,
    ycols = ycols,
    xlagcols = xlagcols,
    eval_idx = eval_idx,
    eligible_idx = eligible_idx,
    blocks = blocks,
    models = rec_model_names_full(ocmt_pvals = ocmt_pvals, bmt_pvals = bmt_pvals)
  )
}

# ============================================================
# ONE BLOCK: ONE TARGET, ONE HORIZON, ONE RETUNE BLOCK
# ============================================================

rec_run_one_target_one_horizon_block <- function(panel_rec_model, rec_target,
                                                 h,
                                                 rows_to_run,
                                                 block_id = 1L,
                                                 n_blocks = 1L,
                                                 run_id = rec_target,
                                                 oos_start = rec_cfg$oos_start,
                                                 oos_end   = rec_cfg$oos_end,
                                                 py = rec_cfg$py,
                                                 px = rec_cfg$px,
                                                 K_fixed = rec_cfg$K_fixed,
                                                 retune_every_months = rec_cfg$retune_every_months,
                                                 training_months = rec_cfg$training_months,
                                                 tcsr_M = rec_cfg$tcsr$M,
                                                 rf_num_trees = rec_cfg$rf$num_trees,
                                                 ocmt_pvals = rec_cfg$ocmt$pvals,
                                                 bmt_pvals = rec_cfg$bmt$pvals,
                                                 p = NULL) {
  ocmt_pvals <- rec_normalize_pval_grid(ocmt_pvals)
  bmt_pvals  <- rec_normalize_pval_grid(bmt_pvals)
  
  prep <- rec_prepare_task_frame(
    panel_rec_model = panel_rec_model,
    rec_target = rec_target,
    h = h,
    oos_start = oos_start,
    oos_end = oos_end,
    py = py,
    px = px,
    training_months = training_months,
    retune_every_months = retune_every_months,
    ocmt_pvals = ocmt_pvals,
    bmt_pvals = bmt_pvals
  )
  
  hist_base <- prep$hist_base
  df_x      <- prep$df_x
  x_names   <- prep$x_names
  ycols     <- prep$ycols
  xlagcols  <- prep$xlagcols
  models    <- prep$models
  
  if (length(rows_to_run) == 0 || nrow(df_x) == 0) {
    return(list(
      target = rec_target,
      h = h,
      block_id = block_id,
      rows = integer(0),
      dates = as.Date(character(0)),
      y_true = numeric(0),
      probs = as.data.frame(matrix(numeric(0), 0, length(models), dimnames = list(NULL, models))),
      null_prob = numeric(0),
      tcsr_fallback_n = 0L,
      tcsr_forecast_n = 0L
    ))
  }
  
  pred_tbl <- as.data.frame(matrix(NA_real_, nrow = length(rows_to_run), ncol = length(models)))
  names(pred_tbl) <- models
  null_prob <- rep(NA_real_, length(rows_to_run))
  
  need_retune <- function(last, now) {
    is.na(last) || rec_months_between(last, now) >= retune_every_months
  }
  
  last_tune <- list(
    ridge = as.Date(NA),
    lasso = as.Date(NA),
    elastic = as.Date(NA),
    adalasso = as.Date(NA),
    ocmt = stats::setNames(as.list(rep(as.Date(NA), length(paste0("OCMT_", names(ocmt_pvals))))), paste0("OCMT_", names(ocmt_pvals))),
    bmt = stats::setNames(as.list(rep(as.Date(NA), length(paste0("BMT_", names(bmt_pvals))))), paste0("BMT_", names(bmt_pvals))),
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
  
  tcsr_fallback_n <- 0L
  tcsr_forecast_n <- 0L
  
  for (kk in seq_along(rows_to_run)) {
    i <- rows_to_run[kk]
    t0 <- df_x$Date[i]
    cutoff <- lubridate::`%m-%`(t0, lubridate::period(months = h))
    
    train_idx <- which(df_x$Date <= cutoff)
    if (length(train_idx) < training_months) next
    
    train_df <- df_x[train_idx, , drop = FALSE]
    test_row <- df_x[i, , drop = FALSE]
    
    pred_tbl[kk, "AR,BIC"] <- rec_fit_predict_AR_BIC_logit(train_df, test_row, py_max = py)
    
    hist_df <- hist_base %>% dplyr::filter(Date <= t0)
    
    # ------------------------------------------------------------
    # STATIC PROBIT WITH FACTORS
    # ------------------------------------------------------------
    X_hist_pf <- as.matrix(hist_df[, x_names, drop = FALSE])
    pca_pf <- rec_pca_fit(X_hist_pf, Kmax = factor_probit_k)
    
    if (!is.null(pca_pf)) {
      F_hist_pf <- pca_pf$scores
      K_pf <- min(factor_probit_k, ncol(F_hist_pf))
      
      if (K_pf > 0) {
        colnames(F_hist_pf) <- paste0("F", seq_len(ncol(F_hist_pf)))
        
        pf_df <- dplyr::bind_cols(
          hist_df %>% dplyr::select(Date, y_h),
          as.data.frame(F_hist_pf)
        )
        
        pf_train <- pf_df %>% dplyr::filter(Date %in% train_df$Date)
        pf_test  <- pf_df %>% dplyr::filter(Date == test_row$Date)
        
        if (nrow(pf_test) == 1) {
          fcols <- paste0("F", seq_len(K_pf))
          pf_train_cc <- pf_train[, c("y_h", fcols), drop = FALSE]
          pf_train_cc <- pf_train_cc[complete.cases(pf_train_cc), , drop = FALSE]
          
          if (nrow(pf_train_cc) > 0) {
            pred_tbl[kk, "Static-Probit-Factors"] <- rec_safe_probit_predict(
              y_train = as.numeric(pf_train_cc$y_h),
              X_train = as.matrix(pf_train_cc[, fcols, drop = FALSE]),
              X_test  = as.matrix(pf_test[, fcols, drop = FALSE])
            )
          }
        }
      }
    }
    
    pred_tbl[kk, "ARDI,BIC"] <- rec_fit_predict_ARDI_BIC_logit(
      hist_df  = hist_df %>% dplyr::select(Date, y_h, dplyr::starts_with("y_L"), dplyr::all_of(x_names)),
      train_df = train_df %>% dplyr::select(Date),
      test_row = test_row %>% dplyr::select(Date, dplyr::all_of(x_names), dplyr::starts_with("y_L"), y_h),
      x_names  = x_names,
      py_max = py, pf_max = px, Kmax = K_fixed
    )
    
    Zx_cols  <- c(ycols, xlagcols)
    Zx_train <- as.matrix(train_df[, Zx_cols, drop = FALSE])
    Zy_train <- as.numeric(train_df$y_h)
    Zx_test  <- as.matrix(test_row[, Zx_cols, drop = FALSE])
    
    ok_null <- is.finite(Zy_train) & Zy_train %in% c(0, 1)
    if (any(ok_null)) null_prob[kk] <- rec_clip_prob(mean(Zy_train[ok_null]))
    
    Xcond_train <- as.matrix(train_df[, ycols, drop = FALSE])
    Xcond_test  <- as.matrix(test_row[, ycols, drop = FALSE])
    
    Xcand_train <- as.matrix(train_df[, xlagcols, drop = FALSE])
    Xcand_test  <- as.matrix(test_row[, xlagcols, drop = FALSE])
    
    if (need_retune(last_tune$ridge, t0)) {
      spec$ridge <- c(list(type = "ridge"), rec_tune_ridge_or_lasso_binom(Zx_train, Zy_train, alpha = 0))
      last_tune$ridge <- t0
    }
    pred_tbl[kk, "Ridge-X"] <- rec_fit_predict_glmnet_binom(Zx_train, Zy_train, Zx_test, spec$ridge)
    
    if (need_retune(last_tune$lasso, t0)) {
      spec$lasso <- c(list(type = "lasso"), rec_tune_ridge_or_lasso_binom(Zx_train, Zy_train, alpha = 1))
      last_tune$lasso <- t0
    }
    pred_tbl[kk, "Lasso-X"] <- rec_fit_predict_glmnet_binom(Zx_train, Zy_train, Zx_test, spec$lasso)
    
    if (need_retune(last_tune$elastic, t0)) {
      te <- rec_tune_elasticnet_binom(Zx_train, Zy_train)
      spec$elastic <- list(type = "elastic", alpha = te$alpha, lambda = te$lambda)
      last_tune$elastic <- t0
    }
    pred_tbl[kk, "Elastic-Net-X"] <- rec_fit_predict_glmnet_binom(Zx_train, Zy_train, Zx_test, spec$elastic)
    
    if (need_retune(last_tune$adalasso, t0)) {
      ta <- rec_tune_adaptive_lasso_binom(Zx_train, Zy_train)
      spec$adalasso <- list(type = "adalasso", lambda = ta$lambda, weights = ta$weights)
      last_tune$adalasso <- t0
    } else {
      spec$adalasso$weights <- rec_make_adalasso_weights_binom(Zx_train, Zy_train)
    }
    pred_tbl[kk, "Adaptive-Lasso-X"] <- rec_fit_predict_glmnet_binom(Zx_train, Zy_train, Zx_test, spec$adalasso)
    
    for (lev in names(ocmt_pvals)) {
      model_name <- paste0("OCMT_", lev)
      
      if (need_retune(last_tune$ocmt[[model_name]], t0)) {
        spec$ocmt_sel[[model_name]] <- rec_select_ocmt(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcond_train = Xcond_train,
          pval = ocmt_pvals[[lev]]
        )
        last_tune$ocmt[[model_name]] <- t0
      }
      
      pred_tbl[kk, model_name] <- rec_predict_from_selection(
        y_train = Zy_train,
        Xcand_train = Xcand_train,
        Xcand_test = Xcand_test,
        Xcond_train = Xcond_train,
        Xcond_test = Xcond_test,
        selected = spec$ocmt_sel[[model_name]]
      )
    }
    
    for (lev in names(bmt_pvals)) {
      model_name <- paste0("BMT_", lev)
      
      if (need_retune(last_tune$bmt[[model_name]], t0)) {
        spec$bmt_sel[[model_name]] <- rec_select_bmt(
          y_train = Zy_train,
          Xcand_train = Xcand_train,
          Xcond_train = Xcond_train,
          pval = bmt_pvals[[lev]]
        )
        last_tune$bmt[[model_name]] <- t0
      }
      
      pred_tbl[kk, model_name] <- rec_predict_from_selection(
        y_train = Zy_train,
        Xcand_train = Xcand_train,
        Xcand_test = Xcand_test,
        Xcond_train = Xcond_train,
        Xcond_test = Xcond_test,
        selected = spec$bmt_sel[[model_name]]
      )
    }
    
    if (need_retune(last_tune$farm, t0)) {
      spec$farm_fit <- rec_fit_farm_native_logit(
        y_train = Zy_train,
        Z_train = Xcand_train,
        loss = rec_cfg$farm$loss,
        robust = rec_cfg$farm$robust,
        cv = rec_cfg$farm$cv,
        tau = rec_cfg$farm$tau,
        K.factors = rec_cfg$farm$K.factors,
        max.iter = rec_cfg$farm$max.iter,
        nfolds = rec_cfg$farm$nfolds,
        eps = rec_cfg$farm$eps
      )
      
      last_tune$farm <- t0
    }
    
    pred_tbl[kk, "FarmSelect"] <- rec_predict_from_farm_selection(
      y_train = Zy_train,
      Xcand_train = Xcand_train,
      Xcand_test = Xcand_test,
      Xcond_train = Xcond_train,
      Xcond_test = Xcond_test,
      farm_obj = spec$farm_fit
    )
    
    pred_tbl[kk, "RF-X"] <- rec_fit_predict_rf(
      Z_train = Zx_train,
      y_train = Zy_train,
      Z_test  = Zx_test,
      num_trees = rf_num_trees
    )
    
    X_hist_all <- as.matrix(hist_df[, x_names, drop = FALSE])
    pcaK <- rec_pca_fit(X_hist_all, Kmax = K_fixed)
    
    if (!is.null(pcaK)) {
      F_all <- pcaK$scores
      K_avail <- ncol(F_all)
      
      if (K_avail > 0) {
        colnames(F_all) <- paste0("F", seq_len(K_avail))
        
        hf2 <- dplyr::bind_cols(
          hist_df %>% dplyr::select(Date, y_h, dplyr::starts_with("y_L")),
          as.data.frame(F_all)
        )
        
        for (k in seq_len(K_avail)) {
          for (L in 0:(py - 1)) {
            hf2[[paste0("F", k, "_L", L)]] <- dplyr::lag(hf2[[paste0("F", k)]], L)
          }
        }
        
        hf2_oos <- hf2 %>% dplyr::filter(Date %in% df_x$Date)
        idx_train2 <- which(hf2_oos$Date <= cutoff)
        idx_test2  <- which(hf2_oos$Date == t0)
        
        if (length(idx_test2) == 1) {
          tr2 <- hf2_oos[idx_train2, , drop = FALSE]
          te2 <- hf2_oos[idx_test2, , drop = FALSE]
          
          fac_lag_cols <- c(
            paste0("y_L", 0:(py - 1)),
            unlist(lapply(seq_len(K_avail), function(k) paste0("F", k, "_L", 0:(py - 1))))
          )
          
          cc_f <- complete.cases(tr2[, c("y_h", fac_lag_cols), drop = FALSE])
          tr2 <- tr2[cc_f, , drop = FALSE]
          
          if (nrow(tr2) >= 50) {
            Zf_train <- as.matrix(tr2[, fac_lag_cols, drop = FALSE])
            yf_train <- as.numeric(tr2$y_h)
            Zf_test  <- as.matrix(te2[, fac_lag_cols, drop = FALSE])
            
            if (need_retune(last_tune$ardi_ridge, t0)) {
              spec$ardi_ridge <- c(list(type = "ridge"), rec_tune_ridge_or_lasso_binom(Zf_train, yf_train, alpha = 0))
              last_tune$ardi_ridge <- t0
            }
            pred_tbl[kk, "ARDI,Ridge"] <- rec_fit_predict_glmnet_binom(Zf_train, yf_train, Zf_test, spec$ardi_ridge)
            
            if (need_retune(last_tune$ardi_lasso, t0)) {
              spec$ardi_lasso <- c(list(type = "lasso"), rec_tune_ridge_or_lasso_binom(Zf_train, yf_train, alpha = 1))
              last_tune$ardi_lasso <- t0
            }
            pred_tbl[kk, "ARDI,Lasso"] <- rec_fit_predict_glmnet_binom(Zf_train, yf_train, Zf_test, spec$ardi_lasso)
            
            if (need_retune(last_tune$ardi_elastic, t0)) {
              tef <- rec_tune_elasticnet_binom(Zf_train, yf_train)
              spec$ardi_elastic <- list(type = "elastic", alpha = tef$alpha, lambda = tef$lambda)
              last_tune$ardi_elastic <- t0
            }
            pred_tbl[kk, "ARDI,Elastic-Net"] <- rec_fit_predict_glmnet_binom(Zf_train, yf_train, Zf_test, spec$ardi_elastic)
            
            if (need_retune(last_tune$ardi_adalasso, t0)) {
              taf <- rec_tune_adaptive_lasso_binom(Zf_train, yf_train)
              spec$ardi_adalasso <- list(type = "adalasso", lambda = taf$lambda, weights = taf$weights)
              last_tune$ardi_adalasso <- t0
            } else {
              spec$ardi_adalasso$weights <- rec_make_adalasso_weights_binom(Zf_train, yf_train)
            }
            pred_tbl[kk, "ARDI,Adaptive-Lasso"] <- rec_fit_predict_glmnet_binom(Zf_train, yf_train, Zf_test, spec$ardi_adalasso)
            
            pred_tbl[kk, "RFARDI"] <- rec_fit_predict_rf(
              Z_train = Zf_train,
              y_train = yf_train,
              Z_test  = Zf_test,
              num_trees = rf_num_trees
            )
          }
        }
      }
    }
    
    if (need_retune(last_tune$tcsr, t0)) {
      spec$tcsr <- list(xstar = rec_tcsr_screen(train_df, x_names))
      last_tune$tcsr <- t0
    }
    
    xstar <- spec$tcsr$xstar
    tcsr_forecast_n <- tcsr_forecast_n + 1L
    
    tcsr_raw <- rec_tcsr_predict(
      train_df,
      test_row,
      xstar = xstar,
      L = rec_cfg$tcsr$L,
      M = tcsr_M,
      seed = 1
    )
    
    if (!is.finite(tcsr_raw) && is.finite(pred_tbl[kk, "AR,BIC"])) {
      pred_tbl[kk, "T-CSR"] <- pred_tbl[kk, "AR,BIC"]
      tcsr_fallback_n <- tcsr_fallback_n + 1L
    } else {
      pred_tbl[kk, "T-CSR"] <- tcsr_raw
    }
    
    pred_tbl[kk, "AVG"] <- rec_simple_average(pred_tbl[kk, setdiff(models, "AVG"), drop = TRUE])
    
    if (kk %% 4L == 0L) gc(FALSE)
  }
  
  gc(FALSE)
  
  if (!is.null(p)) {
    p(amount = 1, message = sprintf("%s | h=%s | block %d/%d", run_id, h, block_id, n_blocks))
  }
  
  list(
    target = rec_target,
    h = h,
    block_id = block_id,
    rows = rows_to_run,
    dates = df_x$Date[rows_to_run],
    y_true = as.numeric(df_x$y_h[rows_to_run]),
    probs = pred_tbl,
    null_prob = null_prob,
    tcsr_fallback_n = tcsr_fallback_n,
    tcsr_forecast_n = tcsr_forecast_n
  )
}

# ============================================================
# FINALIZE BLOCKS INTO FULL TARGET/HORIZON RESULT
# ============================================================

rec_finalize_target_horizon_from_blocks <- function(panel_rec_model, rec_target,
                                                    h,
                                                    block_results,
                                                    oos_start = rec_cfg$oos_start,
                                                    oos_end   = rec_cfg$oos_end,
                                                    py = rec_cfg$py,
                                                    px = rec_cfg$px,
                                                    training_months = rec_cfg$training_months,
                                                    retune_every_months = rec_cfg$retune_every_months,
                                                    ocmt_pvals = rec_cfg$ocmt$pvals,
                                                    bmt_pvals  = rec_cfg$bmt$pvals) {
  prep <- rec_prepare_task_frame(
    panel_rec_model = panel_rec_model,
    rec_target = rec_target,
    h = h,
    oos_start = oos_start,
    oos_end = oos_end,
    py = py,
    px = px,
    training_months = training_months,
    retune_every_months = retune_every_months,
    ocmt_pvals = ocmt_pvals,
    bmt_pvals = bmt_pvals
  )
  
  df_x   <- prep$df_x
  models <- prep$models
  
  pred_tbl <- as.data.frame(matrix(NA_real_, nrow = nrow(df_x), ncol = length(models)))
  names(pred_tbl) <- models
  
  null_prob <- rep(NA_real_, nrow(df_x))
  tcsr_fallback_n <- 0L
  tcsr_forecast_n <- 0L
  
  if (length(block_results) > 0) {
    for (res in block_results) {
      if (length(res$rows) == 0) next
      pred_tbl[res$rows, names(res$probs)] <- res$probs
      null_prob[res$rows] <- res$null_prob
      tcsr_fallback_n <- tcsr_fallback_n + res$tcsr_fallback_n
      tcsr_forecast_n <- tcsr_forecast_n + res$tcsr_forecast_n
    }
  }
  
  if (nrow(df_x) == 0) {
    metrics_df <- data.frame(
      model = models,
      n_oos = NA_integer_,
      qps = NA_real_,
      lps = NA_real_,
      auc = NA_real_,
      mcfadden_r2 = NA_real_,
      estrella_r2 = NA_real_,
      dm_p_qps = NA_real_,
      dm_p_lps = NA_real_,
      stringsAsFactors = FALSE
    )
    
    return(list(
      target = rec_target,
      h = h,
      metrics = metrics_df,
      dates = as.Date(character(0)),
      y_true = numeric(0),
      probs = pred_tbl,
      null_prob = numeric(0),
      tcsr_fallback_n = 0L,
      tcsr_forecast_n = 0L
    ))
  }
  
  y_true <- as.numeric(df_x$y_h)
  
  qps_bench <- rec_qps_vec(y_true, pred_tbl[["AR,BIC"]])
  lps_bench <- rec_lps_vec(y_true, pred_tbl[["AR,BIC"]])
  
  metrics <- lapply(models, function(m) {
    p_hat <- rec_clip_prob(pred_tbl[[m]])
    ok <- is.finite(y_true) & is.finite(p_hat)
    
    y_ok <- y_true[ok]
    p_ok <- p_hat[ok]
    p0_ok <- null_prob[ok]
    
    qps <- if (length(y_ok) == 0) NA_real_ else mean(rec_qps_vec(y_ok, p_ok))
    lps <- if (length(y_ok) == 0) NA_real_ else mean(rec_lps_vec(y_ok, p_ok))
    auc <- rec_auc_safe(y_ok, p_ok)
    mcfadden_r2 <- rec_mcfadden_r2(y_ok, p_ok, p0_ok)
    estrella_r2 <- rec_estrella_r2(y_ok, p_ok, p0_ok)
    
    qps_dm_p <- NA_real_
    lps_dm_p <- NA_real_
    
    if (m != "AR,BIC") {
      qps_dm_p <- rec_dm_test_loss(rec_qps_vec(y_true, p_hat), qps_bench, h = h)
      lps_dm_p <- rec_dm_test_loss(rec_lps_vec(y_true, p_hat), lps_bench, h = h)
    }
    
    list(
      n_oos = length(y_ok),
      qps = qps,
      lps = lps,
      auc = auc,
      mcfadden_r2 = mcfadden_r2,
      estrella_r2 = estrella_r2,
      dm_p_qps = qps_dm_p,
      dm_p_lps = lps_dm_p
    )
  })
  names(metrics) <- models
  
  metrics_df <- data.frame(
    model = models,
    n_oos = sapply(metrics, `[[`, "n_oos"),
    qps = sapply(metrics, `[[`, "qps"),
    lps = sapply(metrics, `[[`, "lps"),
    auc = sapply(metrics, `[[`, "auc"),
    mcfadden_r2 = sapply(metrics, `[[`, "mcfadden_r2"),
    estrella_r2 = sapply(metrics, `[[`, "estrella_r2"),
    dm_p_qps = sapply(metrics, `[[`, "dm_p_qps"),
    dm_p_lps = sapply(metrics, `[[`, "dm_p_lps"),
    stringsAsFactors = FALSE
  )
  
  list(
    target = rec_target,
    h = h,
    metrics = metrics_df,
    dates = df_x$Date,
    y_true = y_true,
    probs = pred_tbl,
    null_prob = null_prob,
    tcsr_fallback_n = tcsr_fallback_n,
    tcsr_forecast_n = tcsr_forecast_n
  )
}

# ============================================================
# PARALLEL RUNNER (BLOCK-BASED)
# ============================================================

rec_run_all_targets_parallel <- function(panel_rec_model,
                                         rec_targets,
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
                                         bmt_pvals = rec_cfg$bmt$pvals) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  task_specs <- list()
  
  for (tg in rec_targets) {
    for (h in horizons) {
      prep <- rec_prepare_task_frame(
        panel_rec_model = panel_rec_model,
        rec_target = tg,
        h = h,
        oos_start = oos_start,
        oos_end = oos_end,
        py = py,
        px = px,
        training_months = training_months,
        retune_every_months = retune_every_months,
        ocmt_pvals = ocmt_pvals,
        bmt_pvals = bmt_pvals
      )
      
      n_blocks <- length(prep$blocks)
      
      if (n_blocks > 0) {
        for (b in seq_len(n_blocks)) {
          task_specs[[length(task_specs) + 1L]] <- list(
            target = tg,
            h = h,
            block_id = b,
            n_blocks = n_blocks,
            rows = prep$blocks[[b]]
          )
        }
      }
    }
  }
  
  n_workers_use <- max(1L, min(n_workers, max(1L, length(task_specs))))
  
  if (length(task_specs) == 0) {
    out <- setNames(vector("list", length(rec_targets)), rec_targets)
    for (tg in rec_targets) {
      out[[tg]] <- setNames(vector("list", length(horizons)), paste0("h", horizons))
      for (h in horizons) {
        out[[tg]][[paste0("h", h)]] <- rec_finalize_target_horizon_from_blocks(
          panel_rec_model = panel_rec_model,
          rec_target = tg,
          h = h,
          block_results = list(),
          oos_start = oos_start,
          oos_end = oos_end,
          py = py,
          px = px,
          training_months = training_months,
          retune_every_months = retune_every_months,
          ocmt_pvals = ocmt_pvals,
          bmt_pvals = bmt_pvals
        )
      }
    }
    return(out)
  }
  
  if (n_workers_use <= 1L) {
    future::plan(future::sequential)
    
    all_block_results <- progressr::with_progress({
      p <- progressr::progressor(steps = length(task_specs))
      
      out_list <- vector("list", length(task_specs))
      
      for (jj in seq_along(task_specs)) {
        task <- task_specs[[jj]]
        
        out_list[[jj]] <- rec_run_one_target_one_horizon_block(
          panel_rec_model = panel_rec_model,
          rec_target = task$target,
          h = task$h,
          rows_to_run = task$rows,
          block_id = task$block_id,
          n_blocks = task$n_blocks,
          run_id = task$target,
          oos_start = oos_start,
          oos_end = oos_end,
          py = py,
          px = px,
          K_fixed = K_fixed,
          retune_every_months = retune_every_months,
          training_months = training_months,
          tcsr_M = tcsr_M,
          rf_num_trees = rf_num_trees,
          ocmt_pvals = ocmt_pvals,
          bmt_pvals = bmt_pvals,
          p = p
        )
        
        gc(FALSE)
      }
      
      out_list
    })
  } else {
    future::plan(future::multisession, workers = n_workers_use, gc = TRUE)
    
    all_block_results <- progressr::with_progress({
      p <- progressr::progressor(steps = length(task_specs))
      
      furrr::future_map(
        task_specs,
        function(task) {
          res <- rec_run_one_target_one_horizon_block(
            panel_rec_model = panel_rec_model,
            rec_target = task$target,
            h = task$h,
            rows_to_run = task$rows,
            block_id = task$block_id,
            n_blocks = task$n_blocks,
            run_id = task$target,
            oos_start = oos_start,
            oos_end = oos_end,
            py = py,
            px = px,
            K_fixed = K_fixed,
            retune_every_months = retune_every_months,
            training_months = training_months,
            tcsr_M = tcsr_M,
            rf_num_trees = rf_num_trees,
            ocmt_pvals = ocmt_pvals,
            bmt_pvals = bmt_pvals,
            p = p
          )
          gc(FALSE)
          res
        },
        .options = furrr::furrr_options(
          seed = TRUE,
          scheduling = 1,
          packages = c(
            "dplyr", "purrr", "glmnet", "ranger",
            "lubridate", "progressr", "pROC",
            "OCMT", "FarmSelect", "MultipleTestingBoosting"
          )
        )
      )
    })
  }
  
  out <- setNames(vector("list", length(rec_targets)), rec_targets)
  for (tg in rec_targets) {
    out[[tg]] <- setNames(vector("list", length(horizons)), paste0("h", horizons))
  }
  
  for (tg in rec_targets) {
    for (h in horizons) {
      subset_blocks <- Filter(function(x) identical(x$target, tg) && identical(x$h, h), all_block_results)
      
      out[[tg]][[paste0("h", h)]] <- rec_finalize_target_horizon_from_blocks(
        panel_rec_model = panel_rec_model,
        rec_target = tg,
        h = h,
        block_results = subset_blocks,
        oos_start = oos_start,
        oos_end = oos_end,
        py = py,
        px = px,
        training_months = training_months,
        retune_every_months = retune_every_months,
        ocmt_pvals = ocmt_pvals,
        bmt_pvals = bmt_pvals
      )
    }
  }
  
  out
}

# ============================================================
# TABLE HELPERS
# ============================================================

rec_metric_direction <- function(metric) {
  if (metric %in% c("qps", "lps")) return("min")
  if (metric %in% c("auc", "mcfadden_r2", "estrella_r2")) return("max")
  stop("Unknown metric: ", metric)
}

rec_make_metric_table <- function(rec_all_results,
                                  rec_targets,
                                  metric = c("qps", "lps", "auc", "mcfadden_r2", "estrella_r2"),
                                  horizons = rec_cfg$horizons,
                                  digits = 2) {
  metric <- match.arg(metric)
  
  first_target <- rec_targets[1]
  first_h <- paste0("h", horizons[1])
  
  models <- rec_all_results[[first_target]][[first_h]]$metrics$model
  labels <- unname(rec_model_display_names[models])
  
  out <- data.frame(
    Methods = labels,
    stringsAsFactors = FALSE
  )
  
  use_dm_star <- metric %in% c("qps", "lps")
  dm_col <- if (metric == "qps") "dm_p_qps" else if (metric == "lps") "dm_p_lps" else NULL
  direction <- rec_metric_direction(metric)
  
  for (h in horizons) {
    hh <- paste0("h", h)
    mdf <- rec_all_results[[first_target]][[hh]]$metrics
    mdf <- mdf[match(models, mdf$model), , drop = FALSE]
    
    vals <- mdf[[metric]]
    formatted <- ifelse(is.na(vals), "", formatC(vals, format = "f", digits = digits))
    
    if (use_dm_star) {
      stars <- rec_star_p10(mdf[[dm_col]])
      formatted <- ifelse(nzchar(formatted), paste0(formatted, stars), formatted)
    }
    
    if (all(is.na(vals))) {
      out[[paste0("h=", h)]] <- formatted
      next
    }
    
    best_val <- if (direction == "min") min(vals, na.rm = TRUE) else max(vals, na.rm = TRUE)
    best_idx <- which(!is.na(vals) & vals == best_val)
    
    formatted[best_idx] <- paste0("\\cellcolor{green!15}", formatted[best_idx])
    
    out[[paste0("h=", h)]] <- formatted
  }
  
  out
}

rec_collect_summary <- function(rec_all_results,
                                rec_targets,
                                horizons = rec_cfg$horizons) {
  rows <- list()
  
  for (tg in rec_targets) {
    for (h in horizons) {
      hh <- paste0("h", h)
      one_res <- rec_all_results[[tg]][[hh]]
      mdf <- one_res$metrics
      
      tmp <- mdf
      tmp$model <- unname(rec_model_display_names[tmp$model])
      tmp$target <- tg
      tmp$h <- h
      rows[[paste0(tg, "_", h)]] <- tmp
    }
  }
  
  dplyr::bind_rows(rows) %>%
    dplyr::select(target, h, model, n_oos, qps, lps, auc, mcfadden_r2, estrella_r2, dm_p_qps, dm_p_lps)
}

rec_collect_tcsr_fallback_summary <- function(rec_all_results,
                                              rec_targets,
                                              horizons = rec_cfg$horizons) {
  rows <- list()
  
  for (tg in rec_targets) {
    for (h in horizons) {
      hh <- paste0("h", h)
      one_res <- rec_all_results[[tg]][[hh]]
      
      rows[[paste0(tg, "_", h)]] <- data.frame(
        target = tg,
        h = h,
        tcsr_fallback_n = one_res$tcsr_fallback_n,
        tcsr_forecast_n = one_res$tcsr_forecast_n,
        tcsr_fallback_share = if (one_res$tcsr_forecast_n > 0) {
          one_res$tcsr_fallback_n / one_res$tcsr_forecast_n
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  }
  
  dplyr::bind_rows(rows)
}
