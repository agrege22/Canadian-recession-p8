#' @useDynLib FarmSelect
#' @importFrom Rcpp sourceCpp
#' @importFrom graphics mtext plot points axis par barplot
#' @importFrom fBasics rowSds
#' @import methods
#' @import utils
#' @import grDevices
#' @import stats
#' @import ncvreg

NULL

###################################################################################
## Main function that performs model selection
###################################################################################
#' Factor-adjusted robust model selection
#'
#' Given a covariate matrix and output vector, this function first adjusts the
#' covariates for underlying factors and then performs penalized model selection.
#' The returned object can also be used for out-of-sample prediction.
#'
#' @param X an n x p covariate matrix with each row being a sample.
#' @param Y a size n outcome vector.
#' @param loss a character string specifying the loss function to be minimized.
#' Must be one of "scad", "mcp", or "lasso".
#' @param robust logical; whether to use robust estimators.
#' @param cv logical; whether to use cross-validation for robust tuning.
#' @param tau >0, multiplier for the tuning parameter for Huber loss.
#' @param lin.reg logical; TRUE for linear regression, FALSE for logistic model.
#' @param K.factors number of factors to be estimated. Otherwise estimated internally.
#' @param max.iter maximum number of iterations across the regularization path.
#' @param nfolds the number of cross-validation folds.
#' @param eps convergence threshold for \code{ncvreg}.
#' @param verbose logical; whether to print runtime updates.
#'
#' @return A \code{farm.select} object with selection output and prediction info.
#' @export
farm.select <- function(
    X, Y,
    loss = c("scad", "mcp", "lasso"),
    robust = TRUE,
    cv = FALSE,
    tau = 2,
    lin.reg = TRUE,
    K.factors = NULL,
    max.iter = 10000,
    nfolds = ceiling(length(Y) / 3),
    eps = 1e-4,
    verbose = TRUE
) {
  loss <- match.arg(loss)

  X <- t(X)

  if (tau <= 0) stop("tau should be a positive number")
  if (NCOL(X) != NROW(Y)) {
    stop("number of rows in covariate matrix should be size of the outcome vector")
  }

  output <- farm.select.adjusted(
    X = X,
    Y = Y,
    loss = loss,
    robust = robust,
    cv = cv,
    tau = tau,
    lin.reg = lin.reg,
    K.factors = K.factors,
    max.iter = max.iter,
    nfolds = nfolds,
    eps = eps,
    verbose = verbose
  )

  attr(output, "class") <- "farm.select"
  output
}

###################################################################################
## Print method
###################################################################################
#' @export
print.farm.select <- function(x, ...) {
  cat(
    paste0(
      "\nFactor Adjusted ",
      if (isTRUE(x$robust)) "Robust " else "",
      "Model Selection\n"
    )
  )
  cat(paste0("loss function used: ", x$loss, "\n"))
  cat(paste0("family: ", x$family, "\n"))
  cat(paste0("\np = ", x$p, ", n = ", x$n, "\n"))
  cat(paste0("factors found: ", x$nfactors, "\n"))
  cat("size of model selected:\n")
  cat(paste0(" ", x$model.size, "\n"))
}

###################################################################################
## Helper: safe generalized inverse
###################################################################################
farm.safe_inv <- function(A, tol = 1e-8) {
  A <- as.matrix(A)

  if (nrow(A) == 0 || ncol(A) == 0) {
    return(matrix(0, nrow(A), ncol(A)))
  }

  s <- svd(A)
  keep <- s$d > tol

  if (!any(keep)) {
    return(matrix(0, nrow(A), ncol(A)))
  }

  s$v[, keep, drop = FALSE] %*%
    (t(s$u[, keep, drop = FALSE]) / s$d[keep])
}

###################################################################################
## Helper: extract full coefficient vector from ncvreg object
## Returns a named numeric vector including intercept as first entry when possible
###################################################################################
farm.extract_ncvreg_coef <- function(fit, lambda, X = NULL) {
  attempts <- list(
    function() stats::coef(fit, lambda = lambda),
    function() stats::coef(fit, s = lambda),
    function() predict(fit, type = "coefficients", lambda = lambda),
    function() predict(fit, X = X, type = "coefficients", lambda = lambda),
    function() predict(fit, type = "coefficients", which = which.min(abs(fit$lambda - lambda))),
    function() predict(fit, X = X, type = "coefficients", which = which.min(abs(fit$lambda - lambda)))
  )

  for (f in attempts) {
    out <- try(f(), silent = TRUE)
    if (!inherits(out, "try-error") && !is.null(out) && length(out) > 0) {
      out <- drop(as.matrix(out))
      out <- as.numeric(out) |> `names<-`(names(drop(as.matrix(out))))
      if (length(out) > 0 && all(is.finite(out) | is.na(out))) {
        return(out)
      }
    }
  }

  stop("Could not extract coefficient vector from ncvreg object.")
}

###################################################################################
## Empty result helper
###################################################################################
farm.empty_solution <- function(fit = NULL, lambda.min = NA_real_) {
  list(
    beta_chosen = NULL,
    coef_chosen = NULL,
    model_size = 0L,
    intercept = NA_real_,
    factor_coef = numeric(0),
    coef.full = numeric(0),
    coef.names = character(0),
    selected.names = character(0),
    fit = fit,
    lambda.min = lambda.min
  )
}

###################################################################################
## Build output from coefficient vector
###################################################################################
farm.build_solution <- function(beta_all, Kfactors = 0L, fit = NULL, lambda.min = NA_real_) {
  beta_names <- names(beta_all)
  beta_all   <- as.numeric(beta_all)

  if (!is.null(beta_names) && length(beta_names) == length(beta_all)) {
    names(beta_all) <- beta_names
  }

  if (length(beta_all) == 0) {
    return(farm.empty_solution(fit = fit, lambda.min = lambda.min))
  }

  intercept <- beta_all[1]

  if (Kfactors > 0L) {
    factor_idx  <- seq.int(2L, Kfactors + 1L)
    factor_coef <- beta_all[factor_idx]
  } else {
    factor_coef <- numeric(0)
  }

  resid_start <- Kfactors + 2L

  if (length(beta_all) >= resid_start) {
    beta_x_full <- beta_all[resid_start:length(beta_all)]
  } else {
    beta_x_full <- numeric(0)
  }

  inds <- which(is.finite(beta_x_full) & abs(beta_x_full) > 0)

  if (length(inds) == 0L) {
    return(list(
      beta_chosen = NULL,
      coef_chosen = NULL,
      model_size = 0L,
      intercept = intercept,
      factor_coef = factor_coef,
      coef.full = beta_all,
      coef.names = names(beta_all),
      selected.names = character(0),
      fit = fit,
      lambda.min = lambda.min
    ))
  }

  coef_chosen   <- beta_x_full[inds]
  selected_names <- names(beta_x_full)[inds]

  list(
    beta_chosen = inds,
    coef_chosen = coef_chosen,
    model_size = length(inds),
    intercept = intercept,
    factor_coef = factor_coef,
    coef.full = beta_all,
    coef.names = names(beta_all),
    selected.names = selected_names,
    fit = fit,
    lambda.min = lambda.min
  )
}

###################################################################################
## Main adjusted fit: stores enough objects for prediction
###################################################################################
farm.select.adjusted <- function(
    X, Y, loss, robust, cv = cv, tau,
    lin.reg, K.factors, max.iter, nfolds, eps, verbose
) {
  n <- length(Y)
  p <- NROW(X)

  Y.mean <- NULL
  X.mean <- NULL

  if (robust == TRUE) {
    if (cv == TRUE) {
      if (lin.reg == TRUE) {
        Y.mean <- mu_robust_F(matrix(Y, 1, n), matrix(rep(1, n), n, 1))
        Y <- Y - rep(Y.mean, n)
      }

      X.mean <- mu_robust_F(matrix(X, p, n), matrix(rep(1, n), n, 1))
      X <- sweep(X, 1, X.mean, "-")
    } else {
      if (lin.reg == TRUE) {
        CTy <- tau * stats::sd(Y) * sqrt(n / log(n))
        Y.mean <- mu_robust_F_noCV(
          matrix(Y, 1, n),
          matrix(rep(1, n), n, 1),
          matrix(CTy, 1, 1)
        )
        Y <- Y - rep(Y.mean, n)
      }

      CTx <- tau * rowSds(X) * sqrt(n / log(p * n))
      X.mean <- mu_robust_F_noCV(
        matrix(X, p, n),
        matrix(rep(1, n), n, 1),
        matrix(CTx, p, 1)
      )
      X <- sweep(X, 1, X.mean, "-")
    }
  } else {
    if (lin.reg == TRUE) {
      Y.mean <- mean(Y)
      Y <- Y - Y.mean
    }

    X.mean <- rowMeans(X)
    X <- sweep(X, 1, X.mean, "-")
  }

  output.adjust <- farm.adjust(
    Y = Y,
    X = t(X),
    robust = robust,
    cv = cv,
    tau = tau,
    lin.reg = lin.reg,
    K.factors = K.factors,
    verbose = verbose
  )

  X.residual <- output.adjust$X.residual
  Y.residual <- output.adjust$Y.residual
  F_hat      <- output.adjust$F_hat
  Lambda_hat <- output.adjust$Lambda_hat
  nfactors   <- output.adjust$nfactors

  fam <- if (lin.reg) "gaussian" else "binomial"

  output.chosen <- farm.select.temp(
    X.res = X.residual,
    Y.res = Y.residual,
    loss = loss,
    max.iter = max.iter,
    nfolds = nfolds,
    eps = eps,
    factors = F_hat,
    family = fam
  )

  list(
    model.size   = output.chosen$model_size,
    beta.chosen  = output.chosen$beta_chosen,
    coef.chosen  = output.chosen$coef_chosen,

    intercept    = output.chosen$intercept,
    factor.coef  = output.chosen$factor_coef,
    coef.full    = output.chosen$coef.full,
    coef.names   = output.chosen$coef.names,
    selected.names = output.chosen$selected.names,

    lambda.min   = output.chosen$lambda.min,
    fit          = output.chosen$fit,

    nfactors     = nfactors,
    factors      = unname(F_hat),
    loadings     = unname(Lambda_hat),
    X.residual   = unname(X.residual),

    x.mean       = X.mean,
    y.mean       = if (lin.reg) Y.mean else NULL,

    family       = fam,
    lin.reg      = lin.reg,
    robust       = robust,
    cv           = cv,
    tau          = tau,
    loss         = loss,

    p            = NCOL(X.residual),
    n            = n,
    p.raw        = length(X.mean)
  )
}

###################################################################################
## Adjusting for factors
###################################################################################
farm.adjust <- function(X, Y, robust, cv = cv, tau = tau, lin.reg, K.factors, verbose) {
  X <- t(X)
  p <- NROW(X)
  n <- NCOL(X)

  if (min(n, p) <= 4) stop("\n n and p must be at least 4 \n")

  if (robust == TRUE) {
    if (cv == TRUE) {
      if (verbose) cat("calculating covariance matrix...\n")
      covx <- Cov_Huber(matrix(X, p, n), matrix(rep(1, n), n, 1))
      eigs <- Eigen_Decomp(covx)
      values <- eigs[, p + 1]
    } else {
      if (verbose) cat("calculating tuning parameters...\n")
      CT <- Cov_Huber_tune(X, tau)
      if (verbose) cat("calculating covariance matrix...\n")
      covx <- Cov_Huber_noCV(matrix(X, p, n), matrix(rep(1, n), n, 1), matrix(CT, p, p))
      eigs <- Eigen_Decomp(covx)
      values <- eigs[, p + 1]
    }
  } else {
    covx <- stats::cov(t(X))
    pca_fit <- stats::prcomp(X, center = TRUE, scale = TRUE)
    values <- (pca_fit$sdev)^2
  }

  if (verbose) cat("fitting model...\n")

  values <- pmax(values, 0)
  ratio <- c()

  K.factors <- if (is.null(K.factors)) {
    for (i in 1:(floor(min(n, p) / 2))) {
      ratio <- append(ratio, values[i + 1] / values[i])
    }
    ratio <- ratio[is.finite(ratio)]
    which.min(ratio)
  } else {
    K.factors
  }

  if (K.factors > min(n, p) / 2) {
    warning("\n Warning: Number of factors supplied is > min(n,p)/2. May cause numerical inconsistencies \n")
  }
  if (K.factors > max(n, p)) {
    stop("\n Number of factors supplied cannot be larger than n or p \n")
  }

  Lambda_hat <- Find_lambda_class(Sigma = matrix(covx, p, p), X, n, p, K.factors)
  F_hat      <- Find_factors_class(Lambda_hat, X, n, p, K.factors)
  X.res2     <- Find_X_star_class(F_hat, Lambda_hat, X)

  if (NCOL(X) != NROW(Y)) {
    stop("\n number of rows in covariate matrix should be size of the outcome vector \n")
  }

  list(
    X.residual = X.res2,
    Y.residual = matrix(Y, n, 1),
    nfactors   = K.factors,
    F_hat      = F_hat,
    Lambda_hat = Lambda_hat
  )
}

###################################################################################
## Penalized model selection on forecastable design
## - gaussian: cv.ncvreg()
## - binomial: ncvreg() + BIC
###################################################################################
farm.select.temp <- function(
    X.res, Y.res, loss, max.iter, nfolds, eps,
    factors = NULL,
    family = c("gaussian", "binomial")
) {
  family <- match.arg(family)

  X.res <- as.matrix(X.res)
  Y.res <- as.numeric(Y.res)

  P <- NCOL(X.res)

  if (is.null(factors)) {
    Kfactors <- 0L
    X.fit <- X.res
    penalty.factor <- rep(1, P)
  } else {
    factors <- as.matrix(factors)
    Kfactors <- NCOL(factors)
    X.fit <- cbind(factors, X.res)
    penalty.factor <- c(rep(0, Kfactors), rep(1, P))
  }

  if (ncol(X.fit) > 0 && is.null(colnames(X.fit))) {
    x_names <- c(
      if (Kfactors > 0L) paste0("Factor", seq_len(Kfactors)) else character(0),
      paste0("X", seq_len(P))
    )
    colnames(X.fit) <- x_names
  }

  penalty_name <- switch(
    tolower(loss),
    "mcp"   = "MCP",
    "lasso" = "lasso",
    "scad"  = "SCAD",
    stop("Unknown loss")
  )

  # ----------------------------------------------------------
  # BINOMIAL: use ncvreg path + BIC, no cv.ncvreg
  # ----------------------------------------------------------
  if (family == "binomial") {
    y_ok <- is.finite(Y.res) & Y.res %in% c(0, 1)
    x_ok <- if (ncol(X.fit) == 0) {
      rep(TRUE, nrow(X.fit))
    } else {
      apply(X.fit, 1, function(r) all(is.finite(r)))
    }
    ok <- y_ok & x_ok

    Y.use <- Y.res[ok]
    X.use <- X.fit[ok, , drop = FALSE]

    if (length(Y.use) == 0 || length(unique(Y.use)) < 2) {
      return(farm.empty_solution())
    }

    fit0 <- try(
      ncvreg(
        X = X.use,
        y = Y.use,
        family = "binomial",
        penalty = penalty_name,
        max.iter = max.iter,
        eps = eps,
        penalty.factor = penalty.factor
      ),
      silent = TRUE
    )

    if (inherits(fit0, "try-error") || is.null(fit0$lambda) || length(fit0$lambda) == 0) {
      return(farm.empty_solution())
    }

    p_mat <- try(
      as.matrix(predict(fit0, X = X.use, type = "response")),
      silent = TRUE
    )

    if (inherits(p_mat, "try-error") || is.null(p_mat)) {
      return(farm.empty_solution(fit = fit0))
    }

    if (!is.matrix(p_mat)) {
      p_mat <- matrix(p_mat, ncol = 1)
    }

    p_mat <- pmin(pmax(p_mat, 1e-8), 1 - 1e-8)

    n <- length(Y.use)
    y_mat <- matrix(Y.use, nrow = n, ncol = ncol(p_mat))

    ll <- colSums(y_mat * log(p_mat) + (1 - y_mat) * log(1 - p_mat))

    df_path <- 1L + colSums(is.finite(fit0$beta) & abs(fit0$beta) > 0)
    bic <- -2 * ll + log(n) * df_path
    bic[!is.finite(bic)] <- Inf

    j <- which.min(bic)

    if (!length(j) || !is.finite(bic[j])) {
      return(farm.empty_solution(fit = fit0))
    }

    lambda_min <- fit0$lambda[j]

    beta_all <- try(
      farm.extract_ncvreg_coef(fit = fit0, lambda = lambda_min, X = X.use),
      silent = TRUE
    )

    if (inherits(beta_all, "try-error") || length(beta_all) == 0 || anyNA(beta_all[1])) {
      return(farm.empty_solution(fit = fit0, lambda.min = lambda_min))
    }

    if (is.null(names(beta_all)) || length(names(beta_all)) != length(beta_all)) {
      names(beta_all) <- c("(Intercept)", colnames(X.use))
    }

    return(
      farm.build_solution(
        beta_all = beta_all,
        Kfactors = Kfactors,
        fit = fit0,
        lambda.min = lambda_min
      )
    )
  }

  # ----------------------------------------------------------
  # GAUSSIAN: keep cv.ncvreg
  # ----------------------------------------------------------
  cvfit <- try(
    cv.ncvreg(
      X = X.fit,
      y = Y.res,
      penalty = penalty_name,
      seed = 100,
      nfolds = nfolds,
      max.iter = max.iter,
      eps = eps,
      family = "gaussian",
      penalty.factor = penalty.factor
    ),
    silent = TRUE
  )

  if (inherits(cvfit, "try-error") || is.null(cvfit$lambda.min) || !is.finite(cvfit$lambda.min)) {
    return(farm.empty_solution())
  }

  beta_all <- try(
    stats::coef(cvfit, lambda = cvfit$lambda.min),
    silent = TRUE
  )

  if (inherits(beta_all, "try-error") || length(beta_all) == 0) {
    return(farm.empty_solution(fit = cvfit, lambda.min = cvfit$lambda.min))
  }

  beta_all <- drop(as.matrix(beta_all))

  if (is.null(names(beta_all)) || length(names(beta_all)) != length(beta_all)) {
    names(beta_all) <- c("(Intercept)", colnames(X.fit))
  }

  farm.build_solution(
    beta_all = beta_all,
    Kfactors = Kfactors,
    fit = cvfit,
    lambda.min = cvfit$lambda.min
  )
}

###################################################################################
## Adjusting a data matrix for underlying factors
###################################################################################
#' @export
farm.res <- function(X, K.factors = NULL, robust = TRUE, cv = FALSE, tau = 2, verbose = TRUE) {
  X <- t(X)
  p <- NROW(X)
  n <- NCOL(X)

  if (min(n, p) <= 4) stop("\n n and p must be at least 4 \n")
  if (tau <= 0) stop("tau should be a positive number")

  if (robust == TRUE) {
    if (cv == TRUE) {
      X.mean <- mu_robust_F(matrix(X, p, n), matrix(rep(1, n), n, 1))
      X <- sweep(X, 1, X.mean, "-")
    } else {
      CT <- tau * rowSds(X) * sqrt(n / log(p * n))
      X.mean <- mu_robust_F_noCV(
        matrix(X, p, n),
        matrix(rep(1, n), n, 1),
        matrix(CT, p, 1)
      )
      X <- sweep(X, 1, X.mean, "-")
    }
  } else {
    X.mean <- rowMeans(X)
    X <- sweep(X, 1, X.mean, "-")
  }

  if (robust == TRUE) {
    if (cv == TRUE) {
      if (verbose) cat("calculating covariance matrix...\n")
      covx <- Cov_Huber(matrix(X, p, n), matrix(rep(1, n), n, 1))
      eigs <- Eigen_Decomp(covx)
      values <- eigs[, p + 1]
    } else {
      if (verbose) cat("calculating tuning parameters...\n")
      CT <- Cov_Huber_tune(X, tau)
      if (verbose) cat("calculating covariance matrix...\n")
      covx <- Cov_Huber_noCV(matrix(X, p, n), matrix(rep(1, n), n, 1), matrix(CT, p, p))
      eigs <- Eigen_Decomp(covx)
      values <- eigs[, p + 1]
    }
  } else {
    covx <- stats::cov(t(X))
    pca_fit <- stats::prcomp(X, center = TRUE, scale = TRUE)
    values <- (pca_fit$sdev)^2
  }

  values <- pmax(values, 0)
  ratio <- c()

  K.factors <- if (is.null(K.factors)) {
    for (i in 1:(floor(min(n, p) / 2))) {
      ratio <- append(ratio, values[i + 1] / values[i])
    }
    ratio <- ratio[is.finite(ratio)]
    which.min(ratio)
  } else {
    K.factors
  }

  if (K.factors > min(n, p) / 2) {
    warning("\n Warning: Number of factors supplied is > min(n,p)/2. May cause numerical inconsistencies \n")
  }
  if (K.factors > max(n, p)) {
    stop("\n Number of factors supplied cannot be larger than n or p \n")
  }

  Lambda_hat <- Find_lambda_class(Sigma = matrix(covx, p, p), X, n, p, K.factors)
  F_hat      <- Find_factors_class(Lambda_hat, X, n, p, K.factors)
  X.res      <- Find_X_star_class(F_hat, Lambda_hat, X)

  list(
    X.res = X.res,
    nfactors = K.factors,
    factors = F_hat,
    loadings = Lambda_hat,
    x.mean = X.mean
  )
}

###################################################################################
## Internal transform for new data
###################################################################################
farm.transform_newx <- function(object, newx) {
  if (is.null(object$x.mean)) stop("Object does not contain x.mean")
  if (is.null(object$loadings)) stop("Object does not contain loadings")

  newx <- as.matrix(newx)

  if (NCOL(newx) != length(object$x.mean)) {
    stop("newx has wrong number of columns")
  }

  Xc <- sweep(newx, 2, object$x.mean, "-")

  Lambda_hat <- as.matrix(object$loadings)

  if (ncol(Lambda_hat) == 0) {
    F_new <- matrix(numeric(0), nrow(Xc), 0)
    U_new <- Xc
  } else {
    gram_inv <- farm.safe_inv(crossprod(Lambda_hat))
    F_new <- Xc %*% Lambda_hat %*% gram_inv
    U_new <- Xc - F_new %*% t(Lambda_hat)
  }

  list(
    factors = F_new,
    residual = U_new,
    design = cbind(F_new, U_new)
  )
}

###################################################################################
## Prediction helper
###################################################################################
farm.predict <- function(object, newx, type = c("response", "link", "class")) {
  if (!inherits(object, "farm.select")) {
    stop("object must be a farm.select object")
  }

  type <- match.arg(type)
  newx <- as.matrix(newx)

  tr <- farm.transform_newx(object, newx)
  Xnew <- tr$design

  beta <- object$coef.full

  if (length(beta) == 0) {
    if (object$family == "gaussian") {
      out <- rep(if (is.null(object$y.mean)) 0 else object$y.mean, nrow(newx))
      return(as.numeric(out))
    } else {
      out <- rep(0.5, nrow(newx))
      if (type == "class") return(as.integer(out >= 0.5))
      if (type == "link") return(stats::qlogis(out))
      return(as.numeric(out))
    }
  }

  if (anyNA(beta)) {
    stop("coef.full contains NA values; cannot predict.")
  }

  expected_len <- 1L + ncol(Xnew)
  if (length(beta) != expected_len) {
    stop(
      paste0(
        "Coefficient vector has wrong length. Expected ",
        expected_len, " but got ", length(beta), "."
      )
    )
  }

  eta <- drop(cbind(1, Xnew) %*% beta)

  if (object$family == "gaussian") {
    if (type == "class") stop("type='class' not available for gaussian model")
    if (type == "link") return(as.numeric(eta))
    return(as.numeric(eta + object$y.mean))
  }

  p <- stats::plogis(eta)

  if (type == "link") return(as.numeric(eta))
  if (type == "class") return(as.integer(p >= 0.5))
  as.numeric(p)
}

###################################################################################
## S3 predict method
###################################################################################
#' @export
predict.farm.select <- function(object, newx, type = c("response", "link", "class"), ...) {
  farm.predict(object = object, newx = newx, type = type)
}
