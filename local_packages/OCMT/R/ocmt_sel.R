#' One-Covariate Multiple Testing (OCMT) selection
#'
#' Implements the OCMT selection idea: for each covariate X[, i], run a regression
#' of Y on X[, i] plus conditioning regressors Xcond, compute a test statistic for
#' the coefficient on X[, i], and select covariates that exceed a Bonferroni-type
#' normal threshold.
#'
#' For \code{link = "mean"}, the test statistic is the absolute OLS \eqn{t}-statistic.
#' For \code{link = "logit"} or \code{link = "probit"}, the test statistic is the
#' absolute Wald \eqn{z}-statistic from a binomial GLM.
#'
#' Optionally runs the "multiple stage" boosting loop.
#'
#' @param Y Numeric vector length N. For \code{link = "logit"} or \code{link = "probit"},
#'   \code{Y} must be a finite 0/1 vector.
#' @param X Numeric matrix N x P (or something coercible to a matrix).
#' @param Xcond Optional numeric matrix N x K (or NULL).
#' @param pval Overall significance level (e.g. 0.05).
#' @param delta1 First-stage exponent (MATLAB: p_value1 = pval / P^(delta1-1)).
#' @param delta2 Second-stage exponent (only used if multi_stage = TRUE).
#' @param multi_stage Logical; run the additional boosting stages.
#' @param max_iter Integer; safety cap on boosting iterations.
#' @param kappa_const Numeric; constant used in first-stage kappa = |b|/sqrt(kappa_const).
#' @param link Character string; one of \code{"mean"}, \code{"logit"}, or \code{"probit"}.
#' @param verbose Logical; if TRUE, prints a short summary + selected-variable table.
#' @param show Integer; max number of selected variables to print when verbose = TRUE.
#'
#' @return A list with:
#' \describe{
#'   \item{ind}{Logical vector length P: selected variables after (optional) boosting.}
#'   \item{ind_first}{Logical vector length P: selected variables after first stage only.}
#'   \item{added_iter}{Integer vector length P: 0 if selected in stage 1, 1/2/... if added in boosting, NA otherwise.}
#'   \item{tstat_first}{Numeric vector length P: first-stage absolute test statistics. These are \eqn{|t|} for \code{link = "mean"} and \eqn{|z|} for \code{link = "logit"} or \code{"probit"}.}
#'   \item{tstat_boost_last}{Numeric vector length P: last boosting-stage absolute test statistic each variable achieved when tested (NA if never tested in boosting).}
#'   \item{kappa_first}{Numeric vector length P: first-stage kappa values.}
#'   \item{threshold1}{First-stage threshold.}
#'   \item{threshold2}{Second-stage threshold (NA if multi_stage = FALSE).}
#'   \item{p_value1}{First-stage p_value1.}
#'   \item{p_value2}{Second-stage p_value2 (NA if multi_stage = FALSE).}
#'   \item{iterations}{Number of boosting iterations performed (0 if multi_stage = FALSE).}
#'   \item{selected}{Integer indices of selected variables.}
#'   \item{selected_names}{Column names of selected variables (NULL if none).}
#' }
#'
#' @export
ocmt_sel <- function(Y, X, Xcond = NULL,
                     pval = 0.05, delta1 = 1, delta2 = 1,
                     multi_stage = FALSE, max_iter = 50,
                     kappa_const = 0.5,
                     link = c("mean", "logit", "probit"),
                     verbose = FALSE, show = 25) {

  link <- match.arg(link)

  # ---- input checks / coercions ----
  if (is.null(Y) || !is.numeric(Y)) stop("Y must be a numeric vector.")
  Y <- as.numeric(Y)

  if (link %in% c("logit", "probit")) {
    if (any(!is.finite(Y)) || !all(Y %in% c(0, 1))) {
      stop("For link = 'logit' or 'probit', Y must be a finite 0/1 vector.")
    }
  }

  X <- as.matrix(X)
  if (!is.numeric(X)) stop("X must be numeric (matrix or coercible to matrix).")

  N <- nrow(X)
  P <- ncol(X)
  if (length(Y) != N) stop("Length of Y must match nrow(X).")

  if (is.null(Xcond)) {
    Xcond <- matrix(numeric(0), nrow = N, ncol = 0)
  } else {
    Xcond <- as.matrix(Xcond)
    if (!is.numeric(Xcond)) stop("Xcond must be numeric (matrix or coercible to matrix).")
    if (nrow(Xcond) != N) stop("nrow(Xcond) must match nrow(X).")
  }

  # helper: absolute test statistic for the SECOND column (the focal covariate)
  # in the design matrix:
  # - OLS |t| if link = "mean"
  # - GLM Wald |z| if link = "logit" or "probit"
  stat_for_second_col <- function(y, design) {
    ok <- is.finite(y) & (rowSums(!is.finite(design)) == 0L)
    y2 <- y[ok]
    X2 <- design[ok, , drop = FALSE]

    n <- length(y2)
    k <- ncol(X2)

    if (n <= k) {
      return(list(beta1 = NA_real_, se1 = NA_real_, stat = NA_real_))
    }

    if (link %in% c("logit", "probit")) {
      if (length(unique(y2)) < 2) {
        return(list(beta1 = NA_real_, se1 = NA_real_, stat = NA_real_))
      }

      # Match the working BMT pattern: glm() on a data frame + Wald z for V1
      df <- data.frame(y = y2, X2[, -1, drop = FALSE], check.names = FALSE)
      colnames(df) <- c("y", paste0("V", seq_len(ncol(df) - 1)))

      fml <- stats::as.formula(
        paste0("y ~ ", paste(colnames(df)[-1], collapse = " + "))
      )

      fam <- if (link == "logit") stats::binomial("logit") else stats::binomial("probit")

      fit <- tryCatch(
        suppressWarnings(
          stats::glm(
            fml,
            data = df,
            family = fam,
            control = stats::glm.control(maxit = max_iter)
          )
        ),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        return(list(beta1 = NA_real_, se1 = NA_real_, stat = NA_real_))
      }

      sm <- tryCatch(summary(fit), error = function(e) NULL)
      if (is.null(sm) || is.null(sm$coefficients) || !("V1" %in% rownames(sm$coefficients))) {
        return(list(beta1 = NA_real_, se1 = NA_real_, stat = NA_real_))
      }

      beta1 <- sm$coefficients["V1", "Estimate"]
      se1   <- sm$coefficients["V1", "Std. Error"]
      stat  <- abs(sm$coefficients["V1", "z value"])

      return(list(beta1 = beta1, se1 = se1, stat = stat))
    }

    # ---- mean link: OLS |t| ----
    qrX <- qr(X2)
    coef <- qr.coef(qrX, y2)
    resid <- qr.resid(qrX, y2)

    df_res <- n - k
    sigma2 <- sum(resid^2) / df_res

    R <- qr.R(qrX)
    XtX_inv <- tryCatch(chol2inv(R), error = function(e) NULL)
    if (is.null(XtX_inv) || any(!is.finite(XtX_inv))) {
      return(list(beta1 = NA_real_, se1 = NA_real_, stat = NA_real_))
    }

    beta1 <- coef[2]
    se1 <- sqrt(XtX_inv[2, 2] * sigma2)
    stat <- abs(beta1) / se1

    list(beta1 = beta1, se1 = se1, stat = stat)
  }

  # ---- thresholds (match MATLAB structure) ----
  p_value1 <- pval / (P^(delta1 - 1))
  threshold1 <- stats::qnorm(1 - (p_value1 / (2 * P)))

  # ---- first stage ----
  tstat_first <- rep(NA_real_, P)
  kappa_first <- rep(NA_real_, P)
  ind <- rep(FALSE, P)

  for (i in seq_len(P)) {
    Xi <- X[, i]
    design <- cbind(1, Xi, Xcond)
    fit <- stat_for_second_col(Y, design)

    tstat_first[i] <- fit$stat
    kappa_first[i] <- abs(fit$beta1) / sqrt(kappa_const)
    ind[i] <- is.finite(fit$stat) && (fit$stat > threshold1)
  }

  ind_first <- ind

  # Track which stage added the variable: 0 = first stage, 1.. = boosting iteration
  added_iter <- rep(NA_integer_, P)
  added_iter[ind_first] <- 0L

  # Store last boosting-stage test statistic for each variable
  tstat_boost_last <- rep(NA_real_, P)

  iterations <- 0L
  threshold2 <- NA_real_
  p_value2 <- NA_real_

  # ---- multiple stage (boosting) ----
  if (isTRUE(multi_stage)) {
    p_value2 <- pval / (P^(delta2 - 1))
    threshold2 <- stats::qnorm(1 - (p_value2 / (2 * P)))

    for (itr in seq_len(max_iter)) {
      ind0 <- ind
      add <- rep(FALSE, P)

      sel_idx <- which(ind0)
      Xsel <- if (length(sel_idx) > 0) X[, sel_idx, drop = FALSE] else matrix(numeric(0), N, 0)

      for (i in seq_len(P)) {
        if (!ind0[i]) {
          Xi <- X[, i]
          design <- cbind(1, Xi, Xsel, Xcond)
          fit <- stat_for_second_col(Y, design)

          tstat_boost_last[i] <- fit$stat
          add[i] <- is.finite(fit$stat) && (fit$stat > threshold2)
        }
      }

      newly_added <- which(add)
      if (length(newly_added) > 0) {
        added_iter[newly_added] <- itr
      }

      ind <- ind0 | add
      iterations <- itr
      if (!any(add)) break
    }
  }

  sel <- which(ind)
  nm <- colnames(X)
  selected_names <- if (!is.null(nm) && length(sel) > 0) nm[sel] else NULL

  # ---- optional printing ----
  if (isTRUE(verbose)) {
    cat("OCMT summary\n")
    cat("  link =", link, "\n")
    cat("  P =", P, "\n")
    cat("  Selected stage 1:", sum(ind_first), "\n")
    cat("  Selected final:  ", sum(ind), "\n", sep = "")
    cat("  threshold1 =", signif(threshold1, 4), "\n")
    if (isTRUE(multi_stage)) {
      cat("  threshold2 =", signif(threshold2, 4), "  (iterations =", iterations, ")\n", sep = "")
    }

    if (length(sel) > 0) {
      var_names <- if (!is.null(nm)) nm else paste0("V", seq_len(P))

      t_first_sel <- tstat_first[sel]
      t_boost_sel <- tstat_boost_last[sel]
      t_best <- ifelse(is.finite(t_boost_sel), t_boost_sel, t_first_sel)

      tab <- data.frame(
        index = sel,
        name = var_names[sel],
        added_iter = added_iter[sel],
        t_first = t_first_sel,
        t_boost_last = t_boost_sel,
        t_best = t_best,
        stringsAsFactors = FALSE
      )

      tab <- tab[order(tab$added_iter, -tab$t_best), , drop = FALSE]

      cat("\nSelected variables (added_iter: 0=stage1, 1+=boosting round)\n")
      print(utils::head(tab, show), row.names = FALSE)
      if (nrow(tab) > show) cat("... (showing first ", show, ")\n", sep = "")
    }
  }

  list(
    ind = ind,
    ind_first = ind_first,
    added_iter = added_iter,
    tstat_first = tstat_first,
    tstat_boost_last = tstat_boost_last,
    kappa_first = kappa_first,
    threshold1 = threshold1,
    threshold2 = threshold2,
    p_value1 = p_value1,
    p_value2 = p_value2,
    iterations = iterations,
    selected = sel,
    selected_names = selected_names
  )
}
