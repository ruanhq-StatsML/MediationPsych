#' Bootstrap power analysis for generalized linear mediation using deviance pseudo-R2
#'
#' Simulates a mediation model and estimates power via bootstrap confidence intervals
#' for the deviance-based pseudo-R2 effect size.
#'
#' @param n Integer sample size.
#' @param p_M Integer number of mediators.
#' @param p_Y Integer number of response dimensions (default 1).
#' @param B Integer number of bootstrap replicates.
#' @param errorM Numeric mediator noise SD.
#' @param errorY Numeric response noise SD.
#' @param mean_tau Numeric mean direct effect.
#' @param error_tau Numeric SD for tau generation.
#' @param n_mc Integer number of Monte Carlo repetitions.
#' @param mag_XM Integer vector (length p_M) of magnitude labels for X->M path.
#' @param mag_MY Integer vector (length p_M) of magnitude labels for M->Y path (for p_Y = 1)
#'   or matrix (p_M x p_Y) when p_Y > 1.
#' @param alpha_level Significance level.
#' @param family A stats::family object passed through to deviance_r2_es().
#'
#' @return Numeric vector of length p_Y: estimated power for each response dimension.
#' @export
deviance_r2_pma <- function(
  n, p_M, p_Y = 1, B = 500, errorM = 1, errorY = 1,
  mean_tau, error_tau = 1e-4, n_mc = 500, mag_XM, mag_MY,
  alpha_level = 0.05, family = stats::gaussian()
) {
  power_result <- matrix(0, nrow = n_mc, ncol = p_Y)

  # Ensure mag_MY is p_M x p_Y
  if (p_Y == 1 && is.null(dim(mag_MY))) {
    mag_MY <- matrix(mag_MY, nrow = p_M, ncol = 1)
  }

  for (k in seq_len(n_mc)) {
    X <- stats::rnorm(n, 0, 1)

    path_coefficient_XM <- path_coefficient_generation(p_M, mag_XM, type = "alpha")

    M <- matrix(NA_real_, nrow = n, ncol = p_M)
    colnames(M) <- paste0("M", seq_len(p_M))
    for (i in seq_len(p_M)) {
      M[, i] <- X * path_coefficient_XM[i] + stats::rnorm(n, 0, errorM)
    }

    # Direct effect: tau varies across individuals
    tau_direct <- stats::rnorm(n, mean_tau, error_tau)

    # M -> Y
    Y <- matrix(0, nrow = n, ncol = p_Y)
    for (j in seq_len(p_Y)) {
      path_coefficient_MY <- path_coefficient_generation(p_M, mag_MY[, j], type = "beta")
      t <- M %*% path_coefficient_MY + tau_direct * X
      Y[, j] <- t + stats::rnorm(n, 0, errorY)
    }

    r2_boot_es <- matrix(0, nrow = B, ncol = p_Y)
    for (b in seq_len(B)) {
      boot_ind <- sample.int(n, n, replace = TRUE)
      X_B <- X[boot_ind]
      M_B <- M[boot_ind, , drop = FALSE]
      Y_B <- Y[boot_ind, , drop = FALSE]

      # In the original script, plogis() is applied before GLM pseudo-R2.
      r2_boot_es[b, ] <- apply(Y_B, 2, function(ycol) {
        deviance_r2_es(X_B, M_B, stats::plogis(ycol), family = family)
      })
    }

    power_result[k, ] <- apply(r2_boot_es, 2, function(x) {
      lo <- stats::quantile(x, probs = alpha_level / (2 * p_Y), na.rm = TRUE)
      hi <- stats::quantile(x, probs = 1 - alpha_level / (2 * p_Y), na.rm = TRUE)
      ifelse((lo > 0) | (hi < 0), 1, 0)
    })
  }

  colMeans(power_result)
}
