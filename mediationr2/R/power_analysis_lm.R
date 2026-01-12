#' Bootstrap power analysis for linear mediation using R2 effect size
#'
#' Simulates a mediation model with p_M mediators and p_Y responses, then estimates
#' power using bootstrap confidence intervals for the R2 effect size.
#'
#' @param n Integer sample size.
#' @param p_M Integer number of mediators.
#' @param p_Y Integer number of response dimensions.
#' @param B Integer number of bootstrap replicates.
#' @param errorM Numeric mediator noise SD.
#' @param errorY Numeric response noise SD (used when snr_generation is FALSE).
#' @param mean_tau Numeric mean direct effect (tau) used to generate Y.
#' @param error_level Numeric jitter level for path coefficients.
#' @param n_mc Integer number of Monte Carlo repetitions.
#' @param mag_XM Integer vector (length p_M) of magnitude labels for X->M path.
#' @param mag_MY Integer matrix (p_M x p_Y) of magnitude labels for M->Y paths.
#' @param magnitude (Unused) retained for backward compatibility with the original script.
#' @param alpha_level Significance level.
#' @param covariate_adjustment Logical; if TRUE, generates Z covariates correlated with Y and residualizes Y on Z.
#' @param n_Z Integer number of covariates Z (used when covariate_adjustment is TRUE).
#' @param sigma_z Numeric SD for Z generation noise (used when covariate_adjustment is TRUE).
#' @param snr_generation Logical; if TRUE, adds noise to achieve a target SNR.
#' @param snr Numeric target signal-to-noise ratio used when snr_generation is TRUE.
#'
#' @return Numeric vector of length p_Y: estimated power for each response dimension.
#' @export
mediation_power_analysis_lm_r2 <- function(
  n, p_M, p_Y, B = 500, errorM = 1, errorY = 1, mean_tau,
  error_level = 0.02, n_mc = 500, mag_XM, mag_MY, magnitude,
  alpha_level = 0.05, covariate_adjustment = FALSE, n_Z = 2, sigma_z = 1,
  snr_generation = FALSE, snr = 1
) {
  power_result <- matrix(0, nrow = n_mc, ncol = p_Y)

  for (k in seq_len(n_mc)) {
    # Exposure
    X <- stats::rnorm(n, 0, 1)

    # X -> M
    path_coefficient_XM <- path_coefficient_generation(p_M, mag_XM, type = "alpha", err = error_level)

    # Mediators
    M <- matrix(NA_real_, nrow = n, ncol = p_M)
    colnames(M) <- paste0("M", seq_len(p_M))
    for (i in seq_len(p_M)) {
      M[, i] <- X * path_coefficient_XM[i] + stats::rnorm(n, 0, errorM)
    }

    # Direct effect
    tau_direct <- matrix(0, nrow = n, ncol = p_Y)
    for (j in seq_len(p_Y)) {
      tau_direct[, j] <- stats::rnorm(n, mean_tau, error_level) * X
    }

    # M -> Y and responses
    path_coefficient_MY <- matrix(NA_real_, nrow = p_M, ncol = p_Y)
    Y <- matrix(0, nrow = n, ncol = p_Y)

    for (j in seq_len(p_Y)) {
      path_coefficient_MY[, j] <- path_coefficient_generation(p_M, mag_MY[, j], err = error_level, type = "beta")
      Y[, j] <- M %*% path_coefficient_MY[, j] + tau_direct[, j]

      if (isTRUE(snr_generation)) {
        vary <- stats::var(Y[, j])
        errY <- vary / snr
        Y[, j] <- Y[, j] + stats::rnorm(n, 0, sqrt(errY))
      } else {
        Y[, j] <- Y[, j] + stats::rnorm(n, 0, errorY)
      }
    }

    # Optional covariate adjustment: generate Z correlated with Y and residualize
    if (isTRUE(covariate_adjustment)) {
      Z <- matrix(NA_real_, nrow = n, ncol = n_Z)
      r <- rowMeans(Y)
      for (i in seq_len(n_Z)) {
        Z[, i] <- r + stats::rnorm(n, 0, sigma_z)
        r <- stats::residuals(stats::lm(r ~ Z[, 1:i]))
      }
      for (j in seq_len(p_Y)) {
        fit <- stats::lm(Y[, j] ~ Z)
        Y[, j] <- stats::residuals(fit)
      }
    }

    # Bootstrap distribution of effect size
    r2_boot_es <- matrix(0, nrow = B, ncol = p_Y)
    for (b in seq_len(B)) {
      boot_ind <- sample.int(n, n, replace = TRUE)
      r2_boot_es[b, ] <- r2_es(X[boot_ind], M[boot_ind, , drop = FALSE], Y[boot_ind, , drop = FALSE])
    }

    # Bonferroni across responses
    power_result[k, ] <- apply(r2_boot_es, 2, function(x) {
      lo <- stats::quantile(x, probs = alpha_level / (2 * p_Y), na.rm = TRUE)
      hi <- stats::quantile(x, probs = 1 - alpha_level / (2 * p_Y), na.rm = TRUE)
      ifelse((lo > 0) | (hi < 0), 1, 0)
    })
  }

  colMeans(power_result)
}
