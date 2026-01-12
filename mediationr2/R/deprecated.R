# Internal / experimental utilities retained from the original scripts.

simulation_R2_Z <- function(n, r2, corr_mat, beta) {
  .check_optional_pkgs(c("MASS"))
  p <- ncol(corr_mat)
  sample <- as.data.frame(MASS::mvrnorm(n = n, mu = rep(0, p), Sigma = corr_mat, empirical = FALSE))

  # Rough heuristic for error variance to target a given marginal R^2.
  mean_cor <- sqrt((sum(corr_mat^2) - sum(diag(corr_mat^2))) / (p * p - p))
  var_epsilon <- (sum(beta^2) + mean_cor) * ((1 - r2) / r2)

  eps <- stats::rnorm(n, sd = sqrt(var_epsilon))
  sample$Y <- as.numeric(as.matrix(sample[, 1:p]) %*% as.matrix(beta)) + eps

  sample
}
