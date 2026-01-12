#' Simulate binary (categorical) covariates Z
#'
#' Generates an n x n_Z matrix of binary covariates. Each column has a random
#' proportion of ones between 30% and 70%.
#'
#' @param n Integer sample size.
#' @param n_Z Integer number of binary covariates.
#' @return A numeric matrix of dimension n x n_Z with entries in {0,1}.
#' @export
simulate_Z_cate <- function(n, n_Z) {
  Z_cat <- matrix(0, nrow = n, ncol = n_Z)
  for (i in seq_len(n_Z)) {
    n_non_zero <- round(n * stats::runif(1, 0.3, 0.7))
    Z_cat[sample.int(n, n_non_zero, replace = FALSE), i] <- 1
  }
  Z_cat
}

#' Simulate continuous covariates Z (requires suggested packages)
#'
#' This function uses simstudy::genCorMat, corpcor::make.positive.definite, and
#' semTools::mvrnonnorm2 to generate non-normal correlated covariates.
#'
#' @param n Integer sample size.
#' @param n_Z Integer number of continuous covariates.
#' @return A numeric matrix of dimension n x n_Z.
#' @export
simulate_Z_cont <- function(n, n_Z) {
  .check_optional_pkgs(c("simstudy", "corpcor", "semTools"))
  mu <- stats::rnorm(n_Z, 0, 5)
  cor_mat <- corpcor::make.positive.definite(simstudy::genCorMat(n_Z))
  skew_vec <- pmax(stats::rnorm(n_Z, 1, 0.5), 0)
  kurtosis_vec <- pmax(stats::rnorm(n_Z, 2, 1), 0)
  semTools::mvrnonnorm2(n, mu, cor_mat, skew_vec, kurtosis_vec)
}

#' Simulate covariates Z with mixed continuous and binary components
#'
#' @param n Integer sample size.
#' @param n_Z Integer total number of covariates.
#' @param n_cont Integer number of continuous covariates (0 <= n_cont <= n_Z).
#' @return A numeric matrix of dimension n x n_Z.
#' @export
simulate_Z <- function(n, n_Z, n_cont) {
  n_cate <- n_Z - n_cont
  if (n_cont > 0 && n_cate > 0) {
    Z_cont <- simulate_Z_cont(n, n_cont)
    Z_cate <- simulate_Z_cate(n, n_cate)
    Z <- cbind(Z_cont, Z_cate)
  } else if (n_cont == 0) {
    Z <- simulate_Z_cate(n, n_Z)
  } else {
    Z <- simulate_Z_cont(n, n_Z)
  }
  Z
}
