#' Generate path coefficients with magnitude categories
#'
#' Generates a length-m vector of path coefficients according to magnitude labels.
#' The mapping follows MacKinnon-style heuristics:
#' - 0: 0
#' - 1: around 0.14
#' - 2: around 0.39
#' - 3: around 0.59
#'
#' @param m Integer number of coefficients to generate.
#' @param magnitude_effect Integer vector of length m with entries in {0,1,2,3}.
#' @param type Character, either "alpha" or "beta" (currently treated identically).
#' @param err Nonnegative scalar controlling random jitter around the magnitude targets.
#' @return A numeric vector of length m.
#' @export
path_coefficient_generation <- function(m, magnitude_effect, type = "alpha", err = 0.02) {
  if (length(magnitude_effect) != m) {
    stop("m must equal length(magnitude_effect).", call. = FALSE)
  }
  path_effect <- rep(0, m)

  zero_idx <- which(magnitude_effect == 0)
  path_effect[zero_idx] <- 0

  small_idx <- which(magnitude_effect == 1)
  path_effect[small_idx] <- stats::runif(length(small_idx), 0.14 - err, 0.14 + err)

  medium_idx <- which(magnitude_effect == 2)
  path_effect[medium_idx] <- stats::runif(length(medium_idx), 0.39 - err, 0.39 + err)

  large_idx <- which(magnitude_effect == 3)
  path_effect[large_idx] <- stats::runif(length(large_idx), 0.59 - err, 0.59 + err)

  path_effect
}

#' R2 effect size for mediation (MacKinnon-style)
#'
#' Computes the R2-based effect size:
#'   R2(X,Y) + R2(M,Y) - R2(X,M,Y)
#' for each column of Y.
#'
#' @param X Numeric vector (exposure).
#' @param M Numeric matrix (mediators).
#' @param Y Numeric matrix or vector (response(s)).
#' @return Numeric vector of length ncol(Y) (or length 1 if Y is a vector).
#' @export
r2_es <- function(X, M, Y) {
  if (is.null(dim(Y))) {
    Y <- matrix(Y, ncol = 1)
  }
  out <- numeric(ncol(Y))
  for (j in seq_len(ncol(Y))) {
    df_XY <- data.frame(X = X, Y = Y[, j])
    r2_xy <- summary(stats::lm(Y ~ X, data = df_XY))$r.squared

    df_YMX <- data.frame(cbind(Y[, j], M, X))
    r2_ymx <- summary(stats::lm(V1 ~ ., data = df_YMX))$r.squared

    df_YM <- data.frame(cbind(Y[, j], M))
    r2_ym <- summary(stats::lm(V1 ~ ., data = df_YM))$r.squared

    out[j] <- r2_xy + r2_ym - r2_ymx
  }
  out
}

#' Deviance-based pseudo-R2 effect size (requires suggested package pscl)
#'
#' Uses pscl::pR2 on GLM fits to compute an effect size analogous to r2_es.
#'
#' @param X Numeric vector (exposure).
#' @param M Numeric matrix (mediators).
#' @param Y Numeric vector (response).
#' @param family A stats::family object passed to stats::glm.
#' @return Numeric scalar effect size.
#' @export
deviance_r2_es <- function(X, M, Y, family = stats::gaussian()) {
  .check_optional_pkgs("pscl")
  deviance_r2_yx <- pscl::pR2(stats::glm(Y ~ X, family = family))["r2ML"]

  df_YMX <- data.frame(cbind(Y, M, X))
  deviance_r2_ymx <- pscl::pR2(stats::glm(V1 ~ ., data = df_YMX, family = family))["r2ML"]

  df_YM <- data.frame(cbind(Y, M))
  deviance_r2_ym <- pscl::pR2(stats::glm(V1 ~ ., data = df_YM, family = family))["r2ML"]

  as.numeric(deviance_r2_yx + deviance_r2_ym - deviance_r2_ymx)
}
