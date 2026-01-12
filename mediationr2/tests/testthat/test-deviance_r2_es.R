test_that("deviance_r2_es returns a finite scalar and matches its definition", {
  skip_if_not_installed("pscl")

  set.seed(123)
  n <- 300
  X <- rnorm(n)
  M <- cbind(M1 = 0.7 * X + rnorm(n))

  # Y can be binary or continuous; deviance_r2_es currently relies on glm() defaults.
  # Keep this test aligned with the current implementation by recomputing with the same glm calls.
  eta <- -0.2 + 0.8 * X + 0.9 * M[, 1]
  p <- plogis(eta)
  Y <- rbinom(n, size = 1, prob = p)

  out <- deviance_r2_es(X = X, M = M, Y = Y)
  expect_true(length(out) == 1)
  expect_true(is.finite(out))

  # Manual recomputation mirroring the function body
  dev_yx <- pscl::pR2(glm(Y ~ X))["r2ML"]
  df_YMX <- data.frame(cbind(Y, M, X))
  dev_ymx <- pscl::pR2(glm(V1 ~ ., data = df_YMX))["r2ML"]
  df_YM <- data.frame(cbind(Y, M))
  dev_ym <- pscl::pR2(glm(V1 ~ ., data = df_YM))["r2ML"]

  expect_equal(as.numeric(out), as.numeric(dev_yx + dev_ym - dev_ymx), tolerance = 1e-10)
})
