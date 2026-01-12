test_that("r2_es matches the definition r2_xy + r2_ym - r2_ymx", {
  set.seed(123)
  n <- 200
  X <- rnorm(n)
  M <- cbind(M1 = 0.8 * X + rnorm(n), M2 = rnorm(n))
  Y <- cbind(Y1 = 0.5 * X + 0.7 * M[, 1] + rnorm(n))

  out <- r2_es(X = X, M = M, Y = Y)
  expect_length(out, 1)

  df_XY <- data.frame(X = X, Y = Y[, 1])
  r2_xy <- summary(lm(Y ~ X, data = df_XY))$r.squared

  df_YMX <- data.frame(cbind(Y[, 1], M, X))
  r2_ymx <- summary(lm(V1 ~ ., data = df_YMX))$r.squared

  df_YM <- data.frame(cbind(Y[, 1], M))
  r2_ym <- summary(lm(V1 ~ ., data = df_YM))$r.squared

  expect_equal(out[1], r2_xy + r2_ym - r2_ymx, tolerance = 1e-12)
})

test_that("r2_es returns one value per response column", {
  set.seed(1)
  n <- 120
  X <- rnorm(n)
  M <- cbind(M1 = X + rnorm(n))
  Y <- cbind(Y1 = X + M[, 1] + rnorm(n),
             Y2 = -X + 0.5 * M[, 1] + rnorm(n))

  out <- r2_es(X = X, M = M, Y = Y)
  expect_length(out, 2)
  expect_true(all(is.finite(out)))
})
