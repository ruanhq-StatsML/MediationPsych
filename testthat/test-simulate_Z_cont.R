test_that("simulate_Z_cont returns a numeric matrix with correct dimensions", {
  skip_if_not_installed("simstudy")
  skip_if_not_installed("semTools")
  skip_if_not_installed("corpcor")

  set.seed(123)
  n <- 40
  n_Z <- 3

  Z <- simulate_Z_cont(n = n, n_Z = n_Z)

  Zm <- as.matrix(Z)
  expect_equal(dim(Zm), c(n, n_Z))
  expect_true(is.numeric(Zm))
  expect_true(all(is.finite(Zm)))

  # Sanity: columns should not be all-constant
  expect_true(all(apply(Zm, 2, function(z) sd(z) > 0)))
})
