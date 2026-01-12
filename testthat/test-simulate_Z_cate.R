test_that("simulate_Z_cate returns a binary matrix with correct dimensions", {
  set.seed(123)
  n <- 50
  n_Z <- 4

  Z <- simulate_Z_cate(n = n, n_Z = n_Z)

  expect_true(is.matrix(Z))
  expect_equal(dim(Z), c(n, n_Z))
  expect_true(all(Z %in% c(0, 1)))

  # Each column should have a reasonable number of ones by construction
  col_sums <- colSums(Z)
  lower <- round(0.3 * n) - 1
  upper <- round(0.7 * n) + 1
  expect_true(all(col_sums >= lower))
  expect_true(all(col_sums <= upper))
})
