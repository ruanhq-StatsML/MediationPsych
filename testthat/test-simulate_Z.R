test_that("simulate_Z returns correct dimensions for mixed Z types", {
  skip_if_not_installed("simstudy")
  skip_if_not_installed("semTools")
  skip_if_not_installed("corpcor")

  set.seed(123)
  n <- 60
  n_Z <- 6
  n_cont <- 2

  Z <- simulate_Z(n = n, n_Z = n_Z, n_cont = n_cont)
  Zm <- as.matrix(Z)

  expect_equal(dim(Zm), c(n, n_Z))
  # last (n_Z - n_cont) columns should be binary if you generate categorical Z via simulate_Z_cate()
  Z_cat <- Zm[, (n_cont + 1):n_Z, drop = FALSE]
  expect_true(all(Z_cat %in% c(0, 1)))
})

test_that("simulate_Z works when all covariates are categorical", {
  set.seed(1)
  n <- 20
  n_Z <- 5
  n_cont <- 0

  Z <- simulate_Z(n = n, n_Z = n_Z, n_cont = n_cont)
  expect_equal(dim(Z), c(n, n_Z))
  expect_true(all(Z %in% c(0, 1)))
})

test_that("simulate_Z works when all covariates are continuous", {
  skip_if_not_installed("simstudy")
  skip_if_not_installed("semTools")
  skip_if_not_installed("corpcor")

  set.seed(1)
  n <- 25
  n_Z <- 4
  n_cont <- 4

  Z <- simulate_Z(n = n, n_Z = n_Z, n_cont = n_cont)
  Zm <- as.matrix(Z)
  expect_equal(dim(Zm), c(n, n_Z))
  expect_true(is.numeric(Zm))
})
