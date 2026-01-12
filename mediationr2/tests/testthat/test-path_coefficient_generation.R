test_that("path_coefficient_generation respects magnitude bins", {
  # err = 0 makes the output deterministic (runif(min=max) == constant)
  mag <- c(0, 1, 2, 3)
  out <- path_coefficient_generation(m = length(mag), magnitude_effect = mag, err = 0)

  expect_equal(out, c(0, 0.14, 0.39, 0.59))
})

test_that("path_coefficient_generation errors when m != length(magnitude_effect)", {
  expect_error(path_coefficient_generation(m = 2, magnitude_effect = c(0, 1, 2)))
})

test_that("path_coefficient_generation outputs are within expected ranges when err > 0", {
  set.seed(10)
  err <- 0.02
  mag <- c(0, 1, 1, 2, 2, 3, 3)
  out <- path_coefficient_generation(m = length(mag), magnitude_effect = mag, err = err)

  expect_true(all(out[mag == 0] == 0))
  expect_true(all(out[mag == 1] >= (0.14 - err) & out[mag == 1] <= (0.14 + err)))
  expect_true(all(out[mag == 2] >= (0.39 - err) & out[mag == 2] <= (0.39 + err)))
  expect_true(all(out[mag == 3] >= (0.59 - err) & out[mag == 3] <= (0.59 + err)))
})
