test_that("Seek vector works", {
  mean_here <- 3.533
  sd_here <- 1.561
  n <- 45
  vec <- simulate_vector(mean = mean_here, sd = sd_here, n = n, scale_min = 1, scale_max = 7, dp = 3)
  expect_equivalent(mean(vec), mean_here, tolerance = 1e-3)
  expect_equivalent(sd(vec), sd_here, tolerance = 1e-3)
  expect_equal(length(vec), n)
})

test_that("get sample creates correct samples", {
  # using example from original shiny app
  samples_shiny <- simulate_samples_matrix(max_cases = 9, n = 45, mean = 3.533, sd = 1.561, scale_min = 1, scale_max = 7, dp = 3, fixed = c())
  expect_true(all(round(apply(samples_shiny, 1, mean), 3) == 3.533))
  expect_true(all(round(apply(samples_shiny, 1, sd), 3) == 1.561))
  expect_equal(nrow(samples_shiny), 9)
  expect_equal(ncol(samples_shiny), 45)
})
