test_that("Seek vector works", {
  set.seed(1224)
  mean_here <- 3.533
  sd_here <- 1.561
  n <- 45
  vec <- seek_vector(mean = mean_here, sd = sd_here, n = n, scale_min = 1, scale_max = 7, dp = 3)
  expect_equivalent(mean(vec), mean_here, tolerance = 1e-3)
  expect_equivalent(sd(vec), sd_here, tolerance = 1e-3)
  expect_equal(length(vec), n)
})

test_that("get sample creates correct samples", {
  set.seed(1233)
  N <- 10
  tMean <- 3.5
  tSD <- 1.561
  dp <- 2
  max_cases <- 10
  samples <- get_samples(max_cases = max_cases, n = N, mean = tMean, sd = tSD, scale_min = 1, scale_max = 7, dp = 2)
  expect_equal(nrow(samples), max_cases)
  expect_equal(ncol(samples), N)
  expect_equal(samples[1, ], c(1, 3, 3, 3, 3, 4, 4, 4, 4, 6))
  set.seed(1234)
  # using example from shiny app
  samples_shiny <- get_samples(max_cases = 9, n = 45, mean = 3.533, sd = 1.561, scale_min = 1, scale_max = 7, dp = 3, fixed = c())
  expect_true(all(round(apply(samples_shiny, 1, mean), 3) == 3.533))
  expect_true(all(round(apply(samples_shiny, 1, sd), 3) == 1.561))
})
