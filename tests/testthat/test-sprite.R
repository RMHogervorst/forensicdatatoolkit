context("sprite utils")
test_that("step_size works", {
  set.seed(1234)
  expect_equal(step_size(), 2)
  expect_equal(step_size(), 1)
})

test_that("granularity works", {
  expect_equal(set_granularity(2), (0.005 + rSprite.dust))
  expect_equal(set_granularity(1), (0.05 + rSprite.dust))
})

test_that("mean bumping works", {
  set.seed(1234)
  expect_equal(
    loop_until_mean_is_ok(vec = c(1, 2, 3, 4), mean = 2.5, scale_min = 1, scale_max = 8, dp=2),
    c(1, 2, 3, 4)
  )
  expect_equal(
    loop_until_mean_is_ok(vec = c(1, 1, 2, 4), mean = 2.5, scale_min = 1, scale_max = 8, dp=2),
    c(1, 1, 4, 4)
  )
})

test_that("make_mean_grim works", {
  expect_equal(make_mean_grim(mean = 2.5, n = 4, dp = 2), 2.5)
  expect_warning((test_result <- make_mean_grim(mean = 2.5, n = 3, dp = 2)),
                 regexp = "Fails GRIM test for mean 2.5, N=3 nearest mean is: 2.67"
  )
  expect_equal(test_result, 2.67)
})

test_that("sd_limits returns the same numbers as rSprite.sdLimits", {
  expect_error(determine_sd_limits(2.5, 4, scale_min = 9, scale_max = 1))
  expect_equal(
    determine_sd_limits(mean = 2.5, n = 4, scale_min = 1, scale_max = 9, dp = 2),
    c(0.58, 4.00)
  )
  expect_equal(
    determine_sd_limits(mean = 10.50, n = 5, scale_min = 1, scale_max = 9, dp = 2),
    c(0.55, 4.47) ## todo this one fails
  )
})


test_that("increment and decrement helpers work", {
  vec <- c(1, 1, 1, 3, 4, 5, 8)
  expect_equal(
    select_elements_to_decrement(vec = vec, fixed = c(), delta = 1, scale_min = 1),
    c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(
    select_elements_to_increment(vec = vec, fixed = c(), delta = 1, scale_max = 9),
    c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("pick one works",{
  set.seed(12345)
  expect_equal(pick_one_position(c(TRUE, TRUE, FALSE, FALSE)), 2)
})

test_that("sprite_delta only increases the sd from 1.179 to 1.251", {
  set.seed(1234)
  vec = c(1, 2, 3, 4, 5, 2,2,2,2,2)
  m = mean(vec)
  s = 1.251
  result = sprite_delta(
    vec = vec, mean = m, sd = s, scale_min = 1, scale_max = 9, fixed = c()
  )
  # expect_equal(
  #   result,c(1, 4, 3, 4, 3, 6, 2, 2, 2, 2)
  # )
  expect_equivalent(mean(result), m, tolerance=1e-3)
  expect_equivalent(sd(result), s, tolerance=1e-3)
})

test_that("sprite_delta decreases sd",{
  m = 2.5
  s = 1.251
  set.seed(12345)
  expect_equal(
    sprite_delta(
      vec = c(1, 2, 3, 4, 5, 6), mean = m, sd = s, scale_min = 1, scale_max = 9, fixed = c()
    ),
    c(1, 2, 3, 5,5,5)
  )
})


test_that("loop until sd is ok works",{
  vec <- loop_until_sd_is_ok(vec = c(1,1,3,3,2,2,2,6),
                      mean = 2.5, sd = 1.251, scale_min = 1, scale_max = 9,dp=2)
  expect_equivalent(sd(vec), 1.252, tolerance=1e-3)
})

# increment
#change_value_in_vec

context("SPRITE main functions")



test_that("Seek vector works",{
  set.seed(1224)
  mean_here <- 3.533
  sd_here <- 1.561
  n = 45
  vec <- seek_vector(mean = mean_here,sd = sd_here,n =n,scale_min = 1,scale_max = 7,dp = 3)
  expect_equivalent(mean(vec),mean_here, tolerance=1e-3)
  expect_equivalent(sd(vec),sd_here,tolerance=1e-3)
  expect_equal(length(vec), n)
}
)

test_that("get sample creates correct samples",{
  set.seed(1233)
  N <- 10
  tMean <- 3.5
  tSD <- 1.561
  dp <- 2
  max_cases <- 10
  samples <- get_samples(max_cases = max_cases, n=N,mean = tMean, sd = tSD,scale_min = 1,scale_max=7, dp = 2)
  expect_equal(nrow(samples), max_cases)
  expect_equal(ncol(samples), N)
  expect_equal(samples[1,], c(1,3,3,3,3,4,4,4,4,6))
  set.seed(1234)
  # using example from shiny app
  samples_shiny <- get_samples(max_cases = 9, n=45,mean = 3.533, sd = 1.561,scale_min = 1,scale_max=7, dp = 3,fixed = c())
  expect_true(all(round(apply(samples_shiny, 1, mean),3)==3.533))
  expect_true(all(round(apply(samples_shiny, 1, sd),3)==1.561))
})
