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
    loop_until_mean_is_ok(vec = c(1, 2, 3, 4), mean = 2.5, scale_min = 1, scale_max = 8, dp = 2),
    c(1, 2, 3, 4)
  )
  expect_equal(
    loop_until_mean_is_ok(vec = c(1, 1, 2, 4), mean = 2.5, scale_min = 1, scale_max = 8, dp = 2),
    c(1, 1, 4, 4)
  )
})

test_that("extensive testing makes mean work",{
  m = 7.5
  max_ =14
  min_ =1
  for (i in 1:10) {
    vec <- create_init_vec(n=10, m+1,scale_min = min_,scale_max =  max_)
    result <- loop_until_mean_is_ok(vec,m,scale_min = min_,scale_max = max_, dp = 2,fixed = c())
    expect_equivalent(mean(result), m, tolerance=5e-3)
  }

})

test_that("minimal functionality of init vec works",{
  result <- create_init_vec(10, 3, scale_min=1, scale_max=10)
  expect_equal(length(result),10)
  expect_true(all(result <=10))
  expect_true(all(result >=1))
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
    c(0.55, 4.47)
  )
})

test_that("pick one works", {
  set.seed(12345)
  expect_equal(pick_one_position(c(TRUE, TRUE, FALSE, FALSE)), 2)
})

test_that("dont_pick_exact_opposite works", {
  result <- dont_pick_exact_opposite(vec = 1:4, increasable = c(TRUE, TRUE, TRUE, FALSE), 4, 1)
  expect_equal(result, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("not pick works", {
  expect_equal(not_pick(5, 5), c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("determine_positions with increase sd increases sd", {
  set.seed(1234)
  vec <- 1:10
  idx <- determine_positions(increaseSD = TRUE, delta = 1, vec = vec, fixed = c(), scale_min = 1, scale_max = 10)
  increase <- vec
  increase <- increment(increase, idx$incr, 1)
  increase <- increment(increase, idx$decr, -1)
  expect_true(idx$decr != idx$incr)
  expect_equal(mean(increase), mean(vec))
  expect_true(sd(vec) < sd(increase))
})

test_that("determine_positions with decrease sd decreases sd", {
  set.seed(1234)
  vec <- 1:10
  idx <- determine_positions(increaseSD = FALSE, delta = 1, vec = vec, fixed = c(), scale_min = 1, scale_max = 10)
  decrease <- vec
  decrease <- increment(decrease, idx$incr, 1)
  decrease <- increment(decrease, idx$decr, -1)
  expect_true(idx$decr != idx$incr)
  expect_equal(mean(decrease), mean(vec))
  expect_true(sd(vec) > sd(decrease))
})

### constant values for some tests
vec <- c(1, 1, 3, 3, 2, 2, 2, 6)
tMean <- 2.5
tSD <- 1.195
scaleMin <- 1
scaleMax <- 9
dp <- 2
fixed <- c()

test_that("specific determine_positions works just as rsprite delta", {
  result <- determine_positions(increaseSD = FALSE, delta = 1, vec = vec, fixed = fixed, scale_min = scaleMin, scale_max = scaleMax)
  expect_true(result$incr %in% c(1,3,5))
  expect_true(result$decr %in% c(3,5,8))
  result2 <- determine_positions(increaseSD = TRUE, delta = 1, vec = vec, fixed = fixed, scale_min = scaleMin, scale_max = scaleMax)
  expect_true(result2$decr %in% c(3, 5))
  expect_true(result2$incr %in% c(1, 3, 8))
})

test_that("loop until sd is ok works", {
  vec <- loop_until_sd_is_ok(
    vec = vec,
    required_mean = tMean, required_sd = tSD,
    scale_min = scaleMin, scale_max = scaleMax,
    dp = dp
  )
  expect_equivalent(sd(vec), tSD, tolerance = 5e-3)
  expect_equivalent(mean(vec), tMean, tolerance = 5e-3)
})

test_that("loop sd does not change the mean value",{
  m = 8
  s=3.83
  max_ =15
  min_ =1
  dp =2
  fixed <- c()
  for (i in 1:5) {
    vec <- create_init_vec(n=10, m+1,scale_min = min_,scale_max =  max_)
    vec <- loop_until_mean_is_ok(vec,m,scale_min = min_,scale_max = max_, dp = dp,fixed = fixed)
    result <- loop_until_sd_is_ok(vec,m,s,min_,max_,dp = dp,fixed = fixed)
    expect_equivalent(mean(result), m, tolerance=5e-3)
    expect_equivalent(sd(result), s, tolerance=5e-3)
  }
})

test_that("determine_positions deals with weird values", {
  # decrement:
  # not fixed+delta for decreasing # excludes 3
  # larger than scaleMin + delta-1 # excludes 1
  # (if increasesd) vec < max(vec) # excludes 4
  result1 <- determine_positions(TRUE, 1, c(1, 2, 3, 4), fixed = c(2), scale_min = 1, scale_max = 10)
  expect_equal(result1$decr, 2)
  expect_equal(
    determine_positions(TRUE, 1, c(1, 2, 3), fixed = c(2), scale_min = 1, scale_max = 10),
    list(incr = 3, decr = 2)
  )
  # unless no other option  c(1,2,3) /no fixed, # not 1, not 3
  # increment:
  # not fixed-delta   # excl 2
  # unless there is no other option
  # when decreasing sd pick values smaller than decrement TRUE #
  # unless no other option
  # not delta smaller than increment (would negate effect)
  # unless no other option
  result2 <- determine_positions(FALSE, 1, c(1, 2, 3), fixed = c(3), scale_min = 1, scale_max = 3)
  expect_equal(result2$incr, 1)
  expect_true(result2$decr %in% c(2, 3))
  # when not possible return missing
  expect_warning(
    id_impos <- determine_positions(increaseSD = FALSE, delta = 1, vec = c(1, 1, 1),
                                    fixed = c(), scale_min = 1, scale_max = 10),regexp = "could not find"
  )

  expect_equal(id_impos, list(incr = NA, decr = NA))
})


test_that("increment works", {
  expect_equal(increment(c(1, 2, 3, 4), 2, 2), c(1, 4, 3, 4))
  expect_equal(increment(c(1, 2, 3, 4), 4, -2), c(1, 2, 3, 2))
  vec <- vec2 <- 1:5
  whichWillDec =2
  whichWillInc =5
  delta =1
  vec[whichWillDec] <- vec[whichWillDec] - delta
  vec[whichWillInc] <- vec[whichWillInc] + delta
  vec2 <- increment(vec2,whichWillDec, -delta)
  vec2 <- increment(vec2,whichWillInc, delta)
  expect_equal(vec, vec2)
})
