test_that("check_sd works", {
  expect_true(check_sd(mean=8, sd=3.83, n=10,scale_min=1, scale_max=15))
  expect_false(check_sd(mean=2, sd=3.83, n=10,scale_min=1, scale_max=15))
})
