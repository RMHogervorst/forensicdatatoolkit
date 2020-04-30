test_that("warning function warns", {
  expect_warning(warn_sample_size_gt(n=2, limit =1),regexp = "This test works best with n<1")
  expect_warning(warn_sample_size_gt(n=2, limit =1, test = "GALF"),regexp = "GALF works best with n<1")
  expect_invisible(warn_sample_size_gt(1, 2))
})

test_that("relative integer works",{
  expect_error(relative_integer(2.3))
  expect_equal(relative_integer(2), 2L)
})

test_that("fraction extraction works",{
  expect_equal(extract_fraction(2.3),.3)
  expect_equal(extract_fraction(2.0),.0)
})
