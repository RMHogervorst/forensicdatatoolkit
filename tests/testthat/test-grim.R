test_that("grim works", {
  expect_true(grim_multi_item(mean = 19.20, n = 5, items = 2))
  expect_true(grim_multi_item(mean = 19.25, n = 4, items = 3))
  expect_false(grim_multi_item(mean = 0.80, n = 4, items = 2))
  expect_true(grim_multi_item(mean = 0.80, n = 5, items = 2))
  expect_false(grim_multi_item(20.95, 1, 12))
  expect_false(grim_multi_item(.25, n = 2))
  expect_false(grim(20.95, n = 12))
  expect_false(grim(.25, n = 2))
  expect_true(grim(18.25, n = 4))
  expect_warning(grim_multi_item(20.45, n = 101, items = 1), "GRIM works best with n<100")
})


test_that("Grim limit works", {
  expect_equal(grim_limit_n(decimals = 2, G = 1), 100)
  expect_equal(grim_limit_n(decimals = 3, G = 1), 1000)
})
