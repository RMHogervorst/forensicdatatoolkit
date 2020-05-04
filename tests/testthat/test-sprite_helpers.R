test_that("sprite_into_df unrolls matrix correctly by row", {
  A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
  B <- data.frame(
    sample_id = rep(c(1, 2), 3),
    answer = c(1, 2, 3, 4, 5, 6)
  )
  class(B) <- c(class(B), "sprite_df")
  expect_equal(
    sprite_into_df(A), B
  )
})
