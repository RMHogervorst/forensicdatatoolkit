#' Aggregate sprite results into data.frame
#'
#' Convenience function for sprite so you can easily work
#' with the results in further work.
#' Sprite returns a matrix with a result per row.
#' This is computational efficient but not easy to work with.
sprite_into_df <- function(sprite_matrix) {
  len_sprite <- nrow(sprite_matrix)
  n <- ncol(sprite_matrix)
  vec <- rep(seq_len(len_sprite), n)
  holding_frame <- data.frame(
    result_id = vec,
    answer = vec
  )
  for (i in seq_len(len_sprite)) {
    answers <- as.vector(sprite_matrix[i, ])
    start <- (i - 1) * n + 1
    end <- i * n
    holding_frame$answer[start:end] <- answers
  }
  holding_frame
}
