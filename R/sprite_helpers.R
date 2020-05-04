#' Aggregate sprite results into data.frame
#'
#' Convenience function for sprite so you can easily work
#' with the results in further work.
#' Sprite returns a matrix with a result per row.
#' This is computational efficient but not easy to work with.
#' @family sprite
#' @export
sprite_into_df <- function(sprite_matrix) {
  len_sprite <- nrow(sprite_matrix)
  n <- ncol(sprite_matrix)
  vec <- rep(seq_len(len_sprite), n)
  holding_frame <- data.frame(
    sample_id = vec,
    answer = vec
  )
  for (i in seq_len(len_sprite)) {
    answers <- as.vector(sprite_matrix[i, ])
    start <- (i - 1) * n + 1
    end <- i * n
    holding_frame$answer[start:end] <- answers
  }
  class(holding_frame) <- c(class(holding_frame) ,"sprite_df")
  holding_frame
}

#' Create a generic plot for sprite results
#'
#' This is a convenience function to quickly plot
#' the result of sprite. You can create and customize one yourself by using
#' ggplot(result,aes(answer))+geom_bar()+facet_wrap(~sample_id)
#' @param sprite_df a dataframe that contains samples and answers from sprite.
#' @param ... futher arguments to pass on to ggplot2
#' @export
autoplot.sprite_df <- function(sprite_df, ...){

  data <- sprite_df
  min_data <- min(data$answer)
  max_data <- max(data[["answer"]])
  caption <- paste0("min: ",min_data," max: ",max_data)
  plot<- ggplot(data, aes(.data$answer, group=.data$sample_id))+
    geom_bar()+
    facet_wrap(~sample_id)+
    scale_x_continuous()+
    labs(caption = caption)
  plot
}



# convenience functions to clearly show intent
#' Count unique results
#' @param matrix to count the rows of
#' @return number.
n_unique_rows <- function(matrix) {
  sum(unique_rows(matrix))
}
#' All unique columns
#' combine non zero and not duplicate cols
#' @inheritParams n_unique_rows
#' @return a logical vector indicating unique columns in a matrix
unique_rows <- function(matrix) {
  non_zero_rows(matrix) & not_duplicate_rows(matrix)
}
#' Find non duplicate rows in a matrix
#' @inheritParams n_unique_rows
#' @return logical vector of rows that are not duplicates
not_duplicate_rows <- function(matrix) {
  !duplicated.matrix(matrix, MARGIN = 1)
}
#' Find rows that are not zero
#' Which we began with.
#' @inheritParams n_unique_rows
#' @return logical vector of rows that are not zero
non_zero_rows <- function(matrix) {
  rowSums(matrix) != 0L
}



increment <- function(vec, ind_vec, delta) {
  vec[ind_vec] <- vec[ind_vec] + delta
  vec
}

#' Function that outputs 1 most of the times but <20% of the time 2
step_size <- function() {
  if ((stats::runif(1) < 0.2)) {
    2
  } else {
    1
  }
}


dont_pick_exact_opposite <- function(vec, increasable, decreasing_pick, delta) {
  diff <- decreasing_pick - delta
  result <- increasable & (vec != diff)
  if (sum(result) == 0) {
    result <- increasable
  }
  result
}



#' select a random TRUE position out of logical vector
pick_one_position <- function(logical_vector) {
  if (sum(logical_vector) == 0) stop("There are no TRUE values in vector")
  if (class(logical_vector) != "logical") stop("Not a logical vector")
  pos <- which(logical_vector)
  pos[as.integer(stats::runif(1) * length(pos)) + 1]
}


not_pick <- function(pick, n) {
  truths <- rep(TRUE, n)
  truths[pick] <- FALSE
  truths
}



calc_sd <- function(a, b, total, n, dp) {
  k <- round((total - (n * b)) / (a - b))
  k <- min(max(k, 1), n - 1) # don't get it yet
  vec <- c(rep(a, k), rep(b, n - k))
  round(sd(vec), dp)
}

set_granularity <- function(dp) {
  ((0.1^dp) / 2) + rSprite.dust
}

create_init_vec <- function(n, mean, scale_min,scale_max){
  as.integer(pmax(pmin(as.integer(stats::runif(n) * 2 * mean), scale_max), scale_min))
}



#' Copy from script, slight modifications to keep argument names consistent.
make_mean_grim <- function(mean, n, dp = 2) {
  gMean <- mean
  int <- round(mean * n) # nearest integer; doesn't matter if this rounds up or down
  frac <- int / n
  dif <- abs(mean - frac)
  # allow for rounding errors
  gran <- set_granularity(dp)
  if (dif > gran) {
    gMean <- round(frac, dp)
    warning(paste0("Fails GRIM test for mean ", mean, ", N=", n, " nearest mean is: ", gMean))
  }
  return(gMean)
}

determine_sd_limits <- function(mean, n, scale_min, scale_max, dp = 2) {
  if (scale_min >= scale_max) {
    stop("scale_min cannot be larger than scale_max")
  }
  maxtotal <- round(mean * n)
  aMax <- scale_min # "aMax" means "value of a to produce the max SD"
  aMin <- floor(mean)
  bMax <- max(scale_max, scale_min + 1, aMin + 1)
  bMin <- aMin + 1
  result <- c(
    calc_sd(aMin, bMin, maxtotal, n, dp),
    calc_sd(aMax, bMax, maxtotal, n, dp)
  )
  result
}

