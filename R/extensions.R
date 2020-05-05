
#' Check standard deviation given a mean
#'
#' Uses sprite in the background to find a matching vector.
#' If the process cannot find a possible match it will output a FALSE.
#' This usually means the standard deviation is not possible but if it is
#' difficult to find but possible solution try increasing the max_rounds.
#'
#' @param mean required mean value
#' @param sd   required standard deviation
#' @param n    total cases
#' @param scale_min underpoint of scale
#' @param scale_max upper point of scale
#' @family sprite
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#' check_sd(mean=8, sd=3.83, n=10,scale_min=1, scale_max=15)
#' check_sd(mean=2, sd=3.83, n=10,scale_min=1, scale_max=15)
check_sd<- function(mean, sd, n,scale_min, scale_max, dp=2, max_rounds=1000L){
  # TODO: guard against obvious wrong input values
  vec<- suppressWarnings(simulate_vector(
    mean=mean, sd =sd, n =n, scale_min = scale_min, scale_max = scale_max, dp=2, max_rounds=max_rounds))
  all(vec != rep(0,n))
}
