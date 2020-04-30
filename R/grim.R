#' Granularity-Related Inconsistency of Means (GRIM)
#'
#' This technique uses a very simple test to see if means are mathematically
#' possible  with
#' a given sample size.
#'
#' @details
#' Technique first described by Nicholas J. L. Brown & James A. J. Heathers.
#' @source \url{https://osf.io/t47wf/}
#' The default options provided work with most cases
#' One item, 2 digits precision and integer scale (granularity =1)
#' So a mean of 15.25 with sample size (n) 4 is plausible
#' @param mean mean to check
#' @param n   number of observations
#' @param items how many observations
#' @param digits digits precision
#' @param G   granularity
#' @export
#' @examples
#' grim(mean = 20.54, n=24)
grim_multi_item <- function(mean,n, items=1, digits = 2, G=1){
  warn_sample_size_gt(
    n,
    limit = grim_limit_n(decimals = digits, G=G),
    test="GRIM")

  mean_guess = round(n * mean * items)/items/n

  mean_guess == mean
}

#' @describeIn grim_multi_item
grim <- function(mean, n){ #.25 , 2
  fraction <- extract_fraction(mean)
  smallest_step <- 1/n
  result = round(fraction / smallest_step,2)
  result == as.integer(result)
}

granularity_of_means <- function(n){
  1/n
}


#' Grim limit
#'
#' If the mean is reported to D decimal places
#' then the test can detect inconsistent means.
#' granularity / N > 10e-D
#' In other words, using only integers (G=1)
#' and 2 decimals, what is the limit where
#' grim is no longer able to find issues?
#' 100, so up to and including 99 it works fine.
grim_limit_n <- function(decimals, G =1){
  G/10^-decimals
}



# grimmer <- function(sd, n, ){
#   # 1. Determine how many decimals (D) the SD is reported to
#   # 2. Identify lower and upper bounds by(SD±0.5/10D)^2
#   # 3. Floor and ceil these values just to be safe
#   # 4. Enumerate the possible variances between these values 5. Convert to SD and round to D decimals
#   # 6. Check if any match the reported SD
#   # 7. If mean was provided perform GRIM test and check if mean matches the variance
# }


