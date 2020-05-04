#' Granularity checks
#'
#' Most of the functions in this package make use of granularity checks on
#' summary statistics. Given a certain limits of sample size, lower and upper
#' scale and the fact that you can only answer with integers makes it
#' relatively easy to check for inconsistencies.
#'
#' - Check mean given certain n: [check_mean()][forensicdatatoolkit::check_mean()]
#' - Check standard deviation given n and mean for binary data [check_sd_binary()][forensicdatatoolkit::check_sd_binary()]
#' @family granularity
#' @name granularity
NULL
