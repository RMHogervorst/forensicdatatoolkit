#DEBIT

#' Check sd of binary variables
#'
#' Binary variables can only take on 1 or 0 and that makes the
#' mean and standard deviation linked to each other.
#' Test is also called DEBIT [citation]
#' @param mean reported mean value
#' @param sd reported sd value you want to check
#' @param n reported sample size
#' @param dp degrees of precision, rounding digits applied. dp=3 rounds 0.0035 to 0.004.
#' @export
check_sd_binary <- function(mean, sd,n, dp=3){
  m<- mean
  exp_sd <- sqrt(n/(n-1)*m*(1-m))
  round(exp_sd, dp) == round(sd, dp)
}

#http://www.isaacpub.org/images/PaperPDF/PRA_100019_2019120516352740821.pdf Our
#objective  is  to  take  some  hypothetical  binary  variable  data  and  show
#how  merely  visualizing  the  data could reveal inconsistencies that might
#suggest serious problems, even fabrication of data.
