% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary.R
\name{check_sd_binary}
\alias{check_sd_binary}
\title{Check Standard Deviation of binary variables}
\usage{
check_sd_binary(mean, sd, n, dp = 3)
}
\arguments{
\item{mean}{reported mean value}

\item{sd}{reported sd value you want to check}

\item{n}{reported sample size}

\item{dp}{degrees of precision, rounding digits applied. dp=3 rounds 0.0035 to 0.004.}
}
\description{
Binary variables can only take on 1 or 0 and that makes the
mean and standard deviation linked to each other.
Test is also called DEBIT \link{citation}
}
\seealso{
Other granularity: 
\code{\link{check_mean_multi_item}()},
\code{\link{granularity}}
}
\concept{granularity}
