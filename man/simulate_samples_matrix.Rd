% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprite.R
\name{simulate_samples_matrix}
\alias{simulate_samples_matrix}
\title{Get sprite samples}
\usage{
simulate_samples_matrix(
  max_cases,
  n,
  mean,
  sd,
  scale_min,
  scale_max,
  dp = 2,
  fixed = c()
)
}
\arguments{
\item{max_cases}{maximum cases}

\item{n}{total cases}

\item{mean}{mean value}

\item{sd}{standard deviation}

\item{scale_min}{minimum of scale}

\item{scale_max}{maximum of scale}

\item{dp}{degrees of precision}

\item{fixed}{a vector of values that you know are in the sprite sample}
}
\description{
Create samples that match the mean and sd.
}
\seealso{
Other sprite: 
\code{\link{simulate_samples}()},
\code{\link{simulate_vector}()},
\code{\link{sprite_into_df}()}
}
\concept{sprite}
