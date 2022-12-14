% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examine.R
\name{examine.numeric}
\alias{examine.numeric}
\title{Numeric examine}
\usage{
\method{examine}{numeric}(
  effect_size,
  standard_error,
  alpha = 0.05,
  df = Inf,
  number_simulations = 10
)
}
\arguments{
\item{effect_size}{a numeric, an estimate of the "true" effect size}

\item{standard_error}{a numeric, standard error of the estimate}

\item{alpha}{a numeric, the statistical significance threshold}

\item{df}{a numeric, the degrees of freedom. df=Inf is equivalent
to a normal distribution.}

\item{number_simulations}{a numeric, how many times to simulate when calculating Type M
error}
}
\value{
A list of length 3 containing the power, type s, and type M
error.
}
\description{
examine.numeric is the S3 method of the generic examine() function,
used when a single numeric is passed for the effect_size.
}
\examples{
examine(1,3.28)
examine(2,8.1)
examine(.5,1,df=10)
}
