% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_stats.R
\name{avg_mean}
\alias{avg_mean}
\title{Calculate the weighted mean value of a vector.}
\usage{
avg_mean(x, weights = c())
}
\arguments{
\item{x}{A vector.}

\item{weights}{An optional vector of weights.}
}
\value{
The weighted mean of \code{x}.
}
\description{
Calculate the weighted mean value of a vector.
}
\examples{
avg_mean(c(1,2,3,4))
avg_mean(c(1,2,3,4), c(0.1, 0.2, 0.3, 0.4))
}
