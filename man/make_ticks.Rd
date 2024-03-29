% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{make_ticks}
\alias{make_ticks}
\title{Set x-axis ticks}
\usage{
make_ticks(at = NULL, xlim, refline = 1, x_trans = "none")
}
\arguments{
\item{at}{Numerical vector, create ticks at given values.}

\item{xlim}{Limits for the x axis as a vector of length 2, i.e. c(low, high). It
will take the minimum and maximum of the lower and upper value if not provided.
This will apply to all CI columns if provided, and will be calculated automatically
for each column if not provided. This should be a list with the same length of
\code{ci_column} if different \code{xlim} for different column is desired.}

\item{x_trans}{Change axis scale, Allowed values are one of c("none", "log", "log2",
"log10"). Default is \code{"none"}, no transformation will be applied.
The formatted label will be used for \code{scale  = "log2"} or \code{"log10"}, change
this in \code{x_trans}. Set this to \code{"log"} if x-axis tick marks assume values
are exponential, e.g. for logistic regression (OR), survival estimates (HR), Poisson
regression etc.}
}
\value{
A vector
}
\description{
Create ticks points.
}
\keyword{internal}
