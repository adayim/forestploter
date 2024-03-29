% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-arrow.R
\name{make_arrow}
\alias{make_arrow}
\title{Make arrow}
\usage{
make_arrow(x0 = 1, arrow_lab, arrow_gp, col_width, xlim, x_trans = "none")
}
\arguments{
\item{x0}{Position of vertical line for 0 or 1.}

\item{arrow_lab}{Labels for the arrows, string vector of length two (left and
right). The theme of arrow will inherit from the x-axis. This should be a list
if different arrow labels for each column is desired.}

\item{arrow_gp}{Graphical parameters for arrow.}

\item{col_width}{Width of the column arrow to be fitted.}

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
\description{
Make arrow
}
\keyword{internal}
