\name{gsd}
\alias{gsd}
\title{Calculate Geometric Standard Deviation}
\description{
Calculate the geometric standard devations of a vector of values
}
\usage{
gsd(x, na.rm = FALSE, force = FALSE)
}
\arguments{
  \item{x}{a vector of numeric values}
  \item{na.rm}{whether to remove missing values or not}
  \item{force}{whether to force removal of values equal to zero or smaller}
}
\details{
Calculate the geometric standard deviation of raw data
by taking in sequence the natural logarithm, the arithmetic
standard deviation, and the exponential.

If \code{force==TRUE}, then values of zero or smaller are removed,
and a warning given, otherwise a fatal error is issued.
}
\value{The geometric standard deviation of the values in \code{x}
}
\author{Neil Klepeis}
\keyword{misc}
