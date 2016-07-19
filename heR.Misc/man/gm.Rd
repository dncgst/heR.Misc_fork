\name{gm}
\alias{gm}
\title{Calculate the Geometric Mean}
\description{Calculate the geometric mean of a vector of values
}
\usage{
gm(x, na.rm = FALSE, force = FALSE)
}
\arguments{
  \item{x}{Numeric values}
  \item{na.rm}{whether to remove missing values or not}
  \item{force}{whehter to force the removal of values equal to zero or smaller}
}
\details{
Calculate the geometric mean from raw data, by taking in
sequence:  the natural log-transform, the arithmetic mean,
and then exponential

If \code{force==TRUE}, then values of zero or smaller are removed,
and a warning given, otherwise a fatal error is issued.
}
\value{The geometric mean of numeric values in \code{x}
}
\author{Neil Klepeis}
\keyword{misc}
