\name{prec}
\alias{prec}
\title{Find the Highest Precision Digit}
\description{Find the hightest precision digit from
a vector of numbers.
}
\usage{
prec(x)
}
\arguments{
  \item{x}{A vector of numbers}
}
\details{This function takes a vector of numbers and returns the
highest precision digit as a number that is a power of 10.  For example, the
highest precision digit of 1.234 is 0.001, which is \eqn{10^{-3}}{10^{-3}}.
}
\value{
A single value giving the highest precision digit as a power of 10.
}
\examples{
prec(1.234)
}
\author{Neil Klepeis}
\keyword{misc}
