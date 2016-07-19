\name{collapse}
\alias{collapse}
\title{Provide index to collapse vector or factor to nonrepeating elements}
\description{Return an index that can be used to condense a vector or factor, so that subsequences of repeated values are collapsed to a single value.
}
\usage{
collapse(x)
}
\arguments{
  \item{x}{a vector or factor}
}
\details{The returned index gives the position of the first occurrence of a series of repeated values.
}
\value{
A numeric vector giving the index to use to collapse the input vector or
factor to nonrepeated elements
}
\author{Neil Klepeis}
\examples{
y <- c("A","A","B","B","B","A","A")
collapse(y)
y[collapse(y)]
}
\keyword{manip}
