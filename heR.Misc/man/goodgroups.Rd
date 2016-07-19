\name{goodgroups}
\alias{goodgroups}
\title{Identify Contiguous Groups of Non-Missing Data}
\description{
Return a factor that can be used to split
a vector into groups of continguous non-NA values.
}
\usage{
goodgroups(x)
}
\arguments{
  \item{x}{a vector of data}
}
\details{
Returns NA if all passed values are NA
}
\value{
A factor designating groups that have contiguous non-missing
values
}
\seealso{This function is used by the \code{\link{areaplot}} function to
fill in the area under a curve when some data are missing.}
\author{Neil Klepeis}

\keyword{misc}
