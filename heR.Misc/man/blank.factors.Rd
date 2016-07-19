\name{blank.factors}
\alias{blank.factors}
\title{Blank Nested Factors Under Top Levels}
\description{
  Sets factors to blank values under the top levels in a nested
  pattern
}
\usage{
blank.factors(x)
}
\arguments{
  \item{x}{a dataframe containing factors}
}
\details{
Function to create an easy-to-read version of factor labels,
by blanking labels in each group of nested labels except
the leading one, where \code{x} is a dataframe of factor labels.
We assume that the factors have already been sorted into the
desired nested arrangement.
}
\value{a dataframe of factors with all but the top nested level
assigned blank values
}
\author{Neil Klepeis}

\keyword{manip}
