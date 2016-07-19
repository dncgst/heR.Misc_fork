\name{rcdf}
\alias{rcdf}
\title{Pick Random Numbers from Empirical Data}
\description{
Randomly sample n values from
an empirical cdf based on continuous numeric data with optional
cumulative probabilities
}
\usage{
rcdf(n, data, cumprob)
}
\arguments{
  \item{n}{the nunber of values to simulate}
  \item{data}{the raw data, or right bin limits of aggregated data}
  \item{cumprob}{optional cumulative probabilities associated with binned data values, if not given then they are calculated from \code{data}}
}
\details{
Data can be raw continuous data, and cumprob omitted, to calculate
the cumulative probabilities automatically.  Or data
can be right bin limits, with the passed cumprob holding
the corresponding probabilities.

\code{\link{cdf}}, the cdf plotting function in the \code{heR.Misc}
package, and this function,
currently don't deal with ties when raw data are passed.
}
\value{A vector of simulated values from raw data, possibly binned
}
\author{Neil Klepeis}

\keyword{univar}
