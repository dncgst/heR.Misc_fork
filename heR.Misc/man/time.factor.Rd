\name{time.factor}
\alias{time.factor}
\alias{time.factor.2}
\title{Factor a Time Series into Even Interval Groups}
\description{
Create a factor that can be used to split a time series into constant-length
consecutive intervals (groups)
}
\usage{
time.factor(times, interval, origin = min(times),
            integer.levels=FALSE, tolerance=0.00001,
	    relax.increasing=FALSE)
time.factor.2(times, interval, origin = min(times),
            integer.levels=FALSE, tolerance=0.00001,
	    relax.increasing=FALSE)
}
\arguments{
  \item{times}{a numeric vector of increasing times, e.g., in seconds or minutes}
  \item{interval}{the interval used to define time series groups, must be positive}
  \item{origin}{the time where grouping will start, can be any number within or outside of the given range of \code{times}.}
  \item{integer.levels}{return factor levels as increasing consecutive integers instead of the left limit of each interval, defaults to FALSE}
  \item{tolerance}{tolerance for placing times in the following (right) bin when they are very close to the right hand limit of the current bin.}
  \item{relax.increasing}{whether to check for increasing times or not; the routine can handle both increasing and non-increasing times, but, by default, will inform the user of non-increasing times in case an error has been made.}
}
\details{
This function will create factor levels for fixed-length
consecutive time intervals from times that are either
irregularly or regularly spaced.  The factor can be used to split
a time series and calculate grouped statistics, e.g., means,
typically for the purpose of creating a new regular time series or one
that has time intervals that are consistent with other time series
of interest.

Any origin can be specified, even if it
is outside of the range of \code{times}.   In this case, 
the returned integer levels when \code{integer.levels=TRUE}
may not start or end at 1

The \code{tolerance} establishes a limit for distance from 
the right limit for each bin so that times that are
within the tolerance will be placed in the next
highest bin.  This can be used when 
times are fractions that don't make it to the
next highest bin but we want them to be there anyway.
A tolerance of 0.5*\code{interval} can be used 
to make the returned leftmost bins
the effective midpoint for each interval group.

The \code{time.factor.2} is the preferred version because it makes
use of the built-in R function \code{cut}, whereas \code{time.factor} uses
a home-grown routine.  The older \code{time.factor} will be dropped in the
future.
}

\value{
A factor containing factor levels, either equal to the left-limit of each grouping interval or consisting of integers, corresponding to each consecutive
time series group.  If integer factor levels are used, then negative levels are used below `origin' and positive levels above `origin'.
}
\author{Neil Klepeis}

\seealso{\code{\link{cut.POSIXt}}, \code{\link{cut}}}

\keyword{misc}
