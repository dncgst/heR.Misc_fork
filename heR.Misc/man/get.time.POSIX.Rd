\name{get.time.POSIX}
\alias{get.time.POSIX}
\title{Create Standard Time Variables from Character Strings}
\description{
Function to convert a character vector or factor into a dataframe containing standard POSIX date-time values, elapsed times, and a time factor that can be used for calculating grouped statistics and/or merging time-based data sets.
}
\usage{
get.time.POSIX(date.time, format="\%d/\%m/\%y \%H:\%M:\%S", units= "mins", index = 1, start = NULL, names=c("Time.POSIX", paste("Elapsed", units, sep = "."), "Time.Factor"))
}
\arguments{
  \item{date.time}{character vector or factor containing date-time information}
  \item{format}{the format for the date.time information, see \code{\link{strptime}}}
  \item{units}{character string giving units for elapsed time and time factor intervals, e.g., "mins", "hours"; see  \code{\link{difftime}}}
  \item{index}{an integer specifying the starting index where elapsed time will begin}
  \item{start}{if non-NULL, a date-time string or a POSIXct (or POSIXlt) object giving the starting point for calculating elapsed time.  If a string, it must be in the same format as `date.time'.}
  \item{names}{column names for the returned data frame, defaulting to appropriate descriptive names}
}
\details{
This function returns a data frame containing the POSIX
object of a date-time character or character factor vector,
including the elapsed time in 'units'. 

Elapsed numeric times are calculated starting from the `index', which
is assumed to be the first value in the vector, UNLESS a `start'
is specified in the same format as that of `date.time', in which case we
use that as the reference point for elapsed numeric time units.

The function also returns a time factor showing which records belong to a given
time interval (defined by units), starting at the origin specified by `start' or `index',
}

\value{A dataframe containing POSIX date-time values, elapsed time, and a time factor with distinct values for each `units' interval
}
\author{Neil Klepeis}
\seealso{\code{\link{time.factor}}}
\keyword{misc}
