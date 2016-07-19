\name{runmean}
\alias{runmean}
\alias{runmedian}
\alias{runsd}
\alias{runstderr}
\alias{run.ci}
\title{Calculate Running Statistics}
\description{
Functions to calculate running statistics for a data series
}
\usage{
runmean(x, ...)
runmedian(x, ...)
runsd(x, ...)
runstderr(x, ...)
run.ci(x, b = 0.9, e = 0.1, ...)
}
\arguments{
  \item{x}{a vector of numeric data values}
  \item{b}{a single value giving the confidence interval bound (0,1), defaulting to 0.9, i.e., 90\% confidence.}
  \item{e}{a single value giving the desired relative error (0,1), defaulting to 0.10, i.e., a relative error of 10\%.}
  \item{\dots}{extra parameters passed to the \code{mean}, \code{median}, and/or \code{sd} functions, which are used by the functions described here.}
}
\details{
A "running mean" or other running statistic is defined as the ongoing calculation of a statistic using progressively more of the available data values, starting with a single value (the first value), continuing with more data values in the order that they are supplied, and ending with all of the values.

\code{runmean} : Calculate the running mean of a vector, starting with just the
first value and ending with all the values.  Returns a vector
of means equal in length to the original vector.

\code{runmedian} : Calculate the running median of a vector, starting with just the first value and ending with all the values.  Returns a vector
of medians equal in length to the original vector.

\code{runsd} : Calculate the running standard deviation of a vector, starting with just the first value (NA) and ending with all the values.  Returns a vector
of standard deviations equal in length to the original vector.

\code{runstderr} : Calculate the running standard error (sample standard deviaton of the mean) starting with just the first value (NA)  and ending with all the values. Returns a vector of standard errors equal in length to the original vector.

\code{run.ci} : Calculate the running confidence interval half-length of the sample mean starting with just the first value (NA) and ending with all the values.
All returned vectors/matrices have a length/nrows one less than for \code{x}:
Returns a list with four components:
\enumerate{
\item A vector of confidence interval half-lengths
\item A matrix containing columns for \code{X-ci}, \code{X}, and \code{X+ci},
where \code{X} is the sample mean, and \code{ci} is the confidence half length,
\item A vector of the relative errors calculated as the confidence interval
half length divided by the sample mean
\item The element of \code{x} where the relative error drops below the
adjusted \code{e}, which is calculated as \code{e/(1-e)}.
}

See "Simulation Modeling and Analysis" by Law and Kelton, p. 539 for
information on determining the "stability" of simulated quantities, which 
is one possible use of running confidence intervals.
}
\value{See above.}
\author{Neil Klepeis}

\keyword{misc}
