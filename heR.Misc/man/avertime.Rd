\name{avertime}

\alias{avertime}

\title{Time Series Statistics for Increasing Averaging Times}

\description{A function to conveniently calculate raw group averages
or overall statistics from a time series using incrementally increasing
averaging times (i.e., number of grouped observations), returning
the results as a data frame
}


\usage{
avertime(x, n=10, FUN=var, return.raw=FALSE)
}


\arguments{
\item{x}{a time series object}
\item{n}{the maximum number of observations to group together (i.e., average)}
\item{FUN}{the function that produces the final overall statistic for each averaging time, i.e., the statistic applied to the group averages}
\item{return.raw}{whether to return a dataframe containing one row per averaging time, which contains the overall statistic (returned by \code{FUN}) across all groups for each averaging time (the default), or to return a dataframe of the raw average values for each group. }
}


\details{
When \code{return.raw=TRUE}, raw group averages are returned for
each averaging time (i.e., the number of grouped observations).
When \code{return.raw=FALSE}, the function calculates
the statistic across the raw group average values using \code{FUN}, 
as well as the theoretical group variance assuming zero autocorrelation.

Group averages are calculated starting with the
original time series (with a given number of observations per
 unit time, i.e., its frequency) and ending with \code{n} grouped 
observations, i.e., the number of grouped observations ranges from
1 to \code{n}.

This function assumes \code{x} is a regularly-spaced time series.
    
The \code{\link{aggregate}} function, which is used to calculate group
averages for different averaging times, only returns results for complete
groups of a given length (i.e., lengths between 1 and \code{n} elements long)
}


\value{
If \code{return.raw==TRUE}, the function returns a dataframe object
containing columns for the raw group averages
for each averaging time (\code{averages}),
the number of original observations in each group (\code{avertime};
i.e., the averaging
time in units equal to the interval between each original observation),
and the number of group averages in each group (\code{N}).
If \code{return.raw=FALSE}, the default, then each row of the
dataframe contains the same variables, except
the overall statistic (\code{stat}) 
for each averaging time replaces
the raw group averages, and an additional column
contains the theoretical variance (\code{var.N}).
}


\author{Neil E. Klepeis
\cr 
\url{http://klepeis.net} 
}


\keyword{ts}



