\name{data2lnorm}

\alias{data2lnorm}

\title{Convert raw data to a log-normal distribution}

\description{ 
Function to take a data vector and
fit a lognormal model to it, optionally plotting
the fit and the data together as a CDF.
}


\usage{
data2lnorm(y, plot=TRUE, forceNA = FALSE, ...)
}


\arguments{
\item{y}{a vector of numerical data}
\item{plot}{a logical indicating whether to plot the fit or not}
\item{forceNA}{a logical indicating whether to convert data values
less than 0 into \code{NA} values}
\item{...}{additional arguments to \code{plot}}
}


\details{
This function creates a log-probability plot (a kind of cumulative distribution
function or CDF) with the fitted line through the
data points.  It is based on the \code{qqplot} and \code{qnorm} R functions.
}


\value{
A list containing the fitted geometric mean
and the fitted geometric standard deviation,

}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}

\seealso{
\code{\link{qqplot}},
\code{\link{qnorm}},
\code{\link{bin2lnorm}},
\code{\link{lpplot.data}},
\code{\link{lpplot}},
\code{\link{add.lp.bin}},
\code{\link{add.lp.data}},
\code{\link{add.lp.lnorm}}}

\keyword{models}



