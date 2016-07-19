\name{fit.ts.empirical}

\alias{fit.ts.empirical}

\title{Empirically fit a time series}

\description{ 
Function to fit the cyclical parts of
a time series using empirical patterns in the data, e.g., mean values
by time-of-day or day-of-week.
}


\usage{
fit.ts.empirical(x, f, init.step=0, par=NULL,
                 plot=TRUE, plot.res=FALSE, maxits=100)
}


\arguments{
\item{x}{the observed time series to be modeled}
\item{f}{a matrix or data frame containing \code{n} cyclical factor
specifications}
\item{init.step}{the initial "shift" parameter, i.e., the parameter that 
shifts the whole parameter sum up or down; used only if \code{par} is not specified}
\item{par}{if specified, is a vector of the initial parameters
    (e.g., shift parameter and factored means).  NOT CHECKED FOR VALIDITY!}
\item{plot}{whether to plot the fit or not}
\item{plot.res}{whether to plot residuals or not}
\item{maxits}{the maximum number of iterations to perform}
}


\details{The model is simply the sum of the parameters across
each of the cyclical parts of the time series plus a constant shift parameter.
If \code{par} is not specified, then the means across the factors
in \code{f} are used as the initial parameters.

The function currently
uses \code{nlm} to fit the model using non-linear minimization.  I realize
this can be done with a \emph{linear} minimization routine, and will make the change someday soon ....
}


\value{
A list containing a data frame with the original data \code{x}, the residuals 
for the fit, and the model data, and the results of the minimization, see
\code{nlm}:
the return success code, the minimum value, the number of required iterations,
and the best estimate of the parameters.
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{ts}


