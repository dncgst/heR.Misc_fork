\name{plot.fit.ts.sin}

\alias{plot.fit.ts.sin}

\title{Plot a Sine Function Time Series Fit}

\description{
Function to plot a \code{fit.ts.sin} output
object, \code{x}, consisting of the observed data, the model,
the residuals (on a separate plot), and the autocorrelation
function of the residuals (also a separate plot).
}


\usage{
plot.fit.ts.sin(x, xlim=c(start(x$observ)[1],end(x$observ)[1]),
                main="Sinusoidal Fit to an Observed Time Series",
                na.action=na.contiguous)
}


\arguments{
\item{x}{a \code{fit.ts.sin} object}
\item{xlim}{two-element vector containing the limits for the x axis}
\item{main}{title for the plot}
\item{na.action}{method used to treat \code{NA} values}
}


\details{
This function depends on the \code{ts} package.

It's redundant to have \code{fit.sin} output the residuals, since
they can be determined from the observed time series and
the model parameters.
}


\value{
Invoked for the side effect of producing a plot
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\seealso{\code{\link{fit.ts.sin}}, \code{\link{fit.ts.empirical}},
\code{\link{plot.fit.ts.emp}}}

\keyword{hplot}





