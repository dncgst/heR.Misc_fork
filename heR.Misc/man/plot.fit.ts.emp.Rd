\name{plot.fit.ts.emp}

\alias{plot.fit.ts.emp}

\title{Plot an Empirical Time Series Fit}

\description{ 
Function to plot a \code{fit.ts.empirical}
object, \code{x}, consisting of the observed data, the model,
the residuals (on a separate plot), and the autocorrelation
function of the residuals (also a separate plot).
}


\usage{
plot.fit.ts.emp (x,
                 main="Empirical Fit to an Observed Time Series",
                 na.action=na.contiguous)
}


\arguments{
\item{x}{a \code{fit.ts.empirical} object}
\item{main}{the title of the plot}
\item{na.action}{the method used to remove \code{NA} values}
}


\details{
This functions depends on the \code{ts} package.

It's redundant to have \code{fit.ts.empirical} output the residuals, since
they can be determined from the observed time series and
the model parameters.
}


\value{
Invoked for the side effect of creating a plot
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}

\seealso{\code{\link{fit.ts.empirical}}, \code{\link{fit.ts.sin}}}


\keyword{hplot}



