\name{integrate.curve}

\alias{integrate.curve}

\title{Integrate along a curve}

\description{Integrates y-values along a curve}

\usage{integrate.curve(x, y, n=50, plot=FALSE)}

\arguments{
\item{x}{x data values}
\item{y}{y data values}
\item{n}{number of linear segments to use}
\item{plot}{logical, whether to plot result or not}
}

\details{The function first fits a spline to the x-y data and then
 integrates the y-values along the curve in small linear segments
 to estimate.   The sum of the resulting values is returned to obtain
 an overall curve integration.
 The number of segments (bins) to use is given by \code{n}.

 I'm sure what this is for or if it really works....}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}


\keyword{manip}



