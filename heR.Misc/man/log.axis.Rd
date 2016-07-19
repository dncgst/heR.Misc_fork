\name{log.axis}

\alias{log.axis}

\title{Add a log-10 Axis to a Plot}

\description{
Function to add a nice log base-10 axis
to a plot, with major/minor tics and optional gridlines.
See \code{log.tics} for details on generating tic positions.
}


\usage{
log.axis(side, limits, exp.format=TRUE,
         grid=TRUE, grid.lty="solid", grid.col="lightgray", grid.lwd=1,
         major=TRUE, minor=TRUE, label.minor=FALSE,
         minor.cex=0.75, major.cex=1, ...)
}

\arguments{
\item{side}{Side of plot where axis will be added: 1,2,3,4}
\item{limits}{Numeric limits over which tics will be added}
\item{exp.format}{whether or not labels should be in exponential format}
\item{grid}{logical, draw gridlines or not}
\item{grid.lty}{line type for gridlines}
\item{grid.col}{color for gridlines}
\item{grid.lwd}{line width for gridlines}
\item{major}{logical, draw major tics or not}
\item{minor}{logical, draw minor tics or not}
\item{label.minor}{logical, include minor tic labels or not}
\item{minor.cex}{magnification for minor tic labels}
\item{major.cex}{magnification for major tic labels}
\item{...}{More arguments to pass to the \code{axis} function}
}

\details{You can add log-axis tics and labels to the
 1. Bottom, 2. Left, 3. Top, or 4. Right side of the plot.

This function will add tics/labels in units of 10$^i$ as major
tics, where i = ..., -2, -1, 0, 1, 2, ..., e.g., 0.1, 1, 10

Minor tics will be added as integer multiples of the major tics, e.g.,
as 0.2, 0.3, 0.4, ..., 0.9, for tics betwen 0.1 and 1
}

\value{Invoked for the effect of adding tics and tic labels 
to an existing plot.}

\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}


\keyword{aplot}



