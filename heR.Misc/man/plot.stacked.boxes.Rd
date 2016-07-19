\name{plot.stacked.boxes}

\alias{plot.stacked.boxes}

\title{Plot min, median, max as stacked boxes}

\description{A type of box plot to represent
uncertainty in a variate
value as stacked boxes, where the horizontal extent
corresponds to the key variate range.}


\usage{
plot.stacked.boxes(x, m, ...)
}


\arguments{
\item{x}{vector containing x-values for the boxes}
\item{m}{matrix contain y-values in its columns}
\item{...}{additional graphical parameters}
}


\details{
Plot the columns in a matrix m as y-axis values for stacked boxes,
sorting each row of data from lowest to highest.
x-axis values in x.   Matrix has one less row than length of x.
}


\value{
Called for the side-effect of adding boxes to an existing plot.
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{aplot}



