\name{panel.superpose.2}

\alias{panel.superpose.2}

\title{Trellis panel function to superpose data sets}

\description{ 
This is a version of the \code{panel.superpose}
Trellis panel function that allows the plot \code{type} to change between
superimposed (overlayed) data sets
}


\usage{
panel.superpose.2(x, y, subscripts, groups, col, col.line = superpose.line$col, 
                  col.symbol = superpose.symbol$col, pch = superpose.symbol$pch,
                  cex = superpose.symbol$cex, lty = superpose.line$lty,
                  lwd = superpose.line$lwd, type="p", ...)
}


\arguments{
\item{x}{x data values}
\item{y}{y data values}
\item{subscripts}{subscript specifying data to plot}
\item{groups}{group specification}
\item{col}{colors for plot}
\item{col.line}{line colors}
\item{col.symbol}{symbol colors}
\item{pch}{plotting symbols}
\item{cex}{symbol magnification}
\item{lty}{line types}
\item{lwd}{line widths}
\item{type}{type of plot}
\item{...}{additional parameters}
}


\details{
See the orignal \code{\link{panel.superpose}} documentation for details
on the arguments passed. 

See the \code{panel.xyplot} function
for details on the \code{type} option which is usually a single character,
but here is a character vector with each element specifying the
plot style of each subsequently-overlayed plot.

This function will probably be included in the next \code{lattice}
package release.
}


\value{
A Trellis xyplot with superimposed data sets
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{dplot}




