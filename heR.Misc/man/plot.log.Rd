\name{plot.log}

\alias{plot.log}

\title{Plot X-Y Data with Nice Log Axes}

\description{Function to plot data with nice log base-10 axes
and gridlines.  Values less than 0 are
converted to the value NA.  Can set axis tics to exact multiples of
ten on the plot, e.g., 0.1, 1, 10, if \code{exact}, or nearest 10$^i$
increment, e.g., 0.2 or 0.3 (for i = -1), otherwise.}


\usage{
plot.log(x, y, topx=NULL, ylim=range(y), xlim=range(x), exact10=TRUE,
         type="s", log="x", col="black", grid=TRUE, rect=FALSE,
         shade.col="tan",  ...)
}

\arguments{
\item{x}{x data values}
\item{y}{y data values}
\item{topx}{absolute maximum value}
\item{ylim}{y-axis limits}
\item{xlim}{x-axis limits}
\item{exact10}{logical, whether plot to nearest multiple of 10 or not}
\item{type}{type of plotting symbol}
\item{log}{log axis specification, e.g., "x" or "y" or "xy"}
\item{col}{color for plotting symbols}
\item{grid}{logical, draw gridlines or not}
\item{rect}{logical, draw rectangles or not}
\item{shade.col}{color to shade rectangles}
\item{...}{additional graphical parameters}
}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}


\keyword{hplot}



