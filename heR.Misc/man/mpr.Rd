\name{mpr}

\alias{mpr}

\title{Create multiple plots containing line fits}

\description{Create multiple plots each containing a least
squares fit -- based on variates in two data frames
}

\usage{
mpr(x, y, ...)
}

\arguments{
\item{x}{a dataframe object containing key, or independent, variate(s)}
\item{y}{a dataframe object containing response, or dependent, variate(s)}
\item{...}{additional graphical parameters for \code{plot}}
}


\details{
Multiple panels of scatter plots are created with least
square linear fits of the \code{y}'s versus the corresponding \code{x}'s.
The variate names are used to label each figure with the
fitting information placed in each plot's main title area.

Trellis plots, which are available in the \code{lattice} R package,
provide better and more functionality for creating panels of plots.
}


\value{
Invoked for the side effect of producing plots
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{hplot}



