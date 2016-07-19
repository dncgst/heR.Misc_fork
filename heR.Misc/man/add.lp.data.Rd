\name{add.lp.data}

\alias{add.lp.data}

\title{Add raw data to a log-probability plot}


\description{   
Function to add raw data
to a log-probability plot.
}


\usage{ 
add.lp.data(y, type = "b", include.zeroes=TRUE,
            verbose=TRUE, na.rm=FALSE, ...)
}


\arguments{
\item{y}{vector of numeric data to add to the plot}
\item{type}{type of plot symbol to use}
\item{include.zeroes}{whether to adjust probabilities for zero-valued data or not}
\item{verbose}{whether to produce verbose messages or not}
\item{na.rm}{whether to automatically remove missing values or produce an error message}
\item{...}{additional graphical parameters to \code{points}}
}


\details{This function adds continuous numeric data to a log-probability plot
where the x axis corresponds to  normal probabilities nad the 
y axis corresponds sample quantiles.

Ties in the data, e.g., if there is discretization, 
will produce vertical lines.  Gaps in the data, i.e., intervals
without any data, will produce horizontal lines.

The \code{include.zeroes=TRUE} option allows for adjusting the
probabilities by the number of values less than or
equal to zero. Otherwise, they are ignored, i.e., 
removed completely as if they never existed.
}


\value{
Invoked for the side effect of adding points to a plot.
}


\seealso{\code{\link{lpplot.data}}, \code{\link{lpplot}}, 
\code{\link{add.lp.bin}}, \code{\link{add.lp.lnorm}}
}

\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}

}


\keyword{aplot}



