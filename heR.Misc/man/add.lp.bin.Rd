\name{add.lp.bin}

\alias{add.lp.bin}

\title{Add binned data to a log-probability plot}


\description{   
Function to add binned data (i.e., a histogram)
to a log-probability plot.
}


\usage{ 
add.lp.bin(h, l, type = "b")
}


\arguments{
\item{h}{frequency that each data value occurs in each class interval, i.e., bin, given by the limits in \code{l}}
\item{l}{the class interval limits}
\item{type}{type of plot symbol to use}
}


\details{The number of bins must be equal to the number of limits
minus 1.  The limits must be strictly increasing.
Like \code{bin2lnorm}, this function omits the last point
of the cumulative distribution.
}


\value{
Invoked for the side effect of adding points to a plot.
}


\seealso{\code{\link{lpplot.data}}, \code{\link{lpplot}}, 
\code{\link{add.lp.data}}, \code{\link{add.lp.lnorm}},
\code{\link{bin2lnorm}}, \code{\link{data2lnorm}}
}

\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}

}


\keyword{aplot}



