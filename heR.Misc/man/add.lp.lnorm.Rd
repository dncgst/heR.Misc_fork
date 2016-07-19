\name{add.lp.lnorm}

\alias{add.lp.lnorm}

\title{Add log-normal distribution to a log-probability plot}


\description{   
Function to add a log-normal distribution 
to a log-probability plot.
}


\usage{ 
add.lp.lnorm(gm=1, gsd=exp(1), lty=1, lwd=1, col="black")
}


\arguments{
\item{gm}{the geometric mean for the distribution}
\item{gsd}{the geometric standard deviation for the distribution}
\item{lty}{the line type to use}
\item{lwd}{the line width to use}
\item{col}{the color to use}
}


\details{A log-normal distribution takes the form of a straight
line on a log-probability plot.
}


\value{
Invoked for the side effect of adding a line to a plot.
}


\seealso{\code{\link{lpplot.data}}, \code{\link{lpplot}}, 
\code{\link{add.lp.bin}}, \code{\link{add.lp.data}}
}

\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}

}


\keyword{aplot}



