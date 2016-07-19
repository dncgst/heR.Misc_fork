\name{lpplot.data}

\alias{lpplot.data}

\title{Log-probability plot from raw data}


\description{This function creates a log-probability
plot from raw data
}


\usage{
lpplot.data(y, ylim, main = "Log-Probability Plot",
	    xlab = "Standard Normal Probability (\%)",
    	    ylab = "Sample Quantiles", plot.it = TRUE,
	    overlay = FALSE, forceNA = FALSE, ...)
}


\arguments{
\item{y}{raw data to plot}
\item{ylim}{a two-element vector containing the y-axis limits}
\item{main,xlab,ylab}{Plot title and axis labels}
\item{plot.it}{logical, create the plot or not}
\item{overlay}{logical, whether to overlay plot on an existing plot,
               \code{overlay=TRUE}, or create a new one, \code{overlay=FALSE}}
\item{forceNA}{logical, whether to force all data <=0 to be \code{NA} or not}
\item{...}{additional graphical parameters for \code{plot}}
}


\details{
You should probably be using the \code{lpplot} and
\code{add.lp.data} functions instead of this function.  Please
refer to their documentation.
}


\value{
Invoked for the side effect of producing a plot
}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}

\seealso{\code{\link{lpplot}}, \code{\link{add.lp.data}},
\code{\link{add.lp.bin}}, \code{\link{add.lp.lnorm}}}

\keyword{hplot}




