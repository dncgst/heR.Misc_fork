\name{lpplot}

\alias{lpplot}

\title{Create an empty log-probability plot}

\description{This function creates an empty log-probability
plot with nice tics and, optionally, gridlines
}


\usage{
lpplot(xlim, ylim, grid=TRUE, grid.lty="solid", grid.col="lightgray",
       main = "Log-Probability Plot",
       xlab = "Standard Normal Cumulative Probability (\%)", 
       ylab = "Sample Quantiles",
       bty="o", axes = TRUE, cex.main=1, cex.lab=1, cex.axis=1, las=1,
       tck.minor=-0.009, tck.major=-0.017,
       xtic.minor=FALSE, ...)
}


\arguments{
\item{ylim}{a two-element vector giving the y-axis limits, defaulting to c(1,1000)}
\item{grid}{logical, whether to draw grid lines or not, defaulting to \code{TRUE}}
\item{grid.lty}{line type to use for grid lines, defaulting to "dotted"}
\item{main,xlab,ylab}{The plot title and axis labels, defaulting to appropriate values}
\item{cex.main,cex.lab,cex.axis}{magnification factors for the main title, axis label, and axis tic labels, respectively}
\item{las}{specification for the orientation (style) of axis tic labels, can have values of 1:4, defaults to horizontal labels for both axes, \code{las=1}}
\item{tck.minor, tck.major}{specifications for the length (and direction) of major and minor axis tic marks in units of a fraction of the plotting region; negative values make the tics point away from the plot (i.e., the typical direction)}
\item{xtic.minor}{logical, whether the tic marks for the x-axis will be the length of minor tics or not, defaults to \code{FALSE}, meaning that they will be the length of major tics.}
\item{...}{additional graphics parameters for the \code{axis} function}
}


\details{
The log-probability plot is a type of Q-Q (quantile-quantile) plot commonly
used to judge how data conform to a specific model or data vector.
The y-axis is a log-scale, so that sample points appear as if they were log-transformed.  They are plotted against corresponding normal quantiles on the x-axis.

If the curve appears as a straight line on this plot, then
the data are lognormally distributed, or, in other words, that the log of the data are normally distributed.

The x-axis, corresponding to normal quantiles, is labeled with cumulative normal probabilities, and the y-axis is labeled with untransformed data (sample) quantiles. 

Use the functions \code{add.lp.data},
\code{add.lp.bin}, and \code{add.lp.lnorm} to add points or lines to 
a log-probablity plot produced by this function.

Use the \code{\link{qqnorm}} function for a normal quantile-quantile plot.

See the \code{\link{axis}} function for explanations of graphical parameters associated with axes.

}


\value{
Invoked for the side effect of creating an empty log-probability plot
}


\author{Neil E. Klepeis
}

\seealso{\code{\link{lpplot.data}}, \code{\link{add.lp.data}},
\code{\link{add.lp.bin}}, \code{\link{add.lp.lnorm}}, \code{\link{qqnorm}},
\code{\link{axis}}}

\keyword{hplot}


