\name{bin2lnorm}

\alias{bin2lnorm}

\title{Fit binned data to a log-normal distribution}

\description{This function takes binned data and
fits a lognormal model to it, using weighted least squares,
and optionally plotting the fit and the data together 
}


\usage{
bin2lnorm(l, h, wt, plot=FALSE, density=FALSE, mcol="red", dcol="green",
          lty="dashed", lwd=3, shade.density=15, shade.angle=35,
          rect.border.col=NULL, rect.lwd=1, rect.lty=1, cex=1.4, main,
          xlab, ylab, ...)
}

\arguments{
\item{l}{a vector containing the bin limits}
\item{h}{a vector of frequency counts in each bin, i.e., a histogram}
\item{wt}{an optional vector of weights corresponding to each bin}
\item{plot}{a logical indicating whether to plot the result or not}
\item{density}{a logical indicating whether to produce a density or a
log-probability (cumulative) plot, when \code{plot=TRUE}}
\item{mcol}{the color to use in plotting the model}
\item{dcol}{the color to use in plotting the data}
\item{lty}{fit line type}
\item{lwd}{fit line width}
\item{shade.density}{density to shade bars for data in density plot}
\item{shade.angle}{angle for bar shade lines for data in density plot}
\item{rect.border.col}{bar border col for data in density plot}
\item{rect.lwd}{bar line width}
\item{rect.lty}{bar line type for data in density plot}
\item{cex}{the symbol and label magnification factor}
\item{main}{plot title}
\item{xlab}{x-axis label}
\item{ylab}{y-axis label}
\item{...}{additional graphics parameters to pass to \code{plot}}
}


\details{
If \code{density = TRUE}, then the function will plot a histogram with
the fitted lognormal function superimposed, otherwise it plots
a CDF (log-probability) with the fitted line through the
data points.

This is similar to the 'method of quantiles' for
estimating the parameters of a lognormal distributtion,
except that we use all of the data excluding the top point. See
Ott, 1995, page 268, "Environmental Statistics and Data
Analysis".
}


\value{
A list containing \code{h}, \code{l}, \code{log(l)},
the probability density (PDF), the cumulative probabilities (CDF),
the normal quartiles, the geometric mean (GM), and the
geometric standard deviation (GSD).}


\seealso{
\code{\link{data2lnorm}},
\code{\link{lpplot}},
\code{\link{lpplot.data}},
\code{\link{add.lp.data}}, \code{\link{add.lp.bin}},
\code{\link{add.lp.lnorm}}}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{models}



