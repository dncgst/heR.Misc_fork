\name{whist}

\alias{whist}

\title{Weighted Histogram}

\description{ 
Function adapted from VR's \code{truehist} function in MASS library
to calculate a weighted histogram
}


\usage{
whist(data, w = NULL, nbins = NULL, h, x0 = -h/1000, plot=TRUE, 
      axes=TRUE, breaks, prob = TRUE, xlim = range(breaks), ymax = max(est),
      col = 1, xlab = deparse(substitute(data)), bty = "n", main = NULL,
      verbose=T,  ...)
}


\arguments{
\item{data}{raw data from which to calculate the histogram}
\item{w}{vector of weights with same length as the data vector,
if \code{NULL}, then the usual histogram is produced}
\item{nbins}{number of bins}
\item{h}{bin width, takes precedence over \code{nbins}}
\item{x0}{shift for the bins}
\item{plot}{logical, whether to produce the plot or not}
\item{axes}{logical, whether to include the axes or not}
\item{breaks}{actual break points, takes precendence over \code{nbins}}
\item{prob}{logical, whether to plot probabilities or frequencies}
\item{xlim}{two-element vector giving the x axis limits}
\item{ymax}{the upper limit for the y axis}
\item{col}{the color to use for histogram rectangles}
\item{xlab}{the label for the x axis}
\item{bty}{the box type for the histogram plot}
\item{main}{the title of the plot}
\item{verbose}{logical, if \code{TRUE}, then output the number of bins
and data size after \code{NA} removal}
\item{...}{additional graphical parameters}
}


\details{
Deletes all records that have missing values in either 
\code{data} or \code{w}.

Due to an issue with \code{factor}, this won't work
if there are zero counts in any given class interval.

This function is based on \code{truehist} by Brian Ripley
in the \code{MASS} library.
}


\value{
A list of histogram breaks, probabilities or weighted probabilities and
sum of weights if weights are specified, normalized probabilities or
weighted normalized probabilities, and the raw counts
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\seealso{\code{\link{hist}},
\code{\link{truehist}}}

\keyword{hplot}




