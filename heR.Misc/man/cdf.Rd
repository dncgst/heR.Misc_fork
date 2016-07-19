\name{cdf}

\alias{cdf}

\title{Empirical cumulative distribution function}

\description{This function plots an
empirical cumulative distribution function (CDF) and, optionally,
annotates it with arbitrary quantiles and/or percentiles.
}


\usage{
cdf(x, xlim, ylim, main = "Empirical Cumulative Density Function (cdf)",
    xlab = "Sample Values",
    ylab = "Cumulative Probability", type = "l", plot.it = TRUE,
    exclude = FALSE, exval = 0, oneminus = FALSE,
    addq = NULL, addp = NULL, new = TRUE, cex=1,...)
}


\arguments{
\item{x}{a vector of numerical data}
\item{xlim,ylim}{two-element vectors containing axis ranges}
\item{xlab,ylab,main}{Plot axis labels and main title}
\item{type}{Type of plot, e.g., \code{type="l"} for lines}
\item{plot.it}{logical, indicating whether or not to plot the CDF}
\item{exclude}{logical, if true the values in the data equal to \code{exval}
               will be removed}
\item{exval}{a value in the data that will be removed if \code{exclude=TRUE}}
\item{oneminus}{if \code{TRUE}, plot the survival-type function, or
1 - F(x), where F(x) is the CDF.}
\item{addq}{a vector of quantiles to show on the plot}
\item{addp}{a vector of percentiles to show on the plot}
\item{new}{a logical, which if \code{TRUE}, will result in a new plot, otherwise
           points are added to an existing plot}
\item{cex}{the magnification for text and symbols}
\item{...}{additional arguments to pass to \code{plot}}
}


\details{
Plotted quantiles and percentiles are determined by linearly interpolating
the given data.

}


\value{
An invisible list containing:
\item{data}{the sorted data vector \code{x}}
\item{cumprob}{the cumulative probabilities}
\item{newquant}{a two-component list containing the specified quantiles and corresponding calculated percentiles}
\item{newperc}{a two-component list containing the specified percentiles and corresponding calculated quantiles}
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{univar}



