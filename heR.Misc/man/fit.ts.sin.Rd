\name{fit.ts.sin}

\alias{fit.ts.sin}

\title{Fit a time series with a sum of sines}

\description{This function fits any number of cycles in a times
series using a sum of sine functions
}


\usage{
fit.ts.sin(x, plot=TRUE, plot.res=FALSE, maxits=200,
           wave=c(24, 24, 24*7, 24*7*365),
           phase=c(rep(0,length(wave))),
           amp=c(0,rep(1,length(wave))))
}


\arguments{
\item{x}{a time series to be fitted}
\item{plot}{logical, whether to plot the fit results or not}
\item{plot.res}{logical, whether to plot the residuals or not}
\item{maxits}{the maximum number of iterations}
\item{wave}{the wavelengths of the sine waves}
\item{phase}{the phases of the sine waves}
\item{amp}{the amplitudes of the sine waves}
}


\details{
This function fits a sum of sinusoids to
a time series with terms for each different cyclical
pattern, e.g., diurnal, weekly, yearly, etc.

The residuals are expected to be a simple autoregressive
time series that can be fit with the R function \code{ar}.

Alternative:  Use the R function \code{stl}, but which only seems to 
examine a single cycle (defined by the time series' \code{frequency}).
}


\value{
Returns a list containing the original time series, the residual values for the
best fit of the model to the observed time series, the number of iterations
for the fit, and the final fitted model parameters: \code{amp, phase, wave}
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{ts}



