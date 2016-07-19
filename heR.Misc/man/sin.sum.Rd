\name{sin.sum}

\alias{sin.sum}

\title{Sum of Sines}

\description{ 
Function to calculate a time series based on the
sum of an arbitrary number of sine functions with different
wavelengths, phases, and amplitudes.
}


\usage{
sin.sum(t,wave=c(24, 24, 24*7, 24*7*365),
        phase=c(rep(0,length(wave))),
        amp=c(0,rep(1,length(wave))))
}


\arguments{
\item{t}{vector of increasing times at which to calculate the model}
\item{wave}{vector containing the wavelengths of the sin functions}
\item{phase}{vector containing the initial phases for the sin functions}
\item{amp}{vector containing the initial amplitudes for the sin functions}
}


\details{
Designed to be used with \code{fit.ts.sin}
}


\value{
Returns a vector with length equal to \code{length(t)} that
consists of the sum of sine waves for each given time
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\seealso{\code{\link{fit.ts.sin}}}

\keyword{datagen}





