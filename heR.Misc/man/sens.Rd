\name{sens}

\alias{sens}

\title{Sensitivitiy Coefficients}

\description{ 
Function to calculate sensitivity coefficients from an array
consisting of low, medium, and high function values corresponding
to an arbitrary number of parameters
}


\usage{
sens(a, p, plot=FALSE, perspective=FALSE)
}


\arguments{
\item{a}{array of function values, with dimension equal to 3 for
     each level (corresponding to each parameter)}
\item{p}{matrix of low, medium, and high values in columns
    corresponding to each parameter}
\item{plot}{if \code{TRUE}, then a matrix of plots is produced of
 \code{f(p[,i])} vs. \code{p[,i]} for each parameter}
\item{perspective}{logical, whether to use perspective plots or 2D
xy-plots}
}


\details{
Current coeff: normalized change in optimization surface with a
normalized change in parameter, given all other parameters are held
constant at their central value. [normalization is by the central value]
}


\value{
A list of mean absolute and complete (positive and negative) sensitivity
coefficients
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{math}





