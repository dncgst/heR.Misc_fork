\name{a2df}

\alias{a2df}

\title{Convert an array to a data frame}


\description{   
A function to convert a multi-way array
to a data frame with one variable for each dimension
of the array. 
}


\usage{a2df(a, names=c(paste("Var.",1:length(dim(a)),sep=""),"Response"),
          sel=list(), values=list())}


\arguments{
\item{a}{a multi-way array}
\item{names}{a character vector of names for the variables
           in each dimension}
\item{sel}{selected indices for each dimension (variable)}
\item{values}{a list containing vectors of actual values for
            each variable (dimension) of the array.}
}

\details{
The values in \code{values} are substituted
for the indices if provided, otherwise a series of
integer factors make up the data for each variable.
It should contain the values for ALL the indices, whether
they are selected or not.}


\value{A data frame object
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{manip}


