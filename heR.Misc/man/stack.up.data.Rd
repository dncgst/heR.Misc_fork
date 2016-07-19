\name{stack.up.data}

\alias{stack.up.data}

\title{Convert columns into rows}

\description{
Convert columns of data in a data frame or matrix
into rows of stacked data with a factor variable
identifying each of the original data variables.}


\usage{
stack.up.data(z)
}

\arguments{
\item{z}{data frame to stack up}
}

\details{
The first column is taken as the independent variable
and is repeated for each stacked set of data.

See the \code{stack} and \code{reshape} functions in the
R \code{base} package for more sophisticated versions of this
function.
}

\value{
Returns a data frame containing the stacked data with one column for
the independent variable and one column for the dependent
variables.  A 3rd column contains the identifying
factor corresponding to the original dependent variable columns.}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}

\seealso{\code{\link{reshape}}, \code{\link{stack}}}

\keyword{manip}



