\name{lo}

\alias{lo}

\title{Create graphics layouts}

\description{This function creates square or rectangular
lay-outs for a number of equal-size plots.
}

\usage{
lo(n, mat=TRUE, land=TRUE, byrow=TRUE, respect=TRUE, show=FALSE) 
}

\arguments{
\item{n}{the number of plots to layout}
\item{land}{logical, whether to orient the plots in landscape or not, i.e.,
in portrait}
\item{mat}{logical, if \code{TRUE}, a layout for a matrix of plots is
generated.  Otherwise a single row or column is generated.}
\item{byrow}{logical, when \code{mat=FALSE}, the function will create
a single column layout if \code{byrow=FALSE}, a single row if \code{byrow=TRUE}}
\item{respect}{logical or matrix, whether to make unit width equal to unit height or not, see documentation for \code{\link{layout}}}
\item{show}{logical, whether to show, with numbers, the order of the plot
spaces}
}


\details{
This function sets up the screen to fit the number of
plots specified using the R \code{layout} function.

All plot spaces are of equal size.

The layout is square if possible. If there is not an
even square root of the number of plots, then the layout
is rectangular.  In this case, the layout can be either
landscape (\code{land=TRUE}) where there are more plots horizontally
than vertically, or portrait (\code{land=FALSE}) where there
are more plots vertically than horizontally.
}


\value{
Invoked for the side effect of setting up the graphics device
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}

\seealso{\code{\link{layout}}, \code{\link{split.screen}}}


\keyword{hplot}



