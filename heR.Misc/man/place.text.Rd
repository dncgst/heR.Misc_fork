\name{place.text}

\alias{place.text}

\title{Place vector of text and expressions}

\description{Function to let users interactively place any number of
  text and expressions on graphs using the \code{locate} and
  \code{text} functions}

\usage{place.text(t, l=NULL, adj=c(0.5,1), ...)}


\arguments{
\item{t}{a vector containing text and/or mathematical expressions}
\item{l}{an optional list of places to position text automatically}
\item{adj}{adjustment of text, see \code{\link{text}}}
\item{...}{arguments to pass to \code{text}}
}


\details{The user clicks on the graphics device window to place
  each specified text string or expression}

\value{Invisibly returns a list of places where text was positioned}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}


\seealso{\code{\link{text}}, \code{\link{locator}}}


\keyword{hplot}



