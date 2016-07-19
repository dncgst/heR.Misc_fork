\name{rose}
\alias{rose}
\title{Plot a Rose Diagram}
\description{
Plot a directional vector histogram (rose diagram), i.e., a histogram of directions resembling the shape of a rose
}
\usage{
rose(x, bins = 36, rscale = NULL, labels = TRUE, rings = TRUE, ...)
}
\arguments{
  \item{x}{a vector of direction values}
  \item{bins}{Approximate number of bins}
  \item{rscale}{Ring Scale, the approximate number of rings for scaling see \code{\link{pretty}}.  NULL value will call \code{pretty} with default number of rings}
  \item{labels}{(T/F) draw labels for the top 10\% largest petals and the cardinal dirctions}
  \item{rings}{(T/F) draw scale rings}  
  \item{\dots}{extra parameters to the \code{polygon} function}
}
\details{
This function is used for plotting directional data such as
wind direction or the
angles of imbricated pebbles in rivers and streams. This is basically
an extension of the \code{\link{hist}} function though I did not
implement all of
\code{hist}. I have placed limits on the range of bins so that they always
fall within 0 and 360 (i.e. directions of the compass). The standard
color and line adjustment commands work as well but you will need to add
annotation (i.e.. \code{main}, \code{xlab}, \code{ylab}) separately
(see \code{\link{par}}).
}
\value{Produces a rose plot
}
\author{David Finlayson (with help from Joerg Maeder and Ben Bolker)}
\seealso{\code{\link{rose2}}, the updated version of \code{rose} by Neil Klepeis}

\examples{
test <- runif(30) * 360
rose(test)
rose(test, bins=10, rscale=2, labels=TRUE, rings=TRUE, col="cyan", lwd=2)
}

\keyword{hplot}
