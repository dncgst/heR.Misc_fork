\name{cat.cols}
\alias{cat.cols}
\title{Categorical Colors}
\description{
Return a set of colors that are (hopefully)
appropriate for use in differentiating categories.
}
\usage{
cat.cols(n, set = "set1", plot=FALSE)
}
\arguments{
  \item{n}{the number of colors to return}
  \item{set}{character string giving the name of a palette to use, see below}
 \item{plot}{whether to generate a plot of the colors or not}
}
\details{
This function uses categorical color palettes from the
\code{RColorBrewer} package
for eight and fewer colors with \code{spline.colors} providing a backup
when colors are needed for more categories.

The available sets are: "set1", "set2", "set3", "accent", "dark2", "pastel1",
and "pastel2".
}
\value{
A vector of \code{n} color specifications
}
\author{Neil Klepeis}
\seealso{
\code{\link{spline.colors}}
}
\keyword{misc}
