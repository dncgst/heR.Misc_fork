\name{spline.colors}
\alias{spline.colors}
\title{Colors Defined by a Spline}
\description{
Return colors selected evenly along a spline curve fit
to arbitrary values of hue and saturation
}
\usage{
spline.colors(n, h = c(0, 0.3, 0.6),
              s = c(1, 0.3, 1), v = 1,
	      gamma = 1)
}
\arguments{
  \item{n}{the number of evenly-spaced colors to return}
  \item{h}{a vector of ``hue'' values, between 0 and 1}
  \item{s}{a vector of ``saturation'' values, between 0 and 1, length must be equal to that of \code{h}}
  \item{v}{the ``value'' specificationm, defaulting to 1}
  \item{gamma}{the ``''gamma specification, defaulting to 1}
}
\details{
This function picks \code{n} evenly-spaced colors (in the "hue" direction)
along a spline curve fit through the passed values
of hue (h) and saturation (s).  "Value" and "gamma" are fixed
at the passed values (\code{v} and \code{gamma}).

By default, the returned colors begin with shades of red and
end with shades of blue.
}
\value{
A vector of color specifications
}
\author{Neil Klepeis}
\keyword{misc}
