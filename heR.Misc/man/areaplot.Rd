\name{areaplot}
\alias{areaplot}
\alias{areaplot.segmented}
\title{Draw Area Under a Curve}
\description{
A function to fill in the area under the curve described by
a set of x-y points with the specified color.
}
\usage{
areaplot(x, y, col = "gray", new = FALSE, base=0, density=NULL, angle=NULL,  ...)
areaplot.segmented(x, y, f, col = "gray", new = FALSE, base=0, density=NULL, angle=NULL, axes=FALSE,  ...)
}
\arguments{
  \item{x}{a vector of x values}
  \item{y}{as vector of y values}
  \item{f}{a factor delineating the segments in x and y}
  \item{col}{the color to use for filling in the area under the curve}
  \item{new}{logical, whether to create a new plot or not}
  \item{base}{baseline y value, defaulting to zero}
  \item{density}{density of fill lines}
  \item{angle}{angle of fill lines}
  \item{axes}{logical, whether to draw the axes or not when new=TRUE}
  \item{\dots}{additional arguments to the \code{polygon} function}
}
\details{
This function would normally be used on an existing plot to fill
in the "area under a curve."  It is intended to be used on
linear axes, but might work on logarithmic scales. 

We add points with a baseline of \code{base} y-values (defaulting to zero)
at the beginning and end to ensure that the area
is drawn to the baseline.  The fill is drawn to the
minimum y point or \code{base}, whichever is lower.

The segmented version adds a parameter, f, containing a factor to plot different-colored segments of an areaplot.  In this case, the col, density, and angle parameters are vectors with elements corresponding to successive segments.
}
\value{
Invoked for producing a new plot or adding one or more filled polygons to the
current plot.
}
\author{Neil Klepeis}
\seealso{\code{\link{polygon}}}
\keyword{hplot}
