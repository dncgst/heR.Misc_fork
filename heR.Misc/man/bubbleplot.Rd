\name{bubbleplot}

\alias{bubbleplot}

\title{Create a ``bubbleplot''}

\description{Function to plot a trivariate
x-y-z data set with the value of the third, non-negative, z,
variable represented by differently-sized, and/or
-colored, plotting symbols -- so that the plot
resembles \emph{bubbles floating in air}.
}


\usage{
bubbleplot (x, y, z, fact, mult=5,
            area=FALSE,
            inches=FALSE, fg=1, bg=1,
            main=NULL, ylab=NULL, xlab=NULL,
            xlim=c(min(x)-max(z),max(x)+max(z)),
            ylim=c(min(y)-max(z),max(y)+max(z)),
            axes=TRUE, box=TRUE, add=FALSE, ...)
}


\arguments{
\item{x,y,z}{equal-length vectors of numerical data coresponding to each variable}
\item{fact}{normalizing factor to multiply by each z value.}
\item{mult}{multiple of largest z-value circle radius that will fit along x-axis}
\item{area}{logical, whether to map(interpret) the `z' data values to(as) circle area or circle radius.}
\item{inches}{scale z-values to 1-inch maximum if TRUE, or to any other number
of inches if set equal to a positive nmber.  If FALSE, circles are scaled
in x-axis units}
\item{fg}{foreground colors to use in plotting the circles}
\item{bg}{background colors to use in plotting the circles}
\item{main,xlab,ylab}{Plot title and axis labels}
\item{xlim,ylim}{two-element vectors containing the axis limits}
#\item{type}{the type of plot, e.g., \code{type="l"} or \code{type="p"}}
\item{axes}{logical, plot axes or not}
\item{box}{logical, include a box around the plot or not}
\item{add}{logical indicating whether or not to add points to an existing plot}
\item{...}{additional graphical parameters}
}


\details{
\code{x,y,z} must have the same length.  \code{fg} and/or \code{bg}
can be used to give each different point a different color or plotting symbol.
The given value(s) will be recycled over all the points.

Can only specify mult OR scale; scale spec overrides mult spec.
Use `mult' the first time to get circles right, and use the
printed fact value to set `fact' in future calls when a consistent
size is needed between plots with different z-values (and most likely
the same x-axis).

The sizes are assigned from the normalized z-values,
whereas the colors of each z value must be manually-specified by the user.

If \code{add=TRUE}, then the function adds points to an
existing plot.

To scale the *area* of the circles (bubbles) according to the
data `z' values, then set area=`TRUE', otherwise the *radii*
of the circles are scaled as the data `z' values (the default).
Keep in mind that the radii of the drawn circles are always adjusted
by the `fact' argument before being plotted, regardless of whether
the `z' values correspond to circle areas or not.  This is OK since
the relationship between areas is preserved when a constant is
multiplied by each radius, where the "area" constant is the
the square of the "radius" constant (circle area = pi*radius$^2$).

Uses the wonderful `symbol' base R function to
draw circles.   No other symbols besides circles are
available (Did you ever see a square bubble? ....Well maybe....)
Use \code{\link{symbols}} directly if you want more symbols (rectangles, stars, etc.)
}


\value{
The function invisibly returns the adjusted values of \code{z}.
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{hplot}



