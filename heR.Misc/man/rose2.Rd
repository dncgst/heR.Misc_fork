\name{rose2}
\alias{rose2}
\title{Plot a Rose Diagram of Magnitude and Direction}
\description{
Creates a directional histogram, i.e., a rose diagram, 
representing data on direction, and possibly magnitude
}
\usage{
rose2(dir, mag=NULL, nplumes=16,  shrink=0.6,
      shrink.top=0.55, shrink.bottom=0.7,
      fg="black", bg="linen", border="black", lwd.border=1.9,
      mag.bins=c(0.5,1,2,4,8,16,32,100),
      mag.col=c("springgreen","cyan","blue","magenta",
                "yellow","tomato","tan"),
      fg.lows="black", bg.lows="azure",
      rscale=NULL, rings=TRUE, ring.labels=TRUE, lwd.ring=1,
      lty.ring=2, col.ring="black", lwd.axis=2,
      cex.lab=1.5, cex.dir=3, ...)
}
\arguments{
  \item{dir}{a vector of direction values usually interpreted as the direction that wind, is COMING FROM.}
  \item{mag}{an optional vector of magnitudes corresponding to the specified values of direction}
  \item{nplumes}{the number of plumes, i.e., bins, to use}
  \item{shrink}{an overall shrinkage factor to create space between plumes}
  \item{shrink.top}{a shrinkage factor for the top of plume segments}
  \item{shrink.bottom}{a shrinkage factor for the bottom of plume segments}
  \item{fg}{the foreground color for the plot}
  \item{bg}{the background color for the plot}
  \item{border}{the color for the border of all plume segments}
  \item{lwd.border}{width of line used to draw plume segment borders}
  \item{mag.bins}{the bin boundaries for magnitudes}
  \item{mag.col}{the colors used to fill each plume segment corresponding to each magnitude bin }
  \item{fg.lows}{foreground color used to draw circle for low magnitudes}
  \item{bg.lows}{background color used to draw circle for low magnitudes}
  \item{rscale}{the values to use in drawing the radial scale, defaults to avlues determing using \code{\link{pretty}}}
  \item{rings}{whether to draw rings or not}
  \item{ring.labels}{whether to draw ring labels or not, consisting of percentage values}
  \item{lwd.ring}{the line width used to draw rings}
  \item{lty.ring}{the line type used to draw rings}
  \item{col.ring}{the color used to draw rings}
  \item{lwd.axis}{line width used to draw axis lines}
  \item{cex.lab}{magnification factor for drawing text labels}
  \item{cex.dir}{magnification factor for drawing the direction labels, N, S, E, and W}
  \item{\dots}{extra parameters to pass to \code{polygon}}
}
\details{
This function creates a directional histogram that may also contain information on magnitude.  The most common use of this function is to make a frequenty 
plot of simultaneous wind speed and wind direction.  In this case, the plumes that extend from the origin of the plot indicate the frequency of
wind that is COMING FROM that direction.

There can be any number of `plumes' specified by \code{nplumes},
which are individual histograms centered along specific radial 
directions around a circle.  If a non-NULL value of \code{mag} is
given, then each plume is segmented according to the values
in \code{mag.bins}, with each segment corresponding to a different 
range of magnitudes.

This function is an updated version of David Finlayson's \code{\link{rose}},
adding space between plumes and using segmented
plumes to represent different magnitudes.  There is also a center
area of the diagram corresponding to calm winds below
a specified threshold, which is taken as the lowest value in \code{mag.bins}.

The degrees for direction are interpreted as follows (compass degrees).
For wind data, these angles indicate by convention the direction that wind is
COMING FROM:

\tabular{lcl}{
North \tab = \tab 0 degrees \cr
East \tab = \tab 90 degrees \cr
South \tab = \tab 180 degrees \cr
West \tab = \tab 270 degrees
}
The compass degrees are converted to conventional polar coordinates
prior to plotting.

The shrinkage factors are used to specify the fraction of the full angle
that each plume will actually occupy for the purpose of improving readability
by giving more space in between plumes.  For example, if \code{shrink} is
set to 0.5 and there are 12 plumes, then each plume will
only occupy half of the full angle of 30 degrees, i.e., it will occupy
15 degrees.   The \code{shrink.top} and \code{shrink.bottom} factors
act in a similar fashion, but apply to the angle across the top (outer edge)
and bottom (inner edge) 
of each segment, respectively.  By adjusting these two factors one
can give the plumes the appearance of a flower or telescope.  Note that
while the angles that are spanned at the top and bottom of the segment might
be different (i.e., shrunk), the length of the lines connecting each 
vertice of the segment might be the same or different depending on the
radial distance of the segment being drawn.
}
\value{Produces a rose plot; invisibly returns a list of percents for each magnitude and direction
}
\author{Neil Klepeis}
\seealso{\code{\link{rose}}}
\examples{
dir <- runif(30) * 360
mag <- runif(30) * 50
rose2(dir,mag)
}

\keyword{hplot}
