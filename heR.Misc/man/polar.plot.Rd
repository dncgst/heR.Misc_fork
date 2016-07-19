\name{polar.plot}
\alias{polar.plot}
\title{Create a Polar Plot}
\description{Plot coupled angular and radial data
}
\usage{
polar.plot(r, theta, theta.zero = 0, theta.clw = FALSE,
           method = 1, rlabel.axis = 0, dir = 8,
	   rlimits = NULL, grid.circle.pos = NULL,
	   grid.lwd = 1, grid.col = "black", points.pch = 20,
	   points.cex = 1, lp.col = "black", lines.lwd = 1,
	   lines.lty = 1, polygon.col = NA, polygon.bottom = TRUE,
	   overlay = NULL, pi2.lab = TRUE, text.lab = NULL,
	   num.lab = NULL, rlabel.method = 1, rlabel.pos = 3,
	   rlabel.cex = 1, rlabel.col = "black", tlabel.offset = 0.1,
	   tlabel.cex = 1.5, tlabel.col = "black", main = NULL,
	   sub = NULL)
}
\arguments{
  \item{r}{vector of radial data}
  \item{theta}{vector of angular data in radians}
  \item{theta.zero}{angle direction on plot of theta=0}
  \item{theta.clw}{have clockwise orientation of theta values or not}
  \item{method}{integer specifying the method of plotting (r,theta)-data: 1. points (default); 2. line; 3. polygon}
 \item{rlabel.axis}{angular direction on the plot of radial label axis (in radians)}
 \item{dir}{number of radial grid lines (default=8)}
 \item{rlimits}{Interval for radial axis as a numeric vector: c(lower,upper). Interval will be extended by the default use of pretty()-function. (default = NULL)}
 \item{grid.circle.pos}{radial axis position of grid circles as numeric vector of minimum length 2. Overrides the default positioning of grid circles by pretty()-function. (default = NULL)}
 \item{grid.lwd}{grid line width}
 \item{grid.col}{grid line color}
 \item{points.pch}{points plotting symbol}
 \item{points.cex}{character expansion factor for points}
 \item{lp.col}{color of points (method 1) or lines (method 2 and method 3) . In method 3, set lp.col=0 for polygons without border}
 \item{lines.lwd}{line width for plotting methods 2 and 3 (default = 1)}
 \item{lines.lty}{line type (default = 1)}
 \item{polygon.col}{color of polygon (default = NA)}
 \item{polygon.bottom}{polygon to the back i.e. behind the grid (default = TRUE)}
 \item{overlay}{NULL (default), no overlay}; 1. overlay data on existing plot; 2. overlay data, grid and labels on existing plot.
 \item{pi2.lab}{angular labels in radians (0, pi/2, pi, 3*pi/2) (default)}
 \item{text.lab}{angular axis labels from a character vector c("N","E","S","W") (default = NULL)}
 \item{num.lab}{numeric angular axis labels in interval [0;num.lab] (default = NULL). Number of labels: dir}
 \item{rlabel.method}{integer specifying method of plotting radial axis labels: 0. no radial labels; 1. labels at pretty radial distances (default); 2. exclude label at radial distace 0; 3. exclude label at maximum radial distance; 4. exclude radial labels at distance 0 and at maximum radial distance}
 \item{rlabel.pos}{text position of radial axis labels (NULL,1,2,3,4)}
 \item{rlabel.cex}{cex for radial axis labels}
 \item{rlabel.col}{color of the radial labels}
 \item{tlabel.offset}{radial offset for angular axis labels in fraction of maximum radial value (default = 0.1)}
 \item{tlabel.cex}{cex for angular axis labels}
 \item{tlabel.col}{angular labels color}
 \item{main}{plot main title}
 \item{sub}{plot sub title}
}
\details{
Function for creating a polar plot from angular and radial data vectors.

Based on original code obtained from R-help by Karsten Dalsgaard Bjerre.

}
\value{Produces a polar plot of coupled angular and radial data
}
\author{Karsten Dalsgaard Bjerre and Neil Klepeis}

\keyword{hplot}
