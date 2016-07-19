\name{prob.axis}
\alias{prob.axis}
\title{Probability Axis}
\description{Add a normal probability axis to an existing plot
}
\usage{
prob.axis(side, at = c(1e-04, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5,
                       0.75, 0.9, 0.95, 0.99, 0.999, 0.9999),
          labels = c(0.01, 0.1, 1, 5, 10, 25, 50, 75, 90, 95,
	               99, 99.9, 99.99), 
	  grid = TRUE, grid.lty = "solid", grid.col = "lightgray", ...)
}
\arguments{
  \item{side}{The side of the plot where the axis will be added; 1, 2, 3, 4 for
  bottom, left, top, right}
  \item{at}{the probabilities where the tics and grids will be drawn}
  \item{labels}{lables for each probability, defaulting to percentages}
  \item{grid}{whether to draw gridlines or not}
  \item{grid.lty}{the line type for gridlines}
  \item{grid.col}{the color of gridlines}
  \item{\dots}{extra graphical parameters to the \code{axis} function}
}
\details{This function adds a normal probability axis to an exisiting plot using
the existing \code{axis} R function.
By default, the labels are increasing cumulative probabilities
in percentage units.  The user may change the probabilites and corresponding
labels.   
}
\value{Adds a normal probability axis to an existing plot
}
\author{Neil Klepeis}
\keyword{aplot}
