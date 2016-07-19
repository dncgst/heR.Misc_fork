\name{striptext}
\alias{striptext}
\title{Annotate a Plot With a Strip of Labels}
\description{
Plot a strip of labels on a plot at a single Y
value and specified X values, with optional arrows, border,
and fill colors
}
\usage{
striptext(labels, Y = 0, X = seq(along = labels), x = NULL, y = NULL,
          as.colors = FALSE, rotated = FALSE,
	  vfont = c("sans serif", "plain"), col = "black", cex = 1,
	  border = TRUE, to.edge = TRUE, border.col = "black",
	  border.lwd = 2, border.offset = c(0, 0), fill = NA,
	  arrow.lwd = 1, arrow.angle = 30, arrow.length = 0.1,
	  arrow.col = "black", ...)
}
\arguments{
  \item{labels}{a character vector containing text to plot as a strip}
  \item{Y}{the Y value where the strip of text will be placed}
  \item{X}{a vector of X values where each character in \code{labels} will be drawn}
  \item{x}{optional vector giving X values for the endpoints of arrows, set equal to \code{X} by default}
  \item{y}{optional vector of Y values for the endpoints of arrows, if not specified then no arrows will be drawn}
  \item{as.colors}{logical, whether to draw colored boxes instead of text, if \code{TRUE} then \code{labels} are interpreted as color names}
  \item{rotated}{logical, whether to draw the text rotated 90 degrees or not}
  \item{vfont}{the vector font specification, defaulting to plain sans serif}
  \item{col}{the color(s) of the text characters}
  \item{cex}{the magnification factor for text}
  \item{border}{logical, whether to draw an optionally filled border around the text or not}
  \item{to.edge}{logical, whether to draw the border flush with the edge of the plotting area or not}
  \item{border.col}{the color of the border}
  \item{border.lwd}{the line width of the border}
  \item{border.offset}{a two-element vector giving x and y offsets for the border in addition to the existing 1/2 character offset}
  \item{fill}{optional color to use when filling the border area, defaulting to no color (unfilled)}
  \item{arrow.lwd}{the line width for arrows, defaulting to 1}
  \item{arrow.angle}{the angle of the arrowhead lines, defaulting to 30 degrees}
  \item{arrow.length}{the length of the arrowheads defaulting to 0.1}
  \item{arrow.col}{the color of arrows}
  \item{\dots}{additional arguments to the \code{\link{text}} function}
}
\details{
In the future, features for automatically plotting stacked strips on a plot or for plotting a legend may be added.
} 
\value{
Invoked for adding a strip of text, and possibly arrows, to an existing plot
for the purpose of annotation.
}
\author{Neil Klepeis}
\keyword{hplot}
