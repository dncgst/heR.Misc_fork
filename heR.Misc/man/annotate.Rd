\name{annotate}
\alias{annotate}
\title{Annotate Plots with Text and Arrows}
\description{
Annotate plots with one or more horizontal
labels, an optional filled rectangular box, and
optional arrows
}
\usage{
annotate(labels, X, Y = NULL, x = NULL, y = NULL,
         adj = c(0, 0), arrow.pos = 1, col = "black",
	 cex = 1, border = TRUE, border.col = "black",
	 border.lwd = 1, fill = NA, offset = c(0, 0),
	 arrow.lwd = 1, arrow.angle = 30,
	 arrow.length = 0.1, arrow.col = "black")
}
\arguments{
  \item{labels}{a character vector of text to add to the plot}
  \item{X}{x coordinates corresponding to the labels}
  \item{Y}{y coordinates corresponding to the labels}
  \item{x}{optional x coordinates for arrowheads corresponding to each label}
  \item{y}{optional y coordinates for arrowheads corresponding to each label}
  \item{adj}{a 2-element vector containing x and y adjustments in the range [0,1] for placement of the labels, defaulting to left and bottom, respectively, all labels with be drawn with this adjustment}
  \item{arrow.pos}{integers in the range 1..4 for positions of the arrow tails corresponding to each label, defaulting to the bottom}
  \item{col}{the colors of the labels}
  \item{cex}{the magnification factors for the labels}
  \item{border}{logical vectir, whether to draw borders or not around the labels}
  \item{border.col}{the colors of the borders}
  \item{border.lwd}{the line width of the borders}
  \item{fill}{the fill colors for the borders, defaulting to no (transparent) fill}
  \item{offset}{the offset of the border from the text in addition to the defajult buffer of 1 character width}
  \item{arrow.lwd}{the line widths of arrows}
  \item{arrow.angle}{the arrowhead angles}
  \item{arrow.length}{the arrowhead lengths}
  \item{arrow.col}{colors for the arrows}
}
\details{
Either of the \code{X} or \code{x} arguments can contain a list
with x and y components, for example by using the \code{locator} function,
in which case the corresponding \code{y}
or \code{Y} arguments are ignored.

This function is a different approach than that taken inthe
\code{striptext} function, which is intended to annotate plots using
single characters placed along a continuous strip extending the
width of the plot..
}
\value{
Invoked for adding annotations to a plot
}
\seealso{\code{\link{striptext}}, \code{\link{locator}}}
\author{Neil Klepeis}
\examples{
plot(sin(1:10),type="l")
cat("\nClick on the plot four times to place labels and arrowheads.\n")
annotate(c("Here is one label.","And another."),X=locator(2),
x=locator(2),cex=c(1.4,3),arrow.lwd=c(2,4),arrow.length=c(0.1,0.25),
border.lwd=c(1,3), fill=c("pink","white"), col=c("blue","green"),
adj=c(0,1),border.col=c("orange","purple"))
}
\keyword{hplot}
