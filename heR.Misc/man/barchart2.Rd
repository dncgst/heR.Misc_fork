\name{barchart2}
\alias{barchart2}
\title{Plot Grouped Data as a Trellis of Barcharts}
\description{
A replacement for the original bar chart function in the lattice
package, dealing with grouped data by plotting stacked or side-by-side bars
just like the bar plotting function in base R graphics 
}
\usage{
barchart2(formula, data = parent.frame(), panel = "panel.superbar",
          prepanel = "prepanel.superbar", strip = TRUE, box.ratio = 10,
	  groups = NULL, beside = FALSE, horizontal = NULL, subset = TRUE,
	  subscripts = !is.null(groups), ...)
}
\arguments{
  \item{formula}{a formula specification containing the y, x,  and conditioning variables; the x and y variables can be interpreted either as a factor (independent variable) or the dependent variable depending on the value of \code{horizontal} as described below}
  \item{data}{the dataframe containing variables in the formula}
  \item{panel}{a function for plotting the conditioned data in each panel, defatuls to \code{panel.superbar}}
  \item{prepanel}{a function for determining the horizontal and vertical limits of each panel, defaulting to \code{prepanel.superplot}}
  \item{strip}{logical, whether to draw strips or not}
  \item{box.ratio}{the ratio of bar width to the space between bars, defaulting to 10}
#  \item{group.ratio}{the ratio of group width (i.e., the width of all bars making up each group) to the space between groups, defaulting to 1; only used for side-by-side bar groups}
  \item{groups}{specification of the grouping variables}
  \item{beside}{logical, whether to draw bars side-by-side or stacked}
  \item{horizontal}{logical, whether to draw bars horizontally or vertically; make sure to specify an appropriate factor as the independent variable, which will be the 'y' variable for horizontal bars and the 'x' variable for vertical bars}
  \item{subset}{logical vector containing the index of rows in \code{data} that will be plotted}
  \item{subscripts}{subscripts for groups}
  \item{\dots}{additional arguments}
}
\details{
See the \code{\link{xyplot}} documentation for more complete information on 
how to use the lattice graphics functions, including descriptions of most
of the arguments listed above.

The only visible difference in usage from the
original lattice barchart function is the `beside' argument and the
`group.ratio' argument.  Internally,
new panel and prepanel functions (panel.superbar and prepanel.superbar)
are used to plot the bars in each panel and specify the default
x and y limits for each panel.
}
\value{
A \code{trellis} lattice graphics object
}
\author{Neil Klepeis, adapted from the \code{barchart} function included in the lattics package and \code{barplot} in the base R package.}
\seealso{\code{\link{barchart}} is the original lattice function for plotting (univariate) barcharts in each panel.

\code{\link{panel.superbar}} is the panel function used to plot superimposed (group) data as stacked or side-by-side bars.

\code{\link{prepanel.superbar}} is the prepanel function used to specify default horizontal and vertical limits for each panel for either stacked or side-by-side bars.

\code{\link{barplot}} is the original non-lattice function in base R used to plot univariate or grouped data as a bar chart.}
\examples{
test <- data.frame(x=rep(1:4,2),y=1:8, f=gl(2,4))
# vertical side-by-side bar chart
barchart2(y ~ x, groups=f, data=test, beside=TRUE, horizontal=FALSE, col=1:2)
# horizontal stacked bar chart
barchart2(x ~ y, groups=f, data=test, beside=FALSE, horizontal=TRUE, col=1:2)
}
\keyword{hplot}
