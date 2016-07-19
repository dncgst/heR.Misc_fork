\name{panel.superbar}
\alias{panel.superbar}
\title{Lattice Panel Function for Grouped Data Barcharts}
\description{
Lattice panel function for plotting barcharts with grouping variables,
where x-y data for grouping factors are plotted as stacked or side-by-side
bars
}
\usage{
panel.superbar(x, y, box.ratio = 10, group.ratio = 1,
               horizontal = TRUE, beside = FALSE, subscripts,
	       groups, col = bar.fill$col, ...)
}
\arguments{
  \item{x}{a vector of numeric data to be plotted against the x axis}
  \item{y}{a vector of numeric data to be plotted against the y axis, of equal length as \code{x}}
  \item{box.ratio}{the ratio of bar width to the space between bars}
  \item{group.ratio}{the ratio of group width (i.e., the width of all the bars in a given group) to the space between groups; only used for side-by-side bar groups, i.e., when \code{beside=TRUE}}
  \item{horizontal}{logical, whether to draw bars horizontally or vertically; if \code{TRUE}, then the 'y' variable will be interpreted as a factor (independent variable), otherwise the 'x' variable will be}
  \item{beside}{logical, whether to draw bar groups side-by-side or stacked}
  \item{subscripts}{indices for the current panel data with respect to the complete, unconditioned data}
  \item{groups}{the vector of groups with respect to the complete, unconditioned data}
  \item{col}{the color(s) to use for filling bars, replicated to match the number of groups present in \code{groups}}
  \item{\dots}{additional unused arguments}
}
\details{
This function is intended to be used with \code{\link{barchart2}}
which calls \code{\link{bwplot}} and deals with data where 
either x or y is to be intepreted as a factor and
plotted at even integer positions instead of at arbitrary numeric
positions (as with \code{\link{xyplot}}).  The factors are passed  to this
function as integers and not as the actual factor level values
(character strings).

If \code{horizontal=TRUE}, then y is interpreted as a factor
(independent variable)
otherwise x is interpreted as a factor.  Stacked bars are plotted at
increasing integer x or y values, whereas side-by-side bars are centered
around each integer value.
Bar heights (dependent variable) are matched to the unique factor values
across all groups, so there will be a blank place instead of a bar
when no dependent variable
value is provided for a given factor value in a given grup.
It is an error if there is a repeated factor in a group.

The rules for negative values are that negative bars are OK for
side-by-side bars, but for stacked bars negative values are coerced to
absolute values.  The rationale for this behavior is that 
it only makes sense to compare positive lengths when stacking data,
but hanging bars (below the zero
line) in side-by-side plots may provide a desired visual effect.
}
\value{
None.
}
\author{Neil Klepeis}
\seealso{The \code{\link{barchart2}} function is the root function
for plotting barcharts across a grouping variable}
\keyword{hplot}
