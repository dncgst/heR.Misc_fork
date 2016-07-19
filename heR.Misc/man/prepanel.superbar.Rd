\name{prepanel.superbar}
\alias{prepanel.superbar}
\title{Prepanel Lattice Function for Plotting Grouped Data Barcharts}
\description{
This function is used as an argument in the \code{barchart2} function
}
\usage{
prepanel.superbar(x, y, box.ratio, groups = NULL,
                  subscripts = NULL, horizontal = TRUE,
		  beside = FALSE, ...)
}
\arguments{
  \item{x}{vector of x data}
  \item{y}{vector of y data}
  \item{box.ratio}{ratio of bar widths to space between bars}
  \item{groups}{a vector of groups in complete, unconditioned data}
  \item{subscripts}{a set of indices for current panel data with respect to the complete, unconditioned data}
  \item{horizontal}{logical, whether to plot bars horizontally or vertically}
  \item{beside}{logical, whether to plot bars side-by-side or stacked}
#  \item{levels.fos}{I have no idea what this is(was) for}
  \item{\dots}{additional unused arguments}
}
\details{
For stacked bars, lower limit is always zero (only makes sense for
stacked comparisons of absolute values of passed dependent variables), but
it can be negative for side-by-side bars, since 'hanging' bars can
be illustrative.  Upper limit for stacked bar panels is set to the largest
sum of dependent values across the independent variable factor levels.
}
\value{
  A list containing:
  \item{xlim}{the x limits for the current panel}
  \item{ylim}{the y limits for the current panel}
  \item{dy}{returned as 1}
  \item{dy}{returned as 1}
}
\author{Neil Klepeis}
\seealso{The \code{\link{barchart2}} function is the root
function for plotting barcharts across a grouping variable}
\keyword{hplot}
