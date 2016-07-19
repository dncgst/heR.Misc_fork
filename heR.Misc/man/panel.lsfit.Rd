\name{panel.lsfit}
\alias{panel.lsfit}
\title{Lattice Panel Function for Linear Least Squares Fit}
\description{
Produce panels showing results of a regression line fit through data points and print text in each panel giving the fitted parameter values
}
\usage{
panel.lsfit(x, y, intercept = TRUE, pch = plot.symbol$pch, col, col.line = plot.line$col, col.symbol = plot.symbol$col, font = plot.symbol$font, fontfamily = plot.symbol$fontfamily, fontface = plot.symbol$fontface, lty = plot.line$lty, cex = plot.symbol$cex, lwd = plot.line$lwd, horizontal = FALSE, right = FALSE, usr = c(min(x), max(x), min(y), max(y)), text.line = 1/12, cex.text = 1, ...)
}
\arguments{
  \item{x,y}{data vectors for current lattice panel}
  \item{intercept}{whether to include the intercept or not in the fit}
  \item{pch}{the symbol to use for data points}
  \item{col}{the color}
  \item{col.line}{the color of the regression line}
  \item{col.symbol}{the color of the data points}
  \item{font}{the font of in-panel labels}
  \item{fontfamily}{the font family of in-panel labels}
  \item{fontface}{the font face of in-panel labels}
  \item{lty}{type of line to draw fro regression line}
  \item{cex}{the size of the data points}
  \item{lwd}{tbe width of the regression line}
  \item{horizontal}{not used}
  \item{right}{not used}
  \item{usr}{user coordinates to use}
  \item{text.line}{the size the line of text}
  \item{cex.text}{the size of the text}
  \item{\dots}{other parameters}
}
\details{
This function is intended to be used with the lattic xyplot function as a
replacedment of the standard panel.xplot function.   This function adds
a regression line to every panel of plotted data (optionally forcing the
 line through the origin) and prints text in each panel giving the
details of the fit.
}
\author{Neil Klepeis}
\seealso{\code{\link{panel.xyplot}}, {\code{\link{xyplot}},{\code{\link{lattice}}}}}
\keyword{lattice}
