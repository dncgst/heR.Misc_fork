\name{xyplot.overlay}

\alias{xyplot.overlay}

\title{Overlaid Trellis XYplots}

\description{
Function that creates a Trellis
plot that overlays a series of data frames in a
number of xyplot panels according to the value of
a conditioning variable.
}

\usage{
xyplot.overlay(formula, z, lwd=2, lty=1, col=1:length(z), type="l",
               pch=1:length(z), cex=rep(1,length(z)), strip.style=1,
               strip.bg=trellis.par.get("strip.background")$col,
               strip.fg="black", strip.cex=1,
               panel=function(...) panel.superpose.2(...,lwd=lwd,lty=lty,
                                      col=col,pch=pch,cex=cex),
               strip=function(...,bg) strip.default(...,bg=strip.bg,
                                style=strip.style,strip.names=T),
               as.table=TRUE,...)
}


\arguments{
\item{formula}{a formula that species the form of the
             plot, i.e., the dependent, independent, and
             conditioning variables: \cr
             \code{d ~ i | c} \cr
             \code{d ~ i | c1 * c2} }
\item{z}{list containing the data frame objects. The
       variables in the formula need to be included in
       each data frame to be plotted.  The data frames
       must have matching numbers of columns (variables)}
\item{lwd}{vector of line widths to apply to each consecutive overlaid plot}
\item{lty}{vector of line types to apply to each consecutive overlaid plot}
\item{col}{vector of colors to apply to each consecutive overlaid plot}
\item{type}{vector of plot types to apply to each consecutive overlaid plot}
\item{pch}{vector of plot symbols to apply to each consecutive overlaid plot}
\item{cex}{vector of symbol sizes to apply to each consecutive overlaid plot}
\item{strip.style}{the style of the strip above each panel, see below}
\item{strip.bg}{the background color of the strips}
\item{strip.fg}{the foreground color of the strips}
\item{strip.cex}{the size of the strip text (magnification)}
\item{panel}{a function specifying how panels are created}
\item{strip}{a strip function specifying how strips are created}
\item{as.table}{logical, whether to draw panels from top left, or not}
\item{...}{any number of parameters to be passed to the
       \code{xyplot} Trellis function}
}


\details{
Notes: 

*  To see the variable name/values in the strips, the conditioning
variables should be encoded as a factor.

*  The \code{type} option is a character vector that includes any of the normal
type plot options, or \code{r} for linear regression, or  \code{smooth} for
a loess-smoothed line.

*  If \code{panel.superpose.2} is used as the panel function, then
the plot \code{type} can change between overlayed plots.   Each element of
the character vector \code{type} specifies the subsequently-plotted
plot type.
\cr

This function needs the \code{lattice} R package, which, in turn,
requires the \code{grid} package.
}


\value{
A Trellis xyplot with overlaid data
}

\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}

\seealso{\code{\link{xyplot}}, \code{\link{panel.superpose.2}},
\code{\link{panel.superpose}}, \code{\link{Lattice}}}


\keyword{hplot}



