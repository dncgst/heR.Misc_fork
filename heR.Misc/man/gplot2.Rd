\name{gplot2}
\alias{gplot2}
\title{Visualize a Graph}
\description{
A function for visualization of directed or undirected network graphs
}
\usage{
gplot2(dat, g=1, gmode="custom", diag=FALSE, labels=c(1:dim(dat)[2]),
       coord=NULL, jitter=FALSE, thresh=0, label.adj=c(1.5,0.5),
       label.offsets=c(0,0), arrows=1,
       edge.offsets=NULL, circles=0.05,
       mode="mds", lower.sel=1, lower.arc=90, 
       pad=NULL, vertex.pch=20,
       label.cex=1, vertex.cex=1, label.col=1,
       display.edge.labels=TRUE, edge.label.pos=1/6, edge.label.cex=1,
       edge.label.offset=0.03,
       edge.col=1, vertex.col=1,
       arrowhead.angle=10, arrowhead.length=0.2,
       edge.lty=1, edge.lwd=0, 
       circles.fg=1, circles.bg=NA, circles.lwd=1, axes=FALSE,
       ylab=expression(lambda[1]), xlab=expression(lambda[2]),
       main=NULL)
}
\arguments{
  \item{dat}{one or more graph data structures;  a graph data structure is an \code{n x n} square matrix containing directional connection information for \code{n} nodes (\emph{vertices}); by default a number greater than 0 in the ith row and jth column indicates that there is a connection between the ith and jth node (see \code{thresh}) in the i->j direction. In the future, node loops will be drawn based on diagonal values, i.e., where i=j.}
  \item{g}{the graph data structure to select from \code{dat}, defaults to 1}
  \item{gmode}{the graph mode, defaults to 'custom' where arrows and segments can be mixed for different connections;  recognized possibilities are 'graph' where all connections are draw as segments, 'digraph' where all connections are drawn as arrows}
  \item{diag}{logical, whether to draw loops or not, currently unimplemented}
  \item{labels}{vector containing label text for each vertex}
  \item{coord}{optional coordinates for each vertex}
  \item{jitter}{logical, add noise to the coordinates or not}
  \item{thresh}{the threshold over which values in \code{dat} are interpreted as a connection}
  \item{label.adj}{a 2-row matrix containing x and y alignment adjustments  for each label between [0,1], see the \code{\link{text}} function.}
  \item{label.offsets}{a 2-row matrix containing x and y offsets for each label, if any element for either x or y is set to "auto", then the corresponding label offset will be automatically assigned (works best for circular arrangments of the nodes)}
  \item{arrows}{an \code{n x n} square matrix with values > 1 designating connections that will be drawn as arrows; other connections will be draw as non-directional line segments}
  \item{edge.offsets}{a vector of offsets for each vertex designating how far from the center of the vertex each edge will extend; if \code{NULL}, edge offsets will be set equal to the radius of any specified vertex circle}
  \item{circles}{an vector of circle radii for each vertex; if \code{NULL} no circles will be drawn}
  \item{mode}{the method for automatically placing vertices;  one of 'circle', 'lucircle', 'princoord', 'eigen', 'mds', 'random', 'circle', 'circrand', 'rmds', 'geodist', 'adj', or 'seham'; see the \code{gplot} function.}
  \item{lower.sel}{if \code{mode="lucircle"}, then these are the indices for the vertices that will occupy the lower circular arc with the remaining vertices occupying the upper circular arc}
  \item{lower.arc}{if \code{mode="lucircle"}, then this is the angle in degrees corresponding to the lower circular arc;  the upper arc angle will then be \code{360 - lower.arc}}
  \item{pad}{the amount to use in padding the edges of the plot to prevent clipping;  if \code{NULL}, then equal to the maximum radius of any specified circles.}
  \item{vertex.pch}{a vector of symbols to draw at each vertex; a value of -1 for any vertex will suppress drawing a symbol.}
  \item{label.cex}{a vector of magnifications for each vertex label}
  \item{vertex.cex}{a vector of magnifications for each vertex symbol}
  \item{label.col}{a vector of colors for each label}
  \item{display.edge.labels}{whether to show labels for edges or not}
  \item{edge.label.pos}{fraction of way along edge that the labels with be drawn}
  \item{edge.label.cex}{magnification factor for size of edge label text}
  \item{edge.label.offset}{offset distance of edge labels from the edge itself}
  \item{edge.col}{an \code{n x n} square matrix of colors for each edge}
  \item{vertex.col}{a vector of colors for each vertex symbol}
  \item{arrowhead.angle}{the angle of the edge arrow heads, defaults to 10}
  \item{arrowhead.length}{the length of the edge arrow heads, defaults to 0.2}
  \item{edge.lty}{an \code{n x n} square matrix of line types for each edge}
  \item{edge.lwd}{an \code{n x n}  square matrix of line widths for each edge}
  \item{circles.fg}{a vector of foreground colors for each vertex circle}
  \item{circles.bg}{a vector of background (fill) colors for each vertex circle}
  \item{circles.lwd}{a vector of line widths for each vertex circle}
  \item{axes}{logical, whether to draw axes or not}
  \item{ylab}{the y axis label text}
  \item{xlab}{the x axis label text}
  \item{main}{the main title text}
}
\details{
This is a rewrite of the \code{gplot} function by Carter Butts,
\email{ctb@andrew.cmu.edu}, included in the Social Network Analysis
(sna) R package version 0.41.   It is meant to produce a visualiztion
of a network by drawing a graph of nodes and connecting lines and/or
arrows.

This version has many more options including the ability to change the
color, offset, line type, symbol, and magnification of each node and/or edge
and/or label, individually.  An optionally filled circle can be drawn
around each node, which will automatically determine the offsets for
edges and possibly labels. Non-directional segments and/or arrows
can be specified individually for each different edge, i.e.,
there can be arrows for some connections and segments for others.
See the argument descriptions for complete information on each option.
}
\value{
Invoked for producing a graphical visualization of a network.
}
\author{Neil Klepeis, original \code{gplot} function by Carter Butts}

\seealso{\code{\link{gplot}}}

\keyword{hplot}
