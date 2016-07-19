\name{arrayplot}

\alias{arrayplot}

\title{Plot a multi-dimensional array}

\description{Function to plot the values
of an array against selected values (i.e., indices) of an
independent variable for selected values of a third (key)
variable and selected values of the remaining
(conditioning) variables.
}


\usage{
arrayplot(a, i, k,
          r, ival, kval, sel.i, sel.k,
          r.integ=NULL, integ=FALSE, FUN=sum,
          col, lty, lwd, pch, axes=TRUE,
          legend=FALSE, man.leg=FALSE, ncol=1, idx.leg=TRUE,
          main.leg=paste("Key Variable, k=",k,sep=""),
          xlim=NULL, ylim=NULL, type="l", log="",
          pretty=TRUE, grid=FALSE,
          main=NULL, xlab=NULL, ylab=NULL, new=TRUE,
          dup.top=FALSE, persp=FALSE, theta=55, phi=27, p.col=7,
          p.box=TRUE, expand=0.5, shade=0.50, border=NA,
          ltheta=-135, lphi=0, level=FALSE, image=FALSE, nlevels=25)
}


\arguments{
\item{a}{is a multi-way array}
\item{i}{is the dimension index of the independent variable}
\item{ival}{is a vector containing all the actual values of \code{i}}
\item{k}{is the dimension index of the key variable}
\item{kval}{is a vector containing all the actual values of \code{k}}
\item{sel.k}{is a vector giving the indices of \code{k} that will
          be plotted}
\item{sel.i}{is a vector of the indices of \code{i} that will
         be plotted}
\item{r}{is a vector containing the indices for the remaining
      variables in \code{a}. Note: By default they are set to 1}
\item{r.integ}{is a list giving the indices for each of the remaining
            variables over which \code{a} will be integrated, iff
            \code{integ=TRUE}. If duplicate indices are given
            they will be used to weight the corresponding
            array values by the number of duplicates.  If \code{integ=FALSE},
            then the plots will be conditioned on the
            single values for the remaining variables given in \code{r}}
\item{integ}{logical, whether to integrate the remaining variables or not}
\item{FUN}{is the function applied to the selected
        remaining variable values.  Default is \code{sum}}
\item{col}{gives the colors that correspond to each index
        of the key variable \code{k} in \code{sel.k}}
\item{lty, lwd, pch}{plotting line style, width, and
                      symbol style \cr
   Note: If too few values are given for colors and plotting
         symbols, they are recycled}
\item{axes}{logical, whether to plot axes or not}
\item{legend}{logical, whether to include a legend or not}
\item{man.leg}{logical, manual specify legend position or not}
\item{ncol}{number of columns in the legend}
\item{idx.leg}{logical, whether to include indices in the legend or not}
\item{main.leg}{main title for the legend}
\item{xlim,ylim}{x-axis and y-axis limits}
\item{type}{type of plotting symbol, recycled}
\item{log}{log axis specification, e.g., "x" or "y" or "xy"}
\item{pretty}{logical, pretty tics for axes or not}
\item{grid}{logical, draw gridlines or not}
\item{main, ylab, xlab}{the titles and axis labels
                             for the plot}
\item{new}{If \code{new=TRUE} then a \code{plot.new()} function call is issued
              before drawing the graph}
\item{dup.top}{logical, if \code{TRUE} the top array value
             for the \code{i} indices is duplicated. The function
             then expects an additional value for \code{ival} too,
             if the largest \code{sel.i} index is the maximum index.
             If the largest \code{sel.i} index is not the maximum
             possible, then the \code{ival} is that corresponding to the
             next highest index.  A warning is issued if \code{ival}
             is smaller than the number of \code{i} indices plus 1,
             and an \code{NA} is introduced if there is no ival
             available.  Note: This \code{dup.top} feature is used
             to include the absolute maximum value on aerosol
             size distribution plots.}
\item{persp}{If \code{new=TRUE}, then draw a perspective (3D) plot (instead of a
            2D plot with multiple series for the \code{k} variable
            values)}
\item{theta}{azimuthal angle for perspective plot}
\item{phi}{colatitude angle for perspective plot}
\item{p.col}{color of perspective plot}
\item{p.box}{logical, draw a box for perspective plot or not}
\item{expand}{expansion factor for perspective plot}
\item{shade}{shade value for perspective plot}
\item{border}{border color for perspective plot}
\item{ltheta}{light azimuthal angle for perspective plot}
\item{lphi}{light colatitude angle for perspective plot}
\item{image}{If \code{image= TRUE}, then draw a "3D" R image plot (or "level" plot) with the array values designated by differently
            colored blocks}
\item{nlevels}{number of levels for image plot}
\item{level}{If \code{level=TRUE}, then draw a level plot, which is similar
to the \code{image} plot except that contours are drawn instead of blocks and a 
key is provided next to the plot.}

}


\details{
If \code{integ = TRUE}, then the array values are *integrated*
over the remaining variables' indices given in the list
\code{r.integ} instead of conditioned on the *single* indices
for each of the remaining variables given in \code{r}.  Instead of
integration (summing), another function \code{FUN} can be specified.

If \code{persp = TRUE}, then a perspective (3D) plot will
be produced, or if \code{image = TRUE} then a 3D R image
plot will be produced, or if \code{level=TRUE} then a level
plot and key will be drawn, otherwise a 2D figure
is produced that has series corresponding to every index
in array dimension \code{k}.

The array values are plotted in "index order".

\emph{Note}. This function allows for the convenient exploration of 
a multi-way array without requiring any conversion.
Instead of using this function to visualize multi-variate data, it might be
better to use the powerful Trellis graphics, which are available in the
\code{lattice} R package.   Trellis graphics require conversion of the
array to a dataframe object.  See \code{a2df}.
}


\value{Invoked for the side effect of producing a plot.

}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\seealso{\code{\link{a2df}}, \code{\link{Lattice}}}

\keyword{hplot}



