\name{plot.monitors}
\alias{plot.monitors}
\title{
Plot real-time monitoring data
}
\description{
Plot real-time monitoring data like that produced by the \code{process.monitors} function. Optionally overlays time-activity data as shaded regions.
}
\usage{
plot.monitors(x, data = alldata, groups = Response, codes = NULL,
	tact.var = NULL, show = TRUE, name = "timeSeries", make.pdf = TRUE,
	make.svg = FALSE, make.bitmap = FALSE, type.bitmap = "png16m", 
	units = "in", make.postscript = FALSE, make.metafile=FALSE,
	onefile = TRUE, width = 11,
	height = 8.5, tz = "", interval = "30 min", format = "\%H:\%M",
	tod=TRUE,
	log = TRUE,	ylab = "Concentration", xlab = "Time of Day",
	main = NULL, type = "l", pch = 16, lty = "solid", lwd = 2,
	layout = NULL, cex = 1, col = "black",
	col.border = "gray70", area = FALSE, origin.area = 0, fill.area = "gray50",
	alpha.area = 0.6, as.table = TRUE, relation.x = "same", relation.y = "free",
	outerStrips = FALSE, rot.x = 0, rot.y = 0, clean = FALSE,
	strips.bg = trellis.par.get("strip.background")$col,
	strips.left.bg = trellis.par.get("strip.background")$col,
	strips = strip.custom(bg = strips.bg),
	strips.left = strip.custom(horizontal = FALSE,
	bg = strips.left.bg), strip.lines = 1, strip.left.lines = 1, ...)
}
\arguments{
  \item{x}{
Formula specifying the X, Y, and conditioning variables to use in plotting the data. See \code{xyplot}.
}
  \item{data}{
The data frame containing all variables used in plotting
}
  \item{show}{
Whether to show the plot in an R window
}
  \item{name}{
The name used for graphics files (without any extension).  The proper extension is added automatically.
}
  \item{make.pdf}{
Whether to create a PDF file of the plots
}
  \item{make.svg}{
Whether to create an SVG file of the plots
}
  \item{make.bitmap}{
Whether to create a bitmap (e.g., PNG) file of the plots
}
  \item{type.bitmap}{
The type of bitmap file to produce, defaults to "png16m"
}
  \item{units}{
The units for the height and width of bitmapped files, e.g., "in", "px"
}
  \item{make.postscript}{
Whether to create a Postscript file of the plots
}
  \item{onefile}{
Whether to place all pages of plots in a single file or create one file per page
}
  \item{width}{
The width of the plotting page, defaults to 11 inches
}
  \item{height}{
The height of the plotting page, defaults to 8.5 inches
}
  \item{tz}{
The time zone
}
  \item{interval}{
The time interval used in plotting date, defaults to "30 min"
}
  \item{format}{
The format for creating time labels, defaults to "%H:%M", which gives, e.g., "22:30"
}
  \item{tod}{
Whether to create a special time-of-day axis using \code{format} and \code{interval} above or plot natively, e.g., elapsed minutes or default time format
}
  \item{groups}{
Expression that evaluates to a grouping variable for plotting different series, see \code{xyplot}
}
  \item{codes}{
Vector giving codes to use in plotting time-activity data
}
  \item{tact.var}{
The variable(s) containing time-activity information (optional)
}
  \item{log}{
Whether to log transform plot Y (response) values, defaults to FALSE, possible values are TRUE (base 10), "e", or a number giving the base of the logarithm transformation.
This can also be accomplished by taking the log in the formula \code{x}
}
  \item{ylab}{
A label for the Y axis
}
  \item{xlab}{
A label for the X axis
}
  \item{main}{
The main title of the plots
}
  \item{type}{
Type of plotting style, e.g., lines ('l'), points ('p'), both ('b'), see \code{par}
}
  \item{pch}{
The plotting symbol to use, see \code{par}
}
  \item{lty}{
The line type to use, see \code{par}
}
  \item{lwd}{
The line width to use, see \code{par}
}
  \item{layout}{
The panel layout to use, see \code{xyplot}
}
  \item{cex}{
The magnification level of the plotting symbols
}
  \item{col}{
The color to use in plotting the response data
}
  \item{col.border}{
The color of the border for time-activity data
}
  \item{area}{
Whether to plot the response use an area-under-the-curve style, requires \code{latticeExtra} package
}
  \item{origin.area}{
The origin to use in area plotting
}
  \item{fill.area}{
The fill color to use in area plotting
}
  \item{alpha.area}{
The alpha value to use in area plotting
}
  \item{as.table}{
Whether to arrange panels in a tabular pattern, see \code{xyplot}
}
  \item{relation.x}{
How to draw X scales in panels, e.g., "free" for each to have its own free scale, or "same" for each
panel to have the same scale, see \code{xyplot}
}
  \item{relation.y}{
How to draw Y scales in panels, e.g., "free" for each to have its own free scale, or "same" for each
panel to have the same scale, see \code{xyplot}
}
  \item{outerStrips}{
Whether to draw strips on the outside of the plot, requires \code{latticeExtra} package
}
  \item{rot.x}{
Degrees to rotate the X axis labels
}
  \item{rot.y}{
Degrees to rotate the Y axis labels
}
  \item{clean}{
Whether to remove records that have NA values for any of the variables
}
  \item{strips.bg}{
The background color to use in drawing main strips
}
  \item{strips.left.bg}{
The background color to use in drawing strips on left of plot
}
  \item{strips}{
Function to specify style of main strips
}
  \item{strips.left}{
Function to specify style of strips on left of plot
}
  \item{strip.lines}{
The number of text lines in the main strip
}
  \item{strip.left.lines}{
The number of text lines in the strips on left of plot
}
  \item{\dots}{
Extra arguments to the underlying \code{xyplot} plotting function
}
}
\details{
This function was especially designed to plot the long-format data generated by the
\code{process.monitors} function, but it may be used to plot any time-series data.
}
\value{
This function is called for its side effect of generating plots of real-time data
}
\author{
Neil Klepeis
}
\seealso{
Also see \code{\link{xytact}}, \code{\link{xyplot}}
}
\keyword{plots}
