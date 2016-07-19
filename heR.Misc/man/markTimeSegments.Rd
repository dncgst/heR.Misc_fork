\name{markTimeSegments}
\Rdversion{1.1}
\alias{markTimeSegments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical interface to mark time segments
}
\description{
From a time series of numeric values, manually mark and label time segments on a 
plot of the times series, and return a file with all marked segments.
}
\usage{
markTimeSegments(date, time, values, format = "\%m/\%d/\%Y \%H:\%M:\%S",
	plot.format = "\%m/\%d/\%y \%a \%H:\%M", type = "l", cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
a character vector containing dates
}
  \item{time}{
a character vector containing times
}
  \item{values}{
a character vector containing numeric values
}
  \item{format}{
a character string giving the date-time format for the merged input data in `date' and `time'
}
  \item{plot.format}{
a character string giving the date-time format to display on the horizontal axis
}
  \item{type}{
the type of plot symbol to use
}
  \item{cex}{
the size of the plot symbol
}
  \item{\dots}{
additional graphic parameters to the `plot' function
}
}
\details{
This function uses RGtk to plot a time series and allows the user
to point and click on the graph, marking starting times, ending times, and labels for
any number of time segments.   All of the marked segments can be saved to a
specified CSV file.
}
\value{
Saves a file with time segment information if specified by the user.
}
\author{
Neil Klepeis
}
\keyword{plot}
