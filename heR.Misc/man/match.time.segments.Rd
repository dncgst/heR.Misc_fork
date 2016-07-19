\name{match.time.segments}
\alias{match.time.segments}
\title{Calculate factor indicating time segements}
\description{
  Take a vector of times and limits for time segments and create a factor that indicates the time segments
}
\usage{
time.segments(times, segment.limits, labels, format)
}
\arguments{
  \item{times}{a vector of POSIXct times, or character representation of times}
  \item{segment.limits}{a vector of POSIXct time limits for the segments, or a character represention of times}
  \item{labels}{optional, labels corresponding to the time segments, must be 1 element smaller than the time segment vector.  Given integer values by default.}
  \item{format}{optional, giving the format of the character represention of times and time segments}
}
\value{
  Returns a factor containing levels given in `labels' showing the specified time segments
}
\author{Neil Klepeis}
\keyword{manip}
