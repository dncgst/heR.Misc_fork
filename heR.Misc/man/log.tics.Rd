\name{log.tics}

\alias{log.tics}

\title{Calculate nice tics for log axes}

\description{Create nice log base-10 axis tics from data or data limits.
 Data values less than 0 are removed.
 Can set axis tics to exact multiples of
 ten, e.g., 0.1, 1, 10, if \code{exact10},
 or nearest 10$^i$ increment, e.g., 0.2 or 0.3 (for i = -1), otherwise.
}


\usage{log.tics(x, exact10=TRUE)}

\arguments{
\item{x}{a vector of values, or limits, over which to create tics}
\item{exact10}{logical, whether to expand the range to nearest major tic
e.g., 0.1, 1, 10, or 100, or to the nearest minor tic, e.g., 0.2, 3, or 50}
}

\value{
A list containing the original limits, the calculated tic limits, 
and the major and minor tic values.
}


\author{Neil E. Klepeis
\cr
\cr
\cr
\url{http://klepeis.net}
}


\keyword{dplot}



