\name{stat.tables}
\alias{stat.tables}
\title{Tabular Summary Statistics}
\description{
Return a dataframe containing summary statistics
across a nested list of factor combinations
}
\usage{
stat.tables(x, tablespecs, latex = FALSE, file = "")
}
\arguments{
  \item{x}{a dataframe containing raw numeric data}
  \item{tablespecs}{a \code{\link{tablespecs}} object containing table variable and label specfications}
  \item{latex}{logical, whether to write a LaTeX-format table in text format or not, in addition to returning a dataframe object}
  \item{file}{the file to contain the LaTeX-format table, if empty the data is written to standard output}
}
\details{
Uses a dataframe and a
\code{\link{tablespecs}} object as input.  Invalid (missing) values
in each subgroup are removed before statistics are calculated.

We take a dataframe x and a list of equal-length factors corresponding
to each row, and use the \code{\link{aggregate}} function to return a
dataframe containing the statistics.  The \code{\link{xtable}} function is
used to optionally print out the data frame in LaTeX format.
The dataframe object is returned regardless.
}
\value{A dataframe containing factors statistics across each factor combination
}
\author{Neil Klepeis}

\keyword{misc}
