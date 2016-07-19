\name{rearrange.table}
\alias{rearrange.table}
\title{Re-arrange a Summary Statistics Table}
\description{
Re-arrange a summary statistics table from "long" to "wide" format with selected factor combinations positioned either vertically or horizontally along the "edges" of the new table, where edges are either leading columns or column names
}
\usage{
rearrange.table(x, stat, hfactors, vfactors, latex=FALSE,
                file="", display="f", digits=1, blank=TRUE)
}
\arguments{
  \item{x}{a dataframe containing factors and one or more data columns (i.e., statistics)}
  \item{stat}{a column in \code{x} (name or index) containing the desired statistic}
  \item{hfactors}{columns (names or indices) in \code{x} containing the factors that will appear horizontally across the returned returned table header (i.e., as the column names of the new dataframe)}
  \item{vfactors}{columns in \code{x} to keep as the vertical headings (i.e., as leading columns of the new dataframe)}
  \item{latex}{whether to generate a LaTeX file or not}
  \item{file}{file containing LaTeX version of table}
  \item{display}{display format for the table entries}
  \item{digits}{number of digits in the table entries}
  \item{blank}{whether to blank repeated levels of factors or not, to improve readability of the table}
}
\details{
This function takes as input a flat summary statistics
table consisting of a dataframe containing columns for
unique factor combinations and columns containing summary statistics.
The user selects a column for a single summary statistic and
one or more columns containing factor combinations to place along either
leading columns or as the names of the returned dataframe.

Important Note:  This function won't work unless there are
complete combinations of the factors.   If any combination of
factors is missing, the function will get confused and choke.
}
\value{
A dataframe of results for a single statistic with leading rows and names containing particular factor combinations.
}
\author{Neil Klepeis}

\keyword{manip}
