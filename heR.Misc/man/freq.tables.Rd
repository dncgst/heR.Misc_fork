\name{freq.tables}
\alias{freq.tables}
\title{Convenient Flat Frequency Tables}
\description{
Return flat frequency tables in a convenient dataframe format
}
\usage{
freq.tables(x, tablespecs, latex = FALSE, file = "")
}
\arguments{
  \item{x}{a dataframe containing raw numeric data}
  \item{tablespecs}{a \code{tablespecs} object with data and table specifications, see below.}
  \item{latex}{logical, whether to write a LaTeX file or not}
  \item{file}{the file to which LaTeX-format data will be written, if empty then the table will be written to standard output}
}
\details{
For each response variable in \code{tablespecs}, one table is generated across
all key variables.  The \code{tablespecs} object
contains all variable, label, etc. specifications for the
table.   These include the following:

The variables are cut into intervals with the \code{\link{cut}} function,
with \code{right=TRUE/FALSE} depending on the \code{tablespecs} value
and according to the breaks given the \code{response.cuts} or \code{key.cuts}
values. A value of \code{NA} means the variable will not be cut.  All
variables are converted to factors, whether or not they are cut.
Labels for binary variables (i.e., those having only 0's and 1's) are set
to \code{TRUE} and \code{FALSE} values.
Important Note:  The user must check to make sure the specified cuts
cover the range of variable values of interest, because values not within the
specified breaks will be assigned NA values during the factoring process.

The frequency table is returned as a probability table if \code{prob} is \code{TRUE}.

If \code{latex=TRUE}, then the tables are printed in LaTeX format
although the \code{ftable} dataframe object(s) is(are) also returned.  If
\code{file != ""}, then the LaTeX output will be written to the named file
with one table appended after another.

This function calls \code{\link{ftable}} and converts the output format
to a dataframe format for easy storage and conversion to LaTeX or HTML, e.g.,
using the \code{\link{xtable}} function.
}
\value{a dataframe containing a flattened frequency table and associated factors}
\author{Neil Klepeis}
\keyword{misc}
