\name{read.fwf.mult}
\alias{read.fwf.mult}

\title{Read Files With Multiple Associated Records in Fixed Width Format}

\description{
This function reads data files that have a different fixed-width format (FWF)
for each record in a series of associated contiguous lines.
Each series of grouped records are checked for consistent
length, concatenated, and the variables they contain are parsed
and assigned to a single row in the returned dataframe.
}

\usage{
read.fwf.mult(file, widths, rows, sep="\t", as.is=FALSE,
              skip=0, row.names, col.names, n=-1,
              blank.lines.skip=FALSE,...)
}

\arguments{
  \item{file}{a fixed-width format data file with groups consisting of multiple records (lines)}
  \item{widths}{a list containing integer data widths (i.e., the number of characters) for each variable in each record of a multi-line group; each list component corresponds to a single record (line).}
  \item{rows}{the number of rows or lines (records) per group}
  \item{sep}{the internal separation characters (used in temporary file)}
  \item{as.is}{logical, to convert variables or not (leave alone), see \code{read.table}}
  \item{skip}{number of initial lines to skip, see \code{scan}}
  \item{col.names}{a list with each component containing the names to assign to each variable in each record of a multi-line group; each list component corresponds to a single record (line).}
  \item{row.names}{names for each row in the returned dataframe; each row contains the concatenated data from the original group records}
  \item{n}{the maximum number of rows in the data file, defaults to no limit, see \code{scan}}
  \item{blank.lines.skip}{logical, whether to skip blank lines in the input file or not, see \code{scan}}
  \item{\dots}{other arguments to the \code{read.table} function}
}
\details{
Designed so that one can treat multi-line FWF files as files
with a single large record for each individual subject or \emph{group}.  A group
is a fixed number of contiguous rows (lines) in a data file, which are
somehow associated, e.g., they belong to the same subject.
Each record can have a different length and/or different formatting.

This function was largely stolen from \code{read.fwf} by Brian Ripley.
I only added the concatenation of multiple-records into a single
record, including checks for premature end-of-lines and padding
with blank spaces.

The function reads multiple contiguous records that `belong' to
a single `individual' or `group', or are somehow associated, and that
each have a different format -- e.g., contain information
for different variables.  If each record of your FWF data file 
has the same format (i.e., the same variables), then you can also
just use \code{read.fwf} and split the data on a grouping or index variable.

This function assumes that the *same* number of contiguous 
records exists for each group.

\bold{Important:}  By default, blanks lines are *not* skipped:
missing information is assigned \code{NA} values.  But
by setting \code{blank.lines.skip=TRUE}, blank lines (i.e., the null space
between two adjacent \code{\n} line feeds) will be completely ignored (skipped).
Note that this will lead to an error if only some groups contain
blank lines.

See the \code{\link{read.fwf}} and \code{\link{read.table}}
documentation for more information on function arguments.
}

\value{
Returns a dataframe containing the parsed contents of
the specified data file}

\author{Neil E. Klepeis \cr
\cr}


\keyword{file}
