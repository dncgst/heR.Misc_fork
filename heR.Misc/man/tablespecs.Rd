\name{tablespecs}
\alias{tablespecs}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Table Specifications}
\description{
Create an object containing
specifications for creating a table,
either a flat frequency tabulation or
a table of summary statistics, both calculated across a
number of factor combinations
}
\usage{
tablespecs(...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{\dots}{one or more arguments, see below for details}
}
\details{
We don't check the validity of the responses, keys,
statistical functions, or the LaTeX digit/display specs,
but we check for consistency in their lengths.

The possible arguments are as follows.  The first four are required.

\describe{
\item{responses}{Required vector argument containing indices or character strings for each response (dependent) variable}
\item{keys}{Required vector argument containing indices or character strings for each key (independent) variable}
\item{response.cuts}{Required list of break points for each response variable}
\item{key.cuts}{Required list of break points for each key variable}
\item{key.labels}{A list of character vectors containing labels for each key variable's factor levels}
\item{response.labels}{A list of character vectors containing labels for each response variable's factor levels}
\item{right}{logical, whether to include right break point values when cutting the data, defaults to \code{TRUE}}
\item{include.lowest}{logical, whether to include the lowest break point when cutting the data, defaults to \code{TRUE}}
\item{blank.labels}{logical, whether to blank-out all factors except the first level in each column, for better readability, or not}
\item{prob}{logical, whether to calculate probabilities instead of frequencies, defaults to \code{FALSE}}
\item{stats}{A character vector of functions to use in calculating statistics, defaulting to \code{c("length", "mean", "sd", "max", "min")}}
\item{digits}{A vector giving the number of digits to use in columns for each statistic for use in writing LaTeX tables, defaults to 1 for each statistic except the length (i.e., sample size) which is assigned a 0}
\item{display}{A character vector giving the type of formatting to use for columns of statistics when writing LaTeX tables, see \code{\link{xtable}} for more information on the possible values, defaults to "d" (decimal) for sample size and "e" (scientific notation) for the others}
}

}
\value{
A \code{tablespecs} object containing specifications for constructing frequency and summary statistics tables
}
\author{Neil Klepeis}
\keyword{misc}
