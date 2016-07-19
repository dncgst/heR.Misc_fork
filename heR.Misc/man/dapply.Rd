\name{dapply}
\alias{dapply}
\title{Apply a Function to Dataframe Variables}
\description{
Aggregate multiple, possibly interacting, dataframe variables
by applying a function to groups
defined by a set of equal-length factors.
}
\usage{
dapply(x, by, FUN, ...)
}
\arguments{
  \item{x}{a dataframe containing one or more variables to aggregate}
  \item{by}{a list of equal-length factors that define the groups used to aggregates variables in \code{x}}
  \item{FUN}{function applied to each group, must return a single row of a dataframe}
  \item{\dots}{extra arguments to pass to the function \code{FUN}}
}
\details{
This function is similar to, and derived from, the \code{\link{tapply}}
and \code{\link{aggregate}}
functions in that it operates on an object \code{x} that is
split by a list of equal-length factors in \code{by} according to
a user-defined function \code{FUN}.  Unlike \code{tapply}, and like
\code{aggregate.data.frame}, it inputs and outputs a dataframe rather
than an array.   Except, unlike \code{aggregate},
it operates on multiple columns in a dataframe, allowing
for their interaction, and the return of multiple values
in the form of a dataframe row.   The \code{aggregate} function
only operates on multiple dataframe columns independently.

This new function \code{dapply} binds the returned dataframe rows into
a single dataframe that also contains unique combinations
of the factors passed in \code{by} corresponding to
each returned row value.

This function was originally designed to calculate the maximum value
e.g., of pollutant concentration, in a given group and return the maximum
value as well as corresponding parameters for other variables such
as monitoring height, angle of placement, or distance from source.

The function is really just a convenient way to split a dataframe
and aggregate parts of it based on one or more response variables
and an index list -- nicely keeping track of all the index variables in
the output.
}
\value{
A dataframe containing the factor levels that defined each unique group and the new aggregate variables 
}
\author{Neil Klepeis}
\seealso{\code{\link{tapply}} and \code{\link{aggregate}} both also aggregate R objects, but they can only treat a single variable at a time, i.e., without
considering interaction among different variables}
\examples{
d <- data.frame(u=1:10, v=rnorm(10), +
                y=LETTERS[1:10], z=c(rep("A",5),rep("B",5)))
dapply(d[c("u","v")], by=d[c("y","z")], +
       FUN=function(x) {idx<-which(max(x)); +
                        data.frame(max=x[idx],vatmax=v[idx])})
}
\keyword{manip}
