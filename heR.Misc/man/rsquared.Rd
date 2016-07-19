\name{rsquared}
\alias{rsquared}
\title{Coefficient of Determination (R-Squared)}
\description{
Calculate the coefficient of determination, commonly called
the "r-squared" value, for a least squares linear fit of two variates
}
\usage{
rsquared(ls.out)
}
\arguments{
  \item{ls.out}{the output object of a \code{\link{lsfit}} calculation}
}
\details{
This function was adapted from the \code{ls.print} function.

This function takes the output from an \code{\link{lsfit}} run and
returns the r-squared value, which is typically used as an indication of how much variation in the response (dependent) variable, y, is "explained" by the key (independent) variable, x.  The r-squared statistic is formally referred
to as the coefficient of determination.  Given two matched
data vectors x and y, it is the proportion of variation
in the reponse, y, that can be attibuted to an approximate
linear relationship between x and y.

The ceofficient of variation, r-squared, is defined as follows:
\tabular{lcl}{
SSresid \tab = \tab the residual sum of squares \cr
SStotal \tab = \tab the total sum of squares \cr
r-squared \tab = \tab 1 - SSresid/SSTotal \cr
}
}
\value{A single value equal to the r-squared for a given linear least squares fit
}
\author{Neil Klepeis}
\keyword{misc}
