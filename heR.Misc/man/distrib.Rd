\name{distrib}
\alias{distrib}
\alias{sim.distrib}
\alias{is.distrib}
\title{Create or Sample a Distribution}
\description{
Functions to create or print a `distrib' object that contains the
information needed to produce a sample from a distribution
model, an empirical distribution, or a number of raw values.
}
\usage{
distrib(values, weights = NULL, simulate)
is.distrib(x)
sim.distrib(x, n=1)
}
\arguments{
  \item{values}{raw empirical data, values for an empirical probability distribution, or distribution model parameter values.}
  \item{weights}{optional probability weights for empirical distributions}
  \item{simulate}{a character string giving the command used to simulate (sample) a value from the distribution, e.g., "sample", "rlnorm", "runif".}
  \item{x}{a \code{distrib} object or an object to test for being a \code{distrib} object}
  \item{n}{the number of samples to return}
}
\details{
The \code{distrib} function simply stores raw data values or distribution model
parameter values along with optional weights (for sampling from raw data values)
and the function used for sampling.  It checks for errors by trying to sample a value using the given arguments.  The values of \code{simulate} should be
one of either "sample" or one of the R simulation functions beginning with
"r", e.g., "runif", "rlnorm", or "runif" for sampling from a uniform, lognormal,
or normal distribution model, respectively.  Weights are only appropriate if
\code{simulate}="sample", and are all assigned values of 1 if they are omitted
for this case.  Weights can be used to specify an empirical probability
distribution.

The \code{sim.distrib} function returns 1 or more
sampled values from the distribution.
}
\value{
Either a \code{distrib} object, a logical specifying whether we have a \code{distrib} object or not, or one or more samples from a \code{distrib} object.
}
\author{Neil Klepeis}
\keyword{univar}
