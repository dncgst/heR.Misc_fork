\name{optgrid}

\alias{optgrid}

\title{Optimize a function via grid search}


\description{Function to optimize an arbitrary function \code{f}
of parameters \code{par} using a local grid search method
}


\usage{
optgrid(f, par, incr, lower, upper, verbose=1, ...) 
}


\arguments{
\item{f}{the function to optimize, which should take a vector of parameter values as it's first argument and return a single function value}
\item{par}{a vector containing the initial set of parameter values for \code{f}}
\item{incr}{a vector containing the amount to increment each parameter value during each iteration}
\item{lower}{a vector containing the smallest allowable values for each parameter}
\item{upper}{a vector containing the largest allowable values for each parameter}
\item{verbose}{an integer that specifies the level of output during
 optimization, see below}
\item{...}{additional parameters to pass to the function \code{f}}
}


\details{
The user specifies the initial parameter values \code{par},
the parameter increments (precision) \code{incr}, the lowest
values \code{lower}, and the highest values \code{upper} for
each parameter. Extra parameters may also be passed to the
specified function \code{f}.

Given \code{n} is the number of parameters, 
the method employs a moving \code{n}-dimensional grid containing three
values per parameter -- low, medium, and high -- with spacing 
determined by \code{incr}.  The algorithm tries to center the 
grid around the minimum point for each dimension (parameter),
moving in an appropriate direction
during each iteration.  The optimization is successful when the 
grid becomes centered on a minimum point across all dimensions.

The advantage of this method is that the best estimates
of the function parameters are obtained at the precision
specified by \code{incr}, and the search is contrained by \code{upper}
and \code{lower} bounds for each parameter.  The disadvantage
is that it is slow, dumb, and inefficient.  But it works :-).

Speed is somewhat improved by the use of a lookup table, which prevents
the recalculation of points.

Set \code{verbose = 0} for no status messages.
Set \code{verbose >= 1} to output more information:
\code{verbose = 1} to output the best set of parameters and function values 
for each iteration;
\code{verbose = 2} to output the current best index for the parameters (ie, an 
index of 1 means go lower on the next iteration;  2 means just right, the
minimum value is centered; and 3 means go higher on the next iteration);
\code{verbose = 3} to output the number of model runs and
lookups performed during each iteration.

Things to do: allow percentage increments instead of actual increment
values; search along a single parameter before doing the whole
grid.
}


\value{
A list containing the final function array (grid) centered on the
minimum value, the lookup table, absolute sensitivity coefficients
around the minimum value, the initial parameters, and the number of
iterations. 
}


\author{Neil E. Klepeis
\cr 
\cr 
\cr
\url{http://klepeis.net}
}


\keyword{optimize}

