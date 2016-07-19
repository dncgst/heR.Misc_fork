\name{scoeff}
\alias{scoeff}
\title{Normalized Sensitivity Coefficients}
\description{
Calculate normalized sensitivity coefficients using base and perturbed 
values for parameters and a response variable
}
\usage{
scoeff(par.base, par.pert, resp.base, resp.pert)
}
\arguments{
  \item{par.base}{vector of base parameter values}
  \item{par.pert}{vector of perturbed parameter values}
  \item{resp.base}{vector of base response values}
  \item{resp.pert}{vector of perturbed response values}
}
\details{
Calculate normalized sensitivity coefficients given base, i.e., central,
values for parameters and corresponding responses with plus and/or minus
perturbations.  The perturbation values can be in any order
but the response perturbations must correspond exactly to
the parameter perturbations.

The normalized sensitivity coefficients are calculated as:

sens <- resp/par

where par = (par.base - par.pert)/par.base and 
resp = (resp.base - resp.pert)/resp.base are the normalized changes in parameters and responses, i.e., the proportional changes.
}
\value{A list containing:
\item{base}{a list containing vectors of the base values of the parametes and response}
\item{pertval}{a list containing vectors of the parameter and response perturbed values)}
\item{pert}{a list containing vectors of the perturbation proportions for the parameters (par) and response (resp)},
\item{sens}{the normalized sensitivity coefficients}
}
\author{Neil Klepeis}

\keyword{misc}
