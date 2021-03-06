% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_marginal.R
\name{extract_marginals_hyper}
\alias{extract_marginals_hyper}
\title{Extract the hyperparameters' marginals}
\usage{
extract_marginals_hyper(.result)
}
\arguments{
\item{.result}{\code{inla} object.}
}
\value{
List of hyperparameter marginals.
}
\description{
Extract the hyperparameters' marginals.
}
\details{
Convert the precision used in the hyperparameers' marginals to standard
deviations and extract other hyperparametrs' marginals as is.
}
